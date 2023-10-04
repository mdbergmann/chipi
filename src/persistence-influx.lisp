(defpackage :cl-hab.influx-persistence
  (:use :cl :cl-hab.persistence)
  (:nicknames :influx-persistence)
  (:import-from #:persp
                #:persistence)
  (:export #:influx-persistence
           #:make-influx-persistence)
  )

(in-package :cl-hab.influx-persistence)

(defclass influx-persistence (persistence)
  ((base-url :initform nil
             :reader base-url)
   (token :initform nil
          :reader token)
   (org :initform nil
        :reader org)
   (bucket :initform nil
           :reader bucket)
   (precision :initform nil
              :reader precision)))

(defun make-influx-persistence (id &key
                                     base-url
                                     token
                                     org
                                     bucket)
  (persp::make-persistence id
                           :type 'influx-persistence
                           :base-url base-url
                           :token token
                           :org org
                           :bucket bucket))

(defmethod act:pre-start ((persistence influx-persistence))
  (log:debug "Pre-starting persistence: ~a" persistence)
  (let ((other-args (act:other-init-args persistence)))
    (log:debug "Other args: ~a" other-args)
    (when other-args
      (with-slots (base-url token org bucket precision) persistence
        (setf base-url (getf other-args :base-url)
              token (getf other-args :token)
              org (getf other-args :org)
              bucket (getf other-args :bucket)
              precision "s"))))
  (call-next-method))

(defmethod initialize ((persistence influx-persistence))
  (log:info "Initializing persistence: ~a" persistence))

(defmethod shutdown ((persistence influx-persistence))
  (log:info "Shutting down persistence: ~a" persistence))

(defun %make-write-request (persistence item-name item-value item-timestamp)
  (drakma:http-request (format nil "~a/api/v2/write" (base-url persistence))
                       :method :post
                       :parameters `(("bucket" . ,(bucket persistence))
                                     ("org" . ,(org persistence))
                                     ("precision" . ,(precision persistence)))
                       :accept "application/json"
                       :additional-headers
                       `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                       :content-type "text/plain; charset=utf-8"
                       :content
                       (cond
                         ((stringp item-value)
                          (format nil "~a,item=~a value=\"~a\" ~a"
                                  item-name item-name item-value
                                  (local-time:timestamp-to-unix item-timestamp)))
                         ((integerp item-value)
                          (format nil "~a,item=~a value=~ai ~a"
                                  item-name item-name item-value
                                  (local-time:timestamp-to-unix item-timestamp)))
                         ((floatp item-value)
                          (format nil "~a,item=~a value=~a ~a"
                                  item-name item-name item-value
                                  (local-time:timestamp-to-unix item-timestamp)))
                         ((or (eq item-value 'item:true)
                              (eq item-value 'item:false))
                          (format nil "~a,item=~a value=~a ~a"
                                  item-name item-name
                                  (if (eq item-value 'item:true) "true" "false")
                                  (local-time:timestamp-to-unix item-timestamp)))
                         (t (error "Unsupported item value type: ~a" (type-of item-value)))
                         )))

(defmethod persist ((persistence influx-persistence) item)
  (log:debug "Persisting, item: ~a" item)
  (let* ((item-name (act-cell:name item))
         (item-state (item:get-item-stateq item))
         (item-value (item:item-state-value item-state))
         (item-timestamp (local-time:universal-to-timestamp
                          (item:item-state-timestamp item-state))))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-write-request persistence item-name item-value item-timestamp)
          (case status
            (204 (log:info "Persisted item OK: ~a" item))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to persist item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to persist item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to persist item: ~a, with error: ~a" item e)))))

(defun %make-query-request (persistence item-name)
  (drakma:http-request (format nil "~a/api/v2/query" (base-url persistence))
                       :method :post
                       :parameters `(("org" . ,(org persistence)))
                       :accept "application/json"
                       :additional-headers
                       `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                       :content-type "application/vnd.flux"
                       :content 
                       (format nil "from(bucket:\"~a\")
|> range(start: 0)
|> filter(fn: (r) => r._measurement == \"~a\")
|> last()" (bucket persistence) item-name)))

(defun %parse-csv-to-header-val-pairs (body)
  (let* ((csv-lines (str:split (format nil "~C~C" #\return #\linefeed) body))
         (csv-columns
           (mapcar (lambda (s) (str:split "," s :omit-nulls t))
                   csv-lines))
         (header-val-pairs
           (mapcar #'list (first csv-columns) (second csv-columns))))
    header-val-pairs))

(defun %parsed-persisted-item (response-body type-hint)
  (let* ((header-val-pairs (%parse-csv-to-header-val-pairs response-body))
         (timestamp
           (find "_time" header-val-pairs :test #'equal :key #'car))
         (value
           (find "_value" header-val-pairs :test #'equal :key #'car)))
    (let ((persisted-item
            (make-persisted-item
             :value (cond
                      ((eq type-hint 'integer)
                       (parse-integer (second value)))
                      ((eq type-hint 'float)
                       (parse-float:parse-float (second value)))
                      ((eq type-hint 'boolean)
                       (if (string= "true" (second value))
                           'item:true
                           'item:false))
                      ((eq type-hint 'string)
                       (second value))
                      (t (error "Unsupported type: ~a" type-hint)))
             :timestamp (local-time:timestamp-to-universal
                         (local-time:parse-timestring (second timestamp))))))
      (log:debug "Loaded persisted item: ~a" persisted-item)
      persisted-item)))

(defmethod retrieve ((persistence influx-persistence) item)
  "Output format of influxdb is csv, so we need to parse it."
  (log:debug "Reading item: ~a" item)
  (let ((item-name (act-cell:name item))
        (type-hint (item:value-type-hint item)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-query-request persistence item-name)
          (case status
            (200
             (progn
               (log:info "Read item OK: ~a" item)
               (log:debug "Response: ~a" body)
               (%parsed-persisted-item body type-hint)))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to read item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to read item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to read item: ~a, with error: ~a" item e)
        `(:error . ,e)))))
