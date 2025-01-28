(defpackage :chipi.influx-persistence
  (:use :cl :chipi.persistence)
  (:nicknames :influx-persistence)
  (:import-from #:persp
                #:persistence)
  (:export #:influx-persistence
           #:make-influx-persistence)
  )

(in-package :chipi.influx-persistence)

(defclass influx-persistence (persistence)
  ((base-url :initarg :base-url
             :initform nil
             :reader base-url)
   (token :initarg :token
          :initform nil
          :reader token)
   (org :initarg :org
        :initform nil
        :reader org)
   (bucket :initarg :bucket
           :initform nil
           :reader bucket)
   (precision :initarg :precision
              :initform "s"
              :reader precision))
  (:documentation "Influx persistence implementation.
Influx persistence supports the following value types:
- string
- integer
- float
- boolean, for boolean values, use the symbols 'item:true and 'item:false.

Other value types are not supported.
Specify those types as 'type-hint' in the item definition."))

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

(defmethod initialize ((persistence influx-persistence))
  (log:info "Initializing persistence: ~a" persistence))

(defmethod shutdown ((persistence influx-persistence))
  (log:info "Shutting down persistence: ~a" persistence))

;; ---------------------------------------
;; Writing
;; ---------------------------------------

(defun %make-write-request (persistence item-name item-value item-timestamp)
  (let ((data-string
          (cond
            ((stringp item-value)
             (format nil "~a,item=~a value=\"~a\" ~a"
                     item-name item-name item-value item-timestamp))
            ((integerp item-value)
             (format nil "~a,item=~a value=~ai ~a"
                     item-name item-name item-value item-timestamp))
            ((floatp item-value)
             (format nil "~a,item=~a value=~f ~a"
                     item-name item-name item-value item-timestamp))
            ((or (eq item-value 'item:true)
                 (eq item-value 'item:false))
             (format nil "~a,item=~a value=~a ~a"
                     item-name item-name
                     (if (eq item-value 'item:true) "true" "false")
                     item-timestamp))
            (t (error "Unsupported item value type: ~a" (type-of item-value))))))
    (log:debug "Write request data string: ~a" data-string)
    (drakma:http-request (format nil "~a/api/v2/write" (base-url persistence))
                         :method :post
                         :parameters `(("bucket" . ,(bucket persistence))
                                       ("org" . ,(org persistence))
                                       ("precision" . ,(precision persistence)))
                         :accept "application/json"
                         :additional-headers
                         `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                         :content-type "text/plain; charset=utf-8"
                         :content data-string)))

(defmethod persist ((persistence influx-persistence) item)
  (log:debug "Persisting item: ~a" (item:name item))
  (let* ((item-name (act-cell:name item))
         (item-state (item:get-item-stateq item))
         (item-value (item:item-state-value item-state))
         (item-timestamp (local-time:universal-to-timestamp
                          (item:item-state-timestamp item-state)))
         (unix-timestamp (local-time:timestamp-to-unix item-timestamp)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-write-request persistence item-name item-value unix-timestamp)
          (case status
            (204 (log:info "Persisted item OK: ~a, timestamp: ~a" item-name item-timestamp))
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

;; ---------------------------------------
;; Querying
;; ---------------------------------------

(defun %universal-to-unix-ts (uts)
  (local-time:timestamp-to-unix
   (local-time:universal-to-timestamp uts)))

(defun %relative-range-to-string (range)
  (let ((days (persp:days range))
        (hours (persp:hours range))
        (minutes (persp:minutes range))
        (seconds (persp:seconds range))
        (end-ts (persp:end-ts range)))
    (flet ((absolute ()
             (let* ((start-ts
                      (cond
                        (days (- end-ts (* days 24 60 60)))
                        (hours (- end-ts (* hours 60 60)))
                        (minutes (- end-ts (* minutes 60)))
                        (seconds (- end-ts seconds))
                        (t end-ts)))
                    (end-unix-ts (%universal-to-unix-ts end-ts))
                    (start-unix-ts (%universal-to-unix-ts start-ts)))
               (format nil "start: ~a, stop: ~a" start-unix-ts end-unix-ts)))
           (relative ()
             (format nil "start: ~a"
                     (cond
                       (days (format nil "-~ad" days))
                       (hours (format nil "-~ah" hours))
                       (minutes (format nil "-~am" minutes))
                       (seconds (format nil "-~as" seconds))
                       (t "0")))))
      (if end-ts
          (absolute)
          (relative)))))

(defun %range-to-string (range)
  (if (not range) "0"
      (typecase range
        (relative-range (%relative-range-to-string range)))))

(defun %make-single-query-string (persistence item-name)
  (format nil "from(bucket:\"~a\")
|> range(start: 0)
|> filter(fn: (r) => r._measurement == \"~a\")
|> last()"
          (bucket persistence)
          item-name))

(defun %make-range-query-string (persistence item-name range)
  (format nil "from(bucket:\"~a\")
|> range(~a)
|> filter(fn: (r) => r._measurement == \"~a\")"
          (bucket persistence)
          (%range-to-string range)
          item-name))

(defun %make-query-request (persistence item-name &optional range)
  (let ((data-string (if range
                         (%make-range-query-string persistence item-name range)
                         (%make-single-query-string persistence item-name))))
    (log:debug "Query request data string: ~a" data-string)
    (drakma:http-request (format nil "~a/api/v2/query" (base-url persistence))
                         :method :post
                         :parameters `(("org" . ,(org persistence)))
                         :accept "application/json"
                         :additional-headers
                         `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                         :content-type "application/vnd.flux"
                         :content data-string)))

(defun %parse-csv-to-time-value-pairs (body)
  (let* ((csv-lines (str:split (format nil "~C~C" #\return #\linefeed) body :omit-nulls t))
         (csv-headers (str:split "," (first csv-lines) :omit-nulls t))
         (index-time (position "_time" csv-headers :test #'equal))
         (index-value (position "_value" csv-headers :test #'equal))
         (csv-columns
           (mapcar (lambda (s) (str:split "," s :omit-nulls t))
                   (rest csv-lines)))
         (time-value-pairs
           (mapcar (lambda (row)
                     (cons (nth index-time row) (nth index-value row)))
                   csv-columns)))
    time-value-pairs))

(defun %make-persisted-item (timestamp value type-hint)
  (make-persisted-item
   :value (cond
            ((eq type-hint 'integer)
             (parse-integer value))
            ((eq type-hint 'float)
             (coerce (parse-float:parse-float value) 'single-float))
            ((eq type-hint 'boolean)
             (if (string= "true" value)
                 'item:true
                 'item:false))
            ((eq type-hint 'string)
             value)
            (t (error "Unsupported type: ~a" type-hint)))
   :timestamp (local-time:timestamp-to-universal
               (local-time:parse-timestring timestamp))))

;; -------------------------------------
;; Retrieve last
;; -------------------------------------

(defun %parsed-last-persisted-item (response-body type-hint)
  (let ((pairs (%parse-csv-to-time-value-pairs response-body)))
    (log:debug "Parsed pairs: ~a" pairs)
    (let* ((last-pair (first (last pairs)))
           (timestamp (car last-pair))
           (value (cdr last-pair))
           (persisted-item (%make-persisted-item timestamp value type-hint)))
      (log:debug "Loaded persisted item: ~a" persisted-item)
      (log:trace "Loaded persisted item value type: ~a" (type-of (persisted-item-value persisted-item)))
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
               (%parsed-last-persisted-item body type-hint)))
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

;; ------------------------------------------
;; Range retrieval
;; ------------------------------------------

(defun %parsed-range-persisted-item (response-body type-hint)
  (let ((pairs (%parse-csv-to-time-value-pairs response-body)))
    (log:debug "Parsed pairs: ~a" pairs)
    (let ((persisted-items
            (mapcar (lambda (pair)
                      (let ((timestamp (car pair))
                            (value (cdr pair)))
                        (%make-persisted-item timestamp value type-hint)))
                    pairs)))
      (log:debug "Loaded persisted items: ~a" persisted-items)
      persisted-items)))

(defmethod retrieve-range ((persistence influx-persistence) item range)
  (log:debug "Reading item with range: ~a" item)
  (let ((item-name (act-cell:name item))
        (type-hint (item:value-type-hint item)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-query-request persistence item-name range)
          (case status
            (200
             (progn
               (log:info "Read item OK: ~a" item)
               (log:debug "Response: ~a" body)
               (%parsed-range-persisted-item body type-hint)))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to read item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to read item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to read item with range: ~a, with error: ~a" item e)
        `(:error . ,e)))))
