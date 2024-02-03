(defpackage :chipi-web.api
  (:use :cl :snooze)
  (:nicknames :api)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export #:start
           #:stop)
  )

(in-package :chipi-web.api)

(defvar *api* nil "The API server instance")

(defun start ()
  "Start the API server."
  (push (make-hunchentoot-app) hunchentoot:*dispatch-table*)
  (setf *api* (hunchentoot:start
               (make-instance 'hunchentoot:easy-acceptor
                              ;;:ssl-privatekey-file "../../cert/localhost.key"
                              ;;:ssl-certificate-file "../../cert/localhost.crt"
                              :port 8765
                              :address "127.0.0.1"))))

(defun stop ()
  (when *api*
    (hunchentoot:stop *api*)
    (setf hunchentoot:*dispatch-table* nil)))

;; -----------------------------------
;; helpers
;; -----------------------------------

(defun ph (plist)
  "Shorthand for plist-hash-table."
  (alexandria:plist-hash-table plist :test 'equal))

(defun ah (alist)
  "Shorthand for plist-hash-table."
  (alexandria:alist-hash-table alist :test 'equal))

(defun %alist-to-json (alist)
  (jzon:stringify (ah alist)))

(defun %make-json-response-body (alist)
  (%alist-to-json alist))

(defun %make-json-error-body (message)
  (%make-json-response-body `(("error" . ,message))))

;; -----------------------------------
;; decorators
;; -----------------------------------

(defun @json-out ()
  (setf (hunchentoot:content-type*) "application/json"))

(defun @protection-headers-out ()
  (setf (hunchentoot:header-out "X-XSS-Protection")
        "0"
        (hunchentoot:header-out "X-Content-Type-Options")
        "nosniff"
        (hunchentoot:header-out "X-Frame-Options")
        "DENY"
        (hunchentoot:header-out "Cache-Control")
        "no-store"
        (hunchentoot:header-out "Content-Security-Policy")
        "default-src 'none'; frame-ancestors 'none'; sandbox"))
 
;; -----------------------------------
;; items
;; -----------------------------------

(defun @check-api-key ()
  (flet ((error-response (err-message)
           (http-condition hunchentoot:+http-forbidden+ err-message)))
    ;; api-key header
    (let ((apikey (hunchentoot:header-in* "X-Api-Key")))
      (unless apikey
        (error-response "No API key provided"))

      ;; check on apikey
      (handler-case
          (authc:verify-apikey apikey)
        (authc:auth-apikey-unknown-error (c)
          (log:info "~a" c)
          (error-response "Unknown API key"))
        (authc:auth-apikey-expired-error (c)
          (log:info "~a" c)
          (error-response "API key has expired"))
        (authc:auth-apikey-invalid-error (c)
          (log:info "~a" c)
          (error-response "Invalid API key"))
        ))))

(defun @check-payload-length ()
  (let ((content-len (parse-integer (hunchentoot:header-in* "Content-Length"))))
    (format t "Content len: ~a~%" content-len)
    (when (> content-len 256)
      (http-condition hunchentoot:+http-request-entity-too-large+
                      "Oversized payload"))))

(defun %make-items-response (items)
  (cond
    ((car items)
     (jzon:stringify (mapcar #'ph items)))
    (t "[]")))

(defroute items (:get "application/json" &optional item-name)
  (@protection-headers-out)
  (@check-api-key)
  (log:info "item-name: ~a, type: ~a" item-name (type-of item-name))
  (cond
    ((null item-name)
     (%make-items-response (itemsc:retrieve-items)))
    (t
     (let* ((item-plist (itemsc:retrieve-item item-name)))
       (unless item-plist
         (http-condition hunchentoot:+http-not-found+
                         (format nil "Item '~a' not found" item-name)))
       (%make-items-response (list item-plist))))))

(defun %parse-item-value (json-payload)
  (let* ((ht (jzon:parse json-payload)))
    (multiple-value-bind (item-value present)
        (gethash "value" ht)
      (unless present
        (http-condition hunchentoot:+http-bad-request+
                        "No 'value' key found in JSON payload"))
      (cond
        ((typep item-value 'double-float)
         (coerce item-value 'single-float))
        ((eq item-value t)
         'item:true)
        ((eq item-value nil)
         'item:false)
        (t
         item-value)))))

(defroute items (:post "application/json" &optional item-name)
  ;;"`item-name' is not optional"
  (@protection-headers-out)
  (@check-api-key)
  (@check-payload-length)
  (let ((item-value (%parse-item-value (payload-as-string))))
    (unless (itemsc:update-item-value item-name item-value)
      (http-condition hunchentoot:+http-not-found+
                      (format nil "Item '~a' not found" item-name))))
  nil)

(defmethod snooze:explain-condition ((c http-condition)
                                     (resource (eql #'items))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" c)
  (%make-json-error-body (simple-condition-format-control c)))

(defmethod snooze:explain-condition ((c error)
                                     (resource (eql #'items))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" c)
  (typecase c
    (jzon:json-parse-error
     (%make-json-error-body "Unable to parse JSON"))
    (t
     (progn
       ;;(trivial-backtrace:print-backtrace c)
       (%make-json-error-body (simple-condition-format-control c))))))
