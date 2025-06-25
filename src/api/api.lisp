(defpackage :chipi-api.api
  (:use :cl :snooze)
  (:nicknames :api)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:import-from #:chipi-api.events-controller
                #:handle-sse-connection)
  (:export #:start
           #:stop)
  )

(in-package :chipi-api.api)

(defvar *api* nil "The API server instance")

(defun start (&key address)
  "Start the API server."
  (when *api*
    (log:warn "API server already started."))
  (unless *api*
    (push (make-hunchentoot-app) hunchentoot:*dispatch-table*)
    ;; Add custom SSE handler for events endpoint
    (push (hunchentoot:create-regex-dispatcher "^/events/items$" 'events-stream)
          hunchentoot:*dispatch-table*)
    (setf *api* (hunchentoot:start
                 (make-instance 'hunchentoot:easy-acceptor
                                ;;:ssl-privatekey-file "../../cert/localhost.key"
                                ;;:ssl-certificate-file "../../cert/localhost.crt"
                                :port 8765
                                :address address)))
    ;; register shutdown hook
    (hab:add-to-shutdown
     (lambda ()
       (stop)))))

(defun stop ()
  (log:info "Stopping API server...")
  (unless *api*
    (log:info "Not running."))
  (when *api*
    (hunchentoot:stop *api*)
    (setf hunchentoot:*dispatch-table* nil)
    (setf *api* nil)))

;; -----------------------------------
;; helpers
;; -----------------------------------

(defun ah (alist)
  "Shorthand for alist-hash-table."
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
           (http-condition hunchentoot:+http-authorization-required+ err-message)))
    ;; api-key header
    (let ((apikey (hunchentoot:header-in* "X-Api-Key")))
      (unless apikey
        (error-response "No API key provided"))

      ;; check on apikey
      (handler-case
          (authc:verify-apikey apikey)
        (authc:auth-apikey-unknown-error (condition)
          (log:info "~a" condition)
          (error-response "Unknown API key"))
        (authc:auth-apikey-invalid-error (condition)
          (log:info "~a" condition)
          (error-response "Invalid API key"))
        ))))

(defun @check-access-rights (required-rights)
  "Assumes that API key exists and is valid"
  (flet ((error-response (err-message)
           (http-condition hunchentoot:+http-forbidden+ err-message)))
    (let ((apikey (hunchentoot:header-in* "X-Api-Key")))
      (handler-case
          (authc:verify-access-rights apikey required-rights)
        (authc:auth-access-rights-error (condition)
          (log:info "~a" condition)
          (error-response "Insufficient access rights"))
        ))))

(defun @check-payload-length ()
  (let ((content-len (parse-integer (hunchentoot:header-in* "Content-Length"))))
    (format t "Content len: ~a~%" content-len)
    (when (> content-len 256)
      (http-condition hunchentoot:+http-request-entity-too-large+
                      "Oversized payload"))))

(defun %make-items-response (items)
  (cond
    ((car items) (jzon:stringify items))
    (t "[]")))

(defun %make-itemgroups-response (groups)
  "Convert list of itemgroup hash-tables to a JSON string (empty array if none)."
  (cond
    ((car groups) (jzon:stringify groups))
    (t "[]")))

(defroute items (:get "application/json" &optional item-name)
  (@protection-headers-out)
  (@check-api-key)
  (@check-access-rights '(:read))
  (@json-out)
  (log:info "item-name: ~a, type: ~a" item-name (type-of item-name))
  (cond
    ((null item-name)
     (%make-items-response (itemsc:retrieve-items)))
    (t
     (let* ((item-ht (itemsc:retrieve-item item-name)))
       (unless item-ht
         (http-condition hunchentoot:+http-not-found+
                         (format nil "Item '~a' not found" item-name)))
       (%make-items-response (list item-ht))))))

(defun %parse-item-value (json-payload)
  (let ((ht (jzon:parse json-payload)))
    (multiple-value-bind (item-value present)
        (gethash "value" ht)
      (unless present
        (http-condition hunchentoot:+http-bad-request+
                        "No 'value' key found in JSON payload"))
      (item-ext:item-value-ext-to-internal item-value))))

(defroute items (:post "application/json" &optional item-name)
  ;;"`item-name' is not optional"
  (@protection-headers-out)
  (@check-api-key)
  (@check-access-rights '(:update))
  (@check-payload-length)
  (let ((item-value (%parse-item-value (payload-as-string))))
    (unless (itemsc:update-item-value item-name item-value)
      (http-condition hunchentoot:+http-not-found+
                      (format nil "Item '~a' not found" item-name))))
  nil)

(defmethod snooze:explain-condition ((condition http-condition)
                                     (resource (eql #'items))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" condition)
  (%make-json-error-body (simple-condition-format-control condition)))

(defmethod snooze:explain-condition ((condition error)
                                     (resource (eql #'items))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" condition)
  (typecase condition
    (jzon:json-parse-error
     (%make-json-error-body "Unable to parse JSON"))
    (t
     (progn
       ;;(trivial-backtrace:print-backtrace condition)
       (%make-json-error-body (simple-condition-format-control condition))))))
       
;; -----------------------------------
;; itemgroups
;; -----------------------------------

(defroute itemgroups (:get "application/json" &optional group-name)
  (@protection-headers-out)
  (@check-api-key)
  (@check-access-rights '(:read))
  (@json-out)
  (cond
    ((null group-name)
     (%make-itemgroups-response (itemgroupsc:retrieve-itemgroups)))
    (t
     (let ((grp (itemgroupsc:retrieve-itemgroup group-name)))
       (unless grp
         (http-condition hunchentoot:+http-not-found+
                         (format nil "Itemgroup '~a' not found" group-name)))
       (%make-itemgroups-response (list grp))))))

(defmethod snooze:explain-condition ((condition http-condition)
                                     (resource (eql #'itemgroups))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" condition)
  (%make-json-error-body (simple-condition-format-control condition)))

(defmethod snooze:explain-condition ((condition error)
                                     (resource (eql #'itemgroups))
                                     (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" condition)
  (%make-json-error-body (simple-condition-format-control condition)))

;; -----------------------------------
;; events (SSE)
;; -----------------------------------

(defun @sse-headers-out ()
  "Set SSE-specific headers"
  (setf (hunchentoot:content-type*) "text/event-stream")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (setf (hunchentoot:header-out "Connection") "keep-alive")
  (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
  (setf (hunchentoot:header-out "Access-Control-Allow-Headers") "X-Api-Key"))

(defun events-stream ()
  "SSE endpoint for item value changes"
  (handler-case
      (progn
        (@check-api-key)
        (@check-access-rights '(:read))
        (@sse-headers-out)
        (let ((stream (hunchentoot:send-headers)))
          (handle-sse-connection stream)
          ;; Return empty string to prevent further processing
          ""))
    (condition (c)
      (log:warn "SSE request failed: ~a" c)
      (hunchentoot:abort-request-handler 403))))
