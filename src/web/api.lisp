(defpackage :chipi-web.api
  (:use :cl :snooze)
  (:nicknames :api)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:export #:start
           #:stop)
  )

(in-package :chipi-web.api)

(defvar *api* nil "The API server instance")

(defun start ()
  "Start the API server."
  (push (make-hunchentoot-app) hunchentoot:*dispatch-table*)
  (setf *api* (hunchentoot:start
               (make-instance 'hunchentoot:easy-ssl-acceptor
                              :ssl-privatekey-file "../../cert/localhost.key"
                              :ssl-certificate-file "../../cert/localhost.crt"
                              :port 8443
                              :address "127.0.0.1"))))

(defun stop ()
  (when *api*
    (hunchentoot:stop *api*)
    (setf hunchentoot:*dispatch-table* nil)))

;; -----------------------------------
;; helpers
;; -----------------------------------

(defun %alist-to-json (alist)
  (yason:with-output-to-string* ()
    (yason:encode-alist alist)))

(defun %make-json-response-body (alist)
  (%alist-to-json alist))

(defun %make-json-error-body (message)
  (%make-json-response-body `(("error" . ,message))))

;; -----------------------------------
;; conditions
;; -----------------------------------

(define-condition parameter-validation-error (simple-error)
  ((failed-args :initarg :failed-args
                :reader failed-args))
  (:report (lambda (condition stream)
             (format stream "Parameter validation failed: ~a"
                     (failed-args condition)))))

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

      ;; check on token
      (handler-case
          (authc:verify-apikey apikey)
        (authc:apikey-unknown-error (c)
          (log:info "~a" c)
          (error-response "Unknown API key"))
        (authc:apikey-expired-error (c)
          (log:info "~a" c)
          (error-response "API key has expired"))))))

(defun %make-items-response (items)
  (format t "items: ~a~%" items)
  (if (car items)
      (yason:with-output-to-string* ()
        (let ((yason:*symbol-key-encoder* 
                'yason:encode-symbol-as-lowercase))
          (yason:encode
           (mapcar #'plist-hash-table items))))
      "[]"))

(defroute items (:get "application/json" &optional item-id)
  ;;(@json-out)
  (@protection-headers-out)
  (unless (@check-api-key)
    (log:info "item-name: ~a" item-id)
    (cond
      ((null item-id)
       (%make-items-response (itemsc:retrieve-items)))
      (t
       (let* ((item-plist (itemsc:retrieve-item item-id)))
         (unless item-plist
           (http-condition hunchentoot:+http-not-found+
                           (format nil "Item '~a' not found" item-id)))
         (%make-items-response (list item-plist)))))))

;; (defroute items (:post "text/plain" item-id)
;;   (@protection-headers-out)
;;   (unless (@check-api-key)
;;     (let ((item-value (payload-as-string)))
;;       (itemsc:update-item-value item-id item-value))))

(defmethod explain-condition ((c http-condition)
                              (resource (eql #'items))
                              (ct snooze-types:application/json))
  (log:warn "HTTP condition: ~a" c)
  (%make-json-error-body (simple-condition-format-control c)))
