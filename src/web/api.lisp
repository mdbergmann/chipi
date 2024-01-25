(defpackage :chipi-web.api
  (:use :cl :easy-routes)
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
  (setf *api* (hunchentoot:start
               (make-instance 'easy-routes-ssl-acceptor ;; 'easy-routes-acceptor
                              :ssl-privatekey-file "../../cert/localhost.key"
                              :ssl-certificate-file "../../cert/localhost.crt"
                              :port 8443
                              :address "127.0.0.1"))))

(defun stop ()
  (when *api*
    (hunchentoot:stop *api*)))

;; -----------------------------------
;; helpers
;; -----------------------------------

(defun %alist-to-json (alist)
  (yason:with-output-to-string* ()
    (yason:encode-alist alist)))

(defun %make-http-error (status-code message)
  (http-error status-code
              (%alist-to-json `(("error" . ,message)))))

(defun %make-json-response (alist)
  (%alist-to-json alist))

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

(defun @json-out (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @protection-headers-out (next)
  (setf (hunchentoot:header-out "X-XSS-Protection")
        "0"
        (hunchentoot:header-out "X-Content-Type-Options")
        "nosniff"
        (hunchentoot:header-out "X-Frame-Options")
        "DENY"
        (hunchentoot:header-out "Cache-Control")
        "no-store"
        (hunchentoot:header-out "Content-Security-Policy")
        "default-src 'none'; frame-ancestors 'none'; sandbox")
  (funcall next))

;; -----------------------------------
;; items
;; -----------------------------------

(defun @check-api-key (next)
  (flet ((error-response (err-message)
           (return-from @check-api-key
             (%make-http-error hunchentoot:+http-forbidden+
                               err-message))))
    ;; api-key header
    (let ((apikey (hunchentoot:header-in* "X-Api-Key")))
      (unless apikey
        (error-response "No API key provided"))

      ;; check on token
      (handler-case
          (authc:verify-authorization apikey)
        (authc:apikey-unknown-error (c)
          (log:info "~a" c)
          (error-response "Unknown API key"))
        (authc:apikey-expired-error (c)
          (log:info "~a" c)
          (error-response "API key has expired")))))
  (funcall next))

(defun %make-items-response (items)
  (if (car items)
      (yason:with-output-to-string* ()
        (let ((yason:*symbol-key-encoder* 
                'yason:encode-symbol-as-lowercase))
          (yason:encode
           (mapcar #'plist-hash-table items))))
      "[]"))

(defroute items-get
    ("/api/items"
     :method :get
     :decorators (@json-out
                  @protection-headers-out
                  @check-api-key)) ()
  (%make-items-response (itemsc:retrieve-items)))
