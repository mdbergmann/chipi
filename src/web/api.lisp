(defpackage :chipi-web.api
  (:use :cl :easy-routes)
  (:nicknames :api)
  (:export #:start
           #:stop)
  )

(in-package :chipi-web.api)

(defvar *api* nil "The API server instance")

(defun start ()
  (setf *api* (hunchentoot:start
               (make-instance 'easy-routes-acceptor
                              ;;:ssl-privatekey-file "../cert/localhost.key"
                              ;;:ssl-certificate-file "../cert/localhost.crt"
                              :port 8443
                              :address "0.0.0.0"))))

(defun stop ()
  (when *api*
    (hunchentoot:stop *api*)))

(defun make-http-error (status-code message)
  (http-error status-code
              (yason:with-output-to-string* ()
                (yason:encode-alist `(("error" . ,message))))))

(defun %verify-auth-parameters (username password)
  (when (or (null username)
            (null password))
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Missing username or password")))
  (unless (ppcre:scan "(?i)[a-zA-Z][a-zA-Z0-9]{1,29}" username)
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Invalid username. Must be 2-30 characters with only alpha numeric and number characters.")))
  (when (< (length password) 8)
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Invalid password. Must be at least 8 characters."))))

(defun @json-in (next)
  (if (equal (hunchentoot:header-in* "Content-Type")
             "application/json")
      (funcall next)
      (make-http-error hunchentoot:+http-bad-request+
                       "Content-Type must be application/json")))

(defun @protection-headers (next)
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

(defroute authenticate
    ("/api/authenticate"
     :method :post
     :decorators (@json @json-in @protection-headers))
    ()
  (let* ((post-string
           (babel:octets-to-string
            (hunchentoot:raw-post-data)))
         (json (yason:parse post-string :object-as :alist))
         (username (cdr (assoc "username" json :test #'equal)))
         (password (cdr (assoc "password" json :test #'equal))))
    (let ((verify-params-result
            (%verify-auth-parameters username password)))
      (when verify-params-result
        (return-from authenticate verify-params-result)))

    ;; do something with username and password
    
    ))
