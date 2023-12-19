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

(defvar *scrypt-salt* (babel:string-to-octets "my-awefully-secure-salt"))
(defvar *admin-user* nil "this should be elsewhere, i.e. database")
(defun generate-initial-admin-user (password)
  (let ((username "admin"))
    (let ((pw-base64 (cryp:scrypt-data
                      (babel:string-to-octets password)
                      *scrypt-salt*)))
      (format t "pw-base64url: ~a~%" pw-base64)
      (setf *admin-user* (cons username pw-base64)))))

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
                       "Invalid username. Must be 2-30 characters with only alpha numeric and number characters."))))

(defun %verify-user (username password)
  (if (equal username "admin")
      (let ((existing-pw (cdr *admin-user*)))
        (unless (cryp:equal-string-p existing-pw
                                     (cryp:scrypt-data
                                      (babel:string-to-octets password)
                                      *scrypt-salt*))
          (make-http-error hunchentoot:+http-forbidden+
                           "Invalid password")))
      nil))

(defroute authenticate
    ("/api/authenticate"
     :method :post
     :decorators (@json-out @json-in @protection-headers))
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

    (let ((verify-user-result
            (%verify-user username password)))
      (when verify-user-result
        (return-from authenticate verify-user-result)))
    ;; good result
    ;; generate token
    (yason:with-output-to-string* ()
      (yason:encode-alist
       `(("token" . ,(token-store:create-token username)))))))

(defun @json-out (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

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
