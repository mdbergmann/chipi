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
  (when (> (length username) 30)
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Username too long, max 30 characters"))))

(defroute authenticate ("/api/authenticate" :method :post
                                            :decorators (@json))
    ((username :parameter-type 'string)
     (password :parameter-type 'string))
  (let ((verify-params-result (%verify-auth-parameters username password)))
    (when verify-params-result
      (return-from authenticate verify-params-result)))
  (format t "username, password: ~a, ~a~%" username password))
