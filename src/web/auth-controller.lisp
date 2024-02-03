(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-apikey
           #:auth-apikey-error
           #:auth-apikey-unknown-error
           #:auth-apikey-expired-error
           #:auth-apikey-invalid-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition auth-apikey-error (simple-condition) ()
  (:report (lambda (c s)
             (format s "Auth API key error: ~a"
                     (simple-condition-format-control c)))))

(define-condition auth-apikey-unknown-error (auth-apikey-error)()
  (:default-initargs :format-control "Unknown API key"))

(define-condition auth-apikey-expired-error (auth-apikey-error)()
  (:default-initargs :format-control "API key has expired"))

(define-condition auth-apikey-invalid-error (auth-apikey-error)()
  (:default-initargs :format-control "Invalid API key"))

(defun verify-apikey (apikey)
  "Verifies if apikey exists and is not expired."
  (handler-case
      (progn
        (unless (apikey-store:exists-apikey-p apikey)
          (error 'auth-apikey-unknown-error))
        (when (apikey-store:expired-apikey-p apikey)
          (error 'auth-apikey-expired-error)))
    (apikey-store:apikey-store-error (e)
      (error 'auth-apikey-invalid-error
             :format-control (simple-condition-format-control e)))))
