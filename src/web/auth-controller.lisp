(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-apikey
           #:auth-apikey-error
           #:auth-apikey-unknown-error
           #:auth-apikey-invalid-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition auth-apikey-error (simple-condition) ()
  (:report (lambda (c s)
             (format s "Auth API key error: ~a"
                     (simple-condition-format-control c)))))

(define-condition auth-apikey-unknown-error (auth-apikey-error)()
  (:default-initargs :format-control "Unknown API key"))

(define-condition auth-apikey-invalid-error (auth-apikey-error)()
  (:default-initargs :format-control "Invalid API key"))

(defun verify-apikey (apikey)
  (handler-case
      (progn
        (unless (apikey-store:exists-apikey-p apikey)
          (error 'auth-apikey-unknown-error)))
    (apikey-store:apikey-store-error (e)
      (error 'auth-apikey-invalid-error
             :format-control (simple-condition-format-control e)))))
