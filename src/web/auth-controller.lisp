(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-apikey
           #:verify-access-rights
           ;; conditions
           #:auth-error
           #:auth-apikey-unknown-error
           #:auth-apikey-invalid-error
           #:auth-access-rights-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition auth-error (simple-condition) ()
  (:report (lambda (condition stream)
             (format stream "Auth error: ~a"
                     (simple-condition-format-control condition)))))

(define-condition auth-apikey-unknown-error (auth-error)()
  (:default-initargs :format-control "Unknown API key"))

(define-condition auth-apikey-invalid-error (auth-error)()
  (:default-initargs :format-control "Invalid API key"))

(define-condition auth-access-rights-error (auth-error)()
  (:default-initargs :format-control "Insufficient access rights"))

(defun verify-apikey (apikey)
  (handler-case
      (progn
        (unless (apikey-store:exists-apikey-p apikey)
          (error 'auth-apikey-unknown-error)))
    (apikey-store:apikey-store-error (e)
      (error 'auth-apikey-invalid-error
             :format-control (simple-condition-format-control e)))))

(defun verify-access-rights (apikey access-rights)
  (unless (apikey-store:has-access-rights-p apikey access-rights)
    (error 'auth-access-rights-error)))
