(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-apikey
           #:apikey-error
           #:apikey-unknown-error
           #:apikey-expired-error
           #:apikey-invalid-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition apikey-error (simple-error)
  ((apikey :initarg :identifier :reader apikey)))

(define-condition apikey-unknown-error (apikey-error)()
  (:report (lambda (condition stream)
             (format stream "Unknown API key: ~a" (apikey condition)))))

(define-condition apikey-expired-error (apikey-error)()
  (:report (lambda (condition stream)
             (format stream "API key has expired: ~a" (apikey condition)))))

(define-condition apikey-invalid-error (apikey-error)()
  (:report (lambda (condition stream)
             (format stream "Invalid API key: ~a" (apikey condition)))))

(defun verify-apikey (apikey)
  "Verifies if apikey exists and is not expired."
  (unless (apikey-store:signed-apikey-p apikey)
    (error 'apikey-invalid-error :identifier apikey))
  (unless (apikey-store:exists-apikey-p apikey)
    (error 'apikey-unknown-error :identifier apikey))
  (when (apikey-store:expired-apikey-p apikey)
    (error 'apikey-expired-error :identifier apikey)))
