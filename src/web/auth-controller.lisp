(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-authorization
           #:apikey-unknown-error
           #:apikey-expired-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition apikey-unknown-error (simple-error)
  ((token-id :initarg :token-id :reader token-id))
  (:report (lambda (condition stream)
             (format stream "Unknown token: ~a." (token-id condition)))))

(define-condition apikey-expired-error (simple-error)
  ((token-id :initarg :token-id :reader token-id))
  (:report (lambda (condition stream)
             (format stream "Token expired: ~a." (token-id condition)))))

(defun verify-authorization (token-id)
  "Verifies if token exists and is not expired."
  (unless (token-store:exists-token-p token-id)
    (error 'apikey-unknown-error :token-id token-id))
  (when (token-store:expired-token-p token-id)
    (error 'apikey-expired-error :token-id token-id)))
