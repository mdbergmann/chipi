(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:verify-apikey
           #:apikey-unknown-error
           #:apikey-expired-error)
  )

(in-package :chipi-web.auth-controller)

(define-condition apikey-unknown-error (simple-error)
  ((apikey :initarg :identifier :reader apikey))
  (:report (lambda (condition stream)
             (format stream "Unknown API key: ~a." (apikey condition)))))

(define-condition apikey-expired-error (simple-error)
  ((apikey :initarg :identifier :reader apikey))
  (:report (lambda (condition stream)
             (format stream "API key has expired: ~a." (apikey condition)))))

(defun verify-apikey (apikey)
  "Verifies if apikey exists and is not expired."
  (unless (apikey-store:exists-apikey-p apikey)
    (error 'apikey-unknown-error :identifier apikey))
  (when (apikey-store:expired-apikey-p apikey)
    (error 'apikey-expired-error :identifier apikey)))
