(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:authorize-user
           #:user-not-found
           #:unable-to-authenticate)
  )

(in-package :chipi-web.auth-controller)

(define-condition user-not-found (error)
  ((username :initarg :username :reader username))
  (:report (lambda (condition stream)
             (format stream "User ~a not found." (username condition)))))

(define-condition unable-to-authenticate (error)
  ((username :initarg :username :reader username))
  (:report (lambda (condition stream)
             (format stream "Unable to authenticate user: ~a." (username condition)))))

(defun authorize-user (username password)
  "Verifies if user exists and authenticates with provided password.
Returns `access-token-id` as string if all OK.
Raises condition `user-not-found` if user not found.
Raises condition `unable-to-authenticate` if password is incorrect."
  (unless (user-store:exists-username-p username)
    (error 'user-not-found :username username))
  (unless (user-store:equals-password-p username password)
    (error 'unable-to-authenticate :username username))
  (token-store:create-token username))

