(defpackage :chipi-web.auth-controller
  (:use :cl)
  (:nicknames :authc)
  (:export #:authorize-user
           #:verify-authorization
           #:user-not-found
           #:unable-to-authenticate
           #:unknown-token
           #:token-expired)
  )

(in-package :chipi-web.auth-controller)

(define-condition user-not-found (simple-error)
  ((username :initarg :username :reader username))
  (:report (lambda (condition stream)
             (format stream "User ~a not found." (username condition)))))

(define-condition unable-to-authenticate (simple-error)
  ((username :initarg :username :reader username))
  (:report (lambda (condition stream)
             (format stream "Unable to authenticate user: ~a." (username condition)))))

(define-condition unknown-token (simple-error)
  ((token-id :initarg :token-id :reader token-id))
  (:report (lambda (condition stream)
             (format stream "Unknown token: ~a." (token-id condition)))))

(define-condition token-expired (simple-error)
  ((token-id :initarg :token-id :reader token-id))
  (:report (lambda (condition stream)
             (format stream "Token expired: ~a." (token-id condition)))))

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

(defun verify-authorization (token-id)
  "Verifies if access token exists and is not expired."
  (unless (token-store:exists-token-p token-id)
    (error 'unknown-token :token-id token-id))
  (when (token-store:expired-token-p token-id)
    (error 'token-expired :token-id token-id)))
