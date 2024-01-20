(defpackage :chipi-web.user-store
  (:use :cl)
  (:nicknames :user-store)
  (:import-from #:alexandria
                #:when-let)
  (:export #:user
           #:exists-username-p
           #:equals-password-p))

(in-package :chipi-web.user-store)

;; TODO: inject this via environment variable
(defvar *scrypt-salt* "my-awefully-secure-salt")

(defclass user ()
  ((username :initarg :username :accessor username)
   (password :initarg :password :accessor password)))

(defun make-user (username password)
  (let ((hashed-pw (cryp:create-hash password *scrypt-salt*)))
    (make-instance 'user :username username
                         :password hashed-pw)))

(defvar *user*
  (let ((store (make-hash-table :test #'equal)))
    (setf (gethash "admin" store)
          (make-user "admin" "admin"))
    store))

(defun exists-username-p (username)
  "Returns true if the given username exists in the store."
  (not (null (gethash username *user*))))

(defun equals-password-p (username password)
  "Returns true if the password matches the one stored for the given username."
  (when-let ((user (gethash username *user*)))
    (cryp:equal-string-p
     (password user)
     (cryp:create-hash password *scrypt-salt*))))
