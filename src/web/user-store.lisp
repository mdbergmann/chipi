(defpackage :chipi-web.user-store
  (:use :cl)
  (:nicknames :user-store)
  (:export #:user
           #:find-user-by-username
           #:verify-password))

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

(defun find-user-by-username (username)
  (gethash username *user*))

(defun verify-password (user password)
  "Not implemented")
