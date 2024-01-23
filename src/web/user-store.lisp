(defpackage :chipi-web.user-store
  (:use :cl)
  (:nicknames :user-store)
  (:import-from #:alexandria
                #:when-let)
  (:export #:user
           #:make-user
           #:add-user
           #:exists-username-p
           #:equals-password-p
           #:*user-store-backend*
           #:*simple-file-backend*
           #:make-simple-file-backend))

(in-package :chipi-web.user-store)

(defvar *crypt-salt* (cryp:make-salt)
  "The salt used for hashing passwords.
This is stored (or loaded if exists) to file on startup: see `api-env:init'.")

(defvar *user-store-backend* nil
  "The backend used for storing users.")

(defclass user ()
  ((username :initarg :username :reader username)
   (password :initarg :password :reader password)))

(defun make-user (username password)
  (let ((hashed-pw (cryp:make-hash password *crypt-salt*)))
    (make-instance 'user :username username
                         :password hashed-pw)))

(defun add-user (user)
  "Adds the given user to the store."
  (store-user *user-store-backend* user))

(defun exists-username-p (username)
  "Returns true if the given username exists in the store."
  (not (null (find-user *user-store-backend* username))))

(defun equals-password-p (username password)
  "Returns true if the password matches the one stored for the given username."
  (when-let ((user (find-user *user-store-backend* username)))
    (cryp:equal-string-p
     (password user)
     (cryp:make-hash password *crypt-salt*))))

;; ---------------------------------
;; generic backend
;; ---------------------------------

(defgeneric find-user (backend username)
  (:documentation "Returns the user with the given username, or nil if not found."))
(defgeneric store-user (backend user)
  (:documentation "Stores the given user."))

;; ---------------------------------
;; simple file backed hash-table backend
;; ---------------------------------

(defclass simple-file-backend ()
  ((store :initform (make-hash-table :test #'equal)
          :reader store)
   (filepath :initarg :filepath
             :initform nil
             :reader filepath)))

(defmethod initialize-instance :after ((self simple-file-backend) &key)
  (assert (filepath self) nil "filepath not set")
  (when (probe-file (filepath self))
    (with-open-file (in (filepath self) :direction :input)
      (let ((user-list (marshal:unmarshal (read in))))
        (dolist (user user-list)
          (setf (gethash (username user) (store self)) user))))))

(defun make-simple-file-backend (&optional (dir (envi:ensure-runtime-dir)))
  (make-instance 'simple-file-backend
                 :filepath (merge-pathnames
                            "users"
                            (progn
                              (uiop:ensure-all-directories-exist (list dir))
                              dir))))

(defmethod marshal:class-persistent-slots ((self user))
  '(username password))

(defmethod find-user ((backend simple-file-backend) username)
  (gethash username (store backend)))

(defmethod store-user ((backend simple-file-backend) user)
  (setf (gethash (username user) (store backend)) user)
  ;; store to file
  (with-open-file (out (filepath backend) :direction :output
                                          :if-exists :supersede)
    (let ((user-list
            ;; generate simple list of users
            (loop :for user :being :the :hash-value :of (store backend)
                  :collect user)))
      (prin1 (marshal:marshal user-list) out))))
