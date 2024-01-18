(defpackage :chipi-web.token-store
  (:use :cl)
  (:nicknames :token-store)
  (:export #:create-token
           #:read-token
           #:revoke-token
           #:expired-p
           #:token
           #:token-id
           #:user-id
           #:expiry)
  )

(in-package :chipi-web.token-store)

(defvar *token-store-backend* 'memory)
(defvar *default-token-life-time-seconds* (* 60 10)) ; 10 minutes
(defvar *token-life-time-seconds* *default-token-life-time-seconds*)

(defclass token ()
  ((token-id :initarg :token-id
             :initform (error "token-id is required")
             :reader token-id)
   (user-id :initarg :user-id
            :initform (error "user-id is required")
            :reader user-id)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time) *token-life-time-seconds*)
           :reader expiry)))

(defun %new-random-id ()
  (let ((id (crypto:random-data 20)))
    (base64:usb8-array-to-base64-string id :uri t)))

(defun create-token (username)
  (let ((token (make-instance 'token
                              :token-id (%new-random-id)
                              :user-id username)))
    (store-token *token-store-backend* token)
    (token-id token)))

(defun read-token (token-id)
  (retrieve-token *token-store-backend* token-id))

(defun revoke-token (token-id)
  (delete-token *token-store-backend* token-id))

(defun expired-p (token-id)
  (when (< (expiry (retrieve-token token-id)) (get-universal-time))
    (revoke-token token-id)))

;; ----------------------------------------
;; token store-backend
;; ----------------------------------------

(defgeneric store-token (backend token))
(defgeneric retrieve-token (backend token-id))
(defgeneric delete-token (backend token-id))

;; ----------------------------------------
;; memory backend
;; ----------------------------------------

(defvar *tokens* (make-hash-table :test #'equal))

(defmethod store-token ((backend (eql 'memory)) token)
  (setf (gethash (token-id token) *tokens*) token))

(defmethod retrieve-token ((backend (eql 'memory)) token-id)
  (gethash token-id *tokens*))

(defmethod delete-token ((backend (eql 'memory)) token-id)
  (remhash token-id *tokens*))
