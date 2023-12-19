(defpackage :chipi-web.token-store
  (:use :cl)
  (:nicknames :token-store)
  (:export #:create-token
           #:token
           #:token-id
           #:user-id
           #:expiry)
  )

(in-package :chipi-web.token-store)

(defclass token ()
  ((token-id :initarg :token-id
             :initform (error "token-id is required")
             :reader token-id)
   (user-id :initarg :user-id
            :initform (error "user-id is required")
            :reader user-id)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time) (* 60 10)) ; 10 minutes
           :reader expiry)))

(defun %new-random-id ()
  (let ((id (crypto:random-data 20)))
    (base64:usb8-array-to-base64-string id :uri t)))

(defun create-token (username)
  (let ((token (make-instance 'token
                              :token-id (%new-random-id)
                              :user-id username)))
    (store-token 'memory token)
    (token-id token)))

;; ----------------------------------------
;; token store-backend
;; ----------------------------------------

(defgeneric store-token (backend token))

;; ----------------------------------------
;; memory backend
;; ----------------------------------------

(defvar *tokens* (make-hash-table :test #'equal))

(defmethod store-token ((backend (eql 'memory)) token)
  (setf (gethash (token-id token) *tokens*) token))

