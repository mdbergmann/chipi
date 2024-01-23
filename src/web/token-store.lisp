(defpackage :chipi-web.token-store
  (:use :cl)
  (:nicknames :token-store)
  (:import-from #:local-time-duration
                #:duration
                #:duration-as)
  (:export #:create-token
           #:read-token
           #:revoke-token
           #:expired-token-p
           #:exists-token-p
           #:token
           #:token-id
           #:user-id
           #:expiry
           #:*token-life-time-duration*
           #:*memory-backend*
           #:*token-store-backend*)
  )

(in-package :chipi-web.token-store)

(defvar *token-store-backend* nil)

(defvar *default-token-life-time-duration* (duration :minute 10))
(defvar *token-life-time-duration* *default-token-life-time-duration*
  "The life time of a token as `ltd:duration' object.")

(defclass token ()
  ((token-id :initarg :token-id
             :initform (error "token-id is required")
             :reader token-id)
   (user-id :initarg :user-id
            :initform (error "user-id is required")
            :reader user-id)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time)
                        (duration-as *token-life-time-duration* :sec))
           :reader expiry
           :documentation "The universal-time when the token expires.")))

(defun %new-random-id ()
  (cryp:make-random-string 20 :uri t))

(defun create-token (username)
  (let ((token (make-instance 'token
                              :token-id (%new-random-id)
                              :user-id username)))
    (store-token *token-store-backend* token)
    (token-id token)))

(defun read-token (token-id)
  (check-type token-id string)
  (retrieve-token *token-store-backend* token-id))

(defun revoke-token (token-id)
  (check-type token-id string)
  (delete-token *token-store-backend* token-id))

(defun expired-token-p (token-id)
  (check-type token-id string)
  (when (< (expiry (read-token token-id))
           (get-universal-time))
    (revoke-token token-id)))

(defun exists-token-p (token-id)
  (check-type token-id string)
  (not (null (read-token token-id))))

;; ----------------------------------------
;; token store-backend
;; ----------------------------------------

(defgeneric store-token (backend token))
(defgeneric retrieve-token (backend token-id))
(defgeneric delete-token (backend token-id))

;; ----------------------------------------
;; memory backend
;; ----------------------------------------

(defvar *memory-backend* 'memory)

;; TODO: serialize access to this. Agent?
(defvar *tokens* (make-hash-table :test #'equal))

(defun %dump-store ()
  (maphash (lambda (k v)
             (format t "~a => ~a~%" k v))
           *tokens*))

(defmethod store-token ((backend (eql 'memory)) token)
  (setf (gethash (token-id token) *tokens*) token))

(defmethod retrieve-token ((backend (eql 'memory)) token-id)
  (gethash token-id *tokens*))

(defmethod delete-token ((backend (eql 'memory)) token-id)
  (remhash token-id *tokens*))
