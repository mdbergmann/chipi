(defpackage :chipi-web.apikey-store
  (:use :cl)
  (:nicknames :apikey-store)
  (:import-from #:local-time-duration
                #:duration
                #:duration-as)
  (:export #:create-apikey
           #:read-apikey
           #:revoke-apikey
           #:expired-apikey-p
           #:exists-apikey-p
           #:apikey
           #:identifier
           #:expiry
           #:*apikey-life-time-duration*
           #:make-simple-file-backend
           #:*memory-backend*
           #:*apikey-store-backend*)
  )

(in-package :chipi-web.apikey-store)

(defvar *apikey-store-backend* nil)

(defvar *default-apikey-life-time-duration* (duration :day 100))
(defvar *apikey-life-time-duration* *default-apikey-life-time-duration*
  "The life time of a apikey as `ltd:duration' object.")

(defclass apikey ()
  ((identifier :initarg :identifier
               :initform (error "identifier is required")
               :reader identifier)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time)
                        (duration-as *apikey-life-time-duration* :sec))
           :reader expiry
           :documentation "The universal-time when the apikey expires.")))

(defun %new-random-id ()
  (cryp:make-random-string 20 :uri t))

(defun create-apikey ()
  (let ((apikey (make-instance 'apikey
                               :identifier (%new-random-id))))
    (store-apikey *apikey-store-backend* apikey)
    (identifier apikey)))

(defun read-apikey (identifier)
  (check-type identifier string)
  (retrieve-apikey *apikey-store-backend* identifier))

(defun revoke-apikey (identifier)
  (check-type identifier string)
  (delete-apikey *apikey-store-backend* identifier))

(defun expired-apikey-p (identifier)
  (check-type identifier string)
  (when (< (expiry (read-apikey identifier))
           (get-universal-time))
    (revoke-apikey identifier)))

(defun exists-apikey-p (identifier)
  (check-type identifier string)
  (not (null (read-apikey identifier))))

;; ----------------------------------------
;; apikey store-backend
;; ----------------------------------------

(defgeneric store-apikey (backend apikey))
(defgeneric retrieve-apikey (backend identifier))
(defgeneric delete-apikey (backend identifier))

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
      (let ((apikey-list (marshal:unmarshal (read in))))
        (dolist (apikey apikey-list)
          (setf (gethash (identifier apikey) (store self)) apikey))))))

(defun make-simple-file-backend (&optional (dir (envi:ensure-runtime-dir)))
  (make-instance 'simple-file-backend
                 :filepath (merge-pathnames
                            "apikeys"
                            (progn
                              (uiop:ensure-all-directories-exist (list dir))
                              dir))))

(defmethod marshal:class-persistent-slots ((self apikey))
  '(identifier expiry))

(defun %persist-store (backend)
  (with-open-file (out (filepath backend) :direction :output
                                          :if-exists :supersede)
    (let ((apikey-list
            ;; generate simple list of apikeys
            (loop :for apikey :being :the :hash-value :of (store backend)
                  :collect apikey)))
      (prin1 (marshal:marshal apikey-list) out))))

(defmethod store-apikey ((backend simple-file-backend) apikey)
  (setf (gethash (identifier apikey) (store backend)) apikey)
  (%persist-store backend))

(defmethod retrieve-apikey ((backend simple-file-backend) identifier)
  (gethash identifier (store backend)))

(defmethod delete-apikey ((backend simple-file-backend) identifier)
  (remhash identifier (store backend))
  (%persist-store backend))

;; ----------------------------------------
;; memory backend
;; ----------------------------------------

(defvar *memory-backend* 'memory)

;; TODO: serialize access to this. Agent?
(defvar *apikeys* (make-hash-table :test #'equal))

(defun %dump-store ()
  (maphash (lambda (k v)
             (format t "~a => ~a~%" k v))
           *apikeys*))

(defmethod store-apikey ((backend (eql 'memory)) apikey)
  (setf (gethash (identifier apikey) *apikeys*) apikey))

(defmethod retrieve-apikey ((backend (eql 'memory)) identifier)
  (gethash identifier *apikeys*))

(defmethod delete-apikey ((backend (eql 'memory)) identifier)
  (remhash identifier *apikeys*))
