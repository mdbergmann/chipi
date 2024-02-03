(defpackage :chipi-web.apikey-store
  (:use :cl)
  (:nicknames :apikey-store)
  (:import-from #:local-time-duration
                #:duration
                #:duration-as)
  (:export #:create-apikey
           #:retrieve-apikey
           #:revoke-apikey
           #:expired-apikey-p
           #:exists-apikey-p
           #:retrieve-expired-apikeys
           #:apikey
           #:apikey-p
           #:signed-apikey-p
           #:identifier
           #:expiry
           #:*apikey-life-time-duration*
           #:make-simple-file-backend
           #:*memory-backend*
           #:*apikey-store-backend*
           ;; conditions
           #:apikey-store-error
           #:invalid-apikey-error)
  )

(in-package :chipi-web.apikey-store)

(defvar *apikey-store-backend* nil)

(defvar *default-apikey-life-time-duration* (duration :day 100))
(defvar *apikey-life-time-duration* *default-apikey-life-time-duration*
  "The life time of a apikey as `ltd:duration' object.")

(defvar *sign-key* (cryp:make-random-data 20)
  "The key used to sign apikeys.
This key is stored and loaded from the file `sign-key' in the runtime directory.")

(define-condition apikey-store-error (simple-condition)()
  (:report (lambda (c s)
             (format s "API key store error: ~a"
                     (simple-condition-format-control c)))))

(define-condition invalid-apikey-error (apikey-store-error)()
  (:default-initargs :format-control "Invalid API key structure"))

(defclass apikey ()
  ((identifier :initarg :identifier
               :initform (error "identifier is required")
               :reader identifier)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time)
                        (duration-as *apikey-life-time-duration* :sec))
           :reader expiry
           :documentation "The universal-time when the apikey expires.")))

(defun apikey-p (obj)
  (typep obj 'apikey))

(defun %new-random-id ()
  (cryp:make-random-string 20 :uri t))

(defun %sign-apikey-id (apikey)
  (let* ((apikey-id (identifier apikey))
         (sig (cryp:hmac-sign *sign-key* apikey-id)))
    (format nil "~a.~a" apikey-id sig)))

(defun %verify-apikey-id (identifier)
  "Checks the apikey id structure and return plain-id and sign."
  (handler-case 
      (destructuring-bind (plain-id sign)
          (str:split #\. identifier)
        (values plain-id sign))
    (error ()
      (log:info "Invalid API key")
      nil)))

(defun %verify-apikey-sig (identifier)
  "Checks the apikey signature and returns the plain-id if the signature is valid."
  (multiple-value-bind (plain-id sign)
      (%verify-apikey-id identifier)
    (when plain-id
      (if (cryp:equal-string-p
           sign (cryp:hmac-sign *sign-key* plain-id))
          plain-id
          (let ((err-msg "Invalid API key signature"))
            (log:info err-msg)
            err-msg)))))

(defun signed-apikey-p (identifier)
  (and (stringp identifier)
       (%verify-apikey-id identifier)))

(deftype signed-apikey-identifier ()
  `(satisfies signed-apikey-p))

(defun create-apikey ()
  "Creates a new apikey and stores it in the backend.
Returns `values': signed identifier for the apikey and error indicator."
  (let ((apikey (make-instance 'apikey
                               :identifier (%new-random-id))))
    (store-apikey *apikey-store-backend* apikey)
    (values (%sign-apikey-id apikey) nil)))
    
(defun retrieve-apikey (identifier)
  "Retrieves the apikey with the given identifier.
Returns nil if no apikey with the given identifier exists.
The identifier must be a signed apikey identifier.
If the signature is invalid, nil is returned and the incident logged."
  (check-type identifier signed-apikey-identifier)
  (let ((plain-id (%verify-apikey-sig identifier)))
    (load-apikey *apikey-store-backend* plain-id)))

(defun revoke-apikey (identifier)
  "Revokes the apikey with the given identifier.
The identifier must be a signed apikey identifier.
If the signature is invalid the incident will be logged."
  (check-type identifier signed-apikey-identifier)
  (let ((plain-id (%verify-apikey-sig identifier)))
    (delete-apikey *apikey-store-backend* plain-id)))

(defun retrieve-expired-apikeys ()
  "Retrieves all expired apikeys as a list of signed identifiers."
  (mapcar (lambda (apikey)
            (%sign-apikey-id apikey))
          (retrieve-with-filter *apikey-store-backend*
                                (lambda (apikey)
                                  (< (expiry apikey)
                                     (get-universal-time))))))

(defun expired-apikey-p (identifier)
  "Returns t if the apikey with the given identifier is expired."
  (check-type identifier signed-apikey-identifier)
  (let ((apikey (retrieve-apikey identifier)))
    (unless apikey
      (error "API key does not exist"))
    (when (< (expiry apikey)
             (get-universal-time))
      (revoke-apikey identifier))))

(defun exists-apikey-p (identifier)
  (check-type identifier signed-apikey-identifier)
  (not (null (retrieve-apikey identifier))))

;; ----------------------------------------
;; apikey store-backend
;; ----------------------------------------

(defgeneric store-apikey (backend apikey))
(defgeneric load-apikey (backend identifier))
(defgeneric delete-apikey (backend identifier))
(defgeneric retrieve-with-filter (backend filterfun))

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

(defmethod load-apikey ((backend simple-file-backend) identifier)
  (gethash identifier (store backend)))

(defmethod delete-apikey ((backend simple-file-backend) identifier)
  (remhash identifier (store backend))
  (%persist-store backend))

(defmethod retrieve-with-filter ((backend simple-file-backend) filterfun)
  (loop :for apikey :being :the :hash-value :of (store backend)
        :when (funcall filterfun apikey)
          :collect apikey))

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

(defmethod load-apikey ((backend (eql 'memory)) identifier)
  (gethash identifier *apikeys*))

(defmethod delete-apikey ((backend (eql 'memory)) identifier)
  (remhash identifier *apikeys*))
