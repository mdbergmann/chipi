(defpackage :chipi-web.apikey-store
  (:use :cl)
  (:nicknames :apikey-store)
  (:import-from #:local-time-duration
                #:duration
                #:duration-as)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:create-apikey
           #:revoke-apikey
           #:exists-apikey-p
           #:has-access-rights-p
           #:apikey-p
           #:signed-apikey-p
           #:*apikey-life-time-duration*
           #:*apikey-store-backend*
           #:make-simple-file-backend
           #:*memory-backend*
           ;; conditions
           #:apikey-store-error
           #:apikey-invalid-error
           #:apikey-invalid-sig-error
           #:access-rights-error)
  )

(in-package :chipi-web.apikey-store)

(defvar *apikey-store-backend* nil)

(defvar *default-apikey-life-time-duration* (duration :day 100))
(defvar *apikey-life-time-duration* *default-apikey-life-time-duration*
  "The life time of a apikey as `ltd:duration' object.")

(defvar *sign-key* (cryp:make-random-data 20)
  "The key used to sign apikeys.
This key is stored and loaded from the file `sign-key' in the runtime directory.")

;; ----------------------------------------
;; conditions
;; ----------------------------------------

(define-condition apikey-store-error (simple-condition)()
  (:report (lambda (condition stream)
             (format stream "API key store error: ~a"
                     (simple-condition-format-control condition)))))

(define-condition apikey-invalid-error (apikey-store-error)()
  (:default-initargs :format-control "Invalid API key structure"))

(define-condition apikey-invalid-sig-error (apikey-store-error)()
  (:default-initargs :format-control "Invalid API key signature"))

(define-condition access-rights-error (apikey-store-error)())

;; ----------------------------------------
;; types
;; ----------------------------------------

(defclass apikey ()
  ((identifier :initarg :identifier
               :initform (error "identifier is required")
               :reader identifier)
   (expiry :initarg :expiry
           :initform (+ (get-universal-time)
                        (duration-as *apikey-life-time-duration* :sec))
           :reader expiry
           :documentation "The universal-time when the apikey expires.")
   (access-rights :initarg :access-rights
                  :initform '()
                  :reader access-rights
                  :documentation "The access rights of the apikey.")))

;; ----------------------------------------
;; helper functions
;; ----------------------------------------

(defun %new-random-id ()
  (cryp:make-random-string 20 :uri t))

(defun %sign-apikey-id (apikey)
  (let* ((apikey-id (identifier apikey))
         (sig (cryp:hmac-sign *sign-key* apikey-id)))
    (format nil "~a.~a" apikey-id sig)))

(defun %make-signed-apikey (&key (access-rights '()))
  "Creates a new unstored apikey and returns values: apikey instance and signed identifier."
  (let ((apikey (make-instance 'apikey
                               :identifier (%new-random-id)
                               :access-rights access-rights)))
    (values apikey (%sign-apikey-id apikey))))

(defun %destructure-apikey-id (identifier)
  "Checks the apikey id structure and return plain-id and sign."
  (handler-case 
      (destructuring-bind (plain-id sign)
          (str:split #\. identifier)
        (values plain-id sign))
    (error ()
      (error 'apikey-invalid-error))))

(defun %verify-apikey-sig (identifier)
  "Checks the apikey signature and returns the plain-id if the signature is valid."
  (multiple-value-bind (plain-id sign)
      (%destructure-apikey-id identifier)
    (when (and plain-id sign)
      (unless (cryp:equal-string-p
               sign (cryp:hmac-sign *sign-key* plain-id))
        (error 'apikey-invalid-sig-error))
      plain-id)))

(defun apikey-p (obj)
  (typep obj 'apikey))

(defun signed-apikey-p (identifier)
  (and (stringp identifier)
       (%destructure-apikey-id identifier)))

(deftype signed-apikey-identifier ()
  `(satisfies signed-apikey-p))

;; ----------------------------------------
;; public interface
;; ----------------------------------------

(defun create-apikey (&key (access-rights '()))
  "Creates a new apikey and stores it in the backend.
Returns signed identifier."
  (multiple-value-bind (apikey signed-id)
      (%make-signed-apikey :access-rights access-rights)
    (store-apikey *apikey-store-backend* apikey)
    signed-id))
    
(defun revoke-apikey (identifier)
  "Revokes the apikey with the given identifier.
The identifier must be a signed apikey identifier.
If the signature is invalid, an `akikey-invalid-sig-error' is signaled."
  (check-type identifier string)
  (let ((plain-id (%verify-apikey-sig identifier)))
    (delete-apikey *apikey-store-backend* plain-id)))

(defun exists-apikey-p (identifier)
  "Checks if the apikey identifier exists and in the store, is valid and not expired.
Returns `T' if the apikey with the given identifier exists and is valid.
Returns `NIL' if the apikey with the given identifier does not exist.
If the signature is invalid, an `akikey-invalid-sig-error' is signaled.
If the apikey is expired, it is revoked and `NIL' is returned."
  (check-type identifier string)
  (when-let ((apikey (retrieve-apikey identifier)))
    (if (expired-apikey-p identifier)
        (progn
          (log:info "API key expired")
          nil)
        t)))

;; ----------------------------------------
;; access rights
;; ----------------------------------------

(defvar *access-rights* '((:read . 1)
                          (:update . 5)
                          (:delete . 10)
                          (:admin . 100)))

(defun access-right-value (right)
  (if-let ((value (cdr (assoc right *access-rights*))))
    value
    0))


(defun has-access-rights-p (identifier access-rights)
  "Returns `T' if the apikey with the given identifier has the given access-rights, `NIL' otherwise."
  (check-type identifier string)
  (check-type access-rights list)
  (when (not (car access-rights))
    (error 'access-rights-error :format-controll "No access rights requested"))
  (when-let ((apikey (retrieve-apikey identifier)))
    (let ((key-rights (access-rights apikey)))
      (when (not (car key-rights))
        (return-from has-access-rights-p nil))
      (let ((max-key-right (reduce #'max
                                   (mapcar #'access-right-value
                                           key-rights)))
            (max-requested-right (reduce #'max
                                         (mapcar #'access-right-value
                                                 access-rights))))
        (>= max-key-right max-requested-right)))))

;; ----------------------------------------
;; private interface
;; ----------------------------------------

(defun retrieve-apikey (identifier)
  "Retrieves the apikey with the given identifier.
Returns nil if no apikey with the given identifier exists.
The identifier must be a signed apikey identifier.
If the signature is invalid, nil is returned and the incident logged."
  (check-type identifier string)
  (let ((plain-id (%verify-apikey-sig identifier)))
    (load-apikey *apikey-store-backend* plain-id)))

(defun expired-apikey-p (identifier)
  "Returns `T' if the apikey with the given identifier is expired, `NIL' otherwise.
If the apikey is expired, it is implicitly revoked."
  (check-type identifier string)
  (let ((apikey (retrieve-apikey identifier)))
    (unless apikey
      (error "API key does not exist"))
    (if (< (expiry apikey)
           (get-universal-time))
        (progn
          (delete-apikey *apikey-store-backend* (identifier apikey))
          t)
        nil)))

(defun retrieve-expired-apikeys ()
  "Retrieves all expired apikeys as a list of signed identifiers."
  (mapcar (lambda (apikey)
            (%sign-apikey-id apikey))
          (retrieve-with-filter *apikey-store-backend*
                                (lambda (apikey)
                                  (< (expiry apikey)
                                     (get-universal-time))))))

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
