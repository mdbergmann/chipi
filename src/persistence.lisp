(defpackage :cl-hab.persistence
  (:use :cl)
  (:nicknames :persp)
  (:import-from #:act
                #:actor
                #:!
                #:?
                #:*self*
                #:reply)
  (:export #:persistence
           #:make-persistence
           #:destroy
           #:store
           #:fetch
           ;; generic functions
           #:persist
           #:retrieve
           #:initialize
           #:shutdown)
  )

(in-package :cl-hab.persistence)

(defclass persistence (actor) ()
  (:documentation "This persistence just writes the value of an actor to file."))

(defclass simple-persistence (persistence)
  ((storage-root-path :initform #P""
                      :documentation "The root path where item values are stored.")))

(defgeneric initialize (persistence)
  (:documentation "Initializes the persistence."))
(defmethod initialize ((persistence simple-persistence))
  (log:debug "Initializing persistence: ~a" persistence)
  (with-slots (storage-root-path) persistence
    (uiop:ensure-all-directories-exist (list storage-root-path))))

(defgeneric shutdown (persistence)
  (:documentation "Shuts down the persistence."))
(defmethod shutdown ((persistence simple-persistence))
  (log:debug "Shutting down persistence: ~a" persistence))

(defgeneric persist (persistence item value)
  (:documentation "Stores the value of an item to file."))
(defmethod persist ((persistence simple-persistence) item value)
  (with-slots (storage-root-path) persistence
    (let ((path (merge-pathnames (format nil "~a.store" (act-cell:name item))
                                 storage-root-path)))
      (log:debug "Persisting to file: ~a, item: ~a" path item)
      (with-open-file (stream path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (print value stream)))))

(defgeneric retrieve (persistence item)
  (:documentation "Fetches the value of an item from file."))
(defmethod retrieve ((persistence simple-persistence) item)
  (with-slots (storage-root-path) persistence
    (let ((path (merge-pathnames (format nil "~a.store" (act-cell:name item))
                                 storage-root-path)))
      (log:debug "Reading from file: ~a, item: ~a" path item)
      (with-open-file (stream path)
        (read stream)))))

;; ---------------------------------------------------------------------------

(defun make-persistence (id &rest other-args &key type &allow-other-keys)
  (let ((isys (isys:ensure-isys))
        (type (ccase type
                (:simple 'simple-persistence))))
    (ac:actor-of isys
                 :name id
                 :type type
                 :receive (lambda (msg)
                            (log:debug "Received: ~a, msg: ~a" (car msg) msg)
                            (case (car msg)
                              (:store (persist *self* (cadr msg) (cddr msg)))
                              (:fetch (reply (retrieve *self* (cdr msg))))))
                 :init (lambda (self)
                         (typecase self
                           (simple-persistence
                            (setf (slot-value self 'storage-root-path)
                                  (uiop:ensure-directory-pathname
                                   (getf other-args :storage-root-path #P"")))))
                         (initialize self))
                 :destroy (lambda (self)
                            (shutdown self)))))

(defun store (persistence item)
  (future:fcompleted
      (item:get-value item)
      (result)
    (! persistence `(:store . (,item . ,result))))
  t)

(defun fetch (persistence item)
  (? persistence `(:fetch . ,item)))

(defun destroy (persistence)
  (ac:stop (act:context persistence) persistence :wait t))
