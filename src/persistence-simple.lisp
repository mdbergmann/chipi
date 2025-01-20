(defpackage :chipi.simple-persistence
  (:use :cl :chipi.persistence)
  (:nicknames :simple-persistence)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:import-from #:persp
                #:persistence)
  (:export #:simple-persistence
           #:make-simple-persistence))

(in-package :chipi.simple-persistence)

(defclass simple-persistence (persistence)
  ((storage-root-path :initarg :storage-root-path
                      :initform #P""
                      :reader storage-root-path))
  (:documentation "The root path where item values are stored."))

(defun make-simple-persistence (id &key storage-root-path)
  (persp::make-persistence id
                           :type
                           'simple-persistence
                           :storage-root-path
                           (uiop:ensure-directory-pathname storage-root-path)))

(defmethod initialize ((persistence simple-persistence))
  (log:info "Initializing persistence: ~a" persistence)
  (with-slots (storage-root-path) persistence
    (uiop:ensure-all-directories-exist (list storage-root-path))))

(defmethod shutdown ((persistence simple-persistence))
  (log:info "Shutting down persistence: ~a" persistence))

(defmethod persist ((persistence simple-persistence) item)
  (with-slots (storage-root-path) persistence
    (handler-case
        (let ((path (merge-pathnames (format nil "~a.store" (item:name item))
                                     storage-root-path)))
          (log:debug "Persisting to file: ~a, item: ~a" path item)
          (alexandria:with-output-to-file (stream path
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create)
            (let ((item-state (item:get-item-stateq item)))
              (jzon:stringify (item-ext:item-state-to-ht item-state)
                              :stream stream
                              :pretty t)))
          (log:debug "Persisting to file OK, item: ~a" item))
      (error (e)
        (log:warn "Error persisting item: ~a, error: ~a" item e)))))

(defmethod retrieve ((persistence simple-persistence) item)
  (with-slots (storage-root-path) persistence
    (handler-case
        (let ((path (merge-pathnames (format nil "~a.store" (item:name item))
                                     storage-root-path)))
          (log:debug "Reading from file: ~a, item: ~a" path item)
          (with-open-file (stream path)
            (let* ((ht (jzon:parse stream))
                   (item-state (item-ext:ht-to-item-state ht)))
              (make-persisted-item :value
                                   (item:item-state-value item-state)
                                   :timestamp
                                   (item:item-state-timestamp item-state)))))
      (error (e)
        (log:warn "Error retrieving item: ~a, error: ~a" item e)
        `(:error . ,e)))))
