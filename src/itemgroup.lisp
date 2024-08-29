(defpackage :chipi.itemgroup
  (:use :cl)
  (:nicknames :itemgroup)
  (:export #:make-itemgroup
           #:itemgroup)
  )

(in-package :chipi.itemgroup)

(defclass itemgroup ()
  ((id :initarg :id
       :initform nil
       :reader id)
   (label :initarg :label
          :initform nil
          :reader label)))

(defun make-itemgroup (id &key (label nil))
  (log:info "Creating itemgroup: ~a, label: ~a" id label)
  (make-instance 'itemgroup
                 :id id
                 :label label))

