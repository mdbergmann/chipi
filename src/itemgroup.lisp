(defpackage :chipi.itemgroup
  (:use :cl)
  (:nicknames :itemgroup)
  (:export #:make-itemgroup
           #:itemgroup
           #:label
           #:name
           #:add-item
           #:get-item)
  (:import-from #:item
                #:name
                #:label)
  )

(in-package :chipi.itemgroup)

(defclass itemgroup ()
  ((id :initarg :id
       :initform nil
       :reader id
       :documentation "The identifier of the itemgroup. Should be a symbol for easier lookup.")
   (label :initarg :label
          :initform nil
          :reader label
          :documentation "A displayable name of the itemgroup.")
   (items :initform (make-hash-table :test #'equal)
          :reader items)))

(defun make-itemgroup (id &key (label nil))
  (check-type id symbol)
  (log:info "Creating itemgroup: ~a, label: ~a" id label)
  (make-instance 'itemgroup
                 :id id
                 :label label))

(defmethod name ((group itemgroup))
  (id group))

(defun add-item (itemgroup item)
  (setf (gethash (name item) (items itemgroup)) item))

(defun get-item (itemgroup id)
  (gethash (symbol-name id) (items itemgroup)))

