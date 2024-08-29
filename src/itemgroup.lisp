(defpackage :chipi.itemgroup
  (:use :cl)
  (:nicknames :itemgroup)
  (:export #:make-itemgroup
           #:itemgroup
           #:label
           #:name
           #:add-item
           #:get-item
           #:remove-item
           #:get-value)
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

(defun name (itemgroup)
  (id itemgroup))

(defun add-item (itemgroup item)
  (setf (gethash (item:name item) (items itemgroup)) item))

(defun get-item (itemgroup id)
  (gethash (symbol-name id) (items itemgroup)))

(defun remove-item (itemgroup id)
  (remhash (symbol-name id) (items itemgroup)))

(defun get-value (itemgroup)
  (let ((items
          (loop :for item :being :the :hash-values :of (items itemgroup)
                :collect item)))
    (mapcar #'item:get-value items)))

