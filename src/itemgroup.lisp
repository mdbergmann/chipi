(defpackage :chipi.itemgroup
  (:use :cl)
  (:nicknames :itemgroup)
  (:import-from #:alexandria
                #:hash-table-values)
  (:export #:make-itemgroup
           #:itemgroup
           #:label
           #:name
           #:add-item
           #:get-item
           #:remove-item
           #:get-items
           #:get-value
           #:set-value))

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

(defun get-items (itemgroup)
  (hash-table-values (items itemgroup)))

(defun get-value (itemgroup)
  "Collects values (as futures) from all added items."
  (let ((items (hash-table-values (items itemgroup))))
    (mapcar #'item:get-value items)))

(defun set-value (itemgroup value &key (push t) timestamp (persist t))
  "Sets `value' to all added items.
Parameters mimick the `item:set-value' interface."
  (maphash (lambda (key item)
             (declare (ignore key))
             (item:set-value item value :push push :timestamp timestamp :persist persist))
           (items itemgroup))
  t)
