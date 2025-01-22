(defpackage :chipi.item-ext
  (:use :cl :item)
  (:nicknames :item-ext)
  (:export #:item-state-to-ht
           #:ht-to-item-state
           #:item-to-ht))

(in-package :chipi.item-ext)

(defun item-state-to-ht (item-state)
  "Converts item-state to a hash-table with values converted ready for serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "timestamp" ht) (item-state-timestamp item-state))
    (setf (gethash "value" ht)
          (let ((item-value (item-state-value item-state)))
            (cond
              ((eq item-value 'item:true) t)
              ((eq item-value 'item:false) nil)
              ((null item-value) 'cl:null)
              (t item-value))))
    ht))

(defun ht-to-item-state (ht)
  "Generates `item-state' from values in hash-table. Values are converted back to internal use."
  (let ((item-state (make-item-state)))
    (setf (item-state-timestamp item-state) (gethash "timestamp" ht))
    (setf (item-state-value item-state)
          (let ((ht-value (gethash "value" ht)))
            (cond
              ((eq ht-value t) 'item:true)
              ((eq ht-value nil) 'item:false)
              ((eq ht-value 'cl:null) nil)
              (t ht-value))))
    item-state))

(defun item-to-ht (item)
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (name item))
    (setf (gethash "label" ht) (label item))
    (setf (gethash "item-state" ht) (item-state-to-ht (get-item-stateq item)))
    ht
  ))
