(defpackage :chipi.item-ext
  (:use :cl :item)
  (:nicknames :item-ext)
  (:import-from #:alexandria
                #:plist-hash-table
                #:alist-hash-table)
  (:export #:item-value-ext-to-internal
           #:item-value-internal-to-ext
           #:item-state-to-ht
           #:ht-to-item-state
           #:item-to-ht))

(in-package :chipi.item-ext)

(defun %convert-table (table converter)
  (let ((converted (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (key value)
               (setf (gethash key converted)
                     (funcall converter value)))
             table)
    converted))

(defun item-value-ext-to-internal (ext-value)
  "Converts externally delivered (read) ext-value (`item-state-value') to internal representation
Used e.g. for API or persistences."
  (cond
    ((hash-table-p ext-value)
     (%convert-table ext-value
                     (lambda (value)
                       ;; Keep precision until SET-VALUE checks and normalizes table fields.
                       (if (numberp value) value (item-value-ext-to-internal value)))))
    ((typep ext-value 'double-float)
     (coerce ext-value 'single-float))
    ((eq ext-value t) 'item:true)
    ((eq ext-value nil) 'item:false)
    ((eq ext-value 'cl:null) nil)
    (t ext-value)))

(defun item-value-internal-to-ext (item-value)
  "Converts internal item-value (`item-state-value') to external representation, i.e. used for JSON."
  (cond
    ((hash-table-p item-value)
     (%convert-table item-value #'item-value-internal-to-ext))
    ((eq item-value 'item:true) t)
    ((eq item-value 'item:false) nil)
    ((null item-value) 'cl:null)
    (t item-value)))

(defun item-state-to-ht (item-state)
  "Converts item-state to a hash-table with values converted ready for serialization."
  (plist-hash-table
   (list "timestamp" (local-time:timestamp-to-unix
                      (local-time:universal-to-timestamp (item-state-timestamp item-state)))
         "value" (item-value-internal-to-ext (item-state-value item-state)))
   :test #'equal))

(defun ht-to-item-state (ht)
  "Generates `item-state' from values in hash-table. Values are converted back to internal use."
  (make-item-state :timestamp (local-time:timestamp-to-universal
                               (local-time:unix-to-timestamp (gethash "timestamp" ht)))
                   :value (item-value-ext-to-internal (gethash "value" ht))))

(defun item-to-ht (item)
  (let ((type-hint (value-type-hint item))
        (tags (tags item)))
    (plist-hash-table
     (list "name"       (name item)
           "label"      (label item)
           "type-hint"  (if type-hint (symbol-name type-hint) 'cl:null)
           "tags"       (if tags 
                           (alist-hash-table tags :test #'equal)
                           (make-hash-table :test #'equal))  ; Empty hash table for empty tags
           "item-state" (item-state-to-ht (get-item-stateq item)))
     :test #'equal)))
