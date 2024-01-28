(defpackage :chipi-web.items-controller
  (:use :cl)
  (:nicknames :itemsc)
  (:import-from #:alexandria
                #:if-let)
  (:export #:retrieve-items
           #:retrieve-item
           #:update-item-value
           #:item-to-plist))

(in-package :chipi-web.items-controller)

(defun item-to-plist (item)
  (let* ((name (item:name item))
         (label (item:label item))
         (item-state (item:get-item-stateq item))
         (item-value (item:item-state-value item-state))
         (item-timestamp (item:item-state-timestamp item-state)))
    (list :name name
          :label label
          :value (cond
                   ((eq item-value 'item:true) t)
                   ((eq item-value 'item:false) nil)
                   ((null item-value) 'cl:null)
                   (t item-value))
          :timestamp item-timestamp)))

(defun retrieve-items ()
  "Retrieves all items as list of plists."
  (if-let ((items (hab:get-items)))
    (mapcar #'item-to-plist items)
    (log:info "No items.")))

(defun %get-item-by-name (item-name)
  "This has potential for optimizations.
Maybe it's better to store the item by it's name in `hab:*items*' in the first place."
  (find-if (lambda (item)
             (string= (item:name item) item-name))
           (hab:get-items)))

(defun retrieve-item (item-name)
  (if-let ((item (%get-item-by-name item-name)))
    (item-to-plist item)
    (log:info "No item with id ~a." item-name)))

(defun update-item-value (item-name item-value)
  (if-let ((item (%get-item-by-name item-name)))
    (item:set-value item item-value)
    (log:info "No item with id ~a." item-name)))
