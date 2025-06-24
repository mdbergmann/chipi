(defpackage :chipi-api.items-controller
  (:use :cl)
  (:nicknames :itemsc)
  (:import-from #:alexandria
                #:if-let)
  (:export #:retrieve-items
           #:retrieve-item
           #:update-item-value))

(in-package :chipi-api.items-controller)

(defun retrieve-items ()
  "Retrieves all items as list of plists."
  (if-let ((items (hab:get-items)))
    (mapcar #'item-ext:item-to-ht items)
    (log:info "No items.")))

(defun %get-item-by-name (item-name)
  "This has potential for optimizations.
Maybe it's better to store the item by it's name in `hab:*items*' in the first place."
  (find-if (lambda (item)
             (string= (item:name item) item-name))
           (hab:get-items)))

(defun retrieve-item (item-name)
  (if-let ((item (%get-item-by-name item-name)))
    (item-ext:item-to-ht item)
    (log:info "No item with id ~a." item-name)))

(defun update-item-value (item-name item-value)
  (if-let ((item (%get-item-by-name item-name)))
    (item:set-value item item-value)
    (log:info "No item with id ~a." item-name)))
