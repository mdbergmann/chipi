(defpackage :chipi-web.items-controller
  (:use :cl)
  (:nicknames :itemsc)
  (:import-from #:alexandria
                #:when-let)
  (:export #:retrieve-items
           #:retrieve-item
           #:update-item-value))

(in-package :chipi-web.items-controller)

(defun retrieve-items ()
  "Retrieves all items as list of plists."
  (when-let ((items (hab:get-items)))
    (mapcar (lambda (item)
              (let* ((name (item:name item))
                     (label (item:label item))
                     (item-state (item:get-item-stateq item))
                     (item-value (item:item-state-value item-state)))
                (list :name name
                      :label label
                      :value item-value)))
            items)))

(defun retrieve-item (item-name))

(defun update-item-value (item-name item-value))
