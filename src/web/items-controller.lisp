(defpackage :chipi-web.items-controller
  (:use :cl)
  (:nicknames :itemsc)
  (:import-from #:alexandria
                #:when-let)
  (:export #:retrieve-items
           #:retrieve-item
           #:update-item-value))

(in-package :chipi-web.items-controller)

(defun %item-to-plist (item)
  (let* ((name (item:name item))
         (label (item:label item))
         (item-state (item:get-item-stateq item))
         (item-value (item:item-state-value item-state))
         (item-timestamp (item:item-state-timestamp item-state)))
    (list :name name
          :label label
          :value item-value
          :timestamp item-timestamp)))

(defun retrieve-items ()
  "Retrieves all items as list of plists."
  (when-let ((items (hab:get-items)))
    (mapcar #'%item-to-plist items)))

(defun retrieve-item (item-id)
  (when-let ((item (hab:get-item item-id)))
    (%item-to-plist item)))

(defun update-item-value (item-id item-value))
