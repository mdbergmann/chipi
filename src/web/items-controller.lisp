(defpackage :chipi-web.items-controller
  (:use :cl)
  (:nicknames :itemsc)
  (:import-from #:alexandria
                #:when-let)
  (:export #:retrieve-items))

(in-package :chipi-web.items-controller)

(defun retrieve-items ()
  "Retrieves all items as alist for easy json conversion."
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
