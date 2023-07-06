(defpackage :cl-eta.rule
  (:use :cl)
  (:nicknames :rule)
  (:import-from #:envi
                #:ensure-isys)
  (:import-from #:act
                #:actor)
  (:export #:rule
           #:make-rule
           ))

(in-package :cl-eta.rule)

(defclass rule (actor) ())

(defun make-rule (name &rest keys)
  (let* ((isys (ensure-isys))
         (item-changes (loop :for (key val) :on keys :by #'cddr
                             :if (eq key :when-item-change)
                               :collect val))
         (item-names (mapcar #'symbol-name item-changes))
         (do-fun (getf keys :do))
         (rule (ac:actor-of
                isys
                :name name
                :type 'rule
                :receive (lambda (msg)
                           (log:debug "Received msg: " msg)
                           (when (and
                                  do-fun
                                  (typep msg 'item:item-changed-event))
                             (let ((item-name (act-cell:name
                                               (item:item-changed-event-item msg))))
                               (when (member item-name
                                             item-names
                                             :test #'equal)
                                 (funcall do-fun)))))
                :init (lambda (self)
                        (loop :for item :in item-changes
                              :do (ev:subscribe self self 'item:item-changed-event)))
                        )))
    rule))
