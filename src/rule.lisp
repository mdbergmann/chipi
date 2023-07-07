(defpackage :cl-hab.rule
  (:use :cl)
  (:nicknames :rule)
  (:import-from #:envi
                #:ensure-isys
                #:ensure-cron)
  (:import-from #:act
                #:actor
                #:!)
  (:export #:rule
           #:make-rule))

(in-package :cl-hab.rule)

(defclass rule (actor) ())

(defun make-rule (name &rest keys)
  (let* ((isys (ensure-isys))
         (item-changes (loop :for (key val) :on keys :by #'cddr
                             :if (eq key :when-item-change)
                               :collect val))
         (crons (loop :for (key val) :on keys :by #'cddr
                      :if (eq key :when-cron)
                        :collect val))
         (item-names (mapcar #'symbol-name item-changes))
         (do-fun (getf keys :do)))
    (ensure-cron)
    (ac:actor-of
     isys
     :name name
     :type 'rule
     :receive (lambda (msg)
                (log:debug "Received msg: " msg)
                (when do-fun
                  (when (typep msg 'item:item-changed-event)
                    (let* ((item (item:item-changed-event-item msg))
                           (item-name (act-cell:name item)))
                      (when (member item-name
                                    item-names
                                    :test #'equal)
                        (funcall do-fun `(:item . ,item)))))
                  (when (and (listp msg)
                             (eq (car msg) 'cron-triggered))
                    (funcall do-fun `(:cron . ,(second msg))))))
     :init (lambda (self)
             (when (car item-changes)
               (ev:subscribe self self 'item:item-changed-event))
             (when (car crons)
               (loop :for cron :in crons
                     :do (cron:make-cron-job
                          (lambda ()
                            (! self `(cron-triggered ,cron)))
                          :minute (getf cron :minute :every)
                          :hour (getf cron :hour :every)
                          :day-of-month (getf cron :day-of-month :every)
                          :month (getf cron :month :every)
                          :day-of-week (getf cron :day-of-week :every)
                          :boot-only (getf cron :boot-only nil)))))
     :destroy (lambda (self)
                (ev:unsubscribe self self)))))
