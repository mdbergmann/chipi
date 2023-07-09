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
  "Create a rule actor with the given NAME and KEYS.
KEYS can be: `:when-item-change', `:when-cron', `:do'.
`:when-item-change' is a list of items to subscribe to.
`:when-cron' is a cron expression. See `cl-cron' for details.
`:do' is a function that will be called when the rule is triggered.
It will be called with a single argument, an alist with either
`:item' or `:cron' as the key, depending on what triggered the rule.
`:item' will be the item that triggered the rule, and `:cron' will
be the cron expression that triggered the rule.

Example:
  (make-rule 'my-rule
             :when-item-change 'my-item
             :when-cron '(:minute 0 :hour 0)
             :do (lambda (trigger)
                   (case (car trigger)
                     (:item (log:info \"Item changed: \" (cdr trigger)))
                     (:cron (log:info \"Cron triggered: \" (cdr trigger))))))

This will create a rule that will be triggered when `my-item' changes,
or when the cron expression (:minute 0 :hour 0) is triggered.
When triggered, the rule will log a message to the info log.
"
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
