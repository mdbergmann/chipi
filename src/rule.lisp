(defpackage :cl-hab.rule
  (:use :cl)
  (:nicknames :rule)
  (:import-from #:isys
                #:ensure-isys)
  (:import-from #:act
                #:actor
                #:!)
  (:export #:rule
           #:make-rule
           #:destroy))

(in-package :cl-hab.rule)

(defclass rule (actor)
  ((cron-hashes :initform nil
                :documentation "A list of cron hashes for this rule.")))

(defun make-rule (name &rest keys)
  "Create a rule actor with the given NAME and KEYS.
KEYS can be: `:when-item-change', `:when-cron', `:do'.
`:when-item-change' is an item to subscribe to.
`:when-cron' is a cron expression. See `cl-cron' for details.
Both `:when-item-change' and `:when-cron' can be specified multiple times.
`:do' is a function that will be called when the rule is triggered.
It will be called with a single argument, an alist with either
`:item' or `:cron' as the key, depending on what triggered the rule.
`:item' will be the item that triggered the rule, and `:cron' will
be the cron expression that triggered the rule.

Example:
  (make-rule \"my-rule\"
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
  (log:info "Creating rule '~a'." name)
  (let* ((isys (ensure-isys))
         (item-changes (loop :for (key val) :on keys :by #'cddr
                             :if (eq key :when-item-change)
                               :collect val))
         (crons (loop :for (key val) :on keys :by #'cddr
                      :if (eq key :when-cron)
                        :collect val))
         (item-names (mapcar #'symbol-name item-changes))
         (do-fun (getf keys :do)))
    (ac:actor-of
     isys
     :name name
     :type 'rule
     :receive (lambda (msg)
                (log:debug "Received msg: ~a at rule: ~a" msg name)
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
                     :for boot-only-p = (getf cron :boot-only nil)
                     :for cron-hash = (cr:make-cron-job
                                       (lambda ()
                                         (when boot-only-p
                                           (log:info "Boot-only cron '~a' triggered." name)
                                           (sleep 2) ; let rule initialize first
                                           )
                                         (! self `(cron-triggered ,cron)))
                                       cron)
                     :do (unless boot-only-p
                           (%add-cron-hash self cron-hash))))
             (log:info "Rule '~a' initialized." name))
     :destroy (lambda (self)
                (ev:unsubscribe self self)
                (loop :for cron-hash :in (slot-value self 'cron-hashes)
                      :do (cr:cancel-job cron-hash))
                (setf (slot-value self 'cron-hashes) nil)
                (log:info "Rule '~a' destroyed." name)))))

(defun %add-cron-hash (rule hash)
  "Add the given `HASH' to the list of cron hashes for the given `RULE'."
  (push hash (slot-value rule 'cron-hashes)))

(defun destroy (rule)
  "Destroy the given `RULE'."
  (ac:stop (act:context rule) rule :wait t))
