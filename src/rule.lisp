(defpackage :chipi.rule
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

(in-package :chipi.rule)

(defvar %any-value (gensym "ANY-VALUE")
  "Sentinel for 'match any value' in transition specs.
Needed because NIL is a valid item value.")

(defun %parse-transition-spec (spec)
  "Parse a transition spec like (ITEM-SYM :from VAL :to VAL) into a plist
(:item-name \"ITEM-SYM\" :from val-or-any :to val-or-any)."
  (let* ((item-sym (first spec))
         (rest (rest spec))
         (from-present (member :from rest))
         (to-present (member :to rest))
         (from-val (if from-present (second from-present) %any-value))
         (to-val (if to-present (second to-present) %any-value)))
    (list :item-name (symbol-name item-sym)
          :from from-val
          :to to-val)))

(defun %transition-matches-p (spec old-value new-value)
  "Return T if OLD-VALUE and NEW-VALUE match the transition SPEC's :from/:to."
  (let ((from-val (getf spec :from))
        (to-val (getf spec :to)))
    (and (or (eq from-val %any-value)
             (equal from-val old-value))
         (or (eq to-val %any-value)
             (equal to-val new-value)))))

(defclass rule (actor)
  ((cron-hashes :initform nil
                :documentation "A list of cron hashes for this rule.")))

(defun make-rule (name &rest keys)
  "Create a rule actor with the given NAME and KEYS.
KEYS can be: `:when-item-change', `:when-item-transition', `:when-cron', `:do'.
`:when-item-change' is an item to subscribe to for any value change.
`:when-item-transition' is a list of (ITEM-SYM :from VAL :to VAL) specifying
  a conditional item change. The rule only fires when old/new values match.
  Either :from or :to (or both) can be specified. Omitted means 'any value'.
`:when-cron' is a cron expression. See `cl-cron' for details.
All of `:when-item-change', `:when-item-transition' and `:when-cron' can be
specified multiple times.
`:do' is a function that will be called when the rule is triggered.
It will be called with a single argument, an alist with either
`:item' or `:cron' as the key, depending on what triggered the rule.
`:item' will have an `item:item-changed-event' struct as value, containing
the item and the old-value before the change. `:cron' will be the cron
expression that triggered the rule.

If both `:when-item-change' and `:when-item-transition' match the same item
change, `:do' fires only once (transition match takes precedence).

Example:
  (make-rule \"my-rule\"
             :when-item-change 'my-item
             :when-item-transition '(my-light :from \"false\" :to \"true\")
             :when-cron '(:minute 0 :hour 0)
             :do (lambda (trigger)
                   (case (car trigger)
                     (:item
                      (let* ((event (cdr trigger))
                             (item (item:item-changed-event-item event))
                             (old-value (item:item-changed-event-old-value event)))
                        (log:info \"Item changed: ~a, old: ~a\" item old-value)))
                     (:cron (log:info \"Cron triggered: \" (cdr trigger))))))
"
  (log:info "Creating rule '~a'." name)
  (let* ((isys (ensure-isys))
         (item-changes (loop :for (key val) :on keys :by #'cddr
                             :if (eq key :when-item-change)
                               :collect val))
         (transitions (loop :for (key val) :on keys :by #'cddr
                            :if (eq key :when-item-transition)
                              :collect (%parse-transition-spec val)))
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
                (%receive-fun msg name do-fun item-names transitions))
     :init (lambda (self)
             (when (or (car item-changes) (car transitions))
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
                (dolist (cron-hash (slot-value self 'cron-hashes))
                  (cr:cancel-job cron-hash))
                (setf (slot-value self 'cron-hashes) nil)
                (log:info "Rule '~a' destroyed." name)))))

(defun %receive-fun (msg name do-fun item-names transitions)
  (log:debug "Received msg: ~a at rule: ~a" msg name)
  (when do-fun
    (when (typep msg 'item:item-changed-event)
      (let* ((item (item:item-changed-event-item msg))
             (item-name (act-cell:name item))
             (old-value (item:item-changed-event-old-value msg))
             (new-value (item:item-state-value (item:get-item-stateq item)))
             (transition-matched-p nil))
        ;; Check transitions first (they take precedence for dedup)
        (dolist (spec transitions)
          (when (and (equal item-name (getf spec :item-name))
                     (%transition-matches-p spec old-value new-value))
            (setf transition-matched-p t)
            (funcall do-fun `(:item . ,msg))))
        ;; Check plain item-change, skip if already matched by transition
        (when (and (not transition-matched-p)
                   (member item-name item-names :test #'equal))
          (funcall do-fun `(:item . ,msg)))))
    (when (and (listp msg)
               (eq (car msg) 'cron-triggered))
      (funcall do-fun `(:cron . ,(second msg))))))

(defun %add-cron-hash (rule hash)
  "Add the given `HASH' to the list of cron hashes for the given `RULE'."
  (push hash (slot-value rule 'cron-hashes)))

(defun destroy (rule)
  "Destroy the given `RULE'."
  (ac:stop (act:context rule) rule :wait t))
