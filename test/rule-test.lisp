(defpackage :chipi.rule-test
  (:use :cl :fiveam :chipi.rule)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.rule-test)

(def-suite rule-tests
  :description "Tests for rules"
  :in chipi.tests:test-suite)

(in-suite rule-tests)

(def-fixture init-destroy-env ()
  (unwind-protect
       (progn 
         (&body))
    (envi:shutdown-env)))

(test make-rule--do-when-item-changed
  "Tests rule that fires event when item value changed.
The trigger is (:item . event) where event is an item-changed-event struct."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-change 'item1
                            :do (lambda (trigger)
                                  (assert (eq (car trigger) :item))
                                  (setf received-event (cdr trigger))))))
      (is-true rule)
      (is (typep rule 'rule))
      (item:set-value item 1)
      (is-true (miscutils:await-cond 0.5
                 received-event))
      (is (typep received-event 'item:item-changed-event))
      (is (eq (item:item-changed-event-item received-event) item))
      (is (eq (item:item-changed-event-old-value received-event) t)))))

(test make-rule--do-only-for-subscribed-item
  "Tests rule that fires event when item changed, but only for subscribed item."
  (with-fixture init-destroy-env ()
    (let ((item (item:make-item 'item1))
          (expected))
      (make-rule "test rule"
                 :when-item-change 'not-exists
                 :do (lambda (trigger)
                       (declare (ignore trigger))
                       (setf expected t)))
      (item:set-value item 1)
      (sleep 0.5)
      (is-false expected))))

(test make-rule--do-when-cron-is-reached
  "Tests rule that fires event when cron is reached."
  (with-fixture init-destroy-env ()
    (let* ((expected)
           (rule (make-rule "test rule"
                            :when-cron '(:boot-only t)
                            :do (lambda (trigger)
                                  (format t "trigger: ~a~%" trigger)
                                  (assert (eq (car trigger) :cron))
                                  (setf expected (cdr trigger))))))
      (is-true rule)
      (is (typep rule 'rule))
      (is-true (miscutils:await-cond 3.5
                 (equalp expected '(:boot-only t)))))))

(test make-rule--do-when-item-transition-to-matches
  "Tests rule fires when value changes TO specified value."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item1 :to 1)
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (is-true rule)
      (item:set-value item 1)
      (is-true (miscutils:await-cond 0.5
                 received-event))
      (is (typep received-event 'item:item-changed-event)))))

(test make-rule--no-fire-when-transition-to-does-not-match
  "Tests rule does NOT fire on wrong :to value."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item1 :to 99)
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (declare (ignore rule))
      (item:set-value item 1)
      (sleep 0.5)
      (is-false received-event))))

(test make-rule--do-when-item-transition-from-to-matches
  "Tests rule fires when both :from and :to match."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1 :initial-value "off"))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item1 :from "off" :to "on")
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (declare (ignore rule))
      (item:set-value item "on")
      (is-true (miscutils:await-cond 0.5
                 received-event))
      (is (typep received-event 'item:item-changed-event))
      (is (equal (item:item-changed-event-old-value received-event) "off")))))

(test make-rule--no-fire-when-transition-from-does-not-match
  "Tests rule does NOT fire when :from is wrong, even if :to is right."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1 :initial-value "standby"))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item1 :from "off" :to "on")
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (declare (ignore rule))
      (item:set-value item "on")
      (sleep 0.5)
      (is-false received-event))))

(test make-rule--do-when-item-transition-from-only
  "Tests rule fires on :from match regardless of new value."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1 :initial-value "standby"))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item1 :from "standby")
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (declare (ignore rule))
      (item:set-value item "active")
      (is-true (miscutils:await-cond 0.5
                 received-event))
      (is (typep received-event 'item:item-changed-event)))))

(test make-rule--transition-does-not-fire-for-wrong-item
  "Tests transition only fires for the named item."
  (with-fixture init-destroy-env ()
    (let* ((item1 (item:make-item 'item1))
           (item2 (item:make-item 'item2))
           (received-event)
           (rule (make-rule "test rule"
                            :when-item-transition '(item2 :to 1)
                            :do (lambda (trigger)
                                  (setf received-event (cdr trigger))))))
      (declare (ignore rule))
      (item:set-value item1 1)
      (sleep 0.5)
      (is-false received-event)
      ;; Now change item2 — should fire
      (item:set-value item2 1)
      (is-true (miscutils:await-cond 0.5
                 received-event)))))

(test make-rule--transition-deduplicates-with-item-change
  "With both :when-item-change and :when-item-transition on same item, fires only once."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (fire-count 0)
           (rule (make-rule "test rule"
                            :when-item-change 'item1
                            :when-item-transition '(item1 :to 1)
                            :do (lambda (trigger)
                                  (declare (ignore trigger))
                                  (incf fire-count)))))
      (declare (ignore rule))
      (item:set-value item 1)
      (sleep 0.5)
      (is (= 1 fire-count)))))

(test rule--cancel-cron-tasks-on-destroy
  "Tests rule that cancels cron tasks on destroy."
  (with-fixture init-destroy-env ()
    (let ((rule (make-rule "test rule"
                           :when-cron '(:minute 0 :hour 0)
                           :do (lambda (trigger)
                                 (declare (ignore trigger))))))
      (is (= 1 (cr:num-jobs)))
      (destroy rule)
      (is (= 0 (cr:num-jobs)))
      )))
