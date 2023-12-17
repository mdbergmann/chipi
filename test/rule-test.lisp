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
  "Tests rule that fires event when item value changed."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (expected)
           (rule (make-rule "test rule"
                            :when-item-change 'item1
                            :do (lambda (trigger)
                                  (assert (eq (car trigger) :item))
                                  (setf expected (cdr trigger))))))
      (is-true rule)
      (is (typep rule 'rule))
      (item:set-value item 1)
      (is-true (miscutils:await-cond 0.5
                 (eq expected item))))))

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
