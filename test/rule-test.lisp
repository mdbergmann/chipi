(defpackage :cl-eta.rule-test
  (:use :cl :fiveam :cl-eta.rule)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.rule-test)

(def-suite rule-tests
  :description "Tests for rules"
  :in cl-eta.tests:test-suite)

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
                            :do (lambda () (format t "done")
                                  (setf expected t)))))
      (is-true rule)
      (is (typep rule 'rule))
      (item:set-value item 1)
      (is-true (miscutils:await-cond 0.5 expected)))))

(test make-rule--do-only-for-subscribed-item
  "Tests rule that fires event when item changed, but only for subscribed item."
  (with-fixture init-destroy-env ()
    (let* ((item (item:make-item 'item1))
           (expected)
           (rule (make-rule "test rule"
                            :when-item-change 'not-exists
                            :do (lambda () (format t "done")
                                  (setf expected t)))))
      (item:set-value item 1)
      (sleep 0.5)
      (is-false expected))))

(test make-rule--do-when-cron-is-reached
  "Tests rule that fires event when cron is reached."
  (with-fixture init-destroy-env ()
    (let* ((expected)
           (rule (make-rule "test rule"
                            :when-cron '(:boot-only t)
                            :do (lambda () (format t "done")
                                  (setf expected t)))))
      (is-true rule)
      (is (typep rule 'rule))
      (is-true (miscutils:await-cond 1 expected)))))

(run! 'rule-tests)
