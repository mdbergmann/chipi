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

;; your test code here

(test make-rule
  "Tests making a rule"
  (let ((rule (make-rule "test rule"
                         :when-cron '(:minute 0 :hour 1)
                         :when-item-change 'item1
                         :do
                         (format t "done"))))
    (is-true rule)
    ))

(run! 'rule-tests)
