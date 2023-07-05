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

(def-fixture init-destroy-isys ()
  (unwind-protect
       (progn 
         (&body))
    (envi:shutdown-env)))

(test make-rule--when-item-changed
  "Tests rule that fires event when item value changed."
  (with-fixture init-destroy-isys ()
    (let* ((item (item:make-item 'item1))
           (expected)
           (rule (make-rule "test rule"
                            :when-item-change 'item1
                            :do (lambda () (format t "done")
                                  (setf expected t)))))
      (is-true rule)
      (is (typep rule 'rule))
      (item:set-value item 1)
      (is-true (miscutils:await-cond 0.5
                 (eq expected t)))
      )))

(run! 'rule-tests)
