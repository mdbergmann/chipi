(defpackage :cl-eta.avgs-test
  (:use :cl :fiveam :cl-eta.avgs)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.avgs-test)

(def-suite avgs-tests
  :description "Avgs management tests"
  :in cl-eta.tests:test-suite)

(in-suite avgs-tests)

;; your test code here

(test is-due
  "Tests marking avgs as due."
  (is-false (due-p 0 0))
  )
