(defpackage :chipi-api.tests
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :chipi-api.tests)

(def-suite test-suite
  :description "All catching test suite.")

(in-suite test-suite)

(test foo
  "Trivial test"
  (is (= 1 1)))
