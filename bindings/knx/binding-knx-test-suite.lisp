(defpackage :chipi.binding.knx-test-suite
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :chipi.binding.knx-test-suite)

(def-suite test-suite
  :description "All catching test suite.")

(in-suite test-suite)

(test foo
  "Trivial test"
  (is (= 1 1)))
