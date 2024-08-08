(defpackage :chipi.binding.test-suite
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :chipi.binding.test-suite)

(def-suite test-suite
  :description "All catching test suite for bindings.")

(in-suite test-suite)
