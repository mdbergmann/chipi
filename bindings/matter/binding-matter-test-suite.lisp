(defpackage :chipi.binding.matter-test-suite
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :chipi.binding.matter-test-suite)

(def-suite test-suite
  :description "All catching test suite of the Matter binding."
  :in chipi.binding.test-suite:test-suite)

(in-suite test-suite)
