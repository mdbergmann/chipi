(defpackage :chipi-ui.tests
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:test-suite))

(in-package :chipi-ui.tests)

(def-suite test-suite
  :description "All-catching test suite for chipi-ui.")

(in-suite test-suite)
