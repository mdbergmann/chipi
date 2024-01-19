(defpackage :chipi-web.user-store-test
  (:use :cl :fiveam :chipi-web.user-store)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.user-store-test)

(def-suite user-store-tests
  :description "Tests for user storage."
  :in chipi-web.tests:test-suite)

(in-suite user-store-tests)

