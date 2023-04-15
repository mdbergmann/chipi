(defpackage :cl-eta.ina-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.ina-test)

(def-suite ina-tests
  :description "INA219 currency digitizer tests"
  :in cl-eta.tests:test-suite)

(in-suite ina-tests)

(test foo
  (is-true t))

(test ina-initialization
  "Test that ina actor is up and running.
Actor should call ina219 initialization."
  (is-true (eq :ok (ina-init)))
  (is-true eta::*ina-actor*)
  )

(run! 'ina-tests)
