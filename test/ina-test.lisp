(defpackage :cl-eta.ina-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.ina-test)

(def-suite ina-tests
  :description "INA219 currency digitizer tests"
  :in cl-eta.tests:test-suite)

(in-suite ina-tests)

(def-fixture destroy ()
  (unwind-protect
       (&body)
    (eta:ensure-shutdown)))

(test foo
  (is-true t))

(test ina-initialization
  "Test that ina actor is up and running.
Actor should call ina219 initialization."
  (with-fixture destroy ()

    (with-mocks ()
      (answer ina219-if:init (values :ok))
      
      (is-true (eq :ok (ina-init)))
      (is-true eta::*ina-actor*)

      (is-true (await-cond 1.0
                 (= 1 (length (invocations 'ina219-if:init))))))))



(run! 'ina-tests)
