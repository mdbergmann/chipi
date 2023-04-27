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

(def-fixture destroy-finally ()
  (unwind-protect
       (&body)
    (eta:ensure-shutdown)))


(test ina-initialization
  "Test that ina actor is up and running.
Actor should call ina219 initialization."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (answer ina219-if:init (values :ok))
      
      (is-true (eq :ok (ina-init)))
      (is-true eta::*ina-actor*)
      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'ina219-if:init)))))))
  (is-false eta::*ina-actor*))

(test ina-retrieves-currency
  "Test that ina actor retrieves currency repeatedly every n (configurable) seconds."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (answer ina219-if:init (values :ok))
      (ina-init)

      (answer ina219-if:read-currency (values :ok 1.23))
      (answer (openhab:do-post "ZistSensorCurrency" 1.23) :ok)

      (setf eta:*ina-read-delay-sec* 0.3)
      (is (eq :ok (ina-start-read)))
      (is-true (await-cond 4.0
                 (>= (length (invocations 'ina219-if:read-currency)) 2)))))
  (is-false eta::*ina-read-scheduler-thread*))

(test ina-post-raw-retrieved-to-openhab
  "Tests that the raw currency value is posted to openhab."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (answer ina219-if:init (values :ok))
      (ina-init)

      (answer ina219-if:read-currency (values :ok 1.23))
      (answer (openhab:do-post "ZistSensorCurrency" 1.23) :ok)
      (setf eta:*ina-read-delay-sec* 10)
      (ina-start-read)

      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'ina219-if:read-currency)))))
      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'openhab:do-post)))))
      )))

(test ina-stop
  "Tests that the ina actor is stopped."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (answer ina219-if:init (values :ok))
      (is (eq :ok (ina-init)))
      (is (eq :ok (ina-stop)))
      (is-false eta::*ina-actor*)))
  (is-false eta::*ina-read-scheduler-thread*))
