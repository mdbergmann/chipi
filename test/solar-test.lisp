(defpackage :cl-eta.solar-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.solar-test)

(def-suite solar-tests
  :description "Reading solar panel power tests"
  :in cl-eta.tests:test-suite)

(in-suite solar-tests)

(def-fixture destroy-finally ()
  (unwind-protect
       (progn
         (eta:cron-init)
         (&body))
    (progn
      (eta:ensure-shutdown)
      (eta:cron-stop))))


(test solar-initialization
  "Test that solar actor is up and running."
  (with-fixture destroy-finally ()
    (is-true (eq :ok (solar-init)))
    (is-true eta::*solar-actor*)
    (is (not (null (act-cell:state eta::*solar-actor*)))))
  (is-false eta::*solar-actor*))

(test solar-initialization--init-from-file-state
  "Test that solar actor is up and running."
  (unwind-protect
       (progn
         (with-fixture destroy-finally ()
           (setf eta::*solar-state-file* #P"test-state")
           (eta::%store-state (eta::make-solar-state :total-wh 123)
                              #P"test-state")
           (is-true (eq :ok (solar-init)))
           (is (not (null (act-cell:state eta::*solar-actor*))))
           (is (= 123
                  (eta::solar-state-total-wh (slot-value eta::*solar-actor* 'act-cell:state)))))
         (is-false eta::*solar-actor*))
    (uiop:delete-file-if-exists #P"test-state")))

(test solar-retrieves-power
  "Test that solar actor retrieves power repeatedly every n (configurable) seconds."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (solar-init)

      (answer solar-if:read-power (values :ok 101.23))
      (answer (openhab:do-post "SolarPowerMom" 101.23) :ok)

      (setf eta:*solar-read-delay-sec* 0.3)
      (is (eq :ok (solar-start-read)))
      (is-true (await-cond 4.0
                 (>= (length (invocations 'solar-if:read-power)) 2)))))
  (is-false eta::*solar-read-scheduler-thread*))

(test solar-post-raw-retrieved-to-openhab
  "Tests that the raw power value is posted to openhab."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (solar-init)

      (answer solar-if:read-power (values :ok 101.23))
      (answer (openhab:do-post "SolarPowerMom" 101.23) :ok)
      (setf eta:*solar-read-delay-sec* 10)
      (solar-start-read)

      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'solar-if:read-power)))))
      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'openhab:do-post))))))))

(test solar-stop
  "Tests that `stop' will stop the actor."
  (with-fixture destroy-finally ()
    (is (eq :ok (solar-init)))
    (is (eq :ok (solar-stop)))
    (is-false eta::*solar-actor*))
  (is-false eta::*solar-read-scheduler-thread*))
