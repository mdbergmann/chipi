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
         (uiop:delete-file-if-exists eta::*solar-state-file*)
         (eta:cron-init)
         (&body))
    (progn
      (eta:ensure-shutdown)
      (eta:cron-stop)
      (uiop:delete-file-if-exists eta::*solar-state-file*))))


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
           (eta::%persist-actor-state (eta::make-solar-state :total-wh-day 123)
                                      eta::*solar-state-file*)
           (is-true (eq :ok (solar-init)))
           (is (not (null (act-cell:state eta::*solar-actor*))))
           (is (= 123
                  (eta::solar-state-total-wh-day
                   (slot-value eta::*solar-actor* 'act-cell:state)))))
         (is-false eta::*solar-actor*))))

(test solar-initialization--deploys-cron-job
  "Tests that solar-init creates a cron jobs that at midnight retrieves total wh."
  (with-fixture destroy-finally ()
    (is-true (eq :ok (solar-init)))
    (is (= 1 (hash-table-count cron::*cron-jobs-hash*)))
    (is-true (gethash 'eta::solar-totals-daily cron::*cron-jobs-hash*))))

(test solar-retrieves-power
  "Test that solar actor retrieves power repeatedly every n (configurable) seconds."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (solar-init)

      (answer solar-if:read-power (values :ok 101.23 1234.56))
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

(test solar-post-total-day-to-openhab--with-update-internal-state
  "Tests that the total per day power is posted to openhab and state of actor is updated."
  (with-fixture destroy-finally ()
    (with-mocks ()
      (solar-init)
      (is (= 0 (eta::solar-state-total-wh-day
                (act-cell:state eta::*solar-actor*))))

      (answer solar-if:read-power (values :ok 101.23 1234.56))
      (answer (openhab:do-post "SolarPowerTotalDay" 1234) :ok)
      (act:! eta::*solar-actor* '(:read-total . nil))

      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'solar-if:read-power)))))
      (is-true (await-cond 2.0
                 (= 1 (length (invocations 'openhab:do-post)))))
      (is (= 1235 (eta::solar-state-total-wh-day
                   (act-cell:state eta::*solar-actor*))))
      (is (= 1235 (eta::solar-state-total-wh-day
                   (eta::%read-actor-state eta::*solar-state-file*)))))))

(test solar-stop
  "Tests that `stop' will stop the actor."
  (with-fixture destroy-finally ()
    (is (eq :ok (solar-init)))
    (is (eq :ok (solar-stop)))
    (is-false eta::*solar-actor*))
  (is-false eta::*solar-read-scheduler-thread*))
