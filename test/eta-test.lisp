(defpackage :cl-eta.eta-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-test)

(def-suite eta-tests
  :description "ETA tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-tests)


(test init-serial
  (unwind-protect
       (with-mocks ()
         (is (eq :ok (init-serial "/dev/serial")))
         (is-true (utils:assert-cond
                   (lambda ()
                     (= 1 (length (invocations 'libserialport:open-serial-port))))
                   1.0)))
    (eta:ensure-shutdown)))

(test start-record--ok
  (unwind-protect
       (with-mocks ()
         (is (eq :ok (start-record)))
         (is-true (utils:assert-cond
                   (lambda ()
                     (= 1 (length (invocations 'libserialport:serial-write-data))))
                   1.0)))
    (eta:ensure-shutdown)))

(run! 'init-serial)
(run! 'start-record--ok)
