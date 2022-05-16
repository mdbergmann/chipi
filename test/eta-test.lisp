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


(test start-record--ok
  (setf *actor-system* (asys:make-actor-system))
  (unwind-protect
       (with-mocks ()
         (answer (libserialport:serial-write-data port data)
           (progn
             5))

         (is (eq :ok (start-record)))
         (is (= 1 (length (invocations 'libserialport:serial-write-data))))
         )
    (ac:shutdown *actor-system*)))

(run! 'start-record--ok)
