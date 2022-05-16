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
  (with-mocks ()
    (answer (eta-handler:start-record) t)

    (is (eq :ok (start-record)))
    (is (= 1 (length (invocations 'eta-handler:start-record))))
  ))

(run! 'start-record--ok)
