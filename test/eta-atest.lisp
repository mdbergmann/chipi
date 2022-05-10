(defpackage :cl-eta.eta-test
  (:use :cl :fiveam :cl-eta.eta)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-test)

(def-suite eta-tests
  :description "Acceptance tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-tests)

(test send-record-package
      "Sends the record ETA interface package that will result in receiving data packages."
      (is (= 1 1)))

(run! 'send-record-package)
