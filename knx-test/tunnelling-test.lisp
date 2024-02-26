(defpackage :knx-conn.tunnelling-test
  (:use :cl :fiveam :knx-conn.tunnelling :cemi)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.tunnelling-test)

(def-suite tunnelling-tests
  :description "Tests for tunnelling request"
  :in knx-conn.tests:test-suite)

(in-suite tunnelling-tests)

(test make-tunnelling-request--default
  (let ((req (make-tunnelling-request
              :channel-id 0
              :seq-counter 0
              :cemi (make-cemi 1 2 3))))
    (is-true req)))

(run! 'tunnelling-tests)
