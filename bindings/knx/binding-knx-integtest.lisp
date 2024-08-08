(defpackage :chipi.binding.knx-integtest
  (:use :cl :fiveam :cl-mock :chipi.binding.knx)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.knx-integtest)

(def-suite knx-binding-integtests
  :description "Integration tests for KNX binding"
  :in chipi.binding.knx-test-suite:test-suite)

(in-suite knx-binding-integtests)

;; create udp simulation endpoint (usocket)
;; setup knx against it
;; send bus event
