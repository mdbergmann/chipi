(defpackage :cl-eta.eta-test
  (:use :cl :fiveam :cl-mock :cl-eta.eta :eta-ser-if)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-test)

(def-suite eta-tests
  :description "ETA tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-tests)

(defparameter *open-serial-called* nil)
(defparameter *write-serial-called* nil)

(defclass fake-serial-proxy (eta-ser-if:serial-proxy) ())
(defmethod eta-ser-if:open-serial ((proxy fake-serial-proxy) device)
  (assert proxy)
  (assert (string= "/dev/serial" device))
  (setf *open-serial-called* t))
(defmethod eta-ser-if:write-serial ((proxy fake-serial-proxy) port data)  
  (declare (ignore port data))
  (assert proxy)
  (setf *write-serial-called* 5))

(def-fixture init-destroy ()
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (change-class eta:*serial-proxy* 'fake-serial-proxy)
         (&body))
    (eta:ensure-shutdown)))
  
(test init-serial
  (with-fixture init-destroy ()
    (is (eq :ok (init-serial "/dev/serial")))
    (is-true (utils:assert-cond
              (lambda () *open-serial-called*)
              1.0))))

(test start-record--serial-written
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (= 5 *write-serial-called*))
              1.0))))

(run! 'init-serial)
(run! 'start-record--serial-written)
