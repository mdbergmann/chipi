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
  (cond
    ((string= "/dev/not-exists" device) (error "Can't open!"))
    (t (setf *open-serial-called* t))))
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
    (is-true *open-serial-called*)))

(test init-serial--fail-to-open
  (with-fixture init-destroy ()
    (let ((init-serial-result (multiple-value-list (init-serial "/dev/not-exists"))))
      (is (eq :fail (car init-serial-result)))
      (is (string= "Can't open!" (second init-serial-result))))))

(test start-record--serial-written
  "Tests that the write function on the serial proxy is called.
This is asynchronous and we don't check a result.
A result will be visible when this function is called on the REPL."
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (= 5 *write-serial-called*))
              1.0))))

(run! 'init-serial)
(run! 'init-serial--fail-to-open)
(run! 'start-record--serial-written)
