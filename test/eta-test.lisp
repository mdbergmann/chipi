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

(defparameter *open-serial-called* nil)
(defparameter *write-serial-called* nil)

(defclass fake-serial-proxy (serial-proxy) ())
(defmethod eta::open-serial ((proxy fake-serial-proxy) device)
  (assert proxy)
  (setf *open-serial-called* t))
(defmethod eta::write-serial ((proxy fake-serial-proxy) port data)
  (assert proxy)
  (setf *write-serial-called* 5))

(test init-serial
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (change-class eta:*serial-proxy* 'fake-serial-proxy)
         (is (eq :ok (init-serial "/dev/serial")))
         (is-true (utils:assert-cond
                   (lambda ()
                     *open-serial-called*)
                   1.0)))
  (eta:ensure-shutdown)))

;; (test start-record--ok
;;   (unwind-protect
;;        (with-mocks ()
;;          (is (eq :ok (start-record)))
;;          (is-true (utils:assert-cond
;;                    (lambda ()
;;                      (= 1 (length (invocations 'libserialport:serial-write-data))))
;;                    1.0)))
;;     (eta:ensure-shutdown)))

(run! 'init-serial)
;;(run! 'start-record--ok)
