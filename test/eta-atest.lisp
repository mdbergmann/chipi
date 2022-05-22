(defpackage :cl-eta.eta-atest
  (:use :cl :fiveam :cl-mock :cl-eta.eta)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-atest)

(def-suite eta-atests
  :description "Acceptance tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-atests)

(defvar *path-prefix* "/rest/items/")

(defmethod eta-ser-if:open-serial ((impl (eql :test)) device)
  (declare (ignore proxy device))
  t)
(defmethod eta-ser-if:write-serial ((impl (eql :test)) port data)
  (declare (ignore port data))
  ;; todo: create proper size of start-record package
  0)
(defmethod eta-ser-if:read-serial ((impl (eql :test)) port &optional timeout)
  (declare (ignore port timeout))
  ;; todo: create proper monitor package
  #())

(def-fixture init-destroy ()
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (setf eta:*serial-proxy-impl* :test)
         (&body))
    (eta:ensure-shutdown)))

(test start-record--success--one-item
  "Sends the record ETA interface package that will result in receiving data packages."
  (with-fixture init-destroy ()
    (with-mocks ()
      ;; the `start-record' function is the trigger for the boiler to send monitor data,
      ;; which is eventually forwarded to openHAB.
      ;; So we can expect that after calling `start-record' an http call will go out
      ;; to openHAB with data we expect to be sent.
      (answer (openhab:do-post url data)
        (progn
          (assert (uiop:string-prefix-p "http://" url))
          (assert (uiop:string-suffix-p (format nil "~a/HeatingETAOperatingHours" *path-prefix*) url))
          (assert (floatp data))
          t))

      (is (eq :ok (start-record)))
      (is (= 1 (length (invocations 'openhab:do-post))))    
      )))
