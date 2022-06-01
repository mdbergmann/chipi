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

(defparameter +record-data-pkg-payload+
  '(#x18 0 53 0 99                      ; Betriebsstunden (99 hours)
    ))

(defun new-record-data-pkg ()
  (coerce (alexandria:flatten
           `(#\{
             #\M #\D
             ,(length +record-data-pkg-payload+)
             ,(eta-pkg:check-sum +record-data-pkg-payload+)
             ,+record-data-pkg-payload+
             #\}))
          'vector))


(defmethod eta-ser-if:open-serial ((impl (eql :atest-start-record)) device)
  (declare (ignore impl device))
  t)
(defmethod eta-ser-if:write-serial ((impl (eql :atest-start-record)) port data)
  (declare (ignore impl port data))
  (length (eta-pkg:new-start-record-pkg)))
(defmethod eta-ser-if:read-serial ((impl (eql :atest-start-record)) port &optional timeout)
  (declare (ignore impl port timeout))
  (new-record-data-pkg))

(defparameter *stop-record-written-serial* 0)
(defmethod eta-ser-if:open-serial ((impl (eql :atest-stop-record)) device)
  (declare (ignore impl device))
  t)
(defmethod eta-ser-if:write-serial ((impl (eql :atest-stop-record)) port data)
  (declare (ignore impl port data))
  (setf *stop-record-written-serial* (length (eta-pkg:new-stop-record-pkg))))
(defmethod eta-ser-if:read-serial ((impl (eql :atest-stop-record)) port &optional timeout)
  (declare (ignore impl port timeout))
  #())

(def-fixture init-destroy ()
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (&body))
    (eta:ensure-shutdown)))

(test start-record--success
  "Sends the record ETA interface package that will result in receiving data packages."
  (with-fixture init-destroy ()
    (setf eta:*serial-proxy-impl* :atest-start-record)
    (with-mocks ()
      ;; the `start-record' function is the trigger for the boiler to send monitor data,
      ;; which is eventually forwarded to openHAB.
      ;; So we can expect that after calling `start-record' an http call will go out
      ;; to openHAB with data we expect to be sent.
      (answer (openhab:do-post resource data)
        (progn
          (assert (string= "HeatingETAOperatingHours" resource))
          (assert (floatp data))
          (assert (= data 99.0))
          :ok))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond (lambda ()
                                    (>= (length (invocations 'openhab:do-post)) 1))
                                  0.2)))))

(test stop-record--success
  "Sends the serial command to stop sending record packages."
  (with-fixture init-destroy ()
    (setf eta:*serial-proxy-impl* :atest-stop-record)
    (with-mocks ()
      ;; the `stop-record' function is counterpart of the `start-record' function.
      ;; It will instruct the ETA to stop sending monitor data.
      (is (eq :ok (stop-record)))
      (is-true (utils:assert-cond (lambda ()
                                    (> *stop-record-written-serial* 0))
                                  0.2)))))
