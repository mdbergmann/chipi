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

(test send-record-package--success--one-item
  "Sends the record ETA interface package that will result in receiving data packages."
  (with-mocks ()
    ;; the `send-record-package' function is the trigger for the boiler to send monitor data,
    ;; which is eventually forwarded to openHAB.
    ;; So we can expect that after calling `send-record-package' an http call will go out
    ;; to openHAB with data we expect to be sent.
    (answer (openhab:do-post url data)
      (progn
        (assert (uiop:string-prefix-p "http://" url))
        (assert (uiop:string-suffix-p (format nil "~a/HeatingETAOperatingHours" *path-prefix*) url))
        (assert (floatp data))
        t))

    (is (eq :ok (send-record-package)))
    (is (= 1 (length (invocations 'openhab:do-post))))    
    ))
