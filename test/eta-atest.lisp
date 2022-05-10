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

(test send-record-package
  "Sends the record ETA interface package that will result in receiving data packages."
  (with-mocks ()
    ;; we check against the serial system boundary at this point
    ;; and verify that data is sent via `libserialport' library.
    ;; this of course adds a coupling to `libserialport',
    ;; but it's unlikely this library will be replaced but
    ;; it represents the external interface we can check against
    ;; later we can expand and verify the data that is sent.
    (answer (libserialport:serial-write-data port data))

    (is (eq :ok (send-record-package)))

    (is (= 1 (length (invocations 'libserialport:serial-write-data))))    
    ))
