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

(defvar *open-serial-called* nil)
(defvar *write-serial-called* nil)
(defvar *read-serial-called* 0)

(defmethod eta-ser-if:open-serial ((impl (eql :test)) device)
  (cond
    ((string= "/dev/not-exists" device) (error "Can't open!"))
    (t (setf *open-serial-called* t))))
(defmethod eta-ser-if:write-serial ((impl (eql :test)) port data)  
  (declare (ignore port data))
  (setf *write-serial-called* 5))
(defmethod eta-ser-if:read-serial ((impl (eql :test)) port &optional timeout)
  (declare (ignore port timeout))
  ;; we just do a tiny timeout
  (sleep .1)
  (incf *read-serial-called*)
  #())

(def-fixture init-destroy ()
  (setf *open-serial-called* nil
        *write-serial-called* 0
        *read-serial-called* 0)
  (unwind-protect
       (progn
         (eta:ensure-initialized)
         (setf eta:*serial-proxy-impl* :test)
         (&body))
    (progn
      (eta:ensure-shutdown))))
  
(test init-serial
  (with-fixture init-destroy ()
    (is (eq :ok (init-serial "/dev/serial")))
    (is-true *open-serial-called*)))

(test init-serial--fail-to-open
  (with-fixture init-destroy ()
    (let ((init-serial-result (multiple-value-list (init-serial "/dev/not-exists"))))
      (is (eq :fail (car init-serial-result)))
      (is (string= "Can't open!" (cadr init-serial-result))))))

(test start-record--serial-written
  "Tests that the write function on the serial proxy is called.
This is asynchronous and we don't check a result.
A result will be visible when this function is called on the REPL."
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (= 5 *write-serial-called*))
              1.0))))

(test start-record--serial-written--read-received
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (> *read-serial-called* 1))
              1.0))))

(test start-record--read-received--call-parser
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values nil #()))
      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
              (lambda () (and (> *read-serial-called* 0)
                         (> (length (invocations 'eta-pkg:collect-data)) 0)))
              1.0)))))

(test start-record--read-received--call-parser--no-complete
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values nil #(#\{ 0 1 2 3)))
      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (> (length (invocations 'eta-pkg:collect-data)) 0)))
                1.0)))))

(test start-record--read-received--call-parser--complete--empty-monitor
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(#\{ 0 1 2 3 #\})))      
      (answer eta-pkg:extract-pkg (values :monitor '()))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                1.0)))))

(test start-record--read-received--call-parser--complete--with-monitor
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(#\{ 0 1 2 3 #\})))      
      (answer eta-pkg:extract-pkg
        (values :monitor '(("FooItem" . 1.1))))
      (answer (openhab:do-post res data)
        (progn
          (assert (equal res "FooItem"))
          (assert (= data 1.1))
          :ok))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)
                           (= (length (invocations 'openhab:do-post)) 1)))
                1.0)))))

(test start-record--read-received--call-parser--complete--extract-fail
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(#\{ 0 1 2 3 #\})))      
      (answer eta-pkg:extract-pkg
        (values :fail "Extract failure!"))
      (answer openhab:do-post nil)

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                1.0))
      (is (= (length (invocations 'openhab:do-post)) 0)))))

#|
TODO:
OK - test for read continously
OK - test for call to read handler when data arrived
OK - test for incomplete package handling
OK - test for complete package handling
OK - complete package handling should call eta pkg extractor
OK - result of pkg extractor should extract eta package
OK - extracted package should send openhab post requests for each extract
=> - verify proper eta-packages are used (i.e. for start-record)
- 'stop-record'
- 'shutdown-serial
|#
