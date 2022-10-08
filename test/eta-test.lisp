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

(defvar *open-serial-called* nil)
(defvar *close-serial-called* nil)
(defvar *write-serial-called* nil)
(defvar *read-serial-called* 0)

(defmethod eta-ser-if:open-serial ((impl (eql :test)) device)
  (cond
    ((string= "/dev/not-exists" device) (error "Can't open!"))
    (t (setf *open-serial-called* t))))
(defmethod eta-ser-if:close-serial ((impl (eql :test)) port)
  (declare (ignore port))
  (setf *close-serial-called* t))
(defmethod eta-ser-if:write-serial ((impl (eql :test)) port data)  
  (declare (ignore port))
  (setf *write-serial-called* (length data)))
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

(test close-serial
  (with-fixture init-destroy ()
    (is (eq :ok (init-serial "/dev/serial")))
    (is (eq :ok (close-serial)))
    (is-true *close-serial-called*)))

(test start-record--serial-written
  "Tests that the write function on the serial proxy is called.
This is asynchronous and we don't check a result.
A result will be visible when this function is called on the REPL."
  (with-fixture init-destroy ()
    (with-mocks ()
      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (= (length (eta-pkg:new-start-record-pkg)) *write-serial-called*))
                1.0)))))

(test start-record--serial-written--read-received--repeated
  (with-fixture init-destroy ()
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (> *read-serial-called* 3))  ;; we check for 3
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
      (answer eta-pkg:collect-data (values nil `#(123 0 1 2 3)))
      
      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (> (length (invocations 'eta-pkg:collect-data)) 0)))
                1.0)))))

(test start-record--read-received--call-parser--complete--empty-monitor
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :eta-monitor '()))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                1.0)))))

(test start-record--read-received--call-parser--complete--with-monitor
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
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

(test start-record--complete--with-monitor--build-avg
  "We use `get-state' internal API to retrieve the state of the actor in order to check on the avgs."
  (with-fixture init-destroy ()
    (with-mocks ()
      (setf eta::*avg-items* '(("FooItem" . (("FooItemAvg1" . nil) ("FooItemAvg2" . nil)))))
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (> *read-serial-called* 5))
                1.0))
      (let* ((state (eta::get-state))
             (avgs (eta::actor-state-avgs state))
             (readn *read-serial-called*))
        (is (equalp avgs `(("FooItemAvg1" . ,(/ (* readn 1.1) readn))
                           ("FooItemAvg2" . ,(/ (* readn 1.1) readn)))))))))

(test start-record--read-received--call-parser--complete--extract-fail
  (with-fixture init-destroy ()
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :fail "Extract failure!"))
      (answer openhab:do-post nil)

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                1.0))
      (is (= (length (invocations 'openhab:do-post)) 0)))))

(test stop-record--serial-written
  (with-fixture init-destroy ()
    (with-mocks ()
      (is (eq :ok (stop-record)))
      (is-true (utils:assert-cond
                (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
                1.0)))))

(test stop-record--stops-read
  (with-fixture init-destroy ()
    (with-mocks ()
      (is (eq :ok (start-record)))
      (is (eq :ok (stop-record)))
      (is-true (utils:assert-cond
                (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
                1.0))
      (sleep 0.5)
      (is (< *read-serial-called* 5)))))

(test report-avgs
  (with-fixture init-destroy ()
    (with-mocks ()
      (setf eta::*avg-items* '(("FooItem" . (("FooItemAvg1" . nil) ("FooItemAvg2" . nil)))))
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
      (answer (openhab:do-post res data)
        (progn
          (assert (or (equal res "FooItem")
                      (equal res "FooItemAvg1")
                      (equal res "FooItemAvg2")))
          (assert (= data 1.1))
          :ok))

      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (> *read-serial-called* 5))
                1.0))
      (is (eq :ok (report-avgs)))
      (is-true (utils:assert-cond
                (lambda ()
                  (flet ((containsp (invocs item)
                           (member item invocs :key #'second :test #'string=)))
                    (let ((invocs (invocations 'openhab:do-post)))
                      (not (null
                            (and (containsp invocs "FooItemAvg1")
                                 (containsp invocs "FooItemAvg2")))))))
                1.0)))))
