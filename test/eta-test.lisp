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
(defvar *eta-col-called* 0)
(defvar *eta-col-data* #())
(defvar *openhab-post-called* 0)
(defvar *openhab-post-data* nil)

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

(defmethod eta-col:collect-data ((impl (eql :test)) prev-data new-data)
  (incf *eta-col-called*)
  (values nil (concatenate 'vector prev-data new-data)))

(defmethod eta-col:collect-data ((impl (eql :test-no-complete-pkg)) prev-data new-data)
  (declare (ignore prev-data new-data))
  (format t "counter: ~a~%" *eta-col-called*)
  (incf *eta-col-called*)
  (setf *eta-col-data* #(#\{ 0 1 2 3))
  (values nil *eta-col-data*))

(defmethod eta-col:collect-data ((impl (eql :test-complete-pkg)) prev-data new-data)
  (declare (ignore prev-data new-data))
  (format t "counter: ~a~%" *eta-col-called*)
  (setf *eta-col-data* #(#\{ 0 1 2 3 #\}))
  (values t *eta-col-data*))

(def-fixture init-destroy ()
  (setf *open-serial-called* nil
        *write-serial-called* nil
        *read-serial-called* 0
        *eta-col-called* 0
        *eta-col-data* #())
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
    (setf eta:*eta-collector-impl* :test)
    (is (eq :ok (start-record)))
    (is-true (utils:assert-cond
              (lambda () (and (> *read-serial-called* 0)
                         (> *eta-col-called* 0)))
              1.0))))

(test start-record--read-received--call-parser--no-complete
  (flet ((assert-package-incomplete ()
           (and (= (length *eta-col-data*) 5)
                (equalp #(#\{ 0 1 2 3) *eta-col-data*))))
    (with-fixture init-destroy ()
      (setf eta:*eta-collector-impl* :test-no-complete-pkg)
      (is (eq :ok (start-record)))
      (is-true (utils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (> *eta-col-called* 0)))
                1.0))
      (format t "col-final: ~a~%" *eta-col-data*)
      (is-true (assert-package-incomplete)))))

(test start-record--read-received--call-parser--complete
  (flet ((assert-package-complete ()
           (and (= (length *eta-col-data*) 6)
                (equalp #(#\{ 0 1 2 3 #\}) *eta-col-data*))))
    (with-fixture init-destroy ()
      (setf eta:*eta-collector-impl* :test-complete-pkg)
      (with-mocks ()
        (answer (eta-extract:extract-pkg pkg-data) '())
        
        (is (eq :ok (start-record)))
        (is-true (utils:assert-cond
                  (lambda () (and (> *read-serial-called* 0)
                             (= (length (invocations 'eta-extract:extract-pkg)) 1)))
                  1.0))
        (format t "col-final: ~a~%" *eta-col-data*)
        (is-true (assert-package-complete))))))

#|
TODO:
OK - test for read continously
OK - test for call to read handler when data arrived
OK - test for incomplete package handling
OK - test for complete package handling
=> - complete package handling should call eta pkg extractor
- result of pkg extractor should extract eta package
- extracted package should send openhab post requests for each extract
- test 'start-record' actually sends the proper ETA package
- 'stop-record'
- 'shutdown-serial
|#
