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

(def-fixture init-destroy (&optional avgs-items)
  (setf *open-serial-called* nil
        *write-serial-called* 0
        *read-serial-called* 0)
  (let ((avgs-copy eta::*eta-avg-items*))
    (when avgs-items
      (setf eta::*eta-avg-items* avgs-items))
    (unwind-protect
         (progn
           (eta:ensure-initialized)
           (setf eta:*eta-serial-proxy-impl* :test)
           (setf eta:*eta-serial-device* "/dev/serial")
           (&body))
      (progn
        (eta:ensure-shutdown)
        (setf eta::*eta-avg-items* avgs-copy)))))

(test init-serial
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (is-true *open-serial-called*)))

(test init-serial--fail-to-open
  (with-fixture init-destroy ()
    (setf eta:*eta-serial-device* "/dev/not-exists")
    (let ((init-serial-result (multiple-value-list (eta-init))))
      (is (eq :fail (car init-serial-result)))
      (is (string= "Can't open!" (cadr init-serial-result))))))

(test close-serial
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (is (eq :ok (eta-close-serial)))
    (is-true *close-serial-called*)))

(test start-record--serial-written
  "Tests that the write function on the serial proxy is called.
This is asynchronous and we don't check a result.
A result will be visible when this function is called on the REPL."
  (with-fixture init-destroy ()
    (with-mocks ()
      (is (eq :ok (eta-init)))
      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (= (length (eta-pkg:new-start-record-pkg)) *write-serial-called*))
                1.0)))))

(test start-record--serial-written--read-received--repeated
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (is (eq :ok (eta-start-record)))
    (is-true (miscutils:assert-cond
              (lambda () (> *read-serial-called* 3))  ;; we check for 3
              10.0))))

(test start-record--read-received--call-parser
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values nil #()))
      
      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
              (lambda () (and (> *read-serial-called* 0)
                         (> (length (invocations 'eta-pkg:collect-data)) 0)))
              10.0)))))

(test start-record--read-received--call-parser--no-complete
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values nil `#(123 0 1 2 3)))
      
      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (> (length (invocations 'eta-pkg:collect-data)) 0)))
                10.0)))))

(test start-record--read-received--call-parser--complete--empty-monitor
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :eta-monitor '()))

      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                10.0)))))

(test start-record--read-received--call-parser--complete--with-monitor
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
      (answer (openhab:do-post res data)
        (progn
          (assert (equal res "FooItem"))
          (assert (= data 1.1))
          :ok))

      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)
                           (= (length (invocations 'openhab:do-post)) 1)))
                10.0)))))

(test start-record--complete--with-monitor--build-avg
  "We use `get-state' internal API to retrieve the state of the actor in order to check on the avgs."
  (with-fixture init-destroy ('(("FooItem" .
                                 (("FooItemAvg1" . nil)
                                  ("FooItemAvg2" . nil)))
                                ("FooItem2" .
                                 (("FooItem2Avg" . nil)))))
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1) ("FooItem2" . 2.2))))
      (answer openhab:do-post :ok)

      (is-true (null (eta::eta-actor-state-avgs (eta::eta-get-state))))
      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (> *read-serial-called* 5))
                10.0))
;;      (is (eq :ok (eta-stop-record)))      
      (let* ((state (eta::eta-get-state))
             (avgs (eta::eta-actor-state-avgs state)))
        (is (= (length avgs) 3))
        (is (every (lambda (x) (typep x 'eta::eta-avg-record)) avgs))
        (flet ((assert-avg (name initial-value current-value initial-time current-time)
                 (find-if
                  (lambda (x) (and (string= name (eta::eta-avg-record-cadence-name x))
                              (= initial-value (eta::eta-avg-record-initial-value x))
                              (= current-value (eta::eta-avg-record-current-value x))
                              (> (eta::eta-avg-record-initial-time x) initial-time)
                              (> (eta::eta-avg-record-current-time x) current-time)))
                  avgs)))
          (is-true (and (assert-avg "FooItemAvg1" 1.1 1.1 0 0)
                        (assert-avg "FooItemAvg2" 1.1 1.1 0 0)
                        (assert-avg "FooItem2Avg" 2.2 2.2 0 0))))))))

(test start-record--read-received--call-parser--complete--extract-fail
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))      
      (answer eta-pkg:extract-pkg (values :fail "Extract failure!"))
      (answer openhab:do-post nil)

      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (and (> *read-serial-called* 0)
                           (= (length (invocations 'eta-pkg:extract-pkg)) 1)))
                10.0))
      (is (= (length (invocations 'openhab:do-post)) 0)))))

(test stop-record--serial-written
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (is (eq :ok (eta-stop-record)))
      (is-true (miscutils:assert-cond
                (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
                10.0)))))

(test stop-record--stops-read
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (is (eq :ok (eta-start-record)))
      (is (eq :ok (eta-stop-record)))
      (is-true (miscutils:assert-cond
                (lambda () (= (length (eta-pkg:new-stop-record-pkg)) *write-serial-called*))
                10.0))
      (sleep 0.5)
      (is (< *read-serial-called* 5)))))

(test report-avgs
  (with-fixture init-destroy ('(("FooItem" . (("FooItemAvg1" . nil) ("FooItemAvg2" . nil)))))
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
      (answer (openhab:do-post res data)
        (progn
          ;; all the possible parameter values
          (assert (or (equal res "FooItem")
                      (equal res "FooItemAvg1")
                      (equal res "FooItemAvg2")))
          (assert (or (= data 1.1)
                      (= data 1.2)
                      (= data 2.1)))
          :ok))
      (answer (eta::%calculate-avg avg-item)
        (cond
          ((string= "FooItemAvg1" (eta::eta-avg-record-cadence-name avg-item))
           '("FooItemAvg1" . 1.2)) ; just something to distinguish
          ((string= "FooItemAvg2" (eta::eta-avg-record-cadence-name avg-item))
           '("FooItemAvg2" . 2.1)) ; just something
          (t (error "shouldn't be here!"))))

      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (> *read-serial-called* 1))
                10.0))
      ;; stop read to not accumulate more avgs
      (is (eq :ok (eta-stop-record)))
      (flet ((containsp (invocs item)
               (member item invocs :key #'second :test #'string=)))
        (is (eq :ok (eta-report-avgs "FooItemAvg1")))
        (is-true (miscutils:assert-cond
                  (lambda ()
                    (let ((invocs (invocations 'openhab:do-post)))
                      (and (containsp invocs "FooItemAvg1")
                           (not (containsp invocs "FooItemAvg2")))))
                  10.0))
        
        (is (eq :ok (eta-report-avgs "FooItemAvg2")))
        (is-true (miscutils:assert-cond
                  (lambda ()
                    (let ((invocs (invocations 'openhab:do-post)))
                      (containsp invocs "FooItemAvg2")))
                  10.0)))
      (is (= 2 (length (invocations 'eta::%calculate-avg))))
      ;; check avgs have been cleaned upon submitting
      ;; flacky test
      (is (= 0 (length (eta::eta-actor-state-avgs (eta::eta-get-state)))))
      )))

(test report-avgs--err-on-avg-http
  (with-fixture init-destroy ('(("FooItem" . (("FooItemAvg1" . nil)))))
    (is (eq :ok (eta-init)))
    (with-mocks ()
      (answer eta-pkg:collect-data (values t #(123 0 1 2 3 125)))
      (answer eta-pkg:extract-pkg (values :eta-monitor '(("FooItem" . 1.1))))
      (answer (openhab:do-post res _)
        (if (equal res "FooItem")
            :ok
            (error "Some condition!")))
      (answer (eta::%calculate-avg avg-item)
        (cond
          ((string= "FooItemAvg1" (eta::eta-avg-record-cadence-name avg-item))
           '("FooItemAvg1" . 1.2)) ; just something to distinguish
          (t (error "shouldn't be here!"))))

      (is (eq :ok (eta-start-record)))
      (is-true (miscutils:assert-cond
                (lambda () (> *read-serial-called* 1))
                10.0))
      (flet ((containsp (invocs item)
               (member item invocs :key #'second :test #'string=)))
        (is (eq :ok (eta-report-avgs "FooItemAvg1")))
        (is-true (miscutils:assert-cond
                  (lambda ()
                    (let ((invocs (invocations 'openhab:do-post)))
                      (containsp invocs "FooItemAvg1")))
                    10.0))))))

(test jobdef-to-cronjob
  "Tests conversion of avg job definition to cron-job"
  (let ((job (eta-make-jobdefinition (lambda ()) '(:m 0 :h 0 :d 0 :dow 0 :dom 0 :name test))))
    (is (symbolp job))
    (is (eq 'test job))))

(test after-init-generates-the-cron-jobs-we-want
  (with-fixture init-destroy ('(("FooItem" .
                                 (("FooItemAvg1" . (:m 0 :h 0 :dow 0 :name fooitemavg1))
                                  ("FooItemAvg2" . (:m 0 :h 0 :dow 0 :name fooitemavg2))))))
    (is (eq :ok (eta-init)))
    (is (= 2 (hash-table-count cron::*cron-jobs-hash*)))
    (is (gethash 'fooitemavg1 cron::*cron-jobs-hash*))
    (is (gethash 'fooitemavg2 cron::*cron-jobs-hash*))
    (is (not (null cron::*cron-dispatcher-thread*)))))

(test destroy-cleans-up
  (with-fixture init-destroy ('(("FooItem" .
                                 (("FooItemAvg1" . (:m 0 :h 0 :dow 0 :name fooitemavg1))
                                  ("FooItemAvg2" . (:m 0 :h 0 :dow 0 :name fooitemavg2))))))
    (is (eq :ok (eta-init))))
  (is (= 0 (hash-table-count cron::*cron-jobs-hash*))))

(test real-avgs--have-deployed-cron-jobs
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (is (= 2 (hash-table-count cron::*cron-jobs-hash*)))
    (is (not (null (gethash 'eta::heating-eta-op-hours-per-week cron::*cron-jobs-hash*))))
    (is (not (null (gethash 'eta::heating-eta-ig-count-per-day cron::*cron-jobs-hash*))))))

(test calculate-avg
  (let* ((day-in-secs (* 24 60 60))
         (now (get-universal-time))
         (initial (- now (* 3 day-in-secs)))
         (avg-item (eta::make-eta-avg-record :initial-value 0
                                             :current-value 5
                                             :initial-time initial
                                             :current-time now
                                             :cadence-name "foo")))
    (is (equalp `("foo" . ,(/ (- 5 0)
                              (float (/ (- now initial) day-in-secs))))
                (eta::%calculate-avg avg-item)))))

(test stop-stops-actor-and-serial
  (with-fixture init-destroy ()
    (is (eq :ok (eta-init)))
    (is (eq :ok (eta-stop)))
    (is-false eta::*eta-serial-actor*)
    (is-false eta::*eta-serial-device*)))
