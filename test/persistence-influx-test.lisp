(defpackage :chipi.influx-persistence-test
  (:use :cl :fiveam :cl-mock :chipi.persistence :chipi.influx-persistence)
  (:import-from #:chipi.persistence-test
                #:assert-fetch-error)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.influx-persistence-test)

(def-suite influx-persistence-tests
  :description "Persistence tests for influxdb"
  :in chipi.tests:test-suite)

(in-suite influx-persistence-tests)

(defparameter *influx-test-settings*
  '("CHIPI_TEST_INFLUX_URL" "CHIPI_TEST_INFLUX_TOKEN"
    "CHIPI_TEST_INFLUX_ORG" "CHIPI_TEST_INFLUX_BUCKET"))

(defun test-setting (name)
  (let ((value (uiop:getenv name)))
    (when (and value (plusp (length value)))
      value)))

(def-fixture init-destroy-env ()
  (let ((missing (remove-if #'test-setting *influx-test-settings*)))
    (if missing
        (skip "Missing Influx test settings: ~{~a~^, ~}" missing)
        (unwind-protect
             (progn (&body))
          (envi:shutdown-env)))))

;; This suite is run only by the configuration regression tests below.
(def-suite influx-configuration-probe-tests)

(test (retrieve-range--configuration-probe :suite influx-configuration-probe-tests)
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'configuration-probe :type-hint 'integer)))
      (is (null (persp:retrieve-range cut item (persp:make-relative-range :seconds 20)))))))

(test test-setting--missing-settings-skip-before-http
  (dolist (missing '("CHIPI_TEST_INFLUX_URL" "CHIPI_TEST_INFLUX_TOKEN"
                     "CHIPI_TEST_INFLUX_ORG" "CHIPI_TEST_INFLUX_BUCKET"))
    (dolist (missing-value '(nil ""))
      (with-mocks ()
        (answer (uiop:getenv name)
          (if (string= name missing) missing-value "configured"))
        (answer (drakma:http-request)
          (error "Unexpected HTTP request without ~a" missing))
        (multiple-value-bind (passed failures skipped)
            (results-status (run 'retrieve-range--configuration-probe))
          (is-true passed)
          (is (null failures))
          (is (= 1 (length skipped))))
        (is (null (invocations 'drakma:http-request)))))))

(test test-setting--configured-http-failure-is-not-skipped
  (with-mocks ()
    (answer (uiop:getenv name)
      (if (string= name "CHIPI_TEST_INFLUX_URL") "http://localhost:8086" "configured"))
    (answer (drakma:http-request)
      (error "Configured Influx request failed: ~a" :unavailable))
    (multiple-value-bind (passed failures skipped)
        (results-status (run 'retrieve-range--configuration-probe))
      (is-false passed)
      (is (= 1 (length failures)))
      (is (null skipped)))
    (is (= 1 (length (invocations 'drakma:http-request))))))

(test make-persistence--influx
  "Make a `influx' persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url (test-setting "CHIPI_TEST_INFLUX_URL")
                :token "some"
                :org (test-setting "CHIPI_TEST_INFLUX_ORG")
                :bucket (test-setting "CHIPI_TEST_INFLUX_BUCKET"))))
      (print cut)
      (is-true cut)
      (is (typep cut 'influx-persistence)))))

;; --------------------------------------
;; !!! tests below require a running influxdb instance
;; --------------------------------------

(defun make-cut ()
  (make-influx-persistence
   :persp-influx
   :base-url (test-setting "CHIPI_TEST_INFLUX_URL")
   :token (test-setting "CHIPI_TEST_INFLUX_TOKEN")
   :org (test-setting "CHIPI_TEST_INFLUX_ORG")
   :bucket (test-setting "CHIPI_TEST_INFLUX_BUCKET")))

(defun make-test-item (id &rest args)
  (apply #'item:make-item
         (make-symbol (format nil "~a-~a" id (symbol-name (gensym "test-")))) args))

(defmacro test-type-value (type value itemid)
  `(let ((cut (make-cut))
         (item (make-test-item ,itemid :type-hint ,type)))
     (item:set-value item ,value)
     (persp:store cut item)
     (sleep 1)
     (let ((fetched (persp:fetch cut item)))
       (is-true (miscutils:await-cond 5.0
                  (let ((resolved (future:fresult fetched)))
                    (and (not (eq resolved :not-ready))
                         (equal (persisted-item-value resolved) ,value))))))))

(test influx-persistence--store-and-fetch--string-type
  "Store a string value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'string "some string" 'stringfield)))

(test influx-persistence--write--multiline-scalar-strings
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut)))
      (dolist (separator (list (string #\Newline) (string #\Return)
                              (format nil "~c~c" #\Return #\Newline)))
        (let ((item (make-test-item 'multiline-scalar :type-hint 'string))
              (value (format nil "first~asecond \"quote\" slash\\" separator)))
          ;; Verify write acceptance without changing the scalar CSV parser.
          (is (= 204 (nth-value 1
                               (chipi.influx-persistence::%make-write-request
                                cut (act-cell:name item) value
                                (chipi.influx-persistence::%universal-to-unix-ts
                                 (get-universal-time)))))))))))

(test influx-persistence--store-and-fetch--int-type
  "Store an integer value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'integer 1234 'intfield)))

(test influx-persistence--store-and-fetch--float-type
  "Store a float value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'float 1234.567 'floatfield)))

(test influx-persistence--store-and-fetch--bool-type-t
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'boolean 'item:true 'boolfield)))

(test influx-persistence--store-and-fetch--bool-type-nil
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'boolean 'item:false 'boolfield)))

(test influx-persistence--fetch-with-error--unknown-host
  "Fetch raises error, i.e. unknown host."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url "http://notexist:8086"
                :token ""
                :org (test-setting "CHIPI_TEST_INFLUX_ORG")
                :bucket (test-setting "CHIPI_TEST_INFLUX_BUCKET")))
          (item (make-test-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item)))
        (assert-fetch-error fetched)))))


(test influx-persistence--fetch-with-error--api-response-4xy
  "Fetch raises error, i.e. api response 4xy."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url (test-setting "CHIPI_TEST_INFLUX_URL")
                :token "wrong-token"
                :org (test-setting "CHIPI_TEST_INFLUX_ORG")
                :bucket (test-setting "CHIPI_TEST_INFLUX_BUCKET")))
          (item (make-test-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item)))
        (assert-fetch-error fetched)))))
  
;; --------------------------------------
;; history api tests
;; --------------------------------------

(test influx-persistence--range
  "Retrieves item value history for time frame."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'history :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      (dotimes (x 10)
        (item:set-value item x)
        (sleep 1))
      (sleep 1)
      (flet ((assert-fetched (fetched-range)
               (is-true (miscutils:await-cond 2.0 (future:complete-p fetched-range)))
               (let ((fetched-items (future:fresult fetched-range)))
                 (is (= (length fetched-items) 10))
                 (is (= (persisted-item-value (car fetched-items)) 0))
                 (is (= (persisted-item-value (car (last fetched-items))) 9))
                 (is (= (reduce #'+ (mapcar #'persisted-item-value fetched-items) :initial-value 0) 45)))))
        ;; pure relative
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-relative-range :seconds 20))))
          (assert-fetched fetched-range))
        ;; implicit absolute
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-relative-range
                                           :seconds 20 :end-ts (get-universal-time)))))
          (assert-fetched fetched-range))
        ;; absolute range
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-absolute-range
                                           (- (get-universal-time) 20)
                                           (get-universal-time)))))
          (assert-fetched fetched-range))          
        ))))

(test influx-persistence--range--no-data-yields-empty-list
  "A range fetch for a measurement without any data yields an empty list
instead of an error (influx responds with an empty body)."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'never-persisted-item :type-hint 'float)))
      (is (null (persp:retrieve-range cut item
                                      (persp:make-relative-range :seconds 20)))))))

(test influx-persistence--range--fetch-with-error--api-response-4xy
  "Fetch raises error, i.e. api response 4xy."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url (test-setting "CHIPI_TEST_INFLUX_URL")
                :token "wrong-token"
                :org (test-setting "CHIPI_TEST_INFLUX_ORG")
                :bucket (test-setting "CHIPI_TEST_INFLUX_BUCKET")))
          (item (make-test-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item (persp:make-relative-range :seconds 20))))
        (assert-fetch-error fetched)))))

;; --------------------------------------
;; aggregate API tests
;; --------------------------------------

(test influx-persistence--aggregate--influx-fn
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'avgfield :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      ;; push values 1,2,3,4,5 one per second
      (dotimes (i 5)
        (item:set-value item (1+ i))
        (sleep 1))
      (sleep 1)
      ;; fetch over last 20 seconds
      (flet ((assert-fetched (aggregate-fun value)
               (let ((fut (persp:fetch cut item
                                       (persp:make-relative-range :seconds 20)
                                       aggregate-fun)))
                 (is-true (miscutils:await-cond 2.0 (future:complete-p fut)))
                 (let ((result (future:fresult fut)))
                   (is (= (length result) 1))
                   (is (= (persisted-item-value (car result)) value))))))
        (assert-fetched :avg 3)
        (assert-fetched :min 1)
        (assert-fetched :max 5)
        (assert-fetched :sum 15)
        (assert-fetched :count 5)
        (assert-fetched :median 3)
        ))))

(test influx-persistence--aggregate--error--bad-aggregate-parameter
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'avgfield :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      (let ((fut (persp:fetch cut item
                              (persp:make-relative-range :seconds 20)
                              :not-supported)))
        (is-true (miscutils:await-cond 2.0 (future:complete-p fut)))
        (let ((result (future:fresult fut)))
          (is (eq (car result) :error)))))))

(defun table-value (&rest fields)
  (alexandria:plist-hash-table fields :test 'equal))

(defun write-table-point (persistence item fields universal-time)
  (is (= 204 (nth-value 1
                       (chipi.influx-persistence::%make-write-request
                        persistence (act-cell:name item) fields
                        (chipi.influx-persistence::%universal-to-unix-ts universal-time))))))

(test retrieve-range--multiline-table-strings-roundtrip
  (with-fixture init-destroy-env ()
    (let* ((cut (make-cut))
           (item (make-test-item 'multiline-table :type-hint 'table))
           (timestamp (- (get-universal-time) 5))
           (lf (format nil "first~%second \"quote\" slash\\"))
           (crlf (format nil "first~c~csecond \"quote\" slash\\" #\Return #\Newline))
           (cr (format nil "first~csecond \"quote\" slash\\" #\Return))
           (range (persp:make-absolute-range (1- timestamp) (1+ timestamp))))
      (write-table-point cut item (table-value "LF" lf "CRLF" crlf "CR" cr) timestamp)
      (let* ((latest (persp:retrieve cut item))
             (history (persp:retrieve-range cut item range)))
        (is (= 1 (length history)))
        (dolist (point (list latest (first history)))
          (is (= timestamp (persisted-item-timestamp point)))
          (let ((value (persisted-item-value point)))
            (is (= 3 (hash-table-count value)))
            ;; InfluxDB CSV normalizes LF to CRLF and removes bare CR.
            ;; A raw HTTP probe confirms this happens before Lisp parses CSV.
            (is (string= crlf (gethash "LF" value)))
            (is (string= crlf (gethash "CRLF" value)))
            (is (string= "firstsecond \"quote\" slash\\" (gethash "CR" value)))))))))

(test retrieve-range--tables-roundtrip-and-aggregates
  (with-fixture init-destroy-env ()
    (let* ((cut (make-cut))
           (item (make-test-item '|Room, A=B"| :type-hint 'table))
           (first-time (- (get-universal-time) 40))
           (second-time (+ first-time 10))
           (special-key "Room, A=B\\field")
           (special-value "a, quote\" and slash\\")
           (range (persp:make-absolute-range (- first-time 1) (+ second-time 1))))
      (write-table-point cut item
                         (table-value "Temp" 30 "temp" 20 "Removed" 10
                                      special-key special-value "Label" "first"
                                      "Empty" "" "On" 'item:true "Off" 'item:false)
                         first-time)
      (write-table-point cut item
                         (table-value "Temp" 30.5 "temp" 22
                                      special-key special-value "Label" "second"
                                      "Empty" "" "On" 'item:false "Off" 'item:true)
                         second-time)
      ;; Inspect the HTTP response itself: Lisp must not receive old snapshots.
      (multiple-value-bind (body status)
          (chipi.influx-persistence::%make-query-request
           cut (act-cell:name item) nil nil t)
        (is (= 200 status))
        (let ((rows (chipi.influx-persistence::%parse-csv-rows body)))
          (is (= 7 (length rows)))
          (dolist (row rows)
            (is (= second-time
                   (chipi.influx-persistence::%parse-timestamp
                    (chipi.influx-persistence::influx-row-timestamp row))))
            (is (not (string= "Removed" (chipi.influx-persistence::influx-row-field row)))))))
      (let* ((latest (persp:retrieve cut item))
             (value (persisted-item-value latest)))
        (is (= second-time (persisted-item-timestamp latest)))
        (is (= 7 (hash-table-count value)))
        (is (= 30.5 (gethash "Temp" value)))
        (is (= 22 (gethash "temp" value)))
        (is (string= special-value (gethash special-key value)))
        (is (string= "" (gethash "Empty" value)))
        (is (eq 'item:false (gethash "On" value)))
        (is (eq 'item:true (gethash "Off" value)))
        (is (not (nth-value 1 (gethash "Removed" value)))))
      (let ((history (persp:retrieve-range cut item range)))
        (is (= 2 (length history)))
        (is (= first-time (persisted-item-timestamp (first history))))
        (is (= second-time (persisted-item-timestamp (second history))))
        (is (= 10 (gethash "Removed" (persisted-item-value (first history)))))
        (is (not (nth-value 1 (gethash "Removed" (persisted-item-value (second history)))))))
      (loop :for aggregate :in '(:avg :min :max :sum :median :count)
            :for expected :in '(30.25 30 30.5 60.5 30.25 2)
            :do (let* ((results (persp:retrieve-range cut item range aggregate))
                       (entry (first results))
                       (table (persisted-item-value entry)))
                  (is (= 1 (length results)))
                  (is (eq :undefined (persisted-item-timestamp entry)))
                  (is (= expected (gethash "Temp" table)))
                  (if (eq aggregate :count)
                      (progn
                        (is (= 8 (hash-table-count table)))
                        (is (= 2 (gethash "Label" table)))
                        (is (= 2 (gethash "On" table)))
                        (is (= 1 (gethash "Removed" table))))
                      (progn
                        (is (= 3 (hash-table-count table)))
                        (is (not (nth-value 1 (gethash "Label" table))))) ))))))

(test retrieve--table-no-data-preserves-startup-state
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'table-no-data :type-hint 'table
                                 :initial-value (table-value "Initial" 7))))
      (is (eq :error (car (persp:retrieve cut item))))
      (multiple-value-bind (body status)
          (chipi.influx-persistence::%make-query-request
           cut (act-cell:name item) nil nil t)
        (is (= 200 status))
        (is (null (chipi.influx-persistence::%parse-csv-rows body))))
      (is (null (persp:retrieve-range cut item (persp:make-relative-range :seconds 60))))
      (is (null (persp:retrieve-range cut item (persp:make-relative-range :seconds 60) :avg)))
      (is (= 7 (gethash "Initial" (item:item-state-value (item:get-item-stateq item))))))))

(test retrieve--reserved-looking-table-keys-roundtrip
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (make-test-item 'reserved-keys :type-hint 'table)))
      (write-table-point cut item
                         (table-value "_field" "field" "_measurement" "measurement"
                                      "_value" "_value" "Time" 1)
                         (- (get-universal-time) 1))
      (let ((value (persisted-item-value (persp:retrieve cut item))))
        (is (= 4 (hash-table-count value)))
        (is (string= "field" (gethash "_field" value)))
        (is (string= "measurement" (gethash "_measurement" value)))
        (is (string= "_value" (gethash "_value" value)))
        (is (= 1 (gethash "Time" value)))))))

(test retrieve--canonical-table-numbers-do-not-republish-original-reading
  (with-fixture init-destroy-env ()
    (let* ((cut (make-cut))
           (original (table-value "Temperature" 29.08d0 "Other" 21.1d0
                                  "Count" 16777217 "NegativeCount" -16777217
                                  "Epoch" 1763370000 "NegativeEpoch" -1763370000
                                  "ExactDouble" 9007199254740992
                                  "NegativeExactDouble" -9007199254740992
                                  "Battery" 87 "Whole" 21
                                  "Max" most-positive-single-float
                                  "Min" least-positive-single-float))
           (item (make-test-item 'canonical-numbers :type-hint 'table :initial-value original))
           (stored (item:item-state-value (item:get-item-stateq item)))
           (events 0))
      (is (typep (gethash "Count" stored) 'integer))
      (is (typep (gethash "Temperature" stored) 'single-float))
      (write-table-point cut item stored (- (get-universal-time) 1))
      (let ((restored (persisted-item-value (persp:retrieve cut item))))
        (is (equalp stored restored))
        (dolist (key '("Count" "NegativeCount" "Epoch" "NegativeEpoch"
                       "ExactDouble" "NegativeExactDouble" "Battery" "Whole"))
          (is (integerp (gethash key restored)))
          (is (= (gethash key original) (gethash key restored)))))
      (item:set-value item (table-value) :push nil :persist nil)
      (let ((barrier (item:get-value item)))
        (is-true (miscutils:await-cond 1
                   (let ((value (future:fresult barrier)))
                     (and (hash-table-p value) (zerop (hash-table-count value)))))))
      (with-mocks ()
        (answer ev:publish (incf events))
        (answer persp:store t)
        (item:add-persistence item cut :load-on-start t :frequency :every-change)
        (is-true (miscutils:await-cond 2
                   (equalp stored (item:item-state-value (item:get-item-stateq item)))))
        (let ((barrier (item:get-value item)))
          (is-true (miscutils:await-cond 1 (hash-table-p (future:fresult barrier)))))
        (is (= 1 events))
        (is (null (invocations 'persp:store)))
        (let ((timestamp (item:item-state-timestamp (item:get-item-stateq item))))
          (setf (gethash "Battery" original) 87.0
                (gethash "Whole" original) 21.0)
          (item:set-value item original :timestamp (1+ timestamp))
          (let ((barrier (item:get-value item)))
            (is-true (miscutils:await-cond 1 (hash-table-p (future:fresult barrier)))))
          (is (= timestamp (item:item-state-timestamp (item:get-item-stateq item))))
          (is (= 1 events))
          (is (null (invocations 'persp:store))))))))
