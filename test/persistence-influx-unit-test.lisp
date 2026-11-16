(defpackage :chipi.influx-persistence-unit-test
  (:use :cl :fiveam :cl-mock)
  (:local-nicknames (:influx :chipi.influx-persistence)))
(in-package :chipi.influx-persistence-unit-test)

(def-suite influx-persistence-unit-tests :in chipi.tests:test-suite)
(in-suite influx-persistence-unit-tests)

(defun table (&rest fields)
  (alexandria:plist-hash-table fields :test 'equal))

(test make-single-query-string--selects-latest-table-time-on-server
  (let* ((persistence (influx:make-influx-persistence
                       :query-test :base-url "http://localhost" :token ""
                       :org "test" :bucket "test"))
         (base (influx::%make-range-query-string persistence "sensor" nil))
         (query (influx::%make-single-query-string persistence "sensor" t)))
    (is (search "|> keep(columns: [\"_time\"])" query))
    (is (search "|> group(columns: [])" query))
    (is (search "|> sort(columns: [\"_time\"], desc: true)" query))
    (is (search "|> limit(n: 1)" query))
    (is (search "|> findColumn(fn: (key) => true, column: \"_time\")" query))
    (is (search "contains(value: r._time, set: latestTimes)" query))
    (is (string= (format nil "~a~%|> last()" base)
                 (influx::%make-single-query-string persistence "sensor")))))

(test serialize-point--typed-and-escaped-fields
  (let ((line (influx::%serialize-point
               "Room, A=B" (table "Temp" 30 "temp" 30.5 "On" 'item:true
                                     "Off" 'item:false "a, b=c" "quote\" slash\\") 123)))
    (is (search "Room\\,\\ A=B,item=Room\\,\\ A\\=B " line))
    (is (search "Temp=30.0" line))
    (is (search "temp=30.5" line))
    (is (search "On=true" line))
    (is (search "Off=false" line))
    (is (search "a\\,\\ b\\=c=\"quote\\\" slash\\\\\"" line))
    (is (alexandria:ends-with-subseq " 123" line))))

(test serialize-point--invalid-snapshot-never-calls-http
  (with-mocks ()
    (answer (drakma:http-request) (error "Unexpected HTTP request: ~a" :invalid))
    (signals item:invalid-table-value
      (influx::%make-write-request nil "name" (table "ok" 1 "bad" '(1 2)) 123))
    (signals error
      (influx::%make-write-request nil "name" (table "multiline" (format nil "a~%b")) 123))
    (signals error
      (influx::%make-write-request nil "name" (table "" 1) 123))
    (signals error
      (influx::%make-write-request nil "name" (table "trailing\\" 1) 123))
    (signals error
      (influx::%make-write-request nil "name" (table "time" 1) 123))
    (is (null (invocations 'drakma:http-request)))))

(test serialize-point--empty-snapshot-skips-http
  (with-mocks ()
    (answer (drakma:http-request) (error "Unexpected HTTP request: ~a" :empty))
    (is (eq :skipped (nth-value 1
                               (influx::%make-write-request nil "name" (table) 123))))
    (is (null (invocations 'drakma:http-request)))))

(defun csv-fixture (rows)
  ;; Quote every fixture cell; parser edge cases below use literal input.
  (with-output-to-string (output)
    (dolist (row rows)
      (loop :for value :in row
            :for first-p = t :then nil
            :do (unless first-p (write-char #\, output))
                (write-char #\" output)
                (loop :for character :across value
                      :do (write-char character output)
                          (when (char= character #\") (write-char character output)))
                (write-char #\" output))
      (terpri output))))

(defun annotated-csv (datatype rows)
  (csv-fixture
   (append (list (list "#datatype" "string" "long" "dateTime:RFC3339" "string" datatype)
                 '("#group" "false" "false" "false" "true" "false")
                 '("#default" "_result" "" "" "" "")
                 '("" "result" "table" "_time" "_field" "_value"))
           rows)))

(test parse-csv--table-numbers-restore-without-republishing
  (unwind-protect
       (let* ((stored (table "Battery" 87 "Temperature" 21.3))
              (csv (annotated-csv
                    "double"
                    '(("" "" "0" "2026-11-15T12:00:00Z" "Battery" "87")
                      ("" "" "0" "2026-11-15T12:00:00Z" "Temperature" "21.3"))))
              (restored (influx::%parsed-last-persisted-item csv 'table))
              (persistence (influx:make-influx-persistence
                            :numeric-restore-persistence :base-url "http://localhost" :token ""
                            :org "test" :bucket "test"))
              (item (item:make-item 'numeric-restore :type-hint 'table))
              (events 0)
              (subscribed nil))
         (is (equalp stored (persp:persisted-item-value restored)))
         (maphash (lambda (key value)
                    (declare (ignore key))
                    (is (typep value 'single-float)))
                  (persp:persisted-item-value restored))
         (ac:actor-of (isys:ensure-isys)
                      :init (lambda (self)
                              (ev:subscribe self self 'item:item-changed-event)
                              (setf subscribed t))
                      :receive (lambda (msg)
                                 (when (typep msg 'item:item-changed-event)
                                   (incf events))))
         (is-true (miscutils:await-cond 1 subscribed))
         (with-mocks ()
           (answer influx::%make-query-request (values csv 200))
           (answer persp:store t)
           (item:add-persistence item persistence :load-on-start t)
           (is-true (miscutils:await-cond 2
                      (equalp stored (item:item-state-value (item:get-item-stateq item)))))
           ;; Restore may publish an event; only subsequent events count here.
           (sleep 0.1)
           (let ((events-after-restore events)
                 (stores-after-restore (length (invocations 'persp:store)))
                 (timestamp (item:item-state-timestamp (item:get-item-stateq item))))
             (item:set-value item stored :timestamp (1+ timestamp))
             (item:get-item-stateq item)
             (sleep 0.1)
             (is (= timestamp (item:item-state-timestamp (item:get-item-stateq item))))
             (is (= events-after-restore events))
             (is (= stores-after-restore (length (invocations 'persp:store)))))))
    (envi:shutdown-env)))

(test parse-csv--multiple-tables-types-multiline-and-empty-strings
  (let* ((old "2026-11-15T12:00:00Z")
         (new "2026-11-15T12:00:01Z")
         (text (format nil "comma, quote\"~%next line"))
         (csv (concatenate
               'string
               (annotated-csv "double" (list (list "" "" "0" new "Temp" "30.5")
                                             (list "" "" "0" old "Removed" "10")))
               (format nil "~%")
               (annotated-csv "string" (list (list "" "" "1" old "Label" text)
                                             (list "" "" "1" new "Label" "")))
               (annotated-csv "boolean" (list (list "" "" "2" old "On" "true")
                                              (list "" "" "2" new "On" "false")))))
         (items (influx::%parsed-range-persisted-item csv 'table))
         (first (persp:persisted-item-value (first items)))
         (last (persp:persisted-item-value (second items))))
    (is (= 2 (length items)))
    (is (string= text (gethash "Label" first)))
    (is (eq 'item:true (gethash "On" first)))
    (is (= 30.5 (gethash "Temp" last)))
    (is (string= "" (gethash "Label" last)))
    (is (eq 'item:false (gethash "On" last)))
    (is (not (nth-value 1 (gethash "Removed" last))))
    (is (= 3 (hash-table-count
              (persp:persisted-item-value
               (influx::%parsed-last-persisted-item csv 'table)))))))

(test parse-csv--aggregate-datatypes-without-timestamps
  (let* ((csv (csv-fixture
               '(("#datatype" "string" "long" "string" "long")
                 ("" "result" "table" "_field" "_value")
                 ("" "_result" "0" "Label" "2")
                 ("" "_result" "1" "On" "3"))))
         (items (influx::%parsed-range-persisted-item csv 'table)))
    (is (= 1 (length items)))
    (is (eq :undefined (persp:persisted-item-timestamp (first items))))
    (is (= 2 (gethash "Label" (persp:persisted-item-value (first items)))))
    (is (= 3 (gethash "On" (persp:persisted-item-value (first items)))))))

(test parse-csv--no-table-data
  (is (null (influx::%parsed-range-persisted-item "" 'table)))
  (signals error (influx::%parsed-last-persisted-item "" 'table)))

(test make-query-request--scalar-flux-and-table-annotated-json
  (let ((persistence (influx:make-influx-persistence
                      :request-test :base-url "http://localhost" :token "token"
                      :org "test" :bucket "test")))
    (dolist (table-p '(nil t))
      (dolist (range (list nil (persp:make-relative-range :hours 1)))
        (with-mocks ()
          (if-called 'drakma:http-request
           (lambda (url &rest arguments)
            (is (string= "http://localhost/api/v2/query" url))
            (let ((content (getf arguments :content))
                  (query (if range
                             (influx::%make-range-query-string persistence "sensor" range nil table-p)
                             (influx::%make-single-query-string persistence "sensor" table-p))))
              (if table-p
                  (let ((body (com.inuoe.jzon:parse content)))
                    (is (string= "application/json" (getf arguments :content-type)))
                    (is (string= query (gethash "query" body)))
                    (is (equalp #("datatype" "group" "default")
                                (gethash "annotations" (gethash "dialect" body)))))
                  (progn
                    (is (string= "application/vnd.flux" (getf arguments :content-type)))
                    (is (string= query content)))))
            (values "" 200)))
          (influx::%make-query-request persistence "sensor" range nil table-p)
          (is (= 1 (length (invocations 'drakma:http-request)))))))))

(test parse-csv--scalar-retains-main-parser-and-response-order
  (let* ((csv (format nil ",result,table,_time,_value~c~c,_result,0,2026-11-15T12:00:01Z,2~c~c,_result,0,2026-11-15T12:00:00Z,1~c~c"
                      #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed))
         (items (influx::%parsed-range-persisted-item csv 'integer)))
    (is (equal '(2 1) (mapcar #'persp:persisted-item-value items)))
    (is (= 1 (persp:persisted-item-value
              (influx::%parsed-last-persisted-item csv 'integer)))))
  (is (equal '((nil . "3"))
             (influx::%parse-csv-to-time-value-pairs
              (format nil ",result,table,_value~c~c,_result,0,3~c~c"
                      #\Return #\Linefeed #\Return #\Linefeed))))
  ;; Main splits cells directly; quoted scalar values retain that behavior.
  (is (equal '(("timestamp" . "\"a"))
             (influx::%parse-csv-lines '(",_time,_value" ",timestamp,\"a,b\""))))
  (is (null (influx::%parse-csv-to-time-value-pairs "")))
  (is (null (influx::%parse-csv-to-time-value-pairs nil))))

(test parse-csv--reserved-looking-field-and-value-are-data
  (let* ((csv (annotated-csv
               "string"
               '(("" "" "0" "2026-11-15T12:00:00Z" "_value" "_value")
                 ("" "" "1" "2026-11-15T12:00:00Z" "spaced" " tail "))))
         (table (persp:persisted-item-value
                 (first (influx::%parsed-range-persisted-item csv 'table)))))
    (is (string= "_value" (gethash "_value" table)))
    (is (string= " tail " (gethash "spaced" table)))))

(test read-csv-records--quoted-empty-multiline-and-significant-whitespace
  (let* ((multiline (format nil "first~c~csecond~%third" #\Return #\Newline))
         (csv (format nil "plain,\"a,b\",\"say \"\"hi\"\"\",,\"\", tail ,\" padded \"~c~c\"~a\",end~%last,"
                      #\Return #\Newline multiline)))
    (is (equal (list '("plain" "a,b" "say \"hi\"" "" "" " tail " " padded ")
                     (list multiline "end") '("last" ""))
               (influx::%read-csv-records csv))))
  (is (null (influx::%read-csv-records "")))
  (is (equal '(("")) (influx::%read-csv-records (format nil "~%")))))

(test read-csv-records--malformed-quotes-signal-errors
  (dolist (csv '("\"unfinished" "unquoted\"quote" "\"closed\"tail" "\"escaped\"\""))
    (signals error (influx::%read-csv-records csv))))
