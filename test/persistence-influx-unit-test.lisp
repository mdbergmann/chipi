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

(defun annotated-csv (datatype rows)
  (cl-csv:write-csv
   (append (list (list "#datatype" "string" "long" "dateTime:RFC3339" "string" datatype)
                 '("#group" "false" "false" "false" "true" "false")
                 '("#default" "_result" "" "" "" "")
                 '("" "result" "table" "_time" "_field" "_value"))
           rows)))

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
  (let* ((csv (cl-csv:write-csv
               '(("#datatype" "string" "long" "string" "long")
                 ("" "result" "table" "_field" "_value")
                 ("" "_result" "0" "Label" "2")
                 ("" "_result" "1" "On" "3"))))
         (items (influx::%parsed-range-persisted-item csv 'table)))
    (is (= 1 (length items)))
    (is (eq :undefined (persp:persisted-item-timestamp (first items))))
    (is (= 2 (gethash "Label" (persp:persisted-item-value (first items)))))
    (is (= 3 (gethash "On" (persp:persisted-item-value (first items)))))))

(test parse-csv--no-data-and-scalar-quoted-strings
  (is (null (influx::%parsed-range-persisted-item "" 'table)))
  (signals error (influx::%parsed-last-persisted-item "" 'table))
  (let ((items (influx::%parsed-range-persisted-item
                (annotated-csv "string"
                               '(("" "" "0" "2026-11-15T12:00:00Z" "value" "a,b\"c")))
                'string)))
    (is (string= "a,b\"c" (persp:persisted-item-value (first items))))))

(test parse-csv--reserved-looking-field-and-value-are-data
  (let* ((csv (annotated-csv
               "string"
               '(("" "" "0" "2026-11-15T12:00:00Z" "_value" "_value")
                 ("" "" "1" "2026-11-15T12:00:00Z" "spaced" " tail "))))
         (table (persp:persisted-item-value
                 (first (influx::%parsed-range-persisted-item csv 'table)))))
    (is (string= "_value" (gethash "_value" table)))
    (is (string= " tail " (gethash "spaced" table)))))
