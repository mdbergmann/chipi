(defpackage :chipi.influx-persistence
  (:use :cl :chipi.persistence)
  (:nicknames :influx-persistence)
  (:import-from #:persp
                #:base-historic-persistence)
  (:export #:influx-persistence
           #:make-influx-persistence)
  )

(in-package :chipi.influx-persistence)

(defclass influx-persistence (base-historic-persistence)
  ((base-url :initarg :base-url
             :initform nil
             :reader base-url)
   (token :initarg :token
          :initform nil
          :reader token)
   (org :initarg :org
        :initform nil
        :reader org)
   (bucket :initarg :bucket
           :initform nil
           :reader bucket)
   (precision :initarg :precision
              :initform "s"
              :reader precision))
  (:documentation "Influx persistence implementation.
Influx persistence supports the following value types:
- string
- integer
- float
- boolean, for boolean values, use the symbols 'item:true and 'item:false.
- hash-table snapshots with string keys and finite real, string or boolean fields.
Table numbers use Influx floats. Influx skips empty table snapshots, so restore
returns the last nonempty snapshot. Simple file persistence stores and restores
empty snapshots as well. Line protocol cannot
represent CR/LF, empty field names, the reserved field name time, or names ending
with a backslash.

Other value types are not supported.
Specify those types as 'type-hint' in the item definition."))

(defun make-influx-persistence (id &key
                                     base-url
                                     token
                                     org
                                     bucket)
  (persp::make-persistence id
                           :type 'influx-persistence
                           :base-url base-url
                           :token token
                           :org org
                           :bucket bucket))

(defmethod initialize ((persistence influx-persistence))
  (log:info "Initializing persistence: ~a" persistence))

(defmethod shutdown ((persistence influx-persistence))
  (log:info "Shutting down persistence: ~a" persistence))

;; ---------------------------------------
;; Writing
;; ---------------------------------------

(defun %check-line-text (text)
  (when (find-if (lambda (character) (find character '(#\Return #\Newline))) text)
    (error "Influx line protocol does not support CR/LF in names or strings: ~s" text))
  text)

(defun %escape-line-name (name &optional measurement-p)
  "Escape a measurement, tag value or field key without changing its case."
  (with-output-to-string (stream)
    (%check-line-text name)
    (when (and (plusp (length name)) (char= (char name (1- (length name))) #\\))
      (error "Influx line protocol cannot delimit a name ending with backslash: ~s" name))
    (loop :for character :across name
          :do (when (or (find character " ,")
                        (and (not measurement-p) (char= character #\=)))
                (write-char #\\ stream))
              (write-char character stream))))

(defun %line-string (value)
  (%check-line-text value)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop :for character :across value
          :do (when (find character '(#\\ #\"))
                (write-char #\\ stream))
              (write-char character stream))
    (write-char #\" stream)))

(defun %line-value (value &optional table-p)
  (cond
    ((stringp value) (%line-string value))
    ((eq value 'item:true) "true")
    ((eq value 'item:false) "false")
    ((and (integerp value) (not table-p)) (format nil "~di" value))
    ((realp value)
     ;; Fixed-point formatting avoids Lisp exponent markers and always writes
     ;; table numbers as Influx floats, even when the observation is integral.
     (format nil "~f" (if table-p (coerce value 'double-float) value)))
    (t (error "Unsupported item value type: ~a" (type-of value)))))

(defun %serialize-point (item-name value timestamp)
  "Validate a complete snapshot before producing any line protocol."
  (let ((value (if (hash-table-p value) (item:copy-table-value value) value)))
    (when (and (hash-table-p value) (zerop (hash-table-count value)))
      (return-from %serialize-point nil))
    (with-output-to-string (stream)
      (format stream "~a,item=~a " (%escape-line-name item-name t)
              (%escape-line-name item-name))
      (if (hash-table-p value)
          (loop :for key :in (sort (alexandria:hash-table-keys value) #'string<)
                :for first-p = t :then nil
                :do (when (zerop (length key))
                      (error "Influx line protocol does not support an empty field name: ~s" key))
                    (when (string= key "time")
                      (error "Influx silently discards the reserved field name: ~s" key))
                    (unless first-p (write-char #\, stream))
                    (format stream "~a=~a" (%escape-line-name key)
                            (%line-value (gethash key value) t)))
          (format stream "value=~a" (%line-value value)))
      (format stream " ~d" timestamp))))

(defun %make-write-request (persistence item-name item-value item-timestamp)
  (let ((data-string (%serialize-point item-name item-value item-timestamp)))
    (unless data-string
      (log:debug "Skipping empty table snapshot for item: ~a" item-name)
      (return-from %make-write-request (values nil :skipped)))
    (log:debug "Write request data string: ~a" data-string)
    (drakma:http-request (format nil "~a/api/v2/write" (base-url persistence))
                        :method :post
                        :parameters `(("bucket" . ,(bucket persistence))
                                      ("org" . ,(org persistence))
                                      ("precision" . ,(precision persistence)))
                        :accept "application/json"
                        :additional-headers
                        `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                        :content-type "text/plain; charset=utf-8"
                        :content data-string)))

(defmethod persist ((persistence influx-persistence) item)
  (log:debug "Persisting item: ~a" (item:name item))
  (let* ((item-name (act-cell:name item))
         (item-state (item:get-item-stateq item))
         (item-value (item:item-state-value item-state))
         (item-timestamp (local-time:universal-to-timestamp
                          (item:item-state-timestamp item-state)))
         (unix-timestamp (local-time:timestamp-to-unix item-timestamp)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-write-request persistence item-name item-value unix-timestamp)
          (case status
            (:skipped nil)
            (204 (log:info "Persisted item OK: ~a, timestamp: ~a" item-name item-timestamp))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to persist item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to persist item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to persist item: ~a, with error: ~a" item e)))))

;; ---------------------------------------
;; Querying
;; ---------------------------------------

(defun %universal-to-unix-ts (uts)
  (local-time:timestamp-to-unix
   (local-time:universal-to-timestamp uts)))

(defun %%absolute-range-to-string (start-ts end-ts)
  (let ((end-unix-ts (%universal-to-unix-ts end-ts))
        (start-unix-ts (%universal-to-unix-ts start-ts)))
    (format nil "start: ~a, stop: ~a" start-unix-ts end-unix-ts)))

(defun %relative-range-to-string (range)
  (let ((days (persp:days range))
        (hours (persp:hours range))
        (minutes (persp:minutes range))
        (seconds (persp:seconds range))
        (end-ts (persp:end-ts range)))
    (flet ((absolute ()
             (let ((start-ts
                     (cond
                       (days (- end-ts (* days 24 60 60)))
                       (hours (- end-ts (* hours 60 60)))
                       (minutes (- end-ts (* minutes 60)))
                       (seconds (- end-ts seconds))
                       (t end-ts))))
               (%%absolute-range-to-string start-ts end-ts)))
           (relative ()
             (format nil "start: ~a"
                     (cond
                       (days (format nil "-~ad" days))
                       (hours (format nil "-~ah" hours))
                       (minutes (format nil "-~am" minutes))
                       (seconds (format nil "-~as" seconds))
                       (t "0")))))
      (if end-ts
          (absolute)
          (relative)))))

(defun %absolute-range-to-string (range)
  (%%absolute-range-to-string (persp:start-ts range)
                              (persp:end-ts range)))

(defun %range-to-string (range)
  (if (not range) "0"
      (typecase range
        (relative-range (%relative-range-to-string range))
        (absolute-range (%absolute-range-to-string range)))))

(defun %flux-string (value)
  ;; JSON and Flux use the same quoted string escapes for these names.
  (com.inuoe.jzon:stringify value))

(defun %make-single-query-string (persistence item-name &optional table-p)
  (let ((query (%make-range-query-string persistence item-name nil)))
    (if table-p
        ;; Group timestamps alone to avoid merging heterogeneous _value columns.
        ;; Keep the original typed tables and omit fields absent at the newest time.
        (format nil "observations = ~a~%
latestTimes = observations
|> keep(columns: [\"_time\"])
|> group(columns: [])
|> sort(columns: [\"_time\"], desc: true)
|> limit(n: 1)
|> findColumn(fn: (key) => true, column: \"_time\")~%
observations
|> filter(fn: (r) => contains(value: r._time, set: latestTimes))" query)
        (format nil "~a~%|> last()" query))))

(defun %make-range-query-string (persistence item-name range &optional aggregate table-p)
  (let ((base-pipe
          (format nil "from(bucket:~a)~%|> range(~a)~%|> filter(fn: (r) => r._measurement == ~a)"
                  (%flux-string (bucket persistence))
                  (if range (%range-to-string range) "start: 0")
                  (%flux-string item-name))))
    (if aggregate
        (format nil "~a~a~%|> ~a()~a"
                (if (and table-p (not (eq aggregate :count)))
                    (format nil "import \"types\"~%") "")
                (concatenate 'string base-pipe
                             (if (and table-p (not (eq aggregate :count)))
                                 (format nil "~%|> filter(fn: (r) => types.isNumeric(v: r._value))")
                                 ""))
                (ecase aggregate
                  (:avg "mean") (:min "min") (:max "max")
                  (:sum "sum") (:count "count") (:median "median"))
                (if table-p (format nil "~%|> drop(columns: [\"_time\"])") ""))
        base-pipe)))

(defvar *supported-influx-aggregate-funs* '(:avg :min :max :sum :count :median))

(defun %make-query-request (persistence item-name &optional range aggregate table-p)
  (let ((data-string (if range
                         (%make-range-query-string persistence item-name range aggregate table-p)
                         (%make-single-query-string persistence item-name table-p))))
    (log:debug "Query request data string: ~a" data-string)
    (drakma:http-request (format nil "~a/api/v2/query" (base-url persistence))
                         :method :post
                         :parameters `(("org" . ,(org persistence)))
                         :accept "application/json"
                         :additional-headers
                         `(("Authorization" . ,(format nil "Token ~a" (token persistence))))
                         :content-type (if table-p "application/json" "application/vnd.flux")
                         :content (if table-p
                                      (com.inuoe.jzon:stringify
                                       (alexandria:plist-hash-table
                                        (list "query" data-string
                                              "dialect" (alexandria:plist-hash-table
                                                         (list "annotations"
                                                               #("datatype" "group" "default"))
                                                         :test 'equal))
                                        :test 'equal))
                                      data-string))))

(defstruct influx-row timestamp field value datatype)

(defun %csv-text (body)
  (etypecase body
    (null "")
    (string body)
    ((vector (unsigned-byte 8)) (babel:octets-to-string body :encoding :utf-8))))

(defun %read-csv-records (text)
  "Read CSV records without trimming cells or changing quoted line endings."
  (with-input-from-string (input text)
    (let ((cell (make-string-output-stream))
          (state :start) (row nil) (rows nil) (pending-p nil))
      (labels ((finish-cell ()
                 (push (get-output-stream-string cell) row)
                 (setf state :start))
               (finish-row ()
                 (finish-cell)
                 (push (nreverse row) rows)
                 (setf row nil pending-p nil)))
        (loop :for character = (read-char input nil nil)
              :while character
              :do (setf pending-p t)
                  (cond
                    ((eq state :quoted)
                     (if (char= character #\")
                         (setf state :closed)
                         (write-char character cell)))
                    ((char= character #\")
                     (ecase state
                       (:start (setf state :quoted))
                       (:closed (write-char character cell) (setf state :quoted))
                       (:unquoted (error "Quote inside unquoted CSV cell: ~s" text))))
                    ((char= character #\,)
                     (finish-cell))
                    ((or (char= character #\Return) (char= character #\Newline))
                     (when (and (char= character #\Return)
                                (eql (peek-char nil input nil nil) #\Newline))
                       (read-char input))
                     (finish-row))
                    ((eq state :closed)
                     (error "Unexpected character after closing CSV quote: ~s" character))
                    (t (write-char character cell) (setf state :unquoted))))
        (when (eq state :quoted)
          (error "Unterminated quoted CSV cell: ~s" text))
        (when pending-p (finish-row))
        (nreverse rows)))))

(defun %parse-csv-rows (body)
  "Read annotated Flux CSV, retaining blank strings and multiline quoted cells."
  (let ((headers nil) (datatypes nil) (result nil))
    (dolist (row (%read-csv-records (%csv-text body)))
      (cond
        ((string= (first row) "#datatype") (setf datatypes row headers nil))
        ((and (first row) (alexandria:starts-with #\# (first row))) nil)
        ((every (lambda (cell) (or (null cell) (string= cell ""))) row) nil)
        ((and (equal (second row) "result") (equal (third row) "table")
              (member "_value" row :test #'equal))
         (setf headers row))
        (headers
         (flet ((cell (name)
                  (let ((index (position name headers :test #'equal)))
                    (and index (nth index row)))))
           (let ((index (position "_value" headers :test #'equal))
                 (timestamp (cell "_time")))
             (push (make-influx-row
                    :timestamp (unless (or (null timestamp) (string= timestamp "")) timestamp)
                    :field (cell "_field") :value (cell "_value")
                    :datatype (nth index datatypes)) result))))))
    (nreverse result)))

(defun %parse-csv-to-time-value-pairs (body)
  "Parses influx output csv.
Returns a list of `(timestamp . value) pairs.
An empty response body (the query yielded no data) parses to an empty list.
Applied aggregate functions omit the _time column, in this case the timestamo in return as `NIL`."
  (let ((csv-lines (and body
                        (str:split (format nil "~c~c" #\return #\linefeed) body
                                   :omit-nulls t))))
    (if (null csv-lines)
        '()
        (%parse-csv-lines csv-lines))))

(defun %parse-csv-lines (csv-lines)
  (let* ((csv-headers (str:split "," (first csv-lines) :omit-nulls t))
         (index-time (position "_time" csv-headers :test #'equal))
         (index-value (position "_value" csv-headers :test #'equal))
         (csv-columns
           (mapcar (lambda (s) (str:split "," s :omit-nulls t))
                   (rest csv-lines)))
         (time-value-pairs
           (mapcar (lambda (row)
                     (cons (and index-time
                                (nth index-time row))
                           (nth index-value row)))
                   csv-columns)))
    time-value-pairs))

(defun %typed-csv-value (row)
  (let ((value (influx-row-value row)) (datatype (influx-row-datatype row)))
    (cond
      ((equal datatype "string") value)
      ((equal datatype "boolean")
       (cond ((string= value "true") 'item:true)
             ((string= value "false") 'item:false)
             (t (error "Invalid CSV boolean: ~s" value))))
      ((member datatype '("long" "unsignedLong") :test #'equal) (parse-integer value))
      ((equal datatype "double") (parse-float:parse-float value :type 'single-float))
      (t (error "Unsupported CSV datatype: ~s" datatype)))))

(defun %table-persisted-items (body)
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (row (%parse-csv-rows body))
      (let* ((timestamp (influx-row-timestamp row))
             (table (or (gethash timestamp groups)
                        (setf (gethash timestamp groups) (make-hash-table :test 'equal)))))
        (setf (gethash (influx-row-field row) table) (%typed-csv-value row))))
    (sort (loop :for timestamp :being :the :hash-keys :of groups
                :using (hash-value table)
                :collect (make-persisted-item
                          :value table :timestamp (%parse-timestamp timestamp)))
          #'< :key (lambda (entry)
                     (let ((timestamp (persisted-item-timestamp entry)))
                       (if (eq timestamp :undefined) 0 timestamp))))))

(defun %parse-timestamp (timestamp)
  (if timestamp
      (local-time:timestamp-to-universal (local-time:parse-timestring timestamp))
      :undefined))

(defun %make-persisted-item (timestamp value type-hint)
  "Returns a persisted-item instance.
`timestamp' may be `NIL' which is transformed to `:undefined'."
  (make-persisted-item
   :value (cond
            ((eq type-hint 'integer)
             (parse-integer value))
            ((eq type-hint 'float)
             (coerce (parse-float:parse-float value) 'single-float))
            ((eq type-hint 'boolean)
             (if (string= "true" value)
                 'item:true
                 'item:false))
            ((eq type-hint 'string)
             value)
            (t (error "Unsupported type: ~a" type-hint)))
   :timestamp (if timestamp
                  (local-time:timestamp-to-universal
                   (local-time:parse-timestring timestamp))
                  :undefined)))

;; -------------------------------------
;; Retrieve last
;; -------------------------------------

(defun %parsed-last-persisted-item (response-body type-hint)
  (if (item:table-type-hint-p type-hint)
      (or (car (last (%table-persisted-items response-body)))
          (error "No persisted value for type ~a" type-hint))
      (let ((pairs (%parse-csv-to-time-value-pairs response-body)))
        (log:debug "Parsed pairs: ~a" pairs)
        (let* ((last-pair (first (last pairs)))
               (timestamp (car last-pair))
               (value (cdr last-pair))
               (persisted-item (%make-persisted-item timestamp value type-hint)))
          (log:debug "Loaded persisted item: ~a" persisted-item)
          (log:trace "Loaded persisted item value type: ~a" (type-of (persisted-item-value persisted-item)))
          persisted-item))))

(defmethod retrieve ((persistence influx-persistence) item)
  "Output format of influxdb is csv, so we need to parse it."
  (log:debug "Reading item: ~a" item)
  (let ((item-name (act-cell:name item))
        (type-hint (item:value-type-hint item)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-query-request persistence item-name nil nil
                                 (item:table-type-hint-p type-hint))
          (case status
            (200
             (progn
               (log:info "Read item OK: ~a" item)
               (log:debug "Response: ~a" body)
               (%parsed-last-persisted-item body type-hint)))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to read item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to read item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to read item: ~a, with error: ~a" item e)
        `(:error . ,e)))))

;; ------------------------------------------
;; Range retrieval
;; ------------------------------------------

(defun %parsed-range-persisted-item (response-body type-hint)
  (if (item:table-type-hint-p type-hint)
      (%table-persisted-items response-body)
      (mapcar (lambda (pair)
                (%make-persisted-item (car pair) (cdr pair) type-hint))
              (%parse-csv-to-time-value-pairs response-body))))

(defmethod retrieve-range ((persistence influx-persistence) item range &optional aggregate)
  (log:debug "Reading item with range: ~a, aggregate: ~a" item aggregate)
  (assert (or (null aggregate)
              (member aggregate *supported-influx-aggregate-funs*)))
  (let ((item-name (act-cell:name item))
        (type-hint (item:value-type-hint item)))
    (handler-case
        (multiple-value-bind (body status headers)
            (%make-query-request persistence item-name range aggregate
                                 (item:table-type-hint-p type-hint))
          (case status
            (200
             (progn
               (log:info "Read item OK: ~a" item)
               (log:debug "Response: ~a" body)
               (%parsed-range-persisted-item body type-hint)))
            (t
             (let ((message (babel:octets-to-string body)))
               (log:warn "Failed to read item: ~a" item)
               (log:warn "Response: ~a" message)
               (log:warn "Status: ~a" status)
               (log:warn "Headers: ~a" headers)
               (error "Failed to read item, status: ~a, message: ~a"
                      status message)))))
      (error (e)
        (log:warn "Failed to read item with range: ~a, with error: ~a" item e)
        `(:error . ,e)))))
