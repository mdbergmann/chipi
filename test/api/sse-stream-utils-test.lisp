(defpackage :chipi-api.sse-stream-utils.tests
  (:use :cl :fiveam)
  (:import-from #:chipi-api.sse-stream-utils
                #:write-sse-message
                #:write-sse-data
                #:write-sse-heartbeat
                #:write-sse-connection
                #:stream-closed-error)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:com.inuoe.jzon
                #:parse))

(in-package :chipi-api.sse-stream-utils.tests)

(def-suite sse-stream-utils-tests
  :description "Tests for SSE stream utilities")

(in-suite sse-stream-utils-tests)

;; Helper functions for testing
(defun make-test-stream ()
  "Create a test stream for writing"
  (flexi-streams:make-in-memory-output-stream))

(defun get-stream-output (stream)
  "Get the output from a test stream as string"
  (flexi-streams:octets-to-string
   (flexi-streams:get-output-stream-sequence stream)
   :external-format :utf-8))

(defun parse-json-output (output)
  "Parse JSON output from stream"
  (let ((lines (remove-if (lambda (line) (string= line ""))
                          (split-sequence:split-sequence #\Newline output))))
    (mapcar #'parse lines)))

;; Tests for write-sse-message
(test write-sse-message-with-string
  "Test write-sse-message with string data"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-message stream "{\"test\": \"value\"}")))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output))))
      (is (equal "value" (gethash "test" (gethash "data" parsed)))))))

(test write-sse-message-with-hash-table
  "Test write-sse-message with hash table data"
  (let ((stream (make-test-stream))
        (data (plist-hash-table '("key" "value" "number" 42) :test #'equal)))
    (is (eq t (write-sse-message stream data)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "value" (gethash "key" data-part)))
      (is (= 42 (gethash "number" data-part))))))

(test write-sse-message-closed-stream
  "Test write-sse-message with closed stream"
  (let ((stream (make-test-stream)))
    (close stream)
    (signals stream-closed-error
      (write-sse-message stream "test"))))

;; Tests for write-sse-data
(test write-sse-data-with-string
  "Test write-sse-data with string input"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-data stream "{\"message\": \"simple string\"}")))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "simple string" (gethash "message" data-part))))))

(test write-sse-data-with-hash-table
  "Test write-sse-data with hash table input"
  (let ((stream (make-test-stream))
        (data (plist-hash-table '("type" "test" "value" 123) :test #'equal)))
    (is (eq t (write-sse-data stream data)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "test" (gethash "type" data-part)))
      (is (= 123 (gethash "value" data-part))))))

(test write-sse-data-with-plist
  "Test write-sse-data with plist input"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-data stream '("name" "item1" "value" 42))))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "item1" (gethash "name" data-part)))
      (is (= 42 (gethash "value" data-part))))))

(test write-sse-data-with-complex-data
  "Test write-sse-data with nested data structures"
  (let ((stream (make-test-stream))
        (data (alexandria:plist-hash-table 
               (list "item" (alexandria:plist-hash-table
                             (list "name" "test-item" 
                                   "state" (alexandria:plist-hash-table
                                            (list "value" 100 "timestamp" 1234567890)
                                            :test #'equal))
                             :test #'equal)
                     "metadata" (alexandria:plist-hash-table
                                 (list "tags" (alexandria:plist-hash-table
                                               (list "readonly" "true")
                                               :test #'equal))
                                 :test #'equal))
               :test #'equal)))
    (is (eq t (write-sse-data stream data)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (hash-table-p data-part))
      (is (hash-table-p (gethash "item" data-part)))
      (is (equal "test-item" (gethash "name" (gethash "item" data-part)))))))

;; Tests for write-sse-heartbeat
(test write-sse-heartbeat
  "Test write-sse-heartbeat"
  (let ((stream (make-test-stream))
        (timestamp 1234567890))
    (is (eq t (write-sse-heartbeat stream timestamp)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "heartbeat" (gethash "type" data-part)))
      (is (= timestamp (gethash "timestamp" data-part))))))

;; Tests for write-sse-connection
(test write-sse-connection
  "Test write-sse-connection"
  (let ((stream (make-test-stream))
        (message "Connected successfully"))
    (is (eq t (write-sse-connection stream message)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (equal "connection" (gethash "type" data-part)))
      (is (equal message (gethash "message" data-part))))))

;; Edge case tests
(test write-sse-data-empty-hash-table
  "Test write-sse-data with empty hash table"
  (let ((stream (make-test-stream))
        (data (make-hash-table :test #'equal)))
    (is (eq t (write-sse-data stream data)))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (hash-table-p data-part))
      (is (= 0 (hash-table-count data-part))))))

(test write-sse-data-empty-plist
  "Test write-sse-data with empty plist"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-data stream '())))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (hash-table-p data-part))
      (is (= 0 (hash-table-count data-part))))))

(test write-sse-data-boolean-values
  "Test write-sse-data with boolean values"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-data stream '("enabled" t "disabled" nil))))
    (let* ((output (get-stream-output stream))
           (parsed (first (parse-json-output output)))
           (data-part (gethash "data" parsed)))
      (is (eq t (gethash "enabled" data-part)))
      (is (eq nil (gethash "disabled" data-part))))))

(test multiple-writes-to-same-stream
  "Test multiple writes to the same stream"
  (let ((stream (make-test-stream)))
    (is (eq t (write-sse-data stream '("message" "first"))))
    (is (eq t (write-sse-data stream '("message" "second"))))
    (let* ((output (get-stream-output stream))
           (parsed (parse-json-output output)))
      (is (= 2 (length parsed)))
      (is (equal "first" (gethash "message" (gethash "data" (first parsed)))))
      (is (equal "second" (gethash "message" (gethash "data" (second parsed))))))))
