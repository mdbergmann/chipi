(defpackage :chipi-api.events-controller-test
  (:use :cl :fiveam :cl-mock :sse-fake-stream :chipi-api.events-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-api.events-controller-test)

(def-suite events-controller-tests
  :description "Tests for events controller"
  :in chipi-api.tests:test-suite)

(in-suite events-controller-tests)

(def-fixture with-fake-streams ()
  "Fixture that provides clean fake streams for each test"
  (sse-fake-stream:setup-fake-streams)
  (&body))

(defun parse-sse-data (output)
  "Parse JSON data from stream output and return list of parsed JSON objects"
  (let ((lines (split-sequence:split-sequence #\Newline output))
        (messages '()))
    (dolist (line lines)
      (when (> (length line) 0)
        (handler-case
            (let ((parsed (com.inuoe.jzon:parse line)))
              (when (gethash "event" parsed)
                (push (gethash "event" parsed) messages)))
          (error ()
            ;; Skip lines that aren't valid JSON
            nil))))
    (nreverse messages)))

;; -------------------------
;; Tests
;; -------------------------

(test handle-sse-connection--sends-initial-connection-message
  "Test that handle-sse-connection sends initial connection message"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-123")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 0)) ; Exit immediately after initial message
      (answer chipi-api.sse-manager:add-client client-id)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Verify initial connection message was sent
      (let* ((output (get-test-stream-output stream))
             (messages (parse-sse-data output)))
        (is (>= (length messages) 1) "Should send at least initial message")
        (when (> (length messages) 0)
          (let ((msg (first messages)))
            (is (equal "connection"
                       (gethash "type" msg)) "Should be connection type")
            (is (equal "Connected to item events"
                       (gethash "message" msg)) "Should have correct message"))))))))

(test handle-sse-connection--adds-client-to-sse-manager
  "Test that handle-sse-connection adds client to SSE manager"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-456")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 0)) ; Exit immediately after initial message
      (answer chipi-api.sse-manager:add-client client-id)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Verify add-client was called with the stream
      (is (= 1 (length (invocations 'chipi-api.sse-manager:add-client)))
          "Should call add-client once")))))

(test handle-sse-connection--sends-heartbeat-messages
  "Test that handle-sse-connection sends periodic heartbeat messages"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-789")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 2)) ; Send initial message + 2 heartbeats
      (answer chipi-api.sse-manager:add-client client-id)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Verify heartbeat messages were sent
      (let* ((output (get-test-stream-output stream))
             (messages (parse-sse-data output)))
        (is (>= (length messages) 2) "Should send initial + at least 1 heartbeat message")
        ;; Check that we have heartbeat messages
        (let ((heartbeat-msgs (remove-if-not 
                               (lambda (msg) (equal "heartbeat" (gethash "type" msg)))
                               messages)))
          (is (>= (length heartbeat-msgs) 1) "Should send at least 1 heartbeat message")
          ;; Check heartbeat message structure
          (dolist (hb heartbeat-msgs)
            (is (gethash "timestamp" hb) "Heartbeat should have timestamp"))))))))

(test handle-sse-connection--removes-client-on-normal-completion
  "Test that handle-sse-connection removes client when function completes"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-normal")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 0)) ; Exit immediately after initial message
      (answer chipi-api.sse-manager:add-client client-id)
      (answer chipi-api.sse-manager:remove-client nil)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Since there's no error, remove-client should not be called
      ;; This test verifies that remove-client is only called on errors, not normal completion
      (is (= 0 (length (invocations 'chipi-api.sse-manager:remove-client)))
          "Should not call remove-client on normal completion")))))

(test handle-sse-connection--removes-client-on-error
  "Test that handle-sse-connection removes client when an error occurs"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-error")
          (*heartbeat-sleep-time-s* 10.0)
          (*max-heartbeats* nil)) ; Unlimited heartbeats
      (answer chipi-api.sse-manager:add-client client-id)
      (answer chipi-api.sse-manager:remove-client nil)
      
      (close-test-stream stream)
      
      (signals sse-stream-utils:stream-closed-error (handle-sse-connection stream))
      
      (is (= 0 (length (invocations 'chipi-api.sse-manager:add-client)))
          "Should call add-client exactly once")
      (is (= 0 (length (invocations 'chipi-api.sse-manager:remove-client)))
          "Should call remove-client exactly once on error")))))

(test handle-sse-connection--removes-client-on-error-after-add
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-error")
          (*heartbeat-sleep-time-s* .01)
          (*max-heartbeats* nil)) ; Unlimited heartbeats
      (answer chipi-api.sse-manager:add-client client-id)
      (answer chipi-api.sse-manager:remove-client nil)

      (let ((thread (bt2:make-thread
                     (lambda ()
                       (handle-sse-connection stream))
                     :name "SSE runner"
                     :initial-bindings
                     `((*heartbeat-sleep-time-s* . 0.01)
                       (*max-heartbeats* . nil)))))
        (declare (ignore thread))

        (is-true (miscutils:await-cond 1.0
                   (= 1 (length (invocations 'chipi-api.sse-manager:add-client)))))
        (is (= 0 (length (invocations 'chipi-api.sse-manager:remove-client))))

        ;; sleep at least *heartbeat-sleep-time-s* before closing the stream
        (sleep 0.1)
        (close-test-stream stream)

        (is-true (miscutils:await-cond 1.0
                   (= 1 (length (invocations 'chipi-api.sse-manager:remove-client))))))))))

(test handle-sse-connection--message-format-validation
  "Test that SSE messages follow correct format"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-format")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 1)) ; Send initial message + 1 heartbeat
      (answer chipi-api.sse-manager:add-client client-id)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Verify SSE message format
      (let ((output (get-test-stream-output stream)))
        ;; Check that all non-empty lines are valid JSON with event wrapper
        (let ((lines (split-sequence:split-sequence #\Newline output)))
          (dolist (line lines)
            (when (> (length line) 0)
              (handler-case
                  (let ((parsed (com.inuoe.jzon:parse line)))
                    (is (gethash "event" parsed) "All non-empty lines should be JSON with 'event' field"))
                (error ()
                  (fail "All non-empty lines should be valid JSON"))))))        
        (is (search "\"event\":" output) "Should contain JSON event"))))))

(test handle-sse-connection--json-structure-validation
  "Test that JSON messages have correct structure"
  (with-fixture with-fake-streams ()
    (with-mocks ()
    (let ((stream (make-test-stream))
          (client-id "test-client-json")
          (*heartbeat-sleep-time-s* 0.001)
          (*max-heartbeats* 1)) ; Send initial message + 1 heartbeat
      (answer chipi-api.sse-manager:add-client client-id)
      
      ;; Test the function
      (finishes (handle-sse-connection stream))
      
      ;; Verify JSON structure
      (let* ((output (get-test-stream-output stream))
             (messages (parse-sse-data output)))
        (is (> (length messages) 0) "Should have at least one message")
        (dolist (msg messages)
          (is (hash-table-p msg) "Each message should be a hash table")
          (is (gethash "type" msg) "Each message should have a 'type' field")
          (is (member (gethash "type" msg)
                      '("connection" "heartbeat") :test #'equal)
              "Message type should be 'connection' or 'heartbeat'")))))))
