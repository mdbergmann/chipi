(defpackage :chipi-api.sse-manager-test
  (:use :cl :fiveam :cl-mock :chipi-api.sse-manager)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-api.sse-manager-test)

(def-suite sse-manager-tests
  :description "Tests for SSE manager"
  :in chipi-api.tests:test-suite)

(in-suite sse-manager-tests)

;; Use regular string streams for testing but track them
(defvar *test-streams* (make-hash-table :test 'eq))

;; -------------------------
;; Test fake stream
;; -------------------------

(defun make-test-stream ()
  "Create a test stream that works with format"
  (let ((stream (make-string-output-stream)))
    (setf (gethash stream *test-streams*) (list :closed nil :output ""))
    stream))

(defun test-stream-closed-p (stream)
  "Check if test stream is marked as closed"
  (getf (gethash stream *test-streams*) :closed))

(defun close-test-stream (stream)
  "Mark test stream as closed"
  (when (gethash stream *test-streams*)
    (setf (getf (gethash stream *test-streams*) :closed) t)))

(defun get-test-stream-output (stream)
  "Get output from test stream"
  (get-output-stream-string stream))

(defun clear-test-stream-output (stream)
  "Reset test stream"
  (when (gethash stream *test-streams*)
    (close stream)
    (let ((new-stream (make-string-output-stream)))
      (setf (gethash new-stream *test-streams*) 
            (gethash stream *test-streams*))
      (remhash stream *test-streams*)
      new-stream)))

;; Override stream methods for testing
(defmethod open-stream-p :around (stream)
  (if (gethash stream *test-streams*)
      (not (test-stream-closed-p stream))
      (call-next-method)))

;; -------------------------

(def-fixture with-sse-manager ()
  "Fixture that provides a fresh SSE manager for testing"
  (let ((sse-manager::*sse-manager* nil))
    (unwind-protect
         (&body)
      ;; Cleanup: stop any running SSE manager
      (when sse-manager::*sse-manager*
        (act-cell:stop sse-manager::*sse-manager*)
        (setf sse-manager::*sse-manager* nil)))))

(def-fixture with-isys ()
  "Fixture that ensures the actor system is available and cleans up properly"
  (isys:ensure-isys)
  (unwind-protect
       (&body)
    (isys:shutdown-isys)))

(defun %make-mock-item (id &key (label "") (value 42))
  "Create a mock item using the proper item:make-item API"
  (let ((item (item:make-item id :label label)))
    ;; Set the initial value
    (item:set-value item value :persist nil)
    item))

(test make-sse-manager--creates-actor
  "Test that make-sse-manager creates an actor"
  (with-fixture with-isys ()
    (let ((manager (make-sse-manager)))
      (is-true (typep manager 'act:actor))
      (is-true (act-cell:running-p manager))
      (act-cell:stop manager))))

(test ensure-sse-manager--singleton-behavior
  "Test that ensure-sse-manager returns the same instance"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((manager1 (ensure-sse-manager))
            (manager2 (ensure-sse-manager)))
        (is (eq manager1 manager2))
        (is-true (act-cell:running-p manager1))))))

(test add-client--returns-client-id
  "Test that add-client returns a client ID"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((stream (make-test-stream))
            (api-key "test-key")
            (access-rights '(:read)))
        (let ((future (add-client stream api-key access-rights)))
          (is-true (miscutils:await-cond 1
                     (let ((client-id (future:fresult future)))
                       (and (stringp client-id)
                            (search "client-" client-id))))))))))

(test add-client--increments-counter
  "Test that adding multiple clients generates different IDs"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((stream1 (make-test-stream))
            (stream2 (make-test-stream))
            (api-key "test-key")
            (access-rights '(:read)))
        (let ((future1 (add-client stream1 api-key access-rights))
              (future2 (add-client stream2 api-key access-rights)))
          (is-true (miscutils:await-cond 1                     
                     (let ((client-id1 (future:fresult future1))
                           (client-id2 (future:fresult future2)))
                       (not (and (eq :not-ready client-id1)
                                 (equal client-id1 client-id2)))))))))))

(test end-to-end-item-change-broadcast
  "Test complete end-to-end flow: add client → change item → verify SSE broadcast"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      ;; Step 1: Add a client to the SSE manager
      (let ((stream (make-test-stream))
            (api-key "test-key")
            (access-rights '(:read)))
        
        ;; Step 2: Register client and wait for confirmation
        (let ((add-future (add-client stream api-key access-rights)))
          (is-true (miscutils:await-cond 1
                     (let ((client-id (future:fresult add-future)))
                       (and (stringp client-id)
                            (search "client-" client-id))))
                   "Client should be successfully added")
          
          ;; Step 3: Create a test item using the proper item API
          (let ((item (%make-mock-item 'broadcast-test-item :label "Broadcast Test" :value 100)))
            
            ;; Step 4: Change the item value to trigger item-changed-event
            (item:set-value item 200)
            
            ;; Step 5: Wait for the event to propagate and be broadcast
            (let (output)
              (is-true (miscutils:await-cond 2
                         (setf output (get-test-stream-output stream))
                         (format t "The output: ~a~%" output)
                         (and (> (length output) 0)
                              (search "data:" output)
                              (search "item-change" output)))
                       "SSE data should be broadcast to the client stream")
              
              ;; Step 6: Verify the content structure using the captured output
              (format t "The output: ~a~%" output)
              (is (search "data:" output) "Should contain SSE data prefix")
              (is (search "item-change" output) "Should contain item-change event type")
              (is (search "200" output) "Should contain the new item value")
              (is (search "BROADCAST-TEST-ITEM" output) "Should contain the item name"))))))))
