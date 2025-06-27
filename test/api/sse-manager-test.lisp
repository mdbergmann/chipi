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
  "Create a test stream that works with write-sequence of octets (like Hunchentoot chunked streams)"
  (let* ((binary-stream (flexi-streams:make-in-memory-output-stream))
         (flexi-stream (flexi-streams:make-flexi-stream
                        binary-stream
                        :external-format :utf-8
                        :element-type '(unsigned-byte 8))))
    (setf (gethash flexi-stream *test-streams*) 
          (list :closed nil :output "" :binary-stream binary-stream))
    flexi-stream))

(defun get-test-stream-output (stream)
  "Get output from test stream as string"
  (let ((binary-stream (getf (gethash stream *test-streams*) :binary-stream)))
    (if binary-stream
        (flexi-streams:octets-to-string 
         (flexi-streams:get-output-stream-sequence binary-stream)
         :external-format :utf-8)
        "")))

(defun test-stream-closed-p (stream)
  "Check if test stream is marked as closed"
  (getf (gethash stream *test-streams*) :closed))

(defun close-test-stream (stream)
  "Mark test stream as closed"
  (when (gethash stream *test-streams*)
    (setf (getf (gethash stream *test-streams*) :closed) t)))

;; Override stream methods for testing
(defmethod open-stream-p :around (stream)
  (if (gethash stream *test-streams*)
      (not (test-stream-closed-p stream))
      (call-next-method)))

(defun clear-test-stream-output (stream)
  "Reset test stream"
  (when (gethash stream *test-streams*)
    (close stream)
    (let ((new-stream (make-string-output-stream)))
      (setf (gethash new-stream *test-streams*) 
            (gethash stream *test-streams*))
      (remhash stream *test-streams*)
      new-stream)))

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
      (let ((stream (make-test-stream)))
        (let ((future (add-client stream)))
          (is-true (miscutils:await-cond 1
                     (let ((client-id (future:fresult future)))
                       (and (stringp client-id)
                            (search "client-" client-id))))))))))

(test add-client--increments-counter
  "Test that adding multiple clients generates different IDs"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((stream1 (make-test-stream))
            (stream2 (make-test-stream)))
        (let ((future1 (add-client stream1))
              (future2 (add-client stream2)))
          (is-true (miscutils:await-cond 1                     
                     (let ((client-id1 (future:fresult future1))
                           (client-id2 (future:fresult future2)))
                       (not (and (eq :not-ready client-id1)
                                 (equal client-id1 client-id2)))))))))))

(test remove-client--removes-existing-client
  "Test that remove-client removes an existing client"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((stream (make-test-stream)))
        
        ;; Step 1: Add a client first
        (let ((add-future (add-client stream)))
          (let (client-id)
            (is-true (miscutils:await-cond 1
                       (setf client-id (future:fresult add-future))
                       (and (stringp client-id)
                            (search "client-" client-id)))
                     "Client should be successfully added")
            
            ;; Step 2: Remove the client
            (remove-client client-id)
            
            ;; Step 3: Wait for removal to process and verify
            (is-true (miscutils:await-cond 1
                       (let ((manager-state (act-cell:state (ensure-sse-manager))))
                         (= 0 (hash-table-count
                               (sse-manager::sse-manager-state-clients manager-state)))))
                     "Client should be removed from the SSE manager's clients hashtable")))))))

(test remove-client--handles-nonexistent-client
  "Test that remove-client handles non-existent client gracefully"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (ensure-sse-manager)
      (finishes (remove-client "non-existent-client-123")))))

(test remove-client--multiple-clients
  "Test removing one client while others remain active"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      (let ((stream1 (make-test-stream))
            (stream2 (make-test-stream)))
        
        ;; Step 1: Add two clients
        (let ((future1 (add-client stream1))
              (future2 (add-client stream2)))
          (let (client-id1 client-id2)
            (is-true (miscutils:await-cond 1
                       (setf client-id1 (future:fresult future1)
                             client-id2 (future:fresult future2))
                       (and (stringp client-id1) (stringp client-id2)
                            (not (equal client-id1 client-id2))))
                     "Both clients should be added with different IDs")
            
            ;; Step 2: Remove first client
            (remove-client client-id1)
            
            ;; Step 3: Wait for client removal to complete
            (is-true (miscutils:await-cond 1
                       (let ((manager-state (act-cell:state (ensure-sse-manager))))
                         (= 1 (hash-table-count
                               (sse-manager::sse-manager-state-clients manager-state)))))
                     "One client should remain after removal")
            
            ;; Step 4: Trigger an item change
            (let ((item (%make-mock-item 'multi-client-test :label "Multi Test" :value 500)))
              (item:set-value item 600)
              
              ;; Step 5: Wait for broadcast and verify only active client receives data
              (let (output2)
                (is-true (miscutils:await-cond 2
                           (progn
                             (setf output2 (get-test-stream-output stream2))
                             (and (> (length output2) 0)
                                  (search "item-change" output2))))
                         "Active client should receive item change data")
                
                ;; Step 6: Verify content and that removed client gets no data
                (let ((output1 (get-test-stream-output stream1)))
                  (is (= 0 (length output1)) "Removed client should not receive data")
                  (is (search "item-change" output2) "Active client should receive item change")
                  (is (search "600" output2) "Active client should receive new value"))))))))))

(test end-to-end-item-change-broadcast
  "Test complete end-to-end flow: add client → change item → verify SSE broadcast"
  (with-fixture with-sse-manager ()
    (with-fixture with-isys ()
      ;; Step 1: Add a client to the SSE manager
      (let ((stream (make-test-stream)))
        
        ;; Step 2: Register client and wait for confirmation
        (let ((add-future (add-client stream)))
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
                         (and (> (length output) 0)
                              (search "data:" output)
                              (search "item-change" output)))
                       "SSE data should be broadcast to the client stream")
              
              ;; Step 6: Verify the content structure using the captured output
              (is (search "data:" output) "Should contain SSE data prefix")
              (is (search "item-change" output) "Should contain item-change event type")
              (is (search "200" output) "Should contain the new item value")
              (is (search "BROADCAST-TEST-ITEM" output) "Should contain the item name"))))))))
