(in-package #:chipi.binding.mqtt)


(defclass mqtt-input ()
  ((client :initform nil
           :type (or null cl-mqtt::mqtt-client)
           :accessor mqtt-input-client)
   (client-id :initform (format nil "chipi-~32,'0x"
                                (random (ash 1 128) (make-random-state t)))
              :type string
              :reader mqtt-input-client-id
              :documentation "An independent client identity, stable across reconnects.")
   (stopping :initform nil :accessor mqtt-input-stopping)
   (watchdog :initform nil :accessor mqtt-input-watchdog)
   (health-state :initform :unknown :accessor mqtt-input-health-state)
   (broker-topics :initform nil :accessor mqtt-input-broker-topics)
   (subscriptions :initform nil :accessor mqtt-input-subscriptions)
   (thread :initform nil
           :type (or null bt2:thread)
           :accessor mqtt-input-thread
           :documentation "A thread to run mqtt async loop.")
   (heartbeat :initform nil
              :accessor mqtt-input-heartbeat
              :documentation "Monotonic time of the last event-loop queue tick.")
   (lifecycle-lock :initform (bt2:make-lock :name "mqtt-lifecycle")
                   :reader mqtt-input-lifecycle-lock)
   (queue :initform nil
          :type list
          :accessor mqtt-input-queue
          :documentation "A queue for commands to be execute in the mqtt async loop.")
   (lock :initform (bt2:make-lock :name "mqtt-input-lock")
         :type bt2:lock
         :accessor mqtt-input-lock
         :documentation "A lock to protect queue from simultaneus access from mqtt async loop's thread and other threads.")
   (host :initarg :host
         :type string
         :accessor mqtt-input-host)
   (port :initarg :port
         :type integer
         :accessor mqtt-input-port)
   (user :initarg :user
         :type (or null string)
         :accessor mqtt-input-user)
   (pass :initarg :pass
         :type (or null string)
         :accessor mqtt-input-pass)
   (callbacks :initform nil
              :type list
              :accessor mqtt-input-callbacks
              :documentation "An a list where each item's CAR is a string with a topic pattern and CDR is a callback function of two arguments TOPIC and PAYLOAD.

                              When you call SUBSCRIBE-ON, a new item might be added to this list. And when you call UNSUBSCRIBE-FROM, an item might ber removed.
                              When you subscribe twice with the same pattern, then callback from the second call will be saved.")))


(defun make-mqtt-input (&key (host "localhost") (port 1883) user pass)
  "Create an independent transport with explicit broker settings."
  (make-instance 'mqtt-input :host host :port port :user user :pass pass))

(defmethod print-object ((obj mqtt-input) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A:~A"
            (mqtt-input-host obj)
            (mqtt-input-port obj))))


(defstruct mqtt-subscription token topic handler)

(defun %enqueue-command (input command)
  "Append COMMAND in FIFO order. The caller must hold INPUT's queue lock."
  (setf (mqtt-input-queue input)
        (nconc (mqtt-input-queue input) (list command))))

(defun subscribe-on (input topic callback)
  "Replace the compatibility callback for TOPIC."
  (bt2:with-lock-held ((mqtt-input-lock input))
    (setf (assoc-value (mqtt-input-callbacks input) topic :test #'string=) callback)
    (%enqueue-command input (list :subscribe topic)))
  (values))

(defun unsubscribe-from (input topic)
  "Remove the compatibility callback for TOPIC."
  (bt2:with-lock-held ((mqtt-input-lock input))
    (removef (mqtt-input-callbacks input) topic :key #'car :test #'string=)
    (%enqueue-command input (list :unsubscribe topic)))
  (values))

(defun unsubscribe-from-all-topics (input)
  (dolist (entry (copy-list (mqtt-input-callbacks input)))
    (unsubscribe-from input (car entry))))

(declaim (ftype (function (mqtt-input string (or string hash-table)
                          &key (:qos integer) (:retain t))
                         (values &optional))
                publish-to))

(defun publish-to (input topic payload &key (qos 0) retain)
  "Queue a publication without waiting for transport acceptance."
  (let ((prepared-payload (%prepare-payload payload)))
    (bt2:with-lock-held ((mqtt-input-lock input))
      (%enqueue-command input (list :publish topic :payload prepared-payload
                                    :qos qos :retain retain))))
  (values))

(define-condition mqtt-error (error) ())

(define-condition mqtt-publication-error (mqtt-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "MQTT publication failed." stream))))
(define-condition mqtt-timeout (mqtt-publication-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "MQTT publication timed out." stream))))
(define-condition mqtt-disconnected (mqtt-publication-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "MQTT connection unavailable." stream))))

(defun %prepare-payload (payload)
  "Serialize string-keyed JSON objects without exposing payloads in conditions."
  (handler-case
      (etypecase payload
        (string payload)
        (hash-table
         (let ((ancestors (make-hash-table :test #'eq)))
           (labels ((validate (value)
                      (when (and (not (stringp value))
                                 (or (hash-table-p value) (consp value) (vectorp value)))
                        (when (gethash value ancestors) (error 'mqtt-publication-error))
                        (setf (gethash value ancestors) t)
                        (typecase value
                          (hash-table
                           (maphash (lambda (key child)
                                      (unless (stringp key) (error 'mqtt-publication-error))
                                      (validate child)) value))
                          (cons (validate (car value)) (validate (cdr value)))
                          (vector (map nil #'validate value)))
                        (remhash value ancestors))))
             (validate payload)))
         (jzon:stringify payload)))
    (error () (error 'mqtt-publication-error))))

(defun %check-wait-context (input)
  (when (or (eq (bt2:current-thread) (mqtt-input-thread input)) act:*self*)
    (error 'mqtt-publication-error)))

(defclass publication-request ()
  ((retain :initarg :retain :initform nil :reader publication-request-retain)
   (state :initform :pending :accessor publication-request-state)
   (connection :initarg :connection :reader publication-request-connection)
   (deadline :initarg :deadline :reader publication-request-deadline)
   (lock :initform (bt2:make-lock :name "mqtt-publication")
         :reader publication-request-lock)))

(defun make-publication-request (connection deadline)
  (make-instance 'publication-request :connection connection :deadline deadline))

(defun connected-p (mqtt-input)
  "Return true while the transport has an established MQTT client."
  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
    (not (null (mqtt-input-client mqtt-input)))))

(defun health-snapshot (mqtt-input &key (heartbeat-timeout 3))
  "Return independent connection and event-loop health with monotonic heartbeat age.
The string keys are connected, loop_alive and heartbeat_age. Boolean values
are Lisp booleans; heartbeat_age is seconds or NIL before the first tick."
  (check-type heartbeat-timeout (real (0)))
  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
    (let* ((heartbeat (mqtt-input-heartbeat mqtt-input))
           (age (when heartbeat
                  (/ (- (get-internal-real-time) heartbeat)
                     internal-time-units-per-second)))
           (thread (mqtt-input-thread mqtt-input)))
      (alexandria:plist-hash-table
       (list "connected" (not (null (mqtt-input-client mqtt-input)))
             "loop_alive" (and thread (bt2:thread-alive-p thread)
                               age (< age heartbeat-timeout) t)
             "heartbeat_age" age)
       :test #'equal))))

(defun record-loop-heartbeat (mqtt-input)
  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
    (setf (mqtt-input-heartbeat mqtt-input) (get-internal-real-time))))

(defun reset-connection (mqtt-input)
  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
    (setf (mqtt-input-client mqtt-input) nil
          (mqtt-input-broker-topics mqtt-input) nil)))

(defun publish-and-wait (mqtt-input topic payload &key (timeout 3))
  "Wait for local MQTT client acceptance, returning :SENT, or signal an error.
A timeout cancels a queued request. No request is replayed on a new connection.
QoS 0 acceptance does not confirm broker delivery or persistence."
  (check-type timeout (real (0)))
  (%check-wait-context mqtt-input)
  (let* ((deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second)))
         (prepared-payload (%prepare-payload payload))
         (request
           (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
             (unless (mqtt-input-client mqtt-input)
               (error 'mqtt-disconnected))
             (let ((request (make-publication-request
                             (mqtt-input-client mqtt-input) deadline)))
               (%enqueue-command mqtt-input
                                 (list :publish-request topic
                                       :payload prepared-payload :request request))
               request))))
    (loop
      (bt2:with-lock-held ((publication-request-lock request))
        (case (publication-request-state request)
          (:sent (return-from publish-and-wait :sent))
          (:failed (error 'mqtt-publication-error)))
        (when (or (>= (get-internal-real-time) deadline)
                  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
                    (not (eq (publication-request-connection request)
                             (mqtt-input-client mqtt-input)))))
          (setf (publication-request-state request) :cancelled)
          (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
            (setf (mqtt-input-queue mqtt-input)
                  (remove request (mqtt-input-queue mqtt-input)
                          :key (lambda (command) (getf (cddr command) :request)))))
          (error (if (>= (get-internal-real-time) deadline) 'mqtt-timeout 'mqtt-disconnected))))
      (sleep 0.01))))

(defun process-publication-request (mqtt-input conn topic payload request)
  ;; The request lock serializes cancellation with the nonblocking publish call.
  ;; Never hold the queue lock while calling MQTT: callbacks may acquire it.
  (bt2:with-lock-held ((publication-request-lock request))
    (when (eq (publication-request-state request) :pending)
      (setf (publication-request-state request) :failed)
      (when (and (< (get-internal-real-time) (publication-request-deadline request))
                 (eq conn (publication-request-connection request))
                 (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
                   (eq conn (mqtt-input-client mqtt-input))))
        (handler-case
            (progn
              (mqtt:publish conn topic payload :qos 0 :retain nil)
              (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
                (when (eq conn (mqtt-input-client mqtt-input))
                  (setf (publication-request-state request) :sent))))
          (error () nil))))))

(defun cancel-confirmed-request (mqtt-input request)
  (bt2:with-lock-held ((publication-request-lock request))
    (unless (eq (publication-request-state request) :acknowledged)
      (setf (publication-request-state request) :cancelled)))
  (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
    (setf (mqtt-input-queue mqtt-input)
          (remove request (mqtt-input-queue mqtt-input)
                  :key (lambda (command) (getf (cddr command) :request))))))

(defun publish-confirmed (mqtt-input topic payload &key (timeout 3) retain)
  "Return :ACKNOWLEDGED only after the broker's QoS 1 PUBACK.
Timeouts, disconnects and rejections signal distinct sanitized conditions. Cancelled commands
never replay on another connection. This does not confirm physical actuation."
  (check-type timeout (real (0)))
  (%check-wait-context mqtt-input)
  (let* ((deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second)))
         (prepared-payload (%prepare-payload payload))
         (request
           (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
             (unless (mqtt-input-client mqtt-input)
               (error 'mqtt-disconnected))
             (let ((request (make-publication-request
                             (mqtt-input-client mqtt-input) deadline)))
               (setf (slot-value request 'retain) retain)
               (%enqueue-command mqtt-input
                                 (list :publish-confirmed topic
                                       :payload prepared-payload :request request))
               request))))
    (unwind-protect
         (loop
           (bt2:with-lock-held ((publication-request-lock request))
             (case (publication-request-state request)
               (:acknowledged (return :acknowledged))
               (:failed (error 'mqtt-publication-error))
               (:cancelled
                (error (if (>= (get-internal-real-time) deadline)
                           'mqtt-timeout 'mqtt-disconnected))))
             (when (or (>= (get-internal-real-time) deadline)
                       (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
                         (not (eq (publication-request-connection request)
                                  (mqtt-input-client mqtt-input)))))
               (setf (publication-request-state request) :cancelled)
               (error (if (>= (get-internal-real-time) deadline) 'mqtt-timeout 'mqtt-disconnected))))
           (sleep 0.01))
      (cancel-confirmed-request mqtt-input request))))

(defun finish-confirmed-request (mqtt-input conn request state)
  (bt2:with-lock-held ((publication-request-lock request))
    (when (eq (publication-request-state request) :publishing)
      (setf (publication-request-state request)
            (if (and (< (get-internal-real-time) (publication-request-deadline request))
                     (bt2:with-lock-held ((mqtt-input-lock mqtt-input))
                       (eq conn (mqtt-input-client mqtt-input))))
                state
                :cancelled)))))

(defun process-confirmed-request (input conn topic payload request)
  (let ((promise
          ;; Serialise cancellation with the nonblocking publish invocation.
          ;; Promise callbacks are installed only after releasing this lock:
          ;; Blackbird invokes callbacks for resolved promises synchronously.
          (bt2:with-lock-held ((publication-request-lock request))
            (when (eq (publication-request-state request) :pending)
              (setf (publication-request-state request) :cancelled)
              (when (and (eq conn (publication-request-connection request))
                         (< (get-internal-real-time) (publication-request-deadline request))
                         (bt2:with-lock-held ((mqtt-input-lock input))
                           (eq conn (mqtt-input-client input))))
                (setf (publication-request-state request) :publishing)
                (handler-case
                    (let ((result (mqtt:publish
                                   conn topic payload :qos 1
                                   :retain (publication-request-retain request))))
                      (if (bb:promisep result) result
                          (progn (setf (publication-request-state request) :failed) nil)))
                  (error () (setf (publication-request-state request) :failed) nil)))))))
    (when promise
      (bb:attach-errback
       (bb:attach promise
         (lambda (&rest values)
           (declare (ignore values))
           (finish-confirmed-request input conn request :acknowledged)))
       (lambda (&rest errors)
         (declare (ignore errors))
         (finish-confirmed-request input conn request :failed))))))

(defun %topic-required-p (input topic)
  (or (assoc topic (mqtt-input-callbacks input) :test #'string=)
      (find topic (mqtt-input-subscriptions input)
            :key #'mqtt-subscription-topic :test #'string=)))

(defun process-next-command (input conn)
  (let ((command (bt2:with-lock-held ((mqtt-input-lock input))
                   (pop (mqtt-input-queue input)))))
    (when command
      (destructuring-bind (method topic &key payload callback request (qos 0) retain connection)
          command
        (declare (ignore callback))
        (case method
          (:publish
           (when (or (null connection) (eq connection conn))
             (mqtt:publish conn topic payload :qos qos :retain retain)))
          (:publish-confirmed (process-confirmed-request input conn topic payload request))
          (:publish-request (process-publication-request input conn topic payload request))
          (:subscribe
           (unless (bt2:with-lock-held ((mqtt-input-lock input))
                     (member topic (mqtt-input-broker-topics input) :test #'string=))
             (mqtt:subscribe conn topic)
             (bt2:with-lock-held ((mqtt-input-lock input))
               (pushnew topic (mqtt-input-broker-topics input) :test #'string=))))
          (:unsubscribe
           (unless (bt2:with-lock-held ((mqtt-input-lock input))
                     (%topic-required-p input topic))
             (mqtt:unsubscribe conn topic)
             (bt2:with-lock-held ((mqtt-input-lock input))
               (setf (mqtt-input-broker-topics input)
                     (remove topic (mqtt-input-broker-topics input) :test #'string=))))))))))

(declaim (ftype (function (string string) (values boolean &optional)) topic-match-p))

(defun topic-match-p (pattern topic)
  "Enhanced MQTT topic pattern matcher with full spec compliance."

  (when (and (plusp (length topic)) (char= (char topic 0) #\$)
             (or (zerop (length pattern)) (char/= (char pattern 0) #\$)))
    (return-from topic-match-p nil))
  (labels ((split (str)
             (if (string= str "")
               '("")                    ; Empty string has one empty level
               (let ((parts '())
                     (start 0))
                 (loop for i from 0 below (length str) do
                   (when (char= (char str i) #\/)
                     (push (subseq str start i) parts)
                     (setf start (1+ i))))
                 (push (subseq str start) parts)
                 (nreverse parts)))))

    (let ((p-parts (split pattern))
          (t-parts (split topic)))

      (do ((p-remaining p-parts (rest p-remaining))
           (t-remaining t-parts (rest t-remaining)))
          ((or (null p-remaining) (null t-remaining))

           (cond
             ;; Both exhausted - exact match
             ((and (null p-remaining) (null t-remaining)) t)

             ;; Topic exhausted but pattern has more (and next isn't #)
             ((null t-remaining)
              (and (equal p-remaining '("#")) t))

             ;; Pattern exhausted but topic has more - no match
             ((null p-remaining) nil)

             (t nil)))

        (let ((p-part (first p-remaining))
              (t-part (first t-remaining)))

          (cond
            ;; Multi-level wildcard
            ((string= p-part "#")
             ;; # must be last part in pattern
             (return-from topic-match-p
               (and (null (rest p-remaining)) t)))

            ;; Single-level wildcard
            ((string= p-part "+")
             ;; + matches any single level (including empty)
             t)                         ; Continue to next iteration

            ;; Literal match
            ((string= p-part t-part)
             t)                         ; Continue to next iteration

            ;; Mismatch
            (t (return-from topic-match-p nil))))))))


(defvar *message-received-at* nil
  "Arrival time shared by all consumers of the current broker message.")

(defun dispatch-raw-message (input topic payload &optional received-at)
  "Call each matching consumer without holding the transport lock."
  (let ((*message-received-at* (or received-at (get-universal-time)))
        (callbacks
          (bt2:with-lock-held ((mqtt-input-lock input))
            (append (copy-list (mqtt-input-callbacks input))
                    (loop for sub in (mqtt-input-subscriptions input)
                          collect (cons (mqtt-subscription-topic sub)
                                        (mqtt-subscription-handler sub)))))))
    (dolist (entry callbacks)
      (when (topic-match-p (car entry) topic)
        (handler-case (funcall (cdr entry) topic payload)
          (error () (log:warn "MQTT consumer failed")))))))

(defun on-message (input message)
  (dispatch-raw-message input (mqtt:mqtt-message-topic message)
                        (babel:octets-to-string (mqtt:mqtt-message-payload message)
                                               :encoding :utf-8)
                        (get-universal-time)))

(defun reschedule-subscriptions (input)
  (bt2:with-lock-held ((mqtt-input-lock input))
    (setf (mqtt-input-queue input)
          (append (loop for topic in
                        (remove-duplicates
                         (append (mapcar #'car (mqtt-input-callbacks input))
                                 (mapcar #'mqtt-subscription-topic
                                         (mqtt-input-subscriptions input))) :test #'string=)
                        collect (list :subscribe topic))
                  (remove :subscribe (mqtt-input-queue input) :key #'car))))
  (values))

(defun on-error (input condition)
  (declare (ignore condition))
  (reset-connection input)
  (as:exit-event-loop))

(defun %health-tick (input)
  (let* ((snapshot (health-snapshot input))
         (state (list (gethash "connected" snapshot) (gethash "loop_alive" snapshot)))
         (changed nil))
    (bt2:with-lock-held ((mqtt-input-lock input))
      (unless (equal state (mqtt-input-health-state input))
        (setf (mqtt-input-health-state input) state changed t)))
    (when (and changed (fboundp '%transport-health-changed))
      ;; BT2's default thread IO syntax enables readable printing. Chipi's
      ;; lazy actor-system startup logs unreadable runtime objects.
      (let ((*print-readably* nil))
        (handler-case (%transport-health-changed input snapshot)
          (error () (log:warn "MQTT health observer failed")))))))

(defun process-mqtt-messages (input &key on-connect)
  (labels ((tick ()
             (record-loop-heartbeat input)
             (if (mqtt-input-stopping input)
                 (as:exit-event-loop)
                 (progn
                   (let ((conn (mqtt-input-client input)))
                     (when conn
                       (handler-case (process-next-command input conn)
                         (error () (log:warn "MQTT command failed")))))
                   (as:delay #'tick :time 0.05)))))
    (tick)
    ;; CL-MQTT rejects its connection promise before creating a client on TCP
    ;; failure. Its error-handler therefore cannot handle initial TCP failures.
    (bb:attach-errback
     (bb:alet ((conn (mqtt:connect
                      (mqtt-input-host input) :port (mqtt-input-port input)
                      :client-id (mqtt-input-client-id input)
                      :user (or (mqtt-input-user input) "")
                      :password (or (mqtt-input-pass input) "")
                      :error-handler (lambda (condition) (on-error input condition))
                      :on-message (lambda (message)
                                    (handler-case (on-message input message)
                                      (error () (log:warn "Invalid MQTT message")))))))
       ;; CONNECT's promise reports a written packet; require an accepted CONNACK
       ;; before making this connection available to application publications.
       (bb:alet ((message (mqtt::wait-for-message conn :connack)))
         (if (eq (mqtt::mqtt-message-ret-code message) :accepted)
             (progn
               (bt2:with-lock-held ((mqtt-input-lock input))
                 (unless (mqtt-input-stopping input)
                   (setf (mqtt-input-client input) conn)))
               (reschedule-subscriptions input)
               (when on-connect (funcall on-connect)))
             (on-error input nil))))
     (lambda (&rest errors)
       (declare (ignore errors))
       (on-error input nil)))))

(defun %stop-worker (input)
  (setf (mqtt-input-stopping input) t)
  ;; The timer exits the event loop on its own thread; CL-ASYNC's unwind cleanup
  ;; closes its libuv handles and releases socket buffers before JOIN returns.
  (dolist (thread (list (mqtt-input-thread input) (mqtt-input-watchdog input)))
    (when (and thread (not (eq thread (bt2:current-thread))))
      (handler-case (bt2:join-thread thread)
        (bt2:abnormal-exit () nil))))
  (setf (mqtt-input-thread input) nil (mqtt-input-watchdog input) nil)
  (reset-connection input)
  (%health-tick input))

(defun ensure-thread-running (input &key restart (resubscribe t))
  "Start exactly one worker and one independent health watchdog."
  (declare (ignore resubscribe))
  (bt2:with-lock-held ((mqtt-input-lifecycle-lock input))
    (let ((thread (mqtt-input-thread input)))
      (when (and thread (bt2:thread-alive-p thread) (not restart))
        (return-from ensure-thread-running (values)))
      ;; Retire a surviving watchdog before replacing a failed worker.
      (when (or thread (mqtt-input-watchdog input))
        (%stop-worker input)))
    (setf (mqtt-input-stopping input) nil)
    (setf (mqtt-input-thread input)
          (bt2:make-thread
           (lambda ()
             (unwind-protect
                  (loop until (mqtt-input-stopping input)
                        do (handler-case
                               (as:start-event-loop
                                (lambda () (process-mqtt-messages input)))
                             (error () (log:warn "MQTT event loop failed")))
                           (reset-connection input)
                           (loop repeat 10 until (mqtt-input-stopping input)
                                 do (sleep 0.1)))
               (reset-connection input)))
           :name "mqtt-message-processor"))
    (setf (mqtt-input-watchdog input)
          (bt2:make-thread
           (lambda ()
             (loop until (mqtt-input-stopping input)
                   do (%health-tick input) (sleep 0.1)))
           :name "mqtt-health-watchdog")))
  (values))

(defun stop (input)
  "Stop an independent transport; preserve legacy asynchronous queued commands."
  (bt2:with-lock-held ((mqtt-input-lifecycle-lock input))
    (%stop-worker input))
  (values))

(defvar *mqtt-input* nil "The single explicitly configured managed MQTT transport.")
(defvar *mqtt-manager-lock* (bt2:make-lock :name "mqtt-manager"))

(defun mqtt-init (&key (host "localhost") (port 1883) user pass)
  "Start the managed transport. Changed settings require MQTT-SHUTDOWN first."
  (bt2:with-lock-held (*mqtt-manager-lock*)
    (when (and *mqtt-input*
               (not (equal (list host port user pass)
                           (list (mqtt-input-host *mqtt-input*)
                                 (mqtt-input-port *mqtt-input*)
                                 (mqtt-input-user *mqtt-input*)
                                 (mqtt-input-pass *mqtt-input*)))))
      (error 'mqtt-publication-error))
    (unless *mqtt-input*
      (setf *mqtt-input* (make-mqtt-input :host host :port port :user user :pass pass)))
    (ensure-thread-running *mqtt-input*)
    (unless (member #'mqtt-shutdown hab::*shutdown-hooks*)
      (hab:add-to-shutdown #'mqtt-shutdown))
    *mqtt-input*))

(defun mqtt-shutdown ()
  "Stop the managed transport and clear all consumers and queued commands.
Call outside raw callbacks: the event-loop thread cannot join itself."
  (when (and *mqtt-input*
             (eq (bt2:current-thread) (mqtt-input-thread *mqtt-input*)))
    (error 'mqtt-publication-error))
  (when (fboundp '%shutdown-bindings) (%shutdown-bindings))
  (bt2:with-lock-held (*mqtt-manager-lock*)
    (when *mqtt-input*
      (stop *mqtt-input*)
      (bt2:with-lock-held ((mqtt-input-lock *mqtt-input*))
        (setf (mqtt-input-queue *mqtt-input*) nil
              (mqtt-input-subscriptions *mqtt-input*) nil
              (mqtt-input-callbacks *mqtt-input*) nil))
      (setf *mqtt-input* nil)))
  (values))

(defun %managed-input ()
  (or *mqtt-input* (error 'mqtt-disconnected)))

(defun mqtt-subscribe (topic handler)
  "Register an independent raw TOPIC/PAYLOAD handler and return its unique token."
  (let ((input (%managed-input)) (token (gensym "MQTT-SUB-")))
    (bt2:with-lock-held ((mqtt-input-lock input))
      (unless (%topic-required-p input topic)
        (%enqueue-command input (list :subscribe topic)))
      (push (make-mqtt-subscription :token token :topic topic :handler handler)
            (mqtt-input-subscriptions input)))
    token))

(defun mqtt-unsubscribe (token)
  "Remove only the consumer identified by TOKEN. Unknown tokens have no effect."
  (let ((input *mqtt-input*))
    (when input
      (bt2:with-lock-held ((mqtt-input-lock input))
        (let ((sub (find token (mqtt-input-subscriptions input)
                         :key #'mqtt-subscription-token)))
          (when sub
            (setf (mqtt-input-subscriptions input)
                  (remove sub (mqtt-input-subscriptions input)))
            (unless (%topic-required-p input (mqtt-subscription-topic sub))
              (%enqueue-command input (list :unsubscribe (mqtt-subscription-topic sub)))))))))
  (values))

(defun mqtt-publish (topic payload &key (qos 0) retain)
  "Queue an asynchronous publication on the managed connection."
  (check-type qos (integer 0 2))
  (let ((input (%managed-input))
        (prepared (%prepare-payload payload)))
    (bt2:with-lock-held ((mqtt-input-lock input))
      (unless (mqtt-input-client input) (error 'mqtt-disconnected))
      (%enqueue-command input (list :publish topic :payload prepared :qos qos :retain retain
             :connection (mqtt-input-client input)))))
  (values))

(defun mqtt-publish-confirmed (topic payload &key retain (timeout 3))
  "Wait for QoS 1 PUBACK; reject actor and event-loop blocking contexts."
  (publish-confirmed (%managed-input) topic payload :retain retain :timeout timeout))

(defun mqtt-health-snapshot (&rest arguments)
  "Return independent broker and loop health for an optional explicit transport.
Accept keyword-only options for the managed transport, or an input first."
  (let ((input (if (and arguments (not (keywordp (first arguments))))
                   (pop arguments) *mqtt-input*)))
    (destructuring-bind (&key (heartbeat-timeout 3)) arguments
      (check-type heartbeat-timeout (real (0)))
      (if input (health-snapshot input :heartbeat-timeout heartbeat-timeout)
          (alexandria:plist-hash-table
           (list "connected" nil "loop_alive" nil "heartbeat_age" nil) :test #'equal)))))
