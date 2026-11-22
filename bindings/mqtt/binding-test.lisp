(uiop:define-package #:chipi.binding.mqtt-test
  (:use #:cl)
  (:local-nicknames (#:mqtt #:chipi.binding.mqtt) (#:fm #:fiveam))
)
(in-package #:chipi.binding.mqtt-test)

(fm:def-suite :mqtt-tests :description "MQTT binding and managed transport contracts")
(fm:in-suite :mqtt-tests)

(defvar *subscriptions*)
(defvar *unsubscriptions*)
(defvar *publications*)
(defvar *updates*)
(defvar *diagnostics*)

(defmacro with-functions (replacements &body body)
  (let ((old (loop for replacement in replacements collect (gensym "OLD-"))))
    `(let ,(loop for (name lambda-list . forms) in replacements
                 for saved in old
                 collect `(,saved (symbol-function ',name)))
       (unwind-protect
            (progn
              ,@(loop for (name lambda-list . forms) in replacements
                      collect `(setf (symbol-function ',name) (lambda ,lambda-list ,@forms)))
              ,@body)
         ,@(loop for (name . rest) in replacements for saved in old
                 collect `(setf (symbol-function ',name) ,saved))))))

(defmacro with-binding-fixture (&body body)
  `(let ((*subscriptions* nil) (*unsubscriptions* nil) (*publications* nil)
         (*updates* nil) (*diagnostics* nil)
         (mqtt::*topic-registry* (make-hash-table :test #'equal)))
     (with-functions
         ((mqtt:mqtt-subscribe (topic handler)
            (let ((token (gensym "SUB-")))
              (push (list topic handler token) *subscriptions*) token))
          (mqtt:mqtt-unsubscribe (token) (push token *unsubscriptions*))
          (mqtt:mqtt-publish (topic payload &key qos retain)
            (push (list topic payload :qos qos :retain retain) *publications*))
          (item:set-value (item value &key push timestamp (persist t))
            (declare (ignore persist))
            (push (list item value :push push :timestamp timestamp) *updates*))
          (mqtt::%diagnose (topic reason) (push (list topic reason) *diagnostics*)))
       ,@body)))

(defun deliver (topic payload)
  (let ((subscription (find topic *subscriptions* :key #'first :test #'equal)))
    (unless subscription (error "No test subscription for ~A" topic))
    (funcall (second subscription) topic payload)))

(defun attach (binding item)
  (binding:bind-item binding item)
  binding)

(defun update-value (item)
  (second (find item *updates* :key #'first)))

(fm:test shared-topic-parses-once-and-does-not-echo
  (with-binding-fixture
    (let ((parse-count 0))
      (with-functions ((yason:parse (payload &rest options)
                           (declare (ignore options))
                           (loop while (read-char payload nil nil))
                           (incf parse-count)
                           (serapeum:dict "temperature" 21 "humidity" 45)))
        (attach (mqtt:mqtt-binding :topic "room" :field "temperature"
                                   :publish-topic "room/set") :temperature)
        (attach (mqtt:mqtt-binding :topic "room" :field "humidity") :humidity)
        (deliver "room" "{\"temperature\":21,\"humidity\":45}")
        (fm:is (= 1 parse-count))
        (fm:is (= 1 (length *subscriptions*)))
        (fm:is (= 21 (update-value :temperature)))
        (fm:is (= 45 (update-value :humidity)))
        (fm:is (every (lambda (update) (null (getf (cddr update) :push))) *updates*))
        (fm:is (null *publications*))))))

(fm:test false-null-missing-invalid-and-decode
  (with-binding-fixture
    (attach (mqtt:mqtt-binding :topic "sensor" :field "state") :sensor)
    (deliver "sensor" "{\"state\":false}")
    (fm:is (eq 'item:false (update-value :sensor)))
    (setf *updates* nil)
    (dolist (payload '("{\"state\":null}" "{}" "{"))
      (deliver "sensor" payload))
    (fm:is (null *updates*))
    (fm:is (equal '(:invalid-json :missing-field :null-value)
                  (mapcar #'second *diagnostics*)))
    (attach (mqtt:mqtt-binding :topic "broken" :decode (lambda (value)
                                                      (declare (ignore value))
                                                      (error "Decode failed: ~A" :test))) :broken)
    (deliver "broken" "12")
    (fm:is (null *updates*))
    (fm:is (eq :decode-error (second (first *diagnostics*))))))

(fm:test three-binding-directions
  (with-binding-fixture
    (let ((in (attach (mqtt:mqtt-binding :topic "in") :in))
          (out (attach (mqtt:mqtt-binding :publish-topic "out") :out))
          (both (attach (mqtt:mqtt-binding :topic "both" :publish-topic "both/set") :both)))
      (fm:is (not (binding:call-push-p in)))
      (fm:is (binding:call-push-p out))
      (fm:is (binding:call-push-p both))
      (fm:is (= 2 (length *subscriptions*)))
      (deliver "in" "true")
      (deliver "both" "7")
      (fm:is (eq 'item:true (update-value :in)))
      (fm:is (= 7 (update-value :both)))
      (fm:is (null *publications*))
      (binding:exec-push out 3)
      (binding:exec-push both 4)
      (fm:is (equal '("both/set" "out") (mapcar #'first *publications*))))))

(fm:test encode-before-publish-field-with-qos-retain
  (with-binding-fixture
    (let ((binding (mqtt:mqtt-binding :publish-topic "command" :publish-field "target"
                                      :encode (lambda (value) (* 2 value)) :qos 2 :retain t)))
      (binding:exec-push binding 6)
      (destructuring-bind (topic payload &key qos retain) (first *publications*)
        (fm:is (string= "command" topic))
        (fm:is (= 12 (gethash "target" payload)))
        (fm:is (= 2 qos))
        (fm:is (eq t retain))))
    (binding:exec-push (mqtt:mqtt-binding :publish-topic "boolean") 'item:false)
    (fm:is (string= "false" (second (first *publications*))))
    (binding:exec-push (mqtt:mqtt-binding :publish-topic "text") "OPEN")
    (fm:is (string= "\"OPEN\"" (second (first *publications*))))))

(fm:test late-binding-uses-original-time-and-duplicate-refreshes-received-time
  (with-binding-fixture
    (let ((first (attach (mqtt:mqtt-binding :topic "cache" :field "value") :first)))
      (let ((mqtt::*message-received-at* 100))
        (deliver "cache" "{\"value\":8}"))
      (let ((timestamp (mqtt:mqtt-binding-received-at first))
             (late (attach (mqtt:mqtt-binding :topic "cache" :field "value") :late)))
        (fm:is (= 8 (update-value :late)))
        (fm:is (= timestamp (mqtt:mqtt-binding-received-at late)))
        (fm:is (= timestamp (getf (cddr (first *updates*)) :timestamp)))
        (setf *updates* nil)
        (let ((mqtt::*message-received-at* 200))
          (deliver "cache" "{\"value\":8}"))
        (fm:is (= 200 (mqtt:mqtt-binding-received-at first)))
        (fm:is (= 2 (length *updates*)))))))

(fm:test destroy-preserves-other-consumer-and-redefinition-subscribes-once
  (with-binding-fixture
    (let ((first (attach (mqtt:mqtt-binding :topic "life") :first))
          (second (attach (mqtt:mqtt-binding :topic "life") :second)))
      (binding:destroy first)
      (fm:is (null *unsubscriptions*))
      (deliver "life" "2")
      (fm:is (null (update-value :first)))
      (fm:is (= 2 (update-value :second)))
      (binding:destroy second)
      (binding:destroy second)
      (fm:is (= 1 (length *unsubscriptions*)))
      (fm:is (= 0 (hash-table-count mqtt::*topic-registry*)))
      (let ((replacement (attach (mqtt:mqtt-binding :topic "life") :replacement)))
        (binding:bind-item replacement :another)
        (fm:is (= 2 (length *subscriptions*)))
        (deliver "life" "3")
        (fm:is (= 3 (update-value :replacement)))
        (fm:is (= 3 (update-value :another)))))))

(fm:test same-item-command-push-always
  ;; Use the real item actor to verify its command path reaches the MQTT binding.
  (let ((publications nil))
    (unwind-protect
         (with-functions ((mqtt:mqtt-publish (topic payload &key qos retain)
                              (declare (ignore topic qos retain))
                              (push payload publications)))
           (let ((item (item:make-item (gensym "MQTT-COMMAND-") :initial-value 5)))
             (item:add-binding item (mqtt:mqtt-binding :publish-topic "command"))
             (item:set-value item 5 :push :always)
             ;; A request queued after SET-VALUE acts as an actor mailbox barrier.
             (future:fresult (item:get-value item))
             (fm:is (equal '("5") publications))))
      (hab:shutdown))))

(fm:test real-item-redefinition-releases-old-subscription
  (let ((hab:*items* (make-hash-table)) (hab:*itemgroups* (make-hash-table))
        (hab:*rules* (make-hash-table)) (hab:*persistences* (make-hash-table)))
    (with-binding-fixture
    (unwind-protect
         (let* ((id (gensym "MQTT-REDEFINE-"))
                (group (gensym "MQTT-GROUP-"))
                (created-group (hab:defitemgroup group "MQTT test"))
                (old (hab:defitem id "MQTT test" nil
                       (mqtt:mqtt-binding :topic "redefined") :group (list group))))
           (declare (ignore created-group))
           (fm:is (= 1 (length *subscriptions*)))
           (let ((new (hab:defitem id "MQTT test" nil
                        (mqtt:mqtt-binding :topic "redefined") :group (list group))))
             (fm:is (not (eq old new)))
             (fm:is (= 1 (length *unsubscriptions*)))
             (fm:is (= 2 (length *subscriptions*)))
             (deliver "redefined" "42")
             (fm:is (= 42 (update-value new)))
             (fm:is (null (update-value old)))))
      (hab:shutdown)))))

(fm:test managed-init-retains-consumers-and-rejects-changed-config
  (let ((mqtt::*mqtt-input* nil)
        (hab::*shutdown-hooks* nil)
        (mqtt::*topic-registry* (make-hash-table :test #'equal))
        (starts nil) (stops nil))
    (with-functions ((mqtt:ensure-thread-running (input &key restart resubscribe)
                       (declare (ignore restart resubscribe)) (push input starts))
                     (mqtt:stop (input) (push input stops)))
      (let ((first (mqtt:mqtt-init :host "test.local" :port 28883 :pass "secret"))
             (token (mqtt:mqtt-subscribe "init" (lambda (&rest ignored)
                                                (declare (ignore ignored))))))
        (fm:is (eq first (mqtt:mqtt-init :host "test.local" :port 28883 :pass "secret")))
        (fm:is (= 1 (length (mqtt::mqtt-input-subscriptions first))))
        (fm:is (= 1 (length hab::*shutdown-hooks*)))
        (fm:signals mqtt:mqtt-publication-error (mqtt:mqtt-init :host "changed"))
        (fm:is (eq first mqtt::*mqtt-input*))
        (mqtt:mqtt-unsubscribe token)
        (mqtt:mqtt-shutdown)
        (fm:is (null mqtt::*mqtt-input*))
        (fm:is (equal (list first) stops))
        (fm:is (= 2 (length starts)))))))

(fm:test raw-consumers-repeat-isolate-failure-and-unsubscribe-independently
  (let* ((mqtt::*mqtt-input* (mqtt:make-mqtt-input :host "test.local"))
         (input mqtt::*mqtt-input*) (seen nil)
         (first (mqtt:mqtt-subscribe "room/+" (lambda (topic payload)
                                                (push (list :first topic payload) seen))))
         (second (mqtt:mqtt-subscribe "room/one" (lambda (topic payload)
                                                   (push (list :second topic payload) seen)))))
    (mqtt:mqtt-subscribe "room/one" (lambda (&rest ignored)
                                      (declare (ignore ignored)) (error "Consumer failed: ~A" :test)))
    (loop repeat 2 do
      (mqtt::dispatch-raw-message input "room/one" "false"))
    (fm:is (= 4 (length seen)))
    (fm:is (= 2 (count :first seen :key #'first)))
    (mqtt:mqtt-unsubscribe second)
    (mqtt:mqtt-unsubscribe second)
    (setf seen nil)
    (mqtt::dispatch-raw-message input "room/one" "false")
    (fm:is (equal '((:first "room/one" "false")) seen))
    (mqtt:mqtt-unsubscribe first)
    (fm:is (= 1 (length (mqtt::mqtt-input-subscriptions input))))))

(fm:test subscription-churn-rebuilds-one-wire-subscription
  (let* ((mqtt::*mqtt-input* (mqtt:make-mqtt-input :host "test.local"))
         (input mqtt::*mqtt-input*) (subscribes 0) (unsubscribes 0))
    (with-functions ((cl-mqtt:subscribe (client topic &rest options)
                       (declare (ignore client topic options)) (incf subscribes))
                     (cl-mqtt:unsubscribe (client topic)
                       (declare (ignore client topic)) (incf unsubscribes)))
      (let ((old (mqtt:mqtt-subscribe "churn" #'list)))
        (mqtt::process-next-command input nil)
        (mqtt:mqtt-unsubscribe old)
        (mqtt:mqtt-subscribe "churn" #'list)
        (loop while (mqtt::mqtt-input-queue input) do (mqtt::process-next-command input nil))
        (fm:is (= 1 subscribes))
        (fm:is (= 0 unsubscribes))
        (mqtt::reset-connection input)
        (mqtt::reschedule-subscriptions input)
        (mqtt::reschedule-subscriptions input)
        (fm:is (= 1 (length (mqtt::mqtt-input-queue input))))
        (mqtt::process-next-command input nil)
        (fm:is (= 2 subscribes))))))

(fm:test cancelled-confirmation-cannot-be-revived-and-errors-are-sanitized
  (let ((input (mqtt:make-mqtt-input :host "test.local"))
        (request (mqtt::make-publication-request nil (+ (get-internal-real-time)
                                                         internal-time-units-per-second))))
    (setf (mqtt::publication-request-state request) :cancelled)
    (mqtt::finish-confirmed-request input nil request :acknowledged)
    (fm:is (eq :cancelled (mqtt::publication-request-state request)))
    (fm:signals mqtt:mqtt-disconnected (mqtt:publish-confirmed input "topic" "secret"))
    (dolist (type '(mqtt:mqtt-timeout mqtt:mqtt-disconnected mqtt:mqtt-publication-error))
      (let ((message (princ-to-string (make-condition type))))
        (fm:is (typep (make-condition type) 'mqtt:mqtt-error))
        (fm:is (not (search "secret" message)))))))

(fm:test trailing-json-garbage-preserves-value-and-snapshot
  (with-binding-fixture
    (attach (mqtt:mqtt-binding :topic "strict") :strict)
    (deliver "strict" "12")
    (setf *updates* nil)
    (deliver "strict" "13 forbidden")
    (fm:is (null *updates*))
    (fm:is (eq :invalid-json (second (first *diagnostics*))))
    (attach (mqtt:mqtt-binding :topic "strict") :late)
    (fm:is (= 12 (update-value :late)))))

(fm:test confirmed-publication-signals-distinct-timeout-and-failure
  (let ((input (mqtt:make-mqtt-input :host "test.local")))
    (with-functions ((mqtt::mqtt-input-client (input)
                       (declare (ignore input)) :test-connection)
                     (mqtt::make-publication-request (connection deadline)
                       (declare (ignore deadline))
                       (make-instance 'mqtt::publication-request :connection connection :deadline 0)))
      (fm:signals mqtt:mqtt-timeout
        (mqtt:publish-confirmed input "command" "secret" :timeout 0.01))
      (fm:is (null (mqtt::mqtt-input-queue input))))
    (with-functions ((mqtt::mqtt-input-client (input)
                       (declare (ignore input)) :test-connection)
                     (mqtt::make-publication-request (connection deadline)
                       (let ((request (make-instance 'mqtt::publication-request
                                                     :connection connection :deadline deadline)))
                         (setf (mqtt::publication-request-state request) :failed) request)))
      (fm:signals mqtt:mqtt-publication-error
        (mqtt:publish-confirmed input "command" "secret" :timeout 0.01))
      (fm:is (null (mqtt::mqtt-input-queue input))))))

(fm:test complete-json-converts-booleans-before-decode
  (with-binding-fixture
    (let ((decoded nil))
      (attach (mqtt:mqtt-binding :topic "table"
                                 :decode (lambda (value)
                                           (setf decoded value)
                                           (gethash "values" value))) :table)
      (deliver "table" "{\"values\":[true,false,7]}")
      (fm:is (hash-table-p decoded))
      (fm:is (equalp (vector 'item:true 'item:false 7) (update-value :table))))))

(fm:test reachability-requires-connection-and-live-loop
  (with-binding-fixture
    (let ((health (serapeum:dict "connected" t "loop_alive" nil))
          (binding (mqtt:mqtt-binding :publish-topic "command")))
      (with-functions ((mqtt:mqtt-health-snapshot (&rest ignored)
                         (declare (ignore ignored)) health))
        (fm:is (not (binding:binding-reachable-p binding)))
        (setf (gethash "loop_alive" health) t)
        (fm:is (binding:binding-reachable-p binding))
        (setf (gethash "connected" health) nil)
        (fm:is (not (binding:binding-reachable-p binding)))
        (setf (gethash "connected" health) t)
        (binding:destroy binding)
        (fm:is (not (binding:binding-reachable-p binding)))))))

(fm:test managed-async-publication-does-not-replay-on-new-connection
  (let* ((mqtt::*mqtt-input* (mqtt:make-mqtt-input :host "test.local"))
         (input mqtt::*mqtt-input*) (sent nil))
    (fm:signals mqtt:mqtt-disconnected (mqtt:mqtt-publish "command" "secret"))
    (with-functions ((mqtt::mqtt-input-client (input)
                       (declare (ignore input)) :original-connection)
                     (cl-mqtt:publish (client topic payload &key qos retain)
                       (push (list client topic payload qos retain) sent)))
      (mqtt:mqtt-publish "command" "first" :qos 1 :retain t)
      (fm:is (eq :original-connection
                 (getf (cddr (first (mqtt::mqtt-input-queue input))) :connection)))
      (mqtt::process-next-command input :replacement-connection)
      (fm:is (null sent))
      (mqtt:mqtt-publish "command" "second" :qos 2 :retain t)
      (mqtt::process-next-command input :original-connection)
      (fm:is (equal '((:original-connection "command" "second" 2 t)) sent)))))

(fm:test health-keyword-options-and-payload-errors
  (let ((mqtt::*mqtt-input* (mqtt:make-mqtt-input)))
    (fm:is (null (gethash "connected" (mqtt:mqtt-health-snapshot :heartbeat-timeout 1))))
    (fm:is (null (gethash "loop_alive" (mqtt:mqtt-health-snapshot mqtt::*mqtt-input*
                                                                              :heartbeat-timeout 1))))
    (let ((invalid (make-hash-table)))
      (setf (gethash :invalid invalid) "private-payload")
      (handler-case (progn (mqtt:mqtt-publish "test" invalid) (fm:fail "Invalid keys accepted"))
        (mqtt:mqtt-publication-error (condition)
          (fm:is (not (search "private-payload" (princ-to-string condition)))))))))
