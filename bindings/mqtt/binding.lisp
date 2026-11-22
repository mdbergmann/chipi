(in-package #:chipi.binding.mqtt)

(defstruct mqtt-diagnostic-event
  "A sanitized MQTT diagnostic published on Chipi's event stream."
  topic reason)

(defstruct (topic-state (:constructor %make-topic-state))
  subscription (bindings nil) snapshot received-at (sequence 0))

(defvar *topic-registry* (make-hash-table :test #'equal))
(defvar *binding-registry-lock* (bt2:make-lock :name "mqtt-bindings"))

(defclass mqtt-binding (binding:binding)
  ((topic :initarg :topic :initform nil :reader %binding-topic)
   (field :initarg :field :initform nil :reader %binding-field)
   (publish-topic :initarg :publish-topic :initform nil :reader %publish-topic)
   (publish-field :initarg :publish-field :initform nil :reader %publish-field)
   (qos :initarg :qos :initform 0 :reader %binding-qos)
   (retain :initarg :retain :initform nil :reader %binding-retain)
   (decode :initarg :decode :initform nil :reader %binding-decode)
   (encode :initarg :encode :initform nil :reader %binding-encode)
   (received-at :initform nil :reader mqtt-binding-received-at)
   (sequence :initform -1)
   (registered-p :initform nil)
   (destroyed-p :initform nil)
   (lock :initform (bt2:make-lock :name "mqtt-binding") :reader %binding-lock)))

(defun %diagnose (topic reason)
  (log:warn "MQTT binding diagnostic on ~A: ~A" topic reason)
  (ev:publish (isys:ensure-isys) (make-mqtt-diagnostic-event :topic topic :reason reason)))

(defun %transport-health-changed (&rest ignored)
  (declare (ignore ignored))
  (binding:publish-reachability-changed))

(define-condition unsupported-mqtt-null (error) ())

(defun %json-to-item (value)
  ;; MQTT-BINDING DR-3/4: null must not be confused with missing or false.
  (cond ((eq value :null) (error 'unsupported-mqtt-null))
        ((eq value 'yason:true) 'item:true)
        ((eq value 'yason:false) 'item:false)
        ((hash-table-p value)
         (let ((copy (make-hash-table :test #'equal)))
           (maphash (lambda (key val) (setf (gethash key copy) (%json-to-item val))) value)
           copy))
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'%json-to-item value))
        (t value)))

(defun %item-to-json (value)
  (cond ((eq value 'item:true) 'yason:true)
        ((eq value 'item:false) 'yason:false)
        ((hash-table-p value)
         (let ((copy (make-hash-table :test #'equal)))
           (maphash (lambda (key val)
                      (check-type key string)
                      (setf (gethash key copy) (%item-to-json val))) value)
           copy))
        ((and (vectorp value) (not (stringp value)))
         (map 'vector #'%item-to-json value))
        (t value)))

(defun %binding-receive (binding snapshot received-at sequence)
  (handler-case
      (let* ((field (%binding-field binding))
             (raw (if field
                      (multiple-value-bind (value present)
                          (if (hash-table-p snapshot) (gethash field snapshot) (values nil nil))
                        (unless present
                          (%diagnose (%binding-topic binding) :missing-field)
                          (return-from %binding-receive))
                        value)
                      snapshot))
             (value (%json-to-item raw))
             (decoded (if (%binding-decode binding)
                          (handler-case (funcall (%binding-decode binding) value)
                            (error ()
                              (%diagnose (%binding-topic binding) :decode-error)
                              (return-from %binding-receive)))
                          value)))
        ;; Decode outside the lock: application functions may call the registry.
        ;; Sequence checking prevents a late cached value overwriting a newer message.
        (%apply-binding-value binding decoded received-at sequence))
    (unsupported-mqtt-null () (%diagnose (%binding-topic binding) :null-value))
    (error () (%diagnose (%binding-topic binding) :invalid-value))))

(defun %apply-binding-value (binding value timestamp message-sequence)
  (bt2:with-lock-held ((%binding-lock binding))
    (with-slots (destroyed-p received-at sequence) binding
      (unless (or destroyed-p (< message-sequence sequence))
        (setf sequence message-sequence received-at timestamp)
        (dolist (item (binding::bound-items binding))
          (item:set-value item value :push nil :timestamp timestamp))))))

(defun %receive-topic (topic payload)
  (let ((snapshot
          (handler-case
              (with-input-from-string (stream payload)
                (prog1
                    (yason:parse stream :json-arrays-as-vectors t
                                       :json-booleans-as-symbols t :json-nulls-as-keyword t)
                  (loop :for char = (read-char stream nil nil) :while char
                        :unless (find char '(#\Space #\Tab #\Newline #\Return))
                          :do (error "Trailing MQTT JSON data: ~A" :invalid-json))))
            (error () (%diagnose topic :invalid-json) (return-from %receive-topic))))
        (timestamp (or (and (boundp '*message-received-at*) *message-received-at*)
                       (get-universal-time))))
    (multiple-value-bind (bindings sequence)
        (bt2:with-lock-held (*binding-registry-lock*)
          (let ((state (gethash topic *topic-registry*)))
            (unless state (return-from %receive-topic))
            (setf (topic-state-snapshot state) snapshot
                  (topic-state-received-at state) timestamp)
            (values (copy-list (topic-state-bindings state))
                    (incf (topic-state-sequence state)))))
      (dolist (binding bindings)
        (%binding-receive binding snapshot timestamp sequence)))))

(defun %register-binding (binding)
  (let ((topic (%binding-topic binding)) (snapshot nil) (timestamp nil) (sequence nil))
    (when topic
      (bt2:with-lock-held (*binding-registry-lock*)
        (let ((state (gethash topic *topic-registry*)))
          (unless state
            (setf state (%make-topic-state)
                  (gethash topic *topic-registry*) state)
            (handler-case
                (setf (topic-state-subscription state)
                      (mqtt-subscribe topic (lambda (actual payload)
                                              (declare (ignore actual))
                                              (%receive-topic topic payload))))
              (error (condition)
                (remhash topic *topic-registry*)
                (error condition))))
          (pushnew binding (topic-state-bindings state))
          (setf snapshot (topic-state-snapshot state)
                timestamp (topic-state-received-at state)
                sequence (topic-state-sequence state)
                (slot-value binding 'registered-p) t)))
      (when timestamp (%binding-receive binding snapshot timestamp sequence)))))

(defmethod binding:bind-item :after ((binding mqtt-binding) item)
  (declare (ignore item))
  (%register-binding binding))

(defmethod binding:destroy :before ((binding mqtt-binding))
  (bt2:with-lock-held ((%binding-lock binding))
    (setf (slot-value binding 'destroyed-p) t))
  (bt2:with-lock-held (*binding-registry-lock*)
    (let* ((topic (%binding-topic binding)) (state (gethash topic *topic-registry*)))
      (when state
        (setf (topic-state-bindings state) (remove binding (topic-state-bindings state)))
        (unless (topic-state-bindings state)
          (mqtt-unsubscribe (topic-state-subscription state))
          (remhash topic *topic-registry*)))))
  (setf (slot-value binding 'registered-p) nil))

(defun %shutdown-bindings ()
  (let ((bindings
          (bt2:with-lock-held (*binding-registry-lock*)
            (loop :for state :being :the :hash-values :of *topic-registry*
                  :append (copy-list (topic-state-bindings state))))))
    (dolist (binding bindings) (binding:destroy binding))))

(defmethod binding:binding-reachable-p ((binding mqtt-binding))
  (and (not (slot-value binding 'destroyed-p))
       (let ((health (mqtt-health-snapshot)))
         (and (gethash "connected" health) (gethash "loop_alive" health)))))

(defun %push-binding-value (binding value)
  (let* ((encoded (if (%binding-encode binding)
                      (funcall (%binding-encode binding) value) value))
         (json (%item-to-json encoded))
         (payload (if (%publish-field binding)
                      (serapeum:dict (%publish-field binding) json)
                      ;; Strings representing item values are JSON strings on the wire.
                      (yason:with-output-to-string* () (yason:encode json)))))
    (mqtt-publish (%publish-topic binding) payload
                  :qos (%binding-qos binding) :retain (%binding-retain binding))))

(defun mqtt-binding (&rest options &key topic field publish-topic publish-field qos retain decode encode)
  "Create an MQTT binding. Publish only when PUBLISH-TOPIC is supplied.
FIELD selects a top-level JSON field; omitted FIELD takes the complete JSON.
ENCODE runs before wrapping PUBLISH-FIELD. Incoming updates never push back."
  (declare (ignore qos retain decode encode))
  (unless (or topic publish-topic)
    (error "MQTT binding requires ~A" "an input or publication topic"))
  (when topic (check-type topic string))
  (when field (check-type field string))
  (when publish-topic (check-type publish-topic string))
  (when publish-field (check-type publish-field string))
  (let ((binding (apply #'make-instance 'mqtt-binding
                        :call-push-p (not (null publish-topic)) options)))
    (let ((qos (%binding-qos binding))) (check-type qos (integer 0 2)))
    (when publish-topic
      (setf (binding::push-fun binding) (lambda (value) (%push-binding-value binding value))))
    binding))
