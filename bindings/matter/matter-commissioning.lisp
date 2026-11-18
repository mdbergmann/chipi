(in-package :chipi.binding.matter)

;; How a device becomes a node of Chipi's fabric, and what is done to a
;; node as a whole: discovery, the commissioning job, leaving the fabric,
;; the window for the next administrator, labels, and what a node is.
;;
;;   (matter-discover)                              ; what is on the network
;;   (matter-commission "3497-011-2332" :label "kitchen-lamp")
;;   (matter-describe "kitchen-lamp")               ; prints the definers to paste
;;
;; The controller (matter-controller.lisp) is single-threaded and polled,
;; so a commissioning is a job the controller's actor works through, state
;; by state, while its caller watches: `matter-start-commissioning' returns
;; the job at once, `matter-commission' is the same and waits.  The search
;; for the device is mDNS, which blocks for seconds: it runs as a task on
;; the `:tasks' dispatcher, never in the actor.
;;
;; `matter-set-label', `matter-decommission' and `matter-open-window' are
;; requests to a node like `matter-read': they return futures.
;; `matter-await' waits on one, for the REPL.

;; -----------------------------
;; pairing codes
;; -----------------------------

(defun %decode-code (code)
  "The onboarding payload of `CODE': a manual pairing code, 11 or 21 digits with or without dashes, or the text of a QR code (`MT:...'). An error for anything else."
  (unless (stringp code)
    (error "A pairing code is a string -- the digits, or the QR code's text -- not: ~s" code))
  (let ((code (string-trim '(#\Space #\Tab #\Newline #\Return) code)))
    (if (and (>= (length code) 3) (string-equal "MT:" code :end2 3))
        (matter:decode-qr-code code)
        (matter:decode-manual-pairing-code code))))

(defun %short-discriminator-p (payload)
  (matter:onboarding-payload-short-discriminator-p payload))

(defun matter-decode-pairing-code (code)
  "What the pairing code `CODE' says, as a plist; an error for a code that is none (a wrong check digit, say).
`CODE' is a manual pairing code -- 11 or 21 digits, dashes and spaces ignored -- or the text of a QR code (`MT:...').
`:kind' is `:manual' or `:qr'; `:short-discriminator' the 4 bits both carry, `:discriminator' the 12 bits of a QR code, NIL for a manual code; `:vendor-id' and `:product-id' where the code has them (a QR code, a manual code of 21 digits), else NIL; `:passcode' the setup passcode."
  (let ((payload (%decode-code code)))
    (flet ((id (value) (and (plusp value) value)))
      (list :kind (if (%short-discriminator-p payload) :manual :qr)
            :short-discriminator (matter:onboarding-payload-short-discriminator payload)
            :discriminator (and (not (%short-discriminator-p payload))
                                (matter:onboarding-payload-discriminator payload))
            :vendor-id (id (matter:onboarding-payload-vendor-id payload))
            :product-id (id (matter:onboarding-payload-product-id payload))
            :passcode (matter:onboarding-payload-passcode payload)))))

(defun %device-matches-code-p (payload device)
  "Whether the advertisement `DEVICE' can be the device the code of `PAYLOAD' belongs to: its discriminator is the code's, as far as the code carries it. A device that advertises none matches."
  (let ((advertised (getf device :discriminator)))
    (or (null advertised)
        (if (%short-discriminator-p payload)
            (= (ash advertised -8) (matter:onboarding-payload-short-discriminator payload))
            (= advertised (matter:onboarding-payload-discriminator payload))))))

;; -----------------------------
;; discovery
;; -----------------------------

(defparameter *discovery-timeout* 3
  "Seconds a search listens for answers, per kind of service.")

(defun %advert-name (device)
  "What to call the advertisement `DEVICE': the name it gives itself, else its vendor and product ids."
  (or (getf device :name)
      (getf device :device-name)
      (and (getf device :vendor-id)
           (format nil "Vendor #x~4,'0x~@[ product #x~4,'0x~]" (getf device :vendor-id) (getf device :product-id)))
      "Matter device"))

(defun %present-device (device)
  "The advertisement `DEVICE' as `matter-discover' hands it out: cl-matter's plist with a `:name' and the `:device-type-name' in front."
  (list* :name (%advert-name device)
         :device-type-name (and (getf device :device-type) (device-type-name (getf device :device-type)))
         device))

(defun %browse-commissionable (&key discriminator short-discriminator (timeout *discovery-timeout*) interface)
  "The nodes on the network whose commissioning window is open -- all of them, or those with the long `DISCRIMINATOR' or the `SHORT-DISCRIMINATOR' --, each as `%present-device' makes it. Blocks for `TIMEOUT' seconds: never in an actor."
  (mapcar #'%present-device
          (remove-if-not (lambda (device)
                           (member (getf device :commissioning-mode)
                                   (list matter:+commissioning-mode-basic+ matter:+commissioning-mode-enhanced+)))
                         (matter:discover-commissionable-nodes :discriminator discriminator
                                                               :short-discriminator short-discriminator
                                                               :timeout timeout :interface interface))))

(defun %operational-node-id (instance fabric-prefix)
  "The node id in `INSTANCE', the name of an operational service (`<compressed fabric id>-<node id>._matter._tcp.local'), when it is of the fabric whose compressed id in hex is `FABRIC-PREFIX'; else NIL."
  (let* ((name (subseq instance 0 (position #\. instance)))
         (dash (position #\- name)))
    (and dash
         (string-equal fabric-prefix name :end2 dash)
         (ignore-errors (parse-integer name :start (1+ dash) :radix 16)))))

(defun %browse-operational (fabric &key (timeout *discovery-timeout*) interface)
  "The nodes of `FABRIC' that are on the air, as a list of (node-id :addresses list :port port). Blocks for `TIMEOUT' seconds: never in an actor."
  (let* ((prefix (subseq (matter:fabric-operational-instance-name fabric 0) 0 16))
         (type (format nil "_I~a._sub.~a" prefix matter:*operational-service-type*)))
    (dns-sd:with-mdns-socket (socket :interface interface)
      (multiple-value-bind (instances records) (dns-sd:browse type :socket socket :timeout timeout)
        (loop :for instance :in instances
              :for node-id := (%operational-node-id instance prefix)
              :for service := (and node-id
                                   (dns-sd:resolve instance :socket socket :records records :timeout timeout))
              :when service
                :collect (list node-id :addresses (getf service :addresses) :port (getf service :port)))))))

(defun matter-discover (&key (timeout *discovery-timeout*))
  "What Matter has on the network right now, as a plist of three lists. Takes `TIMEOUT' seconds per list that is asked for and blocks that long: call it from the REPL or a task, not from a rule's actor.
`:commissionable' -- the devices whose pairing window is open: new ones, and ones another app put into pairing mode. Each a plist: `:name', `:device-type-name', `:discriminator', `:vendor-id', `:product-id', `:device-type', `:commissioning-mode' (1 the device's own window, 2 one an app opened), `:pairing-hint', `:pairing-instruction', `:addresses', `:port', `:instance', `:host' -- what the device advertises of them. One of these is what `matter-commission' takes as `:device'.
`:operational' -- the nodes of Chipi's fabric that answer, as `matter-nodes' lists them, `:addresses' and `:port' as the network shows them now.
`:silent' -- the nodes of the registry that did not answer.
Without a running controller only the first is filled."
  (destructuring-bind (&optional fabric nodes interface)
      (and *controller*
           (%controller-call
            (lambda ()
              (list (matter:commissioner-fabric (controller-commissioner *controller*))
                    (mapcar #'node-entry-plist (controller-nodes *controller*))
                    (controller-interface *controller*)))))
    (let ((commissionable (%browse-commissionable :timeout timeout :interface interface))
          (found (and nodes (%browse-operational fabric :timeout timeout :interface interface))))
      (list :commissionable commissionable
            :operational (loop :for node :in nodes
                               :for seen := (cdr (assoc (getf node :node-id) found))
                               :when seen
                                 :collect (let ((node (copy-list node)))
                                            (setf (getf node :addresses) (getf seen :addresses)
                                                  (getf node :port) (getf seen :port))
                                            node))
            :silent (remove-if (lambda (node) (assoc (getf node :node-id) found)) nodes)))))

(defun %choose-device (payload devices)
  "The one of the advertisements `DEVICES' -- those that answered to the discriminator of `PAYLOAD' -- to commission: the only one, or of several the only one whose vendor and product are the code's. An error that says what to do when there is none, or no telling which."
  (let ((devices (remove-if-not (lambda (device) (%device-matches-code-p payload device)) devices)))
    (cond ((null devices)
           (error "No device with this pairing code is ready to pair. Is its pairing window open?"))
          ((null (rest devices))
           (first devices))
          (t
           (let* ((vendor (matter:onboarding-payload-vendor-id payload))
                  (product (matter:onboarding-payload-product-id payload))
                  (same (and (plusp vendor)
                             (remove-if-not (lambda (device)
                                              (and (eql vendor (getf device :vendor-id))
                                                   (or (zerop product) (eql product (getf device :product-id)))))
                                            devices))))
             (if (and same (null (rest same)))
                 (first same)
                 (error "~d devices answer to this pairing code: ~{~a~^, ~}. Say which with :device, one of (matter-discover)'s."
                        (length devices) (mapcar (lambda (device) (getf device :name)) devices))))))))

;; -----------------------------
;; the commissioning job
;; -----------------------------

(defparameter *commissioning-states*
  '((:discovering . "finding the device")
    (:pairing . "establishing the pairing session")
    (:attesting . "checking the device's identity")
    (:credentials . "issuing credentials")
    (:completing . "completing")
    (:reading . "reading what the device is")
    (:labeling . "labeling"))
  "The states a commissioning job goes through, in their order, each with a line for who watches. After them it is `:done' or `:failed'.")

(defstruct (commissioning-job (:constructor %make-commissioning-job))
  "One commissioning, from pairing code to node. STATE is one of
`*commissioning-states*', then `:done' or `:failed'; LABEL the label asked
for, and once the node is registered the one it got; DEVICE the
advertisement of the device being commissioned, when it was found; NODE the
new node's plist once the job is `:done'; ERROR the
`matter-commissioning-error' of one that `:failed'."
  label
  (state :discovering)
  device node error
  ;; not for who watches: the code's payload, and what the job was started with
  payload passcode country-code (device-label t) on-step on-complete)

(defmethod print-object ((job commissioning-job) stream)
  ;; never the passcode
  (print-unreadable-object (job stream :type t)
    (format stream "~@[~s ~]~(~s~)" (commissioning-job-label job) (commissioning-job-state job))))

(defun commissioning-job-running-p (job)
  "Whether `JOB' is neither `:done' nor `:failed' yet."
  (not (member (commissioning-job-state job) '(:done :failed))))

(defun matter-commissioning-state-text (state)
  "A line for the state `STATE' of a commissioning job: \"finding the device\"."
  (or (cdr (assoc state *commissioning-states*))
      (string-downcase (symbol-name state))))

(defun %timeout-p (condition)
  "Whether `CONDITION' says that the device did not answer."
  (or (and (typep condition 'matter:interaction-error)
           (eql matter:+im-status-timeout+ (matter:interaction-error-status condition)))
      (search "timed out" (princ-to-string condition) :test #'char-equal)
      (search "timeout" (princ-to-string condition) :test #'char-equal)))

(defun %failure-text (state cause)
  "Why a commissioning failed in `STATE' with the condition `CAUSE', in plain words."
  (let ((text (princ-to-string cause))
        (what (matter-commissioning-state-text state)))
    (cond ((eq state :discovering)
           text)
          ((typep cause 'matter:attestation-error)
           (format nil "The device's identity could not be verified: ~a" text))
          ((search "CommissioningError 4" text)
           "The device is being paired by another app. Try again in a minute.")
          ((search "NodeOperationalCertStatus 5" text)
           "The device has no room for another app: remove it from one it is paired with.")
          ((%timeout-p cause)
           (format nil "The device did not answer while ~a.~:[~; Is its pairing window still open?~]"
                   what (eq state :pairing)))
          (t
           (format nil "It failed while ~a: ~a" what text)))))

(define-condition matter-commissioning-error (error)
  ((state :initarg :state
          :reader matter-commissioning-error-state
          :documentation "The state the job was in when it failed.")
   (cause :initarg :cause
          :reader matter-commissioning-error-cause
          :documentation "The condition it failed with."))
  (:report (lambda (condition stream)
             (write-string (%failure-text (matter-commissioning-error-state condition)
                                          (matter-commissioning-error-cause condition))
                           stream)))
  (:documentation "Why a commissioning job failed: the state it was in and the condition it ran into, reported in plain words."))

(defun %job-notify (job function)
  "Calls `FUNCTION', one of the job's callbacks, with `JOB'; an error of the watcher's is logged."
  (when function
    (handler-case (funcall function job)
      (error (c)
        (log:warn "Matter controller: a watcher of ~a signalled: ~a" job c)))))

(defun %job-state (job state)
  "Moves the running `JOB' on to `STATE' and tells who watches."
  (when (and (commissioning-job-running-p job)
             (not (eq state (commissioning-job-state job))))
    (setf (commissioning-job-state job) state)
    (log:info "Matter controller: commissioning~@[ ~s~]: ~a"
              (commissioning-job-label job) (matter-commissioning-state-text state))
    (%job-notify job (commissioning-job-on-step job))))

(defun %job-finish (job result)
  "The end of the running `JOB': `RESULT' is the new node's plist, or the condition that says why there is none. Who watches is told the last state, then the result."
  (when (commissioning-job-running-p job)
    (cond ((typep result 'condition)
           (let ((error (if (typep result 'matter-commissioning-error)
                            result
                            (make-condition 'matter-commissioning-error
                                            :state (commissioning-job-state job) :cause result))))
             (log:warn "Matter controller: commissioning~@[ ~s~] failed: ~a" (commissioning-job-label job) error)
             (setf (commissioning-job-error job) error
                   (commissioning-job-state job) :failed)))
          (t
           (setf (commissioning-job-node job) result
                 (commissioning-job-state job) :done)))
    (%job-notify job (commissioning-job-on-step job))
    (when-let ((on-complete (commissioning-job-on-complete job)))
      (handler-case (funcall on-complete (or (commissioning-job-error job) result))
        (error (c)
          (log:warn "Matter controller: a watcher of ~a signalled: ~a" job c))))))

(defun %job-stopped (controller)
  "The controller stops: a commissioning that runs fails with that."
  (when-let ((job (controller-job controller)))
    (%job-finish job (make-condition 'simple-error
                                     :format-control "The Matter controller was stopped"))))

(defun %step-state (step)
  "The job state that the step `STEP' of cl-matter's commissioning flow belongs to, or NIL for its last, which is not the job's."
  (case step
    ((:pase :arm-fail-safe :read-basic-information :regulatory-config) :pairing)
    ((:dac :pai :attestation) :attesting)
    ((:csr :add-root :add-noc) :credentials)
    ((:case :complete) :completing)))

;; -----------------------------
;; what a new node is asked and told
;; -----------------------------

(defconstant +attribute-node-label+ 5)
(defconstant +node-label-length+ 32)

(defun %describe-paths ()
  "What a node is asked about itself: its Basic Information and the Descriptor of every endpoint."
  (list (matter:attribute-path 0 matter:+cluster-basic-information+ nil)
        (matter:attribute-path nil matter:+cluster-descriptor+ nil)))

(defun %label-fabric (controller entry session done)
  "Tells the node of `ENTRY' the name of Chipi's fabric, then calls `DONE'. A node that does not take it is no failure of the commissioning: it is logged."
  (let ((label (controller-fabric-label controller)))
    (if (and label (plusp (length label)))
        (matter:start-fabric-label-update
         (controller-commissioner controller) session label
         :on-complete (lambda (interaction error)
                        (declare (ignore interaction))
                        (when error
                          (log:warn "Matter controller: ~a did not take the fabric label ~s: ~a" entry label error))
                        (funcall done)))
        (funcall done))))

(defun %write-node-label (controller entry session done)
  "Writes the label of `ENTRY' to its node's NodeLabel -- the name the node's other administrators show --, cut to what the attribute holds, then calls `DONE'. A node that does not take it is logged."
  (let* ((label (node-entry-label entry))
         (label (subseq label 0 (min +node-label-length+ (length label)))))
    (matter:start-write
     (controller-model controller) session
     (list (matter:attribute-write 0 matter:+cluster-basic-information+ +attribute-node-label+ label))
     :on-complete (lambda (interaction)
                    ;; whatever the answer is made of, DONE is called
                    (handler-case
                        (multiple-value-bind (statuses condition)
                            (%interaction-outcome controller entry session interaction)
                          (when condition (error condition))
                          (let ((status (and statuses (matter:attribute-status-status (first statuses)))))
                            (unless (and status (matter:im-status-success-p status))
                              (error "the node answered ~a"
                                     (and status (matter:im-status-name (matter:im-status-status status)))))))
                      (error (c)
                        (log:warn "Matter controller: ~a did not take its label as NodeLabel: ~a" entry c)))
                    (funcall done)))))

(defun %describe-node (controller entry session done)
  "Reads what the node of `ENTRY' says about itself into the entry and the registry file, then calls `DONE'. A node that does not say is logged and stays registered by its id and label."
  (matter:start-read (controller-model controller) session (%describe-paths)
                     :on-complete (lambda (interaction)
                                    ;; whatever the answer is made of, DONE is called
                                    (handler-case
                                        (multiple-value-bind (reports condition)
                                            (%interaction-outcome controller entry session interaction)
                                          (when condition (error condition))
                                          (describe-node-entry entry reports)
                                          (%write-registry controller))
                                      (error (c)
                                        (log:warn "Matter controller: ~a does not describe itself: ~a" entry c)))
                                    (funcall done))))

;; -----------------------------
;; the job, in the controller's actor
;; -----------------------------

(defun %commissioned (controller job commissioning host port)
  "The end of cl-matter's commissioning flow for `JOB': a failed one fails the job; a node that joined is registered at once -- it is on the fabric whatever happens next --, then described, labelled and told the fabric's name, and the job is done with its plist."
  (if (not (matter:commissioning-complete-p commissioning))
      (%job-finish job (matter:commissioning-condition commissioning))
      (let* ((result (matter:commissioning-result commissioning))
             (session (getf result :session))
             (now (get-universal-time))
             (label (or (commissioning-job-label job)
                        (getf (commissioning-job-device job) :device-name)
                        "matter-node"))
             (entry (make-node-entry :node-id (getf result :node-id)
                                     :label (unique-node-label (controller-nodes controller) label)
                                     :addresses (list host) :port port
                                     :commissioned-at now :last-seen now)))
        (setf (controller-nodes controller) (append (controller-nodes controller) (list entry))
              (connection-session (%connection controller (node-entry-node-id entry))) session
              (commissioning-job-label job) (node-entry-label entry))
        (%write-commissioner-state controller)
        (%write-registry controller)
        (log:info "Matter controller: commissioned ~a at [~a]:~d" entry host port)
        ;; bindings that were defined before their node was commissioned
        (%schedule-subscribe controller entry)
        (labels ((finish ()
                   (%job-finish job (node-entry-plist entry)))
                 (guarded (what thunk)
                   ;; the node is commissioned: what fails from here on is logged, and the job is done
                   (handler-case (funcall thunk)
                     (error (c)
                       (log:warn "Matter controller: ~a, ~a: ~a" entry what c)
                       (finish))))
                 (label-fabric ()
                   (guarded "fabric label"
                            (lambda () (%label-fabric controller entry session #'finish))))
                 (label-node ()
                   (%job-state job :labeling)
                   (if (commissioning-job-device-label job)
                       (guarded "node label"
                                (lambda () (%write-node-label controller entry session #'label-fabric)))
                       (label-fabric))))
          (%job-state job :reading)
          (guarded "description"
                   (lambda () (%describe-node controller entry session #'label-node)))))))

(defun %job-pair (controller job device)
  "Commissions `DEVICE', an advertisement -- its first address and its port are what counts -- for `JOB'. In the controller's actor."
  (when (commissioning-job-running-p job)
    (handler-case
        (let ((host (first (getf device :addresses)))
              (port (getf device :port)))
          (unless (and host port)
            (error "The device ~a advertises no address" (%advert-name device)))
          (setf (commissioning-job-device job) device)
          (%job-state job :pairing)
          (matter:start-commissioning
           (controller-commissioner controller) host port
           :passcode (commissioning-job-passcode job)
           :country-code (commissioning-job-country-code job)
           :attestation (controller-attestation controller)
           :on-step (lambda (commissioning)
                      (when-let ((state (%step-state (matter:commissioning-step commissioning))))
                        (%job-state job state)))
           :on-complete (lambda (commissioning)
                          (handler-case (%commissioned controller job commissioning host port)
                            (error (c) (%job-finish job c))))))
      (error (c)
        (%job-finish job c)))))

(defun %claim-job (controller job)
  "Makes `JOB' the controller's commissioning job; an error while another one runs, or when its label is a node's already. In the controller's actor."
  (let ((current (controller-job controller))
        (label (commissioning-job-label job)))
    (when (and current (commissioning-job-running-p current))
      (error "A commissioning is under way~@[, of ~s~]: one at a time" (commissioning-job-label current)))
    (when (and label (find-node-entry (controller-nodes controller) label))
      (error "A Matter node ~s is commissioned already" label))
    (setf (controller-job controller) job)))

(defun %check-label (label)
  (unless (and (stringp label) (plusp (length label)))
    (error "A node needs a label, a string that is not empty, not: ~s" label))
  label)

(defun %commission-at (host port &key passcode label (country-code *commissioning-country-code*))
  "Commissions the device at `HOST':`PORT', whose commissioning window is open for `PASSCODE', into Chipi's fabric and registers it under `LABEL': a commissioning job without the search. Returns a future of the new node's plist, as `matter-nodes' lists it; it fails with the condition that says why the device did not join.
The device's attestation is checked as the controller was configured (`:attestation'). The pure-Lisp handshakes take their seconds: wait on the future with a generous timeout."
  (%check-label label)
  (%controller-future
   (lambda (controller resolve)
     (let ((job (%make-commissioning-job :label label :passcode passcode :country-code country-code
                                         :on-complete resolve)))
       (%claim-job controller job)
       (%job-pair controller job (list :addresses (list host) :port port))))))

(defun %run-task (thunk)
  "Runs `THUNK' on the `:tasks' dispatcher of chipi's actor system: what blocks and must not do so in an actor."
  (tasks:with-context ((isys:ensure-isys) :tasks)
    (tasks:task-start thunk)))

(defun %abort-job (controller job condition)
  "Ends the running `JOB' with `CONDITION' when its hand-off to the controller's actor went wrong, so that it does not stay for good the one under way. Logs when even that fails."
  (handler-case
      (matter-actor-run (controller-actor controller)
                        (lambda () (%job-finish job condition)))
    (error (c)
      (log:warn "Matter controller: ~a could not be ended with ~a: ~a" job condition c))))

(defun %job-discover (controller job timeout)
  "Finds the device `JOB's pairing code belongs to and hands it to the controller's actor to commission. Blocks for the search: on a task's thread."
  (let* ((payload (commissioning-job-payload job))
         (device (handler-case
                     (%choose-device
                      payload
                      (if (%short-discriminator-p payload)
                          (%browse-commissionable
                           :short-discriminator (matter:onboarding-payload-short-discriminator payload)
                           :timeout timeout :interface (controller-interface controller))
                          (%browse-commissionable
                           :discriminator (matter:onboarding-payload-discriminator payload)
                           :timeout timeout :interface (controller-interface controller))))
                   (error (c) c))))
    ;; a controller that was stopped meanwhile has failed the job already
    (when (eq controller *controller*)
      (handler-case
          (matter-actor-run (controller-actor controller)
                            (lambda ()
                              (if (typep device 'condition)
                                  (%job-finish job device)
                                  (%job-pair controller job device))))
        (error (c)
          (%abort-job controller job c))))))

(defun matter-start-commissioning (code &key label device (device-label t) on-step on-complete
                                          (timeout *discovery-timeout*))
  "Starts commissioning the device the pairing code `CODE' belongs to into Chipi's fabric and returns at once, with the `commissioning-job' to watch. `matter-commission' is this and waits.
`CODE': the manual pairing code, 11 or 21 digits with or without dashes, or the text of the QR code (`MT:...'). The device's pairing window must be open: a new device opens it when powered; one that is in Apple Home or Google Home gets it opened there (\"pairing mode\"), which also shows the code.
`LABEL': the name of the node in Chipi, the `:node' of its `matter-binding's. Default: the name the device advertises, else \"matter-node\"; made unique with -2, -3 when taken. A `LABEL' given that a node has already is an error.
`DEVICE': one of the `:commissionable' devices of `matter-discover', to commission that one without a search. Without it the device is searched for by the code's discriminator, `TIMEOUT' seconds; several that answer are told apart by the code's vendor and product, and when that does not tell, the job fails and says so.
`DEVICE-LABEL': whether the label is also written to the device's NodeLabel, the name its other apps show. Default T.
`ON-STEP': a function of the job, called on every change of its state: `:discovering', `:pairing', `:attesting', `:credentials', `:completing', `:reading', `:labeling', then `:done' or `:failed' (`*commissioning-states*', `matter-commissioning-state-text').
`ON-COMPLETE': a function called once, with the new node's plist as `matter-nodes' lists it, or with the `matter-commissioning-error' that says why there is none.
Both run on the controller actor's thread: they must not block, nor call into the controller and wait.
A code that is none, a `DEVICE' the code does not belong to, a commissioning that is under way: these are signalled here. One commissioning at a time."
  (let ((controller (or *controller* (error "The Matter controller is not running")))
        (payload (%decode-code code)))
    (when label
      (%check-label label))
    (when (and device (not (%device-matches-code-p payload device)))
      (error "This code is for another device: its discriminator is ~d, ~a advertises ~d"
             (if (%short-discriminator-p payload)
                 (matter:onboarding-payload-short-discriminator payload)
                 (matter:onboarding-payload-discriminator payload))
             (%advert-name device)
             (if (%short-discriminator-p payload)
                 (ash (getf device :discriminator) -8)
                 (getf device :discriminator))))
    (let ((job (%make-commissioning-job :label label :payload payload
                                        :passcode (matter:onboarding-payload-passcode payload)
                                        :country-code *commissioning-country-code*
                                        :device device :device-label device-label
                                        :on-step on-step :on-complete on-complete)))
      (matter-actor-call (controller-actor controller)
                         (lambda () (%claim-job controller job)))
      (handler-case
          (if device
              (matter-actor-run (controller-actor controller)
                                (lambda () (%job-pair controller job device)))
              (%run-task (lambda () (%job-discover controller job timeout))))
        (error (c)
          ;; the caller gets the error: the job is no one's to watch, and must not stay the one under way
          (setf (commissioning-job-on-step job) nil
                (commissioning-job-on-complete job) nil)
          (%abort-job controller job c)
          (error c)))
      job)))

(defun matter-commission (code &rest args &key label device device-label (wait 120) &allow-other-keys)
  "Commissions the device the pairing code `CODE' belongs to into Chipi's fabric and returns the new node's plist, as `matter-nodes' lists it.

  (matter-commission \"3497-011-2332\" :label \"kitchen-lamp\")
  (matter-commission \"MT:-24J0AFN00KA0648G00\" :label \"kitchen-lamp\")

The keys are those of `matter-start-commissioning', which this is, waiting up to `WAIT' seconds for the job: the pure-Lisp handshakes take their time, a device on Thread more. A commissioning that fails is signalled, as the `matter-commissioning-error' that says in which state and why. One that is not through in time is signalled too and goes on: `matter-commissioning-job' shows it."
  (declare (ignore label device device-label))
  (let ((job (apply #'matter-start-commissioning code (remove-from-plist args :wait)))
        (deadline (+ (get-internal-real-time) (* wait internal-time-units-per-second))))
    (loop :while (and (commissioning-job-running-p job)
                      (< (get-internal-real-time) deadline))
          :do (sleep 0.1))
    (case (commissioning-job-state job)
      (:done (commissioning-job-node job))
      (:failed (error (commissioning-job-error job)))
      (t (error "The commissioning is not through after ~a seconds (~a); it goes on, (matter-commissioning-job) shows it"
                wait (matter-commissioning-state-text (commissioning-job-state job)))))))

(defun matter-commissioning-job ()
  "The commissioning job that runs, or the one that ran last; NIL when there was none since the controller started."
  (%controller-call (lambda () (controller-job *controller*))))

;; -----------------------------
;; requests to a node as a whole
;; -----------------------------

(defun matter-await (future &key (timeout 30))
  "Waits up to `TIMEOUT' seconds for `FUTURE', one of the controller's -- `matter-read', `matter-invoke', `matter-open-window' ... --, and returns its value. A future that failed is signalled, with the condition it failed with; so is one that is not there in time. For the REPL: never in an actor."
  (multiple-value-bind (result fut)
      (future:fawait future :timeout timeout :sleep-time 0.01)
    (unless (future:complete-p fut)
      (error "No answer within ~a seconds" timeout))
    (when (typep result 'condition)
      (error result))
    result))

(defun matter-set-label (node label &key (device-label t))
  "Gives the commissioned `NODE' -- its label or node id -- the new label `LABEL' and returns a future of its plist.
The label is the `:node' of the node's `matter-binding's: bindings that name the new label bind the node from now on, those that name the old one no longer do -- change them in the configuration along with it. A binding by node id is not touched.
With `DEVICE-LABEL', the default, the label is also written to the device's NodeLabel, the name its other apps show; a device that does not take it, or is not reached, is logged and the label stays changed here.
The future fails when there is no such node, or when another node has the label."
  (%check-label label)
  (%controller-future
   (lambda (controller resolve)
     (let* ((entry (%require-node controller node))
            (other (find-node-entry (controller-nodes controller) label)))
       (when (and other (not (eq other entry)))
         (error "A Matter node ~s is commissioned already" label))
       (log:info "Matter controller: ~a is ~s from now on" entry label)
       (setf (node-entry-label entry) label)
       (%write-registry controller)
       ;; the bindings of the node are others now
       (%schedule-subscribe controller entry)
       (flet ((finish ()
                (funcall resolve (node-entry-plist entry))))
         (if device-label
             (%with-session controller entry
                            (lambda (session)
                              (%write-node-label controller entry session #'finish))
                            (lambda (condition)
                              (log:warn "Matter controller: ~a was not told its label: ~a" entry condition)
                              (finish)))
             (finish)))))))

(defun %forget-node (controller entry)
  "Takes the node of `ENTRY' out of the registry and lets go of its subscription and session; what waited for a session to it fails."
  (let* ((node-id (node-entry-node-id entry))
         (connection (gethash node-id (controller-connections controller))))
    (setf (controller-nodes controller) (remove entry (controller-nodes controller)))
    (when connection
      (remhash node-id (controller-connections controller))
      (when-let ((subscription (connection-subscription connection)))
        (ignore-errors (matter:unsubscribe subscription)))
      (let ((session (connection-session connection)))
        (when (and session (matter:session-active-p session))
          (ignore-errors (matter:close-session (controller-channel controller) session))))
      (let ((condition (make-condition 'simple-error
                                       :format-control "The Matter node ~s was removed"
                                       :format-arguments (list (node-entry-label entry)))))
        (dolist (waiter (reverse (connection-waiters connection)))
          (ignore-errors (funcall (cdr waiter) condition)))))
    (%write-registry controller)
    (log:info "Matter controller: ~a is no node of the fabric any more" entry)))

(defun matter-decommission (node &key force)
  "Takes the commissioned `NODE' -- its label or node id -- out of Chipi's fabric and returns a future of T: the device is told to remove the fabric (`RemoveFabric'), its session and subscription end, and the registry forgets it. The device stays with its other apps; one that has none opens its pairing window again.
The future fails when the device does not answer or refuses, and the node stays. With `FORCE' the node is forgotten all the same -- for a device that is gone for good; one that comes back still holds the fabric and has to be reset.
Items bound to the node keep their last value and get no more."
  (%controller-future
   (lambda (controller resolve)
     (let ((entry (%require-node controller node))
           (model (controller-model controller)))
       (labels ((forget ()
                  (%forget-node controller entry)
                  (funcall resolve t))
                (failed (condition)
                  (cond (force
                         (log:warn "Matter controller: ~a did not leave the fabric (~a); forgotten all the same"
                                   entry condition)
                         (forget))
                        (t
                         (funcall resolve
                                  (make-condition 'simple-error
                                                  :format-control "~a did not leave the fabric: ~a. With :force t it is forgotten all the same."
                                                  :format-arguments (list entry condition))))))
                (guarded (thunk)
                  (handler-case (funcall thunk)
                    (error (c) (funcall resolve c))))
                (remove-fabric (index)
                  (%start-request
                   controller entry
                   (lambda (session on-complete)
                     (matter:start-invoke
                      model session
                      (matter:command-path 0 (%cluster-id :operational-credentials)
                                           (%command-id :operational-credentials :remove-fabric))
                      (%command-fields :operational-credentials :remove-fabric (list :fabric-index index))
                      :on-complete on-complete))
                   (lambda (responses)
                     (let ((answer (%response-value (first responses) "RemoveFabric")))
                       (unless (and (listp answer) (eql matter:+noc-status-ok+ (getf answer :status-code)))
                         (error "RemoveFabric was answered ~s" answer))
                       t))
                   (lambda (result)
                     (guarded (lambda ()
                                (if (typep result 'condition)
                                    (failed result)
                                    (forget))))))))
         ;; which of the device's fabrics is Chipi's: the device says
         (%start-request
          controller entry
          (lambda (session on-complete)
            (matter:start-read
             model session
             (list (matter:attribute-path 0 (%cluster-id :operational-credentials)
                                          (%attribute-id :operational-credentials :current-fabric-index)))
             :on-complete on-complete))
          (lambda (reports)
            (%report-value (first reports) "the fabric index"))
          (lambda (result)
            (guarded (lambda ()
                       (if (typep result 'condition)
                           (failed result)
                           (remove-fabric result)))))))))))

(defun matter-open-window (node &key (timeout 300))
  "Asks the commissioned `NODE' -- its label or node id -- to open its pairing window for `TIMEOUT' seconds (180 to 900), for another app to add the device: the reverse of how Chipi got in.
Returns a future of a plist: `:manual-code', the 11 digits to type into the other app, `:qr-code', the text of the QR code (`qr-string' and `qr-svg' draw it), `:discriminator' and `:timeout'. The code is made for this window and good for it only.
The future fails when the device does not answer or does not open: a window is open already, say."
  (%controller-future
   (lambda (controller resolve)
     (let ((entry (%require-node controller node))
           (payload nil))
       (%start-request
        controller entry
        (lambda (session on-complete)
          (matter:start-commissioning-window-request
           (controller-commissioner controller) session
           :timeout timeout
           :vendor-id (or (node-entry-vendor-id entry) 0)
           :product-id (or (node-entry-product-id entry) 0)
           :on-complete (lambda (interaction onboarding-payload)
                          (setf payload onboarding-payload)
                          (funcall on-complete interaction))))
        (lambda (responses)
          (%response-value (first responses) (format nil "OpenCommissioningWindow on ~s" node))
          (log:info "Matter controller: ~a has its pairing window open for ~d s" entry timeout)
          (list :manual-code (matter:encode-manual-pairing-code payload)
                :qr-code (matter:encode-qr-code payload)
                :discriminator (matter:onboarding-payload-discriminator payload)
                :timeout timeout))
        resolve)))))

;; -----------------------------
;; what a node is, and the items it has
;; -----------------------------

(defun %bindings-of (label node-id)
  "The live `matter-binding's that name the node by `LABEL' or by `NODE-ID'."
  (remove-if-not (lambda (binding)
                   (let ((node (matter-binding-node binding)))
                     (if (stringp node)
                         (string= node label)
                         (eql node node-id))))
                 (bt:with-lock-held (*matter-bindings-lock*)
                   (copy-list *matter-bindings*))))

(defun %item-names (bindings)
  "The names of the items `BINDINGS' are bound to, each once, in the order of the bindings' making."
  (remove-duplicates
   (loop :for binding :in (reverse bindings)
         :append (mapcar #'item:name (binding::bound-items binding)))
   :test #'string=))

(defun %node-plist (node)
  "The plist of the commissioned `NODE', its label or node id; an error when there is none."
  (%controller-call
   (lambda ()
     (node-entry-plist (%require-node *controller* node)))))

(defun matter-items-of (node)
  "The names of the items bound to the commissioned `NODE' -- its label or node id -- with a `matter-binding'."
  (let ((plist (%node-plist node)))
    (%item-names (%bindings-of (getf plist :label) (getf plist :node-id)))))

(defun matter-node-of (item)
  "The label of the Matter node the item `ITEM' -- an `item:item' or the id of one -- is bound to with a `matter-binding', as second value the node's id; NIL for an item that has none, or whose node is not commissioned."
  (let* ((item (if (symbolp item) (hab:get-item item) item))
         (binding (and item (find-if (lambda (binding) (typep binding 'matter-binding))
                                     (item:bindings item))))
         (entry (and binding *controller*
                     (%controller-call
                      (lambda ()
                        (when-let ((entry (find-node-entry (controller-nodes *controller*)
                                                           (matter-binding-node binding))))
                          (cons (node-entry-label entry) (node-entry-node-id entry))))))))
    (and entry (values (car entry) (cdr entry)))))

(defun %cluster-name (id)
  "The name cl-matter has for the cluster `ID', else its id in hex."
  (let ((definition (matter:find-cluster-definition id nil)))
    (if definition
        (string-downcase (symbol-name (matter:cluster-definition-name definition)))
        (format nil "#x~4,'0x" id))))

(defun node-suggestions (plist bindings)
  "What each endpoint of the node `PLIST' -- as `matter-nodes' lists it -- is, and the definer suggested for it; `BINDINGS' are the live `matter-binding's of the node. A list of plists, one per endpoint: `:endpoint', its id; `:device-types', their names; `:clusters', the names of its server clusters; `:items', the names of the items bound to it; `:definer', the text of the form that defines an item for it, or NIL for an endpoint that has no house kind."
  (let* ((label (getf plist :label))
         (suggested (loop :for endpoint :in (getf plist :endpoints)
                          :for kind := (some (lambda (type) (device-type-suggestion (car type)))
                                             (getf endpoint :device-types))
                          :when kind
                            :collect (cons (getf endpoint :id) kind)))
         (name (item-name-of-label label)))
    (loop :for endpoint :in (getf plist :endpoints)
          :for id := (getf endpoint :id)
          :for kind := (cdr (assoc id suggested))
          :collect (list :endpoint id
                         :device-types (mapcar (lambda (type) (device-type-name (car type)))
                                               (getf endpoint :device-types))
                         :clusters (mapcar #'%cluster-name (getf endpoint :server-clusters))
                         :items (%item-names (remove-if-not (lambda (binding)
                                                           (eql id (matter-binding-endpoint binding)))
                                                         bindings))
                         :definer (and kind
                                       (suggest-definer kind
                                                        ;; one device of several parts: each its own name
                                                        (if (rest suggested) (format nil "~a-~d" name id) name)
                                                        label label id))))))

(defun matter-suggestions (node)
  "What each endpoint of the commissioned `NODE' -- its label or node id -- is and how to make an item of it: a list of plists, one per endpoint, of `:endpoint', `:device-types' (names), `:clusters' (names), `:items' (the names of the items bound to it) and `:definer', the text of a house definer to paste into the configuration, or NIL for an endpoint that is none of the house's kinds."
  (let ((plist (%node-plist node)))
    (node-suggestions plist (%bindings-of (getf plist :label) (getf plist :node-id)))))

(defun %time-string (universal-time)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour minute)))

(defun matter-describe (node &optional (stream *standard-output*))
  "Prints to `STREAM' what the commissioned `NODE' -- its label or node id -- is: who made it, where it is reached, and each endpoint with its device types, its clusters, the items bound to it and, for an endpoint no item is bound to, the house definer to paste into the configuration. `matter-suggestions' is the same as data."
  (let* ((plist (%node-plist node))
         (suggestions (node-suggestions plist (%bindings-of (getf plist :label) (getf plist :node-id)))))
    (format stream "~&Matter node ~s, node id ~d~%" (getf plist :label) (getf plist :node-id))
    (format stream "  ~@[~a ~]~@[~a~]~@[, vendor #x~4,'0x~]~@[ product #x~4,'0x~]~@[, serial ~a~]~%"
            (getf plist :vendor-name) (getf plist :product-name)
            (getf plist :vendor-id) (getf plist :product-id) (getf plist :serial-number))
    (format stream "  at ~{~a~^, ~}~@[ port ~d~]~@[, commissioned ~a~]~%"
            (getf plist :addresses) (getf plist :port)
            (and (getf plist :commissioned-at) (%time-string (getf plist :commissioned-at))))
    (dolist (suggestion suggestions)
      (format stream "~%  endpoint ~d: ~{~a~^, ~}~%    clusters: ~{~a~^, ~}~%"
              (getf suggestion :endpoint) (getf suggestion :device-types) (getf suggestion :clusters))
      (cond ((getf suggestion :items)
             (format stream "    items: ~{~(~a~)~^, ~}~%" (getf suggestion :items)))
            ((getf suggestion :definer)
             (format stream "~%~a~%" (getf suggestion :definer)))))
    (values)))
