(in-package :chipi.binding.matter)

;; The controller role: Chipi is the commissioner and administrator of a
;; Matter fabric of its own, and the nodes of that fabric -- lamps, plugs,
;; sensors -- are what items read and switch.
;;
;; One controller per process.  Its actor ("matter-controller", on a thread
;; of its own, matter-actors.lisp) owns the exchange manager and with it the
;; commissioner, the node registry (matter-registry.lisp) and one CASE
;; session per node.  Everything below that is not a `matter-...' function
;; runs in that actor's context: the `matter-...' functions send it there.
;;
;; A request to a node is answered by a datagram that a later poll turn
;; receives, so nothing here waits: `matter-read' and `matter-invoke' return
;; futures (`matter-actor-future'), which is what a binding's pull hands to
;; `binding:exec-pull', and what a push simply drops.
;;
;; State comes by subscription: the attributes that `matter-binding's
;; (matter-binding.lisp) bind on a node are one subscription on its session,
;; and what the node reports is set on the bound items.
;;
;; How a device becomes a node -- discovery, the commissioning job -- and
;; what is done to a node as a whole is in matter-commissioning.lisp.
;;
;; What this is not yet: a node is reached at the address it was
;; commissioned at (no re-resolve) and there is no reconnect backoff.  A
;; session whose request timed out is dropped, and the next request opens a
;; new one; a subscription that ended is asked for again every
;; `*resubscribe-delay*'.

(defvar *controller* nil
  "The running Matter controller of this process, a `controller', or NIL.")

(defparameter *commissioning-country-code* nil
  "The two-letter country code the controller sets as a node's regulatory location when it commissions it, or NIL to leave the node's own.")

(defparameter *subscribe-delay* 1
  "Seconds the controller collects the bindings of a node that come and go -- a configuration being loaded defines many -- before it asks the node for one subscription to all their attributes.")

(defparameter *resubscribe-delay* 30
  "Seconds after which the controller asks again for a subscription that ended or that it did not get: the node was off, or restarted.")

(defparameter *subscription-max-interval* 60
  "Seconds: the longest a node may go without a report to its subscription. A node that misses it by cl-matter's grace is taken for gone, its session dropped and the subscription asked for again.")

(defstruct (connection (:constructor %make-connection))
  "What the controller holds per node at run time: the CASE SESSION or NIL,
whether one is being opened (OPENING-P), and the WAITERS for it, conses of
a function of the session and a function of the condition that says why
there is none. And the SUBSCRIPTION to the attributes the node's bindings
bind, or NIL: whether one is being asked for (SUBSCRIBING-P) and is to be
asked for once more when that is through (SUBSCRIBE-AGAIN-P), and whether
a timer is set that will ask (SUBSCRIBE-SCHEDULED-P)."
  session
  opening-p
  (waiters '())
  subscription
  subscribing-p
  subscribe-again-p
  subscribe-scheduled-p)

(defstruct (controller (:constructor %make-controller))
  "Chipi's Matter controller: its ACTOR, the exchange MANAGER the actor
polls, the Secure CHANNEL and Interaction MODEL on it, the cl-matter
COMMISSIONER with Chipi's fabric, the STORAGE directory, the node entries
of the registry (NODES, oldest first), the CONNECTIONS by node id, how a
device's ATTESTATION is checked at commissioning, the FABRIC-LABEL the
nodes are told to list Chipi's fabric under, the network INTERFACE
discovery asks on (NIL: all), and JOB, the `commissioning-job' that runs
or ran last."
  actor manager channel model commissioner
  storage
  (nodes '())
  (connections (make-hash-table))
  (attestation :ignore)
  fabric-label
  interface
  job
  ;; the resolve functions of futures not settled yet; actor context only
  (pending (make-hash-table :test 'eq)))

(defmethod print-object ((controller controller) stream)
  (print-unreadable-object (controller stream :type t)
    (format stream "fabric ~16,'0x, ~d node~:p"
            (matter:fabric-fabric-id (matter:commissioner-fabric (controller-commissioner controller)))
            (length (controller-nodes controller)))))

;; -----------------------------
;; the state files
;; -----------------------------

(defun %commissioner-state-file (storage)
  (merge-pathnames "commissioner.state" storage))

(defun %write-commissioner-state (controller)
  "Writes the commissioner's state -- the fabric with its keys, the next node id, the resumption records -- to its file."
  (matter:write-state-file (matter:commissioner-state (controller-commissioner controller))
                           (%commissioner-state-file (controller-storage controller))))

(defun %write-registry (controller)
  (write-registry (controller-storage controller) (controller-nodes controller)))

;; -----------------------------
;; sessions
;; -----------------------------

(defun %connection (controller node-id)
  "The `connection' of node `NODE-ID', made when there is none yet."
  (or (gethash node-id (controller-connections controller))
      (setf (gethash node-id (controller-connections controller)) (%make-connection))))

(defun %call-waiter (waiter session)
  "Hands `SESSION' to `WAITER'; an error it signals goes to its own error function."
  (handler-case (funcall (car waiter) session)
    (error (c) (funcall (cdr waiter) c))))

(defun %session-opened (controller entry connection session)
  (log:info "Matter controller: session with ~a opened" entry)
  (setf (node-entry-last-seen entry) (get-universal-time))
  (let ((waiters (reverse (connection-waiters connection))))
    (setf (connection-session connection) session
          (connection-opening-p connection) nil
          (connection-waiters connection) '())
    (dolist (waiter waiters)
      (%call-waiter waiter session)))
  (ignore-errors (%write-commissioner-state controller)))

(defun %session-failed (entry connection condition)
  (log:warn "Matter controller: no session with ~a: ~a" entry condition)
  (let ((waiters (reverse (connection-waiters connection))))
    (setf (connection-opening-p connection) nil
          (connection-waiters connection) '())
    (dolist (waiter waiters)
      (funcall (cdr waiter) condition))))

(defun %open-session (controller entry connection)
  "Starts the CASE handshake with the node of `ENTRY' at the address it was last reached at; the waiters of `CONNECTION' hear how it ended."
  (setf (connection-opening-p connection) t)
  (handler-case
      (let ((host (first (node-entry-addresses entry)))
            (port (node-entry-port entry)))
        (unless (and host port)
          (error "No address is known for ~a" entry))
        (matter:start-case (controller-channel controller) host port
                           :fabric (matter:commissioner-fabric (controller-commissioner controller))
                           :peer-node-id (node-entry-node-id entry)
                           :on-complete (lambda (handshake)
                                          (if (matter:handshake-complete-p handshake)
                                              (%session-opened controller entry connection
                                                               (matter:handshake-session handshake))
                                              (%session-failed entry connection
                                                               (matter:handshake-error handshake))))))
    (error (c)
      (%session-failed entry connection c))))

(defun %with-session (controller entry on-session on-error)
  "Calls `ON-SESSION' with the CASE session to the node of `ENTRY': at once when there is one, else when the handshake this starts -- or one that is under way -- is through. `ON-ERROR' is called with the condition when no session can be had, or when `ON-SESSION' signals one."
  (let* ((connection (%connection controller (node-entry-node-id entry)))
         (session (connection-session connection))
         (waiter (cons on-session on-error)))
    (cond ((and session (matter:session-active-p session))
           (%call-waiter waiter session))
          (t
           (setf (connection-session connection) nil)
           (push waiter (connection-waiters connection))
           (unless (connection-opening-p connection)
             (%open-session controller entry connection))))))

(defun %drop-session (controller entry session)
  "Forgets `SESSION' to the node of `ENTRY', telling the node if it still listens: the next request opens a new one."
  (let ((connection (%connection controller (node-entry-node-id entry))))
    (when (eq session (connection-session connection))
      (setf (connection-session connection) nil)))
  (ignore-errors (matter:close-session (controller-channel controller) session)))

(defun %session-closed (controller session)
  "The channel's word that `SESSION' is gone, by whichever side: no connection keeps it, and a subscription on it is over -- it is asked for again on a new session."
  (loop :for node-id :being :the :hash-keys :of (controller-connections controller)
          :using (hash-value connection)
        :for subscription := (connection-subscription connection)
        :do (when (eq session (connection-session connection))
              (setf (connection-session connection) nil))
            (when (and subscription (eq session (matter:subscription-session subscription)))
              (matter:unsubscribe subscription)
              (setf (connection-subscription connection) nil)
              (when-let ((entry (find-node-entry (controller-nodes controller) node-id)))
                (log:warn "Matter controller: the session with ~a is gone and its subscription with it" entry)
                (%schedule-subscribe controller entry *resubscribe-delay*)))))

;; -----------------------------
;; requests
;; -----------------------------

(defun %require-node (controller designator)
  "The entry of the node `DESIGNATOR' names -- a label or a node id --, or an error."
  (or (find-node-entry (controller-nodes controller) designator)
      (error "No Matter node ~s is commissioned" designator)))

(defun %cluster-id (cluster)
  "The id of `CLUSTER', an id or the name of a cluster cl-matter defines (`:on-off')."
  (if (integerp cluster)
      cluster
      (matter:cluster-definition-id (matter:find-cluster-definition cluster))))

(defun %attribute-id (cluster attribute)
  "The id of `ATTRIBUTE' of `CLUSTER': an id, or the name the cluster's definition has for it."
  (if (integerp attribute)
      attribute
      (let* ((definition (matter:find-cluster-definition cluster))
             (found (matter:find-attribute-definition definition attribute)))
        (unless found
          (error "~a has no attribute ~s" definition attribute))
        (matter:attribute-definition-id found))))

(defun %command-id (cluster command)
  "The id of `COMMAND' of `CLUSTER': an id, or the name the cluster's definition has for it."
  (if (integerp command)
      command
      (let* ((definition (matter:find-cluster-definition cluster))
             (found (matter:find-command-definition definition command)))
        (unless found
          (error "~a has no command ~s" definition command))
        (matter:command-definition-id found))))

(defun %command-fields (cluster command fields)
  "`FIELDS' as the TLV structure of `COMMAND': NIL and a TLV element as they are, a plist through the definition of a command cl-matter knows."
  (if (or (null fields) (matter:tlv-element-p fields))
      fields
      (matter:command-fields cluster command fields)))

(defun %interaction-outcome (controller entry session interaction)
  "The result of the finished `INTERACTION' with the node of `ENTRY' over `SESSION', or the condition it failed with as second value. A request that got no answer costs the session: the node may have restarted."
  (cond ((matter:interaction-failed-p interaction)
         (let ((condition (matter:interaction-condition interaction)))
           (when (and (typep condition 'matter:interaction-error)
                      (eql matter:+im-status-timeout+ (matter:interaction-error-status condition)))
             (log:warn "Matter controller: ~a does not answer, session dropped" entry)
             (%drop-session controller entry session))
           (values nil condition)))
        (t
         (setf (node-entry-last-seen entry) (get-universal-time))
         (values (matter:interaction-result interaction) nil))))

(defun %report-value (report what)
  "The value of the attribute `REPORT': a Lisp value for an attribute cl-matter defines, else the TLV element. An error when the node sent a status in its place, or nothing."
  (unless report
    (error "~a: the node reported nothing" what))
  (multiple-value-bind (value kind) (matter:attribute-report-value report)
    (when (eq kind :status)
      (error "~a: the node answered ~a" what (matter:im-status-name (matter:im-status-status value))))
    value))

(defun %response-value (response what)
  "What the invoke `RESPONSE' carries: T for a plain success, the response command's fields as a plist (or its TLV element when cl-matter does not define it). An error when the node refused."
  (unless response
    (error "~a: the node answered nothing" what))
  (multiple-value-bind (value kind) (matter:invoke-response-value response)
    (cond ((not (eq kind :status)) value)
          ((matter:im-status-success-p value) t)
          (t (error "~a: the node answered ~a~@[, cluster status ~a~]" what
                    (matter:im-status-name (matter:im-status-status value))
                    (matter:im-status-cluster-status value))))))

(defun %start-request (controller entry start finish resolve)
  "One request to the node of `ENTRY': `START', called with the session and a completion function, starts a cl-matter interaction; `FINISH' makes the value from its result. `RESOLVE' gets that value, or the condition that says why there is none."
  (%with-session controller entry
                 (lambda (session)
                   (funcall start session
                            (lambda (interaction)
                              (multiple-value-bind (result condition)
                                  (%interaction-outcome controller entry session interaction)
                                (funcall resolve
                                         (or condition
                                             (handler-case (funcall finish result)
                                               (error (c) c))))))))
                 resolve))

(defun %controller-future (fun)
  "A future of what `FUN' resolves in the controller actor's context: `FUN' is called there with the controller and the resolve function of `matter-actor-future'. An error when the controller is not running."
  (let ((controller (or *controller* (error "The Matter controller is not running"))))
    (matter-actor-future (controller-actor controller)
                         (lambda (resolve)
                           ;; the resolve function is kept until it is called, so that
                           ;; stopping the controller can settle what is still open
                           (let ((pending (controller-pending controller))
                                 (wrapped nil))
                             (setf wrapped (lambda (value)
                                             (when (remhash wrapped pending)
                                               (funcall resolve value))))
                             (setf (gethash wrapped pending) t)
                             (handler-case (funcall fun controller wrapped)
                               (error (c) (funcall wrapped c))))))))

(defun %controller-call (thunk)
  "`THUNK' in the controller actor's context, its value returned; an error when the controller is not running."
  (let ((controller (or *controller* (error "The Matter controller is not running"))))
    (matter-actor-call (controller-actor controller) thunk)))

(defun matter-read (node endpoint cluster attribute)
  "Reads `ATTRIBUTE' of `CLUSTER' on `ENDPOINT' of the commissioned `NODE' and returns a future of its value.
`NODE' is the label the node was commissioned under, or its node id. `CLUSTER' and `ATTRIBUTE' are ids, or the names cl-matter's cluster definitions have for them, as keywords: `(matter-read \"lamp\" 1 :on-off :on-off)'.
The value is a Lisp value for an attribute cl-matter defines -- T and NIL for a boolean, `:null' for a null --, else the TLV element as it came.
The future fails (`future:frecover') with the condition that says why there is no value: no such node, no session to be had, no answer, a status in place of the value. A session to the node is opened when there is none.
As a binding's pull: `(binding :pull (lambda () (matter-read \"lamp\" 1 :on-off :on-off)) :transform ...)'."
  (%controller-future
   (lambda (controller resolve)
     (let* ((entry (%require-node controller node))
            (path (matter:attribute-path endpoint (%cluster-id cluster) (%attribute-id cluster attribute)))
            (what (format nil "read of ~a/~a/~a on ~s" endpoint cluster attribute node)))
       (%start-request controller entry
                       (lambda (session on-complete)
                         (matter:start-read (controller-model controller) session (list path)
                                            :on-complete on-complete))
                       (lambda (reports) (%report-value (first reports) what))
                       resolve)))))

(defun matter-invoke (node endpoint cluster command &optional fields)
  "Invokes `COMMAND' of `CLUSTER' on `ENDPOINT' of the commissioned `NODE' and returns a future of the answer: T for a plain success, the fields of a response command as a plist.
`NODE', `CLUSTER' and `COMMAND' as in `matter-read': `(matter-invoke \"lamp\" 1 :on-off :on)'. `FIELDS' is a plist of the command's fields for a command cl-matter defines -- `(matter-invoke \"lamp\" 1 :level-control :move-to-level '(:level 128 :transition-time 0 :options-mask 0 :options-override 0))' --, or a TLV element.
The future fails with the condition that says why not, as `matter-read's does. Who does not care for the answer drops the future: nothing waits on it, which is what a binding's push wants."
  (%controller-future
   (lambda (controller resolve)
     (let* ((entry (%require-node controller node))
            (path (matter:command-path endpoint (%cluster-id cluster) (%command-id cluster command)))
            (tlv (%command-fields cluster command fields))
            (what (format nil "~a/~a/~a on ~s" endpoint cluster command node)))
       (%start-request controller entry
                       (lambda (session on-complete)
                         (matter:start-invoke (controller-model controller) session path tlv
                                              :on-complete on-complete))
                       (lambda (responses) (%response-value (first responses) what))
                       resolve)))))

(defun matter-nodes ()
  "The nodes commissioned into Chipi's fabric, oldest first, each a plist: `:node-id', `:label', `:vendor-id', `:product-id', `:vendor-name', `:product-name', `:serial-number', `:unique-id', `:endpoints' -- plists of `:id', `:device-types' as (id . revision) and `:server-clusters' --, `:addresses', `:port', `:commissioned-at' and `:last-seen'."
  (%controller-call
   (lambda ()
     (mapcar #'node-entry-plist (controller-nodes *controller*)))))

;; -----------------------------
;; subscriptions
;; -----------------------------

;; One subscription per node, to every attribute a live `matter-binding'
;; binds on it.  Bindings come and go one by one while a configuration is
;; loaded, so a change only sets a timer (`*subscribe-delay*'), and what the
;; timer starts asks for the paths of the bindings there are by then.  A new
;; subscription replaces the old one, on the node's side too.

(declaim (ftype function node-bindings binding-path binding-report))

(defun %subscription-paths (bindings)
  "The attribute paths of `BINDINGS', each once, as lists (endpoint cluster-id attribute-id)."
  (remove-duplicates (mapcar #'binding-path bindings) :test #'equal))

(defun %dispatch-reports (controller entry reports)
  "What the node of `ENTRY' reported, set on the items of the bindings that bind it. A status in place of a value is logged; one report that cannot be handled does not cost the others."
  (setf (node-entry-last-seen entry) (get-universal-time))
  (let ((bindings (node-bindings (controller-nodes controller) entry)))
    (dolist (report reports)
      (handler-case
          (let* ((path (matter:attribute-report-path report))
                 (key (list (matter:attribute-path-endpoint path)
                            (matter:attribute-path-cluster path)
                            (matter:attribute-path-attribute path))))
            (multiple-value-bind (value kind) (matter:attribute-report-value report)
              (if (eq kind :status)
                  (log:warn "Matter controller: ~a reports ~a for ~a" entry
                            (matter:im-status-name (matter:im-status-status value)) path)
                  (dolist (binding bindings)
                    (when (equal key (binding-path binding))
                      (binding-report binding value))))))
        (error (c)
          (log:warn "Matter controller: a report of ~a was not handled: ~a" entry c))))))

(defun %schedule-subscribe (controller entry &optional (delay *subscribe-delay*))
  "Has the node of `ENTRY' asked for its subscription in `DELAY' seconds, unless a timer for that is set already."
  (let ((connection (%connection controller (node-entry-node-id entry))))
    (unless (connection-subscribe-scheduled-p connection)
      (setf (connection-subscribe-scheduled-p connection) t)
      (timer:schedule-once
       delay
       (lambda ()
         ;; the timer's thread: back into the actor, if it is still the one
         (when (eq controller *controller*)
           (ignore-errors
            (matter-actor-run (controller-actor controller)
                              (lambda ()
                                (setf (connection-subscribe-scheduled-p connection) nil)
                                (when (find entry (controller-nodes controller))
                                  (%subscribe controller entry)))))))))))

(defun %subscribe-failed (controller entry connection condition)
  (log:warn "Matter controller: no subscription to ~a: ~a; again in ~a s" entry condition *resubscribe-delay*)
  (setf (connection-subscribing-p connection) nil
        (connection-subscribe-again-p connection) nil)
  (%schedule-subscribe controller entry *resubscribe-delay*))

(defun %subscription-ended (controller entry connection subscription reason)
  "cl-matter's word that `SUBSCRIPTION' is over -- the node's reports stopped coming. The node is taken for restarted: its session goes, and the subscription is asked for again."
  (when (eq subscription (connection-subscription connection))
    (log:warn "Matter controller: the subscription to ~a ended (~a)" entry reason)
    (setf (connection-subscription connection) nil)
    (%drop-session controller entry (matter:subscription-session subscription))
    (%schedule-subscribe controller entry)))

(defun %subscribed (controller entry connection session interaction)
  "The end of a subscribe interaction: the new subscription takes the old one's place and its first reports are set on the items."
  (multiple-value-bind (subscription condition)
      (%interaction-outcome controller entry session interaction)
    (cond (condition
           (%subscribe-failed controller entry connection condition))
          (t
           (let ((old (connection-subscription connection))
                 (again-p (connection-subscribe-again-p connection)))
             (when old
               (matter:unsubscribe old))
             (setf (connection-subscription connection) subscription
                   (connection-subscribing-p connection) nil
                   (connection-subscribe-again-p connection) nil)
             (log:info "Matter controller: subscribed to ~d attribute~:p of ~a, a report every ~d s at least"
                       (length (matter:subscription-paths subscription)) entry
                       (matter:subscription-max-interval subscription))
             (%dispatch-reports controller entry (matter:subscription-primed-reports subscription))
             (when again-p
               (%subscribe controller entry)))))))

(defun %subscribe (controller entry)
  "Asks the node of `ENTRY' for one subscription to the attributes its bindings bind, in place of the one there is. A node without such bindings is left without subscription. While one is being asked for, this only notes that it is to be asked for again."
  (let ((connection (%connection controller (node-entry-node-id entry))))
    (if (connection-subscribing-p connection)
        (setf (connection-subscribe-again-p connection) t)
        (let ((paths (%subscription-paths (node-bindings (controller-nodes controller) entry))))
          (cond ((null paths)
                 (when-let ((old (connection-subscription connection)))
                   (matter:unsubscribe old)
                   (setf (connection-subscription connection) nil)))
                (t
                 (setf (connection-subscribing-p connection) t)
                 (%with-session
                  controller entry
                  (lambda (session)
                    (matter:start-subscribe
                     (controller-model controller) session
                     (loop :for (endpoint cluster attribute) :in paths
                           :collect (matter:attribute-path endpoint cluster attribute))
                     :min-interval 0 :max-interval *subscription-max-interval*
                     :on-report (lambda (subscription reports)
                                  (when (eq subscription (connection-subscription connection))
                                    (%dispatch-reports controller entry reports)))
                     :on-failure (lambda (subscription reason)
                                   (%subscription-ended controller entry connection subscription reason))
                     :on-complete (lambda (interaction)
                                    (%subscribed controller entry connection session interaction))))
                  (lambda (condition)
                    (%subscribe-failed controller entry connection condition)))))))))

(defun %bindings-changed (node)
  "A `matter-binding' on `NODE' -- a label or node id -- came or went: its node's subscription is to follow. Nothing while the controller is not running, which asks for all subscriptions when it starts; a node that is not commissioned is logged, and subscribed to when it is."
  (when-let ((controller *controller*))
    (matter-actor-run (controller-actor controller)
                      (lambda ()
                        (let ((entry (find-node-entry (controller-nodes controller) node)))
                          (if entry
                              (%schedule-subscribe controller entry)
                              (log:warn "Matter controller: a binding names the node ~s, which is not commissioned" node)))))))

;; -----------------------------
;; start and stop
;; -----------------------------

(defun %make-commissioner (channel model storage fabric-id vendor-id)
  "The commissioner of the fabric kept in `STORAGE', or of a new fabric `FABRIC-ID' when nothing is kept there yet; as second value whether it is a new one."
  (let ((state (matter:read-state-file (%commissioner-state-file storage))))
    (if state
        (let* ((commissioner (matter:make-commissioner channel model :state state))
               (kept (matter:fabric-fabric-id (matter:commissioner-fabric commissioner))))
          (unless (= kept fabric-id)
            (log:warn "Matter controller: the fabric kept in ~a is ~16,'0x, not the configured ~16,'0x; the kept one stays"
                      storage kept fabric-id))
          (values commissioner nil))
        (values (matter:make-commissioner channel model :fabric-id fabric-id :vendor-id vendor-id)
                t))))

(defun %settle-pending (controller)
  "Fails what is still waiting -- the waiters for a session, the futures of requests in flight -- with a `stopped' error: no answer comes once the manager is closed."
  (let ((condition (make-condition 'simple-error
                                   :format-control "The Matter controller was stopped"))
        (waiters '())
        (pending (loop :for resolve :being :the :hash-keys :of (controller-pending controller)
                       :collect resolve)))
    (loop :for connection :being :the :hash-values :of (controller-connections controller)
          :do (setf waiters (append waiters (reverse (connection-waiters connection)))
                    (connection-waiters connection) '()
                    (connection-opening-p connection) nil))
    (dolist (waiter waiters)
      (ignore-errors (funcall (cdr waiter) condition)))
    (dolist (resolve pending)
      (ignore-errors (funcall resolve condition)))))

(declaim (ftype function %job-stopped))

(defun %controller-stopping (controller)
  "In actor context, before the manager is closed: what waits is failed -- a commissioning that runs with it --, the nodes are told their sessions end, and the state files are written -- a write that fails is logged and does not keep the manager from closing."
  (ignore-errors (%job-stopped controller))
  (%settle-pending controller)
  (loop :for connection :being :the :hash-values :of (controller-connections controller)
        :for session := (connection-session connection)
        :when (and session (matter:session-active-p session))
          :do (ignore-errors (matter:close-session (controller-channel controller) session)))
  (clrhash (controller-connections controller))
  (handler-case (%write-commissioner-state controller)
    (error (c) (log:warn "Matter controller: the commissioner state was not written: ~a" c)))
  (handler-case (%write-registry controller)
    (error (c) (log:warn "Matter controller: the registry was not written: ~a" c))))

(defun start-controller (storage &key (fabric-id 1) (vendor-id #xFFF1) (attestation :ignore) (label "Chipi")
                                   interface (port 0) address (family :ipv6))
  "Starts Chipi's Matter controller; `matter-init' does this with its `:controller' keys.
`STORAGE' is the directory of `commissioner.state' (the fabric with its keys, the node ids handed out, resumption records) and `nodes' (the node registry). A fabric kept there is the one that is used: `FABRIC-ID' and `VENDOR-ID' (the test vendor, fine for a private fabric) make a new one only when there is none.
`ATTESTATION' is how a device is checked when it is commissioned: `:ignore', the default, accepts any device that holds the key of its device attestation certificate -- cl-matter's trust store has the SDK's test authorities only --, `:verify' checks the chain against the trust store.
`LABEL' is the name the nodes list Chipi's fabric under for their other administrators; NIL leaves it unnamed.
`INTERFACE' is the network interface discovery asks on (`\"en0\"'), NIL for all of them.
`PORT' is the UDP port, 0 for any; `FAMILY' and `ADDRESS' bind it elsewhere (tests: `:family :ipv4 :address \"127.0.0.1\"')."
  (when *controller*
    (error "The Matter controller is already running; `matter-shutdown' first"))
  (unless (member attestation '(:ignore :verify))
    (error ":attestation is :ignore or :verify, not: ~s" attestation))
  (let* ((transport (matter:make-udp-transport :family family :port port :address address))
         (manager (matter:make-exchange-manager :transport transport))
         (done nil))
    (unwind-protect
         (let* ((controller (%make-controller :manager manager :storage storage
                                              :attestation attestation :fabric-label label
                                              :interface interface
                                              :nodes (read-registry storage)))
                (channel (matter:make-secure-channel
                          manager
                          :on-session-closed (lambda (channel session)
                                               (declare (ignore channel))
                                               (%session-closed controller session))))
                (model (matter:make-interaction-model manager)))
           (multiple-value-bind (commissioner new-p)
               (%make-commissioner channel model storage fabric-id vendor-id)
             (setf (controller-channel controller) channel
                   (controller-model controller) model
                   (controller-commissioner controller) commissioner)
             (when new-p
               (%write-commissioner-state controller)))
           (setf (controller-actor controller)
                 (make-matter-actor "matter-controller" manager
                                    :on-stop (lambda () (%controller-stopping controller))))
           (setf *controller* controller
                 done t)
           ;; the nodes whose attributes are bound already are subscribed to
           (matter-actor-run (controller-actor controller)
                             (lambda ()
                               (dolist (entry (controller-nodes controller))
                                 (when (node-bindings (controller-nodes controller) entry)
                                   (%schedule-subscribe controller entry)))))
           (multiple-value-bind (host local-port) (matter:transport-local-endpoint transport)
             (log:info "Matter controller on [~a]:~d, fabric ~16,'0x, ~d node~:p"
                       host local-port
                       (matter:fabric-fabric-id (matter:commissioner-fabric (controller-commissioner controller)))
                       (length (controller-nodes controller))))
           controller)
      (unless done
        (ignore-errors (matter:exchange-manager-close manager))))))

(defun stop-controller ()
  "Stops the controller: its sessions are closed, its state files written, its socket closed. The nodes stay commissioned; the next start finds them in the registry."
  (when-let ((controller *controller*))
    (setf *controller* nil)
    (matter-actor-stop (controller-actor controller))
    (log:info "Matter controller stopped")
    t))

(defun matter-controller-running-p ()
  "Whether Chipi's Matter controller is running."
  (and *controller* t))
