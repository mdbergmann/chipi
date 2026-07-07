(defpackage :chipi.binding.knx
  (:use :cl :binding :knxc :knx-conn.address :knx-conn.dpt :knx-conn.knx-obj)
  (:nicknames :binding-knx)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:knx-binding
           #:knx-init
           #:knx-shutdown))

(in-package :chipi.binding.knx)

(defvar *ga-binding-registry* (make-hash-table :test #'equal)
  "Maps GA string -> list of (binding . dpt-type) entries.")

(defvar *knx-bindings* '()
  "All live knx-bindings. Needed for module-level events like the
post-reconnect re-push sweep (see `%repush-pending-bindings').")

(defvar *on-reconnected-fun* nil
  "Optional 0-arity callback set via `knx-init' `:on-reconnect'. Invoked from
the reconnect thread after the tunnel is re-established and pending re-pushes
have been dispatched. Errors are caught and logged.")

(defvar *global-listener-registered-p* nil
  "Whether the single global tunnelling listener has been registered.")

(defvar *gw-host* nil
  "KNXNet/IP gateway host, captured by `knx-init' for use by `%reconnect'.")

(defvar *gw-port* nil
  "KNXNet/IP gateway port, captured by `knx-init' for use by `%reconnect'.")

(defvar *reconnect-lock* (bt:make-lock "knx-binding-reconnect"))

(defvar *reconnect-in-progress-p* nil
  "Single-flight guard for `%reconnect' — protected by `*reconnect-lock*'.")

(defparameter *reconnect-backoff-secs* '(1 2 5 10 30 60)
  "Escalating delays (in seconds) between reconnect attempts. `%do-reconnect'
walks this list and then repeats the final delay indefinitely — it never gives
up on its own. Persisting is essential: once the tunnel is down the heartbeat
is stopped and `knx-client:%trigger-disconnected' can no longer fire (it is
gated on a live channel-id), so nothing else would ever re-arm the reconnect.
The loop only stops when the gateway config is cleared (i.e. on shutdown).")

;; Transport-level write retry — passed to `knxc:write-value' so a negative
;; L_Data.con / timeout is resent instead of discarded.
(defparameter *write-retries* 2)
(defparameter *write-retry-backoff* 0.5)

;; Read-back verify defaults.
(defparameter *default-verify-delay* 3)
(defparameter *default-verify-retries* 3)
(defparameter *verify-read-timeout-secs* 3)

;; Dedicated verify worker pool (never the shared timer thread / item actors).
(defvar *verify-dispatcher-id* :knx-verify)
(defparameter *verify-workers* 2)

;; Test seam: when bound to a function of (delay thunk), used instead of the
;; timer + dispatcher so tests can run the verify flow synchronously.
(defvar *verify-run-fun* nil)

(defclass knx-binding (binding)
  ((ga-read :initarg :ga-read
            :initform (error "Read group-address required!")
            :reader group-address-read)
   (ga-write :initarg :ga-write
             :initform (error "Write group-address required!")
             :reader group-address-write)
   (dpt :initarg :dpt
        :initform (error "DPT type required!")
        :reader dpt-type)
   ;; --- read-back verify policy (works for any DPT) ---
   (verify-p :initarg :verify :initform nil :reader verify-p)
   (verify-delay :initarg :verify-delay
                 :initform *default-verify-delay* :reader verify-delay)
   ;; number of re-push attempts on mismatch
   (verify-retries :initarg :verify-retries
                   :initform *default-verify-retries* :reader verify-retries)
   ;; allowed absolute diff for numeric DPTs; ignored for booleans
   (verify-tolerance :initarg :verify-tolerance
                     :initform 0 :reader verify-tolerance)
   ;; optional 2-arg (desired actual) predicate overriding the default compare
   (verify-compare :initarg :verify-compare
                   :initform nil :reader verify-compare)
   ;; optional 1-arg callback (plist) on exhaustion / permanent write failure
   (verify-on-fail :initarg :verify-on-fail
                   :initform nil :reader verify-on-fail)
   ;; bumped per push (only by the item actor) so a newer push supersedes an
   ;; in-flight verify; single-writer + monotonic, so no lock needed
   (verify-generation :initform 0 :accessor verify-generation)
   ;; NIL, or `(list desired-value)' recorded on permanent push/verify failure.
   ;; Consumed by `%repush-pending-bindings' once the tunnel is back so a value
   ;; the bus never (confirmably) received is re-sent instead of silently lost.
   ;; A new push always supersedes it (cleared at push start).
   (pending-repush :initform nil :accessor pending-repush)))

(defun %dpt-1.x-p (dpt-type)
  "Returns T if DPT-TYPE is any boolean DPT-1.xxx type."
  (let ((name (symbol-name dpt-type)))
    (and (>= (length name) 5)
         (string= "DPT-1." name :end2 6))))

(defun %convert-dpt-1.x-to-item-bool (value dpt-type)
  (cond
    ((%dpt-1.x-p dpt-type)
     (case value
       (:on 'item:true)
       (:off 'item:false)))
    (t value)))

(defun %convert-item-bool-to-dpt-1.x (value dpt-type)
  "Converts between `item:true'/`item:false' and `t'/`nil' as `knxc:write-value' wants it for DPT-1.x types."
  (cond
    ((%dpt-1.x-p dpt-type)
     (cond
       ((eq value 'item:true) t)
       ((eq value 'item:false) nil)))
    (t value)))

(defun %process-matching-bindings (req ga-string)
  "Looks up GA in registry and processes all matching bindings."
  (let ((entries (gethash ga-string *ga-binding-registry*)))
    (when entries
      (let ((cemi (tunnelling:tunnelling-request-cemi req)))
        (dolist (entry entries)
          (destructuring-bind (binding . dpt-type) entry
            (let* ((cemi-data (cemi:cemi-data cemi))
                   (dpt (etypecase cemi-data
                          (dpt cemi-data)
                          ((vector octet) (parse-to-dpt dpt-type cemi-data))))
                   (value (%convert-dpt-1.x-to-item-bool (dpt:dpt-value dpt) dpt-type))
                   (items (binding::bound-items binding)))
              (log:info "Indicated value: ~a for ga: ~a" value ga-string)
              (log:debug "Setting on items (~a)..." (length items))
              (dolist (item items)
                (log:debug "Setting on item: ~a" item)
                (item:set-value item value :push nil)))))))))

(defun %make-global-tunnelling-listener ()
  "Returns a single listener lambda for all KNX bindings."
  (lambda (req)
    (block listener
      (handler-case
          (progn
            (log:debug "KNX global tunnel listener received: ~a" req)
            (unless (eql (tunnelling:tunnelling-cemi-message-code req)
                         cemi:+cemi-mc-l_data.ind+)
              (return-from listener))
            (let ((cemi (tunnelling:tunnelling-request-cemi req)))
              (unless (typep (cemi:cemi-apci cemi) 'cemi:apci-gv-write)
                (return-from listener))
              (let ((ga-string (address:address-string-rep
                                (cemi:cemi-destination-addr cemi))))
                (%process-matching-bindings req ga-string))))
        (error (e)
          (log:warn "Unexpected error in global KNX listener: ~a" e))))))

(defun %register-binding-for-ga (binding ga-obj dpt-type)
  "Registers a binding for the given GA in the global registry."
  (let ((ga-string (address:address-string-rep ga-obj)))
    (push (cons binding dpt-type)
          (gethash ga-string *ga-binding-registry*))))

(defun %ensure-global-listener ()
  "Registers the global tunnelling listener once."
  (unless *global-listener-registered-p*
    (knx-client:add-tunnelling-request-listener
     (%make-global-tunnelling-listener))
    (setf *global-listener-registered-p* t)))

(defun %deregister-binding (binding)
  "Removes binding from the GA registry."
  (let* ((ga-obj (group-address-read binding))
         (ga-string (address:address-string-rep ga-obj)))
    (let ((entries (gethash ga-string *ga-binding-registry*)))
      (setf (gethash ga-string *ga-binding-registry*)
            (remove binding entries :key #'car)))
    (when (null (gethash ga-string *ga-binding-registry*))
      (remhash ga-string *ga-binding-registry*))))

(defun %reset-listener-registry ()
  "Clears the GA registry and resets the global listener flag."
  (clrhash *ga-binding-registry*)
  (setf *global-listener-registered-p* nil))

(defmethod binding:destroy :before ((binding knx-binding))
  (%deregister-binding binding)
  (setf *knx-bindings* (remove binding *knx-bindings*)))

(defun %make-binding-pull-fun (ga-obj dpt-type)
  (lambda ()
    (let ((fut
            (future:fmap
                (knxc:request-value (address:address-string-rep ga-obj)
                                    dpt-type)
                (result)
              (cond
                ((null result)
                 (error "Timed out!"))
                ((typep result 'error)
                 (error "Error: ~a" result))
                (t
                 (let ((value (%convert-dpt-1.x-to-item-bool result dpt-type)))
                   (log:info "Received value: ~a" value)
                   value))))))
      (values 
       fut
       '(:push nil)))))

;; -----------------------------
;; write to bus (transport retry honoured)
;; -----------------------------

(defun %push-value (binding value)
  "Write VALUE to the write GA, honouring (not discarding) the result. The
transport retry lives in `knxc:write-value'. Returns T or the failure condition."
  (let ((ga (address:address-string-rep (group-address-write binding)))
        (dpt-type (dpt-type binding)))
    (handler-case
        (knxc:write-value ga dpt-type
                          (%convert-item-bool-to-dpt-1.x value dpt-type)
                          :retries *write-retries*
                          :retry-backoff *write-retry-backoff*)
      (error (c)
        (log:error "KNX write to ~a signalled: ~a" ga c)
        c))))

;; -----------------------------
;; verify (read-back + re-push)
;; -----------------------------

(defun %ensure-verify-dispatcher ()
  "Register the dedicated verify dispatcher on the item actor-system unless it
already exists (registration is not idempotent)."
  (let ((isys (isys:ensure-isys)))
    (unless (getf (asys:dispatchers isys) *verify-dispatcher-id*)
      (asys:register-dispatcher
       isys
       (disp:make-dispatcher isys *verify-dispatcher-id*
                             :workers *verify-workers*
                             :strategy :round-robin)))))

(defun %dispatch-verify (delay thunk)
  "Run THUNK after DELAY seconds. The wheel-timer only honours the delay; its
callback hands the blocking read-back to the dedicated verify pool, so the timer
thread is never blocked. A bound `*verify-run-fun*' overrides this for tests."
  (if *verify-run-fun*
      (funcall *verify-run-fun* delay thunk)
      (timer:schedule-once
       delay
       (lambda ()
         (handler-case
             (progn
               (%ensure-verify-dispatcher)
               (tasks:with-context ((isys:ensure-isys) *verify-dispatcher-id*)
                 (tasks:task-start thunk)))
           (error (c)
             (log:error "KNX verify dispatch failed: ~a" c)))))))

(defun %verify-current-p (binding gen)
  "T if GEN is still the current generation (not superseded by a newer push)."
  (= gen (verify-generation binding)))

(defun %verify-values-equal (binding desired actual)
  "DPT-aware comparison of the DESIRED vs read-back ACTUAL value."
  (let ((compare (verify-compare binding))
        (tol (verify-tolerance binding)))
    (cond
      (compare (funcall compare desired actual))
      ((and (realp desired) (realp actual))
       (<= (abs (- desired actual)) (or tol 0)))
      (t (equal desired actual)))))

(defun %read-back (binding)
  "Read the actuator status from the read GA, in item-space. Signals on
timeout/error."
  (let* ((ga (address:address-string-rep (group-address-read binding)))
         (dpt-type (dpt-type binding))
         (result (future:fawait (knxc:request-value ga dpt-type)
                                :timeout *verify-read-timeout-secs*)))
    (when (or (null result) (typep result 'error))
      (error "KNX read-back of ~a failed: ~a" ga result))
    (%convert-dpt-1.x-to-item-bool result dpt-type)))

(defun %trigger-verify-fail (binding desired actual-or-error)
  ;; record the unconfirmed value so it is re-sent once the tunnel is back
  ;; (see `%repush-pending-bindings')
  (setf (pending-repush binding) (list desired))
  (let ((on-fail (verify-on-fail binding)))
    (when on-fail
      (handler-case
          (funcall on-fail (list :binding binding
                                 :write-ga (address:address-string-rep
                                            (group-address-write binding))
                                 :items (mapcar #'item:name
                                                (binding::bound-items binding))
                                 :desired desired
                                 :actual actual-or-error))
        (error (c)
          (log:error "KNX verify-on-fail handler error: ~a" c))))))

(defun %run-verify (binding desired-value remaining gen)
  "One verify attempt: read the status back, and on mismatch re-push (while
re-push budget REMAINING allows) or give up. Aborts if superseded by a newer
push (GEN)."
  (unless (%verify-current-p binding gen)
    (log:debug "KNX verify superseded for ~a, skipping."
               (group-address-write binding))
    (return-from %run-verify))
  (let ((write-ga (group-address-write binding)))
    (handler-case
        (let ((actual (%read-back binding)))
          (cond
            ((%verify-values-equal binding desired-value actual)
             (log:info "KNX verify ok for ~a: desired=~a actual=~a"
                       write-ga desired-value actual))
            ((<= remaining 0)
             (log:error "KNX verify FAILED for ~a: desired=~a actual=~a — giving up"
                        write-ga desired-value actual)
             (%trigger-verify-fail binding desired-value actual))
            (t
             (log:warn "KNX verify mismatch for ~a: desired=~a actual=~a — re-pushing (~a left)"
                       write-ga desired-value actual remaining)
             (%push-value binding desired-value)
             (when (%verify-current-p binding gen)
               (%schedule-verify binding desired-value (1- remaining) gen)))))
      (error (c)
        ;; read-back failed: retry the read (no re-push) until budget exhausted
        (cond
          ((not (knx-client:tunnel-connection-established-p))
           ;; retrying against a dead tunnel just burns the budget in seconds;
           ;; fail fast (latching any alarm) and let the post-reconnect
           ;; re-push sweep re-send + re-verify
           (log:warn "KNX verify read error for ~a: ~a — connection down, deferring to reconnect re-push"
                     write-ga c)
           (%trigger-verify-fail binding desired-value c))
          ((<= remaining 0)
           (log:error "KNX verify read error for ~a, giving up: ~a" write-ga c)
           (%trigger-verify-fail binding desired-value c))
          (t
           (log:warn "KNX verify read error for ~a: ~a — retrying (~a left)"
                     write-ga c remaining)
           (when (%verify-current-p binding gen)
             (%schedule-verify binding desired-value (1- remaining) gen))))))))

(defun %schedule-verify (binding desired-value remaining gen)
  (%dispatch-verify
   (verify-delay binding)
   (lambda () (%run-verify binding desired-value remaining gen))))

(defun %arm-verify (binding desired-value)
  "Bump the generation (superseding any pending verify) and schedule a fresh
verify. Runs on the item actor, the only writer of the generation."
  (let ((gen (incf (verify-generation binding))))
    (%schedule-verify binding desired-value (verify-retries binding) gen)))

(defun %make-binding-push-fun (binding)
  (lambda (value)
    (log:info "Writing to bus, value: ~a ga: ~a, dpt: ~a"
              value (group-address-write binding) (dpt-type binding))
    ;; a new push supersedes any recorded re-push; on failure below,
    ;; %trigger-verify-fail records this (latest) value again
    (setf (pending-repush binding) nil)
    (let ((result (%push-value binding value)))
      (cond
        ((eq result t)
         (when (verify-p binding)
           (%arm-verify binding value)))
        (t
         (log:error "KNX write to ~a failed permanently: ~a"
                    (group-address-write binding) result)
         (%trigger-verify-fail binding value result)))
      result)))

(defun %make-knx-binding (&rest other-args
                          &key ga dpt
                            verify verify-delay verify-retries
                            verify-tolerance verify-compare verify-on-fail
                          &allow-other-keys)
  (let* ((ga-read-obj (if (stringp ga)
                          (make-group-address ga)
                          (make-group-address (getf ga :read))))
         (ga-write-obj (if (stringp ga)
                           (make-group-address ga)
                           (make-group-address (getf ga :write))))
         (dpt-type (value-type-string-to-symbol dpt))
         (rest-args (remove-from-plist other-args
                                       :ga :dpt
                                       :verify :verify-delay :verify-retries
                                       :verify-tolerance :verify-compare
                                       :verify-on-fail))
         (binding (apply #'make-instance 'knx-binding
                         :ga-read ga-read-obj
                         :ga-write ga-write-obj
                         :dpt dpt-type
                         :initial-delay (getf other-args :initial-delay 2)
                         :pull-fun (%make-binding-pull-fun ga-read-obj dpt-type)
                         :verify verify
                         :verify-delay (or verify-delay *default-verify-delay*)
                         :verify-retries (or verify-retries *default-verify-retries*)
                         :verify-tolerance (or verify-tolerance 0)
                         :verify-compare verify-compare
                         :verify-on-fail verify-on-fail
                         rest-args)))
    (assert (and ga-read-obj ga-write-obj) nil "Unable to make group-address objects!")
    (assert dpt-type nil "Unable to parse dpt-type!")
    ;; push-fun closes over the fully-constructed binding (for verify policy)
    (setf (slot-value binding 'binding::push-fun)
          (%make-binding-push-fun binding))
    (%ensure-global-listener)
    (%register-binding-for-ga binding ga-read-obj dpt-type)
    (push binding *knx-bindings*)
    binding))

;; -----------------------------
;; Public API
;; -----------------------------

(defmacro knx-binding (&rest other-args &key ga dpt &allow-other-keys)
  "Creates a knx-binding.

Creating the binding expects an initialized knx-conn environment.

Relevant arguments:
- `ga': group-address in
string representation like \"1/2/3\" or a
list like `'(:read \"1/2/3\" :write \"1/2/4\")`
Note, that `:read' is also used to listen (on the bus). A read request only happens when `:initial-delay' (is default with 2 seconds. Can be disabled with `nil') or `:delay' is specified. Otherwise the item value will only be set when changes to the GA happened on the bus.
That means with `:initial-delay' or `:delay' the GA specified must have the 'read' flag set.

- `dpt': dpt-type string, i.e. '1.001'

`other-args' will be forwarded to the `base-binding' constructor. So things like `:call-push-p' and `:delay' also work here. However, be careful with `:push' and `:pull'. Using them redefine the behavior of the knx-binding.
In particular, `:call-push-p' allows to forward item value changes which come from other places than the KNX bus to push to the bus.

Read-back verify policy (optional, works for any DPT):
- `:verify' — when non-nil, after a push the actuator status is read back from
the read GA and, on mismatch, the value is re-pushed.
- `:verify-delay' — seconds to wait after a push before reading back (default
`*default-verify-delay*').
- `:verify-retries' — number of re-push attempts on mismatch (default
`*default-verify-retries*').
- `:verify-tolerance' — allowed absolute difference for numeric DPTs (e.g.
floats); ignored for booleans.
- `:verify-compare' — optional 2-arg predicate (desired actual) overriding the
default DPT-aware comparison.
- `:verify-on-fail' — optional 1-arg callback invoked with a plist
(`:binding' `:write-ga' `:items' `:desired' `:actual') when verification is
exhausted or a write fails permanently. `:items' is the list of bound item
names.

On permanent failure the desired value is additionally recorded on the binding
and automatically re-pushed (full push+verify cycle) once the tunnel connection
is re-established, so a value the bus never confirmably received is not
silently lost. A newer push supersedes the recorded value.

Note: the read GA must have its read flag set so the status can be requested.
Verify never blocks the shared timer thread or the item actors — the delay is
honoured by the timer, and the read-back runs on a dedicated worker pool."
  `(progn
     (assert (or (stringp ,ga)
                 (listp ,ga))
             nil "Parameter ga must be string or a plist with :read and :write keys!")
     (assert (typep ,dpt 'string) nil "Parameter dpt must be string!")
     (log:info "Make knx binding...")
     (%make-knx-binding :ga ,ga :dpt ,dpt ,@other-args)))

(defun %repush-pending-bindings ()
  "Re-run the full push+verify cycle for every binding whose last push/verify
failed permanently (recorded in `pending-repush'). Called from the reconnect
thread after the tunnel is re-established. `exec-push' runs the binding's
push-fun, which clears the pending record first and — on renewed failure —
re-records it for the next reconnect.

Note: calling push-fun from the reconnect thread technically breaks the
item-actor single-writer discipline on `verify-generation'; the worst case of
that race is a duplicated (immediately superseded) read-back, which is benign."
  (dolist (binding *knx-bindings*)
    (let ((pending (pending-repush binding)))
      (when pending
        (let ((value (car pending)))
          (log:info "KNX re-pushing pending value ~a to ~a after reconnect."
                    value (group-address-write binding))
          (handler-case
              (binding:exec-push binding value)
            (error (c)
              (log:warn "KNX re-push to ~a failed: ~a"
                        (group-address-write binding) c))))))))

(defun %repull-read-bindings ()
  "Re-issue the read for every binding with an active read (`initial-delay' or
`delay'). Called from the reconnect thread once the tunnel is back. Recovers the
one-shot `initial-delay' read when it fired before the tunnel was up, and
refreshes values that may have changed while down. Listen-only bindings are
skipped."
  (dolist (binding *knx-bindings*)
    (when (or (slot-value binding 'binding::initial-delay)
              (slot-value binding 'binding::delay))
      (handler-case
          (binding:exec-pull binding)
        (error (c)
          (log:warn "KNX re-pull of ~a failed: ~a"
                    (group-address-read binding) c))))))

(defun %do-reconnect ()
  "Tear down the dead UDP socket, then keep re-establishing the tunnel using the
escalating `*reconnect-backoff-secs*' delays, repeating the final delay
indefinitely until one attempt succeeds. Returns when the tunnel is back up, or
when the gateway config is cleared (shutdown), at which point it stops.

It must not give up while the gateway is still configured: once disconnected the
heartbeat is gone and no further disconnect event can be raised to re-arm it, so
giving up would leave the tunnel dead until the next process restart."
  (let ((backoff *reconnect-backoff-secs*))
    (loop
      (unless *gw-host*
        (log:info "KNX reconnect: gateway config cleared, stopping reconnect loop.")
        (return))
      (let ((delay (or (car backoff)
                       (car (last *reconnect-backoff-secs*))
                       60))
            (established nil))
        ;; advance through the list, then stick on its last (longest) delay
        (when (cdr backoff)
          (setf backoff (cdr backoff)))
        (unwind-protect
            (handler-case
                (progn
                  (log:info "Attempting KNX reconnect to ~a:~a..." *gw-host* *gw-port*)
                  (ignore-errors (ip-client:ip-disconnect))
                  (ip-client:ip-connect *gw-host* *gw-port*)
                  (knx-client:start-async-receive)
                  (knx-client:establish-tunnel-connection)
                  (setf established (knx-client:tunnel-connection-established-p)))
              (error (c)
                (log:warn "KNX reconnect attempt failed: ~a (retry in ~as)" c delay)))
          ;; A failed attempt must not hold its UDP socket (and receive loop)
          ;; open across the backoff sleep — that leaves the gateway endpoint
          ;; idle for up to a minute. Tear it down now; a successful attempt
          ;; keeps the socket, as that is the live tunnel.
          (unless established
            (ignore-errors (ip-client:ip-disconnect))))
        (when established
          (log:info "KNX tunnel reconnected.")
          ;; refresh reads first, then re-send writes so an intended
          ;; write wins over freshly-read bus state on a shared GA
          (%repull-read-bindings)
          (%repush-pending-bindings)
          (when *on-reconnected-fun*
            (handler-case (funcall *on-reconnected-fun*)
              (error (c)
                (log:warn "Error in :on-reconnect callback: ~a" c))))
          (return))
        (sleep delay)))))

(defun %schedule-reconnect (reason)
  "Spawn a worker thread that runs `%do-reconnect'. The single-flight guard
ensures only one reconnect thread runs at a time even if multiple disconnect
events fire in quick succession."
  (bt:with-lock-held (*reconnect-lock*)
    (when *reconnect-in-progress-p*
      (log:info "KNX reconnect already in progress, ignoring ~a." reason)
      (return-from %schedule-reconnect))
    (setf *reconnect-in-progress-p* t))
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (%do-reconnect)
       (bt:with-lock-held (*reconnect-lock*)
         (setf *reconnect-in-progress-p* nil))))
   :name (format nil "knx-binding-reconnect (~a)" reason)))

(defun %on-disconnected (reason)
  "Hook installed into `knx-client:*on-disconnected*' by `knx-init'."
  (log:warn "KNX connection lost (~a). Scheduling reconnect." reason)
  (%schedule-reconnect reason))

(defun knx-init (&key gw-host (gw-port 3671) (auto-reconnect t) (verify-workers 2)
                   on-reconnect)
  "Config and initialize KNX binding.
This should be as part of `hab:defconfig'.
A shutdown hook is added via `hab:add-to-shutdown' which calls `knx-shutdown'.

`gw-host': host or UP address to a KNXNet/IP router/gateway.
`gw-port': port to the gateway, default 3671.
`auto-reconnect': when true (default), register a hook that automatically
re-establishes the tunnel on heartbeat failure or gateway-initiated disconnect.
Set to NIL to opt out — the connection then stays down until `knx-init` is
called again or a manual reconnect is triggered.
`verify-workers': number of workers in the dedicated read-back verify pool
(default 2). The pool is created lazily the first time a verifying binding pushes.
`on-reconnect': optional 0-arity function invoked (from the reconnect thread)
after the tunnel has been re-established and pending re-pushes were dispatched.
Only effective together with `auto-reconnect'.

With `auto-reconnect', a failure to establish the tunnel during init (e.g. the
gateway refuses with E_NO_MORE_UNIQUE_CONNECTIONS because it still holds stale
tunnels from an unclean shutdown, or it doesn't respond at all) does NOT
signal: the reconnect loop takes over with its escalating backoff and the rest
of the application can boot. Failures before the transport/actor structures
exist (UDP socket, actor system) still signal — there is nothing for the
reconnect loop to drive then."
  (hab:add-to-shutdown #'knx-shutdown)
  (setf *gw-host* gw-host)
  (setf *gw-port* gw-port)
  (setf *verify-workers* verify-workers)
  (setf *on-reconnected-fun* on-reconnect)
  (when auto-reconnect
    (setf knx-client:*on-disconnected* #'%on-disconnected))
  (handler-case
      (knxc:knx-conn-init gw-host
                          :port gw-port)
    (error (c)
      ;; a live async-handler means the failure was at the tunnel-establish
      ;; stage; ip/asys structures exist, so %do-reconnect can drive recovery
      (cond
        ((and auto-reconnect knx-client:*async-handler*)
         (log:warn "KNX init: could not establish tunnel (~a). Scheduling reconnect." c)
         (%schedule-reconnect :init-failure))
        (t
         (error c))))))

(defun knx-shutdown ()
  "Shutdown KNX binding and release/clean all resources.
Be aware that the global shutdown function (`hab:shutdown') will also call this so this usually doesn't need to be called manually except in test setups."
  (setf knx-client:*on-disconnected* nil)
  (setf *gw-host* nil)
  (setf *gw-port* nil)
  (setf *on-reconnected-fun* nil)
  (setf *knx-bindings* '())
  (%reset-listener-registry)
  (knxc:knx-conn-destroy))
