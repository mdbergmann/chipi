(defpackage :chipi.binding.knx-test
  (:use :cl :fiveam :cl-mock :chipi.binding.knx)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.knx-test)

(def-suite knx-binding-tests
  :description "KNX binding tests"
  :in chipi.binding.knx-test-suite:test-suite)

(in-suite knx-binding-tests)

(def-fixture destroy-all ()
  (unwind-protect
       (progn
         (&body))
    (hab:shutdown)))

(def-fixture clean-knx-state ()
  (unwind-protect
       (progn
         (setf binding-knx::*global-listener-registered-p* nil)
         (setf binding-knx::*knx-bindings* '())
         (clrhash binding-knx::*ga-binding-registry*)
         (&body))
    (progn
      (setf binding-knx::*global-listener-registered-p* nil)
      (setf binding-knx::*knx-bindings* '())
      (clrhash binding-knx::*ga-binding-registry*))))

(test make-knx-binding
  "Tests creating a knx binding"
  (with-fixture clean-knx-state ()
    (with-mocks ()
      ;; global listener should register once
      (answer knx-client:add-tunnelling-request-listener t)
      (let ((cut (knx-binding
                  :ga "1/2/3"
                  :dpt "9.001"
                  :initial-delay nil)))
        (is-true cut)
        (is (address:knx-group-address-p (binding-knx::group-address-read cut)))
        (is (address:knx-group-address-p (binding-knx::group-address-write cut)))
        (is (eq 'dpt:dpt-9.001 (binding-knx::dpt-type cut)))
        (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener))))
        ;; verify binding is in registry
        (is (= 1 (length (gethash "1/2/3" binding-knx::*ga-binding-registry*))))))))

(test make-knx-binding--with-separate-read-write-gas
  "Tests creating a knx binding that specifies separate read and write GAs."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (let ((cut (knx-binding
                  :ga '(:read "1/2/3" :write "1/2/4")
                  :dpt "9.001"
                  :initial-delay nil)))
        (is-true cut)
        (is (address:knx-group-address-p (binding-knx::group-address-read cut)))
        (is (string= "1/2/3" (address:address-string-rep (binding-knx::group-address-read cut))))
        (is (address:knx-group-address-p (binding-knx::group-address-write cut)))
        (is (string= "1/2/4" (address:address-string-rep (binding-knx::group-address-write cut))))
        (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener))))))))

(defun %make-test-tun-req (ga mc apci &optional (dpt (dpt:make-dpt1 'dpt:dpt-1.001 :on)))
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 0
   :cemi (cemi:make-default-cemi
          :message-code mc
          :dest-address (address:make-group-address ga)
          :apci apci
          :dpt dpt)))

(test binding-listens-on-ga-changes
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (let ((item-set-called-with nil)
            (listener-fun-registered nil))
        ;; capture the global listener
        (answer (knx-client:add-tunnelling-request-listener fun)
          (progn
            (setf listener-fun-registered fun)
            t))
        (answer (item:set-value _ value :push nil)
          (progn
            (setf item-set-called-with value)
            t))

        (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
          (binding:bind-item cut :foo-item)

          (is (functionp listener-fun-registered))
          ;; call the global listener — it dispatches via hash-table
          (funcall listener-fun-registered
                   (%make-test-tun-req "1/2/3"
                                       cemi:+cemi-mc-l_data.ind+
                                       (cemi:make-apci-gv-write)))

          (is (equal 'item:true item-set-called-with)))))))

(test binding-listens-on-ga-changes--check-errors
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (let ((item-set-called-with nil)
            (listener-fun-registered nil))
        (answer (knx-client:add-tunnelling-request-listener fun)
          (progn
            (setf listener-fun-registered fun)
            t))

        (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
          (binding:bind-item cut :foo-item)

          (flet ((assert-no-processing (req)
                   (funcall listener-fun-registered req)
                   (is (eq nil item-set-called-with))))
            ;; wrong ga — silently skipped (no matching registry entry)
            (assert-no-processing
             (%make-test-tun-req "9/8/7"
                                 cemi:+cemi-mc-l_data.ind+
                                 (cemi:make-apci-gv-write)))
            ;; wrong mc — early return
            (assert-no-processing
             (%make-test-tun-req "1/2/3"
                                 cemi:+cemi-mc-l_data.con+
                                 (cemi:make-apci-gv-write)))
            ;; wrong apci — early return
            (assert-no-processing
             (%make-test-tun-req "1/2/3"
                                 cemi:+cemi-mc-l_data.ind+
                                 (cemi:make-apci-gv-read)))))))))

(test binding-listens-on-ga-changes--other-value-than-1.001
  "1.001 must be converted from :on/:off to 'item:true/'item:false"
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (let ((item-set-called-with nil)
            (listener-fun-registered nil))
        (answer (knx-client:add-tunnelling-request-listener fun)
          (progn
            (setf listener-fun-registered fun)
            t))

        (answer (item:set-value _ value :push nil)
          (progn
            (setf item-set-called-with value)
            t))

        (let ((cut (knx-binding :ga "1/2/3" :dpt "5.001" :initial-delay nil)))
          (binding:bind-item cut :foo-item)

          (is (functionp listener-fun-registered))
          (funcall listener-fun-registered
                   (%make-test-tun-req "1/2/3"
                                       cemi:+cemi-mc-l_data.ind+
                                       (cemi:make-apci-gv-write)
                                       (dpt:make-dpt5 'dpt:dpt-5.001 11)))

          (is (equal 11 item-set-called-with)))))))

(test binding-listens-on-ga-changes--dpt-1.009-converts-to-bool
  "Any DPT-1.x type (not just 1.001) must convert :on/:off to item:true/item:false"
  (with-fixture clean-knx-state ()
    (let ((dpt-1.009 (intern "DPT-1.009" :dpt)))
      ;; test the conversion functions directly
      (is (eq 'item:true (binding-knx::%convert-dpt-1.x-to-item-bool :on dpt-1.009)))
      (is (eq 'item:false (binding-knx::%convert-dpt-1.x-to-item-bool :off dpt-1.009)))
      (is (eq t (binding-knx::%convert-item-bool-to-dpt-1.x 'item:true dpt-1.009)))
      (is (eq nil (binding-knx::%convert-item-bool-to-dpt-1.x 'item:false dpt-1.009)))
      ;; non-boolean DPT passes through unchanged
      (is (eq :on (binding-knx::%convert-dpt-1.x-to-item-bool :on 'dpt:dpt-9.001)))
      (is (eq 42 (binding-knx::%convert-item-bool-to-dpt-1.x 42 'dpt:dpt-9.001))))))

(test binding-can-pull--initially
  "Pull is done after initializing the binding (and only once)."
  (with-fixture destroy-all ()
    (with-fixture clean-knx-state ()
      (with-mocks ()
        (let ((item-set-called-with nil))
          (answer knx-client:add-tunnelling-request-listener t)

          (answer (knxc:request-value _ 'dpt:dpt-1.001)
            (future:with-fut :on))

          (answer (item:set-value _ value :push nil)
            (progn
              (setf item-set-called-with value)
              t))

          (binding:bind-item
           (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay .1)
           :foo-item)

          (is-true (miscutils:await-cond 2.5
                     (eq item-set-called-with 'item:true))))))))

(defun binding-can-push-value (dpt-type-str push-value expected-value)
  ;; Reset state since this helper is called from multiple tests
  (setf binding-knx::*global-listener-registered-p* nil)
  (clrhash binding-knx::*ga-binding-registry*)
  (with-mocks ()
    (answer knx-client:add-tunnelling-request-listener t)
    (answer knxc:write-value t)
    (let ((cut (knx-binding :ga "1/2/3" :dpt dpt-type-str)))
      (binding:exec-push cut push-value)
      ;; invocation = (name ga dpt converted-value :retries .. :retry-backoff ..)
      (let* ((inv (first (invocations 'knxc:write-value)))
             (pushed-value (fourth inv)))
        (is (equalp pushed-value expected-value))))))

(test binding-can-push-on-item-value-change
  (binding-can-push-value "1.001" 'item:false nil))

(test binding-can-push-on-item-value-change--uint
  (binding-can-push-value "5.010" 123 123))

(test knx-init--initializes-knx
  (with-mocks ()
    (answer knxc:knx-conn-init t)
    (answer hab:add-to-shutdown t)
    (knx-init :gw-host "foo.bar" :gw-port 3671)
    (is (= 1 (length (invocations 'knxc:knx-conn-init))))
    (is (= 1 (length (invocations 'hab:add-to-shutdown))))
    ;; reconnect plumbing wired up by default
    (is (string= "foo.bar" binding-knx::*gw-host*))
    (is (= 3671 binding-knx::*gw-port*))
    (is (eq #'binding-knx::%on-disconnected
            knx-client:*on-disconnected*))
    ;; cleanup
    (setf knx-client:*on-disconnected* nil)
    (setf binding-knx::*gw-host* nil)
    (setf binding-knx::*gw-port* nil)))

(test knx-init--auto-reconnect-nil-leaves-hook-unset
  (with-mocks ()
    (answer knxc:knx-conn-init t)
    (answer hab:add-to-shutdown t)
    ;; ensure hook starts unset
    (setf knx-client:*on-disconnected* nil)
    (knx-init :gw-host "foo.bar" :gw-port 3671 :auto-reconnect nil)
    (is-false knx-client:*on-disconnected*)
    ;; gw host/port still captured for any later manual reconnect
    (is (string= "foo.bar" binding-knx::*gw-host*))
    ;; cleanup
    (setf binding-knx::*gw-host* nil)
    (setf binding-knx::*gw-port* nil)))

(test shutdown-knx--closes-knx
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      ;; create a binding so there's state to clear
      (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)
      (is-true binding-knx::*global-listener-registered-p*)
      (is (= 1 (hash-table-count binding-knx::*ga-binding-registry*)))
      ;; pretend init wired us up
      (setf knx-client:*on-disconnected* #'binding-knx::%on-disconnected)
      (setf binding-knx::*gw-host* "foo.bar")
      (setf binding-knx::*gw-port* 3671)

      (answer knxc:knx-conn-destroy t)
      (knx-shutdown)
      (is (= 1 (length (invocations 'knxc:knx-conn-destroy))))
      ;; registry should be cleared
      (is-false binding-knx::*global-listener-registered-p*)
      (is (= 0 (hash-table-count binding-knx::*ga-binding-registry*)))
      ;; reconnect plumbing torn down
      (is-false knx-client:*on-disconnected*)
      (is-false binding-knx::*gw-host*)
      (is-false binding-knx::*gw-port*))))

;; --- New tests for global listener dispatch ---

(test make-knx-binding--multiple-bindings-single-listener
  "Two bindings on different GAs should only register one global listener."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (let ((b1 (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil))
            (b2 (knx-binding :ga "4/5/6" :dpt "9.001" :initial-delay nil)))
        (is-true b1)
        (is-true b2)
        ;; only one call to add-tunnelling-request-listener
        (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener))))
        ;; both GAs in registry
        (is (= 1 (length (gethash "1/2/3" binding-knx::*ga-binding-registry*))))
        (is (= 1 (length (gethash "4/5/6" binding-knx::*ga-binding-registry*))))))))

(test make-knx-binding--same-ga-multiple-bindings
  "Two bindings on the same GA should both be in the registry."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (let ((b1 (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil))
            (b2 (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
        (is-true b1)
        (is-true b2)
        (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener))))
        ;; same GA has 2 entries
        (is (= 2 (length (gethash "1/2/3" binding-knx::*ga-binding-registry*))))))))

(test destroy-knx-binding--deregisters-from-registry
  "Destroying a knx-binding removes it from the GA registry."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
        (is (= 1 (length (gethash "1/2/3" binding-knx::*ga-binding-registry*))))
        (binding:destroy cut)
        ;; GA entry removed entirely
        (is (= 0 (hash-table-count binding-knx::*ga-binding-registry*)))))))

;; --- Reconnect plumbing ---

(def-fixture reconnect-state ()
  (let ((orig-progress binding-knx::*reconnect-in-progress-p*)
        (orig-host binding-knx::*gw-host*)
        (orig-port binding-knx::*gw-port*)
        (orig-backoff binding-knx::*reconnect-backoff-secs*))
    (unwind-protect
         (progn
           (setf binding-knx::*reconnect-in-progress-p* nil)
           (setf binding-knx::*gw-host* "foo.bar")
           (setf binding-knx::*gw-port* 3671)
           ;; one quick attempt — keeps tests under a second
           (setf binding-knx::*reconnect-backoff-secs* '(0))
           (&body))
      (setf binding-knx::*reconnect-in-progress-p* orig-progress)
      (setf binding-knx::*gw-host* orig-host)
      (setf binding-knx::*gw-port* orig-port)
      (setf binding-knx::*reconnect-backoff-secs* orig-backoff))))

(test do-reconnect--runs-disconnect-connect-receive-establish-on-success
  "Direct call to `%do-reconnect' executes the full sequence and stops once the tunnel is up.
Run synchronously so cl-mock answers (which are dynamic) are visible."
  (with-fixture reconnect-state ()
    (with-mocks ()
      (answer ip-client:ip-disconnect t)
      (answer ip-client:ip-connect t)
      (answer knx-client:start-async-receive t)
      (answer knx-client:establish-tunnel-connection t)
      (answer knx-client:tunnel-connection-established-p t)
      (binding-knx::%do-reconnect)
      (is (= 1 (length (invocations 'ip-client:ip-connect))))
      (is (= 1 (length (invocations 'knx-client:start-async-receive))))
      (is (= 1 (length (invocations 'knx-client:establish-tunnel-connection)))))))

(test do-reconnect--retries-persistently-until-gw-cleared
  "On persistent failure `%do-reconnect' keeps retrying past the end of the
backoff list (repeating the final delay) and only stops once the gateway config
is cleared — i.e. it never gives up on its own while still configured."
  (with-fixture reconnect-state ()
    ;; a short list so we prove retries continue *beyond* its length
    (setf binding-knx::*reconnect-backoff-secs* '(0 0))
    (with-mocks ()
      (answer ip-client:ip-disconnect t)
      ;; fail every attempt; after the 5th, simulate shutdown by clearing the
      ;; gateway host so the loop terminates on its next top-of-loop check
      (let ((calls 0))
        (answer ip-client:ip-connect
          (progn
            (incf calls)
            (when (>= calls 5)
              (setf binding-knx::*gw-host* nil))
            (error "boom"))))
      (binding-knx::%do-reconnect)
      ;; 5 attempts > backoff length (2) => it did not give up at list end
      (is (= 5 (length (invocations 'ip-client:ip-connect)))))))

(test on-disconnected--spawns-thread-and-flips-guard
  "Hook entry point sets the single-flight flag and returns a live thread."
  (with-fixture reconnect-state ()
    ;; never-resolving establish-tunnel-connection isn't called from this thread
    ;; once the guard is set; use a host that fails fast and a single 0-sec backoff
    (setf binding-knx::*reconnect-backoff-secs* '(0))
    (let ((thread (binding-knx::%on-disconnected :gateway-disconnect-request)))
      (is (typep thread 'bt:thread))
      ;; reconnect is now persistent, so stop it by clearing the gateway config
      ;; (the same signal `knx-shutdown' uses)
      (setf binding-knx::*gw-host* nil)
      (bt:join-thread thread)
      ;; flag released by unwind-protect after the thread exits
      (is-false binding-knx::*reconnect-in-progress-p*))))

(test schedule-reconnect--single-flight-blocks-second-call
  "When a reconnect is already in flight, a second event must not spawn a second thread."
  (with-fixture reconnect-state ()
    (setf binding-knx::*reconnect-in-progress-p* t)
    (let ((result (binding-knx::%schedule-reconnect :heartbeat-failure)))
      (is (null result)))
    ;; guard remains set so the in-flight reconnect still owns it
    (is-true binding-knx::*reconnect-in-progress-p*)))

;; --- write result is honoured (transport retry passed through) ---

(test push--passes-transport-retries-to-write-value
  "The binding push asks `knxc:write-value' to retry rather than discarding a
negative-con/timeout."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (answer knxc:write-value t)
      (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
        (binding:exec-push cut 'item:true)
        (let ((inv (first (invocations 'knxc:write-value))))
          ;; (name ga dpt value :retries N :retry-backoff B)
          (is (eql binding-knx::*write-retries* (getf (cddddr inv) :retries)))
          (is (eql binding-knx::*write-retry-backoff*
                   (getf (cddddr inv) :retry-backoff))))))))

;; --- read-back verify policy ---

(def-fixture sync-verify ()
  "Run the verify flow synchronously and deterministically in the calling thread."
  (let ((binding-knx::*verify-run-fun*
          (lambda (delay thunk) (declare (ignore delay)) (funcall thunk))))
    (&body)))

(defmacro with-verify-binding ((var &rest binding-args) &body body)
  "Create a verifying knx-binding (separate read/write GAs) bound to VAR."
  `(progn
     (setf binding-knx::*global-listener-registered-p* nil)
     (clrhash binding-knx::*ga-binding-registry*)
     (answer knx-client:add-tunnelling-request-listener t)
     (let ((,var (knx-binding :ga '(:read "1/2/3" :write "1/2/4")
                              :dpt "1.001"
                              :initial-delay nil
                              :verify t
                              ,@binding-args)))
       ,@body)))

(test verify--match-does-not-repush
  "When the read-back matches the pushed value, no re-push happens."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value t)
        (answer knxc:request-value (future:with-fut :on)) ; status = on
        (with-verify-binding (cut :verify-retries 3)
          (binding:exec-push cut 'item:true)
          ;; one initial write, one read-back, no re-push
          (is (= 1 (length (invocations 'knxc:write-value))))
          (is (= 1 (length (invocations 'knxc:request-value)))))))))

(test verify--mismatch-then-success-repushes-once
  "A mismatch triggers a re-push; once the status matches, verify stops."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value t)
        (let ((reads 0))
          (answer knxc:request-value
            (future:with-fut (if (zerop (prog1 reads (incf reads))) :off :on))))
        (with-verify-binding (cut :verify-retries 3)
          (binding:exec-push cut 'item:true)
          ;; initial write + exactly one re-push
          (is (= 2 (length (invocations 'knxc:write-value))))
          ;; two read-backs: mismatch, then match
          (is (= 2 (length (invocations 'knxc:request-value)))))))))

(test verify--exhaustion-calls-on-fail
  "Persistent mismatch exhausts the re-push budget and invokes verify-on-fail."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value t)
        (answer knxc:request-value (future:with-fut :off)) ; never matches :on
        (let ((failure nil))
          (with-verify-binding (cut
                                :verify-retries 2
                                :verify-on-fail (lambda (info) (setf failure info)))
            (binding:exec-push cut 'item:true)
            ;; initial write + 2 re-pushes
            (is (= 3 (length (invocations 'knxc:write-value))))
            ;; 3 read-backs (2 with budget left + final give-up)
            (is (= 3 (length (invocations 'knxc:request-value))))
            (is-true failure)
            (is (eq 'item:true (getf failure :desired)))
            (is (string= "1/2/4" (getf failure :write-ga)))))))))

(test verify--write-failure-calls-on-fail-and-skips-readback
  "A permanent write failure is not discarded: on-fail fires and no read-back is
attempted."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (let ((err (make-condition 'knx-client:knx-response-timeout-error
                                   :format-control "no ack")))
          (answer knxc:write-value err)       ; write reports failure
          (answer knxc:request-value (future:with-fut :on))
          (let ((failure nil))
            (with-verify-binding (cut
                                  :verify-on-fail (lambda (info) (setf failure info)))
              (binding:exec-push cut 'item:true)
              (is-true failure)
              (is (eq err (getf failure :actual)))
              ;; no read-back when the write itself failed
              (is (= 0 (length (invocations 'knxc:request-value)))))))))))

(test verify--values-equal-dpt-aware
  "Comparison is exact for booleans/ints and tolerant for floats; an explicit
comparator overrides."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      ;; float binding with tolerance
      (let ((fb (knx-binding :ga '(:read "1/2/3" :write "1/2/4")
                             :dpt "9.001" :initial-delay nil
                             :verify t :verify-tolerance 0.5)))
        (is-true (binding-knx::%verify-values-equal fb 21.0 21.3))
        (is-false (binding-knx::%verify-values-equal fb 21.0 22.0)))
      ;; integer binding, exact (tolerance 0)
      (let ((ib (knx-binding :ga '(:read "1/3/3" :write "1/3/4")
                             :dpt "5.010" :initial-delay nil
                             :verify t)))
        (is-true (binding-knx::%verify-values-equal ib 5 5))
        (is-false (binding-knx::%verify-values-equal ib 5 6)))
      ;; boolean binding, symbol equality
      (let ((bb (knx-binding :ga '(:read "1/4/3" :write "1/4/4")
                             :dpt "1.001" :initial-delay nil
                             :verify t)))
        (is-true (binding-knx::%verify-values-equal bb 'item:true 'item:true))
        (is-false (binding-knx::%verify-values-equal bb 'item:true 'item:false)))
      ;; explicit comparator override
      (let ((cb (knx-binding :ga '(:read "1/5/3" :write "1/5/4")
                             :dpt "5.010" :initial-delay nil
                             :verify t
                             :verify-compare (lambda (d a) (declare (ignore d a)) t))))
        (is-true (binding-knx::%verify-values-equal cb 1 999))))))

(test verify--newer-push-supersedes-pending-verify
  "Bumping the generation (a newer push) makes an older in-flight verify abort."
  (with-fixture clean-knx-state ()
    (with-mocks ()
      (answer knx-client:add-tunnelling-request-listener t)
      (answer knxc:request-value (future:with-fut :off))
      (answer knxc:write-value t)
      (with-verify-binding (cut :verify-retries 5)
        (let ((stale-gen (binding-knx::verify-generation cut)))
          ;; simulate a newer push having armed a fresh verify
          (incf (binding-knx::verify-generation cut))
          ;; the stale verify must not read back nor re-push
          (binding-knx::%run-verify cut 'item:true 5 stale-gen)
          (is (= 0 (length (invocations 'knxc:request-value))))
          (is (= 0 (length (invocations 'knxc:write-value)))))))))

(test verify--read-error-connected--retries-until-exhausted
  "While the tunnel is up, a read-back error is retried until the budget is
exhausted, then on-fail fires."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value t)
        (answer knxc:request-value
          (future:with-fut (make-condition 'simple-error
                                           :format-control "read boom")))
        (let ((failure nil)
              (knx-client::*channel-id* 1)) ; tunnel established
          (with-verify-binding (cut
                                :verify-retries 2
                                :verify-on-fail (lambda (info) (setf failure info)))
            (binding:exec-push cut 'item:true)
            ;; 2 retries with budget left + final give-up, no re-pushes
            (is (= 3 (length (invocations 'knxc:request-value))))
            (is (= 1 (length (invocations 'knxc:write-value))))
            (is-true failure)))))))

(test verify--read-error-disconnected--fails-fast
  "While the tunnel is down, verify does not burn its retry budget against the
dead connection: it fails fast, latches on-fail, and records the pending
re-push for the reconnect sweep."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value t)
        (answer knxc:request-value
          (future:with-fut (make-condition 'simple-error
                                           :format-control "no connection")))
        (let ((failure nil)
              (knx-client::*channel-id* nil)) ; tunnel down
          (with-verify-binding (cut
                                :verify-retries 3
                                :verify-on-fail (lambda (info) (setf failure info)))
            (binding:exec-push cut 'item:true)
            ;; exactly one read attempt, no retry burn
            (is (= 1 (length (invocations 'knxc:request-value))))
            (is-true failure)
            (is (equal '(item:true) (binding-knx::pending-repush cut)))))))))

;; --- pending re-push after reconnect ---

(test repush--write-failure-records-pending--new-push-supersedes
  "A permanent write failure records the desired value for re-push; a newer
push supersedes the record."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (let ((fail-p t)
              (err (make-condition 'knx-client:knx-response-timeout-error
                                   :format-control "no ack")))
          (answer knxc:write-value (if fail-p err t))
          ;; read-back status matches the second push (:off = item:false)
          (answer knxc:request-value (future:with-fut :off))
          (with-verify-binding (cut)
            (binding:exec-push cut 'item:true)
            (is (equal '(item:true) (binding-knx::pending-repush cut)))
            ;; a newer (successful) push supersedes the pending record
            (setf fail-p nil)
            (binding:exec-push cut 'item:false)
            (is (null (binding-knx::pending-repush cut)))))))))

(test repush--reconnect-sweep-repushes-and-clears
  "After reconnect, `%repush-pending-bindings' re-runs the full push+verify
cycle for the recorded value and clears the record."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (let ((fail-p t)
              (err (make-condition 'knx-client:knx-response-timeout-error
                                   :format-control "no ack")))
          (answer knxc:write-value (if fail-p err t))
          (answer knxc:request-value (future:with-fut :on))
          (with-verify-binding (cut)
            (binding:exec-push cut 'item:true)
            (is (equal '(item:true) (binding-knx::pending-repush cut)))
            ;; tunnel re-established, writes succeed again
            (setf fail-p nil)
            (binding-knx::%repush-pending-bindings)
            ;; failed initial push + successful re-push
            (is (= 2 (length (invocations 'knxc:write-value))))
            ;; re-push succeeded and verified: record cleared
            (is (null (binding-knx::pending-repush cut)))
            (is (= 1 (length (invocations 'knxc:request-value))))))))))

(test repush--reconnect-sweep-failure-rerecords
  "When the re-push fails again, the value is re-recorded for the next
reconnect."
  (with-fixture clean-knx-state ()
    (with-fixture sync-verify ()
      (with-mocks ()
        (answer knxc:write-value
          (make-condition 'knx-client:knx-response-timeout-error
                          :format-control "no ack"))
        (with-verify-binding (cut)
          (binding:exec-push cut 'item:true)
          (is (equal '(item:true) (binding-knx::pending-repush cut)))
          (binding-knx::%repush-pending-bindings)
          (is (= 2 (length (invocations 'knxc:write-value))))
          ;; still recorded for the next reconnect
          (is (equal '(item:true) (binding-knx::pending-repush cut))))))))

;; --- async path: real timer + dedicated dispatcher (no cross-thread mocks) ---

(test verify--dispatch-runs-on-dedicated-pool-not-timer
  "`%dispatch-verify' honours the delay via the timer and runs the work on the
dedicated :knx-verify dispatcher, which is registered on demand."
  (with-fixture destroy-all ()
    (let ((ran nil)
          (worker-thread nil)
          (this-thread (bt:current-thread)))
      (binding-knx::%dispatch-verify
       0.1
       (lambda ()
         (setf worker-thread (bt:current-thread))
         (setf ran t)))
      (is-true (miscutils:await-cond 2.5 ran))
      ;; the dedicated dispatcher was registered
      (is-true (getf (asys:dispatchers (isys:ensure-isys))
                     binding-knx::*verify-dispatcher-id*))
      ;; work ran on a worker thread, not the caller
      (is (not (eq this-thread worker-thread))))))
