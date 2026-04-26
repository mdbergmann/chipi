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
         (clrhash binding-knx::*ga-binding-registry*)
         (&body))
    (progn
      (setf binding-knx::*global-listener-registered-p* nil)
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
    (let ((cut (knx-binding :ga "1/2/3" :dpt dpt-type-str))
          (pushed-value nil))
      (answer (knxc:write-value _ _ push-value)
        (setf pushed-value push-value))
      (binding:exec-push cut push-value)
      (is (equalp pushed-value expected-value)))))

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
      (answer knx-client:establish-tunnel-connection
        (sento.future:with-fut-resolve
          (sento.future:fresolve t)))
      (answer knx-client:tunnel-connection-established-p t)
      (binding-knx::%do-reconnect)
      (is (= 1 (length (invocations 'ip-client:ip-connect))))
      (is (= 1 (length (invocations 'knx-client:start-async-receive))))
      (is (= 1 (length (invocations 'knx-client:establish-tunnel-connection)))))))

(test do-reconnect--retries-on-failure-then-gives-up
  "When every attempt errors, `%do-reconnect' walks the full backoff list."
  (with-fixture reconnect-state ()
    ;; three attempts then give up — keep the test fast
    (setf binding-knx::*reconnect-backoff-secs* '(0 0 0))
    (with-mocks ()
      (answer ip-client:ip-disconnect t)
      (answer ip-client:ip-connect (error "boom"))
      (binding-knx::%do-reconnect)
      (is (= 3 (length (invocations 'ip-client:ip-connect)))))))

(test on-disconnected--spawns-thread-and-flips-guard
  "Hook entry point sets the single-flight flag and returns a live thread."
  (with-fixture reconnect-state ()
    ;; never-resolving establish-tunnel-connection isn't called from this thread
    ;; once the guard is set; use a host that fails fast and a single 0-sec backoff
    (setf binding-knx::*reconnect-backoff-secs* '(0))
    (let ((thread (binding-knx::%on-disconnected :gateway-disconnect-request)))
      (is (typep thread 'bt:thread))
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
