(defpackage :chipi.binding.matter-controller-integtest
  (:use :cl :fiveam :chipi.binding.matter :cl-matter)
  (:import-from #:sento.miscutils
                #:await-cond)
  (:import-from #:chipi.binding.matter-test
                #:%temp-storage
                #:%delete-storage
                #:%item-value)
  (:import-from #:chipi.binding.matter-controller-test
                #:*test-controller-args*
                #:%await)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.matter-controller-integtest)

(def-suite matter-controller-integtests
  :description "Matter binding: the controller commissions and controls a cl-matter lamp over loopback UDP"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-controller-integtests)

;; The controller runs on its actor, on an ephemeral IPv4 loopback port; the
;; lamp -- what an Eve plug is to the controller -- is cl-matter's own
;; device side, on a Matter actor of its own.  No hardware, no network
;; beyond loopback, no mDNS: the test knows the lamp's port.

(defparameter *lamp-passcode* 20202021)

(defstruct lamp
  "The lamp of a test: the ACTOR that polls it, its On/Off cluster LIGHT and Level Control cluster LEVEL, the cl-matter DEVICE and the UDP PORT it listens on."
  actor light level device port)

(defun %start-lamp ()
  "A commissionable On/Off Light on loopback, its window open for `*lamp-passcode*'."
  (let* ((transport (make-udp-transport :address "127.0.0.1"))
         (manager (make-exchange-manager :transport transport))
         (light (make-on-off-cluster))
         (level (make-level-control-cluster :current-level 127 :on-off light))
         (node (make-basic-node :vendor-name "ACME" :vendor-id #xFFF1 :product-name "Lamp" :product-id #x8000
                                :serial-number "L-0001"
                                :endpoints (list (make-endpoint 1 :device-types (list (cons +device-type-on-off-light+ 3))
                                                                  :clusters (list (make-identify-cluster) light level)))))
         (channel (make-secure-channel manager))
         (model (make-interaction-model manager :node node))
         (device (make-device :node node :channel channel :model model
                              :credentials (test-attestation-credentials)
                              :discriminator 3840 :passcode *lamp-passcode*
                              :salt binding-matter::*bridge-salt* :iterations 1000)))
    (make-lamp :actor (make-matter-actor "test-lamp" manager)
               :light light :level level :device device
               :port (nth-value 1 (transport-local-endpoint transport)))))

(defun %lamp-side (lamp thunk)
  "`THUNK' in the lamp actor's context."
  (matter-actor-call (lamp-actor lamp) thunk))

(defun %lamp-on-p (lamp)
  (%lamp-side lamp (lambda () (attribute-value (lamp-light lamp) 'on-off))))

(defun %switch-lamp (lamp on)
  "The lamp switched at the lamp, as its own button does."
  (%lamp-side lamp (lambda () (setf (attribute-value (lamp-light lamp) 'on-off) on))))

(defun %commission-lamp (lamp &optional (label "lamp"))
  "The lamp commissioned into the controller's fabric: the new node's plist, or the condition that says why not."
  (%await (binding-matter::%commission-at "127.0.0.1" (lamp-port lamp) :passcode *lamp-passcode* :label label)
          :timeout 90))

(def-fixture lamp-env ()
  "A chipi environment with the Matter controller on loopback, its storage in a fresh directory bound to `storage', and a lamp to commission bound to `lamp'."
  (let ((storage (%temp-storage))
        (lamp nil)
        (subscribe-delay binding-matter::*subscribe-delay*)
        (resubscribe-delay binding-matter::*resubscribe-delay*))
    (unwind-protect
         (progn
           ;; set, not bound: the controller's actor and the timer read them
           (setf binding-matter::*subscribe-delay* 0.1
                 binding-matter::*resubscribe-delay* 0.5)
           (hab:defconfig "chipi")
           (matter-init :storage storage :controller *test-controller-args*)
           (setf lamp (%start-lamp))
           (&body))
      (progn
        (when lamp
          (ignore-errors (matter-actor-stop (lamp-actor lamp))))
        ;; a controller the test started again has no shutdown hook
        (matter-shutdown)
        (hab:shutdown)
        (setf binding-matter::*subscribe-delay* subscribe-delay
              binding-matter::*resubscribe-delay* resubscribe-delay)
        (%delete-storage storage)))))

(defun %lamp-binding ()
  "A binding that reads and switches the On/Off cluster of the node \"lamp\"."
  (hab:binding :pull (lambda ()
                       ;; what the lamp says is not sent back to it
                       (values (matter-read "lamp" 1 :on-off :on-off) '(:push nil)))
               :transform (lambda (on) (if on 'item:true 'item:false))
               :push (lambda (value)
                       (matter-invoke "lamp" 1 :on-off (if (eq value 'item:true) :on :off)))
               :initial-delay 0.1
               :call-push-p t))

(test controller--commissions-a-lamp-and-an-item-reads-and-switches-it
  "The lamp joins the fabric and the registry knows what it is; a `defitem'
reads its state, switches it, and reads what was switched at the lamp."
  (with-fixture lamp-env ()
    (let ((node (%commission-lamp lamp)))
      ;; the node as the registry has it
      (is (listp node))
      (is (= 2 (getf node :node-id)))
      (is (string= "lamp" (getf node :label)))
      (is (string= "ACME" (getf node :vendor-name)))
      (is (= #xFFF1 (getf node :vendor-id)))
      (is (string= "Lamp" (getf node :product-name)))
      (is (= #x8000 (getf node :product-id)))
      (is (string= "L-0001" (getf node :serial-number)))
      (is (equal '("127.0.0.1") (getf node :addresses)))
      (is (= (lamp-port lamp) (getf node :port)))
      (is (integerp (getf node :commissioned-at)))
      (let ((endpoints (getf node :endpoints)))
        (is (equal '(0 1) (mapcar (lambda (e) (getf e :id)) endpoints)))
        (is (equal (list (cons +device-type-root-node+ 3)) (getf (first endpoints) :device-types)))
        (is (equal (list (cons +device-type-on-off-light+ 3)) (getf (second endpoints) :device-types)))
        (is-true (member +cluster-basic-information+ (getf (first endpoints) :server-clusters)))
        (is-true (member +cluster-on-off+ (getf (second endpoints) :server-clusters)))
        (is-true (member +cluster-identify+ (getf (second endpoints) :server-clusters)))
        (is-true (member +cluster-descriptor+ (getf (second endpoints) :server-clusters))))
      (is (equal (list "lamp") (mapcar (lambda (n) (getf n :label)) (matter-nodes))))
      ;; on file, both of them
      (is (equal '("lamp") (mapcar #'binding-matter::node-entry-label (binding-matter::read-registry storage))))
      (is (equal (getf node :endpoints)
                 (binding-matter::node-entry-endpoints (first (binding-matter::read-registry storage)))))
      (is (= 3 (getf (read-state-file (merge-pathnames "commissioner.state" storage)) :next-node-id)))
      ;; the lamp's side: one fabric, named as the controller names it
      (is (equal '("Chipi") (%lamp-side lamp (lambda () (mapcar #'fabric-label (device-fabrics (lamp-device lamp)))))))
      ;; a second device cannot have the label
      (is (search "is commissioned already" (princ-to-string (%commission-lamp lamp))))

      ;; the item reads the lamp
      (hab:defitem 'lamp "Lamp" 'boolean (%lamp-binding))
      (is-true (await-cond 5 (eq 'item:false (%item-value 'lamp))))
      ;; and switches it
      (hab:set-item-value 'lamp 'item:true)
      (is-true (await-cond 5 (eq t (%lamp-on-p lamp))))
      (hab:set-item-value 'lamp 'item:false)
      (is-true (await-cond 5 (eq nil (%lamp-on-p lamp))))
      ;; switched at the lamp, the next pull brings it in -- and does not push it back
      (%switch-lamp lamp t)
      (binding:exec-pull (first (item:bindings (hab:get-item 'lamp))))
      (is-true (await-cond 5 (eq 'item:true (%item-value 'lamp))))
      (is (eq t (%lamp-on-p lamp)))

      ;; the functions themselves: names or ids, a label or a node id
      (is (eq t (%await (matter-read "lamp" 1 :on-off :on-off))))
      (is (eq t (%await (matter-invoke 2 1 +cluster-on-off+ 2))))            ; toggle
      (is (eq nil (%await (matter-read 2 1 +cluster-on-off+ 0))))
      (is (string= "Lamp" (%await (matter-read "lamp" 0 :basic-information :product-name))))
      ;; what the lamp refuses fails the future
      (let ((future (matter-read "lamp" 1 :on-off #x7777)))
        (is (typep (%await future) 'error))
        (is-true (future:error-p future)))
      (let ((future (matter-invoke "lamp" 1 :on-off #x77)))
        (is (typep (%await future) 'error))
        (is-true (future:error-p future)))
      (let ((future (matter-read "lamp" 9 :on-off :on-off)))
        (is (typep (%await future) 'error))
        (is-true (future:error-p future))))))

(test controller--a-dropped-session-is-opened-again
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    (let* ((controller binding-matter::*controller*)
           (actor (binding-matter::controller-actor controller)))
      (flet ((session ()
               (matter-actor-call actor (lambda ()
                                          (binding-matter::connection-session
                                           (binding-matter::%connection controller 2))))))
        (let ((first (session)))
          (is-true first)
          (is (eq nil (%await (matter-read "lamp" 1 :on-off :on-off))))
          (is (eq first (session)))
          ;; the session goes: the connection lets go of it
          (matter-actor-call actor (lambda ()
                                     (binding-matter::%drop-session
                                      controller (binding-matter::%require-node controller "lamp") first)))
          (is (null (session)))
          ;; two requests at once wait for the same new session
          (let ((read (matter-read "lamp" 1 :on-off :on-off))
                (invoke (matter-invoke "lamp" 1 :on-off :on)))
            (is (eq nil (%await read :timeout 30)))
            (is (eq t (%await invoke :timeout 30))))
          (is-true (session))
          (is-false (eq first (session)))
          (is (eq t (%lamp-on-p lamp))))))))

(test controller--a-restarted-controller-still-has-its-nodes
  "The fabric and the registry come back from the storage directory: the lamp
is read without being commissioned again."
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    (%switch-lamp lamp t)
    (stop-controller)
    (is-false (matter-controller-running-p))
    (apply #'start-controller storage *test-controller-args*)
    (let ((nodes (matter-nodes)))
      (is (= 1 (length nodes)))
      (is (string= "lamp" (getf (first nodes) :label)))
      (is (string= "ACME" (getf (first nodes) :vendor-name)))
      (is (equal '(0 1) (mapcar (lambda (e) (getf e :id)) (getf (first nodes) :endpoints)))))
    (is (eq t (%await (matter-read "lamp" 1 :on-off :on-off) :timeout 30)))
    (is (eq t (%await (matter-invoke "lamp" 1 :on-off :off) :timeout 30)))
    (is (eq nil (%lamp-on-p lamp)))
    ;; still one fabric on the lamp
    (is (= 1 (%lamp-side lamp (lambda () (length (device-fabrics (lamp-device lamp)))))))))

;; -----------------------------
;; matter-binding: state by subscription, changes as commands
;; -----------------------------

(defun %lamp-level (lamp)
  (%lamp-side lamp (lambda () (attribute-value (lamp-level lamp) 'current-level))))

(defun %dim-lamp (lamp level)
  "The lamp dimmed at the lamp."
  (%lamp-side lamp (lambda () (setf (attribute-value (lamp-level lamp) 'current-level) level))))

(defun %subscription (&optional (node-id 2))
  "The controller's subscription to the node, or NIL."
  (let ((controller binding-matter::*controller*))
    (matter-actor-call (binding-matter::controller-actor controller)
                       (lambda ()
                         (binding-matter::connection-subscription
                          (binding-matter::%connection controller node-id))))))

(defun %barrier (lamp)
  "A round trip through the lamp side and the controller actor, twice.
The barrier for negative assertions (\"nothing was sent\"): what was queued
before it has been handled once it returns. No time-based waiting."
  (dotimes (i 2)
    (%lamp-side lamp (lambda () nil))
    (when (matter-controller-running-p)
      (%subscription))))

(defun %subscribed-paths (&optional (node-id 2))
  (let ((subscription (%subscription node-id)))
    (and subscription (length (subscription-paths subscription)))))

(test matter-binding--items-follow-the-lamp-and-switch-it
  "Items defined before their node is commissioned get its state with the
subscription, follow what happens at the lamp without sending it back, and
their own changes are commands."
  (with-fixture lamp-env ()
    (hab:defitem 'lamp "Lamp" 'boolean
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
    (hab:defitem 'lamp-level "Lamp level" 'integer
      ;; 64 of 254 is 25.2 %: a whole number for the item
      (matter-binding :node "lamp" :endpoint 1 :cluster :level-control :scale 100/254 :transform #'round
                      :command (:move-to-level :level (:scale 254/100) :transition-time 0)))
    (is (listp (%commission-lamp lamp)))
    ;; the first reports of the subscription
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (is-true (await-cond 10 (eql 50 (%item-value 'lamp-level))))
    (is (= 2 (%subscribed-paths)))
    ;; switched at the lamp: the item follows, and nothing goes back
    (%switch-lamp lamp t)
    (is-true (await-cond 5 (eq 'item:true (%item-value 'lamp))))
    (is (eq t (%lamp-on-p lamp)))
    (%dim-lamp lamp 254)
    (is-true (await-cond 5 (eql 100 (%item-value 'lamp-level))))
    ;; the items' changes are commands
    (hab:set-item-value 'lamp 'item:false)
    (is-true (await-cond 5 (eq nil (%lamp-on-p lamp))))
    (hab:set-item-value 'lamp 'item:true)
    (is-true (await-cond 5 (eq t (%lamp-on-p lamp))))
    (hab:set-item-value 'lamp-level 25)
    (is-true (await-cond 5 (eql 64 (%lamp-level lamp))))
    ;; which the lamp reports in its turn
    (%barrier lamp)
    (is (eql 25 (%item-value 'lamp-level)))
    (is (eq 'item:true (%item-value 'lamp)))

    ;; an item defined later joins the subscription: same attribute, same path
    (hab:defitem 'lamp-state "Lamp state" 'boolean
      (matter-binding :node 2 :endpoint 1 :cluster :on-off :command nil))
    (is-true (await-cond 5 (eq 'item:true (%item-value 'lamp-state))))
    (is (= 2 (%subscribed-paths)))
    (%switch-lamp lamp nil)
    (is-true (await-cond 5 (eq 'item:false (%item-value 'lamp-state))))
    (is-true (await-cond 5 (eq 'item:false (%item-value 'lamp))))
    ;; it is read only
    (hab:set-item-value 'lamp-state 'item:true)
    (%barrier lamp)
    (is (eq nil (%lamp-on-p lamp)))
    ;; a pull reads the attribute
    (%dim-lamp lamp 127)
    (binding:exec-pull (first (item:bindings (hab:get-item 'lamp-level))))
    (is-true (await-cond 5 (eql 50 (%item-value 'lamp-level))))
    (is (eql 127 (%lamp-level lamp)))))

(test matter-binding--a-binding-that-goes-leaves-the-subscription
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    (is (null (%subscription)))
    (let ((switch (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
          (level (matter-binding :node "lamp" :endpoint 1 :cluster :level-control)))
      (is-true (await-cond 10 (eql 2 (%subscribed-paths))))
      (binding:destroy level)
      (is-true (await-cond 10 (eql 1 (%subscribed-paths))))
      (binding:destroy switch)
      (is-true (await-cond 10 (null (%subscription)))))))

(test matter-binding--the-subscription-comes-back-with-a-new-session
  (with-fixture lamp-env ()
    (hab:defitem 'lamp "Lamp" 'boolean
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
    (is (listp (%commission-lamp lamp)))
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (let* ((controller binding-matter::*controller*)
           (first (%subscription)))
      (is-true first)
      ;; the session goes, by a request that got no answer, say
      (matter-actor-call (binding-matter::controller-actor controller)
                         (lambda ()
                           (binding-matter::%drop-session
                            controller (binding-matter::%require-node controller "lamp")
                            (subscription-session first))))
      (is (null (%subscription)))
      (is-false (subscription-active-p first))
      ;; what happens at the lamp meanwhile is in the new subscription's first report
      (%switch-lamp lamp t)
      (is-true (await-cond 30 (%subscription)))
      (is-false (eq first (%subscription)))
      (is-true (await-cond 5 (eq 'item:true (%item-value 'lamp))))
      (%switch-lamp lamp nil)
      (is-true (await-cond 5 (eq 'item:false (%item-value 'lamp)))))))

(test matter-binding--a-restarted-controller-subscribes-again
  (with-fixture lamp-env ()
    (hab:defitem 'lamp "Lamp" 'boolean
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
    (is (listp (%commission-lamp lamp)))
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (stop-controller)
    (%switch-lamp lamp t)
    ;; the binding outlives the controller: a change is logged, not signalled
    (hab:set-item-value 'lamp 'item:false)
    (%barrier lamp)
    (is (eq t (%lamp-on-p lamp)))
    (apply #'start-controller storage *test-controller-args*)
    (is-true (await-cond 30 (eq 'item:true (%item-value 'lamp))))
    (hab:set-item-value 'lamp 'item:false)
    (is-true (await-cond 5 (eq nil (%lamp-on-p lamp))))))
