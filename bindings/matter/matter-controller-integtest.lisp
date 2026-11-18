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

;; -----------------------------
;; from pairing code to item: the job, labels, the window, leaving
;; -----------------------------

(defparameter *lamp-code* "3497-011-2332"
  "The lamp's manual pairing code: discriminator 3840, passcode `*lamp-passcode*'.")

(defun %lamp-advert (lamp &rest more)
  "What the lamp would advertise, were there mDNS: an entry of `matter-discover's `:commissionable'."
  (list* :name "Test Lamp" :device-name "Test Lamp" :discriminator 3840 :commissioning-mode 1
         :addresses '("127.0.0.1") :port (lamp-port lamp) more))

(defun %lamp-fabrics (lamp)
  (%lamp-side lamp (lambda () (length (device-fabrics (lamp-device lamp))))))

(defun %lamp-window-open-p (lamp)
  (%lamp-side lamp (lambda () (commissioning-window-open-p (lamp-device lamp)))))

(defun %described (node)
  (with-output-to-string (out) (matter-describe node out)))

(test matter-start-commissioning--from-pairing-code-to-item
  "The search finds the lamp by the code's discriminator, the job goes through
its states, and what `matter-describe' prints defines the item that follows
and switches the lamp."
  (with-fixture lamp-env ()
    (let ((states '())
          (results '()))
      (cl-mock:with-mocks ()
        (cl-mock:answer binding-matter::%browse-commissionable (list (%lamp-advert lamp)))
        (let ((job (matter-start-commissioning *lamp-code* :label "lamp"
                                                           :on-step (lambda (job)
                                                                      (push (commissioning-job-state job) states))
                                                           :on-complete (lambda (result) (push result results)))))
          (is-true (await-cond 90 (not (commissioning-job-running-p job))))
          (is (eq :done (commissioning-job-state job)))
          (is (null (commissioning-job-error job)))
          (is (equal '(:pairing :attesting :credentials :completing :reading :labeling :done)
                     (reverse states)))
          (is (equal (list (commissioning-job-node job)) results))
          (is (string= "Test Lamp" (getf (commissioning-job-device job) :name)))
          (is (eq job (matter-commissioning-job)))))
      (let ((node (first results)))
        (is (string= "lamp" (getf node :label)))
        (is (string= "ACME" (getf node :vendor-name)))
        (is (equal '(0 1) (mapcar (lambda (e) (getf e :id)) (getf node :endpoints)))))
      (is (equal '("lamp") (mapcar (lambda (n) (getf n :label)) (matter-nodes))))
      ;; the lamp carries the label for its other administrators
      (is (string= "lamp" (matter-await (matter-read "lamp" 0 :basic-information :node-label))))
      (is (equal '("Chipi") (%lamp-side lamp (lambda () (mapcar #'fabric-label (device-fabrics (lamp-device lamp)))))))

      ;; what it is, and the definer for it
      (let ((suggestions (matter-suggestions "lamp"))
            (text (%described "lamp")))
        (is (equal '(("Root Node") ("On/Off Light")) (mapcar (lambda (s) (getf s :device-types)) suggestions)))
        (is (search "Matter node \"lamp\", node id 2" text))
        (is (search "ACME Lamp, vendor #xFFF1 product #x8000, serial L-0001" text))
        (is (search "endpoint 1: On/Off Light" text))
        (is (search "on-off" text))
        (is (search (getf (second suggestions) :definer) text))
        (is (null (matter-items-of "lamp")))
        ;; pasted into the configuration
        (eval (let ((*package* (find-package :chipi.binding.matter-test-config)))
                (read-from-string (getf (second suggestions) :definer)))))
      (let ((item (intern "LAMP" :chipi.binding.matter-test-config)))
        (is-true (await-cond 10 (eq 'item:false (%item-value item))))
        (hab:set-item-value item 'item:true)
        (is-true (await-cond 5 (eq t (%lamp-on-p lamp))))
        (%switch-lamp lamp nil)
        (is-true (await-cond 5 (eq 'item:false (%item-value item))))
        ;; the node knows its items, the item its node
        (is (equal '("LAMP") (matter-items-of "lamp")))
        (is (equal '("LAMP") (matter-items-of 2)))
        (is (equal '("lamp" 2) (multiple-value-list (matter-node-of item))))
        (is (equal '("LAMP") (getf (second (matter-suggestions "lamp")) :items)))
        ;; and an endpoint that has an item is not suggested one
        (let ((text (%described "lamp")))
          (is (search "items: lamp" text))
          (is (null (search "(deflight" text))))))))

(test matter-commission--waits-and-signals
  "A wrong passcode fails the job in its pairing state; the right code, for the
device named, gives the node -- labelled as the device calls itself."
  (with-fixture lamp-env ()
    (let ((wrong (encode-manual-pairing-code (make-onboarding-payload :discriminator 3840 :passcode 12345679))))
      (signals matter-commissioning-error
        (matter-commission wrong :label "lamp" :device (%lamp-advert lamp)))
      (let ((job (matter-commissioning-job)))
        (is (eq :failed (commissioning-job-state job)))
        (is (eq :pairing (matter-commissioning-error-state (commissioning-job-error job)))))
      (is (null (matter-nodes)))
      (is (= 0 (%lamp-fabrics lamp))))
    (let ((node (matter-commission *lamp-code* :device (%lamp-advert lamp) :device-label nil)))
      (is (string= "Test Lamp" (getf node :label)))
      (is (= (lamp-port lamp) (getf node :port)))
      (is (= 1 (%lamp-fabrics lamp)))
      ;; the device was not told
      (is (string= "" (matter-await (matter-read "Test Lamp" 0 :basic-information :node-label)))))
    (is (eq :done (commissioning-job-state (matter-commissioning-job))))))

(test matter-set-label--the-registry-the-device-and-the-bindings
  (with-fixture lamp-env ()
    (hab:defitem 'lamp "Lamp" 'boolean
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
    (hab:defitem 'desk "Desk" 'boolean
      (matter-binding :node "desk" :endpoint 1 :cluster :on-off))
    (is (listp (%commission-lamp lamp)))
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (is (eql 1 (%subscribed-paths)))
    (let ((node (matter-await (matter-set-label "lamp" "desk"))))
      (is (string= "desk" (getf node :label)))
      (is (= 2 (getf node :node-id))))
    (is (equal '("desk") (mapcar (lambda (n) (getf n :label)) (matter-nodes))))
    (is (equal '("desk") (mapcar #'binding-matter::node-entry-label (binding-matter::read-registry storage))))
    (is (string= "desk" (matter-await (matter-read "desk" 0 :basic-information :node-label))))
    (signals error (matter-await (matter-read "lamp" 1 :on-off :on-off)))
    ;; the item that names the new label follows the lamp now, the other no more
    (is (equal '("DESK") (matter-items-of "desk")))
    (%switch-lamp lamp t)
    (is-true (await-cond 10 (eq 'item:true (%item-value 'desk))))
    (%barrier lamp)
    (is (eq 'item:false (%item-value 'lamp)))
    ;; the label alone, the device left as it is
    (is (string= "bureau" (getf (matter-await (matter-set-label 2 "bureau" :device-label nil)) :label)))
    (is (string= "desk" (matter-await (matter-read "bureau" 0 :basic-information :node-label))))
    ;; its own label again is no conflict
    (is (string= "bureau" (getf (matter-await (matter-set-label "bureau" "bureau")) :label)))))

(test matter-open-window--a-code-for-the-next-app
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    (is-false (%lamp-window-open-p lamp))
    ;; a window is open three minutes at least
    (signals error (matter-await (matter-open-window "lamp" :timeout 100)))
    (is-false (%lamp-window-open-p lamp))
    (let* ((window (matter-await (matter-open-window "lamp" :timeout 180)))
           (manual (matter-decode-pairing-code (getf window :manual-code)))
           (qr (matter-decode-pairing-code (getf window :qr-code))))
      (is (= 180 (getf window :timeout)))
      (is (= 11 (length (getf window :manual-code))))
      (is (eq :qr (getf qr :kind)))
      (is (= (getf window :discriminator) (getf qr :discriminator)))
      (is (= (getf manual :passcode) (getf qr :passcode)))
      (is (= #xFFF1 (getf qr :vendor-id)))
      (is (= #x8000 (getf qr :product-id)))
      (is-true (%lamp-window-open-p lamp))
      ;; the window is the lamp's: what it advertises is what the code says
      (is (= (getf window :discriminator)
             (%lamp-side lamp (lambda () (device-window-discriminator (lamp-device lamp)))))))
    ;; one window at a time: the lamp refuses a second
    (signals error (matter-await (matter-open-window "lamp" :timeout 180)))))

(test matter-decommission--the-node-leaves-the-fabric
  (with-fixture lamp-env ()
    (hab:defitem 'lamp "Lamp" 'boolean
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
    (is (listp (%commission-lamp lamp)))
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (is (= 1 (%lamp-fabrics lamp)))
    (is (eq t (matter-await (matter-decommission "lamp"))))
    (is (null (matter-nodes)))
    (is (null (binding-matter::read-registry storage)))
    (is (zerop (hash-table-count (binding-matter::controller-connections binding-matter::*controller*))))
    ;; the lamp lets go of the fabric, and is to be had again
    (is-true (await-cond 10 (= 0 (%lamp-fabrics lamp))))
    (signals error (matter-await (matter-read "lamp" 1 :on-off :on-off)))
    (signals error (matter-await (matter-decommission "lamp")))
    ;; the item stays as it was, and a change of it is a line in the log
    (hab:set-item-value 'lamp 'item:true)
    (%barrier lamp)
    (is (eq nil (%lamp-on-p lamp)))
    (is-true (await-cond 10 (%lamp-window-open-p lamp)))
    (let ((node (matter-commission *lamp-code* :label "lamp" :device (%lamp-advert lamp))))
      (is (string= "lamp" (getf node :label)))
      (is (= 3 (getf node :node-id))))
    ;; the binding by label binds the new node
    (is-true (await-cond 10 (eq 'item:false (%item-value 'lamp))))
    (hab:set-item-value 'lamp 'item:true)
    (is-true (await-cond 5 (eq t (%lamp-on-p lamp))))))

(test matter-decommission--a-node-that-is-gone-is-forgotten-by-force
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    ;; the node is not to be reached: no session, no address
    (let ((controller binding-matter::*controller*))
      (matter-actor-call (binding-matter::controller-actor controller)
                         (lambda ()
                           (let ((entry (binding-matter::%require-node controller "lamp")))
                             (binding-matter::%drop-session
                              controller entry
                              (binding-matter::connection-session (binding-matter::%connection controller 2)))
                             (setf (binding-matter::node-entry-addresses entry) '())))))
    (let ((condition (%await (matter-decommission "lamp"))))
      (is (typep condition 'error))
      (is (search ":force t" (princ-to-string condition))))
    (is (= 1 (length (matter-nodes))))
    (is (eq t (matter-await (matter-decommission "lamp" :force t))))
    (is (null (matter-nodes)))
    (is (null (binding-matter::read-registry storage)))
    ;; the lamp was not asked
    (is (= 1 (%lamp-fabrics lamp)))))

(test matter-discover--the-nodes-that-answer-and-those-that-do-not
  (with-fixture lamp-env ()
    (is (listp (%commission-lamp lamp)))
    (cl-mock:with-mocks ()
      (cl-mock:answer binding-matter::%browse-commissionable '())
      (cl-mock:answer binding-matter::%browse-operational (list (list 2 :addresses '("fe80::2") :port 5541)))
      (let ((found (matter-discover :timeout 1)))
        (is (null (getf found :commissionable)))
        (is (equal '("lamp") (mapcar (lambda (n) (getf n :label)) (getf found :operational))))
        ;; where it is now, not where the registry has it
        (is (equal '("fe80::2") (getf (first (getf found :operational)) :addresses)))
        (is (= 5541 (getf (first (getf found :operational)) :port)))
        (is (null (getf found :silent)))
        (is (equal '("127.0.0.1") (getf (first (matter-nodes)) :addresses)))))
    (cl-mock:with-mocks ()
      (cl-mock:answer binding-matter::%browse-commissionable (list (%lamp-advert lamp)))
      (cl-mock:answer binding-matter::%browse-operational '())
      (let ((found (matter-discover :timeout 1)))
        (is (equal '("Test Lamp") (mapcar (lambda (d) (getf d :name)) (getf found :commissionable))))
        (is (null (getf found :operational)))
        (is (equal '("lamp") (mapcar (lambda (n) (getf n :label)) (getf found :silent))))))))
