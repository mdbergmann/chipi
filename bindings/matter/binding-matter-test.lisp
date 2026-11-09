(defpackage :chipi.binding.matter-test
  (:use :cl :fiveam :cl-mock :chipi.binding.matter)
  (:local-nicknames (:matter :cl-matter))
  (:import-from #:sento.miscutils
                #:await-cond)
  (:export #:run!
           #:all-tests
           #:nil
           #:%temp-storage
           #:%delete-storage
           #:bridge-env
           #:%in-bridge
           #:%bridged-endpoint
           #:%item-value))
(in-package :chipi.binding.matter-test)

(def-suite matter-binding-tests
  :description "Matter binding: the bridge role, without a network"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-binding-tests)

;; -----------------------------
;; helpers and fixtures
;; -----------------------------

(defun %temp-storage ()
  "A fresh, empty directory for the state files of one test."
  (let ((dir (merge-pathnames (format nil "chipi-matter-test-~d-~d/" (get-universal-time) (random 1000000))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun %delete-storage (dir)
  (when (probe-file dir)
    (uiop:delete-directory-tree dir :validate t)))

(defparameter *test-bridge-args*
  '(:port 0 :family :ipv4 :address "127.0.0.1" :advertise nil)
  "A bridge on an ephemeral IPv4 loopback port without mDNS: what SBCL's backend offers, and all a test needs.")

(def-fixture bridge-env (&key (init t))
  "A chipi environment with the Matter bridge running on loopback, its storage in a fresh directory bound to `storage'. With `:init nil' the bridge is not started; the test does it."
  (let ((storage (%temp-storage)))
    (unwind-protect
         (progn
           (hab:defconfig "chipi")
           (when init
             (matter-init :storage storage :bridge *test-bridge-args*))
           (&body))
      (progn
        (hab:shutdown)
        (%delete-storage storage)))))

(defun %in-bridge (thunk)
  "`THUNK' in the bridge actor's context."
  (matter-actor-call (binding-matter::bridge-actor binding-matter::*bridge*) thunk))

(defun %bridged-endpoint (id)
  "The endpoint `ID' of the bridge's node, read in actor context, or NIL."
  (%in-bridge (lambda () (matter:find-endpoint (binding-matter::bridge-node binding-matter::*bridge*) id))))

(defun %attribute (endpoint-id cluster attribute)
  "The value of `ATTRIBUTE' of `CLUSTER' on the bridge's endpoint `ENDPOINT-ID', read in actor context."
  (%in-bridge (lambda ()
                (matter:attribute-value
                 (matter:find-cluster (matter:find-endpoint (binding-matter::bridge-node binding-matter::*bridge*)
                                                             endpoint-id)
                                      cluster)
                 attribute))))

(defun %invoke (endpoint-id cluster command)
  "Invokes `COMMAND' (an id) of `CLUSTER' on the bridge's endpoint `ENDPOINT-ID' in actor context, as an administrator's Invoke would."
  (%in-bridge (lambda ()
                (matter:invoke-cluster-command
                 (matter:find-cluster (matter:find-endpoint (binding-matter::bridge-node binding-matter::*bridge*)
                                                             endpoint-id)
                                      cluster)
                 command nil))))

(defun %item-value (id)
  (hab:get-item-valueq id))

;; -----------------------------
;; the binding itself
;; -----------------------------

(test matter-bridge--defaults
  "A matter-bridge binding forwards item changes by default, takes its
device type from the item unless told, and is known to the module."
  (let ((binding-matter::*bridge-bindings* '()))
    (let ((cut (matter-bridge)))
      (is (typep cut 'matter-bridge-binding))
      (is-true (binding:call-push-p cut))
      (is (null (binding-matter::binding-device-type cut)))
      (is (null (binding-matter::binding-label cut)))
      (is (equal (list cut) binding-matter::*bridge-bindings*)))
    (let ((explicit (matter-bridge :device-type :contact-sensor :label "Door" :call-push-p nil)))
      (is-false (binding:call-push-p explicit))
      (is (eq :contact-sensor (binding-matter::binding-device-type explicit)))
      (is (string= "Door" (binding-matter::binding-label explicit))))
    (signals error (matter-bridge :device-type :dishwasher))))

(test matter-bridge--destroy-forgets-binding
  (let ((binding-matter::*bridge-bindings* '()))
    (let ((cut (matter-bridge)))
      (binding:destroy cut)
      (is (null binding-matter::*bridge-bindings*)))))

;; -----------------------------
;; conversions
;; -----------------------------

(test convert--kinds-to-device-types
  (is (eq :on-off-light (binding-matter::device-type-of-kind :light)))
  (is (eq :on-off-plug-in-unit (binding-matter::device-type-of-kind :plug)))
  (is (eq :contact-sensor (binding-matter::device-type-of-kind :contact)))
  (is (eq :temperature-sensor (binding-matter::device-type-of-kind :temperature)))
  (is (null (binding-matter::device-type-of-kind :shutter-move)))
  (is (null (binding-matter::device-type-of-kind nil)))
  (multiple-value-bind (id revision cluster) (binding-matter::device-type-spec :on-off-light)
    (is (= matter:+device-type-on-off-light+ id))
    (is (= 3 revision))
    (is (eq :on-off cluster)))
  (multiple-value-bind (id revision cluster) (binding-matter::device-type-spec :temperature-sensor)
    (is (= matter:+device-type-temperature-sensor+ id))
    (is (= 2 revision))
    (is (eq :temperature-measurement cluster)))
  (signals error (binding-matter::device-type-spec :dishwasher)))

(test convert--values
  (is (eq t (binding-matter::item-boolean->bool 'item:true)))
  (is (eq nil (binding-matter::item-boolean->bool 'item:false)))
  (is (eq nil (binding-matter::item-boolean->bool nil)))
  (is (eq 'item:true (binding-matter::bool->item-boolean t)))
  (is (eq 'item:false (binding-matter::bool->item-boolean nil)))
  ;; a contact is open when true; Matter's StateValue is true when closed
  (is (eq nil (binding-matter::item-contact->state-value 'item:true)))
  (is (eq t (binding-matter::item-contact->state-value 'item:false)))
  (is (= 2150 (binding-matter::item-temperature->measured-value 21.5)))
  (is (= -50 (binding-matter::item-temperature->measured-value -0.5)))
  (is (eq :null (binding-matter::item-temperature->measured-value nil)))
  (is (eq :null (binding-matter::item-temperature->measured-value "n/a"))))

;; -----------------------------
;; the endpoint id map
;; -----------------------------

(test endpoint-ids--allocated-from-two-and-kept
  "Names get endpoint ids from 2 upwards, a name keeps its id, and the map
survives a restart of the bridge."
  (let ((storage (%temp-storage)))
    (unwind-protect
         (progn
           (let ((bridge (binding-matter::%make-bridge
                          :storage storage
                          :endpoint-map (binding-matter::%read-endpoint-map storage))))
             (is (null (binding-matter::bridge-endpoint-map bridge)))
             (is (= 2 (binding-matter::%endpoint-id bridge "LIGHT-KITCHEN")))
             (is (= 3 (binding-matter::%endpoint-id bridge "LIGHT-HALL")))
             (is (= 2 (binding-matter::%endpoint-id bridge "LIGHT-KITCHEN")))
             (is (equal '(("LIGHT-KITCHEN" . 2) ("LIGHT-HALL" . 3))
                        (binding-matter::bridge-endpoint-map bridge))))
           ;; another bridge on the same storage: the file carries the map
           (let ((bridge (binding-matter::%make-bridge
                          :storage storage
                          :endpoint-map (binding-matter::%read-endpoint-map storage))))
             (is (equal '(("LIGHT-KITCHEN" . 2) ("LIGHT-HALL" . 3))
                        (binding-matter::bridge-endpoint-map bridge)))
             (is (= 3 (binding-matter::%endpoint-id bridge "LIGHT-HALL")))
             ;; a new name gets the next id; a gone one's id is not reused
             (is (= 4 (binding-matter::%endpoint-id bridge "PLUG-GARDEN")))
             (is (equal '(("LIGHT-KITCHEN" . 2) ("LIGHT-HALL" . 3) ("PLUG-GARDEN" . 4))
                        (binding-matter::%read-endpoint-map storage)))))
      (%delete-storage storage))))

(test unique-id--stable-and-32-hex-digits
  (let ((id (binding-matter::%unique-id "LIGHT-KITCHEN")))
    (is (= 32 (length id)))
    (is (every (lambda (c) (digit-char-p c 16)) id))
    (is (string= id (binding-matter::%unique-id "LIGHT-KITCHEN")))
    (is (string/= id (binding-matter::%unique-id "LIGHT-HALL")))))

(test node-label--cut-to-32
  (is (string= "Kitchen" (binding-matter::%node-label "Kitchen")))
  (is (= 32 (length (binding-matter::%node-label (make-string 40 :initial-element #\x))))))

;; -----------------------------
;; configuration
;; -----------------------------

(test storage-directory--default-and-tilde
  ;; the runtime dir override is relative to the chipi system
  (let* ((envi::*rel-runtime-dir* "test-runtime-matter/")
         (runtime (asdf:system-relative-pathname "chipi" envi::*rel-runtime-dir*)))
    (unwind-protect
         (progn
           (envi:ensure-env "chipi")
           (let ((dir (binding-matter::%storage-directory nil)))
             (is (uiop:directory-pathname-p dir))
             (is (string= "matter" (car (last (pathname-directory dir)))))
             (is (probe-file dir))
             (is (uiop:string-prefix-p (namestring (truename runtime))
                                       (namestring (truename dir))))))
      (progn
        (envi:shutdown-env)
        (%delete-storage runtime))))
  (is (string= (namestring (merge-pathnames ".chipi/matter/" (user-homedir-pathname)))
               (binding-matter::%expand-home "~/.chipi/matter/")))
  (is (string= "/var/lib/chipi/matter/" (binding-matter::%expand-home "/var/lib/chipi/matter/")))
  (let ((storage (%temp-storage)))
    (unwind-protect
         (let ((dir (binding-matter::%storage-directory (namestring (merge-pathnames "sub/" storage)))))
           (is (probe-file dir))
           (is (string= "sub" (car (last (pathname-directory dir))))))
      (%delete-storage storage))))

(test matter-init--without-bridge-only-sets-storage
  (let ((storage (%temp-storage)))
    (unwind-protect
         (progn
           (hab:defconfig "chipi")
           (matter-init :storage storage)
           (is (equal (truename storage) (truename binding-matter::*storage*)))
           (is-false (matter-bridge-running-p))
           (signals error (matter-bridge-pairing-code))
           (hab:shutdown)
           (is (null binding-matter::*storage*)))
      (progn
        (hab:shutdown)
        (%delete-storage storage)))))

(test matter-init--starts-bridge-and-shutdown-stops-it
  (with-fixture bridge-env ()
    (is-true (matter-bridge-running-p))
    (signals error (start-bridge storage))
    (multiple-value-bind (manual qr) (matter-bridge-pairing-code)
      (is (= 11 (length manual)))
      (is (every #'digit-char-p manual))
      (is (string= "MT:" (subseq qr 0 3))))
    ;; the aggregator is there, nothing bridged yet
    (is (equal (list (list :device-type matter:+device-type-aggregator+ :revision 1))
               (%attribute 1 'matter::descriptor 'matter::device-type-list)))
    (is (equal '() (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Chipi" (%attribute 0 'matter::basic-information 'matter::node-label)))
    (let ((actor (binding-matter::bridge-actor binding-matter::*bridge*)))
      (matter-shutdown)
      (is-false (matter-bridge-running-p))
      (is (null binding-matter::*bridge-bindings*))
      (is (null (ac:find-actors (isys:ensure-isys) "matter-bridge")))
      (is-false (act-cell:running-p actor)))))

;; -----------------------------
;; publishing items
;; -----------------------------

(test bridge--light-is-published-and-follows-both-ways
  "A `deflight' with a matter-bridge binding is an On/Off Light on endpoint
2; the item's value sets the attribute, a command sets the item."
  (with-fixture bridge-env ()
    (house:deflight light-kitchen "Kitchen"
      (matter-bridge))
    (let ((endpoint (%bridged-endpoint 2)))
      (is-true endpoint)
      (is (= 1 (matter:endpoint-parent endpoint)))
      (is (equal (list (cons matter:+device-type-on-off-light+ 3)
                       (cons matter:+device-type-bridged-node+ 2))
                 (matter:endpoint-device-types endpoint)))
      (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
      (is (string= "Kitchen" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
      (is (eq t (%attribute 2 'matter::bridged-device-basic-information 'matter::reachable)))
      (is (string= (binding-matter::%unique-id "LIGHT-KITCHEN")
                   (%attribute 2 'matter::bridged-device-basic-information 'matter::unique-id)))
      (is-true (matter:find-cluster endpoint 'matter::identify))
      (is (eq nil (%attribute 2 'matter::on-off 'matter::on-off))))
    ;; item -> attribute
    (hab:set-item-value 'light-kitchen 'item:true)
    (is-true (await-cond 2 (eq t (%attribute 2 'matter::on-off 'matter::on-off))))
    ;; an administrator's Off -> item
    (%invoke 2 'matter::on-off 0)
    (is-true (await-cond 2 (eq 'item:false (%item-value 'light-kitchen))))
    (is (eq nil (%attribute 2 'matter::on-off 'matter::on-off)))
    ;; Toggle
    (%invoke 2 'matter::on-off 2)
    (is-true (await-cond 2 (eq 'item:true (%item-value 'light-kitchen))))
    ;; the endpoint map was written
    (is (equal '(("LIGHT-KITCHEN" . 2)) (binding-matter::%read-endpoint-map storage)))))

(test bridge--command-reaches-the-items-other-bindings
  "A command from an administrator sets the item with push: a KNX binding on
the same item would write it to the bus; the bridge binding itself sees
nothing new."
  (with-fixture bridge-env ()
    (let ((pushed '()))
      (house:deflight light-kitchen "Kitchen"
        (binding:make-function-binding :push (lambda (value) (push value pushed)))
        (matter-bridge))
      (%invoke 2 'matter::on-off 1)
      (is-true (await-cond 2 (equal '(item:true) pushed)))
      ;; flush the bridge actor: its own push has been processed by now
      (%in-bridge (lambda () t))
      (is (equal '(item:true) pushed))
      (is (eq t (%attribute 2 'matter::on-off 'matter::on-off))))))

(test bridge--redefined-item-keeps-its-endpoint
  "Redefining the item withdraws the old endpoint and publishes the new one
under the same id; destroying it withdraws the endpoint and keeps the id."
  (with-fixture bridge-env ()
    (house:deflight light-kitchen "Kitchen" (matter-bridge))
    (is (= 2 (matter:endpoint-id (%bridged-endpoint 2))))
    (house:deflight light-kitchen "Kitchen light" (matter-bridge))
    (is (string= "Kitchen light" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (= 1 (length binding-matter::*bridge-bindings*)))
    ;; a second item goes behind it
    (house:defplug plug-garden "Garden" (matter-bridge))
    (is (equal '(2 3) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (item:destroy (hab:get-item 'light-kitchen))
    (is (equal '(3) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (equal '(("LIGHT-KITCHEN" . 2) ("PLUG-GARDEN" . 3)) (binding-matter::%read-endpoint-map storage)))))

(test bridge--contact-and-temperature
  "A contact is a Contact Sensor whose StateValue is true while closed; a
heating's temperature is a Temperature Sensor in hundredths."
  (with-fixture bridge-env ()
    (house:defcontact window-bath "Bath window" (matter-bridge))
    (house:defheating bath "Bath"
      :temperature ((matter-bridge)))
    (let ((contact (%bridged-endpoint 2))
          (temperature (%bridged-endpoint 3)))
      (is (equal (list (cons matter:+device-type-contact-sensor+ 2)
                       (cons matter:+device-type-bridged-node+ 2))
                 (matter:endpoint-device-types contact)))
      (is (equal (list (cons matter:+device-type-temperature-sensor+ 2)
                       (cons matter:+device-type-bridged-node+ 2))
                 (matter:endpoint-device-types temperature)))
      (is (string= "Bath Temperature" (%attribute 3 'matter::bridged-device-basic-information 'matter::node-label))))
    ;; closed at start
    (is (eq t (%attribute 2 'matter::boolean-state 'matter::state-value)))
    (hab:set-item-value 'window-bath 'item:true)
    (is-true (await-cond 2 (eq nil (%attribute 2 'matter::boolean-state 'matter::state-value))))
    ;; 0.0 at start
    (is (= 0 (%attribute 3 'matter::temperature-measurement 'matter::measured-value)))
    (hab:set-item-value 'bath-temp 21.5)
    (is-true (await-cond 2 (eql 2150 (%attribute 3 'matter::temperature-measurement 'matter::measured-value))))))

(test bridge--device-type-override-and-label
  (with-fixture bridge-env ()
    (hab:defitem 'garden-pump "Garden pump" 'boolean
      :initial-value 'item:true
      (matter-bridge :device-type :on-off-plug-in-unit :label "Pump"))
    (let ((endpoint (%bridged-endpoint 2)))
      (is (equal (list (cons matter:+device-type-on-off-plug-in-unit+ 3)
                       (cons matter:+device-type-bridged-node+ 2))
                 (matter:endpoint-device-types endpoint)))
      (is (string= "Pump" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
      ;; the initial value is the item's
      (is (eq t (%attribute 2 'matter::on-off 'matter::on-off))))))

(test bridge--item-without-kind-is-not-published
  "An item the bridge cannot publish is logged and left out; the bridge and
the item live on."
  (with-fixture bridge-env ()
    (hab:defitem 'counter "Counter" 'integer
      (matter-bridge))
    (house:deflight light-hall "Hall" (matter-bridge))
    (is-true (hab:get-item 'counter))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Hall" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (let ((unpublished (find-if (lambda (b) (eq (binding-matter::binding-item b) (hab:get-item 'counter)))
                                binding-matter::*bridge-bindings*)))
      (is-true unpublished)
      (is (null (binding-matter::binding-endpoint unpublished))))
    ;; a value set on it stays home
    (hab:set-item-value 'counter 5)
    (is-true (await-cond 2 (eql 5 (%item-value 'counter))))))

(test bridge--items-defined-before-the-bridge-are-published-at-start
  (with-fixture bridge-env (:init nil)
    (house:deflight light-kitchen "Kitchen" (matter-bridge))
    (house:defcontact door "Door" (matter-bridge))
    (is-false (matter-bridge-running-p))
    (matter-init :storage storage :bridge *test-bridge-args*)
    (is (equal '(2 3) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Kitchen" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (string= "Door" (%attribute 3 'matter::bridged-device-basic-information 'matter::node-label)))
    ;; and they still follow the items
    (hab:set-item-value 'door 'item:true)
    (is-true (await-cond 2 (eq nil (%attribute 3 'matter::boolean-state 'matter::state-value))))))

;; -----------------------------
;; advertising and startup failure
;; -----------------------------

(test ipv6-addresses--filters-interfaces-and-zones
  (with-mocks ()
    (answer cl-matter.net:network-interfaces
      (list (list :name "lo0" :flags '(:up :loopback) :addresses '("::1"))
            (list :name "en0" :flags '(:up) :addresses '("fe80::1%en0" "192.168.1.5" "2001:db8::5"))
            (list :name "en1" :flags '(:up) :addresses '("fe80::2%en1"))
            (list :name "en2" :flags '() :addresses '("2001:db8::9"))))
    ;; every interface that is up, not loopback and routable; zones removed, IPv4 left out
    (is (equal '("fe80::1" "2001:db8::5")
               (binding-matter::%ipv6-addresses nil)))
    ;; a named interface needs no routable address
    (is (equal '("fe80::2") (binding-matter::%ipv6-addresses "en1")))
    (is (equal '() (binding-matter::%ipv6-addresses "nonexistent")))))

(test start-advertising--sets-flag-and-falls-back-on-error
  (with-mocks ()
    (answer cl-matter.net:network-interfaces
      (list (list :name "en0" :flags '(:up) :addresses '("2001:db8::5"))))
    (answer matter:start-device-advertising t)
    (let ((bridge (binding-matter::%make-bridge :device :the-device)))
      (binding-matter::%start-advertising bridge "Chipi" nil)
      (is-true (binding-matter::bridge-advertising-p bridge))
      (is (= 1 (length (invocations 'matter:start-device-advertising))))))
  (with-mocks ()
    (answer cl-matter.net:network-interfaces
      (list (list :name "en0" :flags '(:up) :addresses '("2001:db8::5"))))
    (answer matter:start-device-advertising (error "no mDNS here"))
    (let ((bridge (binding-matter::%make-bridge :device :the-device)))
      ;; logged, not signalled
      (binding-matter::%start-advertising bridge "Chipi" nil)
      (is-false (binding-matter::bridge-advertising-p bridge)))))

(test start-bridge--failing-startup-releases-the-socket
  (let ((storage (%temp-storage))
        (manager nil))
    (unwind-protect
         (progn
           (with-mocks ()
             (answer matter:make-interaction-model (error "boom"))
             (answer matter:exchange-manager-close t)
             (signals error (apply #'start-bridge storage *test-bridge-args*))
             (is-false (matter-bridge-running-p))
             (let ((closes (invocations 'matter:exchange-manager-close)))
               (is (= 1 (length closes)))
               (setf manager (second (first closes)))))
           ;; the mock only recorded the close; do it for real
           (when manager
             (matter:exchange-manager-close manager)))
      (%delete-storage storage))))

;; -----------------------------
;; the actor
;; -----------------------------

(test matter-actor--call-run-and-stop
  "A Matter actor runs thunks in its context, in order with its polls, and
closes its manager when stopped."
  (unwind-protect
       (let* ((transport (matter:make-udp-transport :address "127.0.0.1"))
              (manager (matter:make-exchange-manager :transport transport))
              (stopped nil)
              (actor (make-matter-actor "matter-test" manager :on-stop (lambda () (setf stopped t))))
              (thread nil))
         (is (= 42 (matter-actor-call actor (lambda () (setf thread (bt:current-thread)) 42))))
         (is (eq thread (matter-actor-call actor (lambda () (bt:current-thread)))))
         (is (not (eq thread (bt:current-thread))))
         (signals error (matter-actor-call actor (lambda () (error "boom"))))
         (let ((ran nil))
           (matter-actor-run actor (lambda () (setf ran t)))
           (is-true (await-cond 1 ran)))
         ;; an error in a run thunk is logged, the actor goes on
         (matter-actor-run actor (lambda () (error "boom")))
         (is (= 1 (matter-actor-call actor (lambda () 1))))
         (matter-actor-stop actor)
         (is-true stopped)
         (is-false (act-cell:running-p actor)))
    (envi:shutdown-env)))
