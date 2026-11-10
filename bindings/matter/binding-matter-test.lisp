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

(defun %cluster (endpoint-id cluster)
  "Actor context. `CLUSTER' on the bridge's endpoint `ENDPOINT-ID'."
  (matter:find-cluster (matter:find-endpoint (binding-matter::bridge-node binding-matter::*bridge*) endpoint-id)
                       cluster))

(defun %invoke (endpoint-id cluster command &optional fields)
  "Invokes `COMMAND' (an id) of `CLUSTER' on the bridge's endpoint `ENDPOINT-ID' in actor context, as an administrator's Invoke would; `FIELDS' is the TLV structure of its fields."
  (%in-bridge (lambda ()
                (nth-value 1 (matter:invoke-cluster-command (%cluster endpoint-id cluster) command fields)))))

(defun %write (endpoint-id cluster attribute element)
  "Writes the TLV `ELEMENT' to `ATTRIBUTE' (an id) of `CLUSTER' on the bridge's endpoint `ENDPOINT-ID' in actor context, as an administrator's Write would."
  (%in-bridge (lambda ()
                (matter:write-attribute (%cluster endpoint-id cluster) attribute element))))

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
      (is (null (binding-matter::binding-device-name cut)))
      (is (null (binding-matter::binding-label cut)))
      (is (null (binding-matter::binding-endpoint cut)))
      (is (equal (list cut) binding-matter::*bridge-bindings*)))
    (let ((explicit (matter-bridge :device-type :contact-sensor :label "Door" :call-push-p nil)))
      (is-false (binding:call-push-p explicit))
      (is (eq :contact-sensor (binding-matter::binding-device-type explicit)))
      (is (string= "Door" (binding-matter::binding-label explicit))))
    ;; the device name is taken as written, or evaluated
    (is (eq 'blind-living (binding-matter::binding-device-name (matter-bridge :device blind-living))))
    (is (eq 'blind-living (binding-matter::binding-device-name (matter-bridge :device 'blind-living :label "Blind"))))
    (let ((name 'blind-attic))
      (is (eq 'blind-attic (binding-matter::binding-device-name (make-matter-bridge-binding :device name)))))
    (signals error (matter-bridge :device-type :dishwasher))
    (signals error (make-matter-bridge-binding :device "blind-living"))))

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
  (multiple-value-bind (id revision cluster) (binding-matter::device-type-spec :window-covering)
    (is (= matter:+device-type-window-covering+ id))
    (is (= 2 revision))
    (is (eq :window-covering cluster)))
  (multiple-value-bind (id revision cluster) (binding-matter::device-type-spec :thermostat)
    (is (= matter:+device-type-thermostat+ id))
    (is (= 2 revision))
    (is (eq :thermostat cluster)))
  (signals error (binding-matter::device-type-spec :dishwasher))
  ;; the kinds of several items make a composed device
  (is (eq :window-covering (binding-matter::device-type-of-kinds '(:shutter-move :shutter-stop :shutter-position :shutter-slat))))
  (is (eq :window-covering (binding-matter::device-type-of-kinds '(:shutter-position))))
  (is (eq :thermostat (binding-matter::device-type-of-kinds '(:temperature :setpoint))))
  (is (eq :thermostat (binding-matter::device-type-of-kinds '(:setpoint))))
  (is (eq :temperature-sensor (binding-matter::device-type-of-kinds '(:temperature))))
  (is (eq :on-off-light (binding-matter::device-type-of-kinds '(:light))))
  (is (null (binding-matter::device-type-of-kinds '(:light :plug))))
  (is (null (binding-matter::device-type-of-kinds '(:valve))))
  (is (null (binding-matter::device-type-of-kinds '())))
  (is (equal '(:temperature :setpoint) (binding-matter::composed-device-kinds :thermostat)))
  (is (null (binding-matter::composed-device-kinds :on-off-light))))

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
  (is (eq :null (binding-matter::item-temperature->measured-value "n/a")))
  ;; a shutter position is a percentage both sides, the wire in hundredths
  (is (= 3700 (binding-matter::item-position->percent100ths 37)))
  (is (= 10000 (binding-matter::item-position->percent100ths 100)))
  (is (eq :null (binding-matter::item-position->percent100ths nil)))
  (is (= 37 (binding-matter::percent100ths->item-position 3700)))
  (is (= 38 (binding-matter::percent100ths->item-position 3750)))
  ;; a heating setpoint stays within the limits the thermostat tells
  (is (= 2150 (binding-matter::item-setpoint->heating-setpoint 21.5)))
  (is (= 500 (binding-matter::item-setpoint->heating-setpoint 0.0)))
  (is (= 3500 (binding-matter::item-setpoint->heating-setpoint 40)))
  (is (= 500 (binding-matter::item-setpoint->heating-setpoint nil)))
  (is (= 21.5 (binding-matter::heating-setpoint->item-setpoint 2150)))
  (is (typep (binding-matter::heating-setpoint->item-setpoint 2150) 'float)))

(test common-label--the-words-labels-share
  (is (string= "Bath" (binding-matter::%common-label '("Bath Temperature" "Bath Setpoint"))))
  (is (string= "Blind living room"
               (binding-matter::%common-label '("Blind living room" "Blind living room Stop" "Blind living room Position"))))
  (is (string= "Kitchen" (binding-matter::%common-label '("Kitchen"))))
  ;; no common words: the first label
  (is (string= "Bath Temperature" (binding-matter::%common-label '("Bath Temperature" "Bathroom Setpoint")))))

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

(test bridge--follows-values-another-binding-brought-in
  "A value set with `:push nil' -- how binding-knx sets a telegram's and a
read's value -- is pushed to no binding and still reaches the attribute:
the bridge follows the item's change event. Found on the bench of
2026-11-10: contact and thermostat stood still in the Home app."
  (with-fixture bridge-env ()
    (house:deflight light-kitchen "Kitchen" (matter-bridge))
    (house:defcontact window-bath "Bath window" (matter-bridge))
    (house:defheating bath "Bath"
      :temperature ((matter-bridge :device bath))
      :setpoint ((matter-bridge :device bath)))
    (item:set-value (hab:get-item 'light-kitchen) 'item:true :push nil)
    (is-true (await-cond 2 (eq t (%attribute 2 'matter::on-off 'matter::on-off))))
    (item:set-value (hab:get-item 'window-bath) 'item:true :push nil)
    (is-true (await-cond 2 (eq nil (%attribute 3 'matter::boolean-state 'matter::state-value))))
    (item:set-value (hab:get-item 'bath-temp) 20.1 :push nil)
    (is-true (await-cond 2 (eql 2010 (%attribute 4 'matter::thermostat 'matter::local-temperature))))
    (item:set-value (hab:get-item 'bath-setpoint) 19.0 :push nil)
    (is-true (await-cond 2 (eql 1900 (%attribute 4 'matter::thermostat 'matter::occupied-heating-setpoint))))
    ;; the listener goes with the bridge
    (let ((listener (binding-matter::bridge-listener binding-matter::*bridge*)))
      (is-true listener)
      (matter-shutdown)
      (is-false (binding-matter::matter-bridge-running-p))
      (item:set-value (hab:get-item 'light-kitchen) 'item:false :push nil)
      (is-true (await-cond 2 (eq 'item:false (%item-value 'light-kitchen)))))))

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
;; composed devices
;; -----------------------------

(defun %lift (attribute)
  (%attribute 2 'matter::window-covering attribute))

(test bridge--rollershutter-is-one-window-covering
  "The four channels of a `defrollershutter' naming one `:device' are one
Window Covering with lift and tilt; its position follows the position
item, the commands move the shutter through its items, and the target
tells an administrator where it is going."
  (with-fixture bridge-env ()
    (let ((commands '())
          (positions '()))
      (house:defrollershutter blind-living "Blind living room"
        :move ((binding:make-function-binding :push (lambda (value) (push (cons :move value) commands)))
               (matter-bridge :device blind-living))
        :stop ((binding:make-function-binding :push (lambda (value) (push (cons :stop value) commands)))
               (matter-bridge :device blind-living))
        :position ((binding:make-function-binding :push (lambda (value) (push value positions)))
                   (matter-bridge :device blind-living))
        :slat ((matter-bridge :device blind-living)))
      ;; one endpoint for the four items
      (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
      (let ((endpoint (%bridged-endpoint 2)))
        (is (equal (list (cons matter:+device-type-window-covering+ 2)
                         (cons matter:+device-type-bridged-node+ 2))
                   (matter:endpoint-device-types endpoint))))
      (is (string= "Blind living room" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
      (is (string= (binding-matter::%unique-id "BLIND-LIVING")
                   (%attribute 2 'matter::bridged-device-basic-information 'matter::unique-id)))
      (is (= 23 (%lift 'matter::feature-map)))                 ; lift, tilt, both position aware
      (is (= 0 (%lift 'matter::current-position-lift-percent100ths)))
      (is (= 0 (%lift 'matter::target-position-lift-percent100ths)))
      (is (= 0 (%lift 'matter::current-position-tilt-percent100ths)))
      (is (equal '(("BLIND-LIVING" . 2)) (binding-matter::%read-endpoint-map storage)))
      ;; the position item moves current and target
      (hab:set-item-value 'blind-living-pos 37)
      (is-true (await-cond 2 (eql 3700 (%lift 'matter::current-position-lift-percent100ths))))
      (is (= 3700 (%lift 'matter::target-position-lift-percent100ths)))
      ;; DownOrClose: the move item gets a down command, the target is closed
      (%invoke 2 'matter::window-covering 1)
      (is-true (await-cond 2 (equal '((:move . item:true)) commands)))
      (is (= 10000 (%lift 'matter::target-position-lift-percent100ths)))
      ;; on its way: the target stays
      (hab:set-item-value 'blind-living-pos 60)
      (is-true (await-cond 2 (eql 6000 (%lift 'matter::current-position-lift-percent100ths))))
      (is (= 10000 (%lift 'matter::target-position-lift-percent100ths)))
      ;; StopMotion: the stop item gets a stop command, the target is where it is
      (%invoke 2 'matter::window-covering 2)
      (is-true (await-cond 2 (equal '((:stop . item:true) (:move . item:true)) commands)))
      (%in-bridge (lambda () t))
      (is (= 6000 (%lift 'matter::target-position-lift-percent100ths)))
      ;; GoToLiftPercentage: the position item is set -- with push, the other binding writes it
      (%invoke 2 'matter::window-covering 5 (matter:tlv nil :structure (list (matter:tlv 0 :unsigned 2500))))
      (is-true (await-cond 2 (eql 25 (%item-value 'blind-living-pos))))
      (is-true (await-cond 2 (equal '(25 60 37) positions)))
      (is (equal '((:stop . item:true) (:move . item:true)) commands))
      (is-true (await-cond 2 (eql 2500 (%lift 'matter::current-position-lift-percent100ths))))
      (is (= 2500 (%lift 'matter::target-position-lift-percent100ths)))
      ;; GoToTiltPercentage: the slat item
      (%invoke 2 'matter::window-covering 8 (matter:tlv nil :structure (list (matter:tlv 0 :unsigned 5000))))
      (is-true (await-cond 2 (eql 50 (%item-value 'blind-living-slat))))
      (is-true (await-cond 2 (eql 5000 (%lift 'matter::current-position-tilt-percent100ths))))
      (is (= 5000 (%lift 'matter::target-position-tilt-percent100ths)))
      ;; the wall switch: up, and the target is open while it moves that way
      (blind-living-up)
      (is-true (await-cond 2 (eql 0 (%lift 'matter::target-position-lift-percent100ths))))
      (hab:set-item-value 'blind-living-pos 10)
      (is-true (await-cond 2 (eql 1000 (%lift 'matter::current-position-lift-percent100ths))))
      (is (= 0 (%lift 'matter::target-position-lift-percent100ths)))
      ;; but a position away from the target is where it stopped
      (hab:set-item-value 'blind-living-pos 20)
      (is-true (await-cond 2 (eql 2000 (%lift 'matter::target-position-lift-percent100ths)))))))

(test bridge--shutter-command-is-applied-once-and-repeats-arrive
  "A move command changes the move item and raises an event, yet reaches the
attribute once, through the push; the same command repeated with `:push
:always' raises no event and still arrives."
  (with-fixture bridge-env ()
    (house:defrollershutter blind-once "Blind once"
      :move ((binding:make-function-binding :push (lambda (value) (declare (ignore value)) nil))
             (matter-bridge :device blind-once))
      :position ((binding:make-function-binding :push (lambda (value) (declare (ignore value)) nil))
                 (matter-bridge :device blind-once)))
    (with-mocks ()
      (answer binding-matter::%apply-item-value nil)
      (flet ((moves ()
               (count 'item:true (invocations 'binding-matter::%apply-item-value)
                      :key #'second)))
        (hab:set-item-value 'blind-once 'item:true)
        ;; the position change is the fence: its event comes after the move's
        (hab:set-item-value 'blind-once-pos 5)
        (is-true (await-cond 2 (find 5 (invocations 'binding-matter::%apply-item-value) :key #'second)))
        (is (= 1 (moves)))
        ;; the same value again, pushed regardless
        (item:set-value (hab:get-item 'blind-once) 'item:true :push :always)
        (is-true (await-cond 2 (= 2 (moves))))
        (hab:set-item-value 'blind-once-pos 6)
        (is-true (await-cond 2 (find 6 (invocations 'binding-matter::%apply-item-value) :key #'second)))
        (is (= 2 (moves)))))))

(test bridge--rollershutter-with-fewer-channels
  "A shutter with a position item alone is driven through it; one with a
move item alone has no position and takes no target."
  (with-fixture bridge-env ()
    (house:defrollershutter awning "Awning"
      :position ((matter-bridge :device awning)))
    (house:defrollershutter blind-attic "Attic"
      :move ((matter-bridge :device blind-attic)))
    (is (equal '(2 3) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Awning Position" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (= 5 (%attribute 2 'matter::window-covering 'matter::feature-map)))
    (%invoke 2 'matter::window-covering 1)
    (is-true (await-cond 2 (eql 100 (%item-value 'awning-pos))))
    (%invoke 2 'matter::window-covering 0)
    (is-true (await-cond 2 (eql 0 (%item-value 'awning-pos))))
    ;; a stop with no stop item is dropped, the bridge lives on
    (%invoke 2 'matter::window-covering 2)
    (is (eql 0 (%item-value 'awning-pos)))
    ;; move alone
    (is (= 1 (%attribute 3 'matter::window-covering 'matter::feature-map)))
    (is-false (%in-bridge (lambda () (matter:attribute-present-p (%cluster 3 'matter::window-covering)
                                                                 'matter::target-position-lift-percent100ths))))
    (%invoke 3 'matter::window-covering 1)
    (is-true (await-cond 2 (eq 'item:true (%item-value 'blind-attic))))
    (%invoke 3 'matter::window-covering 0)
    (is-true (await-cond 2 (eq 'item:false (%item-value 'blind-attic))))))

(test bridge--heating-is-a-thermostat
  "The temperature and setpoint of a `defheating' naming one `:device' are
a heating-only Thermostat; its valve stays at home. The setpoint an
administrator writes or steps reaches the setpoint item."
  (with-fixture bridge-env ()
    (let ((pushed '()))
      (house:defheating bath "Bath"
        :temperature ((matter-bridge :device bath))
        :setpoint ((binding:make-function-binding :push (lambda (value) (push value pushed)))
                   (matter-bridge :device bath))
        :valve ())
      (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
      (is (equal (list (cons matter:+device-type-thermostat+ 2)
                       (cons matter:+device-type-bridged-node+ 2))
                 (matter:endpoint-device-types (%bridged-endpoint 2))))
      (is (string= "Bath" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
      (is (= 1 (%attribute 2 'matter::thermostat 'matter::feature-map)))
      (is (= 0 (%attribute 2 'matter::thermostat 'matter::local-temperature)))
      (is (= 500 (%attribute 2 'matter::thermostat 'matter::occupied-heating-setpoint)))   ; 0.0 clamped to the lower limit
      (is (= 500 (%attribute 2 'matter::thermostat 'matter::min-heat-setpoint-limit)))
      (is (= 3500 (%attribute 2 'matter::thermostat 'matter::max-heat-setpoint-limit)))
      ;; the items report
      (hab:set-item-value 'bath-temp 21.5)
      (is-true (await-cond 2 (eql 2150 (%attribute 2 'matter::thermostat 'matter::local-temperature))))
      (hab:set-item-value 'bath-setpoint 22.0)
      (is-true (await-cond 2 (eql 2200 (%attribute 2 'matter::thermostat 'matter::occupied-heating-setpoint))))
      (is (equal '(22.0) pushed))
      ;; an administrator writes the setpoint: the item follows, with push
      (%write 2 'matter::thermostat #x12 (matter:tlv nil :signed 2350))
      (is-true (await-cond 2 (eql 23.5 (%item-value 'bath-setpoint))))
      (is-true (await-cond 2 (equal '(23.5 22.0) pushed)))
      (%in-bridge (lambda () t))
      (is (= 2350 (%attribute 2 'matter::thermostat 'matter::occupied-heating-setpoint)))
      ;; and steps it by half a degree
      (%invoke 2 'matter::thermostat 0 (matter:tlv nil :structure (list (matter:tlv 0 :unsigned 0) (matter:tlv 1 :signed 5))))
      (is-true (await-cond 2 (eql 24.0 (%item-value 'bath-setpoint))))
      ;; the mode is taken note of, nothing more
      (%write 2 'matter::thermostat #x1C (matter:tlv nil :unsigned 0))
      (is (= 0 (%attribute 2 'matter::thermostat 'matter::system-mode))))))

(test bridge--device-items-join-and-leave
  "A device is published afresh as items join and leave it: a temperature
alone is a Temperature Sensor, with a setpoint a Thermostat, and the
endpoint id stays through all of it."
  (with-fixture bridge-env ()
    (house:defheating bath "Bath"
      :temperature ((matter-bridge :device bath)))
    (is (equal (list (cons matter:+device-type-temperature-sensor+ 2)
                     (cons matter:+device-type-bridged-node+ 2))
               (matter:endpoint-device-types (%bridged-endpoint 2))))
    (is (string= "Bath Temperature" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    ;; the setpoint joins: the same endpoint is a Thermostat now
    (house:defheating bath "Bath"
      :temperature ((matter-bridge :device bath))
      :setpoint ((matter-bridge :device bath)))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (equal (list (cons matter:+device-type-thermostat+ 2)
                     (cons matter:+device-type-bridged-node+ 2))
               (matter:endpoint-device-types (%bridged-endpoint 2))))
    (is (string= "Bath" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (= 1 (length (binding-matter::bridge-devices binding-matter::*bridge*))))
    (is (= 2 (length (binding-matter::bridged-device-bindings
                      (first (binding-matter::bridge-devices binding-matter::*bridge*))))))
    (hab:set-item-value 'bath-temp 19.0)
    (is-true (await-cond 2 (eql 1900 (%attribute 2 'matter::thermostat 'matter::local-temperature))))
    ;; the setpoint leaves: a Temperature Sensor again
    (item:destroy (hab:get-item 'bath-setpoint))
    (is (equal (list (cons matter:+device-type-temperature-sensor+ 2)
                     (cons matter:+device-type-bridged-node+ 2))
               (matter:endpoint-device-types (%bridged-endpoint 2))))
    (is (= 1900 (%attribute 2 'matter::temperature-measurement 'matter::measured-value)))
    ;; the last item leaves: the endpoint goes, its id stays reserved
    (item:destroy (hab:get-item 'bath-temp))
    (is (equal '() (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (null (binding-matter::bridge-devices binding-matter::*bridge*)))
    (is (equal '(("BATH" . 2)) (binding-matter::%read-endpoint-map storage)))))

(test bridge--device-of-unfit-items-is-not-published
  "Items that make no device together, or a type naming one item given
two, are logged and left out until an item that fits joins or the odd one
leaves."
  (with-fixture bridge-env ()
    ;; a light and a plug are no device
    (house:deflight light-desk "Desk" (matter-bridge :device desk))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (house:defplug plug-desk "Desk plug" (matter-bridge :device desk))
    (is (equal '() (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is-true (hab:get-item 'plug-desk))
    ;; the plug leaves: the light is back
    (item:destroy (hab:get-item 'plug-desk))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Desk" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    ;; a valve has no place in a thermostat
    (house:defheating bath "Bath"
      :setpoint ((matter-bridge :device bath))
      :valve ((matter-bridge :device bath)))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    ;; two items of one kind
    (house:defcontact door-a "Door A" (matter-bridge :device door))
    (house:defcontact door-b "Door B" (matter-bridge :device door))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    ;; device types that disagree
    (house:deflight lamp-a "Lamp A" (matter-bridge :device lamps :device-type :on-off-light))
    (house:deflight lamp-b "Lamp B" (matter-bridge :device lamps :device-type :on-off-plug-in-unit))
    (is (equal '(2) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    ;; a value on an unpublished part stays home
    (hab:set-item-value 'bath-setpoint 21.0)
    (is-true (await-cond 2 (eql 21.0 (%item-value 'bath-setpoint))))))

(test bridge--devices-defined-before-the-bridge-are-published-once
  "A shutter defined before the bridge starts is published once, whole."
  (with-fixture bridge-env (:init nil)
    (house:defrollershutter blind-living "Blind living room"
      :move ((matter-bridge :device blind-living))
      :stop ((matter-bridge :device blind-living))
      :position ((matter-bridge :device blind-living)))
    (house:deflight light-kitchen "Kitchen" (matter-bridge))
    (matter-init :storage storage :bridge *test-bridge-args*)
    (is (equal '(2 3) (%attribute 1 'matter::descriptor 'matter::parts-list)))
    (is (string= "Blind living room" (%attribute 2 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (= 5 (%attribute 2 'matter::window-covering 'matter::feature-map)))
    (is (string= "Kitchen" (%attribute 3 'matter::bridged-device-basic-information 'matter::node-label)))
    (is (equal '(("BLIND-LIVING" . 2) ("LIGHT-KITCHEN" . 3)) (binding-matter::%read-endpoint-map storage)))
    (%invoke 2 'matter::window-covering 0)
    (is-true (await-cond 2 (eq 'item:false (%item-value 'blind-living))))
    (hab:set-item-value 'blind-living-pos 50)
    (is-true (await-cond 2 (eql 5000 (%attribute 2 'matter::window-covering 'matter::current-position-lift-percent100ths))))))

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

;; -----------------------------
;; sharing the bridge: the window, the administrators, the devices
;; -----------------------------

(test sharing--window-open-from-the-start-closed-and-opened-again
  (with-fixture bridge-env ()
    ;; no app yet: open until closed
    (is (eq t (matter-bridge-window-open-p)))
    (is-true (matter-bridge-close-window))
    (is-false (matter-bridge-window-open-p))
    (multiple-value-bind (manual qr) (matter-bridge-open-window :timeout 600)
      (is (= 11 (length manual)))
      (is (string= "MT:" (subseq qr 0 3)))
      (is (equal (multiple-value-list (matter-bridge-pairing-code)) (list manual qr))))
    (let ((left (matter-bridge-window-open-p)))
      (is (integerp left))
      (is (<= 590 left 600)))
    ;; the default is the 15 minutes Matter allows at most
    (matter-bridge-open-window)
    (is (<= 890 (matter-bridge-window-open-p) 900))
    (signals error (matter-bridge-open-window :timeout -1))))

(test sharing--functions-need-the-running-bridge
  (with-fixture bridge-env (:init nil)
    (signals error (matter-bridge-window-open-p))
    (signals error (matter-bridge-open-window))
    (signals error (matter-bridge-close-window))
    (signals error (matter-bridge-admins))
    (signals error (matter-bridge-remove-admin 1))
    (signals error (matter-bridge-devices))))

(test sharing--admins-listed-and-removed
  (with-fixture bridge-env ()
    (is (equal '() (matter-bridge-admins)))
    ;; a fabric installed as commissioning leaves one, without the network
    (let ((fabric (matter:make-test-fabric :fabric-id #x1D :node-id 7 :vendor-id #x1349)))
      (setf (matter:fabric-label fabric) "Apple Home")
      (%in-bridge (lambda ()
                    (matter::%install-fabric (binding-matter::bridge-device binding-matter::*bridge*) fabric))))
    (let ((admins (matter-bridge-admins)))
      (is (= 1 (length admins)))
      (let ((admin (first admins)))
        (is (= 1 (getf admin :fabric-index)))
        (is (string= "Apple Home" (getf admin :label)))
        (is (= #x1349 (getf admin :vendor-id)))
        (is (= #x1D (getf admin :fabric-id)))
        (is (= 7 (getf admin :node-id)))
        (is-false (matter-bridge-remove-admin 9))
        (is (= 1 (length (matter-bridge-admins))))
        (is-true (matter-bridge-remove-admin (getf admin :fabric-index)))
        (is (equal '() (matter-bridge-admins)))
        (signals error (matter-bridge-remove-admin "1"))))))

(test sharing--devices-as-the-apps-see-them
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall" (matter-bridge))
    (house:defheating bath "Bath"
      :temperature ((matter-bridge :device bath))
      :setpoint ((matter-bridge :device bath)))
    ;; neither a kind nor a device type: not published, and it says why
    (hab:defitem 'nothing "Nothing" 'boolean (matter-bridge))
    (let ((devices (matter-bridge-devices)))
      (is (equal '("LIGHT-HALL" "BATH" "NOTHING") (mapcar (lambda (d) (getf d :name)) devices)))
      (let ((hall (first devices)))
        (is (string= "Hall" (getf hall :label)))
        (is (eq :on-off-light (getf hall :device-type)))
        (is (= 2 (getf hall :endpoint)))
        (is (equal '("LIGHT-HALL") (getf hall :items)))
        (is-true (getf hall :reachable-p))
        (is (null (getf hall :problem))))
      (let ((bath (second devices)))
        (is (string= "Bath" (getf bath :label)))
        (is (eq :thermostat (getf bath :device-type)))
        (is (= 3 (getf bath :endpoint)))
        (is (equal '("BATH-TEMP" "BATH-SETPOINT") (getf bath :items))))
      (let ((nothing (third devices)))
        (is (null (getf nothing :device-type)))
        (is (null (getf nothing :endpoint)))
        (is-false (getf nothing :reachable-p))
        (is-true (search "no house kind" (getf nothing :problem)))))
    ;; the problem goes when the device is published after all
    (hab:defitem 'nothing "Nothing" 'boolean (matter-bridge :device-type :on-off-plug-in-unit))
    (let ((nothing (third (matter-bridge-devices))))
      (is (eq :on-off-plug-in-unit (getf nothing :device-type)))
      (is (= 4 (getf nothing :endpoint)))
      (is (null (getf nothing :problem))))))
