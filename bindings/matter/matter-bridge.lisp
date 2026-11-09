(in-package :chipi.binding.matter)

;; Chipi as a Matter bridge.
;;
;; One cl-matter device on the bridge actor: endpoint 0 is the root,
;; endpoint 1 the Aggregator, and from endpoint 2 on there is one Bridged
;; Node per item that carries a `matter-bridge' binding.  Each bridged
;; endpoint has the device type of the item's house kind (a light, a plug, a
;; contact, a temperature), Identify, Bridged Device Basic Information
;; naming it, and the one cluster that carries the item's value.
;;
;; The two directions: a push from the item becomes an attribute change,
;; which cl-matter reports to subscribers (Apple Home) by itself; a command
;; from an administrator runs the cluster's handler, which sets the item
;; with push, so the item's other bindings (KNX) carry it out -- and the
;; bridge binding's own push then finds the attribute equal and does
;; nothing.
;;
;; Endpoint ids are kept across restarts in the `endpoints' file of the
;; storage directory: administrators keep room, name and automations of a
;; bridged device by its endpoint id, so a name gets the id it had, a new
;; name the next one, and an id is never given to another name.
;;
;; The commissioned fabrics, the ACL and the resumption records are
;; cl-matter's business: `bridge.state' in the same directory.

(defconstant +aggregator-endpoint+ 1
  "The endpoint of the Aggregator; the bridged endpoints are its parts.")

(defconstant +first-bridged-endpoint+ 2
  "The first endpoint id a bridged device gets.")

(defparameter *bridge-salt*
  (babel:string-to-octets "Chipi Matter bridge" :encoding :utf-8)
  "The PASE salt of the bridge's own commissioning window; the passcode is the secret, the salt only has to be 16..32 octets.")

(defparameter *bridge-pase-iterations* 1000)

(defvar *bridge* nil
  "The running `bridge', or NIL.")

(defvar *bridge-bindings* '()
  "Every live `matter-bridge-binding', published or not (a binding that has no item yet, or none the bridge can publish).")

(defvar *bridge-bindings-lock* (bt:make-lock "matter-bridge-bindings")
  "Guards `*bridge-bindings*': bindings are made and destroyed on arbitrary threads.")

(defun %register-bridge-binding (binding)
  (bt:with-lock-held (*bridge-bindings-lock*)
    (push binding *bridge-bindings*)))

(defun %unregister-bridge-binding (binding)
  (bt:with-lock-held (*bridge-bindings-lock*)
    (setf *bridge-bindings* (remove binding *bridge-bindings*))))

(defun %bridge-bindings-snapshot ()
  "A copy of `*bridge-bindings*', newest first."
  (bt:with-lock-held (*bridge-bindings-lock*)
    (copy-list *bridge-bindings*)))

(defun %clear-bridge-bindings ()
  (bt:with-lock-held (*bridge-bindings-lock*)
    (setf *bridge-bindings* '())))

(defstruct (bridge (:constructor %make-bridge))
  "Chipi's Matter bridge: its ACTOR, the exchange MANAGER the actor polls,
the data model NODE with its AGGREGATOR endpoint, the cl-matter DEVICE, the
STORAGE directory, the PASSCODE of the commissioning window, the
ENDPOINT-MAP (an alist of bridged device name to endpoint id) and whether
it is ADVERTISING-P over mDNS."
  actor manager node device aggregator
  storage
  passcode
  (endpoint-map '())
  advertising-p)

(defmethod print-object ((bridge bridge) stream)
  (print-unreadable-object (bridge stream :type t)
    (format stream "~d bridged endpoint~:p, ~d fabric~:p~:[~;, advertising~]"
            (length (bridge-endpoint-map bridge))
            (length (matter:device-fabrics (bridge-device bridge)))
            (bridge-advertising-p bridge))))

;; -----------------------------
;; the endpoint id map
;; -----------------------------

(defun %endpoint-map-file (storage)
  (merge-pathnames "endpoints" storage))

(defun %read-endpoint-map (storage)
  "The endpoint map kept in `STORAGE', an alist of (name . id); empty when there is none yet."
  (getf (matter:read-state-file (%endpoint-map-file storage)) :endpoints))

(defun %write-endpoint-map (bridge)
  (matter:write-state-file (list :format matter:+state-format+
                                 :endpoints (bridge-endpoint-map bridge))
                           (%endpoint-map-file (bridge-storage bridge))))

(defun %endpoint-id (bridge name)
  "The endpoint id of the bridged device `NAME': the one it had, else the next free one.
A new id is written to the map at once, and an entry is never dropped: a device that is gone keeps its id, so a device of another name cannot inherit the room and automations an administrator attached to it."
  (or (cdr (assoc name (bridge-endpoint-map bridge) :test #'string=))
      (let ((id (1+ (reduce #'max (mapcar #'cdr (bridge-endpoint-map bridge))
                            :initial-value (1- +first-bridged-endpoint+)))))
        (setf (bridge-endpoint-map bridge)
              (append (bridge-endpoint-map bridge) (list (cons name id))))
        (%write-endpoint-map bridge)
        id)))

;; -----------------------------
;; the binding
;; -----------------------------

(defclass matter-bridge-binding (binding:binding)
  ((device-type :initarg :device-type
                :initform nil
                :reader binding-device-type
                :documentation "The name of the device type to publish the item as (see `*bridged-device-types*'), or NIL to take it from the item's house kind.")
   (label :initarg :label
          :initform nil
          :reader binding-label
          :documentation "The NodeLabel administrators show for the device, or NIL for the item's label.")
   (item :initform nil
         :accessor binding-item
         :documentation "The item, once `bind-item' gave it.")
   (endpoint :initform nil
             :accessor binding-endpoint
             :documentation "The bridged endpoint while the item is published on the bridge.")
   (cluster :initform nil
            :accessor binding-cluster
            :documentation "The cluster of that endpoint carrying the item's value.")
   (value-cluster :initform nil
                  :accessor binding-value-cluster
                  :documentation "Its kind: :on-off, :boolean-state or :temperature-measurement.")))

(defmethod print-object ((binding matter-bridge-binding) stream)
  (print-unreadable-object (binding stream :type t)
    (format stream "item: ~a, device-type: ~a, endpoint: ~a"
            (and (binding-item binding) (item:name (binding-item binding)))
            (binding-device-type binding)
            (and (binding-endpoint binding) (matter:endpoint-id (binding-endpoint binding))))))

(defun %house-kind (item)
  (cdr (assoc :house-kind (item:tags item))))

(defun %resolve-device-type (binding item)
  "The device type `BINDING' publishes `ITEM' as: its own, else the one of the item's house kind; an error when there is neither."
  (or (binding-device-type binding)
      (device-type-of-kind (%house-kind item))
      (error "Item ~a has no house kind the Matter bridge publishes (~s) and its matter-bridge binding names no :device-type"
             (item:name item) (%house-kind item))))

(defun %item-value (item)
  (item:item-state-value (item:get-item-stateq item)))

(defun %unique-id (name)
  "The UniqueID of the bridged device `NAME': 32 hex digits of its SHA-256, the same after every restart."
  (let ((digest (matter:sha256 (babel:string-to-octets name :encoding :utf-8))))
    (with-output-to-string (out)
      (loop :for i :below 16
            :do (format out "~(~2,'0x~)" (aref digest i))))))

(defun %node-label (string)
  "`STRING' cut to the 32 characters a NodeLabel holds."
  (if (> (length string) 32)
      (subseq string 0 32)
      string))

(defun %make-value-cluster (item value-cluster)
  "The cluster of kind `VALUE-CLUSTER' carrying `ITEM's value, initialised from it. A command on it sets the item -- with push, so the item's other bindings carry it out."
  (ecase value-cluster
    (:on-off
     (matter:make-on-off-cluster
      :on-off (item-boolean->bool (%item-value item))
      :on-change (lambda (cluster on)
                   (declare (ignore cluster))
                   (log:info "Matter bridge: ~a switched ~a" (item:name item) (if on "on" "off"))
                   (item:set-value item (bool->item-boolean on)))))
    (:boolean-state
     (matter:make-boolean-state-cluster
      :state-value (item-contact->state-value (%item-value item))))
    (:temperature-measurement
     (matter:make-temperature-measurement-cluster
      :measured-value (item-temperature->measured-value (%item-value item))))))

(defun %publish-binding (bridge binding)
  "Actor context. Adds the bridged endpoint of `BINDING's item to the bridge's node, and remembers it on the binding.
Returns the endpoint; an item the bridge cannot publish is an error."
  (let* ((item (binding-item binding))
         (name (item:name item))
         (type (%resolve-device-type binding item)))
    (multiple-value-bind (type-id revision value-cluster) (device-type-spec type)
      (let* ((cluster (%make-value-cluster item value-cluster))
             (endpoint (matter:make-bridged-endpoint
                        (%endpoint-id bridge name)
                        :parent +aggregator-endpoint+
                        :node-label (%node-label (or (binding-label binding) (item:label item) name))
                        :unique-id (%unique-id name)
                        :vendor-name "Chipi"
                        :product-name (%node-label (string-capitalize (substitute #\Space #\- (symbol-name type))))
                        :device-types (list (cons type-id revision))
                        :clusters (list (matter:make-identify-cluster
                                         :on-identify (lambda (cluster seconds)
                                                        (declare (ignore cluster))
                                                        (log:info "Matter bridge: identify ~a for ~a seconds" name seconds)))
                                        cluster))))
        (matter:add-endpoint (bridge-node bridge) endpoint)
        (setf (binding-endpoint binding) endpoint
              (binding-cluster binding) cluster
              (binding-value-cluster binding) value-cluster)
        (log:info "Matter bridge: published ~a as ~a on endpoint ~a" name type (matter:endpoint-id endpoint))
        endpoint))))

(defun %unpublish-binding (bridge binding)
  "Actor context. Removes `BINDING's endpoint from the bridge's node; the endpoint id stays reserved for the item's name."
  (when-let ((endpoint (binding-endpoint binding)))
    (matter:remove-endpoint (bridge-node bridge) endpoint)
    (setf (binding-endpoint binding) nil
          (binding-cluster binding) nil)
    (log:info "Matter bridge: withdrew endpoint ~a (~a)"
              (matter:endpoint-id endpoint) (item:name (binding-item binding)))
    t))

(defun %apply-item-value (binding value)
  "Actor context. Sets the attribute of `BINDING's cluster from the item's new `VALUE' -- only when it differs, so a value that came in as a Matter command in the first place changes nothing."
  (when-let ((cluster (binding-cluster binding)))
    (multiple-value-bind (attribute new)
        (ecase (binding-value-cluster binding)
          (:on-off (values :on-off (item-boolean->bool value)))
          (:boolean-state (values :state-value (item-contact->state-value value)))
          (:temperature-measurement (values :measured-value (item-temperature->measured-value value))))
      (unless (equal new (matter:attribute-value cluster attribute))
        (log:debug "Matter bridge: ~a ~a <- ~a" (item:name (binding-item binding)) attribute new)
        (setf (matter:attribute-value cluster attribute) new))
      new)))

(defun %make-push-fun (binding)
  "The push of a bridge binding: the item's value goes to the bridge actor, which puts it on the attribute. Nothing is waited for."
  (lambda (value)
    (let ((bridge *bridge*))
      (if (and bridge (binding-endpoint binding))
          (matter-actor-run (bridge-actor bridge)
                            (lambda () (%apply-item-value binding value)))
          (log:debug "Matter bridge: ~a is not published, value ~a stays home" binding value)))))

(defun %publish-safely (bridge binding)
  "Publishes `BINDING' on `BRIDGE' from outside the actor; an item the bridge cannot publish is logged, not signalled -- one item's mistake must not break the definition of the item or the start of the bridge."
  (handler-case
      (matter-actor-call (bridge-actor bridge)
                         (lambda () (%publish-binding bridge binding)))
    (error (c)
      (log:error "Matter bridge: cannot publish ~a: ~a" binding c)
      nil)))

(defmethod binding:bind-item :after ((binding matter-bridge-binding) item)
  (setf (binding-item binding) item)
  (when *bridge*
    (%publish-safely *bridge* binding)))

(defmethod binding:destroy :before ((binding matter-bridge-binding))
  (%unregister-bridge-binding binding)
  (when-let ((bridge *bridge*))
    (when (binding-endpoint binding)
      (handler-case
          (matter-actor-call (bridge-actor bridge)
                             (lambda () (%unpublish-binding bridge binding)))
        (error (c)
          (log:warn "Matter bridge: cannot withdraw ~a: ~a" binding c))))))

(defun matter-bridge (&rest other-args &key device-type label &allow-other-keys)
  "Creates a `matter-bridge-binding': the item it is given to becomes a bridged device of Chipi's Matter bridge (`matter-init' with `:bridge'), and administrators that commissioned the bridge -- Apple Home, Google Home -- see and control it.

  (deflight light-kitchen \"Kitchen\"
    (knx-binding :ga \"1/2/3\" :dpt \"1.001\")
    (matter-bridge))

`DEVICE-TYPE': what the item is published as, one of `:on-off-light', `:on-off-plug-in-unit', `:contact-sensor', `:temperature-sensor'. Without it the item's house kind decides: `house:deflight' is an On/Off Light, `defplug' an On/Off Plug-in Unit, `defcontact' a Contact Sensor and the temperature of `defheating' a Temperature Sensor. An item with neither is logged and not published.
`LABEL': the name administrators show for the device; default the item's label.
The item's value goes to the device's attribute on every change, and a command from an administrator sets the item -- with push, so a KNX binding on the same item writes it to the bus.
The other keys go to the base binding; `:call-push-p' is T by default."
  (when device-type
    (device-type-spec device-type))
  (let ((binding (apply #'make-instance 'matter-bridge-binding
                        :device-type device-type
                        :label label
                        :call-push-p (getf other-args :call-push-p t)
                        (remove-from-plist other-args :device-type :label :call-push-p))))
    (setf (slot-value binding 'binding::push-fun) (%make-push-fun binding))
    (%register-bridge-binding binding)
    binding))

;; -----------------------------
;; the bridge
;; -----------------------------

(defun %link-local-p (address)
  (and (> (length address) 4) (string-equal "fe80" address :end2 4)))

(defun %ipv6-addresses (interface)
  "This host's IPv6 addresses to advertise, zone identifiers removed: those of `INTERFACE', or of every interface that is up, not the loopback and has a routable address."
  (let ((addresses '()))
    (dolist (spec (cl-matter.net:network-interfaces))
      (let* ((flags (getf spec :flags))
             (all (mapcar (lambda (a) (subseq a 0 (position #\% a))) (getf spec :addresses)))
             (routable (remove-if #'%link-local-p all)))
        (when (and (member :up flags)
                   (not (member :loopback flags))
                   (if interface
                       (string= interface (getf spec :name))
                       routable))
          (dolist (address all)
            (when (find #\: address)
              (pushnew address addresses :test #'string=))))))
    (nreverse addresses)))

(defun %start-advertising (bridge name interface)
  "Puts the bridge on the air over mDNS. A Lisp whose network backend cannot (SBCL today) is logged: the bridge still answers a commissioner that knows its address."
  (handler-case
      (let ((addresses (%ipv6-addresses interface)))
        (matter:start-device-advertising (bridge-device bridge)
                                         :addresses addresses
                                         :interface interface
                                         :device-name name
                                         :device-type matter:+device-type-aggregator+
                                         :pairing-hint 33)
        (setf (bridge-advertising-p bridge) t)
        (log:info "Matter bridge: advertising ~a on ~{~a~^, ~}" name addresses))
    (error (c)
      (log:warn "Matter bridge: not advertised over mDNS (~a); administrators cannot discover it on this Lisp" c))))

(defun %pairing-codes (bridge)
  (let ((payload (matter:device-onboarding-payload (bridge-device bridge) (bridge-passcode bridge))))
    (values (matter:encode-manual-pairing-code payload)
            (matter:encode-qr-code payload))))

(defun %software-version ()
  (or (ignore-errors (asdf:component-version (asdf:find-system "binding-matter")))
      "0"))

(defun start-bridge (storage &key (name "Chipi") (passcode 20202021) (discriminator 3840)
                               (port 5540) interface (advertise t)
                               address (family :ipv6)
                               (vendor-id #xFFF1) (product-id #x8000))
  "Starts Chipi's Matter bridge; `matter-init' does this with its `:bridge' keys.
`STORAGE' is the directory of `bridge.state' (fabrics, ACL, resumption records) and `endpoints' (the endpoint id map).
`NAME' is the bridge's own NodeLabel and the name it advertises under, `PASSCODE' and `DISCRIMINATOR' those of its commissioning window; the window is open from the start while no administrator has commissioned it (cl-matter's own behaviour), and the pairing codes are logged.
`PORT' is the UDP port, 5540 as Matter wants it; `FAMILY' and `ADDRESS' bind it elsewhere (tests: `:family :ipv4 :address \"127.0.0.1\" :port 0').
`ADVERTISE' NIL leaves mDNS alone; `INTERFACE' names the one to advertise on, default every interface with a routable IPv6 address.
`VENDOR-ID' and `PRODUCT-ID' are the test ones of the SDK's example DAC the bridge presents, which is why Apple Home calls it an uncertified accessory.
Items bound to a `matter-bridge' binding by now are published at once, later ones as they are bound."
  (when *bridge*
    (error "The Matter bridge is already running; `matter-shutdown' first"))
  (let* ((transport (matter:make-udp-transport :family family :port port :address address))
         (manager (matter:make-exchange-manager :transport transport))
         (done nil))
    (unwind-protect
         (let* ((aggregator (matter:make-aggregator-endpoint +aggregator-endpoint+))
                (node (matter:make-basic-node :vendor-name "Chipi" :vendor-id vendor-id
                                              :product-name "Chipi bridge" :product-id product-id
                                              :node-label (%node-label name)
                                              :hardware-version-string (%node-label (lisp-implementation-type))
                                              :software-version-string (%software-version)
                                              :endpoints (list aggregator)))
                (channel (matter:make-secure-channel manager))
                (model (matter:make-interaction-model manager :node node))
                (device (matter:make-device
                         :node node :channel channel :model model
                         :credentials (matter:test-attestation-credentials)
                         :discriminator discriminator
                         :passcode passcode :salt *bridge-salt* :iterations *bridge-pase-iterations*
                         :storage (merge-pathnames "bridge.state" storage)
                         :on-commissioned (lambda (device fabric)
                                            (declare (ignore device))
                                            (log:info "Matter bridge: commissioned into fabric ~16,'0x as node ~16,'0x (index ~d)"
                                                      (matter:fabric-fabric-id fabric)
                                                      (matter:fabric-node-id fabric)
                                                      (matter:fabric-index fabric)))
                         :on-fabric-removed (lambda (device fabric)
                                              (declare (ignore device))
                                              (log:info "Matter bridge: fabric ~16,'0x removed"
                                                        (matter:fabric-fabric-id fabric)))))
                (bridge (%make-bridge :manager manager :node node :device device :aggregator aggregator
                                      :storage storage :passcode passcode
                                      :endpoint-map (%read-endpoint-map storage))))
           ;; the bindings that have their item already: nothing polls the
           ;; manager yet, so the node is ours to change here
           (dolist (binding (reverse (%bridge-bindings-snapshot)))
             (when (binding-item binding)
               (handler-case (%publish-binding bridge binding)
                 (error (c)
                   (log:error "Matter bridge: cannot publish ~a: ~a" binding c)))))
           (setf (bridge-actor bridge)
                 (make-matter-actor "matter-bridge" manager
                                    :on-stop (lambda ()
                                               (when (bridge-advertising-p bridge)
                                                 (matter:stop-device-advertising device)))))
           (setf *bridge* bridge
                 done t)
           (when advertise
             (%start-advertising bridge name interface))
           (multiple-value-bind (host local-port) (matter:transport-local-endpoint transport)
             (log:info "Matter bridge ~a on [~a]:~d, ~d fabric~:p, ~d bridged device~:p"
                       name host local-port
                       (length (matter:device-fabrics device))
                       (length (matter:endpoint-parts node aggregator))))
           (multiple-value-bind (manual qr) (%pairing-codes bridge)
             (log:info "Matter bridge: manual pairing code ~a, QR code ~a" manual qr))
           bridge)
      (unless done
        (ignore-errors (matter:exchange-manager-close manager))))))

(defun stop-bridge ()
  "Stops the bridge: no more pushes reach it, its advertisement is withdrawn, its socket closed. The state files stay for the next start."
  (when-let ((bridge *bridge*))
    (setf *bridge* nil)
    (dolist (binding (%bridge-bindings-snapshot))
      (setf (binding-endpoint binding) nil
            (binding-cluster binding) nil))
    (matter-actor-stop (bridge-actor bridge))
    (log:info "Matter bridge stopped")
    t))

(defun matter-bridge-running-p ()
  "Whether Chipi's Matter bridge is running."
  (and *bridge* t))

(defun matter-bridge-pairing-code ()
  "The manual pairing code of Chipi's Matter bridge, 11 digits, and as second value the text of its QR code (`MT:...').
Type the code into Apple Home or Google Home while the bridge's commissioning window is open: it is, from the start, until the first administrator has commissioned the bridge."
  (unless *bridge*
    (error "The Matter bridge is not running"))
  (%pairing-codes *bridge*))
