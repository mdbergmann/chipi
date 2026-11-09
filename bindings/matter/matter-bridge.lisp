(in-package :chipi.binding.matter)

;; Chipi as a Matter bridge.
;;
;; One cl-matter device on the bridge actor: endpoint 0 is the root,
;; endpoint 1 the Aggregator, and from endpoint 2 on there is one Bridged
;; Node per bridged device: an item that carries a `matter-bridge' binding,
;; or the items whose bindings name the same `:device' (the channels of a
;; shutter, the temperature and setpoint of a heating).  Each bridged
;; endpoint has the device type of the items' house kinds (a light, a plug,
;; a contact, a temperature, a window covering, a thermostat), Identify,
;; Bridged Device Basic Information naming it, and the one cluster that
;; carries the device's values.
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
ENDPOINT-MAP (an alist of bridged device name to endpoint id), the
`bridged-device's -- DEVICES -- that the bindings make up, and whether it
is ADVERTISING-P over mDNS."
  actor manager node device aggregator
  storage
  passcode
  (endpoint-map '())
  (devices '())
  advertising-p)

(defmethod print-object ((bridge bridge) stream)
  (print-unreadable-object (bridge stream :type t)
    (format stream "~d bridged device~:p, ~d fabric~:p~:[~;, advertising~]"
            (length (bridge-devices bridge))
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
;; the bridged devices
;; -----------------------------

(defstruct (bridged-device (:constructor %make-bridged-device))
  "One bridged device: its NAME (the item's, or the `:device' several items
share), the BINDINGS of the items that make it up, in the order they came,
and -- while it is published -- its ENDPOINT, the CLUSTER carrying its
values and that cluster's kind, VALUE-CLUSTER (:on-off, :boolean-state,
:temperature-measurement, :window-covering or :thermostat)."
  name
  (bindings '())
  endpoint cluster value-cluster)

(defmethod print-object ((device bridged-device) stream)
  (print-unreadable-object (device stream :type t)
    (format stream "~a, ~d item~:p~@[, endpoint ~a~]"
            (bridged-device-name device)
            (length (bridged-device-bindings device))
            (and (bridged-device-endpoint device) (matter:endpoint-id (bridged-device-endpoint device))))))

;; -----------------------------
;; the binding
;; -----------------------------

(defclass matter-bridge-binding (binding:binding)
  ((device-type :initarg :device-type
                :initform nil
                :reader binding-device-type
                :documentation "The name of the device type to publish the device as (see `*bridged-device-types*'), or NIL to take it from the house kinds of its items.")
   (device-name :initarg :device
                :initform nil
                :reader binding-device-name
                :documentation "The symbol naming the device several items make up, or NIL for an item that is a device by itself.")
   (label :initarg :label
          :initform nil
          :reader binding-label
          :documentation "The NodeLabel administrators show for the device, or NIL for the items' label.")
   (item :initform nil
         :accessor binding-item
         :documentation "The item, once `bind-item' gave it.")
   (kind :initform nil
         :accessor binding-kind
         :documentation "The item's house kind, once `bind-item' gave the item.")
   (device :initform nil
           :accessor binding-device
           :documentation "The `bridged-device' the item is part of while the bridge runs.")))

(defun binding-endpoint (binding)
  "The bridged endpoint of `BINDING's device while it is published, or NIL."
  (when-let ((device (binding-device binding)))
    (bridged-device-endpoint device)))

(defmethod print-object ((binding matter-bridge-binding) stream)
  (print-unreadable-object (binding stream :type t)
    (format stream "item: ~a, device: ~a, device-type: ~a, endpoint: ~a"
            (and (binding-item binding) (item:name (binding-item binding)))
            (binding-device-name binding)
            (binding-device-type binding)
            (and (binding-endpoint binding) (matter:endpoint-id (binding-endpoint binding))))))

(defun %house-kind (item)
  (cdr (assoc :house-kind (item:tags item))))

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

(defun %common-label (labels)
  "The words `LABELS' begin with in common -- \"Bath\" for \"Bath Temperature\" and \"Bath Setpoint\" -- or the first label when they share none."
  (flet ((words (label) (uiop:split-string label :separator " ")))
    (let ((common (reduce (lambda (common label)
                            (loop :for a :in common
                                  :for b :in (words label)
                                  :while (string= a b)
                                  :collect a))
                          (rest labels)
                          :initial-value (words (first labels)))))
      (if common
          (format nil "~{~a~^ ~}" common)
          (first labels)))))

(defun %device-label (device)
  "The NodeLabel of `DEVICE': the `:label' a binding gives, else the items' labels -- the one label of one item, what the labels of several begin with in common."
  (%node-label
   (or (some #'binding-label (bridged-device-bindings device))
       (%common-label (mapcar (lambda (binding)
                                (let ((item (binding-item binding)))
                                  (or (item:label item) (item:name item))))
                              (bridged-device-bindings device))))))

(defun %resolve-device-type (device)
  "The device type `DEVICE' is published as: the one its bindings name (they must agree), else the one of its items' house kinds; an error when there is neither."
  (let ((named (remove-duplicates (remove nil (mapcar #'binding-device-type (bridged-device-bindings device)))))
        (kinds (mapcar #'binding-kind (bridged-device-bindings device))))
    (cond
      ((rest named)
       (error "The items of device ~a name different device types: ~{~s~^, ~}" (bridged-device-name device) named))
      (named (first named))
      ((device-type-of-kinds (remove nil kinds)))
      (t (error "Device ~a has no house kind~p the Matter bridge publishes (~{~s~^, ~}) and no matter-bridge binding of its names a :device-type"
                (bridged-device-name device) (length kinds) kinds)))))

(defun %check-members (device type value-cluster)
  "An error unless `DEVICE's items fit the device `TYPE': one item for a type of one item; for a composed type items of its kinds, one per kind."
  (let ((bindings (bridged-device-bindings device))
        (kinds (composed-device-kinds type)))
    (cond
      ((null kinds)
       (when (rest bindings)
         (error "Device ~a is ~a, a device of one item, but has ~d items"
                (bridged-device-name device) type (length bindings))))
      (t
       (loop :for (binding . others) :on bindings
             :do (unless (member (binding-kind binding) kinds)
                   (error "Item ~a of kind ~s has no place in ~a, a ~a of ~{~s~^, ~}"
                          (item:name (binding-item binding)) (binding-kind binding)
                          (bridged-device-name device) type kinds))
                 (when (find (binding-kind binding) others :key #'binding-kind)
                   (error "Device ~a has two items of kind ~s" (bridged-device-name device) (binding-kind binding))))
       (when (and (eq value-cluster :window-covering)
                  (not (or (%member device :shutter-move) (%member device :shutter-position))))
         (error "Device ~a, a window covering, has neither a move nor a position item" (bridged-device-name device)))
       (when (and (eq value-cluster :thermostat)
                  (not (%member device :setpoint)))
         (error "Device ~a, a thermostat, has no setpoint item" (bridged-device-name device)))))))

(defun %member (device kind)
  "The binding of `DEVICE's item of house `KIND', or NIL."
  (find kind (bridged-device-bindings device) :key #'binding-kind))

(defun %member-value (device kind)
  (when-let ((binding (%member device kind)))
    (%item-value (binding-item binding))))

(defun %set-member (device kind value &key (push t))
  "Sets `DEVICE's item of `KIND' to `VALUE' with `PUSH' (T, or :always for a command), so the item's other bindings carry it out."
  (when-let ((binding (%member device kind)))
    (item:set-value (binding-item binding) value :push push)
    t))

(defun %move-shutter (device direction)
  "Actor context, from a Window Covering command: moves the shutter `DEVICE' in `DIRECTION' (:open, :close, :stop) through its move and stop items, or through its position item when it has no move item."
  (log:info "Matter bridge: ~a ~a" direction (bridged-device-name device))
  (ecase direction
    (:open (or (%set-member device :shutter-move 'item:false :push :always)
               (%set-member device :shutter-position 0 :push :always)))
    (:close (or (%set-member device :shutter-move 'item:true :push :always)
                (%set-member device :shutter-position 100 :push :always)))
    (:stop (or (%set-member device :shutter-stop 'item:true :push :always)
               (log:warn "Matter bridge: ~a has no stop item, the stop is dropped" (bridged-device-name device))))))

(defun %make-value-cluster (device value-cluster)
  "The cluster of kind `VALUE-CLUSTER' carrying `DEVICE's values, initialised from its items. A command on it sets an item -- with push, so the item's other bindings carry it out."
  (let ((name (bridged-device-name device)))
    (flet ((only-value () (%item-value (binding-item (first (bridged-device-bindings device))))))
      (ecase value-cluster
        (:on-off
         (matter:make-on-off-cluster
          :on-off (item-boolean->bool (only-value))
          :on-change (lambda (cluster on)
                       (declare (ignore cluster))
                       (log:info "Matter bridge: ~a switched ~a" name (if on "on" "off"))
                       (item:set-value (binding-item (first (bridged-device-bindings device)))
                                       (bool->item-boolean on)))))
        (:boolean-state
         (matter:make-boolean-state-cluster
          :state-value (item-contact->state-value (only-value))))
        (:temperature-measurement
         (matter:make-temperature-measurement-cluster
          :measured-value (item-temperature->measured-value (only-value))))
        (:window-covering
         (let ((position (%member device :shutter-position))
               (slat (%member device :shutter-slat)))
           (matter:make-window-covering-cluster
            :features (append '(:lift)
                              (when position '(:position-aware-lift))
                              (when slat '(:tilt :position-aware-tilt)))
            :current-lift (item-position->percent100ths (%member-value device :shutter-position))
            :current-tilt (item-position->percent100ths (%member-value device :shutter-slat))
            :on-move (lambda (cluster direction)
                       (declare (ignore cluster))
                       (%move-shutter device direction))
            :on-lift (lambda (cluster percent100ths)
                       (declare (ignore cluster))
                       (log:info "Matter bridge: ~a to ~d %" name (percent100ths->item-position percent100ths))
                       (%set-member device :shutter-position (percent100ths->item-position percent100ths)
                                    :push :always))
            :on-tilt (lambda (cluster percent100ths)
                       (declare (ignore cluster))
                       (log:info "Matter bridge: ~a slats to ~d %" name (percent100ths->item-position percent100ths))
                       (%set-member device :shutter-slat (percent100ths->item-position percent100ths)
                                    :push :always)))))
        (:thermostat
         (matter:make-thermostat-cluster
          :local-temperature (item-temperature->measured-value (%member-value device :temperature))
          :occupied-heating-setpoint (item-setpoint->heating-setpoint (%member-value device :setpoint))
          :min-heat-setpoint-limit (* 100 (car *heating-setpoint-limits*))
          :max-heat-setpoint-limit (* 100 (cdr *heating-setpoint-limits*))
          :on-setpoint-change (lambda (cluster hundredths)
                                (declare (ignore cluster))
                                (log:info "Matter bridge: ~a setpoint ~a" name (heating-setpoint->item-setpoint hundredths))
                                (%set-member device :setpoint (heating-setpoint->item-setpoint hundredths)))
          :on-system-mode-change (lambda (cluster mode)
                                   (declare (ignore cluster))
                                   ;; a `house:defheating' has no mode item: noted, not acted on
                                   (log:info "Matter bridge: ~a system mode ~a (no item takes it)" name mode))))))))

(defun %publish-device (bridge device)
  "Actor context. Adds the bridged endpoint of `DEVICE' to the bridge's node and remembers it on the device.
Returns the endpoint; a device the bridge cannot publish is an error."
  (let* ((name (bridged-device-name device))
         (type (%resolve-device-type device)))
    (multiple-value-bind (type-id revision value-cluster) (device-type-spec type)
      (%check-members device type value-cluster)
      (let* ((cluster (%make-value-cluster device value-cluster))
             (endpoint (matter:make-bridged-endpoint
                        (%endpoint-id bridge name)
                        :parent +aggregator-endpoint+
                        :node-label (%device-label device)
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
        (setf (bridged-device-endpoint device) endpoint
              (bridged-device-cluster device) cluster
              (bridged-device-value-cluster device) value-cluster)
        (log:info "Matter bridge: published ~a as ~a on endpoint ~a (~{~a~^, ~})"
                  name type (matter:endpoint-id endpoint)
                  (mapcar (lambda (b) (item:name (binding-item b))) (bridged-device-bindings device)))
        endpoint))))

(defun %unpublish-device (bridge device)
  "Actor context. Removes `DEVICE's endpoint from the bridge's node; the endpoint id stays reserved for its name."
  (when-let ((endpoint (bridged-device-endpoint device)))
    (matter:remove-endpoint (bridge-node bridge) endpoint)
    (setf (bridged-device-endpoint device) nil
          (bridged-device-cluster device) nil
          (bridged-device-value-cluster device) nil)
    (log:info "Matter bridge: withdrew endpoint ~a (~a)" (matter:endpoint-id endpoint) (bridged-device-name device))
    t))

(defun %republish-device (bridge device)
  "Actor context. Publishes `DEVICE' afresh, as its items are now."
  (%unpublish-device bridge device)
  (%publish-device bridge device))

(defun %device-name (binding)
  "The name of the device `BINDING's item belongs to: its `:device', else the item's own name."
  (if (binding-device-name binding)
      (symbol-name (binding-device-name binding))
      (item:name (binding-item binding))))

(defun %find-device (bridge name)
  (find name (bridge-devices bridge) :key #'bridged-device-name :test #'string=))

(defun %add-member (bridge binding)
  "Actor context. Makes `BINDING's item a part of its device on `BRIDGE', creating the device when it is the first; returns the device, not yet (re)published."
  (let* ((name (%device-name binding))
         (device (or (%find-device bridge name)
                     (let ((device (%make-bridged-device :name name)))
                       (push device (bridge-devices bridge))
                       device))))
    (setf (bridged-device-bindings device)
          (append (remove binding (bridged-device-bindings device)) (list binding))
          (binding-device binding) device)
    device))

(defun %join-device (bridge binding)
  "Actor context. `BINDING's item joins its device, which is published afresh with it."
  (%republish-device bridge (%add-member bridge binding)))

(defun %leave-device (bridge binding)
  "Actor context. `BINDING's item leaves its device: the device is published afresh without it, or withdrawn when it was the last."
  (when-let ((device (binding-device binding)))
    (setf (bridged-device-bindings device) (remove binding (bridged-device-bindings device))
          (binding-device binding) nil)
    (cond
      ((bridged-device-bindings device)
       (%republish-device bridge device))
      (t
       (%unpublish-device bridge device)
       (setf (bridge-devices bridge) (remove device (bridge-devices bridge)))))
    t))

(defun %set-attribute (cluster attribute new)
  "Actor context. Sets `ATTRIBUTE' of `CLUSTER' to `NEW' only when it differs, so a value that came in as a Matter command in the first place changes nothing; returns `NEW'."
  (unless (equal new (matter:attribute-value cluster attribute))
    (log:debug "Matter bridge: ~a ~a <- ~a" cluster attribute new)
    (setf (matter:attribute-value cluster attribute) new))
  new)

(defun %moving-toward-p (old new target)
  "Is a position that went from `OLD' to `NEW' on its way to `TARGET'?"
  (and (integerp old) (integerp target)
       (/= old new)
       (<= (min old target) new (max old target))))

(defun %apply-shutter-value (cluster kind value)
  "Actor context. A shutter item of `KIND' changed to `VALUE': the position becomes the current lift, and the target follows it unless the shutter is on its way to the target an administrator or a move command set; a move command sets the target to the end it moves to, a stop to where the shutter is."
  (flet ((lift-aware-p () (matter:attribute-present-p cluster :target-position-lift-percent100ths)))
    (ecase kind
      (:shutter-position
       (let ((new (item-position->percent100ths value)))
         (when (lift-aware-p)
           (let ((old (matter:attribute-value cluster :current-position-lift-percent100ths))
                 (target (matter:attribute-value cluster :target-position-lift-percent100ths)))
             (%set-attribute cluster :current-position-lift-percent100ths new)
             (unless (%moving-toward-p old new target)
               (%set-attribute cluster :target-position-lift-percent100ths new))))
         new))
      (:shutter-slat
       (let ((new (item-position->percent100ths value)))
         (when (matter:attribute-present-p cluster :target-position-tilt-percent100ths)
           (%set-attribute cluster :current-position-tilt-percent100ths new)
           (%set-attribute cluster :target-position-tilt-percent100ths new))
         new))
      (:shutter-move
       (when (lift-aware-p)
         (%set-attribute cluster :target-position-lift-percent100ths (if (item-boolean->bool value) 10000 0))))
      (:shutter-stop
       (when (and (lift-aware-p) (item-boolean->bool value))
         (%set-attribute cluster :target-position-lift-percent100ths
                         (matter:attribute-value cluster :current-position-lift-percent100ths)))))))

(defun %apply-item-value (binding value)
  "Actor context. Puts the new `VALUE' of `BINDING's item on its device's cluster -- which attribute depends on the cluster and, in a composed device, on the item's kind."
  (when-let* ((device (binding-device binding))
              (cluster (bridged-device-cluster device)))
    (ecase (bridged-device-value-cluster device)
      (:on-off (%set-attribute cluster :on-off (item-boolean->bool value)))
      (:boolean-state (%set-attribute cluster :state-value (item-contact->state-value value)))
      (:temperature-measurement (%set-attribute cluster :measured-value (item-temperature->measured-value value)))
      (:window-covering (%apply-shutter-value cluster (binding-kind binding) value))
      (:thermostat
       (ecase (binding-kind binding)
         (:temperature (%set-attribute cluster :local-temperature (item-temperature->measured-value value)))
         (:setpoint (%set-attribute cluster :occupied-heating-setpoint (item-setpoint->heating-setpoint value))))))))

(defun %make-push-fun (binding)
  "The push of a bridge binding: the item's value goes to the bridge actor, which puts it on the attribute. Nothing is waited for."
  (lambda (value)
    (let ((bridge *bridge*))
      (if (and bridge (binding-device binding))
          (matter-actor-run (bridge-actor bridge)
                            (lambda () (%apply-item-value binding value)))
          (log:debug "Matter bridge: ~a is not published, value ~a stays home" binding value)))))

(defun %publish-safely (bridge binding)
  "Lets `BINDING's item join its device on `BRIDGE' from outside the actor; a device the bridge cannot publish is logged, not signalled -- one item's mistake must not break the definition of the item or the start of the bridge."
  (handler-case
      (matter-actor-call (bridge-actor bridge)
                         (lambda () (%join-device bridge binding)))
    (error (c)
      (log:error "Matter bridge: cannot publish ~a: ~a" binding c)
      nil)))

(defmethod binding:bind-item :after ((binding matter-bridge-binding) item)
  (setf (binding-item binding) item
        (binding-kind binding) (%house-kind item))
  (when *bridge*
    (%publish-safely *bridge* binding)))

(defmethod binding:destroy :before ((binding matter-bridge-binding))
  (%unregister-bridge-binding binding)
  (when-let ((bridge *bridge*))
    (when (binding-device binding)
      (handler-case
          (matter-actor-call (bridge-actor bridge)
                             (lambda () (%leave-device bridge binding)))
        (error (c)
          (log:warn "Matter bridge: cannot withdraw ~a: ~a" binding c))))))

(defmacro matter-bridge (&rest args &key device &allow-other-keys)
  "Creates a `matter-bridge-binding': the item it is given to becomes a bridged device of Chipi's Matter bridge (`matter-init' with `:bridge'), or a part of one, and administrators that commissioned the bridge -- Apple Home, Google Home -- see and control it.

  (deflight light-kitchen \"Kitchen\"
    (knx-binding :ga \"1/2/3\" :dpt \"1.001\")
    (matter-bridge))

  (defrollershutter blind-living \"Blind living room\"
    :move     ((knx-binding ...) (matter-bridge :device blind-living))
    :stop     ((knx-binding ...) (matter-bridge :device blind-living))
    :position ((knx-binding ...) (matter-bridge :device blind-living)))

`DEVICE' names, unevaluated like the name in a definer, the device several items make up; the other arguments are evaluated and are those of `make-matter-bridge-binding', which is the same as a function."
  (if (and device (symbolp device) (not (keywordp device)))
      (let ((position (position :device args)))
        `(make-matter-bridge-binding ,@(subseq args 0 position) :device ',device ,@(subseq args (+ 2 position))))
      `(make-matter-bridge-binding ,@args)))

(defun make-matter-bridge-binding (&rest other-args &key device device-type label &allow-other-keys)
  "Creates a `matter-bridge-binding', as `matter-bridge' does, `DEVICE' evaluated.

`DEVICE': a symbol naming the device several items make up; the items whose bindings name it are one bridged device, by their house kinds: the channels of a `house:defrollershutter' a Window Covering, the temperature and setpoint of a `house:defheating' a Thermostat. Without it the item is a device by itself.
`DEVICE-TYPE': what the device is published as, one of `:on-off-light', `:on-off-plug-in-unit', `:contact-sensor', `:temperature-sensor', `:window-covering', `:thermostat'. Without it the house kinds decide: `house:deflight' is an On/Off Light, `defplug' an On/Off Plug-in Unit, `defcontact' a Contact Sensor and a temperature on its own a Temperature Sensor. A device with neither is logged and not published.
`LABEL': the name administrators show for the device; default the item's label, or what the labels of a device's items begin with in common.
The item's value goes to the device's attribute on every change, and a command from an administrator sets the item -- with push, so a KNX binding on the same item writes it to the bus.
The other keys go to the base binding; `:call-push-p' is T by default."
  (check-type device symbol)
  (when device-type
    (device-type-spec device-type))
  (let ((binding (apply #'make-instance 'matter-bridge-binding
                        :device device
                        :device-type device-type
                        :label label
                        :call-push-p (getf other-args :call-push-p t)
                        (remove-from-plist other-args :device :device-type :label :call-push-p))))
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
Items bound to a `matter-bridge' binding by now are published at once, later ones as they are bound; a device of several items is published afresh as each of them joins."
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
           ;; the bindings that have their item already make up the devices,
           ;; each published once with all its items: nothing polls the
           ;; manager yet, so the node is ours to change here
           (dolist (binding (reverse (%bridge-bindings-snapshot)))
             (when (binding-item binding)
               (%add-member bridge binding)))
           (dolist (device (reverse (bridge-devices bridge)))
             (handler-case (%publish-device bridge device)
               (error (c)
                 (log:error "Matter bridge: cannot publish ~a: ~a" device c))))
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
      (setf (binding-device binding) nil))
    (setf (bridge-devices bridge) '())
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
