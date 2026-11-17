(in-package :chipi.binding.matter)

;; The controller role's binding: an item bound to an attribute, a command,
;; or both, of a cluster on an endpoint of a node of Chipi's fabric.
;;
;;   (defplug plug-kitchen "Kitchen plug"
;;     (matter-binding :node "eve-plug-kitchen" :endpoint 1 :cluster :on-off))
;;
;; State comes in by subscription: the controller (matter-controller.lisp)
;; subscribes to the attributes the live bindings of a node bind and hands
;; each reported value to `binding-report', which sets it on the bound items
;; without pushing it back.  A change of the item goes out as a command,
;; `matter-invoke'.  Nothing here waits for the node.

(defvar *matter-bindings* '()
  "Every live `matter-binding'.")

(defvar *matter-bindings-lock* (bt:make-lock "matter-bindings")
  "Guards `*matter-bindings*': bindings are made and destroyed on arbitrary threads, and the controller's actor reads them.")

(defun %clear-matter-bindings ()
  (bt:with-lock-held (*matter-bindings-lock*)
    (setf *matter-bindings* '())))

(defparameter *cluster-binding-defaults*
  '((:on-off :attribute :on-off :command (:off :on))
    (:level-control :attribute :current-level)
    (:boolean-state :attribute :state-value)
    (:temperature-measurement :attribute :measured-value))
  "What a `matter-binding' binds on a cluster when it does not say: (cluster :attribute name :command spec).")

(defclass matter-binding (binding:binding)
  ((node :initarg :node
         :reader matter-binding-node
         :documentation "The node: the label it was commissioned under, or its node id.")
   (endpoint :initarg :endpoint
             :reader matter-binding-endpoint)
   (cluster :initarg :cluster
            :reader matter-binding-cluster
            :documentation "The cluster: a name cl-matter defines, as a keyword, or an id.")
   (attribute :initarg :attribute
              :initform nil
              :reader matter-binding-attribute
              :documentation "The attribute that is the item's state, a name or an id, or NIL for a binding that only sends commands.")
   (command :initarg :command
            :initform nil
            :reader matter-binding-command
            :documentation "What a push does, parsed: NIL, (:invoke name), (:switch off-name on-name) or (:fields name plist).")
   (scale :initarg :scale
          :initform nil
          :reader matter-binding-scale
          :documentation "A rational the attribute's value is multiplied with on the way to the item, or NIL.")
   (path :initarg :path
         :initform nil
         :reader binding-path
         :documentation "The attribute's path in ids, a list (endpoint cluster-id attribute-id), or NIL without attribute."))
  (:documentation "Binds an item to a cluster of a Matter node of Chipi's fabric: an attribute as its state, a command as what a change of it does. Made with `matter-binding'."))

(defmethod print-object ((binding matter-binding) stream)
  (print-unreadable-object (binding stream :type t)
    (format stream "~s ~a/~a~@[/~a~]"
            (matter-binding-node binding) (matter-binding-endpoint binding)
            (matter-binding-cluster binding) (matter-binding-attribute binding))))

(defun node-bindings (nodes entry)
  "The live bindings that bind an attribute of the node of `ENTRY', one of the registry entries `NODES'."
  (let ((bindings (bt:with-lock-held (*matter-bindings-lock*)
                    (copy-list *matter-bindings*))))
    (remove-if-not (lambda (binding)
                     (and (binding-path binding)
                          (eq entry (find-node-entry nodes (matter-binding-node binding)))))
                   bindings)))

;; -----------------------------
;; values
;; -----------------------------

(defun %plain-value (value)
  "`VALUE' as a Lisp value: what a TLV element of an attribute cl-matter does not define holds, when that is a number, a boolean, a string or a null; anything else as it is."
  (if (matter:tlv-element-p value)
      (case (matter:tlv-element-type value)
        (:null :null)
        ((:structure :array :list) value)
        (t (matter:tlv-element-value value)))
      value))

(defun attribute->item-value (value &optional scale)
  "The item value of the attribute `VALUE' as cl-matter gives it: `item:true' and `item:false' for a boolean, a number multiplied with `SCALE' (a fraction that results becomes a float), anything else as it is. Second value NIL for a null, which is no value for an item."
  (let ((value (%plain-value value)))
    (cond ((eq value :null) (values nil nil))
          ((eq value t) (values 'item:true t))
          ((null value) (values 'item:false t))
          ((and scale (realp value))
           (let ((scaled (* value scale)))
             (values (if (integerp scaled) scaled (float scaled 1.0)) t)))
          (t (values value t)))))

(defun %field-value (spec value)
  "The value of a command field: `SPEC' as it is, but `(:value)' is the item's `VALUE' -- a boolean item's as T or NIL -- and `(:scale r)' the item's number multiplied with R, rounded."
  (cond ((equal spec '(:value))
         (cond ((eq value 'item:true) t)
               ((eq value 'item:false) nil)
               (t value)))
        ((and (consp spec) (eq :scale (first spec)))
         (unless (realp value)
           (error "~s is no number to scale by ~a" value (second spec)))
         (round (* value (second spec))))
        (t spec)))

(defun %command-of-value (command value)
  "What the parsed `COMMAND' of a binding makes of the item `VALUE': the command's name and the plist of its fields, or NIL when the value says nothing to it."
  (ecase (first command)
    (:invoke (values (second command) nil))
    (:switch (cond ((eq value 'item:false) (values (second command) nil))
                   ((eq value 'item:true) (values (third command) nil))
                   (t nil)))
    (:fields (values (second command)
                     (loop :for (field spec) :on (third command) :by #'cddr
                           :collect field
                           :collect (%field-value spec value))))))

(defun %parse-command (cluster command)
  "The `:command' of a binding, checked against `CLUSTER' and parsed: a name is (:invoke name), a list of two names (:switch off on), a name followed by fields and their values (:fields name plist)."
  (flet ((name (name)
           (%command-id cluster name)
           name))
    (cond ((null command) nil)
          ((atom command) (list :invoke (name command)))
          ((and (= 2 (length command)) (every #'atom command))
           (list :switch (name (first command)) (name (second command))))
          ((oddp (length command))
           (loop :for (nil spec) :on (rest command) :by #'cddr
                 :do (when (and (consp spec) (eq :scale (first spec)))
                       (unless (and (= 2 (length spec)) (rationalp (second spec)))
                         (error "A field is scaled by a rational, (:scale r), not: ~s" spec))))
           (list :fields (name (first command)) (rest command)))
          (t
           (error ":command is a command, a pair (off-command on-command), or a command followed by fields and their values, not: ~s"
                  command)))))

;; -----------------------------
;; in and out
;; -----------------------------

(defun binding-report (binding value)
  "Sets the attribute `VALUE' a node reported on the items of `BINDING', without pushing it back to the node. A null changes nothing. The controller calls this, in its actor."
  (multiple-value-bind (item-value value-p)
      (attribute->item-value value (matter-binding-scale binding))
    (when value-p
      (let* ((transform (slot-value binding 'binding::transform-fun))
             (item-value (if transform (funcall transform item-value) item-value)))
        (log:debug "Matter: ~a reports ~a" binding item-value)
        (dolist (item (binding::bound-items binding))
          (item:set-value item item-value :push nil))))))

(defun %make-binding-pull-fun (binding)
  "The pull of `BINDING': a read of its attribute, as a future. For `:initial-delay' and `:delay' -- a node that takes subscriptions needs neither."
  (lambda ()
    (destructuring-bind (endpoint cluster attribute) (binding-path binding)
      (values (future:fmap
                  (matter-read (matter-binding-node binding) endpoint cluster attribute)
                  (value)
                (multiple-value-bind (item-value value-p)
                    (attribute->item-value value (matter-binding-scale binding))
                  (unless value-p
                    (error "~a has no value" binding))
                  item-value))
              '(:push nil)))))

(defun %make-binding-push-fun (binding)
  "The push of `BINDING': the item's value as a command to the node. Nothing waits for the answer; a command the node does not take is logged."
  (lambda (value)
    (handler-case
        (multiple-value-bind (name fields)
            (%command-of-value (matter-binding-command binding) value)
          (if (null name)
              (log:warn "Matter: ~a has no command for the value ~s" binding value)
              (future:frecover
               (matter-invoke (matter-binding-node binding) (matter-binding-endpoint binding)
                              (matter-binding-cluster binding) name fields)
               (error (c)
                      (log:warn "Matter: ~a, command ~a for ~s: ~a" binding name value c)))))
      (error (c)
        (log:warn "Matter: ~a, value ~s not sent: ~a" binding value c)))))

(defmethod binding:destroy :before ((binding matter-binding))
  (bt:with-lock-held (*matter-bindings-lock*)
    (setf *matter-bindings* (remove binding *matter-bindings*)))
  (when (binding-path binding)
    (%bindings-changed (matter-binding-node binding))))

;; -----------------------------
;; the DSL
;; -----------------------------

(defun make-matter-binding (&rest other-args
                            &key node endpoint cluster (attribute nil attribute-p) (command nil command-p) scale
                            &allow-other-keys)
  "Creates a `matter-binding', as the macro `matter-binding' does, `COMMAND' evaluated.

`NODE': the label the node was commissioned under (recommended: it survives a new commissioning) or its node id. The node need not be commissioned yet, nor the controller running: the binding takes effect when they are.
`ENDPOINT': the endpoint id.
`CLUSTER': the cluster, a name cl-matter defines as a keyword (`:on-off') or an id.
`ATTRIBUTE': the attribute that is the item's state, a name of the cluster's definition or an id. The controller subscribes to it; what the node reports is set on the item and not pushed back. Default per cluster: `:on-off' for `:on-off', `:current-level' for `:level-control', `:state-value' for `:boolean-state', `:measured-value' for `:temperature-measurement'; none otherwise, and none with `:attribute nil'.
`COMMAND': what a change of the item sends to the node:
  a command name -- `:stop-motion' --, sent whatever the value;
  a pair -- `(:off :on)' --, the first for `item:false', the second for `item:true';
  a command and its fields -- `(:move-to-level :level (:scale 254/100) :transition-time 0)' --, where the field value `(:scale r)' is the item's number multiplied with R and rounded, `(:value)' the item's value as it is, and anything else itself.
Default `(:off :on)' for `:on-off', none otherwise; without a command the item is read only.
`SCALE': a rational the attribute's value is multiplied with on the way to the item: `1/100' for the hundredths Matter measures in. A fraction that results is a float; `:transform #'round' makes a whole number of it.
A boolean attribute is `item:true' / `item:false' on the item, a null leaves the item as it is.
The other keys go to the base binding: `:transform', a function of the converted value; `:initial-delay' and `:delay', which read the attribute once or again and again, for a node that takes no subscriptions; `:call-push-p', T by default when there is a command."
  (unless (or (integerp node) (and (stringp node) (plusp (length node))))
    (error ":node is the label of a node, a string, or its node id, not: ~s" node))
  (unless (typep endpoint '(integer 0 #xFFFF))
    (error ":endpoint is an endpoint id, not: ~s" endpoint))
  (when (and scale (not (rationalp scale)))
    (error ":scale is a rational, not: ~s" scale))
  (%cluster-id cluster)
  (let* ((defaults (cdr (assoc cluster *cluster-binding-defaults*)))
         (attribute (if attribute-p attribute (getf defaults :attribute)))
         (command (%parse-command cluster (if command-p command (getf defaults :command))))
         (path (and attribute
                    (list endpoint (%cluster-id cluster) (%attribute-id cluster attribute)))))
    (unless (or path command)
      (error "A matter-binding on ~s needs an :attribute or a :command" cluster))
    (let ((binding (apply #'make-instance 'matter-binding
                          :node node :endpoint endpoint :cluster cluster
                          :attribute attribute :command command :scale scale :path path
                          :transform-fun (getf other-args :transform)
                          :call-push-p (getf other-args :call-push-p (and command t))
                          (remove-from-plist other-args :node :endpoint :cluster :attribute :command :scale
                                                        :transform :call-push-p))))
      (when path
        (setf (slot-value binding 'binding::pull-fun) (%make-binding-pull-fun binding)))
      (when command
        (setf (slot-value binding 'binding::push-fun) (%make-binding-push-fun binding)))
      (bt:with-lock-held (*matter-bindings-lock*)
        (push binding *matter-bindings*))
      (when path
        (%bindings-changed node))
      binding)))

(defmacro matter-binding (&rest args &key command &allow-other-keys)
  "Creates a `matter-binding': the item it is given to is an attribute, a command, or both, of a Matter node of Chipi's fabric (`matter-init' with `:controller').

  (defplug plug-kitchen \"Kitchen plug\"
    (matter-binding :node \"eve-plug-kitchen\" :endpoint 1 :cluster :on-off))

  (defitem 'temp-bath \"Bath temperature\" 'float
    (matter-binding :node \"aqara-bath\" :endpoint 1 :cluster :temperature-measurement :scale 1/100))

  (defitem 'light-living-level \"Living room level\" 'integer
    (matter-binding :node \"hue-living\" :endpoint 1 :cluster :level-control
                    :command (:move-to-level-with-on-off :level (:scale 254/100) :transition-time 0)
                    :scale 100/254 :transform #'round))

A `COMMAND' written as a list is taken as it stands, not evaluated; the other arguments are evaluated and are those of `make-matter-binding', which is the same as a function."
  (if (and (consp command) (or (keywordp (first command)) (integerp (first command))))
      (let ((position (position :command args)))
        `(make-matter-binding ,@(subseq args 0 position) :command ',command ,@(subseq args (+ 2 position))))
      `(make-matter-binding ,@args)))
