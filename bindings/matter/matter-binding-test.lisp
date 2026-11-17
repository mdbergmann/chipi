(in-package :chipi.binding.matter-controller-test)

(def-suite matter-binding-tests
  :description "Matter binding: the controller role's `matter-binding' without a node -- keys, values, commands"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-binding-tests)

(defun %live-bindings ()
  binding-matter::*matter-bindings*)

(defun %item-value (id)
  (hab:get-item-valueq id))

;; -----------------------------
;; the keys
;; -----------------------------

(test make-matter-binding--defaults-of-the-on-off-cluster
  (with-fixture controller-env (:init nil)
    (let ((binding (matter-binding :node "plug" :endpoint 1 :cluster :on-off)))
      (is (string= "plug" (binding-matter::matter-binding-node binding)))
      (is (= 1 (binding-matter::matter-binding-endpoint binding)))
      (is (eq :on-off (binding-matter::matter-binding-attribute binding)))
      (is (equal '(:switch :off :on) (binding-matter::matter-binding-command binding)))
      (is (equal (list 1 matter:+cluster-on-off+ 0) (binding-matter::binding-path binding)))
      (is-true (binding:call-push-p binding))
      (is-true (binding::pull-fun binding))
      (is-true (binding::push-fun binding))
      ;; no read unless asked for: the subscription brings the state
      (is (null (slot-value binding 'binding::initial-delay)))
      (is (equal (list binding) (%live-bindings))))))

(test make-matter-binding--other-clusters
  (with-fixture controller-env (:init nil)
    ;; an attribute by default, and no command: read only
    (let ((binding (matter-binding :node 2 :endpoint 3 :cluster :temperature-measurement :scale 1/100)))
      (is (eq :measured-value (binding-matter::matter-binding-attribute binding)))
      (is (null (binding-matter::matter-binding-command binding)))
      (is (equal (list 3 matter:+cluster-temperature-measurement+ 0) (binding-matter::binding-path binding)))
      (is-false (binding:call-push-p binding))
      (is (null (binding::push-fun binding))))
    (is (eq :state-value (binding-matter::matter-binding-attribute
                          (matter-binding :node "c" :endpoint 1 :cluster :boolean-state))))
    (is (eq :current-level (binding-matter::matter-binding-attribute
                            (matter-binding :node "l" :endpoint 1 :cluster :level-control))))
    ;; a command alone: nothing to subscribe to, nothing to pull
    (let ((binding (matter-binding :node "blind" :endpoint 1 :cluster :window-covering :command :stop-motion)))
      (is (null (binding-matter::matter-binding-attribute binding)))
      (is (null (binding-matter::binding-path binding)))
      (is (null (binding::pull-fun binding)))
      (is (equal '(:invoke :stop-motion) (binding-matter::matter-binding-command binding)))
      (is-true (binding:call-push-p binding)))
    ;; the default attribute switched off, the default command replaced
    (let ((binding (matter-binding :node "plug" :endpoint 1 :cluster :on-off :attribute nil :command :toggle)))
      (is (null (binding-matter::binding-path binding)))
      (is (equal '(:invoke :toggle) (binding-matter::matter-binding-command binding))))
    (let ((binding (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command nil)))
      (is (null (binding-matter::matter-binding-command binding)))
      (is-false (binding:call-push-p binding)))
    ;; ids in place of names
    (let ((binding (matter-binding :node 7 :endpoint 1 :cluster #x0090 :attribute 8 :scale 1/1000)))
      (is (equal '(1 #x0090 8) (binding-matter::binding-path binding))))
    ;; the base binding's keys pass through
    (let ((binding (matter-binding :node "plug" :endpoint 1 :cluster :on-off
                                   :call-push-p nil :initial-delay 5 :delay 60
                                   :transform (lambda (value) value))))
      (is-false (binding:call-push-p binding))
      (is (= 5 (slot-value binding 'binding::initial-delay)))
      (is (= 60 (slot-value binding 'binding::delay)))
      (is-true (slot-value binding 'binding::transform-fun)))))

(test make-matter-binding--checks-its-keys
  (with-fixture controller-env (:init nil)
    (signals error (matter-binding :endpoint 1 :cluster :on-off))
    (signals error (matter-binding :node "" :endpoint 1 :cluster :on-off))
    (signals error (matter-binding :node :plug :endpoint 1 :cluster :on-off))
    (signals error (matter-binding :node "plug" :cluster :on-off))
    (signals error (matter-binding :node "plug" :endpoint -1 :cluster :on-off))
    (signals error (matter-binding :node "plug" :endpoint 1))
    (signals error (matter-binding :node "plug" :endpoint 1 :cluster :no-such-cluster))
    (signals error (matter-binding :node "plug" :endpoint 1 :cluster :on-off :attribute :no-such-attribute))
    (signals error (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command :no-such-command))
    (signals error (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command (:off :no-such-command)))
    (signals error (matter-binding :node "plug" :endpoint 1 :cluster :on-off :scale 0.01))
    ;; neither state nor command
    (signals error (matter-binding :node "blind" :endpoint 1 :cluster :window-covering))
    (signals error (matter-binding :node "x" :endpoint 1 :cluster #x0090))
    ;; a field scaled by something that is no rational, fields without values
    (signals error (matter-binding :node "l" :endpoint 1 :cluster :level-control
                                   :command (:move-to-level :level (:scale 2.54))))
    (signals error (matter-binding :node "l" :endpoint 1 :cluster :level-control
                                   :command (:move-to-level :level (:value) :transition-time)))
    (is (null (%live-bindings)))))

(test matter-binding--a-command-list-is-not-evaluated
  (with-fixture controller-env (:init nil)
    (is (equal '(:switch :off :on)
               (binding-matter::matter-binding-command
                (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command (:off :on)))))
    ;; quoted, in a variable, and through the function: evaluated
    (is (equal '(:switch :off :toggle)
               (binding-matter::matter-binding-command
                (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command '(:off :toggle)))))
    (let ((command :toggle))
      (is (equal '(:invoke :toggle)
                 (binding-matter::matter-binding-command
                  (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command command)))))
    (is (equal '(:fields :move-to-level (:level (:scale 254/100) :transition-time 0))
               (binding-matter::matter-binding-command
                (make-matter-binding :node "l" :endpoint 1 :cluster :level-control
                                     :command '(:move-to-level :level (:scale 254/100) :transition-time 0)))))
    ;; ids for commands
    (is (equal '(:switch 0 1)
               (binding-matter::matter-binding-command
                (matter-binding :node "plug" :endpoint 1 :cluster :on-off :command (0 1)))))))

;; -----------------------------
;; values
;; -----------------------------

(defun %item-value-of (value &optional scale)
  (multiple-value-list (binding-matter::attribute->item-value value scale)))

(test attribute->item-value--converts
  (is (equal '(item:true t) (%item-value-of t)))
  (is (equal '(item:false t) (%item-value-of nil)))
  (is (equal '(nil nil) (%item-value-of :null)))
  (is (equal '(42 t) (%item-value-of 42)))
  (is (equal '("abc" t) (%item-value-of "abc")))
  ;; scaled: an integer stays one, a fraction becomes a float
  (is (equal '(21.5 t) (%item-value-of 2150 1/100)))
  (is (equal '(21 t) (%item-value-of 2100 1/100)))
  (is (equal '(50 t) (%item-value-of 127 100/254)))
  (is (equal '(item:true t) (%item-value-of t 1/100)))
  (is (equal '(nil nil) (%item-value-of :null 1/100)))
  ;; what an attribute cl-matter does not define comes as
  (is (equal '(1500 t) (%item-value-of (matter:make-tlv-element nil :unsigned-int 1500))))
  (is (equal '(1.5 t) (%item-value-of (matter:make-tlv-element nil :unsigned-int 1500) 1/1000)))
  (is (equal '(item:true t) (%item-value-of (matter:make-tlv-element nil :boolean t))))
  (is (equal '(nil nil) (%item-value-of (matter:make-tlv-element nil :null nil))))
  (let ((structure (matter:make-tlv-element nil :structure '())))
    (is (equal (list structure t) (%item-value-of structure)))))

(defun %command-of (command value)
  (multiple-value-list
   (binding-matter::%command-of-value (binding-matter::%parse-command :level-control command) value)))

(test command-of-value--makes-the-command
  (is (equal '(:stop nil) (%command-of :stop 'item:true)))
  (is (equal '(:stop nil) (%command-of :stop 17)))
  (flet ((switch (value)
           (multiple-value-list
            (binding-matter::%command-of-value (binding-matter::%parse-command :on-off '(:off :on)) value))))
    (is (equal '(:on nil) (switch 'item:true)))
    (is (equal '(:off nil) (switch 'item:false)))
    ;; a value that is neither says nothing
    (is (equal '(nil) (switch 17)))
    (is (equal '(nil) (switch t))))
  (is (equal '(:move-to-level (:level 127 :transition-time 0))
             (%command-of '(:move-to-level :level (:scale 254/100) :transition-time 0) 50)))
  (is (equal '(:move-to-level (:level 254 :transition-time :null))
             (%command-of '(:move-to-level :level (:scale 254/100) :transition-time :null) 100)))
  (is (equal '(:move-to-level (:level 7 :transition-time 0))
             (%command-of '(:move-to-level :level (:value) :transition-time 0) 7)))
  (is (equal '(:move-to-level (:level t)) (%command-of '(:move-to-level :level (:value)) 'item:true)))
  (is (equal '(:move-to-level (:level nil)) (%command-of '(:move-to-level :level (:value)) 'item:false)))
  ;; no number to scale
  (signals error (%command-of '(:move-to-level :level (:scale 254/100)) 'item:true))
  ;; what comes out is what cl-matter makes the command's fields of
  (is-true (matter:tlv-element-p
            (matter:command-fields :level-control :move-to-level
                                   (second (%command-of '(:move-to-level :level (:scale 254/100) :transition-time 0)
                                                        50))))))

;; -----------------------------
;; in and out, without a node
;; -----------------------------

(test binding-report--sets-the-items-and-does-not-push
  (with-fixture controller-env (:init nil)
    (let* ((pushed '())
           (binding (matter-binding :node "plug" :endpoint 1 :cluster :on-off)))
      (setf (slot-value binding 'binding::push-fun) (lambda (value) (push value pushed)))
      (hab:defitem 'plug "Plug" 'boolean binding :initial-value 'item:false)
      (binding-matter::binding-report binding t)
      (is-true (sento.miscutils:await-cond 2 (eq 'item:true (%item-value 'plug))))
      ;; a null changes nothing
      (binding-matter::binding-report binding :null)
      (binding-matter::binding-report binding nil)
      (is-true (sento.miscutils:await-cond 2 (eq 'item:false (%item-value 'plug))))
      (is (null pushed))
      ;; the item's own change is pushed
      (hab:set-item-value 'plug 'item:true)
      (is-true (sento.miscutils:await-cond 2 (equal '(item:true) pushed))))))

(test binding-report--scales-and-transforms
  (with-fixture controller-env (:init nil)
    (hab:defitem 'temp "Temperature" 'float
      (matter-binding :node "sensor" :endpoint 1 :cluster :temperature-measurement :scale 1/100)
      :initial-value 0.0)
    (hab:defitem 'temp-f "Temperature F" 'float
      (matter-binding :node "sensor" :endpoint 1 :cluster :temperature-measurement :scale 1/100
                      :transform (lambda (celsius) (+ 32 (* 9/5 celsius))))
      :initial-value 0.0)
    (binding-matter::binding-report (first (item:bindings (hab:get-item 'temp))) 2150)
    (binding-matter::binding-report (first (item:bindings (hab:get-item 'temp-f))) 2000)
    (is-true (sento.miscutils:await-cond 2 (eql 21.5 (%item-value 'temp))))
    (is-true (sento.miscutils:await-cond 2 (eql 68 (%item-value 'temp-f))))))

(test matter-binding--a-push-without-controller-is-logged-not-signalled
  (with-fixture controller-env (:init nil)
    (hab:defitem 'plug "Plug" 'boolean
      (matter-binding :node "plug" :endpoint 1 :cluster :on-off)
      :initial-value 'item:false)
    (let ((binding (first (item:bindings (hab:get-item 'plug)))))
      (finishes (binding:exec-push binding 'item:true))
      ;; a value the command has nothing for
      (finishes (binding:exec-push binding 42))
      ;; and a pull is the base binding's business to survive
      (finishes (binding:exec-pull binding)))))

(test matter-binding--a-push-to-a-node-that-is-not-there-fails-quietly
  (with-fixture controller-env ()
    (hab:defitem 'plug "Plug" 'boolean
      (matter-binding :node "plug" :endpoint 1 :cluster :on-off)
      :initial-value 'item:false)
    (hab:set-item-value 'plug 'item:true)
    (is-true (sento.miscutils:await-cond 2 (eq 'item:true (%item-value 'plug))))
    (is-true (matter-controller-running-p))))

;; -----------------------------
;; the live bindings
;; -----------------------------

(test node-bindings--finds-a-node-s-bindings-by-label-and-id
  (with-fixture controller-env (:init nil)
    (let* ((lamp (%entry :node-id 2 :label "lamp"))
           (plug (%entry :node-id 3 :label "plug"))
           (nodes (list lamp plug))
           (by-label (matter-binding :node "lamp" :endpoint 1 :cluster :on-off))
           (by-id (matter-binding :node 2 :endpoint 1 :cluster :level-control))
           (command-only (matter-binding :node "lamp" :endpoint 1 :cluster :on-off :attribute nil))
           (other (matter-binding :node "plug" :endpoint 1 :cluster :on-off)))
      (declare (ignorable command-only))
      (is (null (set-exclusive-or (list by-label by-id) (binding-matter::node-bindings nodes lamp))))
      (is (equal (list other) (binding-matter::node-bindings nodes plug)))
      ;; the paths of a subscription: each once
      (matter-binding :node "lamp" :endpoint 1 :cluster :on-off :command nil)
      (is (= 2 (length (binding-matter::%subscription-paths (binding-matter::node-bindings nodes lamp)))))
      ;; destroyed, a binding is none any more
      (binding:destroy by-id)
      (is (= 2 (length (binding-matter::node-bindings nodes lamp))))
      (is (= 1 (length (binding-matter::%subscription-paths (binding-matter::node-bindings nodes lamp)))))
      ;; and `matter-shutdown' forgets them all
      (matter-shutdown)
      (is (null (%live-bindings))))))
