(defpackage :chipi.house-test
  (:use :cl :fiveam :miscutils :chipi.house)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.house-test)

(def-suite house-tests
  :description "House vocabulary on top of the primitives"
  :in chipi.tests:test-suite)

(in-suite house-tests)

(def-fixture clean-after ()
  (unwind-protect
       (progn
         (&body))
    (hab:shutdown)))

(defun %item-value (item)
  (item:item-state-value (item:get-item-stateq item)))

(defun %collect-changed-events (collect-fun)
  "Subscribes to `item:item-changed-event' and calls `COLLECT-FUN' with each."
  (ac:actor-of (isys:ensure-isys)
               :init (lambda (self)
                       (ev:subscribe self self 'item:item-changed-event))
               :receive (lambda (msg)
                          (when (typep msg 'item:item-changed-event)
                            (funcall collect-fun msg)))))

(test send-command--pushes-to-binding-when-value-is-unchanged
  "Tests that a command reaches the binding every time, also when the item
already holds the commanded value -- where `item:set-value' pushes nothing."
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (item:add-binding item binding)
      (item:set-value item "Foo")
      (sleep 0.3)
      (is-false pushed-values)
      (is-true (send-command item "Foo"))
      (send-command item "Foo")
      (is-true (await-cond 0.5
                 (equal pushed-values '("Foo" "Foo")))))))

(test send-command--sets-value-when-changed
  "Tests that a command with a new value updates the item, raises the changed
event and pushes -- once."
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (events)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (%collect-changed-events (lambda (ev) (push ev events)))
      (item:add-binding item binding)
      (send-command item "Bar")
      (is-true (await-cond 0.5 (= 1 (length events))))
      (is (equal "Foo" (item:item-changed-event-old-value (first events))))
      (is (equal "Bar" (%item-value item)))
      (sleep 0.3)
      (is (equal pushed-values '("Bar"))))))

(test send-command--keeps-state-and-raises-no-event-when-value-is-unchanged
  "Tests that a repeated command only pushes: the item state, its timestamp
and the subscribers are left alone."
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value "Foo"))
           (timestamp (item:item-state-timestamp (item:get-item-stateq item)))
           (pushed-values)
           (events)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (%collect-changed-events (lambda (ev) (push ev events)))
      (item:add-binding item binding)
      (sleep 1)
      (send-command item "Foo")
      (is-true (await-cond 0.5
                 (equal pushed-values '("Foo"))))
      (sleep 0.3)
      (is-false events)
      (is (eql timestamp
               (item:item-state-timestamp (item:get-item-stateq item)))))))

(test send-command--does-no-push-when-binding-has-no-call-push-p
  "Tests that the binding's `call-push-p' keeps precedence over a command."
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :call-push-p nil
                     :push (lambda (value) (push value pushed-values)))))
      (item:add-binding item binding)
      (send-command item "Foo")
      (send-command item "Bar")
      (is-true (await-cond 0.5 (equal "Bar" (%item-value item))))
      (sleep 0.3)
      (is-false pushed-values))))

(test send-command--passes-timestamp-and-persist
  "Tests that `:timestamp' and `:persist' reach the item as with `item:set-value'."
  (with-fixture clean-after ()
    (let ((item (item:make-item 'my-item :initial-value "Foo"))
          (timestamp (- (get-universal-time) 1000)))
      (send-command item "Bar" :timestamp timestamp :persist nil)
      (is-true (await-cond 0.5 (equal "Bar" (%item-value item))))
      (is (eql timestamp
               (item:item-state-timestamp (item:get-item-stateq item)))))))

(test send-command--by-item-id
  "Tests that a command can name the item by the id it was defined with."
  (with-fixture clean-after ()
    (let ((pushed-values))
      (hab:defconfig "chipi")
      (hab:defitem 'door-opener "Door opener" 'boolean
        :initial-value 'item:true
        (hab:binding :push (lambda (value) (push value pushed-values))))
      (send-command 'door-opener 'item:true)
      (send-command 'door-opener 'item:true)
      (send-command 'door-opener 'item:false)
      (is-true (await-cond 0.5
                 (equal pushed-values '(item:false item:true item:true))))
      (is (eq 'item:false (hab:get-item-valueq 'door-opener))))))

(test send-command--signals-on-unknown-item-id
  "Tests that a command to an id no item was defined with is an error rather
than silently going nowhere."
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (signals error (send-command 'no-such-item 'item:true))))

;; -----------------------------
;; pulse
;; -----------------------------

(test pulse--sends-on-and-after-duration-off
  "Tests that a pulse presses and, after the duration, releases -- and that it
can be repeated."
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value 'item:false))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (item:add-binding item binding)
      (is-true (pulse item :duration 0.3))
      (is-true (await-cond 0.2
                 (equal pushed-values '(item:true))))
      (is-true (await-cond 1
                 (equal pushed-values '(item:false item:true))))
      (pulse item :duration 0.3)
      (is-true (await-cond 1
                 (equal pushed-values
                        '(item:false item:true item:false item:true)))))))

(test pulse--with-own-on-and-off-values
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value 0))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (item:add-binding item binding)
      (pulse item :duration 0.2 :on 1 :off 0)
      (is-true (await-cond 1
                 (equal pushed-values '(0 1)))))))

;; -----------------------------
;; single item kinds
;; -----------------------------

(defun %kind (item-id)
  (cdr (assoc :house-kind (item:tags (hab:get-item item-id)))))

(test deflight--defines-boolean-item-of-kind-light
  "Tests that a light is a boolean item, off, of kind `:light', and that the
body is that of `defitem': group, tags and binding arrive on the item."
  (with-fixture clean-after ()
    (let ((pushed-values))
      (hab:defconfig "chipi")
      (hab:defitemgroup 'lights "Lights")
      (deflight light-a "Light A"
        :group '(lights)
        :tags '((:ui-type . "Licht") (:ui-order . 20))
        (hab:binding :push (lambda (value) (push value pushed-values))))
      (let ((item (hab:get-item 'light-a)))
        (is-true item)
        (is (equal "Light A" (item:label item)))
        (is (eq 'boolean (item:value-type-hint item)))
        (is (eq 'item:false (hab:get-item-valueq 'light-a)))
        (is (equal '((:house-kind . :light)
                     (:ui-type . "Licht")
                     (:ui-order . 20))
                   (item:tags item)))
        (is-true (member item (hab:get-items-on-group 'lights))))
      (hab:set-item-value 'light-a 'item:true)
      (is-true (await-cond 0.5
                 (equal pushed-values '(item:true)))))))

(test deflight--initial-value-of-body-replaces-default
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (deflight light-a "Light A"
      :initial-value 'item:true)
    (is (eq 'item:true (hab:get-item-valueq 'light-a)))
    (is (eq :light (%kind 'light-a)))))

(test defplug--defines-boolean-item-of-kind-plug
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defplug plug-a "Plug A")
    (is (eq 'boolean (item:value-type-hint (hab:get-item 'plug-a))))
    (is (eq 'item:false (hab:get-item-valueq 'plug-a)))
    (is (eq :plug (%kind 'plug-a)))))

(test defcontact--defines-boolean-item-of-kind-contact
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defcontact window-a "Window A"
      :tags '((:ui-readonly . t)))
    (is (eq 'boolean (item:value-type-hint (hab:get-item 'window-a))))
    (is (eq 'item:false (hab:get-item-valueq 'window-a)))
    (is (equal '((:house-kind . :contact) (:ui-readonly . t))
               (item:tags (hab:get-item 'window-a))))))

(test defmode--defines-integer-item-of-kind-mode
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defmode mode-a "Mode A")
    (is (eq 'integer (item:value-type-hint (hab:get-item 'mode-a))))
    (is (eql 0 (hab:get-item-valueq 'mode-a)))
    (is (eq :mode (%kind 'mode-a)))))

(test defpulse--defines-item-and-trigger-function
  "Tests that a pulse is a boolean item of kind `:pulse' with a function
NAME-TRIGGER that presses and releases it, as often as it is called."
  (with-fixture clean-after ()
    (let ((pushed-values))
      (hab:defconfig "chipi")
      (defpulse opener-a "Opener A"
        :duration 0.3
        (hab:binding :push (lambda (value) (push value pushed-values))))
      (is (eq 'boolean (item:value-type-hint (hab:get-item 'opener-a))))
      (is (eq 'item:false (hab:get-item-valueq 'opener-a)))
      (is (eq :pulse (%kind 'opener-a)))
      (is-true (fboundp 'opener-a-trigger))
      (funcall 'opener-a-trigger)
      (is-true (await-cond 1
                 (equal pushed-values '(item:false item:true))))
      (funcall 'opener-a-trigger)
      (is-true (await-cond 1
                 (equal pushed-values
                        '(item:false item:true item:false item:true)))))))

(test defpulse--keeps-the-duration-on-the-item
  "Tests that the duration is a tag of the item, 1 second unless given -- so
that whoever holds the item can press it the way it was defined."
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defpulse opener-b "Opener B" :duration 2)
    (defpulse opener-c "Opener C" :tags '((:foo . 1)))
    (flet ((tags (id) (item:tags (hab:get-item id))))
      (is (eql 2 (cdr (assoc :house-pulse-duration (tags 'opener-b)))))
      (is (eql 1 (cdr (assoc :house-pulse-duration (tags 'opener-c)))))
      ;; kind first, own tags kept
      (is (eq :house-kind (car (first (tags 'opener-c)))))
      (is (eql 1 (cdr (assoc :foo (tags 'opener-c))))))))

(test press--pulses-for-the-duration-of-the-push-button
  "Tests that `press' needs nothing but the item: the duration is the one
given to `defpulse'."
  (with-fixture clean-after ()
    (let ((pushed-values))
      (hab:defconfig "chipi")
      (defpulse opener-d "Opener D"
        :duration 0.3
        (hab:binding :push (lambda (value) (push value pushed-values))))
      (is-true (press 'opener-d))
      (is-true (await-cond 0.2
                 (equal pushed-values '(item:true))))
      (is-true (await-cond 1
                 (equal pushed-values '(item:false item:true))))
      ;; the item itself works as well as its id
      (press (hab:get-item 'opener-d))
      (is-true (await-cond 1
                 (equal pushed-values
                        '(item:false item:true item:false item:true)))))))

(test press--one-second-on-an-item-without-a-duration
  (with-fixture clean-after ()
    (let* ((item (item:make-item 'my-item :initial-value 'item:false))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (item:add-binding item binding)
      (press item)
      (is-true (await-cond 0.2
                 (equal pushed-values '(item:true))))
      (sleep 0.5)
      (is (equal pushed-values '(item:true)))
      (is-true (await-cond 1.5
                 (equal pushed-values '(item:false item:true)))))))

;; -----------------------------
;; rollershutter
;; -----------------------------

(test defrollershutter--defines-an-item-per-part
  "Tests ids, types, start values, labels and kinds of the four parts, and
that each part's body reaches its item."
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (hab:defitemgroup 'blinds "Blinds")
    (hab:defitemgroup 'living "Living")
    (is (eq 'blind-a
            (defrollershutter blind-a "Blind A"
              :move ((hab:binding :push #'identity)
                     :group '(blinds))
              :stop ((hab:binding :push #'identity)
                     :group '(blinds))
              :position ((hab:binding :push #'identity)
                         :group '(blinds living)
                         :tags '((:ui-order . 40)))
              :slat ((hab:binding :push #'identity)
                     :group '(blinds living)))))
    (flet ((check (id label type-hint value kind groups)
             (let ((item (hab:get-item id)))
               (is-true item)
               (is (equal label (item:label item)))
               (is (eq type-hint (item:value-type-hint item)))
               (is (eql value (hab:get-item-valueq id)))
               (is (eq kind (%kind id)))
               (is (= 1 (length (item::bindings item))))
               (dolist (group '(blinds living))
                 (is (eq (not (null (member group groups)))
                         (not (null (member item (hab:get-items-on-group group))))))))))
      (check 'blind-a "Blind A" 'boolean 'item:false :shutter-move '(blinds))
      (check 'blind-a-stop "Blind A Stop" 'boolean 'item:false :shutter-stop '(blinds))
      (check 'blind-a-pos "Blind A Position" 'integer 0 :shutter-position '(blinds living))
      (check 'blind-a-slat "Blind A Slat" 'integer 0 :shutter-slat '(blinds living)))
    (is (equal '((:house-kind . :shutter-position) (:ui-order . 40))
               (item:tags (hab:get-item 'blind-a-pos))))))

(test defrollershutter--command-functions-send-every-time
  "Tests that NAME-up, -down and -stop reach the binding of their part every
time they are called, also twice in a row."
  (with-fixture clean-after ()
    (let ((moves) (stops))
      (hab:defconfig "chipi")
      (defrollershutter blind-b "Blind B"
        :move ((hab:binding :push (lambda (value) (push value moves))))
        :stop ((hab:binding :push (lambda (value) (push value stops)))))
      (funcall 'blind-b-up)
      (funcall 'blind-b-up)
      (funcall 'blind-b-down)
      (funcall 'blind-b-down)
      (is-true (await-cond 0.5
                 (equal moves '(item:true item:true item:false item:false))))
      (funcall 'blind-b-stop)
      (funcall 'blind-b-stop)
      (is-true (await-cond 0.5
                 (equal stops '(item:true item:true)))))))

(test defrollershutter--only-given-parts-are-defined
  "Tests that a part left out defines neither its item nor its functions."
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defrollershutter blind-c "Blind C"
      :move ())
    (is-true (hab:get-item 'blind-c))
    (is-true (fboundp 'blind-c-up))
    (is-true (fboundp 'blind-c-down))
    (is-false (fboundp 'blind-c-stop))
    (is-false (hab:get-item 'blind-c-stop))
    (is-false (hab:get-item 'blind-c-pos))
    (is-false (hab:get-item 'blind-c-slat))))

(test defrollershutter--part-takes-own-id-and-label
  "Tests that `:id' and `:label' of a part replace the derived ones, and that
the command functions follow the item."
  (with-fixture clean-after ()
    (let ((stops))
      (hab:defconfig "chipi")
      (defrollershutter blind-d "Blind D"
        :stop ((hab:binding :push (lambda (value) (push value stops)))
               :id blind-d-halt
               :label "Halt D")
        :slat (:id blind-d-lam
               :label "Blind D Lamelle"
               :initial-value 50))
      (is-false (hab:get-item 'blind-d-stop))
      (is-false (hab:get-item 'blind-d-slat))
      (is (equal "Halt D" (item:label (hab:get-item 'blind-d-halt))))
      (is (equal "Blind D Lamelle" (item:label (hab:get-item 'blind-d-lam))))
      (is (eql 50 (hab:get-item-valueq 'blind-d-lam)))
      (is (eq :shutter-slat (%kind 'blind-d-lam)))
      (funcall 'blind-d-stop)
      (is-true (await-cond 0.5
                 (equal stops '(item:true)))))))

(test defrollershutter--rejects-malformed-parts
  "Tests that a part id that is not a plain symbol and a key without a value
are errors when the form is expanded."
  (signals error
    (macroexpand-1 '(defrollershutter blind-e "Blind E"
                     :position (:id 'blind-e-position))))
  (signals error
    (macroexpand-1 '(defrollershutter blind-e "Blind E"
                     :position (:group)))))

;; -----------------------------
;; heating
;; -----------------------------

(test defheating--defines-an-item-per-part
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (hab:defitemgroup 'living "Living")
    (is (eq 'heat-a
            (defheating heat-a "Living"
              :temperature ((hab:binding :pull (lambda () 21.5) :initial-delay 0.1)
                            :group '(living))
              :setpoint ()
              :valve (:group '(living)))))
    (flet ((check (id label type-hint value kind)
             (let ((item (hab:get-item id)))
               (is-true item)
               (is (equal label (item:label item)))
               (is (eq type-hint (item:value-type-hint item)))
               (is (eql value (hab:get-item-valueq id)))
               (is (eq kind (%kind id))))))
      (check 'heat-a-temp "Living Temperature" 'float 0.0 :temperature)
      (check 'heat-a-setpoint "Living Setpoint" 'float 0.0 :setpoint)
      (check 'heat-a-valve "Living Valve" 'integer 0 :valve))
    (let ((in-living (hab:get-items-on-group 'living)))
      (is-true (member (hab:get-item 'heat-a-temp) in-living))
      (is-true (member (hab:get-item 'heat-a-valve) in-living))
      (is-false (member (hab:get-item 'heat-a-setpoint) in-living)))
    ;; the part's binding is live
    (is-true (await-cond 1
               (eql 21.5 (hab:get-item-valueq 'heat-a-temp))))))

(test defheating--only-given-parts--with-own-id-and-label
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defheating heat-b "Bath"
      :temperature (:id temp-heat-b
                    :label "Temperatur Bad"))
    (is (equal "Temperatur Bad" (item:label (hab:get-item 'temp-heat-b))))
    (is (eq :temperature (%kind 'temp-heat-b)))
    (is-false (hab:get-item 'heat-b-temp))
    (is-false (hab:get-item 'heat-b-setpoint))
    (is-false (hab:get-item 'heat-b-valve))))

;; -----------------------------
;; rules
;; -----------------------------

(test def-any-true-rule--target-follows-sources
  "Tests that the target is true while any source is, and false once none is."
  (with-fixture clean-after ()
    (hab:defconfig "chipi")
    (defcontact sash-left "Left")
    (defcontact sash-right "Right")
    (defcontact window-both "Window")
    (def-any-true-rule "window-both-rule"
        (sash-left sash-right)
      window-both)
    (is-true (hab:get-rule "window-both-rule"))
    (flet ((window-open-p ()
             (eq 'item:true (hab:get-item-valueq 'window-both))))
      (hab:set-item-value 'sash-left 'item:true)
      (is-true (await-cond 1 (window-open-p)))
      (hab:set-item-value 'sash-right 'item:true)
      (hab:set-item-value 'sash-left 'item:false)
      (sleep 0.5)
      (is-true (window-open-p))
      (hab:set-item-value 'sash-right 'item:false)
      (is-true (await-cond 1 (not (window-open-p)))))))

(test def-any-true-rule--rejects-malformed-sources
  (signals error
    (macroexpand-1 '(def-any-true-rule "rule" sash-left window-both)))
  (signals error
    (macroexpand-1 '(def-any-true-rule "rule" ("sash-left") window-both))))
