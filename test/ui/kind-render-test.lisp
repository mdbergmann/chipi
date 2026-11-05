(defpackage :chipi-ui.kind-render-test
  (:use :cl :fiveam :cl-mock :chipi-ui.test-utils))

(in-package :chipi-ui.kind-render-test)

(def-suite ui-kind-render-tests
  :description "Offline rendering tests for what an itemgroup card makes of an
item's `:house-kind' tag -- the kind the `chipi.house' definers give an item:
stub the CLOG connection, then assert on the DOM/JS commands CLOG would have
sent and on what the controls do when clicked."
  :in chipi-ui.tests:test-suite)

(in-suite ui-kind-render-tests)

(defparameter +owner+ :kind-render-test-owner)

(def-fixture render-env ()
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (&body))))

(defun %tags (&rest key-value-pairs)
  (let ((ht (make-hash-table)))
    (loop :for (k v) :on key-value-pairs :by #'cddr :do (setf (gethash k ht) v))
    ht))

(defun %render (type-hint value &rest tags)
  "Renders the card row of an item of TYPE-HINT holding VALUE, tagged TAGS."
  (ui-renderer::%render-item
   +owner+
   (make-item-ht :label "Thing" :name "THING" :type-hint type-hint
                 :value value :tags (apply #'%tags tags))
   (make-body)))

(defun %control (type-hint value &rest tags)
  "The control `%create-kind-control' makes for such an item, or `nil'."
  (ui-renderer::%create-kind-control
   +owner+ (clog:create-div (make-body)) "THING" value type-hint
   (apply #'%tags tags)))

(defun %live-update (value)
  (setf *captured* nil)
  (let ((state (make-hash-table :test #'equal)))
    (setf (gethash "value" state) value
          (gethash "timestamp" state) 0)
    (ui-renderer:call-item-value-update-fun "THING" state)))

;; ----------------------------------------------------------------------------
;; kinds that are switched: nothing changes
;; ----------------------------------------------------------------------------

(test kind--light-and-plug-keep-the-toggle
  (dolist (kind '(:light :plug))
    (with-fixture render-env ()
      (%render "BOOLEAN" t :house-kind kind)
      (is-true (js~ "checkbox"))
      (is-false (js~ "<button")))))

(test kind--unknown-kind-renders-by-type
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :teleporter)
    (is-true (js~ "checkbox")))
  (with-fixture render-env ()
    (%render "FLOAT" 21.5 :house-kind :temperature)
    (is-true (js~ "item-value-display"))
    (is-true (js~ "21.50"))
    (is-false (js~ "<button"))))

;; ----------------------------------------------------------------------------
;; contact: reports, is never switched
;; ----------------------------------------------------------------------------

(test kind--contact-is-a-display-not-a-toggle
  "A contact needs no `:ui-readonly' to be kept from being toggled, and no
`:ui-mapping' to read as open / closed."
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :contact)
    (is-false (js~ "checkbox"))
    (is-true (js~ "item-value-display"))
    (is-true (js~ "boolean-true"))
    (is-true (js~ "open</div>"))
    (%live-update nil)
    (is-true (js~ "text('closed')"))
    (is-true (js~ "addClass('boolean-false')"))))

(test kind--contact-own-mapping-wins-over-open-closed
  (with-fixture render-env ()
    (%render "BOOLEAN" nil
             :house-kind :contact
             :ui-mapping (page:value-mapping t "offen" nil "zu"))
    (is-true (js~ "zu</div>"))
    (is-false (js~ "closed"))))

(test kind--value-widget-reads-a-contact-as-open-closed
  (with-fixture render-env ()
    (with-mocks ()
      (answer hab:get-item :mock-item)
      (answer item-ext:item-to-ht
        (make-item-ht :label "Window" :name "WIN" :type-hint "BOOLEAN" :value t
                      :tags (%tags :house-kind :contact)))
      (ui-renderer:render-widget (page:value 'win) +owner+ (make-body))
      (is-true (js~ "open</div>"))
      (is-true (js~ "boolean-true")))))

;; ----------------------------------------------------------------------------
;; momentary commands: buttons, not toggles
;; ----------------------------------------------------------------------------

(test kind--shutter-move-renders-up-and-down-buttons
  (with-fixture render-env ()
    (%render "BOOLEAN" nil :house-kind :shutter-move)
    (is-false (js~ "checkbox"))
    (is (= 2 (count-js "<button")))
    (is-true (js~ "&#9650;"))
    (is-true (js~ "&#9660;"))
    (is-true (js~ "Up"))
    (is-true (js~ "Down"))))

(test kind--shutter-move-buttons-send-commands
  "Up is `item:false', down `item:true' -- sent as commands, so the same
direction goes out as often as it is pressed."
  (with-fixture render-env ()
    (with-mocks ()
      (answer ui-renderer::%item-named :the-item)
      (answer house:send-command t)
      (destructuring-bind (up down)
          (%control "BOOLEAN" nil :house-kind :shutter-move)
        (fire-click up)
        (fire-click down)
        (fire-click down)
        (is (equal '((house:send-command :the-item item:false)
                     (house:send-command :the-item item:true)
                     (house:send-command :the-item item:true))
                   (invocations 'house:send-command)))))))

(test kind--shutter-stop-renders-one-button-that-commands-stop
  (with-fixture render-env ()
    (with-mocks ()
      (answer ui-renderer::%item-named :the-item)
      (answer house:send-command t)
      (let ((buttons (%control "BOOLEAN" nil :house-kind :shutter-stop)))
        (is (= 1 (length buttons)))
        (is-true (js~ "&#9632;"))
        (fire-click (first buttons))
        (fire-click (first buttons))
        (is (equal '((house:send-command :the-item item:true)
                     (house:send-command :the-item item:true))
                   (invocations 'house:send-command)))))))

(test kind--pulse-renders-a-button-that-presses
  (with-fixture render-env ()
    (with-mocks ()
      (answer ui-renderer::%item-named :the-item)
      (answer house:press t)
      (let ((buttons (%control "BOOLEAN" nil :house-kind :pulse)))
        (is (= 1 (length buttons)))
        (is-true (js~ "Press"))
        (fire-click (first buttons))
        (is (equal '((house:press :the-item))
                   (invocations 'house:press)))))))

(test kind--command-on-a-vanished-item-does-not-escape
  (with-fixture render-env ()
    (with-mocks ()
      (answer ui-renderer::%item-named nil)
      (answer house:send-command t)
      (let ((buttons (%control "BOOLEAN" nil :house-kind :shutter-stop)))
        (finishes (fire-click (first buttons)))
        (is (null (invocations 'house:send-command)))))))

(test kind--failing-command-does-not-escape
  (with-fixture render-env ()
    (with-mocks ()
      (answer ui-renderer::%item-named :the-item)
      (answer house:send-command (error "boom"))
      (let ((buttons (%control "BOOLEAN" nil :house-kind :shutter-stop)))
        (finishes (fire-click (first buttons)))))))

;; ----------------------------------------------------------------------------
;; numbers a card can now set
;; ----------------------------------------------------------------------------

(test kind--shutter-position-and-slat-render-a-percent-slider
  (dolist (kind '(:shutter-position :shutter-slat))
    (with-fixture render-env ()
      (%render "INTEGER" 40 :house-kind kind)
      (is-true (js~ "widget-slider"))
      (is-true (js~ "range"))
      (is-true (js~ "prop('min','0')"))
      (is-true (js~ "prop('max','100')"))
      (is-true (js~ "on('change'")))))

(test kind--setpoint-renders-a-stepper
  (with-fixture render-env ()
    (%render "FLOAT" 21.0 :house-kind :setpoint)
    (is-true (js~ "widget-setpoint"))
    (is (= 2 (count-js "setpoint-btn")))
    (is-true (js~ "21.00"))))

(test kind--setpoint-steps-by-half-a-degree
  (with-fixture render-env ()
    (with-mocks ()
      (answer (itemsc:update-item-value name value) (list name value))
      (%render "FLOAT" 21.0 :house-kind :setpoint)
      ;; the stepper's buttons are the only click handlers of the row
      (let ((handlers (loop :for key :being :the :hash-keys
                              :of (gethash +conn-id+ clog-connection::*connection-data*)
                              :using (hash-value handler)
                            :when (and (stringp key) (search ":click" key))
                              :collect handler)))
        (is (= 2 (length handlers)))
        (dolist (handler handlers) (funcall handler ""))
        (is (equal '(20.5 21.5)
                   (sort (mapcar #'third (invocations 'itemsc:update-item-value))
                         #'<)))))))

(test kind--mode-with-a-mapping-renders-a-selection-of-its-codes
  (with-fixture render-env ()
    (%render "INTEGER" 1
             :house-kind :mode
             :ui-mapping (page:value-mapping 2 "Standby" 0 "Auto" 1 "Komfort"))
    (is-true (js~ "widget-selection"))
    (is-true (js~ "on('change'"))
    ;; listed by code, not in whatever order the table holds them
    (let ((js (captured-js)))
      (is (< (search "Auto" js) (search "Komfort" js) (search "Standby" js))))))

(test kind--mode-without-a-mapping-stays-a-value
  (with-fixture render-env ()
    (%render "INTEGER" 1 :house-kind :mode)
    (is-false (js~ "widget-selection"))
    (is-true (js~ "item-value-display"))))

;; ----------------------------------------------------------------------------
;; :ui-readonly keeps any of them a display
;; ----------------------------------------------------------------------------

(test kind--ui-readonly-wins-over-the-kind
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :shutter-move :ui-readonly t)
    (is-false (js~ "<button"))
    (is-true (js~ "ON</div>")))
  (with-fixture render-env ()
    (%render "INTEGER" 40 :house-kind :shutter-position :ui-readonly t)
    (is-false (js~ "range"))
    (is-true (js~ "item-value-display")))
  (with-fixture render-env ()
    (%render "FLOAT" 21.0 :house-kind :setpoint :ui-readonly t)
    (is-false (js~ "setpoint-btn"))
    (is-true (js~ "21.00")))
  (with-fixture render-env ()
    (%render "INTEGER" 1
             :house-kind :mode :ui-readonly t
             :ui-mapping (page:value-mapping 0 "Auto" 1 "Komfort"))
    (is-false (js~ "widget-selection"))
    (is-true (js~ "Komfort</div>")))
  (with-fixture render-env ()
    (%render "BOOLEAN" nil :house-kind :pulse :ui-readonly t)
    (is-false (js~ "<button"))
    (is-true (js~ "OFF</div>"))))

;; ----------------------------------------------------------------------------
;; the badge names the kind
;; ----------------------------------------------------------------------------

(test kind--badge-names-the-kind
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :light)
    (is-true (js~ "Light</div>"))
    (is-false (js~ "Switch</div>")))
  (with-fixture render-env ()
    (%render "FLOAT" 21.0 :house-kind :temperature)
    (is-true (js~ "Temperature</div>"))
    (is-false (js~ "Decimal Number"))))

(test kind--ui-type-wins-over-the-kind-in-the-badge
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :light :ui-type "Licht")
    (is-true (js~ "Licht</div>"))
    (is-false (js~ "Light</div>"))))

(test kind--unknown-kind-keeps-the-type-badge
  (with-fixture render-env ()
    (%render "BOOLEAN" t :house-kind :teleporter)
    (is-true (js~ "Switch</div>"))))
