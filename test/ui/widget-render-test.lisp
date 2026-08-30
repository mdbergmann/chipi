(defpackage :chipi-ui.widget-render-test
  (:use :cl :fiveam :cl-mock :chipi-ui.test-utils))

(in-package :chipi-ui.widget-render-test)

(def-suite ui-widget-render-tests
  :description "Offline rendering tests for the defpage widget renderers and
page navigation: stub the CLOG connection, mock item resolution, then assert
on the DOM/JS commands CLOG would have sent."
  :in chipi-ui.tests:test-suite)

(in-suite ui-widget-render-tests)

(def-fixture render-env ()
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (unwind-protect
         (progn (&body))
      (page:clear-pages))))

(defun make-ctx (body)
  (ui-renderer:make-nav-context :body body
                                :container (clog:create-div body :class "container")))

(defmacro with-item ((&rest item-ht-args) &body body)
  "Runs BODY with item resolution mocked: any item symbol resolves to an item
hash-table built from ITEM-HT-ARGS (as for `make-item-ht')."
  `(with-mocks ()
     (answer hab:get-item :mock-item)
     (answer item-ext:item-to-ht (make-item-ht ,@item-ht-args))
     ,@body))

(defun %js-count (substr)
  "How often SUBSTR appears in the captured JS."
  (let ((s (captured-js)))
    (loop :with start = 0
          :for pos = (search substr s :start2 start)
          :while pos
          :count 1
          :do (setf start (1+ pos)))))

;; ----------------------------------------------------------------------------
;; item widgets -- row rendering + controls.
;; ----------------------------------------------------------------------------

(test widget--toggle-renders-row-label-and-switch
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Lamp" :name "switch.lamp" :type-hint "BOOLEAN" :value t)
          (ui-renderer:render-widget (page:toggle 'lamp) :owner body)
          (is-true (js~ "widget widget-toggle"))
          (is-true (js~ "widget-label"))
          (is-true (js~ "Lamp"))                 ; label from the item
          (is-true (js~ "checkbox"))
          (is-true (js~ "prop('checked'")))))))

(test widget--label-override-wins-over-item-label
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Item label" :name "switch.lamp" :type-hint "BOOLEAN" :value t)
          (ui-renderer:render-widget (page:toggle 'lamp :label "Widget label") :owner body)
          (is-true (js~ "Widget label")))))))

(test widget--value-with-custom-format
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Temp" :name "sensor.temp" :type-hint "FLOAT" :value 23.5)
          (ui-renderer:render-widget (page:value 'temp :format "~,1f °C") :owner body)
          (is-true (js~ "widget widget-value"))
          (is-true (js~ "23.5 °C")))))))

;; ----------------------------------------------------------------------------
;; :ui-mapping -- a coded value shows its label, not the code.
;; ----------------------------------------------------------------------------

(defun %tags (&rest key-value-pairs)
  (let ((ht (make-hash-table)))
    (loop :for (k v) :on key-value-pairs :by #'cddr :do (setf (gethash k ht) v))
    ht))

(defparameter +hvac-mapping+
  (page:value-mapping 0 "Auto" 1 "Komfort" 2 "Standby"))

(test value-mapping--is-a-hash-table
  ;; not an alist: item tags go through jzon, which signals on one -- a single
  ;; alist tag would fail the whole items listing
  (is-true (hash-table-p +hvac-mapping+))
  (is (string= "Komfort" (gethash 1 +hvac-mapping+))))

(test widget--value-shows-mapped-label-instead-of-the-code
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Betriebsmodus" :name "hvac.mode" :type-hint "INTEGER"
                    :value 1 :tags (%tags :ui-mapping +hvac-mapping+))
          (ui-renderer:render-widget (page:value 'mode) :owner body)
          (is-true (js~ "Komfort"))
          (is-false (js~ ">1</div>")))))))

(test widget--value-unmapped-code-shows-itself
  ;; an unlisted code must stay visible rather than read as a known mode
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Betriebsmodus" :name "hvac.mode" :type-hint "INTEGER"
                    :value 9 :tags (%tags :ui-mapping +hvac-mapping+))
          (ui-renderer:render-widget (page:value 'mode) :owner body)
          (is-true (js~ ">9</div>"))
          (is-false (js~ "Komfort")))))))

(test render-item--card-row-shows-mapped-label
  ;; the itemgroup card path, where a coded item has no widget to carry a format
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-item
         :owner
         (make-item-ht :label "Betriebsmodus" :name "hvac.mode"
                       :type-hint "INTEGER" :value 2
                       :tags (%tags :ui-mapping +hvac-mapping+))
         body)
        (is-true (js~ "Standby"))))))

(test render-item--mapping-does-not-disarm-a-writable-boolean
  ;; a switch has no text for a mapping to replace, so it stays a toggle
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-item
         :owner
         (make-item-ht :label "Lamp" :name "switch.lamp" :type-hint "BOOLEAN" :value t
                       :tags (%tags :ui-mapping (page:value-mapping t "An" nil "Aus")))
         body)
        (is-true (js~ "checkbox"))
        (is-false (js~ "An</div>"))))))

(test render-item--readonly-boolean-mapping-replaces-on-off
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-item
         :owner
         (make-item-ht :label "Frost" :name "s.frost" :type-hint "BOOLEAN" :value t
                       :tags (%tags :ui-readonly t
                                    :ui-mapping (page:value-mapping t "Frostschutz"
                                                                    nil "Normal")))
         body)
        (is-true (js~ "Frostschutz"))
        (is-false (js~ ">ON</div>"))))))

(test render-item--non-hash-table-mapping-is-ignored-not-applied
  ;; an alist would render here but fail the items API, so it must not work
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-item
         :owner
         (make-item-ht :label "Betriebsmodus" :name "hvac.mode"
                       :type-hint "INTEGER" :value 1
                       :tags (%tags :ui-mapping '((1 . "Komfort"))))
         body)
        (is-false (js~ "Komfort"))
        (is-true (js~ ">1</div>"))))))

(test widget--value-boolean-renders-on-off
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Door" :name "s.door" :type-hint "BOOLEAN" :value t)
          (ui-renderer:render-widget (page:value 'door) :owner body)
          (is-true (js~ "boolean-true"))
          (is-true (js~ "ON</div>")))))))

(test widget--missing-item-renders-placeholder
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-mocks ()
          (answer hab:get-item nil)
          (ui-renderer:render-widget (page:toggle 'no-such-item) :owner body)
          (is-true (js~ "widget-missing"))
          (is-true (js~ "Unknown item")))))))

(test widget--text-input-renders-and-binds-change
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Note" :name "s.note" :type-hint "STRING" :value "hello")
          (ui-renderer:render-widget (page:text-input 'note) :owner body)
          (is-true (js~ "widget-text-input"))
          (is-true (js~ "hello"))
          (is-true (js~ "on('change'")))))))

(test widget--number-input-with-bounds
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Limit" :name "s.limit" :type-hint "INTEGER" :value 42)
          (ui-renderer:render-widget (page:number-input 'limit :min 0 :max 100 :step 2)
                                     :owner body)
          (is-true (js~ "widget-number-input"))
          (is-true (js~ "prop('min'"))
          (is-true (js~ "prop('max'"))
          (is-true (js~ "attr('step'")))))))

(test widget--slider-renders-range-input-with-value-label
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Dimmer" :name "s.dimmer" :type-hint "INTEGER" :value 50)
          (ui-renderer:render-widget (page:slider 'dimmer :min 0 :max 100 :step 5)
                                     :owner body)
          (is-true (js~ "widget-slider-wrap"))
          (is-true (js~ "slider-value"))
          (is-true (js~ "50"))                    ; initial label content
          (is-true (js~ "range"))
          (is-true (js~ "prop('min'"))
          (is-true (js~ "prop('max'"))
          (is-true (js~ "attr('step'"))
          ;; client-side drag wiring: live label + drag-active flag that
          ;; suppresses server echoes while the user drags
          (is-true (js~ "on('pointerdown touchstart'"))
          (is-true (js~ "on('input'"))
          (is-true (js~ "on('change'")))))))

(test widget--slider-echo-suppressed-while-dragging
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Dimmer" :name "s.dimmer" :type-hint "INTEGER" :value 50)
          (ui-renderer:render-widget (page:slider 'dimmer) :owner body)
          (setf *captured* nil)
          (let ((new-state (make-hash-table :test #'equal)))
            (setf (gethash "value" new-state) 75
                  (gethash "timestamp" new-state) 0)
            (ui-renderer:call-item-value-update-fun "s.dimmer" new-state))
          ;; the label always updates ...
          (is-true (js~ "text('75')"))
          ;; ... the input only when the drag-active flag is not set
          (is-true (js~ "dataset.act"))
          (is-true (js~ "el.value='75'")))))))

(test widget--slider-float-echo-carries-no-reader-exponent-marker
  ;; Regression: the item-change echo runs on an actor thread where
  ;; *read-default-float-format* may not be single-float; a single-float then
  ;; prints as "87.0f0" via ~a, which JS cannot parse -- the range input
  ;; silently reverted to its midpoint ("slider plops back to the middle").
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Energy" :name "s.energy" :type-hint "FLOAT" :value 2.0)
          (ui-renderer:render-widget (page:slider 'energy) :owner body)
          (setf *captured* nil)
          (let ((new-state (make-hash-table :test #'equal))
                (*read-default-float-format* 'double-float))
            (setf (gethash "value" new-state) 87.0
                  (gethash "timestamp" new-state) 0)
            (ui-renderer:call-item-value-update-fun "s.energy" new-state))
          (is-true (js~ "el.value='87.0'"))
          (is-false (js~ "87.0f0")))))))

(test widget--setpoint-renders-buttons-and-value
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Target" :name "s.target" :type-hint "FLOAT" :value 21.0)
          (ui-renderer:render-widget (page:setpoint 'target :min 15.0 :max 25.0 :step 0.5)
                                     :owner body)
          (is-true (js~ "widget-setpoint"))
          (is-true (js~ "setpoint-btn"))
          (is-true (js~ "setpoint-value"))
          (is-true (js~ "21.00"))
          (is-true (js~ "on('click'")))))))

(test widget--selection-renders-options
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Mode" :name "s.mode" :type-hint "STRING" :value "auto")
          (ui-renderer:render-widget
           (page:selection 'mode :choices '(("auto" . "Automatic") ("off" . "Off")))
           :owner body)
          (is-true (js~ "widget-selection"))
          (is-true (js~ "Automatic"))
          (is-true (js~ "Off"))
          (is-true (js~ "on('change'")))))))

;; ----------------------------------------------------------------------------
;; button -- the one widget that is not item-bound: it runs an action.
;; ----------------------------------------------------------------------------

(test widget--button-renders-caption-label-and-click-binding
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-widget
         (page:button "Auf" (lambda ()) :label "Wohnzimmer") :owner body)
        (is-true (js~ "widget widget-button-row"))
        (is-true (js~ "widget-label"))
        (is-true (js~ "Wohnzimmer"))
        (is-true (js~ "<button"))
        (is-true (js~ "widget-button"))
        (is-true (js~ "Auf"))
        (is-true (js~ "on('click'"))))))

(test widget--button-without-label-renders-no-label-div
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-widget (page:button "Öffnen" (lambda ())) :owner body)
        (is-true (js~ "widget-button"))
        (is-false (js~ "widget-label"))))))

(test widget--button-click-runs-the-action
  (with-fixture render-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (presses 0)
             (btn (ui-renderer::%create-button-control
                   (clog:create-div body)
                   (page:button "Auf" (lambda () (incf presses))))))
        (is-true (fire-click btn))
        (is (= 1 presses))
        ;; a momentary command must be repeatable -- unlike an item-bound
        ;; control, which is a no-op when the value does not change
        (fire-click btn)
        (is (= 2 presses))))))

(defvar *action-result* nil
  "What the button action under test last recorded.")

(test widget--button-action-symbol-is-resolved-on-click
  ;; a symbol action is late-bound: redefining the function it names takes
  ;; effect without re-defining the page
  (with-fixture render-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (btn (ui-renderer::%create-button-control
                   (clog:create-div body)
                   (page:button "Auf" 'button-test-action))))
        (setf (fdefinition 'button-test-action) (lambda () (setf *action-result* :first)))
        (fire-click btn)
        (is (eq :first *action-result*))
        (setf (fdefinition 'button-test-action) (lambda () (setf *action-result* :second)))
        (fire-click btn)
        (is (eq :second *action-result*))))))

(test widget--failing-button-action-does-not-escape
  (with-fixture render-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (btn (ui-renderer::%create-button-control
                   (clog:create-div body)
                   (page:button "Boom" (lambda () (error "boom"))))))
        (finishes (fire-click btn))))))

(test widget--button-group-renders-all-buttons-in-one-row
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-widget
         (page:button-group "Wohnzimmer"
                            (page:button "Auf" 'jal-up)
                            (page:button "Stopp" 'jal-stop)
                            (page:button "Ab" 'jal-down))
         :owner body)
        (is (= 1 (%js-count "widget widget-button-row")))
        (is (= 1 (%js-count "widget-buttons")))
        (is (= 3 (%js-count "<button")))
        (is-true (js~ "Wohnzimmer"))
        (is-true (js~ "Auf"))
        (is-true (js~ "Stopp"))
        (is-true (js~ "Ab"))))))

;; ----------------------------------------------------------------------------
;; section -- container card with recursive children.
;; ----------------------------------------------------------------------------

(test widget--section-renders-header-and-children
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (with-item (:label "Temp" :name "s.temp" :type-hint "FLOAT" :value 1.0)
          (ui-renderer:render-widget
           (page:section "Climate" (page:value 'temp) (page:value 'temp))
           :owner body)
          (is-true (js~ "page-section"))
          (is-true (js~ "page-section-header"))
          (is-true (js~ "Climate"))
          (is-true (js~ "widget widget-value")))))))

;; ----------------------------------------------------------------------------
;; chart -- no historic persistence -> placeholder.
;; ----------------------------------------------------------------------------

(test widget--chart-without-persistence-shows-placeholder
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (hab:*persistences* nil))
        (with-mocks ()
          (answer hab:get-item :mock-item)
          (answer item:label "Temperature")
          (ui-renderer:render-widget (page:chart 'temp) :owner body)
          (is-true (js~ "widget-chart"))
          (is-true (js~ "chart-plot"))
          (is-true (js~ "No history available")))))))

;; ----------------------------------------------------------------------------
;; chart -- uPlot data from history and live appends, with :transform.
;; ----------------------------------------------------------------------------

(defun %chart-live-update (item-name value)
  "Dispatches a live value update as the item-change listener would."
  (let ((state (make-hash-table :test #'equal)))
    (setf (gethash "value" state) value
          (gethash "timestamp" state) 1755000000)
    (ui-renderer:call-item-value-update-fun item-name state)))

(defun %series (item-name label &rest values)
  "One %render-uplot series-data entry with one persisted item per value."
  (list item-name label
        (mapcar (lambda (v)
                  (persp:make-persisted-item :value v
                                             :timestamp (get-universal-time)))
                values)))

(test widget--chart-renders-history-and-appends-live
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (w (page:chart 'temp)))
        (ui-renderer::%render-uplot :owner w (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "new uPlot"))
        (is-true (js~ "[2.5]]"))
        (%chart-live-update "sensor.temp" 3.5)
        (is-true (js~ "push(3.5)"))))))

(test widget--chart-y-axis-is-sized-to-its-widest-label
  ;; uPlot's y axis is a fixed 50px, ~35px of it text once ticks and gap are
  ;; subtracted. It formats numbers in the *browser's* locale, so a German
  ;; one renders -20000 as "-20.000" -- which got clipped down to "20.000",
  ;; losing the minus sign and a leading digit on a power chart in W.
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (w (page:chart 'power)))
        (ui-renderer::%render-uplot :owner w (clog:create-div body)
                                    (list (%series "s.power" "Netz" -20000)))
        (is-true (js~ "axes:"))
        (is-true (js~ "size: ySize"))
        (is-true (js~ "measureText"))
        ;; sizing must converge: uPlot re-runs it until the size is stable
        (is-true (js~ "cycleNum > 1"))))))

(test widget--chart-transform-applies-to-history-and-live
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (w (page:chart 'temp :transform (lambda (v) (* 10 v)))))
        (ui-renderer::%render-uplot :owner w (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "[25.0]]"))
        (%chart-live-update "sensor.temp" 3.5)
        (is-true (js~ "push(35.0)"))))))

(test widget--chart-failing-transform-charts-gap
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (w (page:chart 'temp :transform (lambda (v)
                                              (declare (ignore v))
                                              (error "boom")))))
        (ui-renderer::%render-uplot :owner w (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "[null]]"))))))

(test widget--chart-height-defaults-and-is-configurable
  ;; the plot height reaches the browser twice: as uPlot's `height' option and
  ;; as the plot div's min-height, which holds the space open while the
  ;; history loads
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-uplot :owner (page:chart 'temp) (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "height: 220"))))
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-uplot :owner (page:chart 'temp :height 420)
                                    (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "height: 420"))
        (is-false (js~ "height: 220"))))))

(test widget--chart-line-width-defaults-and-is-configurable
  ;; uPlot strokes 1px unless told otherwise, which reads as hairlines on a
  ;; high-density display; every series gets the chart's width
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-uplot :owner (page:chart '(temp power))
                                    (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)
                                          (%series "sensor.power" "Power" 300)))
        (is (= 2 (count-js "width: 2")))))
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer::%render-uplot :owner (page:chart 'temp :line-width 3.5)
                                    (clog:create-div body)
                                    (list (%series "sensor.temp" "Temp" 2.5)))
        (is-true (js~ "width: 3.5"))
        (is-false (js~ "width: 2"))))))

(test widget--chart-height-sets-plot-div-min-height
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (hab:*persistences* nil))
        (with-mocks ()
          (answer hab:get-item :mock-item)
          (answer item:label "Temperature")
          (ui-renderer:render-widget (page:chart 'temp :height 420) :owner body)
          (is-true (js~ "min-height:420px")))))))

(test chart--collect-series-data--success
  (with-fixture render-env ()
    (with-mocks ()
      (answer item:name "sensor.temp")
      (answer persp:retrieve-range
        (list (persp:make-persisted-item :value 1.5 :timestamp 3900000000)))
      (multiple-value-bind (series-data any-ok)
          (ui-renderer::%collect-chart-series-data
           (page:chart 'temp) (list (list :mock-item "Temp")) :mock-persp)
        (is-true any-ok)
        (destructuring-bind (name label items) (first series-data)
          (is (string= "sensor.temp" name))
          (is (string= "Temp" label))
          (is (= 1 (length items))))))))

(test chart--collect-series-data--empty-series-is-ok
  (with-fixture render-env ()
    (with-mocks ()
      (answer item:name "sensor.temp")
      (answer persp:retrieve-range '())
      (multiple-value-bind (series-data any-ok)
          (ui-renderer::%collect-chart-series-data
           (page:chart 'temp) (list (list :mock-item "Temp")) :mock-persp)
        (is-true any-ok)
        (is (null (third (first series-data))))))))

(test chart--collect-series-data--all-failed
  (with-fixture render-env ()
    (with-mocks ()
      (answer item:name "sensor.temp")
      (answer persp:retrieve-range '(:error . "boom"))
      (multiple-value-bind (series-data any-ok)
          (ui-renderer::%collect-chart-series-data
           (page:chart 'temp) (list (list :mock-item "Temp")) :mock-persp)
        (is-false any-ok)
        (is (null (third (first series-data))))))))

(test widget--chart-multi-series-joins-tables-and-pads-live-appends
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body))
            (w (page:chart '(temp1 temp2))))
        (ui-renderer::%render-uplot :owner w (clog:create-div body)
                                    (list (%series "s.temp1" "Kessel" 1.5)
                                          (%series "s.temp2" "Puffer" 2.5)))
        (is-true (js~ "uPlot.join"))
        (is-true (js~ "Kessel"))
        (is-true (js~ "Puffer"))
        (is-true (js~ "spanGaps"))
        ;; a live update on the second series pads the first with null
        (%chart-live-update "s.temp2" 5.5)
        (is-true (js~ "c.data[1].push(null); c.data[2].push(5.5);"))))))

;; ----------------------------------------------------------------------------
;; page-link + navigation.
;; ----------------------------------------------------------------------------

(test widget--page-link-renders-target-label-and-click
  (with-fixture render-env ()
    (page:defpage 'cellar "Cellar" :path "/cellar")
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-widget (page:page-link 'cellar) :owner body)
        (is-true (js~ "page-nav-link"))
        (is-true (js~ "Cellar"))
        (is-true (js~ "on('click'"))))))

(test widget--page-link-to-unknown-page-renders-placeholder
  (with-fixture render-env ()
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-widget (page:page-link 'nowhere) :owner body)
        (is-true (js~ "widget-missing"))
        (is-true (js~ "Unknown page"))))))

(test navigate-to-page--pushes-history-and-renders-with-back-button
  (with-fixture render-env ()
    (page:defpage 'cellar "Cellar" :path "/cellar" :title "The cellar")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:navigate-to-page (page:get-page 'cellar) ctx)
        (is (= 1 (ui-renderer:nav-context-depth ctx)))
        (is-true (js~ "history.pushState"))
        (is-true (js~ "/cellar"))
        ;; navigated view carries a back button (depth > 0)
        (is-true (js~ "back-button"))
        (is-true (js~ "The cellar"))))))

;; ----------------------------------------------------------------------------
;; render-page -- title, heading, no back button at depth 0.
;; ----------------------------------------------------------------------------

(test render-page--title-heading-and-no-back-button-on-entry
  (with-fixture render-env ()
    (page:defpage 'wall "Wall panel" :path "/wall" :title "Ground floor")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-page (page:get-page 'wall) ctx)
        (is-true (js~ "header-line"))
        (is-true (js~ "Ground floor"))
        (is-false (js~ "back-button"))))))

(defun %make-group-ht (&optional (label "Group"))
  "A mock itemgroup hash-table shaped like itemgroup-ext:itemgroup-to-ht output."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "label" ht) label
          (gethash "items" ht) #())
    ht))

(test render-page--consecutive-itemgroup-refs-share-one-grid
  (with-fixture render-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms"
      (page:itemgroup-ref 'g1)
      (page:itemgroup-ref 'g2)
      (page:itemgroup-ref 'g3))
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (with-mocks ()
          (answer hab:get-itemgroup :mock-group)
          (answer itemgroup-ext:itemgroup-to-ht (%make-group-ht))
          (ui-renderer:render-page (page:get-page 'rooms) ctx)
          (is (= 1 (%js-count "itemgroups-container")))
          (is (= 3 (%js-count "itemgroup-card"))))))))

(test render-page--refs-split-by-other-widget-get-separate-grids
  (with-fixture render-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms"
      (page:itemgroup-ref 'g1)
      (page:section "In between")
      (page:itemgroup-ref 'g2))
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (with-mocks ()
          (answer hab:get-itemgroup :mock-group)
          (answer itemgroup-ext:itemgroup-to-ht (%make-group-ht))
          (ui-renderer:render-page (page:get-page 'rooms) ctx)
          (is (= 2 (%js-count "itemgroups-container")))
          (is-true (js~ "In between")))))))

(test render-page--without-title-renders-no-heading
  (with-fixture render-env ()
    (page:defpage 'wall "Wall panel" :path "/wall")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-page (page:get-page 'wall) ctx)
        (is-false (js~ "header-line"))))))

;; ----------------------------------------------------------------------------
;; render-not-found -- lists links to all defined pages.
;; ----------------------------------------------------------------------------

(test render-not-found--lists-defined-pages
  (with-fixture render-env ()
    (page:defpage 'wall "Wall panel" :path "/wall")
    (page:defpage 'cellar "Cellar" :path "/cellar")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-not-found ctx "/nope")
        (is-true (js~ "Page not found"))
        (is-true (js~ "/nope"))
        (is-true (js~ "Wall panel"))
        (is-true (js~ "Cellar"))))))

;; ----------------------------------------------------------------------------
;; registry -- callbacks of dead connections are pruned on dispatch.
;; ----------------------------------------------------------------------------

(test value-update-registry--dead-connection-callbacks-are-pruned
  (with-fixture render-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body))
             (called nil))
        (ui-renderer:set-on-value-update ctx "item.x"
                                         (lambda (state)
                                           (declare (ignore state))
                                           (setf called t)))
        ;; the connection goes away -> dispatch must not run and must prune
        (invalidate-connection)
        (ui-renderer:call-item-value-update-fun "item.x" :state)
        (is-false called)
        (let ((owners (gethash "item.x" ui-renderer:*item-value-form-update-funs*)))
          (is (zerop (hash-table-count owners))))))))

;; ----------------------------------------------------------------------------
;; URL dispatch in chipi-ui.main -- page, overview fallback, not-found.
;; ----------------------------------------------------------------------------

(test dispatch-path--no-pages-renders-overview
  ;; the stubbed connection reports no pathname -> treated as root path
  (with-fixture render-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (chipi-ui.main::%dispatch-path ctx)
        (is-true (js~ "Chipi Home Automation Dashboard"))))))

(test dispatch-path--page-on-root-path-wins-over-overview
  (with-fixture render-env ()
    (page:defpage 'home "Home" :path "/" :title "My home")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (chipi-ui.main::%dispatch-path ctx)
        (is-true (js~ "My home"))
        (is-false (js~ "Chipi Home Automation Dashboard"))))))
