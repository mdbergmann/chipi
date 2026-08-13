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
