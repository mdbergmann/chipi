(defpackage :chipi-ui.page-renderer
  (:use :cl :clog)
  (:nicknames :ui-renderer)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:itemsc
                #:update-item-value)
  (:import-from #:itemgroupsc
                #:retrieve-top-level-itemgroups)
  (:export #:nav-context
           #:make-nav-context
           #:nav-context-body
           #:nav-context-container
           #:nav-context-depth
           #:render-page
           #:render-overview
           #:render-not-found
           #:render-home
           #:render-settings
           #:home-page
           #:render-widget
           #:navigate-to-page
           #:set-on-value-update
           #:clear-value-update-funs
           #:call-item-value-update-fun
           #:*item-value-form-update-funs*))

(in-package :chipi-ui.page-renderer)

;; ---------------------------------------------------------------------------
;; navigation context
;; ---------------------------------------------------------------------------

(defstruct nav-context
  "Per-connection rendering/navigation state.  Every browser connection gets
its own context; it owns the value-update callbacks of the currently rendered
view and carries the navigation depth for the back button."
  body        ; the connection's clog body
  container   ; the top-level container div views render into
  (depth 0))  ; number of in-app pushState navigations

;; ---------------------------------------------------------------------------
;; item value-update registry
;; ---------------------------------------------------------------------------

(defvar *item-value-form-update-funs* (make-hash-table :test #'equal)
  "Maps an item name to a nested hash of owner -> list of value-update
callbacks.  The owner is the `nav-context' of one browser connection: several
connections render components for the same item concurrently, and re-rendering
or closing one connection must only drop that connection's callbacks.")

(defun set-on-value-update (owner item-name fun)
  "Registers FUN as a value-update callback for ITEM-NAME on behalf of OWNER.
Additive per owner: a single item renders several components that each react
to a change (e.g. the value display and the timestamp display), so more than
one callback is registered per item and all of them run on a change."
  (let ((owner-funs (or (gethash item-name *item-value-form-update-funs*)
                        (setf (gethash item-name *item-value-form-update-funs*)
                              (make-hash-table :test #'eq)))))
    (push fun (gethash owner owner-funs))))

(defun clear-value-update-funs (owner)
  "Drops all callbacks registered by OWNER; called before OWNER re-renders."
  (maphash (lambda (item-name owner-funs)
             (declare (ignore item-name))
             (remhash owner owner-funs))
           *item-value-form-update-funs*))

(defun %owner-alive-p (owner)
  "Whether the owner's browser connection is still alive."
  (if (nav-context-p owner)
      (validp (nav-context-body owner))
      t))

(defun call-item-value-update-fun (item-name updated-item-state)
  "Runs all callbacks registered for ITEM-NAME across all owners.  Callbacks
of owners whose connection has gone away are dropped instead of run."
  (let ((owners (gethash item-name *item-value-form-update-funs*)))
    (if owners
        (maphash (lambda (owner funs)
                   (if (%owner-alive-p owner)
                       (dolist (fun funs)
                         (funcall fun updated-item-state))
                       (remhash owner owners)))
                 owners)
        (log:debug "No update function registered for: ~a" item-name))))

;; ---------------------------------------------------------------------------
;; formatting helpers
;; ---------------------------------------------------------------------------

(defun %format-timestamp (timestamp)
  (local-time:format-rfc1123-timestring nil (local-time:unix-to-timestamp timestamp)))

(defun %format-value (value type-hint)
  (cond
    ((string= "FLOAT" type-hint) (format nil "~,2f" value))
    ((string= "INTEGER" type-hint) (format nil "~a" value))
    ((string= "STRING" type-hint) (if value value ""))
    (t (if value (format nil "~a" value) ""))))

(defun %format-type-hint (type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) "Switch")
    ((string= "FLOAT" type-hint) "Decimal Number")
    ((string= "INTEGER" type-hint) "Whole Number")
    ((string= "STRING" type-hint) "Text")
    (t "Undefined type")))

(defun %mapped-label (value mapping)
  "The text MAPPING gives VALUE, or `nil' when it has none.

MAPPING is an item's `:ui-mapping' tag, built by `page:value-mapping': a
hash-table of value -> label for items whose value is a code rather than a
quantity -- an HVAC mode, a fault number.  Only the codes listed are mapped;
anything else falls through to the normal formatting, so an unexpected value
shows as itself instead of silently reading as one of the known ones."
  (when mapping
    (if (hash-table-p mapping)
        (gethash value mapping)
        (progn
          (log:warn "Ignoring :ui-mapping ~s -- expected a hash-table from ~
`page:value-mapping'. An alist here would also fail the items API, which ~
serializes tags to JSON."
                    mapping)
          nil))))

(defun %format-widget-value (value type-hint format &optional mapping)
  "Formats VALUE: the label MAPPING gives it, else the widget's format string
when given, else the type-hint based default formatting."
  (or (%mapped-label value mapping)
      (if (and format value (not (eq value 'cl:null)))
          (format nil format value)
          (%format-value value type-hint))))

(defun %ext-value (value)
  "Maps the external JSON null marker back to `nil' for display purposes."
  (if (eq value 'cl:null) nil value))

(defun %ui-readonly-p (tags)
  "Returns T if the item has the :ui-readonly tag."
  (when (and tags (hash-table-p tags))
    (multiple-value-bind (val present-p)
        (gethash :ui-readonly tags)
      (declare (ignore val))
      present-p)))

(defun %ui-mapping (tags)
  "The item's `:ui-mapping' tag -- a hash-table of value -> label, built by
`page:value-mapping' -- or `nil'."
  (when (and tags (hash-table-p tags))
    (gethash :ui-mapping tags)))

(defun %itemgroup-link-p (itemg)
  "Returns T if the itemgroup should render as a link (has :ui-link tag)."
  (let ((tags (gethash "tags" itemg)))
    (when (and tags (hash-table-p tags))
      (multiple-value-bind (val present-p)
          (gethash :ui-link tags)
        (declare (ignore val))
        present-p))))

(defun %parse-number (str type-hint)
  "Parses STR according to TYPE-HINT.  Returns `nil' if not parsable."
  (handler-case
      (if (string= "INTEGER" type-hint)
          (parse-integer str)
          (coerce (parse-float:parse-float str) 'single-float))
    (error ()
      (log:warn "Cannot parse ~s as ~a" str type-hint)
      nil)))

;; ---------------------------------------------------------------------------
;; value controls
;;
;; The shared building blocks between the widget renderers and the legacy
;; itemgroup card rendering: each creates one reactive control wired to the
;; item via `update-item-value' (input) and the value-update registry (output).
;; ---------------------------------------------------------------------------

(defun %create-boolean-display (owner parent item-name value)
  "A read-only ON/OFF display for a boolean item."
  (let ((value-div (create-div parent :class (format nil "item-value-display ~a"
                                                     (if value "boolean-true" "boolean-false"))
                                      :content (if value "ON" "OFF"))))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (gethash "value" updated-item-state)))
                             (setf (text value-div)
                                   (if updated-value "ON" "OFF"))
                             (if updated-value
                                 (progn (remove-class value-div "boolean-false")
                                        (add-class value-div "boolean-true"))
                                 (progn (remove-class value-div "boolean-true")
                                        (add-class value-div "boolean-false"))))))
    value-div))

(defun %set-checked (toggle-input value)
  "Sets the live checked state of a checkbox.  CLOG's `(setf checkedp)'
writes the 'checked' HTML attribute, which browsers ignore once the user has
interacted with the checkbox; the property reflects the state reliably."
  (jquery-execute toggle-input
                  (format nil "prop('checked',~a)" (if value "true" "false"))))

(defun %create-toggle-control (owner parent item-name value)
  "A switch control for a boolean item."
  (let* ((form-check (create-div parent :class "item-value-boolean"))
         (toggle-input (create-form-element form-check "checkbox"
                                            :role "switch"
                                            :class "item-value-boolean-input")))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (gethash "value" updated-item-state)))
                             (log:debug "Setting value: ~a on component: ~a"
                                        updated-value toggle-input)
                             (%set-checked toggle-input updated-value))))
    (%set-checked toggle-input value)
    (set-on-change toggle-input
                   (lambda (obj)
                     (let ((current-state (checkedp obj)))
                       (log:debug "Toggled (change): ~a = ~a" obj current-state)
                       (update-item-value item-name
                                          (item-ext:item-value-ext-to-internal current-state)))))
    toggle-input))

(defun %create-value-display (owner parent item-name value type-hint
                              &optional format mapping)
  "A read-only formatted display of the item's value."
  (let ((value-div (create-div parent :class "item-value-display"
                                      :content (%format-widget-value value type-hint
                                                                     format mapping))))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (gethash "value" updated-item-state)))
                             (log:debug "Setting value: ~a on component: ~a"
                                        updated-value value-div)
                             (setf (text value-div)
                                   (%format-widget-value updated-value type-hint
                                                         format mapping)))))
    value-div))

(defun %create-text-input-control (owner parent item-name value)
  "A text field writing its value to the item on change."
  (let ((input (create-form-element parent "text"
                                    :class "widget-text-input"
                                    :value (or (%ext-value value) ""))))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (%ext-value (gethash "value" updated-item-state))))
                             (setf (value input) (or updated-value "")))))
    (set-on-change input
                   (lambda (obj)
                     (update-item-value item-name
                                        (item-ext:item-value-ext-to-internal (value obj)))))
    input))

(defun %create-number-input-control (owner parent item-name value type-hint
                                     &key min max step)
  "A number entry field writing its parsed value to the item on change."
  (let ((input (create-form-element parent "number"
                                    :class "widget-number-input"
                                    :value (%format-value (%ext-value value) type-hint))))
    (when min (setf (minimum input) min))
    (when max (setf (maximum input) max))
    (when step (setf (attribute input "step") step))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (setf (value input)
                                 (%format-value (%ext-value (gethash "value" updated-item-state))
                                                type-hint))))
    (set-on-change input
                   (lambda (obj)
                     (when-let ((num (%parse-number (value obj) type-hint)))
                       (update-item-value item-name num))))
    input))

(defun %create-slider-control (owner parent item-name value type-hint
                               &key min max step)
  "A range slider with a value label to its left, writing its value to the
item on change.  Some browsers (Safari) fire change events continuously while
dragging; each one round-trips through the item and echoes back an update.
Applying such an echo mid-drag makes the thumb fight the user, so a
client-side pointer-active flag suppresses echoes while dragging, and the
label updates instantly from client-side input events instead."
  (let* ((wrapper (create-div parent :class "widget-slider-wrap"))
         (value-div (create-div wrapper :class "slider-value"
                                        :content (%format-value (%ext-value value) type-hint)))
         (input (create-form-element wrapper "range"
                                     :class "widget-slider")))
    (setf (minimum input) min)
    (setf (maximum input) max)
    (setf (attribute input "step") step)
    (setf (value input) (%js-number (or (%ext-value value) min)))
    ;; live label while dragging + the drag-active flag, all client-side
    (jquery-execute input
                    (format nil "on('pointerdown touchstart',function(){this.dataset.act='1';}).on('pointerup touchend blur',function(){delete this.dataset.act;}).on('input',function(){$('#~a').text(this.value);})"
                            (html-id value-div)))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (%ext-value (gethash "value" updated-item-state))))
                             (when (numberp updated-value)
                               (setf (text value-div)
                                     (%format-value updated-value type-hint))
                               (js-execute input
                                           (format nil "var el=document.getElementById('~a'); if(el && !el.dataset.act){el.value='~a';}"
                                                   (html-id input)
                                                   (%js-number updated-value)))))))
    (set-on-change input
                   (lambda (obj)
                     (when-let ((num (%parse-number (value obj) type-hint)))
                       (update-item-value item-name num))))
    wrapper))

(defun %create-setpoint-control (owner parent item-name value type-hint
                                 &key min max step)
  "A +/- stepper for a number item.  The displayed value only changes when the
item change comes back from the server, so it always reflects the item state."
  (let* ((wrapper (create-div parent :class "widget-setpoint"))
         (dec-btn (create-button wrapper :class "setpoint-btn" :content "&minus;"))
         (value-div (create-div wrapper :class "setpoint-value"
                                        :content (%format-value (%ext-value value) type-hint)))
         (inc-btn (create-button wrapper :class "setpoint-btn" :content "+"))
         (current (let ((v (%ext-value value)))
                    (if (numberp v) v (or min 0)))))
    (flet ((step-by (direction)
             (let ((new (+ current (* direction step))))
               (when min (setf new (max new min)))
               (when max (setf new (min new max)))
               (when (string= "INTEGER" type-hint)
                 (setf new (round new)))
               (update-item-value item-name new))))
      (set-on-click dec-btn (lambda (obj)
                              (declare (ignore obj))
                              (step-by -1)))
      (set-on-click inc-btn (lambda (obj)
                              (declare (ignore obj))
                              (step-by 1))))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (%ext-value (gethash "value" updated-item-state))))
                             (when (numberp updated-value)
                               (setf current updated-value))
                             (setf (text value-div)
                                   (%format-value updated-value type-hint)))))
    wrapper))

(defun %create-selection-control (owner parent item-name value choices)
  "A dropdown writing the selected choice value to the item on change."
  (let ((select (create-select parent :class "widget-selection")))
    (dolist (choice choices)
      (add-select-option select (car choice) (cdr choice)))
    (when-let ((v (%ext-value value)))
      (setf (value select) (format nil "~a" v)))
    (set-on-value-update owner item-name
                         (lambda (updated-item-state)
                           (let ((updated-value (%ext-value (gethash "value" updated-item-state))))
                             (setf (value select)
                                   (if updated-value (format nil "~a" updated-value) "")))))
    (set-on-change select
                   (lambda (obj)
                     (update-item-value item-name
                                        (item-ext:item-value-ext-to-internal (value obj)))))
    select))

;; ---------------------------------------------------------------------------
;; widget rendering
;; ---------------------------------------------------------------------------

(defgeneric render-widget (widget ctx parent)
  (:documentation "Renders WIDGET into the CLOG PARENT container.
CTX is the connection's `nav-context'; it owns the registered value-update
callbacks and carries navigation state."))

(defun %resolve-item-ht (widget)
  "Resolves the widget's item symbol to the item's hash-table representation,
or `nil' when no such item is defined."
  (when-let ((item (hab:get-item (page:item-widget-item-id widget))))
    (item-ext:item-to-ht item)))

(defun %render-missing (parent kind id)
  (log:warn "Unknown ~a referenced by widget: ~a" kind id)
  (create-div parent :class "widget widget-missing"
                     :content (format nil "Unknown ~a: ~a" kind id)))

(defun %render-item-row (widget ctx parent css-class control-fn)
  "Renders the common row (label + control) for an item-bound widget.
CONTROL-FN is called with the row container and the item hash-table."
  (let ((item-ht (%resolve-item-ht widget)))
    (if (null item-ht)
        (%render-missing parent "item" (page:item-widget-item-id widget))
        (let ((row (create-div parent :class (format nil "widget ~a" css-class))))
          (create-div row :class "widget-label"
                          :content (or (page:item-widget-label widget)
                                       (gethash "label" item-ht)))
          (funcall control-fn row item-ht)
          row))))

(defmacro %with-item-state ((item-ht name value type-hint) &body body)
  "Binds NAME, VALUE and TYPE-HINT from the item hash-table ITEM-HT."
  (let ((ht (gensym "HT")))
    `(let* ((,ht ,item-ht)
            (,name (gethash "name" ,ht))
            (,value (gethash "value" (gethash "item-state" ,ht)))
            (,type-hint (gethash "type-hint" ,ht)))
       (declare (ignorable ,value ,type-hint))
       ,@body)))

(defmethod render-widget ((w page:value-display) ctx parent)
  (%render-item-row w ctx parent "widget-value"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (let ((mapping (%ui-mapping (gethash "tags" item-ht))))
                          (if (and (string= "BOOLEAN" type-hint)
                                   (null (page:value-display-format w))
                                   (null mapping))
                              (%create-boolean-display ctx row name value)
                              (%create-value-display ctx row name value type-hint
                                                     (page:value-display-format w)
                                                     mapping)))))))

(defmethod render-widget ((w page:toggle) ctx parent)
  (%render-item-row w ctx parent "widget-toggle"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-toggle-control ctx row name value)))))

(defmethod render-widget ((w page:text-input) ctx parent)
  (%render-item-row w ctx parent "widget-text"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-text-input-control ctx row name value)))))

(defmethod render-widget ((w page:number-input) ctx parent)
  (%render-item-row w ctx parent "widget-number"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-number-input-control ctx row name value type-hint
                                                      :min (page:number-input-min w)
                                                      :max (page:number-input-max w)
                                                      :step (page:number-input-step w))))))

(defmethod render-widget ((w page:slider) ctx parent)
  (%render-item-row w ctx parent "widget-slider-row"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-slider-control ctx row name value type-hint
                                                :min (page:slider-min w)
                                                :max (page:slider-max w)
                                                :step (page:slider-step w))))))

(defmethod render-widget ((w page:setpoint) ctx parent)
  (%render-item-row w ctx parent "widget-setpoint-row"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-setpoint-control ctx row name value type-hint
                                                  :min (page:setpoint-min w)
                                                  :max (page:setpoint-max w)
                                                  :step (page:setpoint-step w))))))

(defmethod render-widget ((w page:selection) ctx parent)
  (%render-item-row w ctx parent "widget-selection-row"
                    (lambda (row item-ht)
                      (%with-item-state (item-ht name value type-hint)
                        (%create-selection-control ctx row name value
                                                   (page:selection-choices w))))))

(defun %run-button-action (w)
  "Runs the button's action.  CLOG dispatches every browser event on its own
thread, so a slow action (a bus write waiting for its ack) does not stall the
connection; an error in it is logged rather than left to CLOG's generic event
handler, which would only print it."
  (let ((caption (page:button-caption w)))
    (log:info "Button pressed: ~a" caption)
    (handler-case (funcall (page:button-action w))
      (error (c)
        (log:warn "Button action failed (~a): ~a" caption c)))))

(defun %create-button-control (parent w)
  "The button of the `button' widget W, wired to run its action on click."
  (let ((btn (create-button parent :class "widget-button"
                                   :content (page:button-caption w))))
    (set-on-click btn
                  (lambda (obj)
                    (declare (ignore obj))
                    (%run-button-action w)))
    btn))

(defun %render-button-row (parent label control-fn)
  "Renders the common row (optional label + buttons) of the button widgets.
Unlike `%render-item-row' this resolves no item: buttons are not item-bound."
  (let ((row (create-div parent :class "widget widget-button-row")))
    (when label
      (create-div row :class "widget-label" :content label))
    (funcall control-fn row)
    row))

(defmethod render-widget ((w page:button) ctx parent)
  (declare (ignore ctx))
  (%render-button-row parent (page:button-label w)
                      (lambda (row)
                        (%create-button-control row w))))

(defmethod render-widget ((w page:button-group) ctx parent)
  (declare (ignore ctx))
  (%render-button-row parent (page:button-group-label w)
                      (lambda (row)
                        (let ((btns (create-div row :class "widget-buttons")))
                          (dolist (b (page:button-group-buttons w))
                            (%create-button-control btns b))))))

(defmethod render-widget ((w page:section) ctx parent)
  (let ((card (create-div parent :class "page-section")))
    (create-div card :class "page-section-header"
                     :content (page:section-label w))
    (let ((body (create-div card :class "page-section-body")))
      (dolist (child (page:section-children w))
        (render-widget child ctx body)))
    card))

(defmethod render-widget ((w page:row) ctx parent)
  (let* ((children (page:row-children w))
         ;; the column count rides in a custom property rather than in
         ;; `grid-template-columns' directly: an inline template could not be
         ;; overridden by the stylesheet's narrow-screen rule without
         ;; `!important'
         (columns (or (page:row-columns w) (max 1 (length children))))
         (grid (create-div parent :class "widget-row"
                                  :style (format nil "--cols:~a" columns))))
    (dolist (child children)
      (render-widget child ctx grid))
    grid))

(defmethod render-widget ((w page:page-link) ctx parent)
  (let ((target (page:get-page (page:page-link-page-id w))))
    (if (null target)
        (%render-missing parent "page" (page:page-link-page-id w))
        (let ((link-div (create-div parent :class "page-nav-link"
                                           :content (or (page:page-link-label w)
                                                        (page:page-label target)))))
          (set-on-click link-div
                        (lambda (obj)
                          (declare (ignore obj))
                          (navigate-to-page target ctx)))
          link-div))))

(defun %render-itemgroup-ref (w ctx grid)
  "Renders the itemgroup-ref W as a card into GRID (an itemgroups-container)."
  (let* ((group-id (page:itemgroup-ref-group-id w))
         (group (hab:get-itemgroup group-id)))
    (if (null group)
        (%render-missing grid "itemgroup" group-id)
        (%render-itemgroup ctx (itemgroup-ext:itemgroup-to-ht group) grid))))

(defmethod render-widget ((w page:itemgroup-ref) ctx parent)
  (let ((grid (create-div parent :class "itemgroups-container")))
    (%render-itemgroup-ref w ctx grid)
    grid))

;; ---------------------------------------------------------------------------
;; chart widget
;; ---------------------------------------------------------------------------

(defmethod render-widget ((w page:chart) ctx parent)
  (let ((series (loop :for (id . label) :in (page:chart-series w)
                      :for item = (hab:get-item id)
                      :if item
                        :collect (list item (or label (item:label item)))
                      :else
                        :do (log:warn "Chart references unknown item: ~a" id))))
    (if (null series)
        (%render-missing parent "item" (page:item-widget-item-id w))
        (let ((row (create-div parent :class "widget widget-chart")))
          (create-div row :class "widget-label"
                          :content (or (page:item-widget-label w)
                                       (second (first series))))
          ;; The stylesheet's min-height keeps the plot area from collapsing
          ;; while the history loads; it has to track the widget's height, or
          ;; a chart shorter than the default would still reserve 220px.
          (let ((plot-div (create-div row :class "chart-plot"
                                          :style (format nil "min-height:~apx"
                                                         (page:chart-height w))))
                (persistence (%chart-persistence w)))
            (if (null persistence)
                (progn
                  (log:warn "No historic persistence for chart on item: ~a"
                            (page:item-widget-item-id w))
                  (create-div plot-div :class "chart-empty"
                                       :content "No history available"))
                (%load-and-render-chart ctx w series plot-div persistence)))
          row))))

(defun %chart-persistence (w)
  "The persistence to load chart data from: the one named on the widget, or
the first defined historic persistence."
  (if (page:chart-persistence w)
      (hab:get-persistence (page:chart-persistence w))
      (when hab:*persistences*
        (loop :for p :being :the :hash-value :of hab:*persistences*
              :if (typep p 'persp:base-historic-persistence)
                :return p))))

(defun %fetch-chart-series (persistence item range)
  "Fetches ITEM's history for RANGE by calling the persistence's
`retrieve-range' directly on the calling thread instead of asking the
persistence actor via `persp:fetch': an ask-future can intermittently lose
its completion (the actor replies but the future never completes), which
stalled chart rendering.  Historic persistence reads are side-effect free,
so bypassing the actor mailbox is safe here.
Returns (values persisted-items ok-p); nil/not-ok when the fetch errored or
returned something unusable (each logged)."
  (let ((result (handler-case (persp:retrieve-range persistence item range)
                  (error (e) `(:error . ,e)))))
    (cond
      ((and (consp result) (eq :error (car result)))
       (log:warn "Chart data fetch failed for item ~a: ~a"
                 (item:name item) (cdr result))
       nil)
      ((listp result)
       (values result t))
      (t
       (log:warn "Chart data fetch for item ~a returned: ~a"
                 (item:name item) result)
       nil))))

(defun %collect-chart-series-data (w series persistence)
  "Fetches the history of every chart series -- a list of (item label) --
blocking until all are in.  Returns (values series-data any-ok) where
SERIES-DATA is a list of (item-name series-label persisted-items) and
ANY-OK is t when at least one fetch succeeded."
  (let ((any-ok nil))
    (values
     (loop :for (item series-label) :in series
           :collect (list (item:name item)
                          series-label
                          (multiple-value-bind (items ok)
                              (%fetch-chart-series
                               persistence item (page:chart-range w))
                            (when ok (setf any-ok t))
                            items)))
     any-ok)))

(defun %load-and-render-chart (ctx w series plot-div persistence)
  "Fetches the history of every chart series -- a list of (item label) -- in
a background thread (so page rendering is not blocked), then renders the
plot into PLOT-DIV."
  (bt2:make-thread
   (lambda ()
     (multiple-value-bind (series-data any-ok)
         (%collect-chart-series-data w series persistence)
       (if any-ok
           (%render-uplot ctx w plot-div series-data)
           (create-div plot-div :class "chart-empty"
                                :content "Failed to load history"))))
   :name "chipi-ui-chart-loader"))

(defun %universal-to-unix (universal-ts)
  (local-time:timestamp-to-unix (local-time:universal-to-timestamp universal-ts)))

(defun %chart-y-value (ext-value)
  "Maps an external item value to a uPlot y-value: numbers pass through,
booleans chart as 1/0, anything else as a gap (null)."
  (cond
    ((numberp ext-value) ext-value)
    ((eq ext-value t) 1)
    ((null ext-value) 0)
    (t "null")))

(defun %chart-transformed-y (transform ext-value)
  "The uPlot y-value for EXT-VALUE with the chart's TRANSFORM applied.
A failing transform charts the point as a gap instead of breaking the page."
  (let ((y (%chart-y-value ext-value)))
    (if (and transform (numberp y))
        (handler-case (funcall transform y)
          (error (c)
            (log:warn "Chart transform failed for value ~a: ~a" y c)
            "null"))
        y)))

(defun %js-number (value)
  "Formats VALUE as a JS number literal.  Floats print via ~f: with ~a, a
float whose type differs from `*read-default-float-format*' (which varies by
thread) prints with a CL reader exponent marker (\"87.0f0\", \"1.5d8\") that
JS cannot parse — and e.g. a range input silently reverts to its default when
assigned such a string."
  (cond
    ((integerp value) (format nil "~a" value))
    ((realp value) (format nil "~f" value))
    (t value)))

(defun %js-escape (str)
  "Escapes backslashes and single quotes for embedding STR in a JS string."
  (with-output-to-string (out)
    (loop :for ch :across (or str "")
          :do (progn
                (when (or (char= ch #\\) (char= ch #\'))
                  (write-char #\\ out))
                (write-char ch out)))))

(defun %chart-points (persisted-items transform)
  "Extracts (timestamps values) as two lists of JS literals from the persisted
items, skipping entries without a proper timestamp (e.g. aggregates)."
  (let ((timestamps '())
        (values '()))
    (dolist (p persisted-items)
      (let ((ts (persp:persisted-item-timestamp p)))
        (when (integerp ts)
          (push (%universal-to-unix ts) timestamps)
          (push (%js-number
                 (%chart-transformed-y
                  transform
                  (item-ext:item-value-internal-to-ext
                   (%ext-value (persp:persisted-item-value p)))))
                values))))
    (list (nreverse timestamps) (nreverse values))))

(defparameter *chart-series-colors*
  '("#0d6efd" "#dc3545" "#198754" "#fd7e14" "#6f42c1"
    "#20c997" "#d63384" "#0dcaf0" "#6c757d" "#ffc107")
  "Stroke colors assigned to chart series in definition order (cycled).")

(defun %uplot-series-def (label color line-width chart-type single-p)
  "The JS series definition object for one chart series.  LINE-WIDTH is the
stroke width in CSS pixels.  A single series keeps the classic look (area
fill, gaps stay gaps); with several series the gaps that timestamp-alignment
introduces are spanned instead."
  (format nil "{ label: '~a', stroke: '~a', width: ~a~a~a }"
          (%js-escape label) color (%js-number line-width)
          (if single-p ", fill: 'rgba(13,110,253,0.08)'" ", spanGaps: true")
          (if (eq chart-type :bar)
              ", paths: uPlot.paths.bars({size: [0.6, 100]})"
              "")))

(defun %uplot-init-js (plot-id chart-type series-defs tables height)
  "The JS that instantiates the uPlot chart once the uPlot library is loaded.
SERIES-DEFS are JS series objects, TABLES one [[timestamps],[values]] JS
literal per series; several tables are timestamp-aligned via uPlot.join.
HEIGHT is the plot height in CSS pixels; the width follows the container, both
at init and afterwards via a `ResizeObserver'.

The y axis is sized to its widest tick label instead of uPlot's fixed 50px.
That default leaves ~35px for text once ticks and gap are subtracted, which
silently clips wider labels -- uPlot formats numbers with the *browser's*
locale, so 20000 renders as \"20.000\" and -20000 as \"-20.000\", and a power
chart in W lost its minus signs and leading digits."
  (declare (ignore chart-type))
  (format nil "(function() {
  function ySize(u, values, axisIdx, cycleNum) {
    var axis = u.axes[axisIdx];
    // uPlot re-runs sizing until it converges; bail out after the first pass
    if (cycleNum > 1) { return axis._size; }
    var size = (axis.ticks.size || 0) + axis.gap;
    var longest = '';
    (values || []).forEach(function(v) {
      v = String(v);
      if (v.length > longest.length) { longest = v; }
    });
    if (longest !== '') {
      // axis.font is [css-font, size] and already scaled by the pixel ratio,
      // so measureText returns device pixels -- convert back to CSS pixels
      u.ctx.font = axis.font[0];
      size += u.ctx.measureText(longest).width / (window.devicePixelRatio || 1);
    }
    return Math.max(50, Math.ceil(size));
  }
  function init() {
    if (!window.uPlot) { setTimeout(init, 150); return; }
    var el = document.getElementById('~a');
    if (!el) { return; }
    var tables = [~{~a~^,~}];
    var data = tables.length === 1 ? tables[0] : uPlot.join(tables);
    var opts = {
      width: el.clientWidth || 600,
      height: ~a,
      axes: [
        {},
        { size: ySize }
      ],
      series: [
        {},
        ~{~a~^,
        ~}
      ]
    };
    window.chipiCharts = window.chipiCharts || {};
    var u = new uPlot(opts, data, el);
    window.chipiCharts['~a'] = u;
    // uPlot takes a fixed pixel width, so without this a chart keeps the width
    // it was measured at: in a `row' that breaks as soon as the layout changes
    // shape -- rendered two-up and then narrowed past the stylesheet's
    // single-column breakpoint, the canvas would overflow its column.  Only
    // width is reacted to; height comes from the widget, and feeding our own
    // setSize back in as a resize would loop.  The first callback fires with
    // the current size, which the guard makes a no-op -- unless the plot was
    // measured at 0 (rendered while hidden) and fell back to 600, in which case
    // this is what repairs it.
    if (window.ResizeObserver) {
      var lastWidth = opts.width;
      new ResizeObserver(function() {
        var w = el.clientWidth;
        if (w > 0 && w !== lastWidth) {
          lastWidth = w;
          u.setSize({ width: w, height: opts.height });
        }
      }).observe(el);
    }
  }
  init();
})();"
          plot-id tables height series-defs plot-id))

(defun %uplot-append-js (plot-id ts series-count series-idx y-literal)
  "The JS that appends one live value: the new row carries Y-LITERAL in
series SERIES-IDX (1-based) and null in every other series column."
  (format nil "if (window.chipiCharts && window.chipiCharts['~a']) {
  var c = window.chipiCharts['~a'];
  c.data[0].push(~a);~{ c.data[~a].push(~a);~}
  c.setData(c.data);
}"
          plot-id plot-id ts
          (loop :for i :from 1 :to series-count
                :append (list i (if (= i series-idx) y-literal "null")))))

(defun %render-uplot (ctx w plot-div series-data)
  "Renders the uPlot chart for SERIES-DATA -- a list of
(item-name series-label persisted-items) -- and registers a live-append
callback per series item."
  (let* ((plot-id (html-id plot-div))
         (transform (page:chart-transform w))
         (single-p (= 1 (length series-data)))
         (series-count (length series-data))
         (tables (loop :for (nil nil persisted-items) :in series-data
                       :for points = (%chart-points persisted-items transform)
                       :collect (format nil "[[~{~a~^,~}],[~{~a~^,~}]]"
                                        (first points) (second points))))
         (series-defs (loop :for (nil label nil) :in series-data
                            :for idx :from 0
                            :collect (%uplot-series-def
                                      label
                                      (nth (mod idx (length *chart-series-colors*))
                                           *chart-series-colors*)
                                      (page:chart-line-width w)
                                      (page:chart-type w)
                                      single-p))))
    (js-execute plot-div
                (%uplot-init-js plot-id (page:chart-type w) series-defs tables
                                (page:chart-height w)))
    ;; live append on item change; timestamps in the update state are unix already
    (loop :with refresh = (page:chart-refresh w)
          :for (item-name nil nil) :in series-data
          :for series-idx :from 1
          :do (let ((idx series-idx)
                    (last-ts nil))
                (set-on-value-update
                 ctx item-name
                 (lambda (updated-item-state)
                   (let ((ts (gethash "timestamp" updated-item-state)))
                     ;; :refresh drops updates inside the window Lisp-side, so
                     ;; they never reach the browser connection at all
                     (unless (and refresh last-ts ts
                                  (< (- ts last-ts) refresh))
                       (when ts (setf last-ts ts))
                       (let ((y (%chart-transformed-y
                                 transform
                                 (%ext-value (gethash "value" updated-item-state)))))
                         (js-execute plot-div
                                     (%uplot-append-js plot-id ts series-count
                                                       idx (%js-number y))))))))))))

;; ---------------------------------------------------------------------------
;; page rendering / navigation
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; app header
;; ---------------------------------------------------------------------------

(defun %header-button (parent content hint on-click)
  (let ((btn (create-button parent :class "app-header-btn"
                                   :content content)))
    (setf (attribute btn "title") hint)
    (setf (attribute btn "aria-label") hint)
    (set-on-click btn (lambda (obj)
                        (declare (ignore obj))
                        (funcall on-click)))
    btn))

(defun %render-app-header (ctx container)
  "Renders the app bar that every view carries: back (once the connection has
navigated), the app name, and the home and settings buttons.

Installed as a web app the UI has no browser chrome -- no back button, no
address bar -- so this bar is the only navigation the user has left."
  (let* ((header (create-div container :class "app-header"))
         (left (create-div header :class "app-header-side")))
    (when (plusp (nav-context-depth ctx))
      (%header-button left "&larr;" "Back"
                      (lambda ()
                        (js-execute (nav-context-body ctx) "history.back()"))))
    (create-div header :class "app-brand" :content ui-webapp:+app-name+)
    (let ((right (create-div header :class "app-header-side app-header-right")))
      (%header-button right "&#8962;" "Home" (lambda () (navigate-home ctx)))
      (%header-button right "&#9881;" "Settings"
                      (lambda () (navigate-to-settings ctx))))
    header))

(defun render-page (page ctx)
  "Renders PAGE into the context's container, replacing previous content."
  (log:debug "Rendering page: ~a" (page:page-id page))
  (let ((container (nav-context-container ctx))
        (body (nav-context-body ctx)))
    (setf (inner-html container) "")
    (clear-value-update-funs ctx)
    (%render-app-header ctx container)
    (when (page:page-title page)
      (create-div container :class "header-line"
                            :content (page:page-title page)))
    (setf (title (html-document body))
          (or (page:page-title page) (page:page-label page)))
    (%render-page-widgets ctx container (page:page-children page))))

(defun %render-page-widgets (ctx parent widgets)
  "Renders WIDGETS into PARENT.  Consecutive itemgroup-refs share one
itemgroups-container so their cards flow into the responsive grid columns
(several per row on wide screens) instead of stacking one per row."
  (loop :while widgets
        :do (if (page:itemgroup-ref-p (first widgets))
                (let ((grid (create-div parent :class "itemgroups-container")))
                  (loop :while (and widgets (page:itemgroup-ref-p (first widgets)))
                        :do (%render-itemgroup-ref (pop widgets) ctx grid)))
                (render-widget (pop widgets) ctx parent))))

(defun navigate-to-page (page ctx)
  "Navigates the connection to PAGE: pushes the page's path onto the browser
history and renders it.  Browser back (or the rendered back button, which just
calls history.back()) returns via the popstate handler in `chipi-ui.main'."
  (incf (nav-context-depth ctx))
  (url-push-state (window (nav-context-body ctx)) (page:page-path page))
  (render-page page ctx))

(defun navigate-home (ctx)
  "Navigates the connection to the root path and renders its home view."
  (incf (nav-context-depth ctx))
  (url-push-state (window (nav-context-body ctx)) "/")
  (render-home ctx))

(defun navigate-to-settings (ctx)
  "Navigates the connection to the built-in settings view."
  (incf (nav-context-depth ctx))
  (url-push-state (window (nav-context-body ctx)) ui-settings:+settings-path+)
  (render-settings ctx))

(defun render-not-found (ctx path)
  "Renders a 'no page defined' view with links to all defined pages."
  (let ((container (nav-context-container ctx)))
    (setf (inner-html container) "")
    (clear-value-update-funs ctx)
    (%render-app-header ctx container)
    (create-div container :class "header-line" :content "Page not found")
    (create-div container :class "not-found-path"
                          :content (format nil "No page is defined for ~a" path))
    (let ((links (create-div container :class "page-nav-links")))
      (dolist (p (page:get-pages))
        (render-widget (page:page-link (page:page-id p)) ctx links)))))

;; ---------------------------------------------------------------------------
;; auto-generated overview (fallback when no page claims the root path)
;; ---------------------------------------------------------------------------

(defun render-overview (ctx)
  "Renders the overview page with all itemgroups."
  (let ((container (nav-context-container ctx)))
    (setf (inner-html container) "")
    (clear-value-update-funs ctx)
    (%render-app-header ctx container)
    (create-div container :class "header-line"
                          :content "Chipi Home Automation Dashboard")
    (let* ((groups (retrieve-top-level-itemgroups))
           (link-groups (remove-if-not #'%itemgroup-link-p groups))
           (card-groups (remove-if #'%itemgroup-link-p groups)))
      ;; Render link groups as a list if any exist
      (when link-groups
        (let ((links-container (create-div container :class "itemgroup-links-container")))
          (dolist (ig link-groups)
            (%render-itemgroup-link ctx ig links-container
                                    (lambda () (render-overview ctx))))))
      ;; Render card groups in the grid
      (when card-groups
        (let ((itemgroups-container (create-div container :class "itemgroups-container")))
          (dolist (ig card-groups)
            (%render-itemgroup ctx ig itemgroups-container)))))))

(defun %render-itemgroup-link (ctx itemg parent back-fn)
  "Renders an itemgroup as a clickable link."
  (let ((link-div (create-div parent :class "itemgroup-link"
                                     :content (gethash "label" itemg))))
    (set-on-click link-div
                  (lambda (obj)
                    (declare (ignore obj))
                    (%show-itemgroup-detail ctx itemg back-fn)))))

(defun %show-itemgroup-detail (ctx itemg back-fn)
  "Shows a detail page for a single itemgroup with a back button.
Shows child groups (as links or cards) above direct items."
  (let ((container (nav-context-container ctx)))
    (setf (inner-html container) "")
    (clear-value-update-funs ctx)
    (%render-app-header ctx container)
    (let ((back-btn (create-button container
                                   :class "back-button"
                                   :content "&larr; Back")))
      (set-on-click back-btn
                    (lambda (obj)
                      (declare (ignore obj))
                      (funcall back-fn))))
    (create-div container :class "group-header-line"
                          :content (gethash "label" itemg))
    ;; Show child groups if any
    (let* ((children (coerce (gethash "children" itemg) 'list))
           (link-children (remove-if-not #'%itemgroup-link-p children))
           (card-children (remove-if #'%itemgroup-link-p children)))
      (when link-children
        (let ((links-container (create-div container :class "itemgroup-links-container")))
          (dolist (child link-children)
            (%render-itemgroup-link ctx child links-container
                                    (lambda ()
                                      (%show-itemgroup-detail ctx itemg back-fn))))))
      (when card-children
        (let ((itemgroups-container (create-div container :class "itemgroups-container")))
          (dolist (child card-children)
            (%render-itemgroup ctx child itemgroups-container)))))
    ;; Show direct items as card (only if there are items)
    (let ((items (gethash "items" itemg)))
      (when (and items (> (length items) 0))
        (let ((itemgroups-container (create-div container :class "itemgroups-container")))
          (%render-itemgroup ctx itemg itemgroups-container))))))

(defun %render-itemgroup (ctx itemg parent)
  (let* ((col-div (create-div parent :class "itemgroup-column"))
         (card-div (create-div col-div :class "itemgroup-card")))
    (create-div card-div :class "itemgroup-header"
                         :content (gethash "label" itemg))
    (let ((items-container (create-div card-div :class "items-container")))
      (map nil (lambda (item)
                 (%render-item ctx item items-container))
           (gethash "items" itemg)))))

(defun %render-item (ctx item parent)
  (let* ((item-div (create-div parent :class "item-container"))
         (item-label (gethash "label" item))
         (item-name (gethash "name" item))
         (item-state (gethash "item-state" item))
         (type-hint (gethash "type-hint" item))
         (tags (gethash "tags" item))
         (ui-type (when (and tags (hash-table-p tags))
                    (gethash :ui-type tags))))

    (create-div item-div :class "item-label"
                         :content item-label)

    (create-div item-div :class (format nil "item-type-badge ~a" (string-downcase type-hint))
                         :content (or ui-type (%format-type-hint type-hint)))

    (create-div item-div :class "item-name"
                         :content item-name)

    (%render-item-value ctx item-name (gethash "value" item-state) type-hint tags item-div)

    (let ((ts-div
            (create-div item-div :class "item-timestamp-display"
                                 :content (%format-timestamp (gethash "timestamp" item-state)))))
      (set-on-value-update ctx item-name
                           (lambda (updated-item-state)
                             (setf (text ts-div)
                                   (%format-timestamp
                                    (gethash "timestamp" updated-item-state))))))))

(defun %render-item-value (ctx item-name item-value type-hint tags parent)
  (let ((mapping (%ui-mapping tags)))
    (cond
      ;; a writable boolean keeps its toggle -- a switch has no text for a
      ;; mapping to replace
      ((and (string= "BOOLEAN" type-hint) (not (%ui-readonly-p tags)))
       (%create-toggle-control ctx parent item-name item-value))
      ;; ON/OFF is itself a code, so a read-only boolean that carries a
      ;; mapping shows its labels instead
      ((and (string= "BOOLEAN" type-hint) (null mapping))
       (%create-boolean-display ctx parent item-name item-value))
      (t
       (%create-value-display ctx parent item-name item-value type-hint
                              nil mapping)))))

;; ---------------------------------------------------------------------------
;; home view
;; ---------------------------------------------------------------------------

(defun home-page (stored-path)
  "The page the root path should show for a device that stored STORED-PATH:
that page while it exists, else the page claiming the root path, else `nil',
which stands for the generated itemgroup overview.

A stored path outlives the page it names -- a `defpage' gets renamed or
dropped, the device keeps its setting -- so a stale one falls back instead of
leaving the device on a 'page not found'."
  (or (when stored-path
        (page:find-page-by-path stored-path))
      (page:find-page-by-path "/")))

(defun render-home (ctx)
  "Renders the connection's home view: the page this device picked in the
settings, the page claiming \"/\", or the generated itemgroup overview."
  (let* ((stored (ui-settings:home-path (nav-context-body ctx)))
         (page (home-page stored)))
    (when (and stored (not (page:find-page-by-path stored)))
      (log:info "Home page ~a set on this device no longer exists, using the default"
                stored))
    (if page
        (render-page page ctx)
        (render-overview ctx))))

;; ---------------------------------------------------------------------------
;; settings view
;; ---------------------------------------------------------------------------

(defun %default-home-label ()
  "What the default home option resolves to right now."
  (let ((root-page (page:find-page-by-path "/")))
    (if root-page
        (format nil "~a (page at /)" (page:page-label root-page))
        "Itemgroup overview")))

(defun %render-home-option (ctx parent label detail path current)
  "One selectable home-page row.  PATH is what gets stored for it: `nil' for
the default option, which clears the setting again."
  (let* ((selected (equal path current))
         (row (create-div parent :class (if selected
                                            "settings-option selected"
                                            "settings-option"))))
    (create-div row :class "settings-option-label" :content label)
    (create-div row :class "settings-option-detail" :content (or detail ""))
    (create-div row :class "settings-option-check"
                    :content (if selected "&#10003;" "&nbsp;"))
    (set-on-click row (lambda (obj)
                        (declare (ignore obj))
                        (ui-settings:set-home-path (nav-context-body ctx) path)
                        ;; re-render rather than just move the checkmark: the
                        ;; view is cheap and this shows what the browser
                        ;; actually stored
                        (render-settings ctx)))
    row))

(defun render-settings (ctx)
  "Renders the built-in settings view.

Settings are per device -- they live in the browser's `localStorage' -- so the
wall tablet and a phone each open on the page that suits them."
  (let* ((container (nav-context-container ctx))
         (body (nav-context-body ctx))
         (current (ui-settings:home-path body)))
    (setf (inner-html container) "")
    (clear-value-update-funs ctx)
    (%render-app-header ctx container)
    (create-div container :class "header-line" :content "Settings")
    (setf (title (html-document body)) "Settings")
    (let* ((card (create-div container :class "page-section"))
           (card-body (progn
                        (create-div card :class "page-section-header"
                                         :content "Home page")
                        (create-div card :class "page-section-body"))))
      (create-div card-body
                  :class "settings-hint"
                  :content (format nil "The page this device opens on. ~
Stored on this device only, so every phone or tablet can have its own."))
      (%render-home-option ctx card-body "Default" (%default-home-label)
                           nil current)
      ;; the page claiming "/" is already reachable via "Default" above --
      ;; listing it again under its own path would give it two rows whose
      ;; selected state disagrees depending on which one the setting was last
      ;; stored through
      (dolist (p (page:get-pages))
        (unless (equal (page:page-path p) "/")
          (%render-home-option ctx card-body (page:page-label p)
                               (page:page-path p) (page:page-path p) current))))))
