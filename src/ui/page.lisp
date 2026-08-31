(defpackage :chipi-ui.page
  (:use :cl)
  (:nicknames :page)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export ;; page
           #:page
           #:page-id
           #:page-label
           #:page-title
           #:page-path
           #:page-children
           ;; widgets
           #:widget
           #:item-widget
           #:item-widget-item-id
           #:item-widget-label
           #:value-display
           #:value-display-format
           #:toggle
           #:text-input
           #:number-input
           #:number-input-min
           #:number-input-max
           #:number-input-step
           #:slider
           #:slider-min
           #:slider-max
           #:slider-step
           #:setpoint
           #:setpoint-min
           #:setpoint-max
           #:setpoint-step
           #:selection
           #:selection-choices
           #:chart
           #:chart-type
           #:chart-range
           #:chart-persistence
           #:chart-transform
           #:chart-series
           #:chart-height
           #:chart-line-width
           #:chart-refresh
           #:button
           #:button-p
           #:button-caption
           #:button-label
           #:button-action
           #:button-group
           #:button-group-p
           #:button-group-label
           #:button-group-buttons
           #:section
           #:section-label
           #:section-children
           #:page-link
           #:page-link-page-id
           #:page-link-label
           #:itemgroup-ref
           #:itemgroup-ref-p
           #:itemgroup-ref-group-id
           ;; DSL
           #:defpage
           #:value
           #:value-mapping
           ;; registry
           #:*pages*
           #:*page-registered-hook*
           #:get-page
           #:get-pages
           #:find-page-by-path
           #:normalize-path
           #:clear-pages))

(in-package :chipi-ui.page)

;; ---------------------------------------------------------------------------
;; model
;;
;; Pages and widgets are plain data (no CLOG dependency): the DSL below builds
;; this model and `chipi-ui.page-renderer' renders it.  Item references are
;; symbols that are resolved at render time, so `defpage' works regardless of
;; whether the referenced items are defined before or after the page.
;; ---------------------------------------------------------------------------

(defstruct widget)

(defstruct (item-widget (:include widget))
  item-id    ; symbol, as given to `hab:defitem'
  label)     ; nil => the item's own label is used

(defstruct (value-display (:include item-widget))
  format)    ; nil => default formatting per type-hint

(defstruct (toggle (:include item-widget)))

(defstruct (text-input (:include item-widget)))

(defstruct (number-input (:include item-widget))
  min max step)

(defstruct (slider (:include item-widget))
  (min 0) (max 100) (step 1))

(defstruct (setpoint (:include item-widget))
  min max (step 1))

(defstruct (selection (:include item-widget))
  choices)   ; alist of (value . label)

(defstruct (chart (:include item-widget))
  (type :line)  ; :line or :bar
  range         ; persp:range
  persistence   ; persistence id; nil => first defined historic persistence
  transform     ; nil, or 1-arg function applied to every charted value
  series        ; list of (item-id . label-or-nil), one entry per series
  (height 220)  ; plot height in CSS pixels
  (line-width 2) ; stroke width of every series in CSS pixels
  refresh)      ; nil, or minimum seconds between live-appended points

(defstruct (button (:include widget))
  caption    ; the button's own text
  label      ; row label to the left of the button; nil => none
  action)    ; function (or symbol naming one) called with no arguments

(defstruct (button-group (:include widget))
  label      ; row label to the left of the buttons; nil => none
  buttons)   ; list of `button's, rendered side by side in one row

(defstruct (section (:include widget))
  label
  children)

(defstruct (page-link (:include widget))
  page-id
  label)     ; nil => the target page's label is used

(defstruct (itemgroup-ref (:include widget))
  group-id)

(defstruct page
  id
  label      ; short name, used by `page-link's pointing at this page
  title      ; nil => no page heading is rendered
  path       ; URL path the page is served at
  children)

;; ---------------------------------------------------------------------------
;; registry
;; ---------------------------------------------------------------------------

(defvar *pages* (make-hash-table)
  "All defined pages, keyed by page id (symbol).")

(defvar *page-order* '()
  "Page ids, most recently defined first.  Gives `find-page-by-path' a
deterministic winner when two pages claim the same path.")

(defvar *page-registered-hook* nil
  "When set, called with a page right after it was (re-)registered.  The UI
server uses this to register the page's URL as a route; it lives here as a
hook so the page model carries no CLOG dependency.")

(defun get-page (id)
  "Returns the page with `id' if it exists, `nil' otherwise."
  (gethash id *pages*))

(defun get-pages ()
  "Returns all defined pages, most recently defined first."
  (mapcar #'get-page *page-order*))

(defun normalize-path (path)
  "Trims a trailing slash (except for the root path) so \"/wall\" and
\"/wall/\" address the same page.  An empty path is treated as \"/\"."
  (cond
    ((or (null path) (string= path "")) "/")
    ((and (> (length path) 1)
          (char= #\/ (char path (1- (length path)))))
     (subseq path 0 (1- (length path))))
    (t path)))

(defun find-page-by-path (path)
  "Returns the page registered for URL `path', or `nil'."
  (let ((path (normalize-path path)))
    (find path (get-pages) :key #'page-path :test #'string=)))

(defun clear-pages ()
  "Removes all page definitions."
  (clrhash *pages*)
  (setf *page-order* '()))

(defun %default-path (id)
  (format nil "/~(~a~)" id))

(defun %register-page (page)
  (let ((id (page-id page)))
    (dolist (other (get-pages))
      (when (and (not (eq (page-id other) id))
                 (string= (page-path other) (page-path page)))
        (log:warn "Page ~a claims path ~a already used by page ~a; the newer definition wins."
                  id (page-path page) (page-id other))))
    (setf (gethash id *pages*) page)
    (setf *page-order* (cons id (remove id *page-order*)))
    (when *page-registered-hook*
      (funcall *page-registered-hook* page))
    page))

;; ---------------------------------------------------------------------------
;; DSL
;; ---------------------------------------------------------------------------

(defmacro defpage (id label &rest body)
  "Defines a UI page served at its own URL.
It will create the page if it does not exist and replace it if it does.

`label' is the short page name used by `page-link's pointing at this page.
A `:path' key specifies the URL path (defaults to \"/<id>\" downcased).
A `:title' key specifies a heading rendered at the top of the page and used
as the browser tab title; without it no heading is rendered.

All other body forms must evaluate to widgets, e.g.:

  (defpage 'wall-panel \"Wall panel\"
    :path \"/wall\"
    :title \"Ground floor\"
    (section \"Climate\"
      (value 'outside-temp :format \"~,1f °C\")
      (chart 'outside-temp :range '(:days 1)))
    (page-link 'cellar))

Widgets are ordinary values, so they can be composed programmatically
(e.g. `mapcar' over a list of item ids)."
  (with-gensyms (body-forms path title widgets pid)
    `(let* ((,pid ,id)
            (,body-forms (list ,@body))
            (,path (loop :for (k v) :on ,body-forms
                         :if (eq k :path)
                           :return v))
            (,title (loop :for (k v) :on ,body-forms
                          :if (eq k :title)
                            :return v))
            (,widgets (loop :for x :in ,body-forms
                            :if (typep x 'widget)
                              :collect x)))
       (%register-page (make-page :id ,pid
                                  :label ,label
                                  :title ,title
                                  :path (normalize-path
                                         (or ,path (%default-path ,pid)))
                                  :children ,widgets)))))

(defun value (item-id &key label format)
  "A read-only display of the item's value.
`format' is a CL format string applied to the value (e.g. \"~,1f °C\");
without it the value is formatted according to the item's type-hint.
Boolean items render as ON/OFF."
  (make-value-display :item-id item-id :label label :format format))

(defun value-mapping (&rest value-label-pairs)
  "Builds an item's `:ui-mapping' tag from alternating values and labels:

  (defitem 'hvac-mode \"Betriebsmodus\" 'integer
    :tags (list (cons :ui-mapping
                      (page:value-mapping 0 \"Auto\" 1 \"Komfort\" 2 \"Standby\")))
    ...)

An item whose value is a code rather than a quantity displays that code
verbatim -- `2' tells a reader nothing.  With this tag every read-only
display of the item shows the label instead, both the `value' widget and the
item's row in an `itemgroup-ref' card.  Values not listed keep showing
themselves, so an unexpected code is visible rather than silently reading as
one of the known ones.

The result is a hash-table on purpose: item tags are serialized to JSON by
the items API, and `com.inuoe.jzon' renders a hash-table as an object but
signals on an alist -- one alist tag would fail the whole item listing."
  (let ((ht (make-hash-table :test #'equal)))
    (loop :for (value label) :on value-label-pairs :by #'cddr
          :do (setf (gethash value ht) label))
    ht))

(defun toggle (item-id &key label)
  "A switch control for a boolean item."
  (make-toggle :item-id item-id :label label))

(defun text-input (item-id &key label)
  "A text field for a string item."
  (make-text-input :item-id item-id :label label))

(defun number-input (item-id &key label min max step)
  "A number entry field for an integer/float item."
  (make-number-input :item-id item-id :label label :min min :max max :step step))

(defun slider (item-id &key label (min 0) (max 100) (step 1))
  "A slider for a bounded number item."
  (make-slider :item-id item-id :label label :min min :max max :step step))

(defun setpoint (item-id &key label min max (step 1))
  "A stepper (+/- buttons) for a number item, e.g. a target temperature."
  (make-setpoint :item-id item-id :label label :min min :max max :step step))

(defun selection (item-id &key label choices)
  "A dropdown for an item with enumerable values.
`choices' is an alist of (value . label) pairs; the value is written to the
item when selected."
  (make-selection :item-id item-id :label label :choices choices))

(defun chart (item-ids &key label (type :line) range persistence transform
                            (height 220) (line-width 2) refresh)
  "A history chart of one or more items' persisted values.
`item-ids' is a single item id, or a list whose elements are item ids or
`(item-id . \"Series label\")' conses; each entry becomes one chart series
(the series label defaults to the item's own label).
`type' is :line or :bar.
`range' is either a `persp:range' object or a plist passed to
`persp:make-relative-range', e.g. '(:days 1) or '(:hours 6); defaults to
the last day.
`persistence' names the persistence to load from; without it the first
defined historic persistence is used.
`transform', when given, is a one-argument function applied to every charted
value of every series, historic and live alike -- e.g. converting a raw
sensor reading to a display unit.  It is called with a number (booleans
chart as 1/0) and must return a number.
`height' is the plot height in CSS pixels; the width always follows the
container.  Raise it for charts whose series share a wide value range -- at
the default 220 the lines of a multi-series chart sit too close together to
be read apart.
`line-width' is the stroke width of every series in CSS pixels (uPlot scales
it for the device pixel ratio).  The default 2 is twice uPlot's own 1px,
which reads as hairlines on a high-density display.
`refresh', when given, is the minimum number of seconds between two
live-appended points of one series; item updates arriving sooner are not
charted (the item itself and its persistence still see every update).
Without it every value change appends a point, which is the wrong cadence
for an item that broadcasts every few seconds -- a KNX room temperature
sending once a second appends a point per telegram for as long as the page
stays open.  The history load is unaffected: it always shows what the
persistence stored, at the persistence's own granularity."
  (let ((series (cond
                  ((symbolp item-ids) (list (cons item-ids nil)))
                  ;; a single (item-id . "label") cons
                  ((and (consp item-ids) (stringp (cdr item-ids)))
                   (list item-ids))
                  (t (mapcar (lambda (s)
                               (if (consp s) s (cons s nil)))
                             item-ids)))))
    (make-chart :item-id (car (first series))
                :label label :type type
                :persistence persistence
                :transform transform
                :series series
                :height height
                :line-width line-width
                :refresh refresh
                :range (etypecase range
                         (null (persp:make-relative-range :days 1))
                         (cons (apply #'persp:make-relative-range range))
                         (persp:range range)))))

(defun button (caption action &key label)
  "A push-button that calls ACTION when clicked.

Unlike every other widget this one is not bound to an item: it triggers an
action.  That is what momentary commands need -- a door opener, a shutter's
Auf/Ab/Stopp -- because `item:set-value' is a no-op when the value does not
change, so an item-bound control cannot send the same command twice.

ACTION takes no arguments and its return value is ignored; an error it
signals is logged and does not reach the browser connection.  A symbol naming
a function is resolved on every click, so redefining that function (from the
REPL, over Slynk) takes effect without re-evaluating the page:

  (button \"Auf\" 'jal-wz-up)          ; late-bound, survives a redefinition
  (button \"Auf\" #'jal-wz-up)         ; the function as of page definition

`label' renders to the left of the button, like the label of an item widget;
without it the button stands alone in its row."
  (make-button :caption caption :action action :label label))

(defun button-group (label &rest buttons)
  "Several `button's sharing one row and one label, e.g. the Auf/Stopp/Ab of
one shutter:

  (button-group \"Wohnzimmer\"
    (button \"Auf\" 'jal-wz-up)
    (button \"Stopp\" 'jal-wz-stop)
    (button \"Ab\" 'jal-wz-down))"
  (dolist (b buttons)
    (check-type b button))
  (make-button-group :label label :buttons buttons))

(defun section (label &rest children)
  "A titled group of widgets, rendered as a card."
  (make-section :label label :children children))

(defun page-link (page-id &key label)
  "A navigation link to another page defined with `defpage'."
  (make-page-link :page-id page-id :label label))

(defun itemgroup-ref (group-id)
  "Embeds an existing itemgroup, rendered as the same card the auto-generated
overview uses.  Useful for migrating to explicit pages without re-declaring
every item as a widget."
  (make-itemgroup-ref :group-id group-id))
