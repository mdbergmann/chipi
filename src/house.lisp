(defpackage :chipi.house
  (:use :cl)
  (:nicknames :house)
  (:export #:send-command
           #:pulse
           #:press
           ;; single item kinds
           #:deflight
           #:defplug
           #:defcontact
           #:defmode
           #:defpulse
           ;; devices of several items
           #:defrollershutter
           #:defheating
           ;; rules
           #:def-any-true-rule))

(in-package :chipi.house)

;; House vocabulary on top of the chipi primitives.
;;
;; The primitives -- `item', `binding', `hab' -- know nothing about a house and
;; nothing about this package; where a house abstraction needs something from
;; them, they offer it in their own, general terms (`item:set-value's
;; `:push :always' for the command below).  It is also free of any particular
;; binding: what talks to a bus or a device is the binding the item was defined
;; with.
;;
;; The definers below (`deflight', `defrollershutter', ...) therefore define
;; what a thing *is* -- its items, their types, start values and kind -- and
;; leave what it is *connected to* to the caller: every item of a definer takes
;; the body of a `hab:defitem' form, bindings included.

;; -----------------------------
;; commands
;; -----------------------------

(defun %ensure-item (item)
  "Returns the `item:item' for `ITEM', which is the item itself or its id."
  (etypecase item
    (item:item item)
    (symbol (or (hab:get-item item)
                (error "No item with id: ~a" item)))))

(defun send-command (item value &key (timestamp nil) (persist t))
  "Sends `VALUE' as a command to `ITEM': it is pushed to the item's bindings every time, also when the item already holds it.
`ITEM' is an `item:item' or the id of one defined with `hab:defitem'.

`item:set-value' drops a value equal to the current one before any binding sees it, which is right for a state but wrong for a momentary command -- a second \"stop\" of a shutter, another press of a door opener.

Apart from that a command behaves like `item:set-value': a `VALUE' that differs from the current one is set on the item, raises the `item:item-changed-event' and is persisted; one that does not leaves state, timestamp and subscribers alone and is only pushed.
A bindings `call-push-p' still takes precedence on whether it is pushed to.
`TIMESTAMP' and `PERSIST': as for `item:set-value'.

The command is handed to the item and pushed by it, in order with every other value set on that item; `send-command' does not wait for the push."
  (item:set-value (%ensure-item item) value
                  :push :always
                  :timestamp timestamp
                  :persist persist)
  t)

(defun pulse (item &key (duration 1) (on 'item:true) (off 'item:false))
  "Sends `ON' as a command to `ITEM' and, `DURATION' seconds later, `OFF' -- the press and release of a push button: a bell, a door opener.
`ITEM' is an `item:item' or the id of one. Both values are sent with `send-command', so a pulse can be repeated at will.
The release is sent from a timer; an error it runs into, e.g. because the item is gone by then, is logged."
  (send-command item on)
  (timer:schedule-once duration
                       (lambda ()
                         (handler-case
                             (send-command item off)
                           (error (c)
                             (log:warn "Unable to end pulse on: ~a, error: ~a" item c)))))
  t)

(defun press (item)
  "Presses the push button `ITEM' -- an item defined with `defpulse' -- and releases it after the duration it was defined with.
`ITEM' is an `item:item' or the id of one. The duration is the item's `:house-pulse-duration' tag, 1 second for an item that has none.
This is `pulse' for whoever holds nothing but the item: a rule, a UI."
  (let ((item (%ensure-item item)))
    (pulse item :duration (or (cdr (assoc :house-pulse-duration (item:tags item)))
                              1))))

;; -----------------------------
;; definer helpers (used at macroexpansion time)
;; -----------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %parse-item-body (body own-keys)
    "Splits `BODY', the body of a `hab:defitem' form, into what a definer handles itself and what it hands on.
`OWN-KEYS' are the keys the definer handles. Returns two values: the body without them, and a plist of them.
A keyword in `BODY' takes the form after it as its value, anything else is a form of its own (a binding)."
    (let ((rest '())
          (own '()))
      (loop :while body
            :do (let ((form (pop body)))
                  (cond
                    ((keywordp form)
                     (when (null body)
                       (error "Missing value for ~s in item body." form))
                     (let ((value (pop body)))
                       (if (member form own-keys)
                           (setf own (append own (list form value)))
                           (setf rest (append rest (list form value))))))
                    (t
                     (setf rest (append rest (list form)))))))
      (values rest own)))

  (defun %sub-id (name suffix)
    "The id of a part of the device `NAME': NAME-SUFFIX, in the package of `NAME'."
    (intern (format nil "~a-~a" (symbol-name name) suffix)
            (or (symbol-package name) *package*)))

  (defun %item-form (id label type-hint initial-value kind body
                     &optional house-tags)
    "The `hab:defitem' form of an item of kind `KIND'.
`BODY' is the body of a `hab:defitem' form. Its `:initial-value' replaces `INITIAL-VALUE' and its `:tags' are kept, behind the `:house-kind' tag.
`HOUSE-TAGS' are forms of further tags the definer gives the item; they follow the `:house-kind' tag."
    (multiple-value-bind (rest own)
        (%parse-item-body body '(:initial-value :tags))
      `(hab:defitem ',id ,label ',type-hint
         :initial-value ,(getf own :initial-value initial-value)
         :tags (list* (cons :house-kind ,kind)
                      ,@house-tags
                      ,(getf own :tags))
         ,@rest)))

  (defun %part-form (name default-suffix label-var default-label
                     type-hint initial-value kind body)
    "The `hab:defitem' form of one part of the device `NAME', and the id of that part as second value.
On top of what `%item-form' takes, `BODY' may give the part's `:id' (in place of NAME-`DEFAULT-SUFFIX', or of `NAME' itself if that is NIL) and its `:label' (in place of the device label, bound to `LABEL-VAR', followed by `DEFAULT-LABEL')."
    (multiple-value-bind (rest own)
        (%parse-item-body body '(:id :label))
      (let ((id (or (getf own :id)
                    (if default-suffix
                        (%sub-id name default-suffix)
                        name))))
        (check-type id symbol)
        (values
         (%item-form id
                     (cond
                       ((getf own :label))
                       (default-label
                        `(format nil "~a ~a" ,label-var ,default-label))
                       (t label-var))
                     type-hint initial-value kind rest)
         id)))))

;; -----------------------------
;; single item kinds
;; -----------------------------

(defmacro deflight (name label &body body)
  "Defines the item `NAME' as a light: a boolean item, `item:true' while the light is on, initially off.
`NAME' is not evaluated. `LABEL' and `BODY' are those of `hab:defitem': bindings, `:group', `:tags', `:persistence' and, to replace the start value, `:initial-value'.
The item carries the tag `(:house-kind . :light)' in front of its own `:tags'.

  (deflight light-kitchen \"Kitchen\"
    :group '(lights kitchen)
    (knx-binding :ga \"0/0/5\" :dpt \"1.001\"))"
  (check-type name symbol)
  (%item-form name label 'boolean ''item:false :light body))

(defmacro defplug (name label &body body)
  "Defines the item `NAME' as a switched socket: a boolean item, `item:true' while it is powered, initially off.
`NAME', `LABEL' and `BODY': as for `deflight'. The item carries the tag `(:house-kind . :plug)'."
  (check-type name symbol)
  (%item-form name label 'boolean ''item:false :plug body))

(defmacro defcontact (name label &body body)
  "Defines the item `NAME' as a contact of a window or a door: a boolean item, `item:true' while it is open, initially closed.
`NAME', `LABEL' and `BODY': as for `deflight'. The item carries the tag `(:house-kind . :contact)'.
A contact reports and is not switched; tag it as read-only for whoever would offer a control for it (the UI's `:ui-readonly', the API's `:ext-readonly')."
  (check-type name symbol)
  (%item-form name label 'boolean ''item:false :contact body))

(defmacro defmode (name label &body body)
  "Defines the item `NAME' as an operating mode, of a heating controller for instance: an integer item, the number of the mode, initially 0.
`NAME', `LABEL' and `BODY': as for `deflight'. The item carries the tag `(:house-kind . :mode)'.
What the numbers mean is up to the device; give the item a value mapping in its `:tags' to name them (the UI's `:ui-mapping')."
  (check-type name symbol)
  (%item-form name label 'integer 0 :mode body))

(defmacro defpulse (name label &body body)
  "Defines the item `NAME' as a push button that is pressed by `pulse' -- a bell, a door opener -- and the function that presses it:

- the item `NAME', boolean, `item:true' while pressed, initially released, tagged `(:house-kind . :pulse)'
- the function `NAME-TRIGGER' of no arguments, which calls `press' on it.

`NAME', `LABEL' and `BODY': as for `deflight'. In addition `BODY' takes `:duration', the seconds between press and release, 1 by default. It is kept on the item, as the tag `:house-pulse-duration', which is where `press' reads it.

  (defpulse door-opener \"Door opener\"
    :duration 2
    (knx-binding :ga \"3/3/1\" :dpt \"1.001\" :initial-delay nil))

  (door-opener-trigger)"
  (check-type name symbol)
  (multiple-value-bind (rest own)
      (%parse-item-body body '(:duration))
    `(progn
       ,(%item-form name label 'boolean ''item:false :pulse rest
                    `((cons :house-pulse-duration ,(getf own :duration 1))))
       (defun ,(%sub-id name "TRIGGER") ()
         ,(format nil "Presses `~a' for the duration given to `house:defpulse'." name)
         (press ',name))
       ',name)))

;; -----------------------------
;; devices of several items
;; -----------------------------

(defmacro defrollershutter (name label
                            &key (move nil move-p) (stop nil stop-p)
                              (position nil position-p) (slat nil slat-p))
  "Defines the items and command functions of one rollershutter, blind or awning.

A shutter is driven through up to four channels, and an item is bound to one. So each channel given is an item of its own, and each of `MOVE', `STOP', `POSITION' and `SLAT' is the body of the `hab:defitem' form of that item -- its bindings, `:group', `:tags', `:persistence' -- or left out if the shutter has no such channel:

- `MOVE'      -- item `NAME', boolean: `item:true' moves down, `item:false' up.     kind `:shutter-move'
- `STOP'      -- item `NAME-STOP', boolean: `item:true' stops.                     kind `:shutter-stop'
- `POSITION'  -- item `NAME-POS', integer: position in %, 100 is closed.           kind `:shutter-position'
- `SLAT'      -- item `NAME-SLAT', integer: slat angle in %.                       kind `:shutter-slat'

Moving and stopping are momentary commands: the same one must be sendable twice in a row, which setting an item does not do. They are therefore sent with `send-command', by functions defined along with the items:

  (NAME-up)  (NAME-down)   ;; with `MOVE'
  (NAME-stop)              ;; with `STOP'

The values of the move and stop items are thus the last command, not the state of the shutter; what follows the shutter is position and slat, if their bindings report them.

`NAME' is not evaluated, `LABEL' is. A part is labelled `LABEL' followed by \"Stop\", \"Position\" or \"Slat\" (the move item `LABEL' alone). On top of a `hab:defitem' body a part takes `:label', to replace that label, and `:id', an unevaluated symbol to replace its id. The command functions are named after `NAME' in any case.

  (defrollershutter blind-living \"Blind living room\"
    :move     ((knx-binding :ga \"4/0/31\" :dpt \"1.008\" :initial-delay nil)
               :group '(blinds))
    :stop     ((knx-binding :ga \"4/0/30\" :dpt \"1.007\" :initial-delay nil)
               :group '(blinds))
    :position ((knx-binding :ga '(:read \"4/0/33\" :write \"4/0/32\") :dpt \"5.001\")
               :group '(blinds living))
    :slat     ((knx-binding :ga '(:read \"4/0/35\" :write \"4/0/34\") :dpt \"5.001\")
               :group '(blinds living)))"
  (check-type name symbol)
  (let ((label-var (gensym "LABEL"))
        (items '())
        (functions '()))
    (when move-p
      (multiple-value-bind (item-form id)
          (%part-form name nil label-var nil
                      'boolean ''item:false :shutter-move move)
        (push item-form items)
        (push `(defun ,(%sub-id name "UP") ()
                 ,(format nil "Moves the rollershutter `~a' up." name)
                 (send-command ',id 'item:false))
              functions)
        (push `(defun ,(%sub-id name "DOWN") ()
                 ,(format nil "Moves the rollershutter `~a' down." name)
                 (send-command ',id 'item:true))
              functions)))
    (when stop-p
      (multiple-value-bind (item-form id)
          (%part-form name "STOP" label-var "Stop"
                      'boolean ''item:false :shutter-stop stop)
        (push item-form items)
        (push `(defun ,(%sub-id name "STOP") ()
                 ,(format nil "Stops the rollershutter `~a'." name)
                 (send-command ',id 'item:true))
              functions)))
    (when position-p
      (push (%part-form name "POS" label-var "Position"
                        'integer 0 :shutter-position position)
            items))
    (when slat-p
      (push (%part-form name "SLAT" label-var "Slat"
                        'integer 0 :shutter-slat slat)
            items))
    ;; the functions stay top-level forms, outside the binding of the label
    `(progn
       (let ((,label-var ,label))
         (declare (ignorable ,label-var))
         ,@(nreverse items))
       ,@(nreverse functions)
       ',name)))

(defmacro defheating (name label
                      &key (temperature nil temperature-p)
                        (setpoint nil setpoint-p)
                        (valve nil valve-p))
  "Defines the items of the heating of one room.

Each of `TEMPERATURE', `SETPOINT' and `VALVE' is the body of the `hab:defitem' form of one item -- its bindings, `:group', `:tags', `:persistence' -- or left out if the room has no such value:

- `TEMPERATURE'  -- item `NAME-TEMP', float: the measured temperature.        kind `:temperature'
- `SETPOINT'     -- item `NAME-SETPOINT', float: the temperature asked for.   kind `:setpoint'
- `VALVE'        -- item `NAME-VALVE', integer: how far the valve is open, %. kind `:valve'

`NAME' is not evaluated, `LABEL' is. A part is labelled `LABEL' followed by \"Temperature\", \"Setpoint\" or \"Valve\". On top of a `hab:defitem' body a part takes `:label', to replace that label, and `:id', an unevaluated symbol to replace its id.

  (defheating living \"Living room\"
    :temperature ((knx-binding :ga \"0/4/17\" :dpt \"9.001\")
                  :group '(living)
                  :persistence '(:id :influx :frequency :every-30m))
    :setpoint    ((knx-binding :ga '(:read \"0/5/19\" :write \"0/5/18\") :dpt \"9.001\"))
    :valve       ((knx-binding :ga \"0/2/6\" :dpt \"5.001\")
                  :group '(living)))"
  (check-type name symbol)
  (let ((label-var (gensym "LABEL"))
        (forms '()))
    (when temperature-p
      (push (%part-form name "TEMP" label-var "Temperature"
                        'float 0.0 :temperature temperature)
            forms))
    (when setpoint-p
      (push (%part-form name "SETPOINT" label-var "Setpoint"
                        'float 0.0 :setpoint setpoint)
            forms))
    (when valve-p
      (push (%part-form name "VALVE" label-var "Valve"
                        'integer 0 :valve valve)
            forms))
    `(let ((,label-var ,label))
       (declare (ignorable ,label-var))
       ,@(nreverse forms)
       ',name)))

;; -----------------------------
;; rules
;; -----------------------------

(defmacro def-any-true-rule (name sources target)
  "Defines the rule `NAME' that keeps the item `TARGET' at whether any of the items `SOURCES' is `item:true' -- the window that is open while any of its sashes is.
`NAME' is the name of a `hab:defrule' and is evaluated. `SOURCES', a list of item ids, and `TARGET', an item id, are not.
The rule runs whenever one of `SOURCES' changes, and sets `TARGET' with `hab:set-item-value': to `item:true' if any source is, to `item:false' otherwise.

  (def-any-true-rule \"window-bedroom\"
      (window-bedroom-left window-bedroom-right)
    window-bedroom)"
  (check-type target symbol)
  (unless (and (consp sources) (every #'symbolp sources))
    (error "SOURCES must be a list of item ids, not: ~s" sources))
  `(hab:defrule ,name
     ,@(loop :for source :in sources
             :append `(:when-item-change ',source))
     :do (lambda (trigger)
           (declare (ignore trigger))
           (let ((any-true-p (some (lambda (source)
                                     (eq (hab:get-item-valueq source) 'item:true))
                                   ',sources)))
             (log:debug "Any of ~a true: ~a, setting: ~a" ',sources any-true-p ',target)
             (hab:set-item-value ',target
                                 (if any-true-p 'item:true 'item:false))))))
