(defpackage :chipi.house
  (:use :cl)
  (:nicknames :house)
  (:export #:send-command))

(in-package :chipi.house)

;; House vocabulary on top of the chipi primitives.
;;
;; The primitives -- `item', `binding', `hab' -- know nothing about a house and
;; nothing about this package; where a house abstraction needs something from
;; them, they offer it in their own, general terms (`item:set-value's
;; `:push :always' for the command below).  It is also free of any particular
;; binding: what talks to a bus or a device is the binding the item was defined
;; with.

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
