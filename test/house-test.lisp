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
