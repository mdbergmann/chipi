(defpackage :chipi-ui.render-test
  (:use :cl :fiveam))

(in-package :chipi-ui.render-test)

(def-suite ui-render-tests
  :description "Offline rendering tests for chipi-ui.main: stub the CLOG
connection so element creation needs no browser/websocket, then assert on the
DOM/JS commands CLOG would have sent."
  :in chipi-ui.tests:test-suite)

(in-suite ui-render-tests)

(defparameter +conn-id+ "chipi-ui-render-test-conn")

(defvar *captured* nil
  "List of JS command strings the stubbed CLOG connection received (reversed).")

(defmacro with-captured-clog (&body body)
  "Run BODY with clog-connection:execute/query stubbed so that every command
CLOG emits is pushed onto *CAPTURED* instead of going to a browser.  A
connection-data hash is registered for +CONN-ID+ so event binding
(set-on-click / set-on-change) takes the live path rather than no-opping."
  (let ((save-exec (gensym "EXEC")) (save-query (gensym "QUERY")))
    `(let ((*captured* nil)
           (,save-exec (fdefinition 'clog-connection:execute))
           (,save-query (fdefinition 'clog-connection:query)))
       (unwind-protect
            (progn
              (setf (fdefinition 'clog-connection:execute)
                    (lambda (connection-id message)
                      (declare (ignore connection-id))
                      (push message *captured*)
                      nil))
              (setf (fdefinition 'clog-connection:query)
                    (lambda (connection-id script &key default-answer)
                      (declare (ignore connection-id script))
                      default-answer))
              (setf (gethash +conn-id+ clog-connection::*connection-data*)
                    (make-hash-table :test #'equal))
              ,@body)
         (setf (fdefinition 'clog-connection:execute) ,save-exec)
         (setf (fdefinition 'clog-connection:query) ,save-query)
         (remhash +conn-id+ clog-connection::*connection-data*)))))

(defun captured-js ()
  "All captured commands joined into one string for substring assertions."
  (format nil "~{~a~^~%~}" (reverse *captured*)))

(defun js~ (substr)
  "True if SUBSTR appears anywhere in the captured JS."
  (and (search substr (captured-js)) t))

(defun make-body ()
  (clog::make-clog-body +conn-id+))

(defun make-item (&key (label "Temperature") (name "sensor.temp")
                       (type-hint "FLOAT") (value 23.5) (timestamp 0) tags)
  "A mock item hash-table shaped like item-ext:item-to-ht output."
  (let ((item  (make-hash-table :test #'equal))
        (state (make-hash-table :test #'equal)))
    (setf (gethash "value" state)     value
          (gethash "timestamp" state) timestamp)
    (setf (gethash "label" item)      label
          (gethash "name" item)       name
          (gethash "type-hint" item)  type-hint
          (gethash "item-state" item) state
          (gethash "tags" item)       tags)
    item))

;; ----------------------------------------------------------------------------
;; %render-item -- numeric/default value path.
;; ----------------------------------------------------------------------------

(test render-item--float-renders-label-badge-value-and-registers-update
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (let ((body (make-body))
            (item (make-item :label "Temperature" :name "sensor.temp"
                             :type-hint "FLOAT" :value 23.5)))
        (chipi-ui.main::%render-item item body)
        (is-true (js~ "item-container"))
        (is-true (js~ "item-label"))
        (is-true (js~ "Temperature"))
        (is-true (js~ "item-type-badge float"))   ; type-hint downcased into class
        (is-true (js~ "Decimal Number"))          ; badge label for FLOAT
        (is-true (js~ "item-name"))
        (is-true (js~ "sensor.temp"))
        (is-true (js~ "item-value-display"))
        (is-true (js~ "23.50"))                    ; %format-value FLOAT
        (is-true (js~ "item-timestamp-display"))
        ;; live-update callbacks were registered under the item name (value
        ;; display + timestamp display); the registry holds a list of them
        (let ((callbacks (gethash "sensor.temp"
                                  chipi-ui.main::*item-value-form-update-funs*)))
          (is (= 2 (length callbacks)))
          (is-true (every #'functionp callbacks)))))))

;; ----------------------------------------------------------------------------
;; %render-item -- read-only BOOLEAN (ON/OFF text) + the live-update callback.
;; ----------------------------------------------------------------------------

(test render-item--readonly-boolean-shows-on-and-live-updates-to-off
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (let ((tags (make-hash-table))
            (body (make-body)))
        (setf (gethash :ui-readonly tags) t)
        (chipi-ui.main::%render-item
         (make-item :label "Door" :name "switch.door"
                    :type-hint "BOOLEAN" :value t :tags tags)
         body)
        (is-true (js~ "item-value-display"))
        (is-true (js~ "boolean-true"))
        ;; the ON/OFF label is the div's HTML content on first render
        ;; (the text() jQuery call only happens on a live update below)
        (is-true (js~ "ON</div>"))
        ;; Drive a live update through the real dispatch entry point with a
        ;; fresh state (value NIL -> OFF).  call-item-value-update-fun runs
        ;; every callback registered for the item.
        (setf *captured* nil)
        (let ((new-state (make-hash-table :test #'equal)))
          (setf (gethash "value" new-state) nil
                (gethash "timestamp" new-state) 0)
          (chipi-ui.main::call-item-value-update-fun "switch.door" new-state)
          (is-true (js~ "text('OFF')"))
          (is-true (js~ "removeClass('boolean-true')"))
          (is-true (js~ "addClass('boolean-false')")))))))

;; ----------------------------------------------------------------------------
;; %render-item -- writable BOOLEAN renders a checkbox switch input.
;; ----------------------------------------------------------------------------

(test render-item--writable-boolean-creates-checkbox-switch
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (let ((body (make-body)))
        (chipi-ui.main::%render-item
         (make-item :label "Lamp" :name "switch.lamp"
                    :type-hint "BOOLEAN" :value t :tags (make-hash-table))
         body)
        (is-true (js~ "item-value-boolean"))
        (is-true (js~ "checkbox"))               ; <input type='checkbox' ...>
        (is-true (js~ "prop('checked'"))         ; (setf (checkedp ...) t)
        (let ((callbacks (gethash "switch.lamp"
                                  chipi-ui.main::*item-value-form-update-funs*)))
          (is-true callbacks)
          (is-true (every #'functionp callbacks)))))))

;; ----------------------------------------------------------------------------
;; %render-itemgroup -- a card with a header and its items.
;; ----------------------------------------------------------------------------

(test render-itemgroup--renders-card-header-and-items
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (let ((body (make-body))
            (group (make-hash-table :test #'equal)))
        (setf (gethash "label" group) "Living Room"
              (gethash "items" group) (vector (make-item :label "Temp"
                                                         :name "i.temp"
                                                         :type-hint "FLOAT"
                                                         :value 1.0)))
        (chipi-ui.main::%render-itemgroup group body)
        (is-true (js~ "itemgroup-column"))
        (is-true (js~ "itemgroup-card"))
        (is-true (js~ "itemgroup-header"))
        (is-true (js~ "Living Room"))
        (is-true (js~ "items-container"))
        (is-true (js~ "item-container"))))))

;; ----------------------------------------------------------------------------
;; %render-itemgroup-link -- a clickable link that binds a click handler.
;; ----------------------------------------------------------------------------

(test render-itemgroup-link--renders-link-and-binds-click
  (with-captured-clog
    (let ((body  (make-body))
          (group (make-hash-table :test #'equal)))
      (setf (gethash "label" group) "Bedrooms")
      (chipi-ui.main::%render-itemgroup-link
       group body body (lambda () nil))
      (is-true (js~ "itemgroup-link"))
      (is-true (js~ "Bedrooms"))
      ;; set-on-click emits a jQuery .on('click', ...) binding that ws.sends
      ;; the event back to the server.
      (is-true (js~ "on('click'"))
      (is-true (js~ "ws.send('E:")))))

;; ----------------------------------------------------------------------------
;; Regression: an item change must refresh BOTH the value display and the
;; timestamp.  %render-item registers a value-display callback and a timestamp
;; callback under the same item name; previously the second registration
;; clobbered the first (the registry stored a single fn per name), so the
;; displayed value never updated -- only the timestamp did.  The registry now
;; holds a list of callbacks and call-item-value-update-fun runs all of them.
;; ----------------------------------------------------------------------------

(test value-update--refreshes-both-value-display-and-timestamp
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (with-captured-clog
      (let ((body (make-body)))
        (chipi-ui.main::%render-item
         (make-item :label "Temperature" :name "sensor.temp"
                    :type-hint "FLOAT" :value 1.0 :timestamp 0)
         body)
        ;; both callbacks are registered for the one item
        (is (= 2 (length (gethash "sensor.temp"
                                  chipi-ui.main::*item-value-form-update-funs*))))
        ;; simulate an item change carrying a new value + timestamp
        (setf *captured* nil)
        (let ((new-state (make-hash-table :test #'equal)))
          (setf (gethash "value" new-state) 99.5
                (gethash "timestamp" new-state) 0)
          (chipi-ui.main::call-item-value-update-fun "sensor.temp" new-state))
        ;; the value display refreshed to the new formatted value ...
        (is-true (js~ "text('99.50')"))
        ;; ... and the timestamp display refreshed too (epoch 0 -> 1970)
        (is-true (js~ "1970"))))))
