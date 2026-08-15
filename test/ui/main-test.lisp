(defpackage :chipi-ui.main-test
  (:use :cl :fiveam))

(in-package :chipi-ui.main-test)

(def-suite ui-main-tests
  :description "Unit tests for the pure (browser-free) helpers in
chipi-ui.page-renderer."
  :in chipi-ui.tests:test-suite)

(in-suite ui-main-tests)

;; ----------------------------------------------------------------------------
;; %format-value -- renders an item's value to a display string per type-hint.
;; ----------------------------------------------------------------------------

(test %format-value--float-two-decimals
  (is (string= "3.14" (ui-renderer::%format-value 3.14159 "FLOAT")))
  (is (string= "23.50" (ui-renderer::%format-value 23.5 "FLOAT"))))

(test %format-value--integer
  (is (string= "42" (ui-renderer::%format-value 42 "INTEGER"))))

(test %format-value--string
  (is (string= "hello" (ui-renderer::%format-value "hello" "STRING")))
  ;; NIL string value renders as the empty string, not "NIL".
  (is (string= "" (ui-renderer::%format-value nil "STRING"))))

(test %format-value--default-type
  ;; Unknown type-hint: NIL -> "", any other value via ~a.
  (is (string= "" (ui-renderer::%format-value nil "WHATEVER")))
  (is (string= "on" (ui-renderer::%format-value "on" "WHATEVER"))))

;; ----------------------------------------------------------------------------
;; %format-widget-value -- widget format string wins over default formatting.
;; ----------------------------------------------------------------------------

(test %format-widget-value--custom-format
  (is (string= "23.5 °C" (ui-renderer::%format-widget-value 23.5 "FLOAT" "~,1f °C"))))

(test %format-widget-value--nil-format-falls-back
  (is (string= "23.50" (ui-renderer::%format-widget-value 23.5 "FLOAT" nil))))

(test %format-widget-value--nil-value-ignores-format
  (is (string= "" (ui-renderer::%format-widget-value nil "STRING" "~a!"))))

;; ----------------------------------------------------------------------------
;; %format-type-hint -- human-readable badge label for a type-hint.
;; ----------------------------------------------------------------------------

(test %format-type-hint--known-types
  (is (string= "Switch" (ui-renderer::%format-type-hint "BOOLEAN")))
  (is (string= "Decimal Number" (ui-renderer::%format-type-hint "FLOAT")))
  (is (string= "Whole Number" (ui-renderer::%format-type-hint "INTEGER")))
  (is (string= "Text" (ui-renderer::%format-type-hint "STRING"))))

(test %format-type-hint--unknown-type
  (is (string= "Undefined type" (ui-renderer::%format-type-hint "FOO"))))

;; ----------------------------------------------------------------------------
;; %parse-number -- input field string to item value.
;; ----------------------------------------------------------------------------

(test %parse-number--integer
  (is (= 42 (ui-renderer::%parse-number "42" "INTEGER"))))

(test %parse-number--float
  (is (= 23.5 (ui-renderer::%parse-number "23.5" "FLOAT")))
  (is (typep (ui-renderer::%parse-number "23.5" "FLOAT") 'single-float)))

(test %parse-number--garbage-returns-nil
  (is-false (ui-renderer::%parse-number "abc" "INTEGER"))
  (is-false (ui-renderer::%parse-number "abc" "FLOAT")))

;; ----------------------------------------------------------------------------
;; %itemgroup-link-p -- T when the itemgroup carries the :ui-link tag.
;; ----------------------------------------------------------------------------

(defun %group-with-tag-keys (&rest keys)
  "An itemgroup hash-table whose \"tags\" is a hash containing KEYS."
  (let ((group (make-hash-table :test #'equal))
        (tags  (make-hash-table)))
    (dolist (k keys) (setf (gethash k tags) t))
    (setf (gethash "tags" group) tags)
    group))

(test %itemgroup-link-p--with-ui-link-tag
  (is-true (ui-renderer::%itemgroup-link-p (%group-with-tag-keys :ui-link))))

(test %itemgroup-link-p--without-ui-link-tag
  (is-false (ui-renderer::%itemgroup-link-p (%group-with-tag-keys :ui-other))))

(test %itemgroup-link-p--no-tags-key
  (is-false (ui-renderer::%itemgroup-link-p (make-hash-table :test #'equal))))

(test %itemgroup-link-p--tags-not-a-hash-table
  (let ((group (make-hash-table :test #'equal)))
    (setf (gethash "tags" group) "not-a-hash")
    (is-false (ui-renderer::%itemgroup-link-p group))))

;; ----------------------------------------------------------------------------
;; %ui-readonly-p -- T when the tags hash carries the :ui-readonly tag.
;; ----------------------------------------------------------------------------

(test %ui-readonly-p--with-tag
  (let ((tags (make-hash-table)))
    (setf (gethash :ui-readonly tags) t)
    (is-true (ui-renderer::%ui-readonly-p tags))))

(test %ui-readonly-p--without-tag
  (is-false (ui-renderer::%ui-readonly-p (make-hash-table))))

(test %ui-readonly-p--nil-tags
  (is-false (ui-renderer::%ui-readonly-p nil)))

;; ----------------------------------------------------------------------------
;; Item value-update registry -- callbacks are scoped per owner (connection)
;; so that one connection re-rendering (or dying) does not affect others.
;; ----------------------------------------------------------------------------

(defun %registered-funs (owner item-name)
  (let ((owners (gethash item-name ui-renderer:*item-value-form-update-funs*)))
    (when owners (gethash owner owners))))

(test value-update-registry--register-then-call-invokes-fun
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (received :unset))
    (ui-renderer:set-on-value-update
     :owner-a "item.one" (lambda (state) (setf received state)))
    (ui-renderer:call-item-value-update-fun "item.one" :new-state)
    (is (eq received :new-state))))

(test value-update-registry--call-unregistered-is-a-no-op
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal)))
    ;; No function registered for this item -> must not error.
    (finishes (ui-renderer:call-item-value-update-fun "item.missing" :x))))

(test value-update-registry--all-owners-are-called
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (calls '()))
    (ui-renderer:set-on-value-update :owner-a "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :a calls)))
    (ui-renderer:set-on-value-update :owner-b "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :b calls)))
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (= 2 (length calls)))
    (is-true (member :a calls))
    (is-true (member :b calls))))

(test value-update-registry--clear-only-drops-own-callbacks
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (calls '()))
    (ui-renderer:set-on-value-update :owner-a "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :a calls)))
    (ui-renderer:set-on-value-update :owner-b "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :b calls)))
    ;; owner-a re-renders: only its callbacks must go away
    (ui-renderer:clear-value-update-funs :owner-a)
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (equal '(:b) calls))
    (is-false (%registered-funs :owner-a "item.one"))))

;; ----------------------------------------------------------------------------
;; chart helpers -- data conversion to uPlot input.
;; ----------------------------------------------------------------------------

(test %chart-y-value--numbers-booleans-gaps
  (is (= 23.5 (ui-renderer::%chart-y-value 23.5)))
  (is (= 1 (ui-renderer::%chart-y-value t)))
  (is (= 0 (ui-renderer::%chart-y-value nil)))
  (is (string= "null" (ui-renderer::%chart-y-value "some-string"))))

(test %js-number--plain-float-and-integer
  (is (string= "23.5" (ui-renderer::%js-number 23.5)))
  (is (string= "42" (ui-renderer::%js-number 42))))

(test %js-number--no-reader-exponent-markers
  ;; with ~a, a float whose type differs from *read-default-float-format*
  ;; (thread-dependent!) prints as "87.0f0" / "1.5d8" -- JS cannot parse
  ;; those, and a range input assigned one reverts to its midpoint
  (let ((*read-default-float-format* 'double-float))
    (is (string= "87.0" (ui-renderer::%js-number 87.0))))
  (is (string= "150000000.0" (ui-renderer::%js-number 1.5d8))))

(test %js-escape--quotes-and-backslashes
  (is (string= "it\\'s" (ui-renderer::%js-escape "it's")))
  (is (string= "a\\\\b" (ui-renderer::%js-escape "a\\b")))
  (is (string= "" (ui-renderer::%js-escape nil))))

(test %chart-points--converts-timestamps-and-values
  (let* ((universal (encode-universal-time 0 0 12 1 1 2024 0))
         (unix (local-time:timestamp-to-unix
                (local-time:universal-to-timestamp universal)))
         (points (ui-renderer::%chart-points
                  (list (persp:make-persisted-item :value 1.5 :timestamp universal)
                        (persp:make-persisted-item :value 'item:true :timestamp universal))
                  nil)))
    (is (equal (list unix unix) (first points)))
    (is (equal (list "1.5" "1") (second points)))))

(test %chart-points--skips-undefined-timestamps
  (let ((points (ui-renderer::%chart-points
                 (list (persp:make-persisted-item :value 1.0 :timestamp :undefined))
                 nil)))
    (is (null (first points)))
    (is (null (second points)))))
