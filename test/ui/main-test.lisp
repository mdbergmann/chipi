(defpackage :chipi-ui.main-test
  (:use :cl :fiveam))

(in-package :chipi-ui.main-test)

(def-suite ui-main-tests
  :description "Unit tests for the pure (browser-free) helpers in chipi-ui.main."
  :in chipi-ui.tests:test-suite)

(in-suite ui-main-tests)

;; ----------------------------------------------------------------------------
;; %format-value -- renders an item's value to a display string per type-hint.
;; ----------------------------------------------------------------------------

(test %format-value--float-two-decimals
  (is (string= "3.14" (chipi-ui.main::%format-value 3.14159 "FLOAT")))
  (is (string= "23.50" (chipi-ui.main::%format-value 23.5 "FLOAT"))))

(test %format-value--integer
  (is (string= "42" (chipi-ui.main::%format-value 42 "INTEGER"))))

(test %format-value--string
  (is (string= "hello" (chipi-ui.main::%format-value "hello" "STRING")))
  ;; NIL string value renders as the empty string, not "NIL".
  (is (string= "" (chipi-ui.main::%format-value nil "STRING"))))

(test %format-value--default-type
  ;; Unknown type-hint: NIL -> "", any other value via ~a.
  (is (string= "" (chipi-ui.main::%format-value nil "WHATEVER")))
  (is (string= "on" (chipi-ui.main::%format-value "on" "WHATEVER"))))

;; ----------------------------------------------------------------------------
;; %format-type-hint -- human-readable badge label for a type-hint.
;; ----------------------------------------------------------------------------

(test %format-type-hint--known-types
  (is (string= "Switch" (chipi-ui.main::%format-type-hint "BOOLEAN")))
  (is (string= "Decimal Number" (chipi-ui.main::%format-type-hint "FLOAT")))
  (is (string= "Whole Number" (chipi-ui.main::%format-type-hint "INTEGER")))
  (is (string= "Text" (chipi-ui.main::%format-type-hint "STRING"))))

(test %format-type-hint--unknown-type
  (is (string= "Undefined type" (chipi-ui.main::%format-type-hint "FOO"))))

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
  (is-true (chipi-ui.main::%itemgroup-link-p (%group-with-tag-keys :ui-link))))

(test %itemgroup-link-p--without-ui-link-tag
  (is-false (chipi-ui.main::%itemgroup-link-p (%group-with-tag-keys :ui-other))))

(test %itemgroup-link-p--no-tags-key
  (is-false (chipi-ui.main::%itemgroup-link-p (make-hash-table :test #'equal))))

(test %itemgroup-link-p--tags-not-a-hash-table
  (let ((group (make-hash-table :test #'equal)))
    (setf (gethash "tags" group) "not-a-hash")
    (is-false (chipi-ui.main::%itemgroup-link-p group))))

;; ----------------------------------------------------------------------------
;; %ui-readonly-p -- T when the tags hash carries the :ui-readonly tag.
;; ----------------------------------------------------------------------------

(test %ui-readonly-p--with-tag
  (let ((tags (make-hash-table)))
    (setf (gethash :ui-readonly tags) t)
    (is-true (chipi-ui.main::%ui-readonly-p tags))))

(test %ui-readonly-p--without-tag
  (is-false (chipi-ui.main::%ui-readonly-p (make-hash-table))))

(test %ui-readonly-p--nil-tags
  (is-false (chipi-ui.main::%ui-readonly-p nil)))

;; ----------------------------------------------------------------------------
;; Item value-update registry -- per-item callbacks driven by item changes.
;; ----------------------------------------------------------------------------

(test value-update-registry--register-then-call-invokes-fun
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal))
        (received :unset))
    (chipi-ui.main::set-on-value-update
     "item.one" (lambda (state) (setf received state)))
    (chipi-ui.main::call-item-value-update-fun "item.one" :new-state)
    (is (eq received :new-state))))

(test value-update-registry--call-unregistered-is-a-no-op
  (let ((chipi-ui.main::*item-value-form-update-funs* (make-hash-table :test #'equal)))
    ;; No function registered for this item -> must not error.
    (finishes (chipi-ui.main::call-item-value-update-fun "item.missing" :x))))
