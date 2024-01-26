(defpackage :chipi-web.items-controller-test
  (:use :cl :fiveam :cl-mock :chipi-web.items-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.items-controller-test)

(def-suite items-controller-tests
  :description "Test for items controller"
  :in chipi-web.tests:test-suite)

(in-suite items-controller-tests)

(def-fixture with-isys ()
  (unwind-protect
       (progn
         ;; setup a partial environment
         (let ((hab:*items* (make-hash-table)))
           (isys:ensure-isys)
           (&body)))
    (isys:shutdown-isys)))

(test retrieve-items--empty
  (with-fixture with-isys ()
    (is (= (length (retrieve-items)) 0))))

(defun equal-item-props-p (item name label value)
  (and (equal (getf item :name) name)
       (equal (getf item :label) label)
       (equal (getf item :value) value))
       (> (getf item :timestamp) 0))

(test retrieve-items--non-empty
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
    (hab:defitem 'foo2 "foo2-label" nil :initial-value 2)
    (let ((items (retrieve-items)))
      (is (= (length items) 2))
      (is-true (equal-item-props-p (first items) "FOO1" "foo1-label" 1))
      (is-true (equal-item-props-p (second items) "FOO2" "foo2-label" 2)))))

(test retrieve-item--existing
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
    (let ((item (retrieve-item 'foo1)))
      (is-true item)
      (is (listp item))
      (is-true (equal-item-props-p item "FOO1" "foo1-label" 1)))))
