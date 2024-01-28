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

(defun equal-item-props-p (item-plist name label value)
  (and (equal (getf item-plist :name) name)
       (equal (getf item-plist :label) label)
       (equal (getf item-plist :value) value)
       (> (getf item-plist :timestamp) 0)))

(test retrieve-items--non-empty
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
    (hab:defitem 'foo2 "foo2-label" nil :initial-value 2)
    (let ((items (retrieve-items)))
      (is (= (length items) 2))
      (is-true (equal-item-props-p (first items) "FOO1" "foo1-label" 1))
      (is-true (equal-item-props-p (second items) "FOO2" "foo2-label" 2)))))

(test retrieve-items--supported-value-types-mapping
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label-int" 'integer :initial-value 1)
    (hab:defitem 'foo2 "foo2-label-float" 'float :initial-value 2.1)
    (hab:defitem 'foo3 "foo3-label-string" 'string :initial-value "bar")
    (hab:defitem 'foo4 "foo4-label-true" 'boolean :initial-value 'item:true)
    (hab:defitem 'foo5 "foo5-label-false" 'boolean :initial-value 'item:false)
    (hab:defitem 'foo6 "foo6-label-null" nil :initial-value nil)
    (let ((items (retrieve-items)))
      (is (= (length items) 6))
      (print items)
      (is-true (equal-item-props-p (nth 0 items) "FOO1" "foo1-label-int" 1))
      (is-true (equal-item-props-p (nth 1 items) "FOO2" "foo2-label-float" 2.1))
      (is-true (equal-item-props-p (nth 2 items) "FOO3" "foo3-label-string" "bar"))
      (is-true (equal-item-props-p (nth 3 items) "FOO4" "foo4-label-true" t))
      (is-true (equal-item-props-p (nth 4 items) "FOO5" "foo5-label-false" nil))
      (is-true (equal-item-props-p (nth 5 items) "FOO6" "foo6-label-null" 'cl:null))
      )))

(test retrieve-item--existing
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
    (let ((item (retrieve-item 'foo1)))
      (is-true item)
      (is (listp item))
      (is-true (equal-item-props-p item "FOO1" "foo1-label" 1)))))

(test retrieve-item--non-existing
  (with-fixture with-isys ()
    (is-false (retrieve-item 'foo1))))

(test update-item--existing
  (with-fixture with-isys ()
    (hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
    (is-true (update-item-value 'foo1 2))
    (is-true (miscutils:await-cond 0.5
               (= (getf (retrieve-item 'foo1) :value) 2)))))

(test update-item--non-existing
  (with-fixture with-isys ()
    (is-false (update-item-value 'foo1 2))))
