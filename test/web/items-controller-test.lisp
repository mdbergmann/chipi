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

(defun %make-mock-item (id label value)
  (let ((item
          (act:make-actor (lambda ())
                        :name (symbol-name id)
                        :type 'item:item
                        :state (item::make-item-state
                                :value value))))
    (setf (slot-value item 'item::label) label)
    item))

(def-fixture with-isys-mock (item-datas)
  (unwind-protect
       (progn
         ;; setup a partial environment
         (let ((hab:*items* (make-hash-table)))
           (dolist (item-data item-datas)
             (destructuring-bind (id label initial-value) item-data
               (let ((item (%make-mock-item id label initial-value)))
                 (setf (slot-value item 'item::label) label)
                 (setf (gethash id hab:*items*) item))))
           ;;(isys:ensure-isys)
           (&body)))))

(test retrieve-items--empty
  (with-fixture with-isys-mock (nil)
    (is (= (length (retrieve-items)) 0))))

(defun equal-item-props-p (item-plist name label value)
  (and (equal (getf item-plist :name) name)
       (equal (getf item-plist :label) label)
       (equal (getf item-plist :value) value)
       (> (getf item-plist :timestamp) 0)))

(test retrieve-items--non-empty
  (with-fixture with-isys-mock ('((foo1 "foo1-label" 1)
                                  (foo2 "foo2-label" 2)))
    (let ((items (retrieve-items)))
      (is (= (length items) 2))
      (is-true (equal-item-props-p (first items) "FOO1" "foo1-label" 1))
      (is-true (equal-item-props-p (second items) "FOO2" "foo2-label" 2)))))

(test retrieve-items--supported-value-types-mapping
  (with-fixture with-isys-mock ('((foo1 "foo1-label-int" 1)
                                  (foo2 "foo2-label-float" 2.1)
                                  (foo3 "foo3-label-string" "bar")
                                  (foo4 "foo4-label-true" item:true)
                                  (foo5 "foo5-label-false" item:false)
                                  (foo6 "foo6-label-null" nil)))
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
  (with-fixture with-isys-mock ('((foo1 "foo1-label" 1)))
    (let ((item (retrieve-item 'foo1)))
      (is-true item)
      (is (listp item))
      (is-true (equal-item-props-p item "FOO1" "foo1-label" 1)))))

(test retrieve-item--non-existing
  (with-fixture with-isys-mock (nil)
    (is-false (retrieve-item 'foo1))))

(test update-item--existing
  (with-fixture with-isys-mock ('((foo1 "foo1-label" 1)))
    (with-mocks ()
      (answer item:set-value t)
      (is-true (update-item-value 'foo1 2))
      (is (= (length (invocations 'item:set-value)) 1)))))

(test update-item--non-existing
  (with-fixture with-isys-mock (nil)
    (is-false (update-item-value 'foo1 2))))
