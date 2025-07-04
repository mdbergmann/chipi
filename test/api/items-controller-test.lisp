(defpackage :chipi-api.items-controller-test
  (:use :cl :fiveam :cl-mock :chipi-api.items-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-api.items-controller-test)

(def-suite items-controller-tests
  :description "Test for items controller"
  :in chipi-api.tests:test-suite)

(in-suite items-controller-tests)

(defun %make-mock-item (id label value)
  (let ((item
          (act:make-actor (lambda ())
                        :name (symbol-name id)
                        :type 'item:item
                        :state (item::make-item-state
                                :value value))))
    (setf (slot-value item 'item::label) label)
    ;; passender Type-Hint für den Test setzen
    (let ((hint (cond
                  ((integerp value) 'integer)
                  ((floatp   value) 'float)
                  ((stringp  value) 'string)
                  ((member value '(item:true item:false)) 'boolean)
                  (t nil))))
      (when hint
        (setf (slot-value item 'item::value-type-hint) hint)))
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

(defun equal-item-props-p (item-ht name label value type-hint)
  (and (equal (gethash "name" item-ht) name)
       (equal (gethash "label" item-ht) label)
       (equal (gethash "value" (gethash "item-state" item-ht)) value)
       (equal (gethash "type-hint" item-ht) type-hint)
       (> (gethash "timestamp" (gethash "item-state" item-ht)) 0)))

(test retrieve-items--non-empty
  (with-fixture with-isys-mock ('((foo1 "foo1-label" 1)
                                  (foo2 "foo2-label" 2)))
    (let ((items (retrieve-items)))
      (is (= (length items) 2))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO1" "foo1-label" 1 "INTEGER")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO2" "foo2-label" 2 "INTEGER")) items)))))

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
      (is-true (some (lambda (x) (equal-item-props-p x "FOO1" "foo1-label-int" 1 "INTEGER")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO2" "foo2-label-float" 2.1 "FLOAT")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO3" "foo3-label-string" "bar" "STRING")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO4" "foo4-label-true" t "BOOLEAN")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO5" "foo5-label-false" nil "BOOLEAN")) items))
      (is-true (some (lambda (x) (equal-item-props-p x "FOO6" "foo6-label-null" 'cl:null 'cl:null)) items))
      )))

(test retrieve-item--existing
  (with-fixture with-isys-mock ('((foo1 "foo1-label" 1)))
    (let ((item (retrieve-item 'foo1)))
      (is-true item)
      (is (hash-table-p item))
      (is-true (equal-item-props-p item "FOO1" "foo1-label" 1 "INTEGER")))))

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
