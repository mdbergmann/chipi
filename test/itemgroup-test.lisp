(defpackage :chipi.itemgroup-test
  (:use :cl :fiveam :cl-mock :chipi.itemgroup)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.itemgroup-test)

(def-suite itemgroup-tests
  :description "ItemGroup tests"
  :in chipi.tests:test-suite)

(in-suite itemgroup-tests)

(test itemgroup-create
  (let ((cut (make-itemgroup 'foo :label "Foo")))
    (is (typep cut 'itemgroup))
    (is (string= (label cut) "Foo"))
    (is (eq (name cut) 'foo))
    (is (string= (name cut) "FOO"))))

(test itemgroup-create-with-tags
  (let ((cut (make-itemgroup 'foo :label "Foo" :tags '((:ui-link) (:category . "sensor")))))
    (is (typep cut 'itemgroup))
    (is (equal '((:ui-link) (:category . "sensor")) (tags cut)))))

(test itemgroup-create-without-tags
  (let ((cut (make-itemgroup 'foo :label "Foo")))
    (is (typep cut 'itemgroup))
    (is (null (tags cut)))))

(test itemgroup-can-add-items
  (let ((cut (make-itemgroup 'foo)))
    ;; we consiously don't use the item factory function because it requires actor system
    (is-true (add-item cut (make-instance 'item:item :receive t :name "BAR")))
    (is (typep (get-item cut 'bar) 'item:item))))

(test itemgroup-remove-item
  (let ((cut (make-itemgroup 'foo)))
    (is-true (add-item cut (make-instance 'item:item :receive t :name "BAR")))
    (is-true (remove-item cut 'bar))
    (is-false (get-item cut 'bar))))

(test itemgroup-collects--get-value--from-items
  (let ((cut (make-itemgroup 'foo))
        (counter 0))
    (with-mocks ()
      (answer item:get-value (future:with-fut (incf counter)))
      (is-true (add-item cut (make-instance 'item:item :receive t :name "BAR")))
      (is-true (add-item cut (make-instance 'item:item :receive t :name "BAZ")))
      (is (equalp (list 1 2) (mapcar (lambda (x)
                                       (future:fawait x :timeout .5))
                                     (get-value cut))))
      )))

(test itemgroup-distributes--set-value--to-items
  (let ((cut (make-itemgroup 'foo))
        (counter 0))
    (with-mocks ()
      (answer item:set-value (incf counter))
      (is-true (add-item cut (make-instance 'item:item :receive t :name "BAR")))
      (is-true (add-item cut (make-instance 'item:item :receive t :name "BAZ")))
      (is-true (set-value cut counter))
      (is (= 2 (length (invocations 'item:set-value))))
      )))

(test itemgroup-parent-group--default-nil
  (let ((cut (make-itemgroup 'foo :label "Foo")))
    (is (null (parent-group cut)))))

(test itemgroup-parent-group--set
  (let ((cut (make-itemgroup 'foo :label "Foo" :parent-group 'bar)))
    (is (eq 'bar (parent-group cut)))))

(test itemgroup-add-and-get-child-groups
  (let ((parent (make-itemgroup 'parent :label "Parent"))
        (child1 (make-itemgroup 'child1 :label "Child1"))
        (child2 (make-itemgroup 'child2 :label "Child2")))
    (add-child-group parent child1)
    (add-child-group parent child2)
    (is (eq child1 (get-child-group parent 'child1)))
    (is (eq child2 (get-child-group parent 'child2)))
    (is (= 2 (length (get-child-groups parent))))))

(test itemgroup-remove-child-group
  (let ((parent (make-itemgroup 'parent :label "Parent"))
        (child (make-itemgroup 'child :label "Child")))
    (add-child-group parent child)
    (is (eq child (get-child-group parent 'child)))
    (remove-child-group parent 'child)
    (is (null (get-child-group parent 'child)))
    (is (= 0 (length (get-child-groups parent))))))
