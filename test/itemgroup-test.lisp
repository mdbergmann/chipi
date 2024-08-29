(defpackage :chipi.itemgroup-test
  (:use :cl :fiveam :chipi.itemgroup)
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



(run! 'itemgroup-tests)
