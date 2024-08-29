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
    (is (string= (name cut) "FOO"))))

(run! 'itemgroup-tests)
