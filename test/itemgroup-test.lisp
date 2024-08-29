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
  (is (typep (make-itemgroup 'foo :label "Foo") 'itemgroup)))

(run! 'itemgroup-tests)
