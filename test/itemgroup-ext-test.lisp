(defpackage :chipi.itemgroup-ext-test
  (:use :cl :fiveam :itemgroup-ext :itemgroup)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.itemgroup-ext-test)

(def-suite itemgroup-ext-tests
  :description "Tests for itemgroup external representation."
  :in chipi.tests:test-suite)

(in-suite itemgroup-ext-tests)

(defun %make-mock-item (id label value)
  (let ((item
          (act:make-actor (lambda ())
                          :name (symbol-name id)
                          :type 'item:item
                          :state (item::make-item-state
                                  :value value))))
    (setf (slot-value item 'item::label) label)
    item))

(test itemgroup-to-ht--basic
  "Ensure conversion of a simple itemgroup with one item."
  (let* ((item (%make-mock-item 'foo "Foo" 1))
         (id (make-itemgroup 'grp :label "Group")))
    (add-item id item)
    (let ((ht (itemgroup-to-ht id)))
      (is (string= "GRP" (gethash "name" ht)))
      (is (string= "Group" (gethash "label" ht)))
      (let ((its (gethash "items" ht)))
        (is (= (length its) 1))
        (is (string= "FOO" (gethash "name" (first its))))))))
