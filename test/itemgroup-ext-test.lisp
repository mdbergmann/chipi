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
        (is (string= "FOO" (gethash "name" (aref its 0))))))))

;;; ------------------------------------------------------------
;;; NEW test: empty item array instead of NIL/FALSE

(test itemgroup-to-ht--empty-items
  "Ensure conversion of an empty itemgroup returns an empty vector (JSON [])."
  (let* ((grp (make-itemgroup 'empty :label "Empty Group"))
         (ht  (itemgroup-to-ht grp)))
    (let ((items (gethash "items" ht)))
      (is-true (and (vectorp items) (= 0 (length items)))))))

(test itemgroup-to-ht--with-tags
  "Ensure tags appear as hash-table in output."
  (let* ((grp (make-itemgroup 'tagged :label "Tagged"
                :tags '((:ui-link) (:category . "sensor"))))
         (ht  (itemgroup-to-ht grp)))
    (let ((tags-ht (gethash "tags" ht)))
      (is (hash-table-p tags-ht))
      (is (null (gethash :ui-link tags-ht)))
      (is (string= "sensor" (gethash :category tags-ht))))))

(test itemgroup-to-ht--without-tags
  "Ensure empty hash-table when no tags."
  (let* ((grp (make-itemgroup 'notags :label "No Tags"))
         (ht  (itemgroup-to-ht grp)))
    (let ((tags-ht (gethash "tags" ht)))
      (is (hash-table-p tags-ht))
      (is (= 0 (hash-table-count tags-ht))))))

(test itemgroup-to-ht--with-children
  "Ensure children appear as vector of serialized child groups."
  (let* ((parent (make-itemgroup 'parent :label "Parent"))
         (child  (make-itemgroup 'child :label "Child" :parent-group 'parent)))
    (add-child-group parent child)
    (let* ((ht (itemgroup-to-ht parent))
           (children (gethash "children" ht)))
      (is (vectorp children))
      (is (= 1 (length children)))
      (is (string= "CHILD" (gethash "name" (aref children 0))))
      (is (string= "Child" (gethash "label" (aref children 0)))))))

(test itemgroup-to-ht--without-children
  "Ensure empty vector when no children."
  (let* ((grp (make-itemgroup 'solo :label "Solo"))
         (ht  (itemgroup-to-ht grp)))
    (let ((children (gethash "children" ht)))
      (is (vectorp children))
      (is (= 0 (length children))))))

(test itemgroup-to-ht--parent-group
  "Ensure parent-group is serialized as string or NIL."
  (let* ((child (make-itemgroup 'child :label "Child" :parent-group 'parent))
         (top   (make-itemgroup 'top :label "Top"))
         (ht-child (itemgroup-to-ht child))
         (ht-top   (itemgroup-to-ht top)))
    (is (string= "PARENT" (gethash "parent-group" ht-child)))
    (is (null (gethash "parent-group" ht-top)))))
