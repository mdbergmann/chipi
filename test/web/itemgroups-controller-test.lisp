(defpackage :chipi-web.itemgroups-controller-test
  (:use :cl :fiveam :cl-mock :itemgroupsc)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.itemgroups-controller-test)

(def-suite itemgroups-controller-tests
  :description "Tests for itemgroups controller."
  :in chipi-web.tests:test-suite)

(in-suite itemgroups-controller-tests)

(defun %make-mock-item (id label value)
  (let ((item
          (act:make-actor (lambda ())
                          :name (symbol-name id)
                          :type 'item:item
                          :state (item::make-item-state
                                  :value value))))
    (setf (slot-value item 'item::label) label)
    item))

(defun %make-mock-itemgroup (gid glabel items)
  (let ((id (itemgroup:make-itemgroup gid :label glabel)))
    (dolist (it items)
      (itemgroup:add-item id it))
    id))

(def-fixture with-isys-mock (itemgroups)
  (unwind-protect
       (progn
         (let ((hab:*items* (make-hash-table))
               (hab:*itemgroups* (make-hash-table)))
           (dolist (id itemgroups)
             (setf (gethash (itemgroup:name id) hab:*itemgroups*) id))
           (&body)))))

(test retrieve-itemgroups--empty
  (with-fixture with-isys-mock (nil)
    (is (= (length (retrieve-itemgroups)) 0))))

(test retrieve-itemgroups--non-empty
  (with-fixture with-isys-mock
      ((list (%make-mock-itemgroup 'grp "Group"
               (list (%make-mock-item 'foo "Foo" 1)
                     (%make-mock-item 'bar "Bar" 2)))))
    (let ((groups (retrieve-itemgroups)))
      (is (= (length groups) 1))
      (let* ((g (first groups))
             (its (gethash "items" g)))
        (is (string= "GRP" (gethash "name" g)))
        (is (= (length its) 2))))))

(test retrieve-itemgroup--existing
  (with-fixture with-isys-mock
      ((list (%make-mock-itemgroup 'grp "Group"
               (list (%make-mock-item 'foo "Foo" 1)))))
    (let ((g (retrieve-itemgroup 'grp)))
      (is-true g)
      (is (string= "GRP" (gethash "name" g))))))

(test retrieve-itemgroup--non-existing
  (with-fixture with-isys-mock (nil)
    (is-false (retrieve-itemgroup 'grp))))
