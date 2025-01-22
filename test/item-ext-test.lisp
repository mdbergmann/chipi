(defpackage :chipi.item-ext-test
  (:use :cl :fiveam :chipi.item :chipi.item-ext)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.item-ext-test)

(def-suite item-ext-tests
  :description "Tests for item external representation."
  :in chipi.tests:test-suite)

(in-suite item-ext-tests)

(test item-value-ext-to-internal
  (let* ((values (list t nil 'cl:null))
         (values-inte (mapcar #'item-value-ext-to-internal values)))
    (is (equalp (list 'item:true 'item:false nil)
                values-inte))))

(test item-state-to-ht
  (let* ((item-state (item:make-item-state))
         (item-state-ht (item-state-to-ht item-state))
         (item-state-timestamp (item-state-timestamp item-state)))
    (is (hash-table-p item-state-ht))
    (is (= item-state-timestamp
           (gethash "timestamp" item-state-ht)))
    (let* ((values (list t nil 'item:true 'item:false))
           (values-ht
             (mapcar (lambda (value)
                       (setf (item-state-value item-state) value)
                       (setf item-state-ht (item-state-to-ht item-state))
                       (gethash "value" item-state-ht))
                     values)))
      (is (equalp values-ht
                  (list t 'cl:null t nil))))))

(test ht-to-item-state
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "timestamp" ht) (get-universal-time))
    (let* ((item-state (ht-to-item-state ht))
           (ht-timestamp (gethash "timestamp" ht)))
      (is (= ht-timestamp
             (item-state-timestamp item-state)))

      (let* ((values (list t nil 'cl:null))
             (values-item-state
               (mapcar (lambda (value)
                         (setf (gethash "value" ht) value)
                         (setf item-state (ht-to-item-state ht))
                         (item-state-value item-state))
                       values)))
        (is (equalp values-item-state
                    (list 'item:true 'item:false nil)))))))

(test item-to-ht
  (let* ((item (make-instance 'item :receive t
                                    :state (make-item-state :value 1 :timestamp 1234)
                                    :name "item1"
                                    :label "label1"))
         (ht (item-to-ht item)))
    (is (not (null ht)))
    (is (string= "item1" (gethash "name" ht)))
    (is (string= "label1" (gethash "label" ht)))
    (is (hash-table-p (gethash "item-state" ht)))))
