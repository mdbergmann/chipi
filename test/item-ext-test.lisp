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
