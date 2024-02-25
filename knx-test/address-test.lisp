(defpackage :knx-conn.address-test
  (:use :cl :fiveam :knx-conn.address)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.address-test)

(def-suite address-tests
  :description "Group and Individual address tests"
  :in knx-conn.tests:test-suite)

(in-suite address-tests)

(test make-ga-from-string
  (let ((address (make-group-address "1/2/3")))
    (is (knx-group-address-p address))
    (is (string= "1/2/3" (address-string-rep address)))
    (is (equalp #(10 3) (address::address-addr address)))))

(test make-ga--error
  (handler-case
      (make-group-address "1/2/3/4")
    (error (e) (is (typep e 'type-error)))))

(test make-ga-from-vector
  (let ((address (make-group-address #(1 2 3))))
    (is (knx-group-address-p address))
    (is (string= "1/2/3" (address-string-rep address)))
    (is (equalp #(10 3) (address::address-addr address)))))

(test parse-ga-from-bytes
  (is (equalp "1/2/3" (address-string-rep
                       (parse-group-address
                        (vector 10 3))))))

(test parse-ia-from-bytes
  (is (equalp "0.10.3" (address-string-rep
                        (parse-individual-address
                         (vector 10 3))))))

