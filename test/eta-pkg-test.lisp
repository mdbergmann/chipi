(defpackage :cl-eta.package-test
  (:use :cl :fiveam :cl-eta.package)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.package-test)

(def-suite package-tests
  :description "Tests for eta package handling"
  :in cl-eta.tests:test-suite)

(in-suite package-tests)

(test collect-data--empty-empty
  (is (equalp '(nil #()) (multiple-value-list (collect-data #() #())))))

(test collect-data--half-package
  (is (equalp '(nil #(123 1 2)) (multiple-value-list (collect-data #(123 1) #(2))))))

(test collect-data--full-package
  (is (equalp '(t #(123 1 2 125)) (multiple-value-list (collect-data #(123 1) #(2 125))))))


(test extract-pkg--fail-empty
  (is (equalp '(:fail "Undersized package!") (multiple-value-list (extract-pkg #())))))

(test extract-pkg--monitor--one-item
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" 54.5)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  0 ; node id
                  0 167 ; monitor id two byte
                  2 33 ; value two byte (545) / divisor 10
                  125))))))

(test extract-pkg--monitor--more-items
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" 54.5)
                                   (cons "EtaBoiler" 12.4)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  10 ; payload len
                  3 ; checksum of payload (ignored)
                  0 ; node id
                  0 167 ; monitor id two byte
                  2 33 ; value two byte (545) / divisor 10
                  0
                  0 21
                  0 124
                  125))))))

(test extract-pkg--monitor--wrong-payload-size
  (is (equalp (list :fail "Wrong payload size!")
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  2 33 ; value two byte (545) / divisor 10
                  125))))))

;; todo: other package types
