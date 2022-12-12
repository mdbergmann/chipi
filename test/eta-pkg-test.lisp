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

(test extract-pkg--monitor--one-item--negative-value
  (is (equalp (list :eta-monitor (list (cons "EtaBoilerUnten" -2.5)
                                       (cons "EtaBoiler" -2.3)))
              (multiple-value-list
               (extract-pkg
                #(123
                  77 68 ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  0 ; node id
                  0 167 ; monitor id two byte
                  255 250 ; negative value two byte (-545) / divisor 10
                  0
                  0 21
                  255 230 ; negative (-23)
                  125))))))

#|
#(123 77 68 65 203 24 0 19 5 115 24 0 20 2 226 24 0 21 0 0 24 0
22 255 229 24 0 23 1 77 24 0 53 81 31 24 0 77 2 211 24 0 78 1
36 24 0 107 55 45 24 0 112 2 84 24 0 167 0 0 32 0 75 0 191 32
0 145 1 122 125)
#(123 77 68 65 32 24 0 19 5 120 24 0 20 2 232 24 0 21 0 0 24 0
22 255 231 24 0 23 1 119 24 0 53 81 31 24 0 77 2 220 24 0 78
1 38 24 0 107 55 45 24 0 112 2 89 24 0 167 0 0 32 0 75 0 188
32 0 145 1 139 125)
|#

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
