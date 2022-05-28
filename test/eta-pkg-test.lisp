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
  (is (equalp '(nil #(#\{ 1 2)) (multiple-value-list (collect-data #(#\{ 1) #(2))))))

(test collect-data--full-package
  (is (equalp '(t #(#\{ 1 2 #\})) (multiple-value-list (collect-data #(#\{ 1) #(2 #\}))))))


(test extract-pkg--fail-empty
  (is (equalp '(:fail "Undersized package!") (multiple-value-list (extract-pkg #())))))

(test extract-pkg--monitor--one-item
  (is (equalp (list :monitor (list (cons "BoilerUnten" 54.5)))
              (multiple-value-list
               (extract-pkg
                #(#\{
                  #\M #\D ; service identifier
                  5 ; payload len
                  3 ; checksum of payload
                  0 ; node id
                  0 167 ; monitor id two byte
                  2 33 ; value two byte (545) / divisor 10
                  #\}))))))
