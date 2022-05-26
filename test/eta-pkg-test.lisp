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
