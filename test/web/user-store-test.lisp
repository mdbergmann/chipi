(defpackage :chipi-web.user-store-test
  (:use :cl :fiveam :chipi-web.user-store)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.user-store-test)

(def-suite user-store-tests
  :description "Tests for user storage."
  :in chipi-web.tests:test-suite)

(in-suite user-store-tests)

(test exists-username--user-exists
  (is-true (exists-username-p "admin")))

(test exists-username--user-not-exists
  (is-false (exists-username-p "not-exists")))

(test equals-password--correct-password
  (is-true (equals-password-p "admin" "admin")))

(test equals-password--incorrect-password
  (is-false (equals-password-p "admin" "wrong")))

(test equals-password--user-not-exists
  (is-false (equals-password-p "not-exists" "wrong")))
