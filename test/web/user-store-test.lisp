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

(def-fixture simple-file-store ()
  (unwind-protect
       (progn
         (setf *user-store-backend*
               (make-simple-file-backend #p"/tmp/chipi-web-test/"))
         (&body))
    (progn
      (uiop:delete-directory-tree #p"/tmp/chipi-web-test/" :validate t)
      (setf *user-store-backend* nil))))

(test add-user--ok
  (with-fixture simple-file-store ()
    (is-true (add-user (make-user "test" "test")))
    (is-true (uiop:file-exists-p
              (user-store::filepath *user-store-backend*)))))

(test exists-username--user-exists
  (with-fixture simple-file-store ()
    (is-true (exists-username-p "admin"))))

(test exists-username--user-not-exists
  (is-false (exists-username-p "not-exists")))

(test equals-password--correct-password
  (is-true (equals-password-p "admin" "admin")))

(test equals-password--incorrect-password
  (is-false (equals-password-p "admin" "wrong")))

(test equals-password--user-not-exists
  (is-false (equals-password-p "not-exists" "wrong")))
