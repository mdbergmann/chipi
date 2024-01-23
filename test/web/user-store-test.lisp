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
    (is-true (exists-username-p "test"))
    (is-true (uiop:file-exists-p
              (user-store::filepath *user-store-backend*)))))

(test store-loads-user-on-initialize
  (with-fixture simple-file-store ()
    (is-true (add-user (make-user "test" "test")))

    (setf *user-store-backend*
          (make-simple-file-backend #p"/tmp/chipi-web-test/"))
    (is-true (exists-username-p "test"))))

(test exists-username--user-not-exists
  (with-fixture simple-file-store ()
    (is-false (exists-username-p "not-exists"))))

(test equals-password--correct-password
  (with-fixture simple-file-store ()
    (add-user (make-user "test" "test"))
    (is-true (equals-password-p "test" "test"))))

(test equals-password--incorrect-password
  (with-fixture simple-file-store ()
    (add-user (make-user "test" "test"))
    (is-false (equals-password-p "test" "wrong"))))

(test equals-password--user-not-exists
  (with-fixture simple-file-store ()
    (is-false (equals-password-p "not-exists" "wrong"))))
