(defpackage :chipi-web.auth-controller-test
  (:use :cl :fiveam :cl-mock :chipi-web.auth-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.auth-controller-test)

(def-suite auth-controller-tests
  :description "Tests for auth controller"
  :in chipi-web.tests:test-suite)

(in-suite auth-controller-tests)

(test verify-apikey--ok
  (with-mocks ()
    (answer apikey-store:exists-apikey-p t)
    (answer apikey-store:expired-apikey-p nil)
    (verify-apikey "apikey-id")
    (is (= 1 (length (invocations 'apikey-store:exists-apikey-p))))
    (is (= 1 (length (invocations 'apikey-store:expired-apikey-p))))))

(test verify-apikey--not-existing-key-raises-error
  (with-mocks ()
    (answer apikey-store:exists-apikey-p nil)
    (signals apikey-unknown-error
      (verify-apikey "apikey-id"))))

(test verify-apikey--expired-key-raises-error
  (with-mocks ()
    (answer apikey-store:exists-apikey-p t)
    (answer apikey-store:expired-apikey-p t)
    (signals apikey-expired-error
      (verify-apikey "apikey-id"))))
