(defpackage :chipi-api.auth-controller-test
  (:use :cl :fiveam :cl-mock :chipi-api.auth-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-api.auth-controller-test)

(def-suite auth-controller-tests
  :description "Tests for auth controller"
  :in chipi-api.tests:test-suite)

(in-suite auth-controller-tests)

(test verify-apikey--ok
  (with-mocks ()
    (answer apikey-store:exists-apikey-p t)
    (verify-apikey "apikey.id")
    (is (= 1 (length (invocations 'apikey-store:exists-apikey-p))))))

(test verify-apikey--not-existing-key-raises-error
  (with-mocks ()
    (answer apikey-store:exists-apikey-p nil)
    (signals auth-apikey-unknown-error
      (verify-apikey "apikey.id"))))

(test verify-apikey--invalid-apikey-raises-error
  (with-mocks ()
    (answer apikey-store:exists-apikey-p
      (error 'apikey-store:apikey-invalid-error))
    (handler-case
        (verify-apikey "apikey-id")
      (auth-apikey-invalid-error (c)
        (is (equal "Invalid API key structure" (simple-condition-format-control c))))
      (:no-error ()
        (fail "Expected auth-apikey-invalid-error to be signaled")))))

(test verify-access-rights--ok
  (with-mocks ()
    (answer apikey-store:has-access-rights-p t)
    (verify-access-rights "apikey.id" '(:read))
    (is (= 1 (length (invocations 'apikey-store:has-access-rights-p))))))

(test verify-access-rights--not-allowed
  (with-mocks ()
    (answer apikey-store:has-access-rights-p nil)
    (handler-case
        (verify-access-rights "apikey.id" '(:read))
      (auth-access-rights-error (c)
        (is (equal "Insufficient access rights" (simple-condition-format-control c))))
      (:no-error ()
        (fail "Expected auth-access-rights-error to be signaled")))))
