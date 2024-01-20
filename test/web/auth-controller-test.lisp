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

(test authorize-user--user-does-not-exist
  (with-mocks ()
    (answer user-store:exists-username-p nil)
    (handler-case
        (authorize-user "test" "test")
      (condition (c)
        (is (typep c 'user-not-found)))
      (:no-error ()
        (fail "Should have raised an error")))
    (is (= 1 (length (invocations 'user-store:exists-username-p))))))

(test authorize-user--user-exists--password-does-not-match
  (with-mocks ()
    (answer user-store:exists-username-p t)
    (answer user-store:equals-password-p nil)
    (handler-case
        (authorize-user "admin" "no-match")
      (condition (c)
        (is (typep c 'unable-to-authenticate)))
      (:no-error ()
        (fail "Should have raised an error")))
    (is (= 1 (length (invocations 'user-store:equals-password-p))))))
  
(test authorize-user--ok--returns-token-id
  (with-mocks ()
    (answer user-store:exists-username-p t)
    (answer user-store:equals-password-p t)
    (answer token-store:create-token "token-id")
    (let ((auth-result (authorize-user "admin" "test")))
      (is (typep auth-result 'string))
      (is (equal "token-id" auth-result)))
    (is (= 1 (length (invocations 'token-store:create-token))))))
