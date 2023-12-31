(defpackage :chipi-web.token-store-test
  (:use :cl :fiveam :cl-mock :chipi-web.token-store)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.token-store-test)

(def-suite token-store-tests
  :description "Tests for token store"
  :in chipi-web.tests:test-suite)

(in-suite token-store-tests)

(test create-token
  (with-mocks ()
    (answer (token-store::store-token token) t)

    (let ((token-id (create-token "username")))
      (is-true token-id)
      (is (> (length token-id) 0))
      (is (stringp token-id))
      (is-true (base64:base64-string-to-usb8-array token-id :uri t)))
    (is (= (length (invocations 'token-store::store-token)) 1))))

(test read-token
  (with-mocks ()
    (answer token-store::retrieve-token
      (make-instance 'token
                     :user-id "username"
                     :token-id "token-id"))

    (let ((token (read-token "token-id")))
      (is (= (length (invocations 'token-store::retrieve-token)) 1))
      (is-true (typep token 'token))
      (is (string= (token-id token) "token-id"))
      (is (string= (user-id token) "username"))
      (is (integerp (expiry token)))
      )))

(run! 'token-store-tests)
