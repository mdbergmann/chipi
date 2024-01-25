(defpackage :chipi-web.apikey-store-test
  (:use :cl :endecode :fiveam :cl-mock :chipi-web.apikey-store)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.apikey-store-test)

(def-suite apikey-store-tests
  :description "Tests for apikey store - memory backend"
  :in chipi-web.tests:test-suite)

(in-suite apikey-store-tests)

(def-fixture mem-backend ()
  (unwind-protect
       (progn
         (setf *apikey-store-backend* *memory-backend*)
         (&body))
    (setf *apikey-store-backend* nil)))

(test create-apikey
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::store-apikey t)
      (let ((apikey-id (create-apikey)))
        (is-true apikey-id)
        (is (> (length apikey-id) 0))
        (is (stringp apikey-id))
        (is-true (base64-string-to-octets apikey-id t)))
      (is (= (length (invocations 'apikey-store::store-apikey)) 1)))))

(test read-apikey
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::retrieve-apikey
        (make-instance 'apikey
                       :identifier "apikey-id"))
      (let ((apikey (read-apikey "apikey-id")))
        (is (= (length (invocations 'apikey-store::retrieve-apikey)) 1))
        (is-true (typep apikey 'apikey))
        (is (string= (identifier apikey) "apikey-id"))
        (is (integerp (expiry apikey)))))))

(test revoke-apikey--existing
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::delete-apikey t)
      (is-true (revoke-apikey "apikey-id"))
      (is (= (length (invocations 'apikey-store::delete-apikey)) 1)))))

(test revoke-apikey--not-existing
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::delete-apikey nil)
      (is-false (revoke-apikey "apikey-id"))
      (is (= (length (invocations 'apikey-store::delete-apikey)) 1)))))

(test apikey-expired--delete-when-expired
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::retrieve-apikey
        (make-instance 'apikey
                       :identifier "apikey-id"
                       :expiry (- (get-universal-time) 1)))
      (answer apikey-store::delete-apikey t)
      (is-true (expired-apikey-p "apikey-id"))
      (is (= (length (invocations 'apikey-store::delete-apikey)) 1)))))

(test apikey-not-expired
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::retrieve-apikey
        (make-instance 'apikey
                       :identifier "apikey-id"))
      (is-false (expired-apikey-p "apikey-id")))))

(test apikey-exists
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::retrieve-apikey
        (make-instance 'apikey
                       :identifier "apikey-id"))
      (is-true (exists-apikey-p "apikey-id")))))

(test apikey-not-exists
  (with-fixture mem-backend ()
    (with-mocks ()
      (answer apikey-store::retrieve-apikey nil)
      (is-false (exists-apikey-p "apikey-id")))))
