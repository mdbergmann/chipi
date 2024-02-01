(defpackage :chipi-web.apikey-store-test
  (:use :cl :endecode :fiveam :cl-mock :chipi-web.apikey-store)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.apikey-store-test)

(def-suite apikey-store-tests
  :description "Tests for apikey store - simple-file backend"
  :in chipi-web.tests:test-suite)

(in-suite apikey-store-tests)

(def-fixture simple-file-backend ()
  (unwind-protect
       (progn
         (setf *apikey-store-backend*
               (make-simple-file-backend #p"/tmp/chipi-web-test/"))
         (&body))
    (progn
      (uiop:delete-directory-tree #p"/tmp/chipi-web-test/" :validate t)
      (setf *apikey-store-backend* nil))))

(test create-apikey--ok
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      (is-true apikey-id)
      (is (> (length apikey-id) 0))
      (is (stringp apikey-id))
      (is-true (base64-string-to-octets apikey-id t)))
    (is-true (uiop:file-exists-p
              (apikey-store::filepath *apikey-store-backend*)))))

(test store-loads-apikeys-on-initialize
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      ;; set new to trigger reload
      (setf *apikey-store-backend*
            (make-simple-file-backend #p"/tmp/chipi-web-test/"))
      (is-true (exists-apikey-p apikey-id)))))

(test read-apikey--existing
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      (let ((read-apikey (read-apikey apikey-id)))
        (is-true (typep read-apikey 'apikey))
        (is (string= (identifier read-apikey) apikey-id))
        (is (integerp (expiry read-apikey)))))))

(test read-apikey--not-existing
  (with-fixture simple-file-backend ()
    (is-false (read-apikey "some key id"))))

(test revoke-apikey--existing
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      (is-true (revoke-apikey apikey-id))
      (is-false (exists-apikey-p apikey-id))

      ;; reload to check
      (setf *apikey-store-backend*
            (make-simple-file-backend #p"/tmp/chipi-web-test/"))
      (is-false (exists-apikey-p apikey-id)))))

(test apikey-expired--delete-when-expired
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration :sec 1)))
      (let ((apikey-id (create-apikey)))
        (sleep 2)
        (is-true (expired-apikey-p apikey-id))
        (is-false (exists-apikey-p apikey-id))))))

(test apikey-not-expired
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      (is-false (expired-apikey-p apikey-id)))))

(test apikey-exists
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey)))
      (is-true (exists-apikey-p apikey-id)))))

(test apikey-not-exists
  (with-fixture simple-file-backend ()
    (is-false (exists-apikey-p "foo"))))

(test retrieve-expired-apikeys
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration)))
      (create-apikey)
      (create-apikey)
      (sleep 1)
      (let ((apikeys (retrieve-expired-apikeys)))
        (is (= (length apikeys) 2))
        (is (every #'apikey-p apikeys))))))
