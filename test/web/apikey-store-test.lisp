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
       (let ((*apikey-store-backend*
               (make-simple-file-backend #p"/tmp/chipi-web-test/")))
         (&body))
    (uiop:delete-directory-tree #p"/tmp/chipi-web-test/" :validate t)))

(defun assert-signed-apikey (signed-apikey-id)
  (destructuring-bind (plain-id sign)
      (str:split #\. signed-apikey-id)
    (and plain-id
         sign
         (> (length plain-id) 0)
         (> (length sign) 0))))

(test create-apikey--ok
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-true signed-apikey-id)
      (is (> (length signed-apikey-id) 0))
      (is (stringp signed-apikey-id))
      (is-true (assert-signed-apikey signed-apikey-id)))
    (is-true (uiop:file-exists-p
              (apikey-store::filepath *apikey-store-backend*)))))

(test store-loads-apikeys-on-initialize
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      ;; set new to trigger reload
      (let ((*apikey-store-backend*
              (make-simple-file-backend #p"/tmp/chipi-web-test/")))
        (is-true (exists-apikey-p signed-apikey-id))))))

(test retrieve-apikey--existing
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (let ((retrieve-apikey (retrieve-apikey signed-apikey-id)))
        (is-true (typep retrieve-apikey 'apikey))
        (is (str:starts-with-p (identifier retrieve-apikey) signed-apikey-id))
        (is (integerp (expiry retrieve-apikey)))))))

(test retrieve-apikey--not-existing
  (with-fixture simple-file-backend ()
    (signals simple-type-error
      (retrieve-apikey "some key plain-id"))))

(test revoke-apikey--existing
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-true (revoke-apikey signed-apikey-id))
      (is-false (exists-apikey-p signed-apikey-id))

      ;; reload to check
      (let ((*apikey-store-backend*
              (make-simple-file-backend #p"/tmp/chipi-web-test/")))
        (is-false (exists-apikey-p signed-apikey-id))))))

(test expired-apikey--delete-when-expired
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration :sec 0)))
      (let ((signed-apikey-id (create-apikey)))
        (sleep 1)
        (is-true (expired-apikey-p signed-apikey-id))
        (is-false (exists-apikey-p signed-apikey-id))))))

(test expired-apikey--not-expired
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-false (expired-apikey-p signed-apikey-id)))))

(test expired-apikey--apikey-does-not-exist
  (with-fixture simple-file-backend ()
    (signals error (expired-apikey-p "some.key"))))

(test exists-apikey
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-true (exists-apikey-p signed-apikey-id)))))

(test exists-apikey--not-exists
  (with-fixture simple-file-backend ()
    (is-false (exists-apikey-p "foo.bar"))))

(test signature-check--wrong-signature
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (destructuring-bind (plain-id sign)
          (str:split #\. signed-apikey-id)
        (declare (ignore sign))
        (is-false (exists-apikey-p (format nil "~a.bar" plain-id)))))))

(test retrieve-expired-apikey-ids
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration)))
      (create-apikey)
      (create-apikey)
      (sleep 1)
      (let ((apikey-ids (retrieve-expired-apikeys)))
        (is (= (length apikey-ids) 2))
        (is (every #'stringp apikey-ids))
        (is (every #'assert-signed-apikey apikey-ids))))))
