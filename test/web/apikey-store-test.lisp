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
      (is-true (assert-signed-apikey signed-apikey-id))
      (is-true (signed-apikey-p signed-apikey-id)))
    (is-true (uiop:file-exists-p
              (apikey-store::filepath *apikey-store-backend*)))))

(test store-loads-on-initialize
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      ;; set new to trigger reload
      (let ((*apikey-store-backend*
              (make-simple-file-backend #p"/tmp/chipi-web-test/")))
        (is-true (exists-apikey-p signed-apikey-id))))))

(test access-rights--persist-and-reload
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey :access-rights '(:read :update))))
      (is (has-access-rights-p apikey-id '(:read)))
      (is (has-access-rights-p apikey-id '(:update)))
      
      (let ((*apikey-store-backend*
              (make-simple-file-backend #p"/tmp/chipi-web-test/")))
        (is (exists-apikey-p apikey-id))
        
        (is (has-access-rights-p apikey-id '(:read)))
        (is (has-access-rights-p apikey-id '(:update)))))))

(test exists-apikey
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-true (exists-apikey-p signed-apikey-id)))))

(test exists-apikey--not-exists
  (with-fixture simple-file-backend ()
    ;; create unstored apikey
    (multiple-value-bind (apikey unstored-apikey-id)
        (apikey-store::%make-signed-apikey)
      (declare (ignore apikey))
      (is-false (exists-apikey-p unstored-apikey-id)))))

(test exists-apikey--structure-check--wrong-structure
  (with-fixture simple-file-backend ()
    (signals apikey-invalid-error
      (exists-apikey-p "foo"))))

(test exists-apikey--signature-check--wrong-signature
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (destructuring-bind (plain-id sign)
          (str:split #\. signed-apikey-id)
        (declare (ignore sign))
        (signals apikey-invalid-sig-error
          (exists-apikey-p (format nil "~a.bar" plain-id)))))))

(test exists-apikey--revoke-apikey-and-return-nil-when-expired
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration :sec 0)))
      (let ((signed-apikey-id (create-apikey)))
        (sleep 1)
        (is-false (exists-apikey-p signed-apikey-id))))))

(test revoke-apikey--existing
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (is-true (revoke-apikey signed-apikey-id))
      (is-false (exists-apikey-p signed-apikey-id))

      ;; reload to check
      (let ((*apikey-store-backend*
              (make-simple-file-backend #p"/tmp/chipi-web-test/")))
        (is-false (exists-apikey-p signed-apikey-id))))))

(test revoke-apikey--wrong-sig
  (with-fixture simple-file-backend ()
    (let ((signed-apikey-id (create-apikey)))
      (destructuring-bind (plain-id sign)
          (str:split #\. signed-apikey-id)
        (declare (ignore sign))
        (signals apikey-invalid-sig-error
          (revoke-apikey (format nil "~a.bar" plain-id)))))))

(test private--retrieve-expired-apikey-ids
  (with-fixture simple-file-backend ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration)))
      (create-apikey)
      (create-apikey)
      (sleep 1)
      (let ((apikey-ids (apikey-store::retrieve-expired-apikeys)))
        (is (= (length apikey-ids) 2))
        (is (every #'stringp apikey-ids))
        (is (every #'assert-signed-apikey apikey-ids))))))

;; ---------------------------------
;; access-rights
;; ---------------------------------

(test access-rights--invalid-args
  (with-fixture simple-file-backend ()
    (signals type-error
      (has-access-rights-p 1234 '()))
    (signals type-error
      (has-access-rights-p 1234 "foo"))
    ))

(test access-rights--no-requested-access-rights-not-allowed
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey :access-rights '())))
      (signals access-rights-error
        (has-access-rights-p apikey-id '())))))

(test access-rights--returns-false-on-insufficient-rights
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey :access-rights '())))
      (is (not (has-access-rights-p apikey-id '(:read))))
      (is (not (has-access-rights-p apikey-id '(:update))))
      (is (not (has-access-rights-p apikey-id '(:admin))))
      )
    (let ((apikey-id (create-apikey :access-rights '(:read))))
      (is (not (has-access-rights-p apikey-id '(:update))))
      (is (not (has-access-rights-p apikey-id '(:admin))))
      )
    (let ((apikey-id (create-apikey :access-rights '(:update))))
      (is (not (has-access-rights-p apikey-id '(:admin))))
      )
    ))

(test access-rights--returns-true-on-sufficient-rights
  (with-fixture simple-file-backend ()
    (let ((apikey-id (create-apikey :access-rights '(:read))))
      (is (has-access-rights-p apikey-id '(:read))))
    (let ((apikey-id (create-apikey :access-rights '(:update))))
      (is (has-access-rights-p apikey-id '(:read :update))))
    (let ((apikey-id (create-apikey :access-rights '(:delete))))
      (is (has-access-rights-p apikey-id '(:read :update :delete))))
    (let ((apikey-id (create-apikey :access-rights '(:admin))))
      (is (has-access-rights-p apikey-id '(:admin :delete :update :read))))))

