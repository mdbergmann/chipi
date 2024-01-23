(defpackage :chipi-web.api-integtest
  (:use :cl :cl-mock :fiveam :endecode :chipi-web.api)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.api-integtest)

(def-suite api-integtests
  :description "Integration/Acceptance tests for the API"
  :in chipi-web.tests:test-suite)

(in-suite api-integtests)

(setf drakma:*header-stream* *standard-output*)

(def-fixture api-start-stop ()
  (unwind-protect
       (progn
         (api-env:init)
         (setf token-store:*token-life-time-duration*
               token-store::*default-token-life-time-duration*)
         (setf token-store:*token-store-backend*
               token-store:*memory-backend*)
         (setf user-store:*user-store-backend*
               (user-store:make-simple-file-backend))
         (user-store:add-user (user-store:make-user "admin" "admin"))
         (chipi-web.api:start)
         (&body))
    (progn
      (chipi-web.api:stop)
      (setf token-store:*token-store-backend* nil)
      (setf user-store:*user-store-backend* nil)
      (uiop:delete-directory-tree (envi:ensure-runtime-dir) :validate t)
      )))

(defun get-header (name headers)
  (cdr (assoc name headers)))

;; --------------------
;; auth
;; --------------------

(defun make-auth-request (params)
  (drakma:http-request "https://localhost:8443/api/session"
                       :method :post
                       :certificate "../../cert/localhost.crt"
                       ;;:content-type "application/json"
                       :parameters params
                       :verify :required))

(test auth--check-protection-headers
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "foo")
                             ("password" . "12345678")))
      (declare (ignore body status))
      (is (equal (get-header :content-type headers)
                 "application/json"))
      (is (equal (get-header :x-xss-protection headers)
                 "0"))
      (is (equal (get-header :x-content-type-options headers)
                 "nosniff"))
      (is (equal (get-header :x-frame-options headers)
                 "DENY"))
      (is (equal (get-header :cache-control headers)
                 "no-store"))
      (is (equal (get-header :content-security-policy headers)
                 "default-src 'none'; frame-ancestors 'none'; sandbox"))
      )))

(test auth--missing-username
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "foo")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Missing username or password\"}")))))

(test auth--missing-password
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("password" . "bar")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Missing username or password\"}")))))

(test auth--too-long-username
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        ;; max 30 chars
        (make-auth-request '(("username" . "1234567890123456789012345678901")
                             ("password" . "bar")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Invalid username. Must be 2-30 characters with only alpha numeric and number characters.\"}")))))

(test auth--with-initial-existing-admin-user
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "admin")
                             ("password" . "admin")))
      (declare (ignore headers))
      (is (= status 200))
      (is (str:starts-with-p "\{\"token\":\""
                             (octets-to-string body))))))

(test auth--user-does-not-exist
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "unknown")
                             ("password" . "wrong")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"User not found.\"}")))))

(test auth--admin-auth-fail
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "admin")
                             ("password" . "wrong")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Unable to authenticate.\"}")))))

(defun login-admin ()
  (multiple-value-bind (body)
      (make-auth-request `(("username" . "admin")
                           ("password" . "admin")))
    (cdr (assoc "token"
                (yason:parse
                 (octets-to-string body)
                 :object-as :alist)
                :test #'equal))))

(defun make-logout-request (token-id)
  (drakma:http-request "https://localhost:8443/api/session"
                       :method :delete
                       :certificate "../../cert/localhost.crt"
                       :key "../../cert/localhost.key"
                       :additional-headers (if token-id
                                               `(("X-Auth-Token" . ,token-id))
                                               '())))

(test auth--logout
  (with-fixture api-start-stop ()
    (let ((token-id (login-admin)))
      (is-true (token-store:read-token token-id))
      (multiple-value-bind (body status headers)
          (make-logout-request token-id)
        (declare (ignore body headers))
        (is (= status 200))
        (is-false (token-store:read-token token-id))))))

(test auth--logout--no-token-header
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-logout-request nil)
      (declare (ignore headers))
      (is (= status 400))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No X-Auth-Token header\"}")))))

;; --------------------
;; items
;; --------------------

(defun make-get-items-request (headers)
  (drakma:http-request "https://localhost:8443/api/items"
                       :method :get
                       :certificate "../../cert/localhost.crt"
                       :key "../../cert/localhost.key"
                       :additional-headers headers))

(test items--get-all--401--no-auth-header
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request nil)
      (declare (ignore body))
      (is (= status 401))
      (is (equal (get-header :www-authenticate headers)
                 "Bearer realm=\"chipi\", error=\"no token\", error_description=\"No Authorization header\"")))))

(test items--get-all--401--no-token
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request '(("Authorization" . "Bearer")))
      (declare (ignore body))
      (is (= status 401))
      (is (equal (get-header :www-authenticate headers)
                 "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"No token provided\"")))))

(test items--get-all--401--token-not-known
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request '(("Authorization" . "Bearer abcdef")))
      (declare (ignore body))
      (is (= status 401))
      (is (equal (get-header :www-authenticate headers)
                 "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"Unknown token\"")))))

(test items--get-all--401--token-expired
  (with-fixture api-start-stop ()
    (setf token-store::*token-life-time-duration* (ltd:duration :sec 1))
    (let ((token-id (login-admin)))
      (sleep 2.5)
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("Authorization" . ,(format nil "Bearer ~a" token-id))))
        (declare (ignore body))
        (is (= status 401))
        (is (equal (get-header :www-authenticate headers)
                   "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"Token has expired\""))))))

(test items--get-all--empty-ok
  (with-fixture api-start-stop ()
    (let ((token-id (login-admin)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("Authorization" . ,(format nil "Bearer ~a" token-id))))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal (octets-to-string body)
                   "[]"))))))

(test items--get-all--with-actually-some
  (with-fixture api-start-stop ()
    (let ((token-id (login-admin)))
      (with-mocks ()
        (answer itemsc:retrieve-items '((:name "foo" :label "label1" :value "bar")
                                        (:name "foo2" :label "label2" :value "baz")))
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("Authorization" . ,(format nil "Bearer ~a" token-id))))
          (declare (ignore headers))
          (is (= status 200))
          (is (equal (octets-to-string body)
                     "[{\"name\":\"foo\",\"value\":\"bar\",\"label\":\"label1\"},{\"name\":\"foo2\",\"value\":\"baz\",\"label\":\"label2\"}]")))))))

;; --------------------
;; users
;; --------------------

;; this test is not useful here because /auth endpoint just authenticates
;; it doesn't create new users
;; (test auth--too-short-password
;;   (with-fixture api-start-stop ()
;;     (multiple-value-bind (body status headers)
;;         ;; min 8 chars
;;         (make-auth-request '(("username" . "foobarbaz")
;;                              ("password" . "2short")))
;;       (declare (ignore headers))
;;       (is (= status 403))
;;       (is (equal (octets-to-string body)
;;                  "{\"error\":\"Invalid password. Must be at least 8 characters.\"}")))))

