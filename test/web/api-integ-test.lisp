(defpackage :chipi-web.api-integtest
  (:use :cl :fiveam :chipi-web.api)
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
         (setf token-store::*token-life-time-seconds*
               token-store::*default-token-life-time-seconds*)
         (chipi-web.api:start)
         (&body))
    (progn
      (chipi-web.api:stop))))

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
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Missing username or password\"}")))))

(test auth--missing-password
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("password" . "bar")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Missing username or password\"}")))))

(test auth--too-long-username
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        ;; max 30 chars
        (make-auth-request '(("username" . "1234567890123456789012345678901")
                             ("password" . "bar")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Parameter validation failed: Invalid username. Must be 2-30 characters with only alpha numeric and number characters.\"}")))))

(test auth--with-initial-existing-admin-user
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "admin")
                             ("password" . "admin")))
      (declare (ignore headers))
      (is (= status 200))
      (is (str:starts-with-p "\{\"token\":\""
                             (babel:octets-to-string body))))))

(test auth--user-does-not-exist
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "unknown")
                             ("password" . "wrong")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"User not found.\"}")))))

(test auth--admin-auth-fail
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "admin")
                             ("password" . "wrong")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Unable to authenticate.\"}")))))

(defun login-admin ()
  (multiple-value-bind (body)
      (make-auth-request `(("username" . "admin")
                           ("password" . "admin")))
    (cdr (assoc "token"
                (yason:parse
                 (babel:octets-to-string body)
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
      (is (equal (babel:octets-to-string body)
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
    (setf token-store::*token-life-time-seconds* 1)
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
        (is (equal (babel:octets-to-string body)
                   "{\"items\":[]}"))))))

;; TODO: add some real items

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
;;       (is (equal (babel:octets-to-string body)
;;                  "{\"error\":\"Invalid password. Must be at least 8 characters.\"}")))))


;;(run! 'api-integtests)

;; OK do: input validation on length
;; OK do: return json with error message
;; OK do: check on valid characters for username
;; OK do: XSS protection
;; OK send request parameters as json
;; OK check minimum password length
;; OK use scrypt to hash password to compare against stored password
;; OK use a manually generated 'admin' user with scrypted password for initial login in order to use the rest of the API
;; OK implement token auth with bearer
;; OK logout
;; OK check expiry - provide expired-token-p function to token-store and revoke token if expired

#|

TODOS:

=> - implement additional 'controller' layer for auth, items, etc.
- setup runtime folder in 'system' folder
  ;;(print (asdf:system-relative-pathname "chipi-web" ""))
- generate (and read) salt in runtime folder
- store users in runtime folder
- make sure user storage is thread-safe
- initialize environment (chipi.env) on startup, if it isn't already
- have a thread, or actor that cleans up expired tokens via scheduler
- implement retrieving refresh-token with longer expiry
- access-control
- audit log
- pre-flight?
- CORS headers

|#

