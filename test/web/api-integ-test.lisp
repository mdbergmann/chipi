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
         (chipi-web.api:start)
         (&body))
    (progn
      (chipi-web.api:stop))))

(defun make-auth-request (params)
  (drakma:http-request "http://localhost:8443/api/authenticate"
                       :method :post
                       ;;:certificate "../cert/localhost.crt"
                       ;;:key "../cert/localhost.key"
                       :parameters params))

(test auth--missing-username
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "foo")))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Missing username or password\"}")))))

(test auth--missing-password
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("password" . "bar")))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Missing username or password\"}")))))

(test auth--too-long-username
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        ;; max 30 chars
        (make-auth-request '(("username" . "1234567890123456789012345678901")
                             ("password" . "bar")))
      (is (= status 403))
      (is (equal (babel:octets-to-string body)
                 "{\"error\":\"Invalid username. Must be 2-30 characters with only alpha numeric and number characters.\"}")))))

(test auth--check-protection-headers
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-auth-request '(("username" . "foo")
                             ("password" . "bar")))
      (is (= status 200))
      (is (equal (cdr (assoc :content-type headers))
                 "application/json"))
      (is (equal (cdr (assoc :x-xss-protection headers))
                 "0"))
      (is (equal (cdr (assoc :x-content-type-options headers))
                 "nosniff"))
      (is (equal (cdr (assoc :x-frame-options headers))
                 "DENY"))
      (is (equal (cdr (assoc :cache-control headers))
                 "no-store"))
      (is (equal (cdr (assoc :content-security-policy headers))
                 "default-src 'none'; frame-ancestors 'none'; sandbox"))
      (print headers)
      )))

;; OK do: input validation on length
;; OK do: return json with error message
;; OK do: check on valid characters for username
;; => do: XSS protection
(run! 'api-integtests)
