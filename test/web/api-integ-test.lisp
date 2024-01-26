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
         (api-env:init :apikey-store apikey-store:*memory-backend*
                       :apikey-lifetime (ltd:duration :day 1))
         (api:start)
         (&body))
    (progn
      (api:stop)
      (setf apikey-store:*apikey-store-backend* nil)
      (uiop:delete-directory-tree (envi:ensure-runtime-dir) :validate t)
      )))

(defun get-header (name headers)
  (cdr (assoc name headers)))

;; --------------------
;; items
;; --------------------

(defun make-get-items-request (headers &optional (item-name nil))
  (drakma:http-request (format nil "https://localhost:8443/items~a"
                               (if item-name
                                   (format nil "/~a" item-name)
                                   ""))
                       :method :get
                       :accept "application/json"
                       ;;:certificate "../../cert/localhost.crt"
                       :additional-headers headers
                       :verify :required))

(test items--check-protection-headers
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request nil)
      (declare (ignore body status))
      ;; (is (equal (get-header :content-type headers)
      ;;            "application/json"))
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

(test items--get-all--403--require-api-key
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request nil)
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No API key provided\"}")))))

(test items--get-all--403--apikey-not-known
  (with-fixture api-start-stop ()
    (multiple-value-bind (body status headers)
        (make-get-items-request '(("X-Api-Key" . "abcdef")))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Unknown API key\"}")))))

(test items--get-all--403--apikey-expired
  (with-fixture api-start-stop ()
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration :sec 1)))
      (let ((apikey-id (apikey-store:create-apikey)))
        (sleep 2.0)
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
          (is (= status 403))
          (is (equal (octets-to-string body)
                     "{\"error\":\"API key has expired\"}")))))))

(test items--get-all--empty-ok
  (with-fixture api-start-stop ()
    (let ((apikey-id (apikey-store:create-apikey)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal (octets-to-string body)
                   "[]"))))))

(test items--get-all--some-ok
  (with-fixture api-start-stop ()
    (let ((apikey-id (apikey-store:create-apikey)))
      (with-mocks ()
        (answer itemsc:retrieve-items
          '((:name "foo" :label "label1" :value "bar" :timestamp 1234567890)
            (:name "foo2" :label "label2" :value "baz" :timestamp 1234567891))
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
          (declare (ignore headers))
          (is (= status 200))
          (is (equal (octets-to-string body)
                     "[{\"name\":\"foo\",\"label\":\"label1\",\"value\":\"bar\",\"timestamp\":1234567890},{\"name\":\"foo2\",\"label\":\"label2\",\"value\":\"baz\",\"timestamp\":1234567891}]"))))))))

(test items--get-specific-item--ok
  (with-fixture api-start-stop ()
    (let ((apikey-id (apikey-store:create-apikey)))
      (with-mocks ()
        (answer itemsc:retrieve-item
          '(:name "foo" :label "label1" :value "bar" :timestamp 1234567890))
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
          (declare (ignore headers))
          (is (= status 200))
          (is (equal (octets-to-string body)
                     "[{\"timestamp\":1234567890,\"name\":\"foo\",\"value\":\"bar\",\"label\":\"label1\"}]")))))))

(test items--get-specific-item--not-found
  (with-fixture api-start-stop ()
    (let ((apikey-id (apikey-store:create-apikey)))
      (with-mocks ()
        (answer itemsc:retrieve-item nil)
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
          (declare (ignore headers))
          (is (= status 404))
          (is (equal (octets-to-string body)
                     "{\"error\":\"Item 'FOO' not found\"}")))))))

;; (test items--post-value--changes-item-value
;;   (with-fixture api-start-stop ()
;;     (let ((apikey-id (apikey-store:create-apikey)))
;;       (with-mocks ()
;;         (answer itemsc:retrieve-items '((:name "foo" :label "label1" :value "bar")
;;                                         (:name "foo2" :label "label2" :value "baz")))
;;         (answer (itemsc:update-item-value iname ivalue)
;;           (assert (equal iname "foo" nil))
;;           (assert (equal ivalue "bar" nil))
;;           t)
;;         (multiple-value-bind (body status headers)
;;             (drakma:http-request "https://localhost:8443/items/foo"
;;                                  :method :post
;;                                  :accept "application/json"
;;                                  :additional-headers `(("X-Api-Key" . ,apikey-id))
;;                                  :contents "bar2"
;;                                  :verify :required)
;;           (declare (ignore headers))
;;           (is (= status 204)))))))
