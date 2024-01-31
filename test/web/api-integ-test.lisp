(defpackage :chipi-web.api-integtest
  (:use :cl :cl-mock :fiveam :endecode :chipi-web.api)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.api-integtest)

(def-suite api-integtests
  :description "Integration/Acceptance tests for the API"
  :in chipi-web.tests:test-suite)

(in-suite api-integtests)

(setf drakma:*header-stream* *standard-output*)

(def-fixture api-start-stop (with-isys)
  (unwind-protect
       (progn
         (when with-isys
           ;; setup a partial environment
           (setf hab:*items* (make-hash-table :test 'equal))
           (isys:ensure-isys))
         (api-env:init :apikey-store apikey-store:*memory-backend*
                       :apikey-lifetime (ltd:duration :day 1))
         (api:start)
         (&body))
    (progn
      (api:stop)
      (setf apikey-store:*apikey-store-backend* nil)
      (uiop:delete-directory-tree (envi:ensure-runtime-dir) :validate t)
      (when with-isys
        (isys:shutdown-isys)
        (setf hab:*items* nil))
      )))

(defun get-header (name headers)
  (cdr (assoc name headers)))

;; --------------------
;; item-plists
;; --------------------

(defun make-get-items-request (headers &optional (item-name nil))
  (drakma:http-request (format nil "http://localhost:8765/items~a"
                               (if item-name
                                   (format nil "/~a" item-name)
                                   ""))
                       :method :get
                       :accept "application/json"
                       ;;:certificate "../../cert/localhost.crt"
                       :additional-headers headers
                       :verify nil))

(test items--check-protection-headers
  (with-fixture api-start-stop (nil)
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
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-get-items-request nil)
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No API key provided\"}")))))

(test items--get-all--403--apikey-not-known
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-get-items-request '(("X-Api-Key" . "abcdef")))
      (declare (ignore headers))
      (is (= status 403))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Unknown API key\"}")))))

(test items--get-all--403--apikey-expired
  (with-fixture api-start-stop (nil)
    (let ((apikey-store:*apikey-life-time-duration* (ltd:duration :sec 1)))
      (let ((apikey-id (apikey-store:create-apikey)))
        (sleep 2.0)
        (multiple-value-bind (body status headers)
            (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
          (declare (ignore headers))
          (is (= status 403))
          (is (equal (octets-to-string body)
                     "{\"error\":\"API key has expired\"}")))))))

(test items--get-all--empty--200--ok
  (with-fixture api-start-stop (nil)
    (let ((apikey-id (apikey-store:create-apikey)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal (octets-to-string body)
                   "[]"))))))

(defun equal-item-lists-p (json-string plists)
  (format t "json-string: ~a~%" json-string)
  (let ((json-objs (jzon:parse json-string)))
    (assert (= (length json-objs) (length plists)) nil "Length mismatch")
    (dolist (plist plists)
      (loop :for (key value) :on plist :by #'cddr
            :for key-string := (string-downcase (symbol-name key))
            :do
               (format t "~a => ~a~%" key value)
               (assert (find-if
                        (lambda (ht)
                          (let ((ht-val (gethash key-string ht)))
                            (format t "~a ==? ~a (ht-value)~%" value ht-val)
                            (maphash (lambda (k v)
                                       (format t "ht: ~a => ~a~%" k v)) ht)
                            (cond
                              ((eq value nil)
                               (equal ht-val 'cl:null))
                              ((floatp ht-val)
                               (eql (coerce ht-val 'single-float) value))
                              (t
                               (equal ht-val value)))))
                                (coerce json-objs 'list))
                       nil (format nil "Key '~a' with value '~a' not found in json-obj" key value)))))
  t)

(test items--get-all--some--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey))
           (items (list
                   (hab:defitem 'foo "label1" 'string :initial-value "bar")))
           (item-plists (mapcar #'itemsc:item-to-plist items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-plists))))))

(test items--get-all--supported-value-types--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey))
           (items (list
                   (hab:defitem 'foo "label-int" 'integer :initial-value 1)
                   (hab:defitem 'foo2 "label-float" 'float :initial-value 1.1)
                   (hab:defitem 'foo3 "label-string" 'string :initial-value "bar")
                   (hab:defitem 'foo4 "label-true" 'boolean :initial-value 'item:true)
                   (hab:defitem 'foo5 "label-false" 'boolean :initial-value 'item:false)
                   (hab:defitem 'foo6 "label-null" nil :initial-value nil)))
           (item-plists (mapcar #'itemsc:item-to-plist items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-plists))))))

(test items--get-specific-item--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey))
           (items (list
                   (hab:defitem 'foo "label1" 'string :initial-value "bar")))
           (item-plists (mapcar #'itemsc:item-to-plist items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-plists))))))

(test items--get-specific-item--404--not-found
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
        (declare (ignore headers))
        (is (= status 404))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Item 'FOO' not found\"}"))))))

(defun make-post-item-request (headers item-id value)
  (drakma:http-request (format nil "http://localhost:8765/items/~a" item-id)
                       :method :post
                       :accept "application/json"
                       ;;:certificate "../../cert/localhost.crt"
                       :content value
                       :content-type "application/json"
                       :additional-headers headers
                       :verify nil))

(test items--post-item-value--204--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey)))
      (hab:defitem 'foo "label1" 'string :initial-value "bar")
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  "{\"value\":\"baz\"}")
        (declare (ignore headers body))
        (is (= status 204))))))

(test items--post-item-value--400--wrong-json-payload
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey)))
      (hab:defitem 'foo "label1" 'string :initial-value "bar")
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  "{\"ve\":\"baz\"}")
        (declare (ignore headers))
        (is (= status 400))
        (is (equal (octets-to-string body)
                   "{\"error\":\"No 'value' key found in JSON payload\"}"))))))

(test items--post-item-value--404--not-found
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey)))
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo" "{\"value\":\"baz\"}")
        (declare (ignore headers))
        (is (= status 404))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Item 'FOO' not found\"}"))))))

(test items--post-item-value--supported-value-types--ok
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey))
          (item (hab:defitem 'foo "label1" nil :initial-value 0)))
      (labels ((get-item-value ()
                 (let ((value (item:item-state-value (item:get-item-stateq item))))
                   (format t "value: ~a~%" value)
                   value))
               (post-value (value verification)
                 (format t "POSTING: ~a~%" value)
                 (multiple-value-bind (body status headers)
                     (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                             "foo"
                                             (format nil "{\"value\":~a}" value))
                   (declare (ignore headers body))
                   (is (= status 204))
                   (is-true (miscutils:await-cond 0.5
                              (equal (get-item-value) verification))))))
        (post-value 1 1)
        (post-value 1.1 1.1)
        (post-value "\"foo\"" "foo")
        (post-value "true" 'item:true)
        (post-value "false" 'item:false)
        (post-value "null" 'cl:null)))))

(test items--post-item-value--500--parse-error
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey)))
      (hab:defitem 'foo "label1" 'string :initial-value "bar")
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  "{\"value\":something}")
        (declare (ignore headers))
        (is (= status 500))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Unable to parse JSON\"}"))))))

(test items--post--check-payload-length
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey)))
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  (format nil "{\"value\":\"~a\"}"
                                          (make-string 512
                                                       :initial-element #\a)))
        (declare (ignore headers))
        (is (= status 413))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Oversized payload\"}"))))))
