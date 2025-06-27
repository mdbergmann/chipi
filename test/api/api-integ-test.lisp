(defpackage :chipi-api.api-integtest
  (:use :cl :cl-mock :fiveam :endecode :chipi-api.api)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-api.api-integtest)

(def-suite api-integtests
  :description "Integration/Acceptance tests for the API"
  :in chipi-api.tests:test-suite)

(in-suite api-integtests)

(setf drakma:*header-stream* *standard-output*)

(def-fixture api-start-stop (with-isys)
  (unwind-protect
       (progn
         (when with-isys
           ;; setup a partial environment
           (hab:defconfig "chipi"))
         (api-env:init :apikey-store apikey-store:*memory-backend*
                       :apikey-lifetime (ltd:duration :day 1))
         (api:start)
         (&body))
    (progn
      (api:stop)
      (setf apikey-store:*apikey-store-backend* nil)
      (uiop:delete-directory-tree (envi:ensure-runtime-dir) :validate t)
      (when with-isys
        (hab:shutdown)
        (sse-manager::cleanup-sse-manager))
      )))

(defun get-header (name headers)
  (cdr (assoc name headers)))

;; --------------------
;; item-hts
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

;; --------------------
;; get all item
;; --------------------

(test items--get-all--401--require-api-key
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-get-items-request nil)
      (declare (ignore headers))
      (is (= status 401))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No API key provided\"}")))))

(test items--get-all--401--apikey-not-known
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (_apikey id)
        (apikey-store::%make-signed-apikey)
      (declare (ignore _apikey))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,id)))
        (declare (ignore headers))
        (is (= status 401))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Unknown API key\"}"))))))

(test items--get-all--401--apikey-invalid
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-get-items-request '(("X-Api-Key" . "abcdef")))
      (declare (ignore headers))
      (is (= status 401))
      (is (equal (octets-to-string body)
                 "{\"error\":\"Invalid API key\"}")))))

(test items--get-all--403--no-access-rights
  (with-fixture api-start-stop (nil)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '())))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 403))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Insufficient access rights\"}"))))))

(test items--get-all--empty--200--ok
  (with-fixture api-start-stop (nil)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
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
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:read)))
           (items (list
                   (hab:defitem 'foo "label1" 'string :initial-value "bar")))
           (item-hts (mapcar #'item-ext:item-to-ht items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-hts))))))

(test items--get-all--supported-value-types--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:read)))
           (items (list
                   (hab:defitem 'foo "label-int" 'integer :initial-value 1)
                   (hab:defitem 'foo2 "label-float" 'float :initial-value 1.1)
                   (hab:defitem 'foo3 "label-string" 'string :initial-value "bar")
                   (hab:defitem 'foo4 "label-true" 'boolean :initial-value 'item:true)
                   (hab:defitem 'foo5 "label-false" 'boolean :initial-value 'item:false)
                   (hab:defitem 'foo6 "label-null" nil :initial-value nil)))
           (item-hts (mapcar #'item-ext:item-to-ht items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-hts))))))

(test items--get-all--with-tags--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:read)))
           (items (list
                   (hab:defitem 'sensor1 "Temperature Sensor" 'float 
                     :initial-value 22.5
                     :tags '((:ui-readonly . t)
                             (:unit . "celsius")
                             (:category . "sensor")))
                   (hab:defitem 'switch1 "Light Switch" 'boolean
                     :initial-value 'item:false
                     :tags '((:ui-readonly . nil)))
                   (hab:defitem 'text1 "Status Text" 'string
                     :initial-value "OK"))))
      (print items)
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        ;; Parse response and verify tags
        (let ((response-items (jzon:parse (octets-to-string body))))
          (print response-items)
          (is (= 3 (length response-items)))
          ;; Check sensor1 has tags
          (let ((sensor1 (find-if (lambda (item) 
                                    (string= "SENSOR1" (gethash "name" item)))
                                  response-items)))
            (is (not (null sensor1)))
            (let ((tags (gethash "tags" sensor1)))
              (is (hash-table-p tags))
              (is (= 3 (hash-table-count tags)))
              ;; Tags should be a hash table with string keys (after JSON parsing)
              (is (equal t (gethash "ui-readonly" tags)))
              (is (equal "celsius" (gethash "unit" tags)))
              (is (equal "sensor" (gethash "category" tags)))))
          ;; Check switch1 has one tag
          (let ((switch1 (find-if (lambda (item)
                                    (string= "SWITCH1" (gethash "name" item)))
                                  response-items)))
            (is (not (null switch1)))
            (let ((tags (gethash "tags" switch1)))
              (is (hash-table-p tags))
              (is (= 1 (hash-table-count tags)))
              (is (equal nil (gethash "ui-readonly" tags)))))
          ;; Check text1 has empty tags hash table
          (let ((text1 (find-if (lambda (item)
                                  (string= "TEXT1" (gethash "name" item)))
                                response-items)))
            (is (not (null text1)))
            (let ((tags (gethash "tags" text1)))
              (is (hash-table-p tags))
              (is (= 0 (hash-table-count tags))))))))))

;; --------------------
;; get specific item
;; --------------------

(test items--get-specific-item--200--ok
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:read)))
           (items (list
                   (hab:defitem 'foo "label1" 'string :initial-value "bar")))
           (item-hts (mapcar #'item-ext:item-to-ht items)))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
        (declare (ignore headers))
        (is (= status 200))
        (is (equal-item-lists-p (octets-to-string body)
                                item-hts))))))

(test items--get-specific-item--404--not-found
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
      (multiple-value-bind (body status headers)
          (make-get-items-request `(("X-Api-Key" . ,apikey-id)) "foo")
        (declare (ignore headers))
        (is (= status 404))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Item 'FOO' not found\"}"))))))

;; --------------------
;; post item value
;; --------------------

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
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:update))))
      (hab:defitem 'foo "label1" 'string :initial-value "bar")
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  "{\"value\":\"baz\"}")
        (declare (ignore headers body))
        (is (= status 204))))))

(test items--post-item-value--403--no-access-rights
  (with-fixture api-start-stop (nil)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo"
                                  "{\"value\":\"baz\"}")
        (declare (ignore headers))
        (is (= status 403))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Insufficient access rights\"}"))))))

(test items--post-item-value--400--wrong-json-payload
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:update))))
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
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:update))))
      (multiple-value-bind (body status headers)
          (make-post-item-request `(("X-Api-Key" . ,apikey-id))
                                  "foo" "{\"value\":\"baz\"}")
        (declare (ignore headers))
        (is (= status 404))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Item 'FOO' not found\"}"))))))

(test items--post-item-value--supported-value-types--ok
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:update)))
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
        (post-value "null" nil)))))

(test items--post-item-value--500--parse-error
  (with-fixture api-start-stop (t)
    (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:update))))
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
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:update))))
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

;; -------------------------------------------------
;; itemgroups helpers + tests
;; -------------------------------------------------

(defun make-get-itemgroups-request (headers &optional (group-name nil))
  (drakma:http-request (format nil "http://localhost:8765/itemgroups~a"
                               (if group-name
                                   (format nil "/~a" group-name)
                                   ""))
                       :method :get
                       :accept "application/json"
                       :additional-headers headers
                       :verify nil))

;; ------------- itemgroups: GET all ----------------

(test itemgroups--get-all--401--require-api-key
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-get-itemgroups-request nil)
      (declare (ignore headers))
      (is (= status 401))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No API key provided\"}")))))

(test itemgroups--get-all--empty--200--ok
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
      (multiple-value-bind (body status headers)
          (make-get-itemgroups-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 200))
        ;; there is at least the default itemgroup
        (is (string= (octets-to-string body) "[{\"name\":\"CH-DEFAULT\",\"label\":\"Default\",\"items\":[]}]"))))))

;; ------------- itemgroups: GET specific ------------

(test itemgroups--get-specific-itemgroup--404--not-found
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
      (multiple-value-bind (body status headers)
          (make-get-itemgroups-request `(("X-Api-Key" . ,apikey-id)) "foo")
        (declare (ignore headers))
        (is (= status 404))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Itemgroup 'FOO' not found\"}"))))))

;; -------------------------------------------------
;; SSE events tests
;; -------------------------------------------------

(defun make-sse-request (headers &key (want-stream nil) (keep-alive nil))
  (drakma:http-request "http://localhost:8765/events/items"
                       :method :get
                       :accept "application/json"
                       :additional-headers headers
                       :want-stream want-stream
                       :keep-alive keep-alive
                       :close (not keep-alive)))

(test events--sse-connection--401--require-api-key
  (with-fixture api-start-stop (nil)
    (multiple-value-bind (body status headers)
        (make-sse-request nil)
      (declare (ignore headers))
      (is (= status 401))
      (is (equal (octets-to-string body)
                 "{\"error\":\"No API key provided\"}")))))

(test events--sse-connection--403--no-read-permission
  (with-fixture api-start-stop (nil)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '())))
      (multiple-value-bind (body status headers)
          (make-sse-request `(("X-Api-Key" . ,apikey-id)))
        (declare (ignore headers))
        (is (= status 403))
        (is (equal (octets-to-string body)
                   "{\"error\":\"Insufficient access rights\"}"))))))

(test events--sse-connection--200--ok-with-headers
  (with-fixture api-start-stop (t)
    (let ((apikey-id (apikey-store:create-apikey :access-rights '(:read))))
      (multiple-value-bind (stream status headers)
          (make-sse-request `(("X-Api-Key" . ,apikey-id))
                            :want-stream t)
        (is (= status 200))
        (close stream)
        ;; Check SSE-specific headers
        (is (equal (get-header :content-type headers)
                   "text/event-stream"))
        (is (equal (get-header :cache-control headers)
                   "no-cache"))
        (is (equal (get-header :connection headers)
                   "keep-alive"))
        ;; Check security headers
        (is (equal (get-header :x-xss-protection headers)
                   "0"))
        (Is (Equal (get-header :x-content-type-options headers)
                   "nosniff"))
        (is (equal (get-header :x-frame-options headers)
                   "DENY"))
        (is (equal (get-header :content-security-policy headers)
                   "default-src 'none'; frame-ancestors 'none'; sandbox"))))))

(test events--sse-streaming--200--client-registration-and-events-plus-heartbeats
  (with-fixture api-start-stop (t)
    (flet ((find-in-sse-data (pred sse-data)
             (find-if pred sse-data))
           (filter-heartbeat-msgs (sse-data)
             (loop :for line :in sse-data
                   :if (and (stringp line)
                            (search "\"type\":\"heartbeat\"" line))
                     :collect line)))
      (let* ((apikey-id (apikey-store:create-apikey :access-rights '(:read :update)))
             (item (hab:defitem 'test-sensor "Test Sensor" 'float :initial-value 20.0))
             (heartbeat-sleep-s eventsc:*heartbeat-sleep-time-s*)
             (max-heartbeats eventsc:*max-heartbeats*))
        (setf eventsc:*heartbeat-sleep-time-s* 0.1
              eventsc:*max-heartbeats* 2)
      
        ;; Create persistent SSE connection using Drakma with streaming
        (multiple-value-bind (stream status headers)
            (make-sse-request `(("X-Api-Key" . ,apikey-id))
                              :want-stream t
                              :keep-alive t)
          (declare (ignore headers))

          (is (= status 200))
          ;; Read SSE events for a limited time
          (let ((received-all-data)
                (sse-data nil))
            (loop :while (not received-all-data)
                  :for line := (read-line stream nil nil)
                  :do (when (> (length line) 0)
                        (push line sse-data))
                      (when (search "\"type\":\"connection\"" line)
                        ;; now update the value
                        (item:set-value item 25.5))
                      (when (= 4 (length sse-data))
                        (setf received-all-data t)
                        (when stream (close stream))))
        
            (is (find-in-sse-data (lambda (line) 
                                    (and (stringp line)
                                         (search "data:" line)
                                         (search "TEST-SENSOR" line)
                                         (search "25.5" line)))
                                  sse-data))
            (is (= 2 (length (filter-heartbeat-msgs sse-data)))))

          (setf eventsc:*heartbeat-sleep-time-s* heartbeat-sleep-s
                eventsc:*max-heartbeats* max-heartbeats))))))
