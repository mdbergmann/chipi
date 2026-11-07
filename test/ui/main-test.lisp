(defpackage :chipi-ui.main-test
  (:use :cl :fiveam))

(in-package :chipi-ui.main-test)

(def-suite ui-main-tests
  :description "Unit tests for the pure (browser-free) helpers in
chipi-ui.page-renderer."
  :in chipi-ui.tests:test-suite)

(in-suite ui-main-tests)

;; ----------------------------------------------------------------------------
;; %format-value -- renders an item's value to a display string per type-hint.
;; ----------------------------------------------------------------------------

(test %format-value--float-two-decimals
  (is (string= "3.14" (ui-renderer::%format-value 3.14159 "FLOAT")))
  (is (string= "23.50" (ui-renderer::%format-value 23.5 "FLOAT"))))

(test %format-value--integer
  (is (string= "42" (ui-renderer::%format-value 42 "INTEGER"))))

(test %format-value--string
  (is (string= "hello" (ui-renderer::%format-value "hello" "STRING")))
  ;; NIL string value renders as the empty string, not "NIL".
  (is (string= "" (ui-renderer::%format-value nil "STRING"))))

(test %format-value--default-type
  ;; Unknown type-hint: NIL -> "", any other value via ~a.
  (is (string= "" (ui-renderer::%format-value nil "WHATEVER")))
  (is (string= "on" (ui-renderer::%format-value "on" "WHATEVER"))))

;; ----------------------------------------------------------------------------
;; %format-widget-value -- widget format string wins over default formatting.
;; ----------------------------------------------------------------------------

(test %format-widget-value--custom-format
  (is (string= "23.5 °C" (ui-renderer::%format-widget-value 23.5 "FLOAT" "~,1f °C"))))

(test %format-widget-value--nil-format-falls-back
  (is (string= "23.50" (ui-renderer::%format-widget-value 23.5 "FLOAT" nil))))

(test %format-widget-value--nil-value-ignores-format
  (is (string= "" (ui-renderer::%format-widget-value nil "STRING" "~a!"))))

;; ----------------------------------------------------------------------------
;; %format-type-hint -- human-readable badge label for a type-hint.
;; ----------------------------------------------------------------------------

(test %format-type-hint--known-types
  (is (string= "Switch" (ui-renderer::%format-type-hint "BOOLEAN")))
  (is (string= "Decimal Number" (ui-renderer::%format-type-hint "FLOAT")))
  (is (string= "Whole Number" (ui-renderer::%format-type-hint "INTEGER")))
  (is (string= "Text" (ui-renderer::%format-type-hint "STRING"))))

(test %format-type-hint--unknown-type
  (is (string= "Undefined type" (ui-renderer::%format-type-hint "FOO"))))

;; ----------------------------------------------------------------------------
;; %parse-number -- input field string to item value.
;; ----------------------------------------------------------------------------

(test %parse-number--integer
  (is (= 42 (ui-renderer::%parse-number "42" "INTEGER"))))

(test %parse-number--float
  (is (= 23.5 (ui-renderer::%parse-number "23.5" "FLOAT")))
  (is (typep (ui-renderer::%parse-number "23.5" "FLOAT") 'single-float)))

(test %parse-number--garbage-returns-nil
  (is-false (ui-renderer::%parse-number "abc" "INTEGER"))
  (is-false (ui-renderer::%parse-number "abc" "FLOAT")))

;; ----------------------------------------------------------------------------
;; %selection-value -- a <select>'s option string to the item value.
;; ----------------------------------------------------------------------------

(test %selection-value--integer-item-gets-an-integer
  (let ((v (ui-renderer::%selection-value "1" "INTEGER")))
    (is (eql 1 v))))

(test %selection-value--float-item-gets-a-single-float
  (let ((v (ui-renderer::%selection-value "0.5" "FLOAT")))
    (is (= 0.5 v))
    (is (typep v 'single-float))))

(test %selection-value--string-item-keeps-the-string
  (is (string= "auto" (ui-renderer::%selection-value "auto" "STRING"))))

(test %selection-value--unparsable-number-is-nil
  (is-false (ui-renderer::%selection-value "" "INTEGER"))
  (is-false (ui-renderer::%selection-value "abc" "FLOAT")))

;; ----------------------------------------------------------------------------
;; %itemgroup-link-p -- T when the itemgroup carries the :ui-link tag.
;; ----------------------------------------------------------------------------

(defun %group-with-tag-keys (&rest keys)
  "An itemgroup hash-table whose \"tags\" is a hash containing KEYS."
  (let ((group (make-hash-table :test #'equal))
        (tags  (make-hash-table)))
    (dolist (k keys) (setf (gethash k tags) t))
    (setf (gethash "tags" group) tags)
    group))

(test %itemgroup-link-p--with-ui-link-tag
  (is-true (ui-renderer::%itemgroup-link-p (%group-with-tag-keys :ui-link))))

(test %itemgroup-link-p--without-ui-link-tag
  (is-false (ui-renderer::%itemgroup-link-p (%group-with-tag-keys :ui-other))))

(test %itemgroup-link-p--no-tags-key
  (is-false (ui-renderer::%itemgroup-link-p (make-hash-table :test #'equal))))

(test %itemgroup-link-p--tags-not-a-hash-table
  (let ((group (make-hash-table :test #'equal)))
    (setf (gethash "tags" group) "not-a-hash")
    (is-false (ui-renderer::%itemgroup-link-p group))))

;; ----------------------------------------------------------------------------
;; %ui-readonly-p -- T when the tags hash carries the :ui-readonly tag.
;; ----------------------------------------------------------------------------

(test %ui-readonly-p--with-tag
  (let ((tags (make-hash-table)))
    (setf (gethash :ui-readonly tags) t)
    (is-true (ui-renderer::%ui-readonly-p tags))))

(test %ui-readonly-p--without-tag
  (is-false (ui-renderer::%ui-readonly-p (make-hash-table))))

(test %ui-readonly-p--nil-tags
  (is-false (ui-renderer::%ui-readonly-p nil)))

;; ----------------------------------------------------------------------------
;; Item value-update registry -- callbacks are scoped per owner (connection)
;; so that one connection re-rendering (or dying) does not affect others.
;; ----------------------------------------------------------------------------

(defun %registered-funs (owner item-name)
  (let ((owners (gethash item-name ui-renderer:*item-value-form-update-funs*)))
    (when owners (gethash owner owners))))

(test value-update-registry--register-then-call-invokes-fun
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (received :unset))
    (ui-renderer:set-on-value-update
     :owner-a "item.one" (lambda (state) (setf received state)))
    (ui-renderer:call-item-value-update-fun "item.one" :new-state)
    (is (eq received :new-state))))

(test value-update-registry--call-unregistered-is-a-no-op
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal)))
    ;; No function registered for this item -> must not error.
    (finishes (ui-renderer:call-item-value-update-fun "item.missing" :x))))

(test value-update-registry--all-owners-are-called
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (calls '()))
    (ui-renderer:set-on-value-update :owner-a "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :a calls)))
    (ui-renderer:set-on-value-update :owner-b "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :b calls)))
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (= 2 (length calls)))
    (is-true (member :a calls))
    (is-true (member :b calls))))

(test value-update-registry--clear-only-drops-own-callbacks
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (calls '()))
    (ui-renderer:set-on-value-update :owner-a "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :a calls)))
    (ui-renderer:set-on-value-update :owner-b "item.one"
                                     (lambda (state) (declare (ignore state))
                                       (push :b calls)))
    ;; owner-a re-renders: only its callbacks must go away
    (ui-renderer:clear-value-update-funs :owner-a)
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (equal '(:b) calls))
    (is-false (%registered-funs :owner-a "item.one"))))

(test value-update-registry--callback-may-touch-the-registry
  ;; callbacks run outside the registry's lock: one that re-renders -- clears
  ;; and registers -- while being dispatched must not deadlock on it
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))
        (calls 0))
    (ui-renderer:set-on-value-update
     :owner-a "item.one"
     (lambda (state)
       (declare (ignore state))
       (incf calls)
       (ui-renderer:clear-value-update-funs :owner-a)
       (ui-renderer:set-on-value-update :owner-a "item.one"
                                        (lambda (state)
                                          (declare (ignore state))
                                          (incf calls 10)))))
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (= 1 calls))
    (ui-renderer:call-item-value-update-fun "item.one" :state)
    (is (= 11 calls))))

;; ----------------------------------------------------------------------------
;; chart helpers -- data conversion to uPlot input.
;; ----------------------------------------------------------------------------

(test %chart-y-value--numbers-booleans-gaps
  (is (= 23.5 (ui-renderer::%chart-y-value 23.5)))
  (is (= 1 (ui-renderer::%chart-y-value t)))
  (is (= 0 (ui-renderer::%chart-y-value nil)))
  (is (string= "null" (ui-renderer::%chart-y-value "some-string"))))

(test %js-number--plain-float-and-integer
  (is (string= "23.5" (ui-renderer::%js-number 23.5)))
  (is (string= "42" (ui-renderer::%js-number 42))))

(test %js-number--no-reader-exponent-markers
  ;; with ~a, a float whose type differs from *read-default-float-format*
  ;; (thread-dependent!) prints as "87.0f0" / "1.5d8" -- JS cannot parse
  ;; those, and a range input assigned one reverts to its midpoint
  (let ((*read-default-float-format* 'double-float))
    (is (string= "87.0" (ui-renderer::%js-number 87.0))))
  (is (string= "150000000.0" (ui-renderer::%js-number 1.5d8))))

(test %js-escape--quotes-and-backslashes
  (is (string= "it\\'s" (ui-renderer::%js-escape "it's")))
  (is (string= "a\\\\b" (ui-renderer::%js-escape "a\\b")))
  (is (string= "" (ui-renderer::%js-escape nil))))

(test %chart-points--converts-timestamps-and-values
  (let* ((universal (encode-universal-time 0 0 12 1 1 2024 0))
         (unix (local-time:timestamp-to-unix
                (local-time:universal-to-timestamp universal)))
         (points (ui-renderer::%chart-points
                  (list (persp:make-persisted-item :value 1.5 :timestamp universal)
                        (persp:make-persisted-item :value 'item:true :timestamp universal))
                  nil)))
    (is (equal (list unix unix) (first points)))
    (is (equal (list "1.5" "1") (second points)))))

(test %chart-points--skips-undefined-timestamps
  (let ((points (ui-renderer::%chart-points
                 (list (persp:make-persisted-item :value 1.0 :timestamp :undefined))
                 nil)))
    (is (null (first points)))
    (is (null (second points)))))

;; ----------------------------------------------------------------------------
;; %ssl-options -- what `ui:start's TLS files turn into for CLOG.
;; ----------------------------------------------------------------------------

(defun %touch (name)
  (let ((path (merge-pathnames name (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "pem" out))
    path))

(test %ssl-options--neither-file-means-plain-http
  (is-false (ui-main::%ssl-options nil nil)))

(test %ssl-options--both-files-turn-ssl-on
  (let ((cert (%touch "chipi-test.crt"))
        (key (%touch "chipi-test.key")))
    (unwind-protect
         (let ((options (ui-main::%ssl-options cert key)))
           (is (eq t (getf options :ssl)))
           (is (string= (namestring cert) (getf options :ssl-cert-file)))
           (is (string= (namestring key) (getf options :ssl-key-file))))
      (delete-file cert)
      (delete-file key))))

(test %ssl-options--one-file-alone-is-an-error
  (let ((cert (%touch "chipi-test.crt")))
    (unwind-protect
         (progn
           (signals error (ui-main::%ssl-options cert nil))
           (signals error (ui-main::%ssl-options nil cert)))
      (delete-file cert))))

(test %ssl-options--missing-file-is-an-error
  (let ((cert (%touch "chipi-test.crt")))
    (unwind-protect
         (progn
           (signals error (ui-main::%ssl-options cert "/nonexistent/chipi.key"))
           (signals error (ui-main::%ssl-options "/nonexistent/chipi.crt" cert)))
      (delete-file cert))))

;; ----------------------------------------------------------------------------
;; ui:start -- hands the TLS files on and marks the session cookie Secure.
;; ----------------------------------------------------------------------------

(test ui-start--tls-files-reach-start-main-and-secure-the-cookie
  (let ((ui-auth:*secure-cookie* nil))
    (cl-mock:with-mocks ()
      (cl-mock:answer ui-main:start-main nil)
      (cl-mock:answer hab:add-to-shutdown nil)
      (unwind-protect
           (progn
             (ui:start :host "0.0.0.0" :port 8443
                       :ssl-cert-file "/etc/chipi/chipi.crt"
                       :ssl-key-file "/etc/chipi/chipi.key")
             (is-true ui-auth:*secure-cookie*)
             (is (equal '("0.0.0.0" 8443
                          :ssl-cert-file "/etc/chipi/chipi.crt"
                          :ssl-key-file "/etc/chipi/chipi.key")
                        (cdr (first (cl-mock:invocations 'ui-main:start-main)))))
             (ui:start :host "0.0.0.0" :port 8080)
             (is-false ui-auth:*secure-cookie*))
        (ui-auth:configure nil)))))

(test ui-start--failed-start-resets-gate-and-secure-cookie
  (let ((ui-auth:*secure-cookie* nil))
    (cl-mock:with-mocks ()
      (cl-mock:answer ui-main:start-main (error "no such certificate"))
      (cl-mock:answer hab:add-to-shutdown nil)
      (unwind-protect
           (progn
             (signals error
               (ui:start :access-key "secret"
                         :ssl-cert-file "/etc/chipi/chipi.crt"
                         :ssl-key-file "/etc/chipi/chipi.key"))
             (is-false (ui-auth:enabled-p))
             (is-false ui-auth:*secure-cookie*))
        (ui-auth:configure nil)))))

;; ----------------------------------------------------------------------------
;; start-main -- wiring of the gate and the TLS options into CLOG.
;; ----------------------------------------------------------------------------

(test start-main--gate-comes-before-manifest-and-ssl-options-reach-clog
  (let ((cert (%touch "chipi-test.crt"))
        (key (%touch "chipi-test.key"))
        (ui-main::*item-change-listener* :dummy)
        (page:*page-registered-hook* nil))
    (unwind-protect
         (cl-mock:with-mocks ()
           (cl-mock:answer clog:initialize nil)
           (cl-mock:answer clog:set-on-new-window nil)
           (ui-main:start-main "0.0.0.0" 8443
                               :ssl-cert-file cert :ssl-key-file key)
           (let* ((args (first (cl-mock:invocations 'clog:initialize)))
                  (plist (cddr args)))
             (is (equal (list #'ui-auth:gate-middleware
                              #'ui-webapp:manifest-middleware)
                        (getf plist :lack-middleware-list)))
             (is (eq t (getf plist :ssl)))
             (is (string= (namestring cert) (getf plist :ssl-cert-file)))
             (is (string= (namestring key) (getf plist :ssl-key-file)))))
      (delete-file cert)
      (delete-file key))))
