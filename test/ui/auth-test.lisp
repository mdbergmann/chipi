(defpackage :chipi-ui.auth-test
  (:use :cl :fiveam :cl-mock))

(in-package :chipi-ui.auth-test)

(def-suite ui-auth-tests
  :description "Tests for the UI gate: the login page, the session cookie and
the backoff on failed attempts."
  :in chipi-ui.tests:test-suite)

(in-suite ui-auth-tests)

(defparameter +key+ "open-sesame")
(defparameter +now+ 3900000000)

(def-fixture auth-env (&optional (key +key+))
  (let ((ui-auth:*secure-cookie* nil)
        (ui-auth:*session-life-time* 3600)
        (ui-auth:*free-attempts* 3)
        (ui-auth:*base-delay* 2)
        (ui-auth:*max-delay* 900)
        (ui-auth:*forget-after* 3600))
    (unwind-protect
         (with-mocks ()
           (answer ui-auth::%now +now+)
           (ui-auth:configure key)
           (&body))
      (ui-auth:configure nil))))

;; ----------------------------------------------------------------------------
;; helpers
;; ----------------------------------------------------------------------------

(defun make-env (&key (method :get) (path "/") cookie (addr "10.0.0.7")
                   query form)
  "A Lack request environment, as the server hands it to the middleware.
FORM, an alist, is posted as a urlencoded body."
  (let ((headers (make-hash-table :test #'equal))
        (env (list :request-method method
                   :path-info path
                   :request-uri path
                   :query-string query
                   :remote-addr addr)))
    (when cookie
      (setf (gethash "cookie" headers) cookie))
    (when form
      (let ((octets (babel:string-to-octets
                     (format nil "~{~a=~a~^&~}"
                             (loop :for (k . v) :in form
                                   :collect (quri:url-encode k)
                                   :collect (quri:url-encode v))))))
        (setf env (append env
                          (list :content-type "application/x-www-form-urlencoded"
                                :content-length (length octets)
                                :raw-body (flexi-streams:make-in-memory-input-stream
                                           octets))))))
    (append env (list :headers headers))))

(defvar *passed* nil
  "The env the stub app behind the gate was called with, or nil.")

(defun gate ()
  "The middleware wrapping a stub app that records what reaches it."
  (setf *passed* nil)
  (ui-auth:gate-middleware (lambda (env)
                             (setf *passed* env)
                             (list 200 '(:content-type "text/plain") '("app")))))

(defun call (env)
  (funcall (gate) env))

(defun status (response) (first response))
(defun header (response name) (getf (second response) name))
(defun body (response) (apply #'concatenate 'string (third response)))

(defun login (&key (key +key+) (addr "10.0.0.7") query)
  "Posts KEY to the login page and returns the response."
  (call (make-env :method :post :path ui-auth:+login-path+ :addr addr
                  :query query :form `(("key" . ,key)))))

(defun session-cookie (response)
  "The cookie a login response set, as the browser would send it back."
  (let ((set (header response :set-cookie)))
    (when set
      (subseq set 0 (position #\; set)))))

;; ----------------------------------------------------------------------------
;; the gate
;; ----------------------------------------------------------------------------

(test gate--without-a-key-everything-passes
  (with-fixture auth-env (nil)
    (is-false (ui-auth:enabled-p))
    (is (= 200 (status (call (make-env :path "/rooms")))))
    (is-true *passed*)
    ;; the login path is just another path then
    (is (= 200 (status (call (make-env :path ui-auth:+login-path+)))))))

(test gate--without-a-session-redirects-to-login-keeping-the-path
  (with-fixture auth-env ()
    (let ((response (call (make-env :path "/rooms"))))
      (is (= 302 (status response)))
      (is (string= "/login?next=%2Frooms" (header response :location)))
      (is-false *passed*))))

(test gate--root-redirects-to-plain-login
  (with-fixture auth-env ()
    (is (string= "/login" (header (call (make-env :path "/")) :location)))))

(test gate--websocket-and-static-files-are-gated-too
  (with-fixture auth-env ()
    (is (= 302 (status (call (make-env :path "/clog")))))
    (is (= 302 (status (call (make-env :path "/js/boot.js")))))
    (is-false *passed*)))

(test gate--manifest-and-icons-are-open
  (with-fixture auth-env ()
    (is (= 200 (status (call (make-env :path "/manifest.webmanifest")))))
    (is (= 200 (status (call (make-env :path "/icons/icon-192.png")))))
    (is-true *passed*)))

(test gate--valid-session-passes
  (with-fixture auth-env ()
    (let ((cookie (session-cookie (login))))
      (is-true cookie)
      (is (= 200 (status (call (make-env :path "/rooms" :cookie cookie)))))
      (is-true *passed*))))

(test gate--session-cookie-among-others-is-found
  (with-fixture auth-env ()
    (let ((cookie (session-cookie (login))))
      (is (= 200 (status (call (make-env :path "/"
                                         :cookie (format nil "other=1; ~a; x=y"
                                                         cookie)))))))))

(test gate--expired-session-is-refused
  (with-fixture auth-env ()
    (let ((cookie (session-cookie (login))))
      (with-mocks ()
        (answer ui-auth::%now (+ +now+ ui-auth:*session-life-time* 1))
        (is (= 302 (status (call (make-env :path "/" :cookie cookie)))))
        (is-false *passed*)))))

(test gate--tampered-session-is-refused
  (with-fixture auth-env ()
    (let* ((cookie (session-cookie (login)))
           (token (subseq cookie (1+ (position #\= cookie))))
           (parts (str:split #\. token)))
      ;; a later expiry with the old signature
      (let ((forged (format nil "~a=~a.~a.~a" ui-auth:+cookie-name+
                            (+ (parse-integer (first parts)) 100000)
                            (second parts) (third parts))))
        (is (= 302 (status (call (make-env :path "/" :cookie forged))))))
      ;; garbage of the right shape, and of the wrong shape
      (is (= 302 (status (call (make-env :path "/"
                                         :cookie "chipi-session=1.2.3")))))
      (is (= 302 (status (call (make-env :path "/"
                                         :cookie "chipi-session=nonsense")))))
      (is-false *passed*))))

(test gate--session-survives-reconfigure-with-the-same-key
  (with-fixture auth-env ()
    (let ((cookie (session-cookie (login))))
      (ui-auth:configure +key+)
      (is (= 200 (status (call (make-env :path "/" :cookie cookie)))))
      (ui-auth:configure "another-key")
      (is (= 302 (status (call (make-env :path "/" :cookie cookie))))))))

;; ----------------------------------------------------------------------------
;; the login page
;; ----------------------------------------------------------------------------

(test login--get-shows-the-form
  (with-fixture auth-env ()
    (let ((response (call (make-env :path ui-auth:+login-path+
                                    :query "next=%2Frooms"))))
      (is (= 200 (status response)))
      (is (search "text/html" (header response :content-type)))
      (is (search "name=\"key\"" (body response)))
      (is (search "action=\"/login?next=%2Frooms\"" (body response)))
      (is-false *passed*))))

(test login--get-with-a-session-goes-to-next
  (with-fixture auth-env ()
    (let* ((cookie (session-cookie (login)))
           (response (call (make-env :path ui-auth:+login-path+
                                     :query "next=%2Frooms" :cookie cookie))))
      (is (= 302 (status response)))
      (is (string= "/rooms" (header response :location))))))

(test login--right-key-sets-cookie-and-redirects-to-next
  (with-fixture auth-env ()
    (let ((response (login :query "next=%2Frooms")))
      (is (= 302 (status response)))
      (is (string= "/rooms" (header response :location)))
      (let ((set (header response :set-cookie)))
        (is (str:starts-with-p "chipi-session=" set))
        (is (search "; Path=/" set))
        (is (search "; Max-Age=3600" set))
        (is (search "; HttpOnly" set))
        (is (search "; SameSite=Lax" set))
        (is-false (search "Secure" set))))))

(test login--secure-cookie-when-configured
  (with-fixture auth-env ()
    (let ((ui-auth:*secure-cookie* t))
      (is (search "; Secure" (header (login) :set-cookie))))))

(test login--without-next-goes-to-root
  (with-fixture auth-env ()
    (is (string= "/" (header (login) :location)))))

(test login--next-must-be-a-local-path
  (with-fixture auth-env ()
    (is (string= "/" (header (login :query "next=http%3A%2F%2Fevil.example") :location)))
    (is (string= "/" (header (login :query "next=%2F%2Fevil.example") :location)))
    (is (string= "/" (header (login :query "next=%2F%5Cevil.example") :location)))
    (is (string= "/" (header (login :query "next=%2Flogin") :location)))
    (is (string= "/" (header (login :query "next=%2F%09%2Fevil.example") :location)))
    (is (string= "/" (header (login :query "next=%2F%0A%2Fevil.example") :location)))
    (is (string= "/" (header (login :query "next=%2F%0D%0ASet-Cookie%3Ax%3Dy") :location)))
    (is (string= "/" (header (login :query "next=%2Fa%5Cb") :location)))))

(test login--wrong-key-says-so
  (with-fixture auth-env ()
    (let ((response (login :key "nope")))
      (is (= 401 (status response)))
      (is-false (header response :set-cookie))
      (is (search "Wrong key." (body response))))))

(test login--empty-key-is-wrong
  (with-fixture auth-env ()
    (is (= 401 (status (login :key ""))))
    (is (= 401 (status (call (make-env :method :post
                                       :path ui-auth:+login-path+)))))))

;; ----------------------------------------------------------------------------
;; backoff
;; ----------------------------------------------------------------------------

(test backoff--free-attempts-then-doubling-delay
  (with-fixture auth-env ()
    (is (= 0 (ui-auth:note-failure "a" +now+)))
    (is (= 0 (ui-auth:note-failure "a" +now+)))
    (is (= 0 (ui-auth:note-failure "a" +now+)))
    (is-false (ui-auth:blocked-for "a" +now+))
    (is (= 2 (ui-auth:note-failure "a" +now+)))
    (is (= 2 (ui-auth:blocked-for "a" +now+)))
    (is (= 4 (ui-auth:note-failure "a" +now+)))
    (is (= 8 (ui-auth:note-failure "a" +now+)))
    (is (= 16 (ui-auth:note-failure "a" +now+)))
    (is (= 16 (ui-auth:blocked-for "a" +now+)))
    (is (= 6 (ui-auth:blocked-for "a" (+ +now+ 10))))
    (is-false (ui-auth:blocked-for "a" (+ +now+ 16)))))

(test backoff--delay-is-capped
  (with-fixture auth-env ()
    (loop :repeat 20 :do (ui-auth:note-failure "a" +now+))
    (is (= 900 (ui-auth:blocked-for "a" +now+)))))

(test backoff--addresses-are-independent
  (with-fixture auth-env ()
    (loop :repeat 5 :do (ui-auth:note-failure "a" +now+))
    (is-true (ui-auth:blocked-for "a" +now+))
    (is-false (ui-auth:blocked-for "b" +now+))))

(test backoff--success-forgets-the-address
  (with-fixture auth-env ()
    (loop :repeat 5 :do (ui-auth:note-failure "a" +now+))
    (ui-auth:note-success "a")
    (is-false (ui-auth:blocked-for "a" +now+))
    (is (= 0 (ui-auth:note-failure "a" +now+)))))

(test backoff--stale-records-are-forgotten
  (with-fixture auth-env ()
    (loop :repeat 5 :do (ui-auth:note-failure "a" +now+))
    ;; another address's failure long after drops a's record
    (ui-auth:note-failure "b" (+ +now+ ui-auth:*forget-after* 1))
    (is (= 0 (ui-auth:note-failure "a" (+ +now+ ui-auth:*forget-after* 1))))))

(test backoff--reset-forgets-everything
  (with-fixture auth-env ()
    (loop :repeat 5 :do (ui-auth:note-failure "a" +now+))
    (ui-auth:reset-attempts)
    (is-false (ui-auth:blocked-for "a" +now+))))

(test login--blocked-address-is-refused-with-retry-after
  (with-fixture auth-env ()
    (loop :repeat 3 :do (is (= 401 (status (login :key "nope")))))
    ;; the fourth failure starts the wait ...
    (is (= 401 (status (login :key "nope"))))
    ;; ... during which even the right key is not looked at
    (let ((response (login)))
      (is (= 429 (status response)))
      (is (string= "2" (header response :retry-after)))
      (is-false (header response :set-cookie))
      (is (search "Try again in 2 seconds." (body response))))
    ;; once it is over the right key gets in and clears the record
    (with-mocks ()
      (answer ui-auth::%now (+ +now+ 2))
      (is (= 302 (status (login))))
      (is-false (ui-auth:blocked-for "10.0.0.7" (+ +now+ 2))))))

(test login--blocked-attempt-does-not-extend-the-wait
  (with-fixture auth-env ()
    (loop :repeat 4 :do (login :key "nope"))
    (is (= 2 (ui-auth:blocked-for "10.0.0.7" +now+)))
    (login :key "nope")
    (is (= 2 (ui-auth:blocked-for "10.0.0.7" +now+)))))

(test login--other-address-is-not-blocked
  (with-fixture auth-env ()
    (loop :repeat 4 :do (login :key "nope" :addr "10.0.0.7"))
    (is (= 302 (status (login :addr "10.0.0.8"))))))

(test configure--forgets-failed-attempts
  (with-fixture auth-env ()
    (loop :repeat 5 :do (ui-auth:note-failure "a" +now+))
    (ui-auth:configure +key+)
    (is-false (ui-auth:blocked-for "a" +now+))))

(test configure--empty-key-disables
  (with-fixture auth-env ()
    (is-false (ui-auth:configure ""))
    (is-false (ui-auth:enabled-p))))
