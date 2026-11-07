(defpackage :chipi-ui.auth
  (:use :cl)
  (:nicknames :ui-auth)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:+login-path+
           #:+cookie-name+
           #:*access-key*
           #:*session-life-time*
           #:*secure-cookie*
           #:*free-attempts*
           #:*base-delay*
           #:*max-delay*
           #:*forget-after*
           #:configure
           #:enabled-p
           #:gate-middleware
           #:blocked-for
           #:note-failure
           #:note-success
           #:reset-attempts))

(in-package :chipi-ui.auth)

;;; Access control for the UI.
;;;
;;; Everything the browser fetches -- the boot page, the websocket, the static
;;; files -- passes through CLOG's Lack app, so one middleware in front of it
;;; gates the whole UI: a request without a valid session cookie is sent to a
;;; plain login page that is served here and never reaches CLOG.  The login
;;; form takes the access key configured on the server; the right key answers
;;; with a signed session cookie that the browser then carries on every
;;; request, the websocket handshake included.  Installed as a web app the UI
;;; keeps its cookie jar, so a wall tablet logs in once.
;;;
;;; The cookie holds a token, not the key: an expiry and a random id, signed
;;; with HMAC.  The signing key is derived from the access key, so sessions
;;; survive a server restart and changing the key logs every device out.
;;;
;;; Failed logins are throttled per client address: a few attempts are free,
;;; after that each further failure doubles the time the address has to wait
;;; before the next attempt is even looked at.  A blocked attempt is refused
;;; at once (429) rather than delayed -- sleeping in the handler would tie up
;;; a server thread per attempt, which is a denial of service of its own.

(defparameter +login-path+ "/login"
  "URL path of the login page; the form posts to it as well.")

(defparameter +cookie-name+ "chipi-session"
  "Name of the session cookie.")

(defvar *access-key* nil
  "The key the login form asks for, as a string, or `nil' when the UI is open
to everyone.  Set through `ui:start's `:access-key' -- use `configure'.")

(defvar *sign-key* nil
  "The HMAC key sessions are signed with: derived from `*access-key*' by
`configure'.")

(defvar *session-life-time* (* 30 24 60 60)
  "Seconds a session stays valid once logged in.")

(defvar *secure-cookie* nil
  "Whether the session cookie carries the Secure flag, which makes the browser
send it over https only.  Set this when the UI is served over TLS.")

(defparameter +open-paths+ '("/manifest.webmanifest")
  "Paths served without a session.")

(defparameter +open-prefixes+ '("/icons/")
  "Path prefixes served without a session: the browser fetches the manifest
and its icons without credentials, and neither is secret.")

(defun %now ()
  "The current universal time -- a function so tests can mock the clock."
  (get-universal-time))

;; ---------------------------------------------------------------------------
;; configuration
;; ---------------------------------------------------------------------------

(defun configure (access-key)
  "Enables the gate with ACCESS-KEY, a string, or disables it with `nil' (or
an empty string).  Forgets all failed attempts.  Returns whether the gate is
enabled."
  (setf *access-key* (and (stringp access-key)
                          (plusp (length access-key))
                          access-key))
  (setf *sign-key* (when *access-key*
                     ;; HMAC as a key derivation: a 32-byte key that is a
                     ;; function of the access key alone
                     (cryp:hmac-sign (endecode:string-to-octets "chipi-ui-session")
                                     *access-key*
                                     :base64-out nil)))
  (reset-attempts)
  (enabled-p))

(defun enabled-p ()
  "Whether the UI asks for a key at all."
  (and *access-key* t))

(defun %key-matches-p (entered)
  "Whether ENTERED, as posted by the login form, is the access key.  Compared
in constant time."
  (and (enabled-p)
       (stringp entered)
       (cryp:equal-string-p entered *access-key*)))

;; ---------------------------------------------------------------------------
;; session tokens
;; ---------------------------------------------------------------------------

(defun %make-token (now)
  "A fresh session token: \"<expiry>.<random id>.<signature>\"."
  (let* ((payload (format nil "~a.~a"
                          (+ now *session-life-time*)
                          (cryp:make-random-string 16 :uri t)))
         (sig (cryp:hmac-sign *sign-key* payload)))
    (format nil "~a.~a" payload sig)))

(defun %token-valid-p (token now)
  "Whether TOKEN is one of ours, untampered and not yet expired at NOW."
  (when (and *sign-key* (stringp token))
    (let ((parts (str:split #\. token)))
      (when (= 3 (length parts))
        (destructuring-bind (expiry id sig) parts
          (let ((expected (cryp:hmac-sign *sign-key* (format nil "~a.~a" expiry id)))
                (expires-at (ignore-errors (parse-integer expiry))))
            (and (cryp:equal-string-p sig expected)
                 (integerp expires-at)
                 (> expires-at now))))))))

(defun %session-cookie (token)
  "The Set-Cookie header value carrying TOKEN."
  (format nil "~a=~a; Path=/; Max-Age=~a; HttpOnly; SameSite=Lax~:[~;; Secure~]"
          +cookie-name+ token *session-life-time* *secure-cookie*))

(defun %cookie-value (env name)
  "The value of cookie NAME in the request ENV, or `nil'."
  (let ((headers (getf env :headers)))
    (when-let ((header (and (hash-table-p headers)
                            (gethash "cookie" headers))))
      (let ((prefix (format nil "~a=" name)))
        (loop :for part :in (str:split #\; header)
              :for trimmed := (string-trim " " part)
              :when (str:starts-with-p prefix trimmed)
                :return (subseq trimmed (length prefix)))))))

(defun %authenticated-p (env &optional (now (%now)))
  "Whether the request ENV carries a valid session."
  (%token-valid-p (%cookie-value env +cookie-name+) now))

;; ---------------------------------------------------------------------------
;; backoff
;; ---------------------------------------------------------------------------

(defvar *free-attempts* 3
  "Failed attempts an address may make before it is made to wait.")

(defvar *base-delay* 2
  "Seconds the first wait lasts; each further failure doubles it.")

(defvar *max-delay* 900
  "The longest wait, in seconds.")

(defvar *forget-after* 3600
  "Seconds after its last failure an address's record is dropped.")

(defstruct (attempts (:constructor %make-attempts))
  (failures 0)
  (not-before 0)
  (last 0))

(defvar *attempts* (make-hash-table :test #'equal)
  "Client address -> `attempts'.  Guarded by `*attempts-lock*'.")

(defvar *attempts-lock* (bt2:make-lock :name "chipi-ui-auth"))

(defun %delay-for (failures)
  "Seconds the FAILURES-th failure in a row locks an address for: none within
the free attempts, then doubling from `*base-delay*' up to `*max-delay*'."
  (let ((over (- failures *free-attempts*)))
    (if (<= over 0)
        0
        (min *max-delay* (* *base-delay* (expt 2 (1- over)))))))

(defun %forget-stale (now)
  "Drops records whose last failure is older than `*forget-after*'.  Call with
the lock held."
  (loop :for addr :being :the :hash-keys :of *attempts* :using (:hash-value a)
        :when (< (+ (attempts-last a) *forget-after*) now)
          :collect addr :into stale
        :finally (dolist (addr stale) (remhash addr *attempts*))))

(defun blocked-for (addr &optional (now (%now)))
  "Seconds ADDR still has to wait before its next attempt, or `nil' when it
may try now."
  (bt2:with-lock-held (*attempts-lock*)
    (when-let ((a (gethash addr *attempts*)))
      (let ((remaining (- (attempts-not-before a) now)))
        (when (plusp remaining)
          remaining)))))

(defun note-failure (addr &optional (now (%now)))
  "Records a failed attempt from ADDR.  Returns the seconds ADDR is blocked
for from now on, 0 while it is within its free attempts."
  (bt2:with-lock-held (*attempts-lock*)
    (%forget-stale now)
    (let* ((a (or (gethash addr *attempts*)
                  (setf (gethash addr *attempts*) (%make-attempts))))
           (delay (%delay-for (incf (attempts-failures a)))))
      (setf (attempts-last a) now
            (attempts-not-before a) (+ now delay))
      delay)))

(defun note-success (addr)
  "Forgets ADDR's failed attempts."
  (bt2:with-lock-held (*attempts-lock*)
    (remhash addr *attempts*)))

(defun reset-attempts ()
  "Forgets every address's failed attempts."
  (bt2:with-lock-held (*attempts-lock*)
    (clrhash *attempts*)))

;; ---------------------------------------------------------------------------
;; responses
;; ---------------------------------------------------------------------------

(defun %safe-next (next)
  "NEXT as a path to send the browser to after login: a local absolute path,
else \"/\".  Keeps a redirect from pointing off the site."
  (if (and (stringp next)
           (str:starts-with-p "/" next)
           (not (str:starts-with-p "//" next))
           ;; browsers drop tab, CR and LF inside a URL, so "/<tab>/host" would
           ;; become "//host"; control characters and backslashes never belong
           ;; in a local path (and CR/LF would split the Location header)
           (notany (lambda (c)
                     (or (char= c #\\)
                         (< (char-code c) 32)
                         (= (char-code c) 127)))
                   next)
           (not (string= +login-path+ next)))
      next
      "/"))

(defun %login-url (next)
  "The login page's URL, carrying NEXT when it is not the root."
  (if (string= "/" next)
      +login-path+
      (format nil "~a?next=~a" +login-path+ (quri:url-encode next))))

(defun %escape-html (string)
  (with-output-to-string (out)
    (loop :for c :across string
          :do (case c
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (#\& (write-string "&amp;" out))
                (#\" (write-string "&quot;" out))
                (t (write-char c out))))))

(defparameter +login-page-template+
  "<!doctype html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">
<meta name=\"theme-color\" content=\"#0d6efd\">
<link rel=\"icon\" type=\"image/png\" href=\"/icons/icon-192.png\">
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"/icons/icon-180.png\">
<title>Chipi</title>
<style>
body { margin: 0; min-height: 100vh; display: flex; align-items: center; justify-content: center;
       font-family: system-ui, -apple-system, sans-serif; background: #f8f9fa; color: #212529; }
main { width: min(22rem, 90vw); padding: 2rem; background: #fff; border-radius: .75rem;
       box-shadow: 0 .25rem 1rem rgba(0,0,0,.08); }
h1 { margin: 0 0 .5rem; font-size: 1.5rem; }
p { margin: 0 0 1rem; color: #6c757d; }
form { display: flex; gap: .5rem; }
input { flex: 1; min-width: 0; padding: .5rem .75rem; font-size: 1rem; border: 1px solid #ced4da; border-radius: .375rem; }
button { padding: .5rem 1rem; font-size: 1rem; border: 0; border-radius: .375rem; background: #0d6efd; color: #fff; }
.message { margin: 1rem 0 0; color: #dc3545; min-height: 1.5rem; }
</style>
</head>
<body>
<main>
<h1>Chipi</h1>
<p>Enter the access key to open the dashboard.</p>
<form method=\"post\" action=\"~a\">
<input type=\"password\" name=\"key\" placeholder=\"Access key\" aria-label=\"Access key\" autocomplete=\"current-password\" autofocus required>
<button type=\"submit\">Open</button>
</form>
<p class=\"message\">~a</p>
</main>
</body>
</html>
"
  "The login page.  Arguments: the form's action URL and the message line.")

(defun %login-page-response (next &key (status 200) (message "") retry-after)
  "A response showing the login page, with MESSAGE in its message line."
  (list status
        (append (list :content-type "text/html; charset=utf-8"
                      :cache-control "no-store")
                (when retry-after
                  (list :retry-after (princ-to-string retry-after))))
        (list (format nil +login-page-template+
                      (%escape-html (%login-url next))
                      (%escape-html message)))))

(defun %redirect (location &key set-cookie)
  "A response sending the browser to LOCATION, setting a cookie when given."
  (list 302
        (append (list :location location
                      :cache-control "no-store")
                (when set-cookie
                  (list :set-cookie set-cookie)))
        (list "")))

;; ---------------------------------------------------------------------------
;; the gate
;; ---------------------------------------------------------------------------

(defun %client-address (env)
  "What the backoff keys on: the peer's address.  Behind a reverse proxy this
is the proxy, which then shares one record; forwarding headers are not
consulted, as a client could set them itself."
  (or (getf env :remote-addr) "unknown"))

(defun %query-parameter (req name)
  (cdr (assoc name (lack.request:request-query-parameters req) :test #'string=)))

(defun %body-parameter (req name)
  (cdr (assoc name (lack.request:request-body-parameters req) :test #'string=)))

(defun %try-login (env req next)
  "Handles the posted login form."
  (let ((addr (%client-address env))
        (now (%now)))
    (if-let ((wait (blocked-for addr now)))
      (progn
        (log:warn "UI login from ~a refused, blocked for another ~a s" addr wait)
        (%login-page-response
         next
         :status 429
         :retry-after wait
         :message (format nil "Too many attempts. Try again in ~a second~:p." wait)))
      (cond
        ((%key-matches-p (%body-parameter req "key"))
         (note-success addr)
         (log:info "UI login from ~a" addr)
         (%redirect next :set-cookie (%session-cookie (%make-token now))))
        (t
         (let ((delay (note-failure addr now)))
           (log:warn "UI login from ~a failed~@[, blocked for ~a s~]"
                     addr (and (plusp delay) delay))
           (%login-page-response next :status 401 :message "Wrong key.")))))))

(defun %handle-login (env)
  "Serves the login page (GET) and takes the posted key (POST)."
  (let* ((req (lack.request:make-request env))
         (next (%safe-next (%query-parameter req "next"))))
    (case (getf env :request-method)
      (:post (%try-login env req next))
      (t (if (%authenticated-p env)
             (%redirect next)
             (%login-page-response next))))))

(defun %open-path-p (path)
  (or (member path +open-paths+ :test #'string=)
      (some (lambda (prefix) (str:starts-with-p prefix path)) +open-prefixes+)))

(defun gate-middleware (app)
  "Lack middleware that lets a request through to APP only with a valid
session.  Without one it serves the login page at `+login-path+' and sends
every other path there, remembering the path to come back to.  Does nothing
when no access key is configured."
  (lambda (env)
    (let ((path (getf env :path-info)))
      (cond
        ((not (enabled-p)) (funcall app env))
        ((string= +login-path+ path) (%handle-login env))
        ((%open-path-p path) (funcall app env))
        ((%authenticated-p env) (funcall app env))
        (t (%redirect (%login-url (%safe-next path))))))))
