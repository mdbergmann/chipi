(defpackage :chipi-web.api
  (:use :cl :easy-routes)
  (:nicknames :api)
  (:export #:start
           #:stop)
  )

(in-package :chipi-web.api)

(defvar *api* nil "The API server instance")

(defun start ()
  (setf *api* (hunchentoot:start
               (make-instance 'easy-routes-ssl-acceptor ;; 'easy-routes-acceptor
                              :ssl-privatekey-file "../../cert/localhost.key"
                              :ssl-certificate-file "../../cert/localhost.crt"
                              :port 8443
                              :address "127.0.0.1"))))

(defun stop ()
  (when *api*
    (hunchentoot:stop *api*)))

;; -----------------------------------
;; helpers
;; -----------------------------------

(defun %alist-to-json (alist)
  (yason:with-output-to-string* ()
    (yason:encode-alist alist)))

(defun %make-http-error (status-code message)
  (http-error status-code
              (%alist-to-json `(("error" . ,message)))))

(defun %make-json-response (alist)
  (%alist-to-json alist))

(defun %make-token-response (token)
  (%make-json-response `(("token" . ,token))))

;; -----------------------------------
;; conditions
;; -----------------------------------

(define-condition parameter-validation-error (simple-error)
  ((failed-args :initarg :failed-args
                :reader failed-args))
  (:report (lambda (condition stream)
             (format stream "Parameter validation failed: ~a"
                     (failed-args condition)))))

;; -----------------------------------
;; decorators
;; -----------------------------------

(defun @json-out (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @protection-headers-out (next)
  (setf (hunchentoot:header-out "X-XSS-Protection")
        "0"
        (hunchentoot:header-out "X-Content-Type-Options")
        "nosniff"
        (hunchentoot:header-out "X-Frame-Options")
        "DENY"
        (hunchentoot:header-out "Cache-Control")
        "no-store"
        (hunchentoot:header-out "Content-Security-Policy")
        "default-src 'none'; frame-ancestors 'none'; sandbox")
  (funcall next))

(defun %verify-auth-parameters (username password)
  (when (or (null username)
            (null password))
    (error 'parameter-validation-error
           :failed-args "Missing username or password"))

  (unless (ppcre:scan "(?i)[a-zA-Z][a-zA-Z0-9]{1,29}" username)
    (error 'parameter-validation-error
           :failed-args
           "Invalid username. Must be 2-30 characters with only alpha numeric and number characters.")))

(defroute session-create
    ("/api/session"
     :method :post
     :decorators (@json-out
                  @protection-headers-out))
    (&post (username :parameter-type 'string)
           (password :parameter-type 'string))
  (handler-case
      (progn
        (%verify-auth-parameters username password)
        (%make-token-response (authc:authorize-user username password)))
    (parameter-validation-error (c)
      (log:warn "Parameter validation error: ~a" c)
      (%make-http-error hunchentoot:+http-forbidden+
                        (format nil "~a" c)))
    (authc:user-not-found-error (c)
      (log:warn "User not found: ~a" c)
      (%make-http-error hunchentoot:+http-forbidden+
                        "User not found."))
    (authc:unable-to-authenticate-error (c)
      (log:warn "Unable to authenticate: ~a" c)
      (%make-http-error hunchentoot:+http-forbidden+
                        "Unable to authenticate."))
    ))

(defroute session-close
    ("/api/session"
     :method :delete
     :decorators (@json-out
                  @protection-headers-out)) ()
  (let ((token-id (hunchentoot:header-in* "X-Auth-Token")))
    (unless token-id
      (return-from session-close
        (%make-http-error hunchentoot:+http-bad-request+
                         "No X-Auth-Token header")))
    (token-store:revoke-token token-id)
    "{}"))

;; -----------------------------------
;; items
;; -----------------------------------

(defun @check-authorization (next)
  (flet ((error-response (err-message err-descr)
           (setf (hunchentoot:header-out "WWW-Authenticate")
                 (format nil
                         "Bearer realm=\"chipi\", error=\"~a\", error_description=\"~a\""
                         err-message
                         err-descr))
           (return-from @check-authorization
             (%make-http-error hunchentoot:+http-authorization-required+
                               err-descr))))
    ;; auth header
    (let ((auth-header (hunchentoot:header-in* "Authorization")))
      (unless auth-header
        (error-response "no token" "No Authorization header"))

      ;; parse token-id
      (let ((token-id (str:trim (second (str:split "Bearer " auth-header)))))
        (unless token-id
          (error-response "invalid token" "No token provided"))
        
        ;; check on token
        (handler-case
            (authc:verify-authorization token-id)
          (authc:token-unknown-error (c)
            (log:info "~a" c)
            (error-response "invalid token" "Unknown token"))
          (authc:token-expired-error (c)
            (log:info "~a" c)
            (error-response "invalid token" "Token has expired"))))))
  (funcall next))

(defroute items-get
    ("/api/items"
     :method :get
     :decorators (@json-out
                  @protection-headers-out
                  @check-authorization)) ()
  (%make-json-response `(("items" . #()))))
