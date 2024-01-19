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
                              :address "0.0.0.0"))))

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

(define-condition parameter-validation-error (error)
  ((args :initarg :args :reader args))
  (:report (lambda (condition stream)
             (format stream "~a" (args condition)))))

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
           :args "Missing username or password"))

  (unless (ppcre:scan "(?i)[a-zA-Z][a-zA-Z0-9]{1,29}" username)
    (error 'parameter-validation-error
           :args
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
    (authc:user-not-found (c)
      (log:warn "User not found: ~a" c)
      (%make-http-error hunchentoot:+http-forbidden+
                        "User not found."))
    (authc:unable-to-authenticate (c)
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
                         "No X-Auth-Token header!")))
    (token-store:revoke-token token-id)
    "{}"))

;; -----------------------------------
;; items
;; -----------------------------------

(defun @check-authorization (next)
  (let ((auth-header (hunchentoot:header-in* "Authorization")))
    (unless auth-header
      (setf (hunchentoot:header-out "WWW-Authenticate")
            "Bearer realm=\"chipi\", error=\"no token\", error_description=\"No Authorization header\"")
      (return-from @check-authorization
        (%make-http-error hunchentoot:+http-authorization-required+ "No token!")))

    ;; parse token-id
    (let ((token-id (str:trim (second (str:split "Bearer " auth-header)))))
      (when (null token-id)
        (setf (hunchentoot:header-out "WWW-Authenticate")
              "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"No token provided\"")
        (return-from @check-authorization
          (%make-http-error hunchentoot:+http-authorization-required+ "No token!")))

      ;; read token
      (let ((token (token-store:read-token token-id)))
        (unless token
          (setf (hunchentoot:header-out "WWW-Authenticate")
                "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"The provided token is not known\"")
          (return-from @check-authorization
            (%make-http-error hunchentoot:+http-authorization-required+ "No token!")))

        ;; check expiry
        ;; TODO: delete if expired
        (when (token-store:expired-p token)
          (setf (hunchentoot:header-out "WWW-Authenticate")
                "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"Token has expired\"")
          (return-from @check-authorization
            (%make-http-error hunchentoot:+http-authorization-required+ "Expired")))          
        )))
  (funcall next))

(defroute items-get
    ("/api/items"
     :method :get
     :decorators (@json-out
                  @protection-headers-out
                  @check-authorization)) ()
  (yason:with-output-to-string* ()
    (yason:encode-alist
     `(("items" . #())))))
