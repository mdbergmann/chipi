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

(defun %alist-to-json (alist)
  (yason:with-output-to-string* ()
    (yason:encode-alist alist)))

(defun make-http-error (status-code message)
  (http-error status-code
              (%alist-to-json `(("error" . ,message)))))

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

;; TODO: verify in auth-controller
(defun %verify-auth-parameters (username password)
  (when (or (null username)
            (null password))
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Missing username or password")))
  (unless (ppcre:scan "(?i)[a-zA-Z][a-zA-Z0-9]{1,29}" username)
    (return-from %verify-auth-parameters
      (make-http-error hunchentoot:+http-forbidden+
                       "Invalid username. Must be 2-30 characters with only alpha numeric and number characters."))))

(defun authorize-user (username password)
  (flet ((verify-auth-params ()
           (let ((verify-params-result
                   (%verify-auth-parameters username password)))
             (when verify-params-result
               (return-from authorize-user verify-params-result)))))
    (verify-auth-params)
    (handler-case
        (let ((token-id (authc:authorize-user username password)))
          (%alist-to-json `(("token" . ,token-id))))
      (authc:user-not-found ()
        (make-http-error hunchentoot:+http-forbidden+
                         "User not found."))
      (authc:unable-to-authenticate ()
        (make-http-error hunchentoot:+http-forbidden+
                         "Unable to authenticate."))
      ;; TODO: other errors
      )))

(defroute session-create
    ("/api/session"
     :method :post
     :decorators (@json-out
                  @protection-headers-out))
    (&post (username :parameter-type 'string)
           (password :parameter-type 'string))
  (authorize-user username password))

(defroute session-close
    ("/api/session"
     :method :delete
     :decorators (@json-out
                  @protection-headers-out)) ()
  (let ((token-id (hunchentoot:header-in* "X-Auth-Token")))
    (unless token-id
      (return-from session-close
        (make-http-error hunchentoot:+http-bad-request+
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
        (make-http-error hunchentoot:+http-authorization-required+ "No token!")))

    ;; parse token-id
    (let ((token-id (str:trim (second (str:split "Bearer " auth-header)))))
      (when (null token-id)
        (setf (hunchentoot:header-out "WWW-Authenticate")
              "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"No token provided\"")
        (return-from @check-authorization
          (make-http-error hunchentoot:+http-authorization-required+ "No token!")))

      ;; read token
      (let ((token (token-store:read-token token-id)))
        (unless token
          (setf (hunchentoot:header-out "WWW-Authenticate")
                "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"The provided token is not known\"")
          (return-from @check-authorization
            (make-http-error hunchentoot:+http-authorization-required+ "No token!")))

        ;; check expiry
        ;; TODO: delete if expired
        (when (token-store:expired-p token)
          (setf (hunchentoot:header-out "WWW-Authenticate")
                "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"Token has expired\"")
          (return-from @check-authorization
            (make-http-error hunchentoot:+http-authorization-required+ "Expired")))          
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
