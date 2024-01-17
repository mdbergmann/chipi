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

(defvar *scrypt-salt* (babel:string-to-octets "my-awefully-secure-salt"))
(defvar *admin-user* nil "this should be elsewhere, i.e. database")
(defun generate-initial-admin-user (password)
  (let ((username "admin"))
    (let ((pw-base64 (cryp:scrypt-data
                      (babel:string-to-octets password)
                      *scrypt-salt*)))
      (setf *admin-user* (cons username pw-base64)))))

(defun make-http-error (status-code message)
  (http-error status-code
              (yason:with-output-to-string* ()
                (yason:encode-alist `(("error" . ,message))))))

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

(defun %authenticate-user (username password)
  (if (equal username "admin")
      (let ((existing-pw (cdr *admin-user*)))
        (unless (cryp:equal-string-p existing-pw
                                     (cryp:scrypt-data
                                      (babel:string-to-octets password)
                                      *scrypt-salt*))
          (make-http-error hunchentoot:+http-authorization-required+
                           "Unable to authenticate!")))
      (make-http-error hunchentoot:+http-not-implemented+
                       "Other user authentication not implemented yet.")))

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

(defun authenticate-post (username password)
  (flet ((verify-auth-params ()
           (let ((verify-params-result
                   (%verify-auth-parameters username password)))
             (when verify-params-result
               (return-from authenticate-post verify-params-result))))
         (auth-user ()
           (let ((auth-result
                   (%authenticate-user username password)))
             (when auth-result
               (return-from authenticate-post auth-result))))
         (generate-token ()
           (yason:with-output-to-string* ()
             (yason:encode-alist
              `(("token" . ,(token-store:create-token username)))))))             
    (verify-auth-params)
    (auth-user)
    (generate-token)))

(defroute session-create
    ("/api/authenticate"
     :method :post
     :decorators (@json-out
                  @protection-headers-out))
    (&post (username :parameter-type 'string)
           (password :parameter-type 'string))
  (authenticate-post username password))

(defroute session-close
    ("/api/authenticate"
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
        (when (> (get-universal-time) (token-store:expiry token))
          (setf (hunchentoot:header-out "WWW-Authenticate")
                "Bearer realm=\"chipi\", error=\"invalid token\", error_description=\"Token has expired\"")
          (return-from @check-authorization
            (make-http-error hunchentoot:+http-authorization-required+ "Expired")))          
        )))
  (funcall next))

(defroute items
    ("/api/items"
     :method :get
     :decorators (@json-out
                  @protection-headers-out
                  @check-authorization)) ()
  (yason:with-output-to-string* ()
    (yason:encode-alist
     `(("items" . #())))))
