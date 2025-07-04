(defpackage :chipi-api.cryp
  (:use :cl :endecode)
  (:nicknames :cryp)
  (:export #:make-hash
           #:make-salt
           #:make-random-data
           #:make-random-string
           #:equal-p
           #:equal-string-p
           #:hmac-sign)
  )

(in-package :chipi-api.cryp)

(defun make-salt ()
  "Generate a random 16 byte salt."
  (crypto:random-data 16))

(defun make-hash (data &optional (salt nil))
  "Generate a bcrypt hash from `DATA' and an optional `SALT'.
`DATA' can be a string or a vector of unsigned bytes.
If `SALT' is not provided, a random salt is generated which makes the result a one way hash.
If `SALT' is provided, it must be a vector of unsigned bytes with length 16. Use `make-salt' to generate a salt.
The result is a base64url encoded string.
N = 4096, r = 8, and p = 2 are used as parameters for bcrypt."
  (check-type data (or string (simple-array (unsigned-byte 8) (*))))
  (when salt (check-type salt (simple-array (unsigned-byte 8) (*))))

  (let ((salt (cond
                ((null salt) (make-salt))
                (t salt)))
        (data (cond
                ((stringp data) (string-to-octets data))
                (t data))))
    (let* ((kdf (crypto:make-kdf :bcrypt
                                 :n 4096
                                 :r 8
                                 :r 2))
           (pw-digest (crypto:derive-key kdf
                                         data
                                         salt
                                         ; ignored on scrypt
                                         #+:lispworks 128
                                         #+:sbcl 2048
                                         #+:abcl 64
                                         24)))
      (octets-to-base64-string pw-digest t))))

(defun make-random-data (length)
  "Generate a random array of `LENGTH' bytes."
  (check-type length integer)
  (crypto:random-data length))

(defun make-random-string (length &key (uri nil))
  "Generate a random base64 encoded string with a length of the underlying random array of `LENGTH' bytes.
If `URI' is true, the result is a base64url encoded string."
  (check-type length integer)
  (octets-to-base64-string
   (crypto:random-data length) uri))

(defun equal-vector-p (a b)
  "Compare two arrays in constant time."
  (check-type a vector)
  (check-type b vector)
  (crypto:constant-time-equal a b))

(defun equal-string-p (a b)
  "Compare two strings."
  (check-type a string)
  (check-type b string)
  (equal-vector-p (string-to-octets a)
                  (string-to-octets b)))

(defun hmac-sign (key data &key (base64-out t) (uri t))
  "Sign `DATA' with `KEY' using HMAC-SHA256."
  (check-type key (simple-array (unsigned-byte 8) (*)))
  (check-type data (or string (simple-array (unsigned-byte 8) (*))))
  (let* ((mac (crypto:make-hmac key :sha256))
         (vec-data (cond
                    ((stringp data) (string-to-octets data))
                    (t data)))
         (updated-mac (crypto:update-hmac mac vec-data))
         (digest (crypto:hmac-digest updated-mac)))
    (cond
      ((not base64-out) digest)
      (t (octets-to-base64-string digest uri)))))
