(defpackage :chipi-web.cryp
  (:use :cl :endecode)
  (:nicknames :cryp)
  (:export #:create-hash
           #:create-random-data
           #:create-random-string
           #:equal-p
           #:equal-string-p)
  )

(in-package :chipi-web.cryp)

(defun create-hash (data &optional (salt nil))
  "Generate a scrypt hash from `DATA' and an optional `SALT'.
`DATA' can be a string or a vector of unsigned bytes.
If `SALT' is not provided, a random salt is generated which makes the result a one way hash.
If `SALD' is provided, it must be a string or a vector of unsigned bytes.
The result is a base64url encoded string.
N = 4096, r = 8, and p = 2 are used as parameters for scrypt."
  (check-type data (or string (simple-array (unsigned-byte 8) (*))))
  (when salt (check-type salt (or string (simple-array (unsigned-byte 8) (*)))))

  (let ((salt (cond
                ((null salt) (crypto:random-data 16))
                ((stringp salt) (babel:string-to-octets salt))
                (t salt)))
        (data (if (stringp data)
                  (babel:string-to-octets data)
                  data)))
    (let* ((kdf (crypto:make-kdf :scrypt-kdf
                                 :n 4096
                                 :r 8
                                 :r 2))
           (pw-digest (crypto:derive-key kdf
                                         data
                                         salt
                                         nil ; ignored on scrypt
                                         32)))
      (octets-to-base64-uri-string pw-digest))))

(defun create-random-data (length)
  "Generate a random array of `LENGTH' bytes."
  (check-type length integer)
  (crypto:random-data length))

(defun create-random-string (length &key (uri nil))
  "Generate a random base64 encoded string with a length of the underlying random array of `LENGTH' bytes.
If `URI' is true, the result is a base64url encoded string."
  (check-type length integer)
  (base64:usb8-array-to-base64-string
   (crypto:random-data length) :uri uri))

(defun equal-vector-p (a b)
  "Compare two simple arrays in constant time."
  (check-type a vector)
  (check-type b vector)
  (crypto:constant-time-equal a b))

(defun equal-string-p (a b)
  "Compare two strings."
  (check-type a string)
  (check-type b string)
  (equal-vector-p (babel:string-to-octets a)
                  (babel:string-to-octets b)))
