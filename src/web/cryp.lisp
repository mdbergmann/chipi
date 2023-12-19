(defpackage :chipi-web.cryp
  (:use :cl :endecode)
  (:nicknames :cryp)
  (:export #:scrypt-data
           #:equal-p
           #:equal-string-p)
  )

(in-package :chipi-web.cryp)

(defun scrypt-data (data salt)
  "Generate a scrypt hash from `DATA' and `SALT'
Both parameters must be unsigned byte vectors.
The result is a base64url encoded string.
N = 4096, r = 8, and p = 2 are used as parameters for scrypt."
  (check-type data (simple-array (unsigned-byte 8) (*)))
  (check-type salt (simple-array (unsigned-byte 8) (*)))
  (let* ((kdf (crypto:make-kdf :scrypt-kdf
                               :n 4096
                               :r 8
                               :r 2))
         (pw-digest (crypto:derive-key kdf
                                       data
                                       salt
                                       nil ; ignored on scrypt
                                       32)))
    (octets-to-base64-uri-string pw-digest)))

(defun equal-p (a b)
  "Compare two simple arrays in constant time."
  (crypto:constant-time-equal a b))

(defun equal-string-p (a b)
  "Compare two base64 encoded strings."
  (check-type a string)
  (check-type b string)
  (equal-p (babel:string-to-octets a)
           (babel:string-to-octets b)))
