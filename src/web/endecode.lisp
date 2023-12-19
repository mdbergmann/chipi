(defpackage :chipi-web.endecode
  (:use :cl :cl-base64)
  (:nicknames :endecode)
  (:export #:octets-to-base64-uri-string
           #:base64-uri-string-to-octets)
  )

(in-package :chipi-web.endecode)

(defun octets-to-base64-uri-string (vector)
  (usb8-array-to-base64-string vector :uri t))

(defun base64-uri-string-to-octets (string)
  (base64-string-to-usb8-array string :uri t))
