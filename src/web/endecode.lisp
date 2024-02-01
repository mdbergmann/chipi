(defpackage :chipi-web.endecode
  (:use :cl :cl-base64)
  (:nicknames :endecode)
  (:export #:octets-to-base64-string
           #:base64-string-to-octets
           #:octets-to-string
           #:string-to-octets)
  )

(in-package :chipi-web.endecode)

;; we want '=' as padding character for URI compatibility
(setf base64::*uri-pad-char* #\=)

(defun octets-to-base64-string (vector &optional (urienc nil))
  (usb8-array-to-base64-string vector :uri urienc))

(defun base64-string-to-octets (string &optional (urienc nil))
  (base64-string-to-usb8-array string :uri urienc))

(defun octets-to-string (vector)
  (babel:octets-to-string vector))

(defun string-to-octets (string)
  (babel:string-to-octets string))
