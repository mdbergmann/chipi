(defpackage :knx-conn.knx-obj
  (:use :cl :knxutil)
  (:nicknames :knxobj)
  (:export #:knx-obj
           ;; header
           #:knx-header
           #:make-header
           #:header-len
           #:header-knxnetip-version
           #:header-type
           #:header-body-len
           ;; package
           #:knx-package
           #:package-header
           #:package-body
           #:parse-root-knx-object
           ;; generic
           #:to-byte-seq
           #:parse-to-obj
           ;; types
           #:octet
           ))

(in-package :knx-conn.knx-obj)

(deftype octet () '(unsigned-byte 8))

(defgeneric to-byte-seq (knx-obj))
(defgeneric parse-to-obj (obj-type header-data body-data))

(defstruct (knx-obj (:constructor nil))
  "Root knx object")

;; -----------------------------
;; knx header
;; -----------------------------

(defconstant +knx-header-len+ #x06)
(defconstant +knx-netip-version+ #x10)

(defstruct (knx-header (:include knx-obj)
                       (:constructor %make-header)
                       (:conc-name header-))
    "KNXnet/IP header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HEADER_SIZE_10              | KNXNETIP_VERSION                |
| (06h)                       | (10h)                           |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| DESCRIPTION_RESPONSE                                          |
| (0204h)                                                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HEADER_SIZE_10 + sizeof(body)                                 |
|                                                               |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (len +knx-header-len+)
  (knxnetip-version +knx-netip-version+)
  type
  body-len)

(defun make-header (type body-len)
  (%make-header :type type
                :body-len (+ +knx-header-len+ body-len)))

(defun %parse-header (pkg-data)
  (let ((header-size (elt pkg-data 0))
        (knx-version (elt pkg-data 1))
        (type (to-int (elt pkg-data 2)
                       (elt pkg-data 3)))
        (body-size (to-int (elt pkg-data 4)
                            (elt pkg-data 5))))
    (let ((eff-body-size (- body-size +knx-header-len+)))
      (%make-header
       :len header-size
       :knxnetip-version knx-version
       :type type
       :body-len eff-body-size))))

(defmethod to-byte-seq ((obj knx-header))
  (list (header-len obj)
        (header-knxnetip-version obj)
        (int-to-byte-list (header-type obj))
        (int-to-byte-list (header-body-len obj))))

;; -----------------------------
;; knx generic package
;; -----------------------------

(defstruct (knx-package (:include knx-obj)
                        (:conc-name package-))
  (header (error "Header is required!") :type knx-header)
  body)

(defun parse-root-knx-object (pkg-data)
  "Root object parse function.
PKG-DATA: package data, array of bytes representing the package.
PARSE-TYPE-FUN: function to parse the package body based on the specific object type.
Returns the parsed object."
  (let* ((header (%parse-header pkg-data))
         (header-len (header-len header))
         (body (subseq pkg-data
                       header-len
                       (+ (header-body-len header)
                          header-len)))
         (type (header-type header)))
    (parse-to-obj type header body)))

(defmethod to-byte-seq ((obj knx-package))
  (list (to-byte-seq (package-header obj))
        (to-byte-seq (package-body obj))))

(defmethod to-byte-seq ((obj list))
  "Converts a list of objects to a byte sequence."
  (apply #'append (mapcar #'to-byte-seq obj)))
