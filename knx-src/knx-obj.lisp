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
           ;; conditions
           #:knx-error-condition
           #:knx-unable-to-parse
           ))

(in-package :knx-conn.knx-obj)

;; -----------------------------
;; types
;; -----------------------------

(deftype octet () '(unsigned-byte 8))

;; -----------------------------
;; conditions
;; -----------------------------

(define-condition knx-error-condition (simple-condition) ()
  (:report (lambda (condition stream)
             (format stream "Error condition: ~a, args: ~a"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

(define-condition knx-unable-to-parse (knx-error-condition) ())

;; -----------------------------
;; generic functions
;; -----------------------------

(defgeneric to-byte-seq (knx-obj)
  (:documentation "Converts the object to the byte sequence.
The byte-sequence should be a flat vector of octets."))
(defgeneric parse-to-obj (obj-type header-data body-data))


;; -----------------------------

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
  (len +knx-header-len+ :type octet)
  (knxnetip-version +knx-netip-version+ :type octet)
  (type (error "Type is required!") :type integer)
  (body-len (error "Body length is required!") :type integer))

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
  (concatenate 'vector
               (vector (header-len obj)
                       (header-knxnetip-version obj))
               (int-to-byte-vec (header-type obj))
               (int-to-byte-vec (header-body-len obj))))

;; -----------------------------
;; knx generic package
;; -----------------------------

(defstruct (knx-package (:include knx-obj)
                        (:conc-name package-))
  (header (error "Header is required!") :type knx-header)
  ;; body is handled in the separate sub-structs
  )

(defun parse-root-knx-object (pkg-data)
  "Root object parse function.
`PKG-DATA`: package data, array of bytes representing the package.
`PARSE-TYPE-FUN`: function to parse the package body based on the specific object type.
Returns the parsed object."
  (let* ((header (%parse-header pkg-data))
         (header-len (header-len header))
         (body (subseq pkg-data
                       header-len
                       (+ (header-body-len header)
                          header-len)))
         (type (header-type header)))
    (handler-case
        (parse-to-obj type header body)
      (error (e)
        (log:warn "Unable to parse the package: ~a" e)
        (error 'knx-unable-to-parse
               :format-control "Unable to parse data. Message type: "
               :format-arguments (list type))))))

(defmethod to-byte-seq ((obj knx-package))
  (to-byte-seq (package-header obj)))
