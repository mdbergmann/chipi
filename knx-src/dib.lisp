(defpackage :knc-conn.dib
  (:use :cl :knxutil :knxobj)
  (:nicknames :dib)
  (:export #:dib
           #:dib-list
           #:parse-dibs
           #:dib-device-info
           #:dib-supp-svc-families
           ))

(in-package :knc-conn.dib)

;; -----------------------------
;; knx description information block
;; -----------------------------

(defconstant +dib-typecodes-device-info+ #x01)
(defconstant +dib-typecodes-supp-svc-families+ #x02)
(defconstant +dib-typecodes-ip-config+ #x03)
(defconstant +dib-typecodes-ip-cur-config+ #x04)
(defconstant +dib-typecodes-knx-addresses+ #x05)
(defconstant +dib-typecodes-mfr-data+ #xfe)

(defstruct (dib (:include knx-obj)
                (:constructor %make-dib))
  "Device Information Block
Generic DIB structure:
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Description Type Code           |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Description Information Block data                            |
| (?? octets)                                                   |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  (len (error "Required len") :type octet)
  (type (error "Required type") :type octet))

(defstruct (dib-device-info (:include dib)
                            (:constructor %make-dib-device-info))
  "
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Description Type Code           |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| KNX medium                  | Device Status                   |
| (1 Octet)                   | (1 Octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| KNX Individual Address                                        |
| (2 Octets)                                                    |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Project-Installation identifier                               |
| (2 Octets)                                                    |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| KNXnet/IP device KNX Serial Number                            |
| (6 octets)                                                    |
+- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| KNXnet/IP device routing multicast address                    |
| (4 octets)                                                    |
+- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| KNXnet/IP device MAC address                                  |
| (6 octets)                                                    |
+- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Device Friendly Name                                          |
| (30 octets)                                                   |
+- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (knx-medium 0 :type octet)
  (device-status (number-to-bit-vector 0 8) :type (bit-vector 8))
  (knx-individual-address (error "Required knx-individual-address")
   :type (vector octet 2))
  (proj-inst-identifier (error "Required proj-inst-identifier")
   :type (vector octet 2))
  (knx-serial-number (error "Required knx-serial-number")
   :type (vector octet 6))
  (knx-routing-multicast-addr (error "Required knx-routing-multicast-addr")
   :type (vector octet 4))
  (knx-mac-addr (error "Required knx-mac-addr")
   :type (vector octet 6))
  (device-friendly-name (error "Required device-friendly-name")
   :type string))

(defconstant +knx-medium-tp1+ #x02
  "KNX medium TP1")
(defconstant +knx-medium-pl110+ #x04
  "KNX medium PL110")
(defconstant +knx-medium-rf+ #x10
  "KNX medium RF")
(defconstant +knx-medium-ip+ #x20
  "KNX medium IP")

(defun %parse-dib-device-info (len data)
  (let ((device-friendly-name (subseq data 22 52)))
    (%make-dib-device-info
     :len len
     :type +dib-typecodes-device-info+
     :knx-medium (elt data 0)
     :device-status (number-to-bit-vector (elt data 1) 8)
     :knx-individual-address (seq-to-array (subseq data 2 4) :len 2)
     :proj-inst-identifier (seq-to-array (subseq data 4 6) :len 2)
     :knx-serial-number (seq-to-array (subseq data 6 12) :len 6)
     :knx-routing-multicast-addr (seq-to-array (subseq data 12 16) :len 4)
     :knx-mac-addr (seq-to-array (subseq data 16 22) :len 6)
     :device-friendly-name (babel:octets-to-string
                               (seq-to-array
                                (subseq device-friendly-name
                                        0
                                        (or
                                         (position 0 device-friendly-name)
                                         30))
                                :type 'vector)))))

(defstruct (service-family (:include knx-obj)
                           (:constructor %make-service-family))
  "Service Family"
  (id (error "Required id") :type (unsigned-byte 8))
  (version (error "Required version") :type (unsigned-byte 8)))

(defun service-family-list-p (list)
  (and (listp list)
       (every #'service-family-p list)))

(deftype service-family-list ()
  "A list of service families."
  `(satisfies service-family-list-p))

(defstruct (dib-supp-svc-families (:include dib)
                                  (:constructor %make-dib-supp-svc-families))
  "Supported Service Families
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Description Type Code           |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Service Family ID           | Service Family version          |
| (1 Octet)                   | (1 Octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Service Family ID           | Service Family version          |
| (1 Octet)                   | (1 Octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| ....                        | ....                            |
|                             |                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Service Family ID           | Service Family version          |
| (1 Octet)                   | (1 Octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (service-families (error "Required service-families")
   :type service-family-list))

(defun %parse-dib-supp-svc-families (len data)
  (let ((service-families nil)
        (sub-data data))
    (loop
      (when (= 0 (length sub-data))
        (return))
      (let* ((id (elt sub-data 0))
             (version (elt sub-data 1))
             (service-family (%make-service-family :id id :version version)))
        (setf sub-data (subseq sub-data 2))
        (setf service-families (append service-families (list service-family)))))
    (%make-dib-supp-svc-families
     :len len
     :type +dib-typecodes-supp-svc-families+
     :service-families service-families)))

(defun dib-lisp-p (list)
  (and (listp list)
       (every #'dib-p list)))

(deftype dib-list ()
  "A list of dibs."
  `(satisfies dib-lisp-p))

(defun parse-dibs (body-data)
  "Parses a variable list of dibs from the body data and returns them."
  (let ((sub-data body-data)
        (dibs nil))
    (loop
      (when (= 0 (length sub-data))
        (return))
      (let* ((len (- (elt sub-data 0) 2))
             (typecode (elt sub-data 1))
             (end-index (+ 2 len))
             (data (subseq sub-data 2 end-index))
             (dib 
               (cond
                 ((= typecode +dib-typecodes-device-info+)
                  (%parse-dib-device-info len data))
                 ((= typecode +dib-typecodes-supp-svc-families+)
                  (%parse-dib-supp-svc-families len data))
                 (t
                  (log:warn "Unknown dib: ~a" typecode)
                  (%make-dib :len len :type typecode)))))
        (setf sub-data (subseq sub-data end-index))
        (setf dibs (append dibs (list dib)))))
    dibs))

;; TODO:
;; - IP Config DIB
;; - IP Current Config DIB
;; - KNX Addresses DIB
;; - MFR Data DIB
