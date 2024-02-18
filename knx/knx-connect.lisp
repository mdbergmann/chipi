(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload '(:usocket :babel :try :cl-mock :log4cl)))

(defpackage :chipi.knx-connect
  (:use :cl)
  (:nicknames :knxc)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:retrieve-descr-info
           ;; knx header
           #:knx-header
           #:header-len
           #:header-type
           #:header-knxnetip-version
           #:header-body-len
           ;; knx package
           #:knx-package
           #:package-header
           #:package-body
           ;; descr-response
           #:knx-descr-response
           #:descr-response-device-hardware
           #:descr-response-supp-svc-families
           #:descr-response-other-dev-info
           ;; types
           #:dib
           #:dib-device-info
           #:dib-supp-svc-families
           #:dib-list
           ))

(in-package :chipi.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *conn* nil)
(defun connect (address &optional (port 3761))
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type '(unsigned-byte 8))))
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun disconnect ()
  (assert *conn* nil "No connection to disconnect from!")
  (usocket:socket-close *conn*))

(defun send-knx-request (request)
  (assert *conn* nil "No connection to disconnect from!")
  (usocket:socket-send *conn* request (length request)))

(defun receive-knx-response ()
  (assert *conn* nil "No connection to disconnect from!")
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (usocket:socket-receive *conn* buf 1024)))

;; ------------------------------------------

(defun %byte-seq-to-byte-array (byte-seq)
  (let ((pkg (alexandria:flatten byte-seq)))
    (make-array (list (length pkg))
                :initial-contents pkg
                :element-type '(unsigned-byte 8))))

(defun %to-int (upper lower)
  (+ (ash upper 8) lower))

(defun %int-to-byte-vec (int &optional (len 2))
  "Converts an integer to a simple-array (unsigned-int 8)."
  (let ((byte-list (%int-to-byte-list int len)))
    (make-array (length byte-list)
                :element-type '(unsigned-byte 8)
                :initial-contents byte-list)))

(defun %int-to-byte-list (int &optional (len 2))
  "Converts an integer to a list of bytes."
  (reverse
   (loop :for i :from 0 :below len
         :collect (logand (ash int (* i -8)) #xff))))

(defun %to-array (obj &key (len 0 len-present-p) (type 'simple-array))
  (if len-present-p
      (coerce obj `(,type (unsigned-byte 8) (,len)))
      (coerce obj `(,type (unsigned-byte 8)))))

(defun %to-bit-vector (num bit-len)
  (coerce (reverse
           (loop :for i :from 0 :below bit-len
                 :collect (logand (ash num i) 1)))
          `(vector bit ,bit-len)))

;; -----------------------------
;; knx generics
;; -----------------------------

(defconstant +knx-descr-request+ #x0203)
(defconstant +knx-descr-response+ #x0204)

(defgeneric to-byte-seq (obj))

;; -----------------------------
;; knx header
;; -----------------------------

(defconstant +knx-header-len+ #x06)
(defconstant +knx-netip-version+ #x10)

(defstruct (knx-header (:constructor %make-header)
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

(defun %parse-header (pkg-data)
  (let ((header-size (elt pkg-data 0))
        (knx-version (elt pkg-data 1))
        (type (%to-int (elt pkg-data 2)
                       (elt pkg-data 3)))
        (body-size (%to-int (elt pkg-data 4)
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
        (%int-to-byte-list (header-type obj))
        (%int-to-byte-list (header-body-len obj))))

;; -----------------------------
;; knx generic package
;; -----------------------------

(defstruct (knx-package (:conc-name package-))
  (header (error "Header is required!") :type knx-header)
  body)

(defun %parse-knx-package (pkg-data)
  (let* ((header (%parse-header pkg-data))
         (header-len (header-len header))
         (body (subseq pkg-data
                       header-len
                       (+ (header-body-len header)
                          header-len)))
         (type (header-type header)))
    (cond
      ((= type +knx-descr-response+)
       (%parse-descr-response header body)))))

(defmethod to-byte-seq ((obj knx-package))
  (list (to-byte-seq (package-header obj))
        (to-byte-seq (package-body obj))))

;; -----------------------------
;; knx HPAI
;; -----------------------------

(defconstant +hpai-udp+ #x01
  "Host Protocol Address Information (HPAI) UDP")

(defstruct (hpai (:constructor %make-hpai-internal))
  "
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length              | Host Protocol Code            |
| (1 octet = 08h)               | (1 octet)                     |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
|                                                               |
| IP Address                                                    |
| (4 octets)                                                    |
|                                                               |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| IP Port Number                                                |
| (2 Octets)                                                    |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (len #x08 :type (unsigned-byte 8))
  (host-protocol-code +hpai-udp+ :type (unsigned-byte 8))
  (ip-address (error "Required ip-address") :type (array (unsigned-byte 8) (4)))
  (ip-port (error "Required ip-port") :type (array (unsigned-byte 8) (2))))

(defun %make-hpai (ip-address ip-port)
  "Creates a HPAI structure from the given ip-address and ip-port.
The ip-address is a string in the form of \"192.168.1.1\".
The ip-port is an integer between 0 and 65535."
  (check-type ip-address string)
  (check-type ip-port (integer 0 65535))
  (let ((ip-addr (coerce
                  (mapcar #'parse-integer
                          (uiop:split-string ip-address :separator "."))
                  '(array (unsigned-byte 8) (4))))
        (ip-port (%int-to-byte-vec ip-port)))
    (%make-hpai-internal :ip-address ip-addr :ip-port ip-port)))

(defparameter *hpai-unbound-addr*
  (%make-hpai "0.0.0.0" 0))

(defmethod to-byte-seq ((obj hpai))
  (list (hpai-len obj)
        (hpai-host-protocol-code obj)
        (coerce (hpai-ip-address obj) 'list)
        (coerce (hpai-ip-port obj) 'list)))

;; -----------------------------
;; knx description information block
;; -----------------------------

(defconstant +dib-typecodes-device-info+ #x01)
(defconstant +dib-typecodes-supp-svc-families+ #x02)
(defconstant +dib-typecodes-ip-config+ #x03)
(defconstant +dib-typecodes-ip-cur-config+ #x04)
(defconstant +dib-typecodes-knx-addresses+ #x05)
(defconstant +dib-typecodes-mfr-data+ #xfe)

(defstruct (dib (:constructor %make-dib))
  "Device Information Block
Generic DIB structure:
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Description Type Code           |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Description Information Block data                            |
| (?? octets)                                                   |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  len type data)

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
  (knx-medium 0 :type (unsigned-byte 8))
  (device-status (%to-bit-vector 0 8) :type (vector bit 8))
  (knx-individual-address (error "Required knx-individual-address")
   :type (array (unsigned-byte 8) (2)))
  (proj-inst-identifier (error "Required proj-inst-identifier")
   :type (array (unsigned-byte 8) (2)))
  (knx-serial-number (error "Required knx-serial-number")
   :type (array (unsigned-byte 8) (6)))
  (knx-routing-multicast-addr (error "Required knx-routing-multicast-addr")
   :type (array (unsigned-byte 8) (4)))
  (knx-mac-addr (error "Required knx-mac-addr")
   :type (array (unsigned-byte 8) (6)))
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
     :data data
     :knx-medium (elt data 0)
     :device-status (%to-bit-vector (elt data 1) 8)
     :knx-individual-address (%to-array (subseq data 2 4) :len 2)
     :proj-inst-identifier (%to-array (subseq data 4 6) :len 2)
     :knx-serial-number (%to-array (subseq data 6 12) :len 6)
     :knx-routing-multicast-addr (%to-array (subseq data 12 16) :len 4)
     :knx-mac-addr (%to-array (subseq data 16 22) :len 6)
     :device-friendly-name (babel:octets-to-string
                               (%to-array
                                (subseq device-friendly-name
                                        0
                                        (or
                                         (position 0 device-friendly-name)
                                         30))
                                :type 'vector)))))

(defstruct (service-family (:constructor %make-service-family))
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
     :data data
     :service-families service-families)))

(defun dib-lisp-p (list)
  (and (listp list)
       (every #'dib-p list)))

(deftype dib-list ()
  "A list of dibs."
  `(satisfies dib-lisp-p))

(defun %parse-dibs (body-data)
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
                  (%make-dib :len len :type typecode :data data)))))
        (setf sub-data (subseq sub-data end-index))
        (setf dibs (append dibs (list dib)))))
    dibs))

;; TODO:
;; - IP Config DIB
;; - IP Current Config DIB
;; - KNX Addresses DIB
;; - MFR Data DIB

;; -----------------------------
;; knx description request
;; -----------------------------

(defstruct (knx-descr-request (:include knx-package)
                              (:constructor %make-descr-request-internal)
                              (:conc-name descr-request-))
  "KNXnet/IP header (see above)
KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  hpai)

(defun %make-descr-request (hpai)
  (%make-descr-request-internal
   :header (%make-header
            :type +knx-descr-request+
            :body-len (+ +knx-header-len+ (hpai-len hpai)))
   :hpai hpai
   :body hpai))

;; -----------------------------
;; knx description response
;; -----------------------------

(defstruct (knx-descr-response (:include knx-package)
                               (:constructor %make-descr-response)
                               (:conc-name descr-response-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| DIB                                                           |
| device hardware                                               |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| DIB                                                           |
| supported service families                                    |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| DIB                                                           |
| other device information (optional)                           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"  
  (device-hardware (error "Required device-hardware (dip)") :type dib-device-info)
  (supp-svc-families (error "Required supp svc families (dip)") :type dib)
  (other-deb-info nil :type (or null dib-list)))

(defun %parse-descr-response (header body)
  (let ((dibs (%parse-dibs body)))
    (%make-descr-response
     :header header
     :body body
     :device-hardware (first dibs)
     :supp-svc-families (second dibs)
     :other-deb-info (nthcdr 2 dibs))))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defun retrieve-descr-info ()
  (let* ((request (%make-descr-request *hpai-unbound-addr*))
         (bytes (%byte-seq-to-byte-array (to-byte-seq request))))
    (log:debug "Sending request: ~a" request)
    (send-knx-request bytes)
    (let* ((response (receive-knx-response))
           (parsed-response (%parse-knx-package response)))
      (log:debug "Received response: ~a" parsed-response)
      parsed-response)))
