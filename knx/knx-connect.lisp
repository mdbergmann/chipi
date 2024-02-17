(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload '(:usocket :try :cl-mock)))

(defpackage :chipi.knx-connect
  (:use :cl)
  (:nicknames :knxc)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:send-descr-request
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
           #:dib-list
           ))

(in-package :chipi.knx-connect)

(defvar *conn* nil)
(defun connect (address port)
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type '(unsigned-byte 8))))
    (setf *conn* conn)))

(defun disconnect ()
  (when *conn*
    (usocket:socket-close *conn*)))

(defun send-knx-request (request)
  (usocket:socket-send *conn* request (length request)))

(defun receive-knx-response ()
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

(defun %to-vec (short)
  "Converts a short integer to a vector of two bytes."
  (make-array 2
              :element-type '(unsigned-byte 8)
              :initial-contents (list (ash short -8)
                                      (logand short #xff))))

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

(defstruct (knx-package (:conc-name package-))
  (header (error "Header is required!") :type knx-header)
  body)

(defconstant +knx-descr-request+ #x0203)
(defconstant +knx-descr-response+ #x0204)

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
        (ip-port (coerce (%to-vec ip-port)
                         '(array (unsigned-byte 8) (2)))))
    (%make-hpai-internal :ip-address ip-addr :ip-port ip-port)))

(defparameter *hpai-unbound-addr*
  (%make-hpai "0.0.0.0" 0))

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
  (make-knx-descr-request
   :header (%make-header
            :type +knx-descr-request+
            :body-len (+ +knx-header-len+ (hpai-len hpai)))
   :hpai hpai
   :body hpai))

(defgeneric to-byte-seq (obj))
(defmethod to-byte-seq ((obj hpai))
  (list (hpai-len obj)
        (hpai-host-protocol-code obj)
        (coerce (hpai-ip-address obj) 'list)
        (coerce (hpai-ip-port obj) 'list)))

(defmethod to-byte-seq ((obj knx-header))
  (list (header-len obj)
        (header-knxnetip-version obj)
        (%to-list (header-type obj))
        (%to-list (header-body-len obj))))

(defmethod to-byte-seq ((obj knx-package))
  (list (to-byte-seq (package-header obj))
        (to-byte-seq (package-body obj))))

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
             (dib (%make-dib :len len :type typecode :data data)))
        (setf sub-data (subseq sub-data end-index))
        (setf dibs (append dibs (list dib)))))
    dibs))

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
  (device-hardware (error "Required device-hardware (dip)") :type dib)
  (supp-svc-families (error "Required supp svc families (dip)") :type dib)
  (other-dev-info nil :type (or null dib-list)))

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

(defun %parse-descr-response (header body)
  (let ((dibs (%parse-dibs body)))
    (%make-descr-response
     :header header
     :body body
     :device-hardware (first dibs)
     :supp-svc-families (second dibs)
     :other-dev-info (nthcdr 2 dibs))))

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

;; -----------------------------
;; high-level comm
;; -----------------------------

(defun send-descr-request ()
  (send-knx-request *raw-descr-request*)
  (let ((response (receive-knx-response)))
    (%parse-knx-package response)
  ))
