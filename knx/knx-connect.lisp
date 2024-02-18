(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload '(:usocket :try :cl-mock :log4cl)))

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
  (device-hardware (error "Required device-hardware (dip)") :type dib)
  (supp-svc-families (error "Required supp svc families (dip)") :type dib)
  (other-dev-info nil :type (or null dib-list)))

(defun %parse-descr-response (header body)
  (let ((dibs (%parse-dibs body)))
    (%make-descr-response
     :header header
     :body body
     :device-hardware (first dibs)
     :supp-svc-families (second dibs)
     :other-dev-info (nthcdr 2 dibs))))

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
