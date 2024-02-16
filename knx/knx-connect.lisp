(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload '(:usocket :bit-smasher :try :cl-mock)))

(defpackage :chipi.knx-connect
  (:use :cl)
  (:nicknames :knxc)
  (:export #:connect-to-knx
           #:disconnect-from-knx
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
           #:dib
           ))

(in-package :chipi.knx-connect)

(defvar *conn* nil)
(defun connect-to-knx (address port)
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type '(unsigned-byte 8))))
    (setf *conn* conn)))

(defun disconnect-from-knx ()
  (when *conn*
    (usocket:socket-close *conn*)))

(defun send-knx-request (request)
  (usocket:socket-send *conn* request (length request)))

(defun receive-knx-response ()
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (usocket:socket-receive *conn* buf 1024)))

;; ------------------------------------------

(defun %make-pkg (pkg-data-lst)
  (let ((pkg (alexandria:flatten pkg-data-lst)))
    (make-array (list (length pkg))
                :initial-contents pkg
                :element-type '(unsigned-byte 8))))

(defun %negative-p (byte)
  (> (mask-field (byte 8 7) byte) 0))

(defun %to-int (upper lower)
  "On negatve values the number is sent as two complement."
  (let ((neg-p (%negative-p upper)))
    (if neg-p
        (let* ((number (logior (ash upper 8) lower))
               (bit-vector (bitsmash:bits<- number))
               (complement (bit-not bit-vector)))
          (* -1 (bitsmash:int<- complement)))
        (+ (ash upper 8) lower))))

(defparameter *raw-descr-request*
  (%make-pkg '(#x06 #x10
               #x02 #x03
               #x00 #x0e
               ;; HPAI
               #x08
               #x01               ;; udp
               #x00 #x00 #x00 #x00 ;; unbound address
               #x00 #x00
               )))

(defconstant +knx-header-len+ #x06)
(defconstant +knx-netip-version+ #x10)

(defconstant +knx-descr-request+ #x0203)
(defconstant +knx-descr-response+ #x0204)

(defstruct (knx-header (:constructor %make-header)
                       (:conc-name header-))
  (len +knx-header-len+)
  (knxnetip-version +knx-netip-version+)
  type
  body-len)

(defstruct (knx-package (:conc-name package-))
  (header nil :type (or null knx-header))
  body)

(defconstant +dib-typecodes-device-info+ #x01)
(defconstant +dib-typecodes-supp-svc-families+ #x02)
(defconstant +dib-typecodes-ip-config+ #x03)
(defconstant +dib-typecodes-ip-cur-config+ #x04)
(defconstant +dib-typecodes-knx-addresses+ #x05)
(defconstant +dib-typecodes-mfr-data+ #xfe)

(defstruct (dib (:constructor %make-dib))
  len type data)

(defun %parse-dibs (body-data)
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
  (device-hardware (error "Required device-hardware (dip)") :type dib)
  (supp-svc-families (error "Required supp svc families (dip)") :type dib)
  (other-dev-info nil))

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
