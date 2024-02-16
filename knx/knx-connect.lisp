(eval-when (:load-toplevel :compile-toplevel)
  (ql:quickload '(:usocket :bit-smasher :try :cl-mock)))

(defpackage :chipi.knx-connect
  (:use :cl)
  (:nicknames :knxc)
  (:export #:connect-to-knx
           #:disconnect-from-knx
           #:send-descr-request
           #:knx-header
           #:knx-descr-response
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

(defstruct (knx-header (:constructor %make-knx-header)
                       (:conc-name header-))
  (header-size +knx-header-len+)
  (knxnetip-version +knx-netip-version+)
  type
  payload-len)

(defstruct knx-package
  knx-header)

(defstruct (knx-descr-response (:include knx-package)
                               (:constructor %make-knx-descr-response)))

(defun %parse-knx-package (pkg-data)
  (let ((header-size (elt pkg-data 0))
        (knx-version (elt pkg-data 1))
        (type (%to-int (elt pkg-data 2)
                       (elt pkg-data 3)))
        (payload-size (%to-int (elt pkg-data 4)
                               (elt pkg-data 5))))
    (let ((header (%make-knx-header
                   :header-size header-size
                   :knxnetip-version knx-version
                   :type type
                   :payload-len payload-size)))
      (cond
        ((= type +knx-descr-response+)
         (%make-knx-descr-response
          :knx-header header))))))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defun send-descr-request ()
  (send-knx-request *raw-descr-request*)
  (let ((response (receive-knx-response)))
    (%parse-knx-package response)
  ))
