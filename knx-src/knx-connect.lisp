(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :tunnelling :hpai :cemi)
  (:nicknames :knxc)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:retrieve-descr-info
           #:establish-tunnel-connection
           #:send-write-request
           #:send-read-request
           ;; receive data
           #:receive-knx-request
           ))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *conn* nil)
(defun connect (address &optional (port 3761))
  "Connect to the KNXnet/IP gateway at the given `address` and `port`."
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (assert conn nil "Could not connect to ~a on port ~a" address port)
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun disconnect ()
  "Disconnect from the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (usocket:socket-close *conn*))

(defun send-knx-data (request)
  "Send the given `request` to the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (log:debug "Sending obj: ~a" request)
  (let ((req-bytes (to-byte-seq request)))
    (log:debug "Sending bytes: ~a" req-bytes)
    (usocket:socket-send *conn* req-bytes (length req-bytes))))

(defun receive-knx-data ()
  "Receive a KNXnet/IP request from the KNXnet/IP gateway."
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (handler-case 
        (let ((received-obj
                (parse-root-knx-object
                 (usocket:socket-receive *conn* buf 1024))))
          (log:debug "Received obj: ~a" received-obj)
          (values received-obj nil))
      (condition (c)
        (log:info "Condition: ~a" c)
        (values nil c))
      (error (e)
        (log:info "Error: ~a" e)
        (values nil e)))))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defmacro %with-request-response (request)
  `(progn
     (send-knx-data ,request)
     (receive-knx-data)))

(defun retrieve-descr-info ()
  (%with-request-response (make-descr-request *hpai-unbound-addr*)))
  
(defun establish-tunnel-connection ()
  (%with-request-response (make-connect-request)))

;; ---------------------------------

(defun send-write-request (group-address dpt)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Write to the given `address:knx-group-address` with the given data point type to be set."
  (send-knx-data
   (make-tunnelling-request
    :channel-id 0
    :seq-counter 0
    :cemi (make-default-cemi
           :message-code +cemi-mc-l_data.req+
           :dest-address group-address
           :apci (make-apci-gv-write)
           :dpt dpt))))

(defun send-read-request (group-address)
  "Send a tunnelling-request as L-Data.Req with APCI Group-Value-Read to the given `address:knx-group-address`. The response to this request will be received asynchronously."
  (send-knx-data
   (make-tunnelling-request
    :channel-id 0
    :seq-counter 0
    :cemi (make-default-cemi
           :message-code +cemi-mc-l_data.req+
           :dest-address group-address
           :apci (make-apci-gv-read)
           :dpt nil))))
