(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :tunnelling :hpai)
  (:nicknames :knxc)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:retrieve-descr-info
           #:connect-to-endpoint
           ;; receive data
           #:receive-knx-request
           ))

(in-package :knx-conn.knx-connect)

(defparameter *knx-if* "192.168.50.41")

(defvar *conn* nil)
(defun connect (address &optional (port 3761))
  (let ((conn (usocket:socket-connect
               address port
               :protocol :datagram
               :element-type 'octet)))
    (log:info "Connected to ~a on port ~a" address port)
    (setf *conn* conn)))

(defun disconnect ()
  (assert *conn* nil "No connection!")
  (usocket:socket-close *conn*))

(defun send-knx-data (request)
  (assert *conn* nil "No connection!")
  (let ((req-bytes (byte-seq-to-byte-array
                    (to-byte-seq request))))
    (usocket:socket-send *conn* req-bytes (length req-bytes))))

(defun receive-knx-data ()
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (parse-root-knx-object
     (usocket:socket-receive *conn* buf 1024))))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defun on-request-received (request)
  (multiple-value-bind (req err) request
    (when err
      (log:info "Error: ~a" err)
      (return-from on-request-received))
    (typecase req
      (knx-tunnelling-request
       (progn
         (log:info "Received knx-tunnelling-request")
         (log:info "Sending tunnelling-ack...")
         (send-knx-data (make-tunnelling-ack request))
         (log:debug "Sent tunnelling-ack")))
      (t
       (log:info "Received unknown request: ~a" request)))))


(defun receive-knx-request (&optional
                              (request-handler-fun
                               #'on-request-received))
  (let ((request (receive-knx-data)))
    (log:debug "Received obj: ~a" request)
    (when request-handler-fun
      (funcall request-handler-fun request))
    request))

;; ---------------------------------

(defmacro with-request-and-response (request)
  (let ((response (gensym))
        (c (gensym)))
    `(progn
       (log:debug "Sending request: ~a" ,request)
       (send-knx-data ,request)
       (handler-case 
           (let ((,response (receive-knx-data)))
             (log:debug "Received response: ~a" ,response)
             (values ,response nil))
         (condition (,c)
           (log:info "Condition: ~a" ,c)
           (values nil ,c))
         (error (e)
           (log:info "Error: ~a" e)
           (values nil e))))))

(defun retrieve-descr-info ()
  (with-request-and-response (make-descr-request *hpai-unbound-addr*)))
  
(defun connect-to-endpoint ()
  (with-request-and-response (make-connect-request)))
