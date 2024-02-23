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
  (usocket:socket-send *conn* request (length request)))

(defun receive-knx-data ()
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (usocket:socket-receive *conn* buf 1024)))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defun on-request-received (request)
  (typecase request
    (knx-tunnelling-request
     (progn
       (log:info "Received knx-tunnelling-request")
       (log:info "Sending tunnelling-ack...")
       (send-knx-data
        (byte-seq-to-byte-array
         (to-byte-seq (make-tunnelling-ack request))))
       (log:debug "Sent tunnelling-ack")))
    (t
     (log:info "Received unknown request: ~a" request))))


(defun receive-knx-request (&optional
                              (request-handler-fun
                               #'on-request-received))
  (let* ((data (receive-knx-data))
         (parsed-obj
           (parse-root-knx-object data)))
    (log:debug "Received obj: ~a" parsed-obj)
    (when request-handler-fun
      (funcall request-handler-fun parsed-obj))
    parsed-obj))

;; ---------------------------------

(defmacro with-request-and-response (request)
  (let ((bytes (gensym))
        (response (gensym))
        (parsed-response (gensym))
        (c (gensym)))
    `(let ((,bytes (byte-seq-to-byte-array (to-byte-seq ,request))))
       (log:debug "Sending request: ~a" ,request)
       (send-knx-data ,bytes)
       (handler-case 
           (let* ((,response (receive-knx-data))
                  (,parsed-response
                    (parse-root-knx-object ,response)))
             (log:debug "Received response: ~a" ,parsed-response)
             (values ,parsed-response nil))
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
