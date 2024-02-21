(defpackage :knx-conn.knx-connect
  (:use :cl :knxutil :knxobj :descr-info :connect :hpai)
  (:nicknames :knxc)
  (:export #:connect
           #:disconnect
           ;; send requests
           #:retrieve-descr-info
           #:connect-to-endpoint
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

(defun send-knx-request (request)
  (assert *conn* nil "No connection!")
  (usocket:socket-send *conn* request (length request)))

(defun receive-knx-response ()
  (assert *conn* nil "No connection!")
  (let ((buf (make-array 1024 :element-type 'octet)))
    (usocket:socket-receive *conn* buf 1024)))

;; -----------------------------
;; high-level comm
;; -----------------------------

(defmacro with-request (request)
  (let ((bytes (gensym))
        (response (gensym))
        (parsed-response (gensym))
        (c (gensym)))
    `(let ((,bytes (byte-seq-to-byte-array (to-byte-seq ,request))))
       (log:debug "Sending request: ~a" ,request)
       (send-knx-request ,bytes)
       (handler-case 
           (let* ((,response (receive-knx-response))
                  (,parsed-response
                    (parse-root-knx-object ,response)))
             (log:debug "Received response: ~a" ,parsed-response)
             (values ,parsed-response nil))
         (knx-error-condition (,c)
           (log:info "Condition: ~a" ,c)
           (values nil ,c))
         (error (e)
           (log:info "Error: ~a" e)
           (values nil e))))))

(defun retrieve-descr-info ()
  (with-request (make-descr-request *hpai-unbound-addr*)))
  
(defun connect-to-endpoint ()
  (with-request (make-connect-request)))
