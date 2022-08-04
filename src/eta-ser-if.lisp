(defpackage :cl-eta.eta-ser-if
  (:use :cl)
  (:nicknames :eta-ser-if)
  (:export #:open-serial
           #:close-serial
           #:write-serial
           #:read-serial))

(in-package :cl-eta.eta-ser-if)

;; ---------------------
;; serial facade
;; ---------------------

(defgeneric open-serial (impl device))
(defgeneric close-serial (impl port))
(defgeneric write-serial (impl port data))
(defgeneric read-serial (impl port &optional timeout))

;; ---------------------
;; serial facade -- real
;; ---------------------

(defmethod open-serial ((impl (eql :prod)) device)
  (declare (ignore impl))
  (cserial-port:open-serial device
                            :baud-rate 19200
                            :data-bits 8
                            :stop-bits 1
                            :parity :none))

(defmethod close-serial ((impl (eql :prod)) port)
  (declare (ignore impl))
  (cserial-port:close-serial port))

(defmethod write-serial ((impl (eql :prod)) port data)
  (declare (ignore impl))
  (cserial-port:write-serial-byte-vector data port))

(defmethod read-serial ((impl (eql :prod)) port &optional (timeout 2000))
  (declare (ignore impl))
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (handler-case
        (let ((nread (cserial-port:read-serial-byte-vector buf port :timeout-ms timeout)))
          (case nread
            ('() #())
            (otherwise (subseq buf 0 nread))))
      (error ()
        ;; timeout?
        #()))))
