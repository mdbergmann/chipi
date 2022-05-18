(defpackage :cl-eta.eta-ser-if
  (:use :cl :libserialport)
  (:nicknames :eta-ser-if)
  (:export #:serial-proxy
           #:make-real-serial-proxy
           #:open-serial
           #:write-serial
           #:read-serial))

(in-package :cl-eta.eta-ser-if)

;; ---------------------
;; serial facade
;; ---------------------

(defclass serial-proxy () ())
(defgeneric open-serial (serial-proxy device))
(defgeneric write-serial (serial-proxy port data))
(defgeneric read-serial (serial-proxy port))

;; ---------------------
;; serial facade -- real
;; ---------------------

(defun make-real-serial-proxy ()
  (make-instance 'real-serial-proxy))

(defclass real-serial-proxy (serial-proxy) ())
(defmethod open-serial ((proxy real-serial-proxy) device)
  (declare (ignore proxy))
  (libserialport:open-serial-port device
                                  :baud 19200
                                  :bits 8
                                  :stopbits 1
                                  :parity :sp-parity-none
                                  :rts :sp-rts-off
                                  :flowcontrol :sp-flowcontrol-none))
(defmethod write-serial ((proxy real-serial-proxy) port data)
  (declare (ignore proxy))
  (libserialport:serial-write-data port data))

(defmethod read-serial ((proxy real-serial-proxy) port)
  (declare (ignore proxy))
  (libserialport:serial-read-octets-until
   port
   #\}
   :timeout 2000))
