(defpackage :cl-eta.eta-ser-if
  (:use :cl :libserialport)
  (:nicknames :eta-ser-if)
  (:export #:open-serial
           #:write-serial
           #:read-serial))

(in-package :cl-eta.eta-ser-if)

;; ---------------------
;; serial facade
;; ---------------------

(defgeneric open-serial (impl device))
(defgeneric write-serial (impl port data))
(defgeneric read-serial (impl port &optional timeout))

;; ---------------------
;; serial facade -- real
;; ---------------------

(defmethod open-serial ((impl (eql :prod)) device)
  (declare (ignore impl))
  (libserialport:open-serial-port device
                                  :baud 19200
                                  :bits 8
                                  :stopbits 1
                                  :parity :sp-parity-none
                                  :rts :sp-rts-off
                                  :flowcontrol :sp-flowcontrol-none))
(defmethod write-serial ((impl (eql :prod)) port data)
  (declare (ignore impl))
  (libserialport:serial-write-data port data))

(defmethod read-serial ((impl (eql :prod)) port &optional (timeout 2000))
  (declare (ignore impl))
  (libserialport:serial-read-octets-until
   port
   #\}
   :timeout timeout))
