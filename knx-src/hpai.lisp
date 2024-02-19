(defpackage :knx-conn.hpai
  (:use :cl :knxutil :knxobj)
  (:nicknames :hpai)
  (:export #:hpai
           #:make-hpai
           #:hpai-len
           #:*hpai-unbound-addr*
           ))

(in-package :knx-conn.hpai)

(defconstant +hpai-udp+ #x01
  "Host Protocol Address Information (HPAI) UDP")

(defstruct (hpai (:include knx-obj)
                 (:constructor %make-hpai))
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

(defun make-hpai (ip-address ip-port)
  "Creates a HPAI structure from the given ip-address and ip-port.
The ip-address is a string in the form of \"192.168.1.1\".
The ip-port is an integer between 0 and 65535."
  (check-type ip-address string)
  (check-type ip-port (integer 0 65535))
  (let ((ip-addr (coerce
                  (mapcar #'parse-integer
                          (uiop:split-string ip-address :separator "."))
                  '(array (unsigned-byte 8) (4))))
        (ip-port (int-to-byte-vec ip-port)))
    (%make-hpai :ip-address ip-addr :ip-port ip-port)))

(defparameter *hpai-unbound-addr*
  (make-hpai "0.0.0.0" 0))

(defmethod to-byte-seq ((obj hpai))
  (list (hpai-len obj)
        (hpai-host-protocol-code obj)
        (coerce (hpai-ip-address obj) 'list)
        (coerce (hpai-ip-port obj) 'list)))
