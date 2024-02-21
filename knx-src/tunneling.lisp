(defpackage :knx-conn.tunnelling
  (:use :cl :knxobj :cemi)
  (:nicknames :tunnelling)
  (:export #:knx-tunnelling-request
           ))

(in-package :knx-conn.tunnelling)

(defconstant +knx-tunnelling-request+ #x0420)

(defconstant +structure-len+ #x04)

(defstruct (connection-header (:include knx-obj)
                              (:conc-name conn-header-)
                              (:constructor %make-connection-header))
  "Connection header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Communication Channel ID        |
| (1 octet)                   | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Sequence Counter            | reserved                        |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  (len +structure-len+ :type octet)
  (channel-id (error "Required channel.id!") :type octet)
  (seq-counter (error "Required sequence-counter!") :type octet)
  (reserved 0 :type octet))

(defun %parse-conn-header (data)
  (%make-connection-header
   :channel-id (elt data 1)
   :seq-counter (elt data 2)
   :reserved (elt data 3)))

(defstruct (knx-tunnelling-request (:include knx-package)
                                   (:conc-name tunnelling-request-)
                                   (:constructor %make-tunnelling-request))
  "KNXnet/IP header (see above)

KNXnet/IP body
Connection header
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Communication Channel ID        |
| (1 octet)                   | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Sequence Counter            | reserved                        |
| (1 octet)                   | (1 octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
cEMI frame
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Message Code                | Additional Info Length          |
| (1 octet = 08h)             | (1 octet)                       |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Additional Information                                        |
| (optional, variable length)                                   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| Service Information                                           |
| (variable length)                                             |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (conn-header (error "Required conn-header!") :type connection-header)
  (cemi (error "Required cemi!") :type cemi))

(defmethod parse-to-obj ((obj-type (eql +knx-tunnelling-request+)) header body)
  (let ((conn-header (%parse-conn-header
                      (subseq body 0 +structure-len+))))
    (%make-tunnelling-request
     :header header
     :body body
     :conn-header conn-header
     :cemi (parse-cemi (subseq body +structure-len+)))))
