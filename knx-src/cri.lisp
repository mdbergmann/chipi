(defpackage :knx-conn.cri
  (:use :cl :knxobj)
  (:nicknames :cri)
  (:export #:cri
           #:cri-len
           #:tunneling-cri
           #:make-tunneling-cri
           ))

(in-package :knx-conn.cri)

(defconstant +structure-len+ #x04)

;; Connection Type Code
(defconstant +tunnel-connection+ #x04)

;; KNX Layer Type
(defconstant +tunnel-link-layer+ #x02)
(defconstant +tunnel-raw+ #x04)
(defconstant +tunnel-busmonitor+ #x80)


(defstruct (cri (:include knx-obj)
                (:constructor %make-cri))
  "Connection Request Info

+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length            | Connection Type Code            |
| (1 Octet)                   | (1 Octet)                       |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Host Protocol Independent Data                                |
| (variable length, optional)                                   |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
|                                                               |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
|                                                               |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Host Protocol Dependent Data                                  |
| (variable length, optional)                                   |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
|                                                               |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+"
  (len (error "len is a required field") :type octet)
  (conn-type (error "connection type is a required field") :type octet))


(defstruct (tunneling-cri (:include cri)
                          (:constructor %make-tunneling-cri)
                          (:conc-name cri-))
  "Tunneling Connection Request Info

 * +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
 * | Structure Length              | TUNNEL_CONNECTION             |
 * | (1 octet = 04h)               | (1 octet = 04h)               |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
 * | KNX Layer Type                | reserved                      |
 * | (1 octet)                     | (1 octet)                     |
 * +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (knx-layer-type (error "knx-layer-type is a required field") :type octet))

(defmethod to-byte-seq ((cri tunneling-cri))
  (vector (cri-len cri)
          (cri-conn-type cri)
          (cri-knx-layer-type cri)
          0))

(defun make-tunneling-cri ()
  (%make-tunneling-cri :len +structure-len+
                       :conn-type +tunnel-connection+
                       :knx-layer-type +tunnel-link-layer+))
