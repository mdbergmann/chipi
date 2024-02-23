(defpackage :knx-conn.crd
  (:use :cl :knxutil :knxobj :address)
  (:nicknames :crd)
  (:export #:crd
           #:make-crd
           #:crd-individual-address
           #:parse-crd
           ))

(in-package :knx-conn.crd)

(defconstant +structure-len+ #x04)

(defstruct (crd (:include knx-obj)
                (:constructor %make-crd)
                (:conc-name crd-))
  "CRD (Connection Request Data)

+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Structure Length              | TUNNEL_CONNECTION             |
| (1 octet = 04h)               | (1 octet = 04h)               |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| KNX Individual Address                                        |
| (2 Octets)                                                    |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"  
  (len +structure-len+ :type octet)
  (conn-type (error "conn-type required!") :type octet)
  (individual-address (error "individual-address required!")
   :type knx-individual-address))

(defun parse-crd (pkg-data)
  (%make-crd :len (elt pkg-data 0)
             :conn-type (elt pkg-data 1)
             :individual-address (parse-individual-address
                                  (subseq pkg-data 2 4))))
