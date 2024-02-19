(defpackage :knx-conn.connect
  (:use :cl :hpai :cri :knxobj)
  (:nicknames :connect)
  (:export #:knx-connect-request
           #:make-connect-request
           ))


(in-package :knx-conn.connect)

(defconstant +knx-connect-request+ #x0205)
(defconstant +knx-connect-response+ #x0206)

;; -----------------------------
;; knx connect request
;; -----------------------------

(defstruct (knx-connect-request (:include knx-package)
                                (:constructor %make-connect-request)
                                (:conc-name connect-request-))
  "KNXnet/IP header (see above)
KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| HPAI                                                          |
| Data endpoint                                                 |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| CRI                                                           |
| Connection request information                                |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (hpai-ctrl-endpoint (error "Required hpai ctrl endpoint!") :type hpai)
  (hpai-data-endpoint (error "Required hpai data endpoint!") :type hpai)
  (cri (error "Required cri!") :type cri))

(defun make-connect-request ()
  (let ((hpai-ctrl-endpoint *hpai-unbound-addr*)
        (hpai-data-endpoint *hpai-unbound-addr*)
        (cri (make-tunneling-cri)))
    (%make-connect-request
     :header (make-header +knx-connect-request+
                          (+ (hpai-len hpai-ctrl-endpoint)
                             (hpai-len hpai-data-endpoint)
                             (cri-len cri)))
     :hpai-ctrl-endpoint hpai-ctrl-endpoint
     :hpai-data-endpoint hpai-data-endpoint
     :cri cri
     :body (list hpai-ctrl-endpoint hpai-data-endpoint cri))))

;; -----------------------------
;; knx description response
;; -----------------------------
