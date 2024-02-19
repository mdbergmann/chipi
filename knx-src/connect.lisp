(defpackage :knx-conn.connect
  (:use :cl :hpai :cri :crd :knxobj)
  (:nicknames :connect)
  (:export #:knx-connect-request
           #:make-connect-request
           #:knx-connect-response
           #:connect-response-channel-id
           #:connect-response-status
           #:+connect-status-no-error+
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

(defconstant +connect-status-no-error+ #x00)
(defconstant +connect-status-err-conn-type+ #x22)
(defconstant +connect-status-err-conn-option+ #x23)
(defconstant +connect-status-err-no-more-conns+ #x24)

(defstruct (knx-connect-response (:include knx-package)
                                 (:constructor %make-connect-response)
                                 (:conc-name connect-response-))
  "Connect response

KNXnet/IP header (see knx-header)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| Communication Channel ID    | Status                          |
|                             |                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Data endpoint                                                 |
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| CRD                                                           |
| Connection Response Data Block                                |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (channel-id (error "channel-id required!") :type octet)
  (status (error "status required!") :type octet)
  (hpai (error "hpai required!") :type hpai)
  (crd (error "crd required!") :type crd))


(defmethod parse-to-obj ((obj-type (eql +knx-connect-response+)) header body)
  (%make-connect-response
   :header header
   :body body
   :channel-id (elt body 0)
   :status (elt body 1)
   :hpai (parse-hpai (subseq body 2 10))
   :crd (parse-crd (subseq body 10))))
