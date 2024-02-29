(defpackage :knx-conn.descr-info
  (:use :cl :dib :hpai :knxobj)
  (:nicknames :descr-info)
  (:export #:knx-descr-request
           #:make-descr-request
           #:knx-descr-response
           #:make-descr-response
           #:descr-response-device-hardware
           #:descr-response-supp-svc-families
           #:descr-response-other-dib-info
           ))


(in-package :knx-conn.descr-info)

(defconstant +knx-descr-request+ #x0203)
(defconstant +knx-descr-response+ #x0204)

;; -----------------------------
;; knx description request
;; -----------------------------

(defstruct (knx-descr-request (:include knx-package)
                              (:constructor %make-descr-request)
                              (:conc-name descr-request-))
  "KNXnet/IP header (see above)
KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| HPAI                                                          |
| Control endpoint                                              |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"
  (hpai (error "hpai required!") :type hpai))

(defun make-descr-request (hpai)
  (%make-descr-request
   :header (make-header +knx-descr-request+ (hpai-len hpai))
   :hpai hpai))

(defmethod to-byte-seq ((obj knx-descr-request))
  (concatenate 'vector
               (call-next-method obj)
               (to-byte-seq (descr-request-hpai obj))))

;; -----------------------------
;; knx description response
;; -----------------------------

(defstruct (knx-descr-response (:include knx-package)
                               (:constructor %make-descr-response)
                               (:conc-name descr-response-))
  "KNXnet/IP header (see above)

KNXnet/IP body
+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
| DIB                                                           |
| device hardware                                               |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| DIB                                                           |
| supported service families                                    |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
| DIB                                                           |
| other device information (optional)                           |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+"  
  (device-hardware (error "Required device-hardware (dip)") :type dib-device-info)
  (supp-svc-families (error "Required supp svc families (dip)") :type dib)
  (other-dib-info nil :type (or null dib-list)))

(defmethod parse-to-obj ((obj-type (eql +knx-descr-response+)) header body)
  (let ((dibs (parse-dibs body)))
    (%make-descr-response
     :header header
     :device-hardware (first dibs)
     :supp-svc-families (second dibs)
     :other-dib-info (nthcdr 2 dibs))))

(defmethod to-byte-seq ((obj knx-descr-response))
  (concatenate 'vector
               (call-next-method obj)
               (to-byte-seq (descr-response-device-hardware obj))
               (to-byte-seq (descr-response-supp-svc-families obj))
               (map 'vector #'to-byte-seq (descr-response-other-dib-info obj))))
