(defpackage :knx-conn.cemi
  (:use :cl :knxutil :knxobj)
  (:nicknames :cemi)
  (:export #:cemi-l-data
           #:cemi-message-code
           #:cemi-ctrl1
           #:cemi-ctrl2
           #:cemi-source-addr
           #:cemi-destination-addr
           #:cemi-npdu
           #:parse-cemi
           ;; mcs
           #:+cemi-mc-l_data.req+
           #:+cemi-mc-l_data.con+
           #:+cemi-mc-l_data.ind+
           ))

(in-package :knx-conn.cemi)

(defconstant +cemi-mc-l_data.req+ #x11
  "L_Data.req (data service request")
(defconstant +cemi-mc-l_data.con+ #x2e
  "L_Data.con (data service confirmation")
(defconstant +cemi-mc-l_data.ind+ #x29
  "L_Data.ind (data service indication")

(defstruct (cemi (:include knx-obj)
                 (:conc-name cemi-)
                 (:constructor nil))
  "CEMI frame
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
  (message-code (error "Required message-code!") :type octet)
  (info-len (error "Required info-len!") :type octet)
  (additional-info nil :type (or null (vector octet))))

(defstruct (cemi-l-data (:include cemi)
                        (:conc-name cemi-)
                        (:constructor %make-cemi-l-data))
  "L_Data.ind (data service indication
3.3.4.4 L_Data.ind message (EMI1 / EMI2)"
  (ctrl1 (error "Required ctrl1!") :type (bit-vector 8))
  (ctrl2 (error "Required ctrl2!") :type (bit-vector 8))
  (source-addr (error "Required source-address!")
   :type (vector octet 2))
  (destination-addr (error "Required destination-address!")
   :type (vector octet 2))
  (npdu-len (error "Required npcu-len!") :type octet)
  (npdu nil :type (or nil (vector octet))))

(defun parse-cemi (data)
  "Parse CEMI frame from `DATA`"
  (let* ((message-code (elt data 0))
         (info-len (elt data 1))
         (service-info-start (+ 2 info-len))
         (additional-info (if (> info-len 0)
                              (subseq data 2 service-info-start)
                              nil))
         (service-info (subseq data service-info-start)))
    (cond
      ((= message-code +cemi-mc-l_data.ind+)
       (let* ((ctrl1 (elt service-info 0))
              (ctrl2 (elt service-info 1))
              (source-addr (subseq service-info 2 4))
              (destination-addr (subseq service-info 4 6))
              (npdu-len (elt service-info 6))
              (npdu (if (> npdu-len 0)
                        (subseq service-info 7 (+ 7 npdu-len))
                        nil)))
         (%make-cemi-l-data
          :message-code message-code
          :info-len info-len
          :additional-info additional-info
          :ctrl1 (number-to-bit-vector ctrl1 8)
          :ctrl2 (number-to-bit-vector ctrl2 8)
          :source-addr (seq-to-array source-addr :len 2)
          :destination-addr (seq-to-array destination-addr :len 2)
          :npdu-len npdu-len
          :npdu (when npdu (seq-to-array npdu :type 'vector))))))))
