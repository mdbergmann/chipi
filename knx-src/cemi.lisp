(defpackage :knx-conn.cemi
  (:use :cl :knxutil :knxobj)
  (:nicknames :cemi)
  (:export #:cemi
           #:parse-cemi
           ))

(in-package :knx-conn.cemi)

(defconstant +cemi-mc-l_data.ind+ #x29
  "L_Data.ind (data service indication")

(defstruct (cemi (:include knx-obj)
                 (:conc-name cemi-) 
                 (:constructor %make-cemi))
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
  ;;(service-info nil :type (or null (vector octet))))

(defstruct (cemi-l-data-ind (:include cemi)
                            (:conc-name cemi-)
                            (:constructor %make-cemi-l-data-ind))
  "L_Data.ind (data service indication
3.3.4.4 L_Data.ind message (EMI1 / EMI2)"
  (control (error "Required control!") :type (bit-vector 8))
  (source-addr (error "Required source-address!")
   :type (vector octet 2))
  (destination-addr (error "Required destination-address!")
   :type (vector octet 2))
  (npci (error "Required npci!") :type (bit-vector 8))
  (npdu (error "Required npdu!") :type (vector octet)))

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
       (let ((control (elt service-info 0))
             (source-addr (subseq service-info 1 3))
             (destination-addr (subseq service-info 3 5))
             (npci (elt service-info 5))
             (npdu (subseq service-info 6)))
         (%make-cemi-l-data-ind
          :message-code message-code
          :info-len info-len
          :additional-info additional-info
          :control (number-to-bit-vector control 8)
          :source-addr (seq-to-array source-addr :len 2)
          :destination-addr (seq-to-array destination-addr :len 2)
          :npci (number-to-bit-vector npci 8)
          :npdu (seq-to-array npdu :type 'vector)))))))
