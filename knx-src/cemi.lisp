(defpackage :knx-conn.cemi
  (:use :cl :knxutil :knxobj)
  (:nicknames :cemi)
  (:export #:cemi
           #:cemi-l-data
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
   :type knx-address)
  (destination-addr (error "Required destination-address!")
   :type knx-address)
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
          :source-addr (parse-individual-address source-addr)
          :destination-addr (if (= 0 (elt destination-addr 0))
                                (parse-individual-address destination-addr)
                                (parse-group-address destination-addr))
          :npdu-len npdu-len
          :npdu (when npdu (seq-to-array npdu :type 'vector))))))))

(defstruct (knx-address (:include knx-obj)
                        (:constructor nil)
                        (:conc-name address-))
  "Base structure for individual (physical) and group-address."
  (addr (error "Required address!") :type (integer 2))
  (string-rep (error "Required string representation!") :type string))

(defstruct (knx-individual-address (:include knx-address)
                                   (:constructor %make-individual-address))
  "Individual address

An individual address is built up from the 3 levels _area_, _line_, and
_device_. The address structure consists of a 16 bit address, consisting of
(starting from the most significant bit): area (4 bits), line (4 bits), and device (8
bits). The individual address KNX notation follows _area.line.device_, with the
required separator of type '.'.

The combined address levels _area_ and _line_ are referred to as subnetwork
address, i.e., and described by the higher 8 bits of the address value.<br>
The sometimes used term _zone_ is synonymous with _area_.")

(defun parse-individual-address (addr-vector)
  (let ((addr (to-int (elt addr-vector 0) (elt addr-vector 1))))
    (%make-individual-address
     :addr addr
     :string-rep (format nil "~a.~a.~a"
                         (logand (ash addr -12) #x0f)
                         (logand (ash addr -8) #x0f)
                         (logand addr #xff)))))

(defstruct (knx-group-address (:include knx-address)
                              (:constructor %make-group-address))
  "Group address

2-level, 3-level, and free-style addresses are supported. The presentation styles are:

- 2-level group address: main/sub (5/11 bits)
- 3-level group address: main/middle/sub (5/3/8 bits)
- free-style group address: unstructured address (16 bits)

all in decimal format, using '/' as separator if required.  
By default, the 3-level preset is used.

Note, that the most significant bit of the main group, i.e., bit 15 in the unstructured
address, is reserved, but not in use for now. This bit is not checked for, but
nevertheless stored and returned by this implementation.")

(defun parse-group-address (addr-vector)
  (let ((addr (to-int (elt addr-vector 0) (elt addr-vector 1))))
    (%make-group-address
     :addr addr
     :string-rep (format nil "~a/~a/~a"
                         (logand (ash addr -11) #x1f)
                         (logand (ash addr -8) #x07)
                         (logand addr #xff)))))
