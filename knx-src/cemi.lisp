(defpackage :knx-conn.cemi
  (:use :cl :knxutil :knxobj :address :dpt)
  (:nicknames :cemi)
  (:export #:cemi
           #:make-cemi
           #:make-default-cemi
           #:cemi-len
           #:cemi-l-data
           #:cemi-message-code
           #:cemi-ctrl1
           #:cemi-ctrl2
           #:cemi-source-addr
           #:cemi-destination-addr
           #:cemi-tpci
           #:cemi-packet-num
           #:cemi-apci
           #:cemi-data
           #:parse-cemi
           ;; mcs
           #:+cemi-mc-l_data.req+
           #:+cemi-mc-l_data.con+
           #:+cemi-mc-l_data.ind+
           ;; ctrl1
           #:ctrl1-rep
           #:+broadcast-type-system+
           #:+broadcast-type-normal+
           #:+priority-system+
           #:+priority-normal+
           #:+priority-urgent+
           #:+priority-low+
           ;; ctrl2
           #:ctrl2-rep
           ;; tpci
           #:+tcpi-ucd+
           #:+tcpi-udt+
           #:+tcpi-ncd+
           #:+tcpi-ndt+
           ;; apci
           #:apci-gv-read-p
           #:apci-gv-read
           #:apci-gv-response-p
           #:apci-gv-response
           #:apci-gv-write-p
           #:apci-gv-write
           #:apci-equal-p
           #:make-apci-gv-write
           #:make-apci-gv-read
           ))

(in-package :knx-conn.cemi)

(defconstant +cemi-mc-l_data.req+ #x11
  "L_Data.req (data service request")
(defconstant +cemi-mc-l_data.con+ #x2e
  "L_Data.con (data service confirmation")
(defconstant +cemi-mc-l_data.ind+ #x29
  "L_Data.ind (data service indication")
(defun cemi-l_data-p (message-code)
  "Return T if MESSAGE-CODE is a L_Data.*"
  (or (= message-code +cemi-mc-l_data.req+)
      (= message-code +cemi-mc-l_data.con+)
      (= message-code +cemi-mc-l_data.ind+)))

;; TCPI
(defconstant +tcpi-udt+ #x00
  "UDT (Unnumbered Package)")
(defconstant +tcpi-ndt+ #x40
  "NDT (Numbered Package)")
(defconstant +tcpi-ucd+ #x80
  "UCD (Unnumbered Control Data)")
(defconstant +tcpi-ncd+ #xc0
  "NCD (Numbered Control Data)")

;; Broadcast Type
(defconstant +broadcast-type-system+ #x00)
(defconstant +broadcast-type-normal+ #x01)

;; Priority
(defconstant +priority-system+ #x00)
(defconstant +priority-normal+ #x01)
(defconstant +priority-urgent+ #x02)
(defconstant +priority-low+ #x03)

;; ctrl2 values
(defconstant +hop-count-default+ 6)
(defconstant +frame-format-standard+ #x0)

;; APCI
(defstruct (apci (:constructor nil)
                 (:conc-name apci-))
  (start-code #x00 :type octet :read-only t)
  (end-code #x00 :type octet :read-only t))
;; +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
;; |                         0   0 | 0   0   0   0   0   0   0   0 |
;; +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
(defstruct (apci-gv-read (:include apci)
                         (:constructor %make-apci-gv-read))
  "Group Value Read")

(defun make-apci-gv-read ()
  (%make-apci-gv-read :start-code #x00 :end-code #x00))

;; +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
;; |                         0   0 | 0   1   n   n   n   n   n   n |
;; +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
(defstruct (apci-gv-response (:include apci)
                             (:constructor %make-apci-gv-response))
  "Group Value Response")

(defun make-apci-gv-response ()
  (%make-apci-gv-read :start-code #x40 :end-code #x7f))

;; +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
;; |                         0   0 | 1   0   n   n   n   n   n   n |
;; +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
(defstruct (apci-gv-write (:include apci)
                          (:constructor %make-apci-gv-write))
  "Group Value Write")

(defun make-apci-gv-write ()
  (%make-apci-gv-write :start-code #x80 :end-code #xbf))

(defgeneric apci-equal-p (apci apci-value)
  (:documentation "Return T if APCI is equal to APCI-VALUE"))

(defmethod apci-equal-p ((apci apci-gv-write) apci-value)
  (and (>= apci-value (apci-start-code apci))
       (<= apci-value (apci-end-code apci))))

(defmethod apci-equal-p ((apci apci-gv-response) apci-value)
  (and (>= apci-value (apci-start-code apci))
       (<= apci-value (apci-end-code apci))))

(defmethod apci-equal-p ((apci apci-gv-read) apci-value)
  (= apci-value (apci-start-code apci)))

(defparameter *apcis* (list (make-apci-gv-read)
                            (make-apci-gv-response)
                            (make-apci-gv-write))
  "List of supported APCI values")


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

(defgeneric cemi-len (cemi)
  (:documentation "Return the length of the CEMI frame"))

(defmethod cemi-len ((cemi cemi))
  "Return the length of the CEMI frame"
  (+ 2 (cemi-info-len cemi)))

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
  ;; NPDU (Network Protocol Data Unit)
  ;; TPCI (Transport Layer Control Information)
  ;; first two bits of npdu+1
  ;; 00.. .... UDT unnumbered package
  ;; 01.. .... NDT numbered package
  ;; 10.. .... UCD unnumbered control data
  ;; 11.. .... NCD numbered control data
  (tpci (error "Required tcpi!") :type octet)
  ;; ..xx xx.. packet number (of npdu+1)
  (packet-num (error "Required packet-num!") :type octet)
  ;; .... ..xx xx.. .... APCI code
  ;; .... ..00 0000 0000 group value request
  ;; .... ..00 01nn nnnn group value response
  ;; .... ..00 10nn nnnn group value write (not requested)
  ;; .... ..00 1100 0000 individual address write
  ;; .... ..01 0000 0000 individual address read
  ;; .... ..01 0100 0000 individual address response
  (apci (error "Required apci!") :type apci)
  (data nil :type (or null dpt (vector octet))))

(defmethod cemi-len ((cemi cemi-l-data))
  "Return the length of the CEMI frame"
  (+ (call-next-method cemi)
     1 ; ctrl1
     1 ; ctrl2
     (* 2 (address:address-len))
     1 ; npdu-len
     (cemi-npdu-len cemi)))

;; --------------------------
;; control octet 1
;; --------------------------

(defun %make-ctrl1-octet (&key (standard-frame t)
                            (repeat nil)
                            (broadcast-type +broadcast-type-normal+)
                            (priority +priority-low+)
                            (ack-request nil)
                            (err-confirm nil))
  (logior (if standard-frame (ash #x01 7) #x00)
          (if repeat 0 (ash #x01 5))
          (ash broadcast-type 4)
          (ash priority 2)
          (if ack-request #x02 #x00)
          (if err-confirm #x01 #x00)))

(defun ctrl1-rep (cemi)
  (list :standard-frame (%ctrl1-standard-frame-p cemi)
        :repeat-enabled (%ctrl1-repeat-p cemi)
        :broadcast-type (%ctrl1-broadcast-type cemi)
        :priority (%ctrl1-priority cemi)
        :ack-requested (%ctrl1-ack-p cemi)
        :error-confirmation (%ctrl1-error-confirm-p cemi)))

(defun %ctrl1-standard-frame-p (cemi)
  "Return T if CEMI is a standard frame
x... .... frame type
0 = extended frame (9-263 octets)
1 = standard frame (8-23 octets)"
  (= 1 (elt (cemi-ctrl1 cemi) 0)))

(defun %ctrl1-repeat-p (cemi)
  "Return T if CEMI is a repeat frame
..x. .... repeat
0 = repeat on medium if error
1 = do not repeat"
  (= 0 (elt (cemi-ctrl1 cemi) 2)))

(defun %ctrl1-broadcast-type (cemi)
  "Return the broadcast type of CEMI
...x .... broadcast
0 = system broadcast
1 = normal broadcast"
  (cond
    ((= 0 (elt (cemi-ctrl1 cemi) 3))
     +broadcast-type-system+)
    ((= 1 (elt (cemi-ctrl1 cemi) 3))
     +broadcast-type-normal+)))

(defun %ctrl1-priority (cemi)
  "Return the priority of CEMI
.... xx.. priority"
  (let ((ctrl1 (cemi-ctrl1 cemi)))
    (cond
      ((and (elt ctrl1 4) (elt ctrl1 5))
       +priority-low+)
      ((and (elt ctrl1 4) (not (elt ctrl1 5)))
       +priority-urgent+)
      ((and (not (elt ctrl1 4)) (elt ctrl1 5))
       +priority-normal+)
      ((and (not (elt ctrl1 4)) (not (elt ctrl1 5)))
       +priority-system+))))

(defun %ctrl1-ack-p (cemi)
  "Return T if CEMI requests an ACK
.... ..x. acknowledge request flag
0 = no ACK requested
1 = ACK requested"
  (= 1 (elt (cemi-ctrl1 cemi) 6)))

(defun %ctrl1-error-confirm-p (cemi)
  "Return T if CEMI is an error confirmation
.... ...x confirmation flag
0 = no error (confirm)
1 = error (L-Data.Connection)"
  (= 1 (elt (cemi-ctrl1 cemi) 7)))

;; --------------------------
;; control octet 2
;; --------------------------

(defun %make-ctrl2-octet (address)
  (logior (if (knx-group-address-p address) #x80 #x00)
          (ash (logand +hop-count-default+ #x07) 4)
          (logand +frame-format-standard+ #x0f)))

(defun ctrl2-rep (cemi)
  (list :address-type (%ctrl2-address-type cemi)
        :hop-count (%ctrl2-hop-count cemi)
        :frame-format (%ctrl2-frame-format cemi)))

(defun %ctrl2-address-type (cemi)
  "Return T if CEMI is a group address
x... .... destination address type
0 = individual address
1 = group address"
  (if (= 1 (elt (cemi-ctrl2 cemi) 0))
      'knx-group-address
      'knx-individual-address))

(defun %ctrl2-hop-count (cemi)
  "Return the hop count of CEMI
.xxx .... routing / hop count"
  (ash (logand (bit-vector-to-number (cemi-ctrl2 cemi)) #x70) -4))

(defun %ctrl2-frame-format (cemi)
  "Return the frame format of CEMI
.... xxxx extended frame format
.... 0000 for standard frame
.... 01xx for LTE frames
.... 1111 for Escape (reserved by KNX Association)"
  (logand (bit-vector-to-number (cemi-ctrl2 cemi)) #x0f))    

;; --------------------------
;; parsing and construction
;; --------------------------

(defun array-copy (target source &key (start-target 0))
  "Copy elements from SOURCE to TARGET"
  (let ((target-index start-target)
        (copied 0))
    (loop :for b :across source
          :do (setf (elt target target-index) b)
              (incf target-index)
              (incf copied))
    copied))

(defmethod to-byte-seq ((cemi cemi-l-data))
  (let ((bytes (make-array 255 :element-type 'octet))
        (byte-count 0)
        (optimized-apci nil)
        (apci-data-byte-array #()))
    (setf (elt bytes byte-count) (cemi-message-code cemi))
    (incf byte-count)
    (setf (elt bytes byte-count) (cemi-info-len cemi))
    (incf byte-count)
    (when (cemi-additional-info cemi)
      (incf byte-count
            (array-copy bytes
                        (cemi-additional-info cemi)
                        :start-target byte-count)))
    (setf (elt bytes byte-count) (bit-vector-to-number (cemi-ctrl1 cemi)))
    (incf byte-count)
    (setf (elt bytes byte-count) (bit-vector-to-number (cemi-ctrl2 cemi)))
    (incf byte-count)
    (incf byte-count
          (array-copy bytes
                      (to-byte-seq (cemi-source-addr cemi))
                      :start-target byte-count))
    (incf byte-count
          (array-copy bytes
                      (to-byte-seq (cemi-destination-addr cemi))
                      :start-target byte-count))
    (setf (elt bytes byte-count) (cemi-npdu-len cemi))
    (incf byte-count)
    (setf (elt bytes byte-count)
          (logior (cemi-tpci cemi)
                  (ash (cemi-packet-num cemi) 2)
                  (ash (apci-start-code (cemi-apci cemi)) -8)))
    (incf byte-count)
    (setf (elt bytes byte-count)
          (let* ((apci (cemi-apci cemi))
                 (data (cemi-data cemi))
                 (npdu-len (cemi-npdu-len cemi))
                 (dpt-value (cond
                              ((null data) 0)
                              ((arrayp data) (elt data 0)) ; ?
                              (t (dpt-value data)))))
            (logior (logand (apci-start-code apci) #xff)
                    (cond
                      ((apci-gv-read-p apci) #x00)
                      ((or
                        (apci-gv-response-p apci)
                        (apci-gv-write-p apci))
                       (if (= 1 npdu-len)
                           (prog1
                               (logand dpt-value #x3f)
                             (setf optimized-apci t))
                           (prog1
                               #x00
                             (setf apci-data-byte-array
                                   (cond
                                     ((arrayp data) data)
                                     (t (to-byte-seq data)))))))
                      (t (error "APCI not supported"))))))
    (incf byte-count)
    (when (not optimized-apci)
      (incf byte-count
            (array-copy bytes
                        apci-data-byte-array
                        :start-target byte-count)))
    (subseq bytes 0 byte-count)))

(defun parse-cemi (data)
  "Parse CEMI frame from `DATA`"
  (let* ((message-code (elt data 0))
         (info-len (elt data 1))
         (service-info-start (+ 2 info-len))
         (additional-info (if (> info-len 0)
                              (subseq data 2 (1- service-info-start)) ; ?
                              nil))
         (service-info (subseq data service-info-start)))
    (cond
      ((cemi-l_data-p message-code)
       (let* ((ctrl1 (elt service-info 0))
              (ctrl2 (elt service-info 1))
              (source-addr (subseq service-info 2 4))
              (destination-addr (subseq service-info 4 6))
              (npdu-len (elt service-info 6))
              (npdu (if (> npdu-len 0)
                        (seq-to-array
                         (subseq service-info 6); (+ 6 (1+ npdu-len))) ; + len-byte
                         :type 'vector)
                        nil))
              (tpci (when npdu
                      (logand (elt npdu 1) #xc0)))
              (packet-num (when npdu
                            (ash (logand (elt npdu 1) #x3c) -2)))
              (apci (when npdu
                      (let ((apci-value (to-int
                                         (logand (elt npdu 1) #x03)
                                         (logand (elt npdu 2) #xc0))))
                        (find-if (lambda (apci)
                                   (apci-equal-p
                                    apci apci-value))
                                 *apcis*))))
              (data (when npdu
                      (cond
                        ((apci-gv-read-p apci)
                         nil)
                        ((= npdu-len 1)
                         ;; 6 bits, part of apci / optimized dpt
                         (vector (logand (elt npdu 2) #x3f)))
                        (t
                         ;; then bytes are beyond the apci
                         (let* ((start-index 3)
                                (end-index (1- (+ start-index npdu-len))))
                           (seq-to-array
                            (subseq npdu start-index end-index)
                            :type 'vector)))))))
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
          :tpci tpci
          :packet-num packet-num
          :apci apci
          :data data))))))

(defun make-default-cemi (&key message-code dest-address apci dpt)
  (let ((add-info nil)
        (ctrl1 (%make-ctrl1-octet))
        (ctrl2 (%make-ctrl2-octet dest-address))
        (source-addr (make-individual-address "0.0.0"))
        (tpci +tcpi-udt+)
        (packet-num 0))
    (%make-cemi-l-data
     :message-code message-code
     :info-len (length add-info)
     :additional-info add-info
     :ctrl1 (number-to-bit-vector ctrl1 8)
     :ctrl2 (number-to-bit-vector ctrl2 8)
     :source-addr source-addr
     :destination-addr dest-address
     :npdu-len (cond
                 ((apci-gv-read-p apci) 1)
                 ((or
                   (apci-gv-response-p apci)
                   (apci-gv-write-p apci))
                  ;; TODO: support for optimized dpts
                  (+ 1 (dpt-len dpt)))
                 (t (error "APCI not supported")))
     :tpci tpci
     :packet-num packet-num
     :apci apci
     :data dpt)))
