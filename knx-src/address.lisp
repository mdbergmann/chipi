(defpackage :knx-conn.address
  (:use :cl :knxutil :knxobj)
  (:nicknames :address)
  (:export #:knx-address
           #:address-len
           #:knx-individual-address
           #:make-individual-address
           #:knx-group-address
           #:make-group-address
           #:knx-group-address-p
           #:parse-individual-address
           #:parse-group-address
           #:address-string-rep))

(in-package :knx-conn.address)

(defstruct (knx-address (:include knx-obj)
                        (:constructor nil)
                        (:conc-name address-))
  "Base structure for individual (physical) and group-address."
  ;; NOTE: addr is the byte representation as in the knx data stream
  ;; and needs no conversion.
  (addr (error "Required address!") :type (vector octet 2))
  (string-rep (error "Required string representation!") :type string))

(defmethod to-byte-seq ((addr knx-address))
  "Convert the address to a byte sequence."
  (address-addr addr))

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

(defun address-len () 2)

(defgeneric make-individual-address (addr)
  (:documentation "Create an individual address."))

(defmethod make-individual-address ((addr-string string))
  (let ((addr (map '(vector octet 3) #'parse-integer
                   (uiop:split-string addr-string :separator "."))))
    (make-individual-address addr)))

(defmethod make-individual-address ((addr-vector vector))
  (let ((area (elt addr-vector 0))
        (line (elt addr-vector 1))
        (device (elt addr-vector 2)))
    (let* ((upper (ash area 4))
           (upper (logior upper line))
           (lower device))
      (%make-individual-address
       :addr (seq-to-array (vector upper lower) :len 2)
       :string-rep (format nil "~a.~a.~a" area line device)))))

(defun parse-individual-address (addr-vector)
  "Parse a vector of 2 octets to an individual address.
The vector has two elements in the knx spec."
  (let ((upper (elt addr-vector 0)))
    (let ((area (ash upper -4))
          (line (logand upper #x0f))
          (device (elt addr-vector 1)))
      (%make-individual-address
       :addr (seq-to-array addr-vector :len 2)
       :string-rep (format nil "~a.~a.~a" area line device)))))

(defstruct (knx-group-address (:include knx-address)
                              (:constructor %make-group-address))
  "Group address

3-level addresses are supported. The presentation styles are:

- 3-level group address: main/middle/sub (5/3/8 bits)

all in decimal format, using '/' as separator if required.  

Note, that the most significant bit of the main group, i.e., bit 15 in the unstructured
address, is reserved, but not in use for now. This bit is not checked for, but
nevertheless stored and returned by this implementation.")

(defgeneric make-group-address (addr)
  (:documentation "Create a group addres."))

(defmethod make-group-address ((addr-string string))
  (let ((addr (map '(vector octet 3) #'parse-integer
                   (uiop:split-string addr-string :separator "/"))))
    (make-group-address addr)))

(defmethod make-group-address ((addr-vector vector))
  (let ((main (elt addr-vector 0))
        (middle (elt addr-vector 1))
        (sub (elt addr-vector 2)))
    (let* ((upper (ash main 3))
           (upper (logior upper middle))
           (lower sub))
      (%make-group-address
       :addr (seq-to-array (vector upper lower) :len 2)
       :string-rep (format nil "~a/~a/~a"
                           main middle sub)))))
  
(defun parse-group-address (addr-vector)
  "Parse a vector of 2 octets to a group address."
  (let ((upper (elt addr-vector 0)))
    (let ((main (ash upper -3))
          (middle (logand upper #x07))
          (sub (elt addr-vector 1) ))
      (%make-group-address
       :addr (seq-to-array addr-vector :len 2)
       :string-rep (format nil "~a/~a/~a" main middle sub)))))
