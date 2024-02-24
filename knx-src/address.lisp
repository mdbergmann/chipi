(defpackage :knx-conn.address
  (:use :cl :knxutil :knxobj)
  (:nicknames :address)
  (:export #:knx-address
           #:knx-individual-address
           #:knx-group-address
           #:parse-individual-address
           #:parse-group-address
           #:address-string-rep))

(in-package :knx-conn.address)

(defstruct (knx-address (:include knx-obj)
                        (:constructor nil)
                        (:conc-name address-))
  "Base structure for individual (physical) and group-address."
  (addr (error "Required address!") :type (vector octet 2))
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
     :addr (seq-to-array addr-vector :len 2)
     :string-rep (format nil "~a.~a.~a"
                         (logand (ash addr -12) #x0f)
                         (logand (ash addr -8) #x0f)
                         (logand addr #xff)))))

(defstruct (knx-group-address (:include knx-address)
                              (:constructor %make-group-address))
  "Group address

3-level addresses are supported. The presentation styles are:

- 3-level group address: main/middle/sub (5/3/8 bits)

all in decimal format, using '/' as separator if required.  

Note, that the most significant bit of the main group, i.e., bit 15 in the unstructured
address, is reserved, but not in use for now. This bit is not checked for, but
nevertheless stored and returned by this implementation.")

(defun parse-group-address (addr-vector)
  (let ((addr (to-int (elt addr-vector 0) (elt addr-vector 1))))
    (%make-group-address
     :addr (seq-to-array addr-vector :len 2)
     :string-rep (format nil "~a/~a/~a"
                         (logand (ash addr -11) #x1f)
                         (logand (ash addr -8) #x07)
                         (logand addr #xff)))))