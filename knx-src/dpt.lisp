(defpackage :knx-conn.dpt
  (:use :cl :knxobj)
  (:nicknames :dpt)
  (:export #:dpt
           #:dpt1
           #:dpt-len
           #:dpt-value
           #:make-dpt1))

(in-package :knx-conn.dpt)

(defgeneric dpt-len (dpt)
  (:documentation "Return the length of the DPT"))

(defstruct (dpt (:include knx-obj)
                (:constructor nil))
  "A DPT is a data point type. It is a data structure that represents a")

;; ------------------------------
;; DPT1
;; ------------------------------

(defstruct (dpt1 (:include dpt)
                 (:conc-name dpt-)
                 (:constructor %make-dpt1))
  "
            +-7-+-6-+-5-+-4-+-3-+-2-+-1-+-0-+
Field Names |                             b |
Encoding    |                             B |
            +---+---+---+---+---+---+---+---+
Format:     1 bit (B<sub>1</sub>)
Range:      b = {0 = off, 1 = on}"
  (value-type (error "Required value-type") :type string)
  (value (error "Required value!") :type octet))

(defmethod dpt-len ((dpt dpt1))
  1)

(defmethod to-byte-seq ((dpt dpt1))
  (vector (dpt-value dpt)))

(defun make-dpt1 (value-sym value)
  (ecase value-sym
    (:switch
        (%make-dpt1 :value-type "1.001"
                    :value (ecase value
                             (:on 1)
                             (:off 0))))))

;; more

