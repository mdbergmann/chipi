(defpackage :knx-conn.utils
  (:use :cl)
  (:nicknames :knxutil)
  (:export #:byte-seq-to-byte-array
           #:to-int
           #:int-to-byte-vec
           #:int-to-byte-list
           #:seq-to-array
           #:number-to-bit-vector
           #:bit-vector-to-number
           ))

(in-package :knx-conn.utils)

(defun byte-seq-to-byte-array (byte-seq)
  (let ((pkg (alexandria:flatten byte-seq)))
    (make-array (list (length pkg))
                :initial-contents pkg
                :element-type '(unsigned-byte 8))))

(defun to-int (upper lower)
  (+ (ash upper 8) lower))

(defun int-to-byte-vec (int &optional (len 2))
  "Converts an integer to a simple-array (unsigned-int 8)."
  (let ((byte-list (int-to-byte-list int len)))
    (make-array (length byte-list)
                :element-type '(unsigned-byte 8)
                :initial-contents byte-list)))

(defun int-to-byte-list (int &optional (len 2))
  "Converts an integer to a list of bytes."
  (reverse
   (loop :for i :from 0 :below len
         :collect (logand (ash int (* i -8)) #xff))))

(defun seq-to-array (seq &key (len 0 len-present-p) (type 'simple-array))
  (if len-present-p
      (coerce seq `(,type (unsigned-byte 8) (,len)))
      (coerce seq `(,type (unsigned-byte 8)))))

(defun number-to-bit-vector (num bit-len)
  (coerce (reverse
           (loop :for i :from 0 :below bit-len
                 :collect (logand (ash num (* i -1)) 1)))
          `(vector bit ,bit-len)))

(defun bit-vector-to-number (bit-vec)
  (let ((len (length bit-vec)))
    (loop :for i :from 0 :below len
          :sum (ash (elt bit-vec i) (- (1- len) i)))))
