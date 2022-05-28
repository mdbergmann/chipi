(defpackage :cl-eta.package
  (:use :cl)
  (:nicknames :eta-pkg)
  (:export #:collect-data
           #:extract-pkg
           #:check-sum
           #:new-start-record-pkg))

(in-package :cl-eta.package)

(defun collect-data (prev-data new-data)
  "Concatenates `prev-data' and `new-data'.
A full package is when it starts with `#\{' and ends with `#\}'.
In this case the return is `(values t <full-package>)'.
If this is a partial package the return is: `(values nil <partial-package>)'."
  (let* ((data (concatenate 'vector prev-data new-data))
         (data-len (length data)))
    (values
     (if (> data-len 0)
         (let ((first (elt data 0))
               (last (elt data (1- data-len))))
           (and (characterp first)
                (characterp last)
                (char= #\{ first)
                (char= #\} last)))
         nil)
     data)))

(defun check-sum (seq)
  (mod (reduce #'+ seq) 256))

(defun to-int (upper lower)
  (+ (ash upper 8) lower))

(defun to-vec (int)
  (vector (ash (mask-field (byte 16 8) int) -8)
          (mask-field (byte 8 0) int)))

(defparameter +monitor-items+
  '((167 . ("BoilerUnten" . 10.0))))

(defun id-to-item-name (mid)
  (cadr (find mid +monitor-items+ :key #'car :test #'=)))

(defun id-to-item-divisor (mid)
  (cddr (find mid +monitor-items+ :key #'car :test #'=)))

(defun process-monitors (monitor-data)
  (let ((monitors (/ (length monitor-data) 5)))
    (loop :for i :to (1- monitors)
          :for m = (subseq monitor-data (* i 5))
          :for node-id = (elt m 0)
          :for m-id = (to-int (elt m 1) (elt m 2))
          :for m-val = (to-int (elt m 3) (elt m 4))
          :for item-name = (id-to-item-name m-id)
          :for item-div = (id-to-item-divisor m-id)
          :collect `(,item-name . ,(/ m-val item-div)))))


(defun extract-pkg (pkg-data)
  "`pkg-data' is a full eta package with starting `#\{' and ending `#\}'.
If something happens during extraction the return is:
`(values :fail <reason>)'.
If it is a full package with monitors data the return is:
`(values :monitor <alist-of-monitor-items)' where an item consists of: `(cons <openhab-item-name> <item-value>)'."
  (if (< (length pkg-data) 6) ; minimal size of package
      (values :fail "Undersized package!")
      (let ((sid (coerce `#(,(elt pkg-data 1) ,(elt pkg-data 2)) 'string))
            (payload-len (elt pkg-data 3))
            (checksum (elt pkg-data 4))
            (payload (subseq pkg-data 5 (1- (length pkg-data)))))
        (values :monitor (process-monitors payload)))))
      

(defun new-start-record-pkg ()
  "Returns a new `start-record' eta package."
  nil)

