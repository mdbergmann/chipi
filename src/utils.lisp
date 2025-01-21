(defpackage :chipi.utils
  (:use :cl)
  (:nicknames :utils)
  (:export #:ph
           #:ah))

(in-package :chipi.utils)

(defun ph (plist)
  "Shorthand for plist-hash-table."
  (alexandria:plist-hash-table plist :test 'equal))

(defun ah (alist)
  "Shorthand for alist-hash-table."
  (alexandria:alist-hash-table alist :test 'equal))
