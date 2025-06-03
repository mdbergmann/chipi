(defpackage :chipi.itemgroup-ext
  (:use :cl :itemgroup)
  (:nicknames :itemgroup-ext)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:chipi.item-ext
                #:item-to-ht)
  (:export #:itemgroup-to-ht))

(in-package :chipi.itemgroup-ext)

(defun itemgroup-to-ht (ig)
  "Convert an itemgroup with all its items to a hash-table ready for JSON serialization."
  (plist-hash-table
   (list "name"  (itemgroup:name ig)
         "label" (itemgroup:label ig)
         "items" (mapcar #'item-to-ht (itemgroup:get-items ig)))
   :test #'equal))
