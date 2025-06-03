(defpackage :chipi.itemgroup-ext
  (:use :cl :itemgroup)
  (:nicknames :itemgroup-ext)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:import-from #:chipi.item-ext
                #:item-to-ht)
  (:export #:itemgroup-to-ht))

(in-package :chipi.itemgroup-ext)

(defun itemgroup-to-ht (grp)
  "Convert an itemgroup (including its items) to a hash-table ready for JSON
serialization.  Empty item lists are encoded as an empty array ([]) instead of the
JSON boolean «false»."
  (let* ((item-list (mapcar #'item-to-ht (itemgroup:get-items grp)))
         ;; `jzon` encodes NIL as JSON boolean false – map empty list to empty vector #()
         (items-json (if item-list item-list #())))
    (plist-hash-table
     (list "name"  (itemgroup:name grp)
           "label" (itemgroup:label grp)
           "items" items-json)
     :test #'equal)))
