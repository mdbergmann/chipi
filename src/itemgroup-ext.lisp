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
  "Convert an itemgroup with all its items to a hash-table ready for JSON serialization.
Leere Item-Listen werden als leeres Array ([]) und nicht als JSON-Boolean „false“
kodiert."
  (let* ((item-list (mapcar #'item-to-ht (itemgroup:get-items grp)))
         ;; `jzon` kodiert NIL → false; darum bei leerer Liste auf leeren Vektor #() mappen
         (items-json (if item-list item-list #())))
    (plist-hash-table
     (list "name"  (itemgroup:name grp)
           "label" (itemgroup:label grp)
           "items" items-json)
     :test #'equal)))
