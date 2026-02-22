(defpackage :chipi.itemgroup-ext
  (:use :cl :itemgroup)
  (:nicknames :itemgroup-ext)
  (:import-from #:alexandria
                #:plist-hash-table
                #:alist-hash-table)
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
         (items-json (if item-list (coerce item-list 'vector) #()))
         (child-list (mapcar #'itemgroup-to-ht (itemgroup:get-child-groups grp)))
         (children-json (if child-list (coerce child-list 'vector) #()))
         (parent (itemgroup:parent-group grp)))
    (let ((tags (itemgroup:tags grp)))
      (plist-hash-table
       (list "name"  (itemgroup:name grp)
             "label" (itemgroup:label grp)
             "tags"  (if tags
                         (alist-hash-table tags :test #'equal)
                         (make-hash-table :test #'equal))
             "items" items-json
             "children" children-json
             "parent-group" (if parent (symbol-name parent) cl:nil))
       :test #'equal))))
