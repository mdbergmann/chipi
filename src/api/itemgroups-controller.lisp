(defpackage :chipi-api.itemgroups-controller
  (:use :cl)
  (:nicknames :itemgroupsc)
  (:import-from #:alexandria
                #:if-let)
  (:export #:retrieve-itemgroups
           #:retrieve-itemgroup
           #:retrieve-top-level-itemgroups))

(in-package :chipi-api.itemgroups-controller)

(defun retrieve-itemgroups ()
  "Return all itemgroups as list of hash-tables."
  (if-let ((groups (hab:get-itemgroups)))
    (mapcar #'itemgroup-ext:itemgroup-to-ht groups)
    (log:info "No itemgroups.")))

(defun retrieve-top-level-itemgroups ()
  "Return top-level itemgroups (no parent) as list of hash-tables."
  (if-let ((groups (hab:get-top-level-itemgroups)))
    (mapcar #'itemgroup-ext:itemgroup-to-ht groups)
    (log:info "No top-level itemgroups.")))

(defun retrieve-itemgroup (group-name)
  "Return a single itemgroup hash-table or NIL."
  (if-let ((group (hab:get-itemgroup group-name)))
    (itemgroup-ext:itemgroup-to-ht group)
    (log:info "No itemgroup with id ~a." group-name)))
