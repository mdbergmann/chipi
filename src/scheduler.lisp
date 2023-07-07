(defpackage :cl-hab.scheduler
  (:use :cl)
  (:nicknames :sched)
  (:import-from #:envi
                #:ensure-timer)
  (:export #:schedule))

(in-package :cl-hab.scheduler)

(defun schedule (delay fun)
  (let ((timer (ensure-timer)))
    (wt:schedule timer delay fun)))
