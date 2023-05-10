(defpackage :cl-eta.scheduler
  (:use :cl)
  (:nicknames :sched)
  (:import-from #:envi
                #:ensure-timer)
  (:export #:schedule))

(in-package :cl-eta.scheduler)

(defun schedule (delay fun)
  (let ((timer (ensure-timer)))
    (wt:schedule timer delay fun)))
