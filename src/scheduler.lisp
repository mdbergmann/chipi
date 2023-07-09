(defpackage :cl-hab.scheduler
  (:use :cl)
  (:nicknames :sched)
  (:import-from #:envi
                #:ensure-timer)
  (:export #:schedule
           #:cancel))

(in-package :cl-hab.scheduler)

(defun schedule (delay fun)
  (let ((timer-wheel (ensure-timer)))
    (wt:schedule timer-wheel delay fun)))

(defun cancel (timer)
  (let ((timer-wheel (ensure-timer)))
    (wt:cancel timer-wheel timer)))
 
