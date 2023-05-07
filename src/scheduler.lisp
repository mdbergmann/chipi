(defpackage :cl-eta.scheduler
  (:use :cl)
  (:nicknames :sched)
  (:import-from #:hab
                #:*timer*)
  (:export #:schedule))

(in-package :cl-eta.scheduler)

(defun schedule (delay fun)
  (wt:schedule *timer* delay fun))
