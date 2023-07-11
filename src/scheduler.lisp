(defpackage :cl-hab.scheduler
  (:use :cl)
  (:nicknames :sched)
  (:export #:ensure-timer
           #:shutdown-timer
           #:schedule
           #:cancel))

(in-package :cl-hab.scheduler)

(defvar *timer* nil)

(defun ensure-timer ()
  (or *timer*
      (setf *timer* (wt:make-wheel-timer :max-size 300 :resolution 100))))

(defun shutdown-timer ()
  (when *timer*
    (wt:shutdown-wheel-timer *timer*)
    (setf *timer* nil))
  t)

(defun schedule (delay fun)
  "Schedule a function to be called after a delay in seconds"
  (let ((timer-wheel (ensure-timer)))
    (wt:schedule timer-wheel delay fun)))

(defun cancel (timer)
  "Cancel a timer"
  (let ((timer-wheel (ensure-timer)))
    (wt:cancel timer-wheel timer)))
 
