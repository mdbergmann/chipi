(defpackage :cl-hab.timer
  (:documentation "A simple timer package
  This package provides a simple timer that can be used to schedule functions.")
  (:use :cl)
  (:nicknames :timer)
  (:export #:ensure-timer
           #:shutdown-timer
           #:schedule
           #:cancel))

(in-package :cl-hab.timer)

(defvar *timer* nil)

(defun ensure-timer ()
  "Ensure that a timer is running."
  (unless *timer*
    (setf *timer* (wt:make-wheel-timer :max-size 300 :resolution 100))))

(defun shutdown-timer ()
  (when *timer*
    (wt:shutdown-wheel-timer *timer*)
    (setf *timer* nil))
  t)

(defun schedule (delay fun)
  "Schedule a function to be called after a delay in seconds"
  (ensure-timer)
  (wt:schedule *timer* delay fun))

(defun cancel (timer)
  "Cancel a timer"
  (ensure-timer)
  (wt:cancel *timer* timer))
 
