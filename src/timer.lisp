(defpackage :chipi.timer
  (:documentation "A simple timer package
  This package provides a simple timer that can be used to schedule functions.")
  (:use :cl)
  (:nicknames :timer)
  (:export #:ensure-timer
           #:shutdown-timer
           #:schedule-once
           #:schedule-recurring
           #:cancel))

(in-package :chipi.timer)

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

(defun schedule-once (delay fun)
  "Schedule a function to be called after a delay in seconds
Returns a timer signature to be used for `CANCEL'"
  (ensure-timer)
  (wt:schedule-once *timer* delay fun))

(defun schedule-recurring (delay fun &optional (sig nil))
  "Schedule a function to be called after a delay in seconds, recurring.
The function will be called with no arguments.
Returns a timer signature to be used for `CANCEL'"
  (ensure-timer)
  (wt:schedule-recurring *timer* delay delay fun sig))

(defun cancel (sig)
  "Cancel a timer"
  (ensure-timer)
  (wt:cancel *timer* sig))
