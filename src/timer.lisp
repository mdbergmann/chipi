(defpackage :cl-hab.timer
  (:documentation "A simple timer package
  This package provides a simple timer that can be used to schedule functions.")
  (:use :cl)
  (:nicknames :timer)
  (:export #:ensure-timer
           #:shutdown-timer
           #:schedule
           #:schedule-recurring
           #:cancel
           #:cancel-for-sig))

(in-package :cl-hab.timer)

(defvar *timer* nil)
(defvar *timer-hash* nil)

(defun ensure-timer ()
  "Ensure that a timer is running."
  (unless *timer*
    (setf *timer* (wt:make-wheel-timer :max-size 300 :resolution 100)))
  (unless *timer-hash*
    (setf *timer-hash* (make-hash-table :test 'equal))))

(defun shutdown-timer ()
  (when *timer*
    (wt:shutdown-wheel-timer *timer*)
    (setf *timer* nil)
    (setf *timer-hash* nil))
  t)

(defun schedule (delay fun)
  "Schedule a function to be called after a delay in seconds"
  (ensure-timer)
  (wt:schedule *timer* delay fun))

(defun schedule-recurring (delay fun sig)
  "Schedule a function to be called after a delay in seconds, recurring.
The function will be called with no arguments.
The signature is used to identify the timer.
If the timer is cancelled via `cancel-for-sig' the timer will not be called again."
  (ensure-timer)
  (let (recurring-timer-fun)
    (setf recurring-timer-fun
          (lambda ()
            ;; only if signature still exists in hash-table
            (when (gethash sig *timer-hash*)
              (funcall fun)
              (setf (gethash sig *timer-hash*)
                    (schedule delay recurring-timer-fun)))))
    (setf (gethash sig *timer-hash*)
          (schedule delay recurring-timer-fun))))

(defun cancel (timer)
  "Cancel a timer"
  (ensure-timer)
  (wt:cancel *timer* timer))
 
(defun cancel-for-sig (sig)
  "Cancel a timer from a signature"
  (ensure-timer)
  (wt:cancel *timer* (gethash sig *timer-hash*)))
