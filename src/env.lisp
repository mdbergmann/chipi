(defpackage :chipi.env
  (:use :cl)
  (:nicknames :envi)
  (:export #:ensure-env
           #:shutdown-env))

(in-package :chipi.env)

(defun ensure-env ()
  (isys:ensure-isys)
  (timer:ensure-timer)
  (cr:ensure-cron)
  t)

(defun shutdown-env ()
  (isys:shutdown-isys)
  (timer:shutdown-timer)
  (cr:shutdown-cron)
  t)
