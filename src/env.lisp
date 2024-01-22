(defpackage :chipi.env
  (:use :cl)
  (:nicknames :envi)
  (:export #:ensure-env
           #:shutdown-env))

(in-package :chipi.env)

(defvar *runtime-dir* (asdf:system-relative-pathname "chipi" "runtime/"))

(defun ensure-runtime-dir ()
  (uiop:ensure-all-directories-exist (list *runtime-dir*)))

(defun ensure-env ()
  (ensure-runtime-dir)
  (isys:ensure-isys)
  (timer:ensure-timer)
  (cr:ensure-cron)
  t)

(defun shutdown-env ()
  (isys:shutdown-isys)
  (timer:shutdown-timer)
  (cr:shutdown-cron)
  t)
