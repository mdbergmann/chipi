(defpackage :cl-hab.cron
  (:use :cl)
  (:nicknames :cr)
  (:export #:ensure-cron
           #:shutdown-cron
           #:num-jobs
           #:make-cron-job
           #:cancel-job))

(in-package :cl-hab.cron)

(defvar *cron* nil)

(defun ensure-cron ()
  (or *cron*
      (progn
        (setf cl-cron::*cron-jobs-hash* (make-hash-table))
        (cl-cron:start-cron)
        (setf *cron* t))))

(defun shutdown-cron ()
  (when *cron*
    (cl-cron:stop-cron)
    (setf cl-cron::*cron-jobs-hash* (make-hash-table))
    (setf *cron* nil)))

(defun num-jobs ()
  (ensure-cron)
  (hash-table-count cl-cron::*cron-jobs-hash*))

(defun make-cron-job (fun &rest args)
  (ensure-cron)
  (cl-cron:make-cron-job fun
                         :minute (getf args :minute :every)
                         :hour (getf args :hour :every)
                         :day-of-month (getf args :day-of-month :every)
                         :month (getf args :month :every)
                         :day-of-week (getf args :day-of-week :every)
                         :boot-only (getf args :boot-only nil)))

(defun cancel-job (job)
  (ensure-cron)
  (cl-cron:delete-cron-job job))