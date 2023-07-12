(defpackage :cl-hab.env
  (:use :cl)
  (:nicknames :envi)
  (:export #:ensure-isys
           #:ensure-env
           #:shutdown-isys
           #:shutdown-env))

(in-package :cl-hab.env)

(defvar *isys* nil)

(defun ensure-env ()
  (ensure-isys)
  (timer:ensure-timer)
  (cr:ensure-cron)
  t)

(defun shutdown-env ()
  (shutdown-isys)
  (timer:shutdown-timer)
  (cr:shutdown-cron)
  t)

;; actor system

(defun ensure-isys ()
  (or *isys*
      (prog1
          (setf *isys* (asys:make-actor-system))
        ;; separate dispatcher for tasks
        (asys:register-dispatcher *isys*
                                  (disp:make-dispatcher
                                   *isys*
                                   :tasks
                                   :workers 4
                                   :stragety :round-robin))
        (setf tasks:*task-context* *isys*
              tasks:*task-dispatcher* :tasks))))

(defun shutdown-isys ()
  (when *isys*
    (ac:shutdown *isys* :wait t)
    (setf *isys* nil
          tasks:*task-context* nil
          tasks:*task-dispatcher* nil)
    t))
