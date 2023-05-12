(defpackage :cl-eta.env
  (:use :cl)
  (:nicknames :envi)
  (:export #:ensure-isys
           #:ensure-timer
           #:ensure-env
           #:shutdown-isys
           #:shutdown-timer
           #:shutdown-env))

(in-package :cl-eta.env)

(defvar *isys* nil)
(defvar *timer* nil)

(defun ensure-env ()
  (ensure-isys)
  (ensure-timer)
  t)

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

(defun shutdown-env ()
  (shutdown-isys)
  (shutdown-timer)
  t)

(defun ensure-timer ()
  (or *timer*
      (setf *timer* (wt:make-wheel-timer :max-size 300 :resolution 100))))

(defun shutdown-timer ()
  (when *timer*
    (wt:shutdown-wheel-timer *timer*)
    (setf *timer* nil))
  t)
