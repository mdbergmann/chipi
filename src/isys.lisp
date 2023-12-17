(defpackage :chipi.isys
  (:use :cl)
  (:nicknames :isys)
  (:export #:ensure-isys
           #:shutdown-isys))

(in-package :chipi.isys)

(defvar *isys* nil)

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
