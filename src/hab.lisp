(defpackage :cl-hab.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:defconfig
           #:item
           #:binding
           #:rule
           #:shutdown)
  )

(in-package :cl-hab.hab)

(defvar *items* nil "All items")
(defvar *rules* nil "All rules")

(defmacro defconfig (&body body)
  `(progn
     (envi:ensure-env)
     (unless *items*
       (setf *items* (make-hash-table)))
     (unless *rules*
       (setf *rules* (make-hash-table :test #'equal)))
     ,@body))

(defmacro item (id label &body body)
  (let ((item (gensym "item"))
        (old-item (gensym "old-item"))
        (bindings (gensym "bindn"))
        (binding (gensym "bind")))
    `(progn
       (when (and *items* (gethash ,id *items*))
         (log:info "Cleaning old item: " ,id)
         (let ((,old-item (gethash ,id *items*)))
           (item:destroy ,old-item)
           (remhash ,id *items*)))
       (let ((,item (item:make-item ,id ,label))
             (,bindings (list ,@body)))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding))
         (setf (gethash ,id *items*) ,item)))))

(defmacro binding (&rest args)
  `(binding:make-function-binding ,@args))

(defmacro rule (name &rest args)
  (let ((rule (gensym "rule"))
        (old-rule (gensym "old-rule")))
    `(progn
       (when (and *rules* (gethash ,name *rules*))
         (log:info "Cleaning old rule: " ,name)
         (let ((,old-rule (gethash ,name *rules*)))
           (ac:stop (act:context ,old-rule) ,old-rule :wait t)
           (remhash ,name *rules*)))
       (let ((,rule (rule:make-rule ,name ,@args)))
         (setf (gethash ,name *rules*) ,rule)))))

(defun shutdown ()
  (envi:shutdown-env)
  (clrhash *items*)
  (clrhash *rules*))
