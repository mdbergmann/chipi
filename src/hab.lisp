(defpackage :cl-eta.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:defconfig
           #:defitems
           #:item
           #:<->
           #:shutdown)
  )

(in-package :cl-eta.hab)

(defvar *items* nil "All items")

(defmacro defconfig (&body body)
  `(progn
     (envi:ensure-env)
     ,@body))

(defmacro defitems (&body body)
  (let ((items (gensym "items")))
    `(progn
       (setf ,items (list ,@body))
       (setf *items* (alexandria:alist-hash-table ,items)))))

(defmacro item (id label &body body)
  (let ((item (gensym "item"))
        (bindings (gensym "bindn"))
        (binding (gensym "bind")))
    `(progn
       (setf ,item (item:make-item ,id ,label))
       (let ((,bindings (list ,@body)))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding)))
       (cons ,id ,item))))

(defmacro <-> (&rest keys)
  `(binding:make-function-binding ,@keys))

(defun shutdown ()
  (envi:shutdown-env)
  (clrhash *items*))
