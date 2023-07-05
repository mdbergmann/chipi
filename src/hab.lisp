(defpackage :cl-eta.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:defconfig
           #:defitems
           #:item
           #:binding
           #:shutdown)
  )

(in-package :cl-eta.hab)

(defvar *items* nil "All items")
(defvar *rules* nil "All rules")

(defmacro defconfig (&body body)
  `(progn
     (envi:ensure-env)
     ,@body))

(defmacro defitems (&body body)
  (let ((items (gensym "items")))
    `(let ((,items (list ,@body)))
       (setf *items* (alexandria:alist-hash-table ,items)))))

(defmacro item (id label &body body)
  (let ((item (gensym "item"))
        (bindings (gensym "bindn"))
        (binding (gensym "bind")))
    `(let ((,item (item:make-item ,id ,label))
           (,bindings (list ,@body)))
       (dolist (,binding ,bindings)
         (item:add-binding ,item ,binding))
       (cons ,id ,item))))

(defmacro binding (&rest keys)
  `(binding:make-function-binding ,@keys))

(defmacro defrules (&body body)
  (let ((rules (gensym "rules")))
    `(let ((,rules (list ,@body)))
       (setf *rules* (alexandria:alist-hash-table ,rules)))))

(defmacro rule (name &rest keys)
  (let ((rule (gensym "rule")))
    `(let ((,rule (rule:make-rule ,name ,@keys)))
       (cons ,name ,rule))))

(defun shutdown ()
  (envi:shutdown-env)
  (clrhash *items*)
  (clrhash *rules*))
