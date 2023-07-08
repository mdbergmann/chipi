(defpackage :cl-hab.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:defconfig
           #:item
           #:binding
           #:defrules
           #:rule
           #:shutdown)
  )

(in-package :cl-hab.hab)

(defvar *items* nil "All items")
(defvar *rules* nil "All rules")

(defmacro defconfig (&body body)
  `(progn
     (envi:ensure-env)
     (when (null *items*)
       (setf *items* (make-hash-table)))
     (when (null *rules*)
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
           (ac:stop (act:context ,old-item) ,old-item :wait t)
           (remhash ,id *items*)))
       (let ((,item (item:make-item ,id ,label))
             (,bindings (list ,@body)))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding))
         (setf (gethash ,id *items*) ,item)))))

(defmacro binding (&rest keys)
  `(binding:make-function-binding ,@keys))

(defmacro defrules (&body body)
  (let ((rules (gensym "rules")))
    `(let ((,rules (list ,@body)))
       (setf *rules* (alexandria:alist-hash-table ,rules :test #'equalp)))))

(defmacro rule (name &rest keys)
  (let ((rule (gensym "rule")))
    `(let ((,rule (rule:make-rule ,name ,@keys)))
       (cons ,name ,rule))))

(defun shutdown ()
  (envi:shutdown-env)
  (clrhash *items*)
  (clrhash *rules*))
