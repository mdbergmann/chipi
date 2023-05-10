(defpackage :cl-eta.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:defitems
           #:item)
  )

(in-package :cl-eta.hab)

(defvar *items* nil "All items")

(defmacro defitems (&body body)
  (let ((items (gensym "items")))
    `(progn
       (setf ,items (list ,@body))
       (setf *items* (alexandria:alist-plist ,items)))))

(defmacro item (id label)
  (let ((item (gensym "item")))
    `(progn
       (setf ,item (item:make-item ,id ,label))
       (cons ,id ,item))))
