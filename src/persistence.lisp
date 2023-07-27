(defpackage :cl-hab.persistence
  (:use :cl)
  (:nicknames :persp)
  (:export #:persistence
           #:make-persistence
           #:destroy)
  )

(in-package :cl-hab.persistence)

(defclass persistence (actor) ())

(defun make-persistence (id type)
  nil)

(defun destroy (persistence)
  nil)
