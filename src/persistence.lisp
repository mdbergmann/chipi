(defpackage :cl-hab.persistence
  (:use :cl)
  (:nicknames :persp)
  (:import-from #:act
                #:actor
                #:!)
  (:export #:persistence
           #:make-persistence
           #:destroy)
  )

(in-package :cl-hab.persistence)

(defclass persistence (actor) ())

(defclass map-persistence (persistence) ())

(defun make-persistence (id type)
  (let ((isys (isys:ensure-isys))
        (type (ccase type
                (:map 'map-persistence))))
    (ac:actor-of isys
                 :type type
                 :receive (lambda (msg)))))

(defun destroy (persistence)
  nil)
