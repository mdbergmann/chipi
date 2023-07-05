(defpackage :cl-eta.rule
  (:use :cl)
  (:nicknames :rule)
  (:import-from #:envi
                #:ensure-isys)
  (:import-from #:act
                #:actor)
  (:export #:rule
           #:make-rule
           ))

(in-package :cl-eta.rule)

(defclass rule (actor) ())

(defun make-rule (name &rest keys)
  (let* ((isys (ensure-isys))
         (rule (ac:actor-of
                isys
                :name name
                :type 'rule
                :receive (lambda (msg)
                           (log:debug "Received msg: " msg)))))
    rule))
