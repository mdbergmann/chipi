(defpackage :cl-eta.collector
  (:use :cl)
  (:nicknames :eta-col)
  (:export #:collect-data))

(in-package :cl-eta.collector)

(defmethod collect-data ((impl (eql :prod)) prev-data new-data) nil)

