(defpackage :cl-eta.collector
  (:use :cl)
  (:nicknames :eta-col)
  (:export #:collect-data))

(in-package :cl-eta.collector)

(defgeneric collect-data (impl prev-data new-data))

(defmethod collect-data ((impl (eql :prod)) prev-data new-data) 
  nil)

