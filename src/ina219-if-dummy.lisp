(defpackage :cl-eta.ina219-if
  (:use :cl)
  (:nicknames :ina219-if)
  (:export #:init
           #:read-currency))

(in-package :cl-eta.ina219-if)

(defun init ()
  (values :ok))

(defun read-currency ()
  (values :ok 0.1))
