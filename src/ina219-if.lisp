(defpackage :cl-eta.ina219-if
  (:use :cl :py4cl)
  (:nicknames :ina219-if)
  (:export #:init
           #:read-currency))

(in-package :cl-eta.ina219-if)

(defparameter *shunt-ohm* 0.1)
(defparameter *max-currency* 0.4)

(defvar *ina-range-16v* nil)
(defvar *ina-gain-1-40mv* nil)

(defvar *ina* nil)

(defun init ()
  (import-module "ina219" :as "ina219")

  (setf *ina-range-16v* (python-eval "ina219.INA219.RANGE_16V")
        *ina-gain-1-40mv* (python-eval "ina219.INA219.GAIN_1_40MV")
        *ina* (python-call "ina219.INA219" *shunt-ohm* *max-currency*))
  (python-method *ina* 'configure *ina-range-16v* *ina-gain-1-40mv*)

  (values :ok))

(defun %next-currency-value (times)
  (/ 
   (reduce #'+ (loop :repeat times
                     :collect
                     (python-method *ina* 'current))
           :initial-value 0)
   times))

(defun read-currency ()
  (let ((currency (%next-currency-value 10)))
    (values :ok currency)))
