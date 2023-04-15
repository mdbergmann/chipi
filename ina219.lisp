(py4cl:import-module "ina219" :as "ina219")

(defparameter *shunt-ohm* 0.1)
(defparameter *max-currency* 0.4)

(defparameter *ina-range-16v* (py4cl:python-eval "ina219.INA219.RANGE_16V"))
(defparameter *ina-gain-1-40mv* (py4cl:python-eval "ina219.INA219.GAIN_1_40MV"))

(defvar *ina* (py4cl:python-call "ina219.INA219" *shunt-ohm* *max-currency*))

(py4cl:python-method *ina* 'configure *ina-range-16v* *ina-gain-1-40mv*)

(defun next-currency-value (times)
  (/ 
   (reduce #'+ (loop :repeat times
                     :collect
                     (py4cl:python-method *ina* 'current))
           :initial-value 0)
   times))

