(defpackage :cl-eta.openhab
  (:use :cl)
  (:nicknames :openhab)
  (:export #:do-post))

(in-package :cl-eta.openhab)

(defgeneric do-post (impl data))

(defmethod do-post ((impl (eql :prod)) data))
