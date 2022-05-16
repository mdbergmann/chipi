(defpackage :cl-eta.openhab
  (:use :cl)
  (:nicknames :openhab)
  (:export #:do-post))

(in-package :cl-eta.openhab)

(defun do-post (url data)
  (declare (ignore url data))
  nil)
