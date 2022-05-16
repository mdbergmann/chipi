(defpackage :cl-eta.eta
  (:use :cl :gs-user)
  (:nicknames :eta)
  (:export #:start-record))

(in-package :cl-eta.eta)

(defvar *actor-system*)
(defvar *serial-actor*)

(defvar *serial-device*)
(defvar *serial-port*)

(defun start-record ()
  (libserialport:serial-write-data *serial-port* "Foo")
  :ok)
