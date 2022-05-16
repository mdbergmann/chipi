(defpackage :cl-eta.eta
  (:use :cl :gs-user)
  (:nicknames :eta)
  (:export #:init-serial
           #:start-record
           #:*serial-device*
           #:ensure-shutdown))

(in-package :cl-eta.eta)

(defvar *actor-system* nil)

(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)

(defun ensure-initialized ()
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA serial actor"
                                      :receive (lambda (self msg state)
                                                 (%serial-actor-receive self msg state)))))
  (values *serial-actor* *actor-system*))

(defun ensure-shutdown ()
  (when *actor-system*
    (ac:shutdown *actor-system*)
    (setf *actor-system* nil))
  (when *serial-actor*
    (setf *serial-actor* nil)))

(defun init-serial (device)
  (multiple-value-bind (actor)
      (ensure-initialized)
    (setf *serial-device* device)
    (act:tell actor '(:init . nil)))
  :ok)

(defun start-record ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor '(:write . "Foo")))
  :ok)

;; ---------------------
;; actor receive
;; ---------------------

(defun %serial-actor-receive (self msg state)
  (case (car msg)
    (:init
     (setf *serial-port*
           (libserialport:open-serial-port *serial-device*
                                           :baud 19200
                                           :bits 8
                                           :stopbits 1
                                           :parity :sp-parity-none
                                           :rts :sp-rts-off
                                           :flowcontrol :sp-flowcontrol-none)))
    (:write
     (libserialport:serial-write-data *serial-port* (cdr msg))))
  (cons nil state))
