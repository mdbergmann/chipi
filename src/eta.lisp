(defpackage :cl-eta.eta
  (:use :cl :gs-user)
  (:nicknames :eta)
  (:export #:init-serial
           #:start-record
           #:ensure-initialized
           #:ensure-shutdown
           #:serial-proxy
           #:*serial-proxy*))

(in-package :cl-eta.eta)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy* nil)

(defun ensure-initialized ()
  (unless *serial-proxy*
    (setf *serial-proxy* (make-instance 'real-serial-proxy)))
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
    (setf *serial-actor* nil))
  (when *serial-proxy*
    (setf *serial-proxy* nil)))

(defun init-serial (device)
  (multiple-value-bind (actor)
      (ensure-initialized)
    (setf *serial-device* device)
    (act:tell actor '(:init . nil)))
  :ok)

;; ---------------------
;; package functions
;; ---------------------

(defun start-record ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor '(:write . "Foo")))
  :ok)

;; ---------------------
;; serial facade
;; ---------------------

(defclass serial-proxy () ())
(defgeneric open-serial (serial-proxy device))
(defgeneric write-serial (serial-proxy port data))

;; ---------------------
;; serial facade -- real
;; ---------------------

(defclass real-serial-proxy (serial-proxy) ())
(defmethod open-serial ((proxy real-serial-proxy) device)
  (declare (ignore proxy))
  (format t "open-serial--real~%")
  (libserialport:open-serial-port device
                                  :baud 19200
                                  :bits 8
                                  :stopbits 1
                                  :parity :sp-parity-none
                                  :rts :sp-rts-off
                                  :flowcontrol :sp-flowcontrol-none))
(defmethod write-serial ((proxy real-serial-proxy) port data)
  (declare (ignore proxy))
  (libserialport:serial-write-data port data))

;; ---------------------
;; actor receive
;; ---------------------

(defun %serial-actor-receive (self msg state)
  (declare (ignore self))
  (case (car msg)
    (:init
     (setf *serial-port*
           (open-serial *serial-proxy* *serial-device*)))
    (:write
     (write-serial *serial-proxy* *serial-port* (cdr msg))))
  (cons nil state))
