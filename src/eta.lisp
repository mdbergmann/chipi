(defpackage :cl-eta.eta
  (:use :cl :gs-user :eta-ser-if)
  (:nicknames :eta)
  (:export #:init-serial
           #:start-record
           #:ensure-initialized
           #:ensure-shutdown
           #:*serial-proxy*))

(in-package :cl-eta.eta)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy* nil)

(defun ensure-initialized ()
  (unless *serial-proxy*
    (setf *serial-proxy* (make-real-serial-proxy)))
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
