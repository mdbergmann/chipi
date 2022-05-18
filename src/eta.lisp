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
(defvar *serial-proxy* nil "Public, able to `change-class' for tests.")

(defun ensure-initialized ()
  (unless *serial-proxy*
    (setf *serial-proxy* (make-real-serial-proxy)))
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA-serial-actor"
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
    (let ((ask-result (act:ask-s actor '(:init . nil))))
      (cond
        ((listp ask-result)
         (case (car ask-result)
           (:handler-error (values :fail (format nil "~a" (cdr ask-result))))
           (otherwise (values :ok))))
        (t (values :ok))))))

;; ---------------------
;; package functions
;; ---------------------

(defun start-record ()
  "Triggers the recording of data.
Once this command is sent, the ETA will start to send monitor data packages.
So we gotta trigger a read here as well."
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor '(:write . "Foo"))
    (act:tell actor '(:read . nil)))
  :ok)

;; ---------------------
;; actor receive
;; ---------------------

(defun %serial-actor-receive (self msg state)
  (declare (ignore self))
  (let ((resp (case (car msg)
                (:init
                 (setf *serial-port*
                       (open-serial *serial-proxy* *serial-device*)))
                (:write
                 (write-serial *serial-proxy* *serial-port* (cdr msg)))
                (:read
                 (read-serial *serial-proxy* *serial-port*)))))
    (cons resp state)))
