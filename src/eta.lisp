(defpackage :cl-eta.eta
  (:use :cl :gs-user :eta-ser-if)
  (:nicknames :eta)
  (:export #:init-serial
           #:start-record
           #:ensure-initialized
           #:ensure-shutdown
           #:*serial-proxy-impl*))

(in-package :cl-eta.eta)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy-impl* nil)

(defun ensure-initialized ()
  (unless *serial-proxy-impl*
    (setf *serial-proxy-impl* :prod))
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA-serial-actor"
                                      :state (%new-empty-data)
                                      :receive (lambda (self msg state)
                                                 (%serial-actor-receive self msg state)))))
  (values *serial-actor* *actor-system*))

(defun ensure-shutdown ()
  (when *actor-system*
    (ac:shutdown *actor-system* :wait t))
  (setf *actor-system* nil)
  (setf *serial-actor* nil)
  (setf *serial-proxy-impl* nil))

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
    (act:tell actor `(:write . ,(eta-pkg:new-start-record-pkg)))
    (act:tell actor '(:read . nil)))
  :ok)

;; ---------------------
;; actor receive
;; ---------------------

(defun %new-empty-data () #())

(defun %process-complete-pkg (pkg-data)
  (multiple-value-bind (pkg-type items)
      (eta-pkg:extract-pkg pkg-data)
    (case pkg-type
      (:fail (lf:lwarn "Failed package extraction: ~a" pkg-data))
      (:eta-monitor (progn
                      (lf:linfo "Monitor data: ~a" pkg-data)
                      (dolist (item items)
                        (openhab:do-post (car item) (cdr item)))))
      (otherwise (lf:linfo "Unknown extract pkg result!")))))

(defun %handle-init (state)
  (cons
   (setf *serial-port*
         (open-serial *serial-proxy-impl* *serial-device*))
   state))

(defun %handle-write (data state)
  (cons (write-serial *serial-proxy-impl* *serial-port* data) state))

(defun %handle-read (actor state)
  (let ((new-state
          (multiple-value-bind (complete data)
              (eta-pkg:collect-data state
                                    (read-serial *serial-proxy-impl* *serial-port*))
            (if complete
                (progn 
                  (%process-complete-pkg data)
                  (%new-empty-data))
                data))))
    (act:tell actor '(:read . nil))
    (cons t new-state)))

(defun %serial-actor-receive (self msg state)
  (let ((resp
          (case (car msg)
            (:init (%handle-init state))
            (:write (%handle-write (cdr msg) state))
            (:read (%handle-read self state)))))
    resp))
