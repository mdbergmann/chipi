(defpackage :cl-eta.eta
  (:use :cl :gs-user)
  (:nicknames :eta)
  (:export #:init-serial
           #:close-serial
           #:start-record
           #:stop-record
           #:ensure-initialized
           #:ensure-shutdown
           #:*serial-proxy-impl*))

(in-package :cl-eta.eta)

(log:config '(cl-gserver) :warn)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy-impl* nil)

(defvar +new-empty-data+ #())

(defun ensure-initialized ()
  (unless *serial-proxy-impl*
    (setf *serial-proxy-impl* :prod))
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA-serial-actor"
                                      :state +new-empty-data+
                                      :receive (lambda (self msg state)
                                                 (%serial-actor-receive self msg state)))))
  (values *serial-actor* *actor-system*))

(defun ensure-shutdown ()
  (when *actor-system*
    (ac:shutdown *actor-system* :wait t))
  (setf *actor-system* nil)
  (setf *serial-actor* nil)
  (setf *serial-proxy-impl* nil))

;; ---------------------
;; public functions
;; ---------------------

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

(defun close-serial ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (let ((ask-result (act:ask-s actor '(:close . nil))))
      (cond
        ((listp ask-result)
         (case (car ask-result)
           (:handler-error (values :fail (format nil "~a" (cdr ask-result))))
           (otherwise (values :ok))))
        (t (values :ok))))))

(defun start-record ()
  "Triggers the recording of data.
Once this command is sent, the ETA will start to send monitor data packages.
So we gotta trigger a read here as well."
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor `(:write . ,(eta-pkg:new-start-record-pkg)))
    (act:tell actor '(:read . nil)))
  :ok)

(defun stop-record ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor `(:write . ,(eta-pkg:new-stop-record-pkg))))
  :ok)

;; ---------------------
;; actor handling
;; ---------------------

(defun %process-complete-pkg (pkg-data)
  (multiple-value-bind (pkg-type items)
      (eta-pkg:extract-pkg pkg-data)
    (case pkg-type
      (:fail (log:warn "Failed package extraction: ~a" pkg-data))
      (:eta-monitor (progn
                      (log:info "Monitor data: ~a" pkg-data)
                      (dolist (item items)
                        (log:info "Posting item: ~a, value: ~a" (car item) (cdr item))
                        (openhab:do-post (car item) (cdr item)))))
      (otherwise (log:info "Unknown extract pkg result!")))))

(defun %handle-init (state)
  (cons
   (setf *serial-port*
         (eta-ser-if:open-serial *serial-proxy-impl* *serial-device*))
   state))

(defun %handle-close (state)
  (cons
   (eta-ser-if:close-serial *serial-proxy-impl* *serial-port*)
   state))

(defun %handle-write (data state)
  (cons (eta-ser-if:write-serial *serial-proxy-impl* *serial-port* data) state))

(defun %handle-read (actor state)
  (let ((new-state
          (unwind-protect
               (multiple-value-bind (complete data)
                   (eta-pkg:collect-data
                    state
                    (eta-ser-if:read-serial *serial-proxy-impl* *serial-port*))
                 (if complete
                     (progn 
                       (%process-complete-pkg data)
                       +new-empty-data+)
                     data))
            (progn
              (log:warn "Error collecting data!")
              state))))
    (act:tell actor '(:read . nil))
    (cons t new-state)))

(defun %serial-actor-receive (self msg state)
  (let ((resp
          (case (car msg)
            (:init (%handle-init state))
            (:close (%handle-close state))
            (:write (%handle-write (cdr msg) state))
            (:read (%handle-read self state)))))
    resp))
