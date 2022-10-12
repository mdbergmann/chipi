(defpackage :cl-eta.eta
  (:use :cl :sento-user)
  (:nicknames :eta)
  (:export #:init-serial
           #:close-serial
           #:start-record
           #:stop-record
           #:report-avgs
           #:ensure-initialized
           #:ensure-shutdown
           #:make-jobdefinition
           #:*serial-proxy-impl*))

(in-package :cl-eta.eta)

(log:config '(sento) :warn)

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy-impl* nil)

(defvar +new-empty-data+ #())

(defparameter *avg-items*
  '(("HeatingETAOperatingHours" . (("HeatingETAOpHoursPerWeek" .
                                    (:m 0
                                     :h 0
                                     :dow 0
                                     :name heating-eta-op-hours-per-week))))
    ("HeatingETAIgnitionCount" . (("HeatingETAIgCountPerDay" .
                                   (:m 0
                                    :h 0
                                    :name heating-eta-ig-count-per-day))))))

;; this should be part of actor state
(defstruct actor-state
  (serial-data +new-empty-data+)
  (avgs nil)
  (do-read-p nil))

(defstruct avg-record
  (initial-value nil)
  (current-value nil)
  (initial-time nil)
  (current-time nil)
  (cadence-name nil))

;; ---------------------
;; public functions
;; ---------------------

(defun ensure-initialized ()
  (unless *serial-proxy-impl*
    (setf *serial-proxy-impl* :prod))
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system)))
  (unless *serial-actor*
    (setf *serial-actor* (ac:actor-of *actor-system*
                                      :name "ETA-serial-actor"
                                      :state (make-actor-state)
                                      :receive (lambda (self msg state)
                                                 (%serial-actor-receive self msg state))
                                      :init (lambda (self)
                                              (declare (ignore self))
                                              (%init-actor))
                                      :destroy (lambda (self)
                                                 (declare (ignore self))
                                                 (%destroy-actor)))))
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
    (act:tell actor '(:start-read . nil)))
  :ok)

(defun stop-record ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:ask-s actor '(:stop-read . nil))
    (act:ask-s actor `(:write . ,(eta-pkg:new-stop-record-pkg))))
  :ok)

(defun get-state ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:ask-s actor '(:state . nil))))

(defun report-avgs (avg-to-report)
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor `(:report-avgs . ,avg-to-report)))
  :ok)

(defun make-jobdefinition (fun time-def)
  (cron:make-cron-job fun
                      :minute (getf time-def :m :every)
                      :hour (getf time-def :h :every)
                      :day-of-month (getf time-def :dom :every)
                      :day-of-week (getf time-def :dow :every)
                      :hash-key (getf time-def :name)))

;; ---------------------
;; internal functions
;; ---------------------

(defun %init-actor ()
  (clrhash cron::*cron-jobs-hash*)
  (dolist (item *avg-items*)
    (let ((cadences (cdr item)))
      (dolist (cadence cadences)
        (let ((cadence-name (car cadence))
              (cadence-timedef (cdr cadence)))
          (make-jobdefinition
           (lambda () (report-avgs cadence-name))
           cadence-timedef)))))
  (cron:start-cron))

(defun %destroy-actor ()
  (clrhash cron::*cron-jobs-hash*)
  (cron::stop-cron))

(defun %%find-avg-mon-items (mon-items)
  "Finds monitor items where avg definitions exist in `*avg-items*'."
  (utils:filter (lambda (mitem)
                  (member (car mitem) *avg-items* :key #'car :test #'string=))
                mon-items))

(defun %%find-cadences (mon-item)
  "Finds cadences for the given monitor item in `*avg-items*'."
    (let* ((avg-item (find (car mon-item) *avg-items* :key #'car :test #'string=))
           (cadences (cdr avg-item)))
      cadences))

(defun %process-complete-pkg (pkg-data)
  "Transmits monitor items to openhab.
Returns monitor items."
  (multiple-value-bind (pkg-type items)
      (eta-pkg:extract-pkg pkg-data)
    (case pkg-type
      (:fail (progn
               (log:warn "Failed package extraction: ~a" pkg-data)
               nil))
      (:eta-monitor (progn
                      (log:info "Monitor data: ~a" pkg-data)
                      (dolist (item items)
                        (log:info "Posting item: ~a, value: ~a" (car item) (cdr item))
                        (openhab:do-post (car item) (cdr item)))
                      items))
      (otherwise (progn
                   (log:info "Unknown extract pkg result!")
                   nil)))))

(defun %%make-new-avg (mitem-val cadence-name old-avgs)
  (let* ((old-avg
           (find-if (lambda (avg)
                      (string= cadence-name (avg-record-cadence-name avg)))
                    old-avgs))
         (new-avg (if old-avg
                      old-avg
                      (make-avg-record
                       :initial-value mitem-val
                       :initial-time (get-universal-time)
                       :cadence-name cadence-name))))
    (setf (avg-record-current-value new-avg) mitem-val)
    (setf (avg-record-current-time new-avg) (get-universal-time))
    new-avg))

(defun %process-avgs (mon-items avgs)
  "Calculates new avgs for monitor items."
  (log:debug "Mon items: ~a" mon-items)
  (log:debug "Old avgs: ~a" avgs)
  (let* ((mitems
           (mapcar (lambda (mitem)
                     `(,mitem . ,(%%find-cadences mitem)))
                   (%%find-avg-mon-items mon-items)))
         (new-avgs
           (loop :for mitem-with-cadences :in mitems
                 :for mitem = (car mitem-with-cadences)
                 :for cadences = (cdr mitem-with-cadences)
                 :for new-avg = (mapcar (lambda (cadence)
                                          (%%make-new-avg (cdr mitem) (car cadence) avgs))
                                        cadences)
                 :append new-avg)))
    (log:debug "New avgs: ~a" new-avgs)
    new-avgs))

(defun %calculate-avg (avg-item)
  "Calculates the avg per day of the given avg."
  (let ((day-in-secs (* 24 60 60))
        (name (avg-record-cadence-name avg-item))
        (ival (avg-record-initial-value avg-item))
        (cval (avg-record-current-value avg-item))
        (itime (avg-record-initial-time avg-item))
        (ctime (avg-record-current-time avg-item)))
    (let* ((time-diff-secs (- ctime itime))
           (time-diff-days (float (/ time-diff-secs day-in-secs)))
           (diff-value (- cval ival))
           (per-day (/ diff-value time-diff-days)))
      `(,name . ,per-day))))

;; ---------------------
;; actor handling
;; ---------------------

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
  (let ((serial-data (actor-state-serial-data state))
        (avgs (actor-state-avgs state)))
    (let ((new-serial-data
            (handler-case
                (multiple-value-bind (complete data)
                    (eta-pkg:collect-data
                     serial-data
                     (eta-ser-if:read-serial *serial-proxy-impl* *serial-port*))
                  (if complete
                      (let* ((mon-items (%process-complete-pkg data))
                             (new-avgs (%process-avgs mon-items avgs)))
                        (setf (actor-state-avgs state) new-avgs)
                        +new-empty-data+)
                      data))
              (error (c)
                (progn
                  (log:warn "Error collecting data: ~a" c)
                  state)))))
      (setf (actor-state-serial-data state) new-serial-data)
      (when (actor-state-do-read-p state)
        (act:tell actor '(:read . nil)))
      (cons t state))))

(defun %handle-start-read (actor state)
  (act:tell actor '(:read . nil))
  (setf (actor-state-do-read-p state) t)
  (cons t state))

(defun %handle-stop-read (state)
  (setf (actor-state-do-read-p state) nil)
  (cons t state))

(defun %handle-get-state (state)
  (cons state state))

(defun %handle-report-avgs (state avg-to-report)
  (let* ((avgs (actor-state-avgs state))
         (filtered (utils:filter (lambda (avg)
                                   (string= (avg-record-cadence-name avg) avg-to-report))
                                 avgs))
         (calculated (mapcar #'%calculate-avg filtered)))
    (flet ((remove-avg (avg-name)
             (delete-if (lambda (avg)
                          (string= avg-name (avg-record-cadence-name avg)))
                        avgs)))
      (dolist (calc calculated)
        (log:info "Posting avg:~a" calc)
        (openhab:do-post (car calc) (cdr calc))
        (setf avgs (remove-avg (car calc)))))
    (cons t state)))

(defun %serial-actor-receive (self msg state)
  (let ((resp
          (case (car msg)
            (:init (%handle-init state))
            (:close (%handle-close state))
            (:write (%handle-write (cdr msg) state))
            (:read (%handle-read self state))
            (:start-read (%handle-start-read self state))
            (:stop-read (%handle-stop-read state))
            (:state (%handle-get-state state))
            (:report-avgs (%handle-report-avgs state (cdr msg))))))
    ;;(format t "msg:~a, resp:~a~%" msg resp)
    resp))
