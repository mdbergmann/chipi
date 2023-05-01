(defpackage :cl-eta.eta
  (:use :cl :sento-user)
  (:nicknames :eta)
  (:import-from #:act
                #:*state*
                #:*self*
                #:!)
  (:export #:eta-init
           #:eta-stop
           #:eta-close-serial
           #:eta-start-record
           #:eta-stop-record
           #:eta-report-avgs
           #:eta-make-jobdefinition
           #:*eta-serial-proxy-impl*
           #:*eta-serial-device*
           #:ina-init
           #:ina-stop
           #:ina-start-read
           #:*ina-read-delay-sec*
           #:solar-init
           #:solar-stop
           #:solar-start-read
           #:*solar-read-delay-sec*
           #:ensure-initialized
           #:ensure-shutdown
           #:cron-init
           #:cron-start
           #:cron-stop
           #:init-all
           #:stop-all))

(in-package :cl-eta.eta)

(log:config :info :sane :this-console :daily "logs/app.log")
(log:config '(sento) :warn)

(defvar *actor-system* nil)

;; ---------------------
;; common internal functions
;; ---------------------

(defun %persist-actor-state (state filename)
  (log:debug "Persisting state: ~a, to: ~a" state filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (print state stream)))

(defun %read-actor-state (filename)
  (with-open-file (stream filename)
    (read stream)))

;; ---------------------
;; eta-serial
;; ---------------------

(defvar *eta-actor* nil)
(defvar *eta-serial-device* "/dev/ttyUSB0")
(defvar *eta-serial-port* nil)
(defvar *eta-serial-proxy-impl* nil)
(defvar *eta-state-file* #P"eta-actor-state")
(defvar +eta-new-empty-data+ #())

(defparameter *eta-avg-items*
  '(("HeatingETAOperatingHours" . (("HeatingETAOpHoursPerWeek" .
                                    (:m 0
                                     :h 0
                                     :dow 0
                                     :name heating-eta-op-hours-per-week))))
    ("HeatingETAIgnitionCount" . (("HeatingETAIgCountPerDay" .
                                   (:m 0
                                    :h 0
                                    :dow 0
                                    :name heating-eta-ig-count-per-day)))))
  "Items that report averages. Scheduling definitions:
`:m' minutes
`:h' hour
`:dow' day of week
`:name' name of scheduler entry")

;; this should be part of actor state
(defstruct eta-actor-state
  (serial-data +eta-new-empty-data+)
  (avgs nil)
  (do-read-p nil))

(defstruct eta-avg-record
  "An average record.
The first value reading will fill `initial-value' and `initial-time'.
Subsequent value reads will fill `current-value' and `current-time'.
When time is due to report (see `*eta-avg-items*')
then average values will be calculated, see `%calculate-avg'.
`cadence-name' must match caadr of `*eta-avg-items*'."
  (initial-value nil)
  (current-value nil)
  (initial-time nil)
  (current-time nil)
  (cadence-name nil))

;; Public functions

(defun eta-init ()
  "High level start function. Counterpart to `eta-stop'."
  (multiple-value-bind (asys)
      (ensure-initialized)
    (unless *eta-serial-proxy-impl*
      (setf *eta-serial-proxy-impl* :prod))
    (unless *eta-actor*
      (setf *eta-actor*
            (ac:actor-of asys
                         :name "ETA-serial-actor"
                         :dispatcher :shared
                         :state (make-eta-actor-state)
                         :receive (lambda (msg)
                                    (%eta-serial-actor-receive msg))
                         :init (lambda (self)
                                 (declare (ignore self))
                                 (%eta-actor-init))
                         :destroy (lambda (self)
                                    (declare (ignore self))
                                    (%eta-actor-destroy)))))
    (let ((ask-result (act:ask-s *eta-actor* '(:init . nil))))
      (cond
        ((listp ask-result)
         (case (car ask-result)
           (:handler-error (values :fail (format nil "~a" (cdr ask-result))))
           (otherwise (values :ok))))
        (t (values :ok))))))

(defun eta-stop ()
  "High level stop function. Counterpart to `eta-start'."
  (when *eta-serial-device*
    (eta-close-serial)
    (setf *eta-serial-device* nil))
  (when *eta-actor*
    (ac:stop (act:context *eta-actor*) *eta-actor*)
    (setf *eta-actor* nil))
  (values :ok))

(defun eta-close-serial ()
  (let ((ask-result (act:ask-s *eta-actor* '(:close . nil))))
    (cond
      ((listp ask-result)
       (case (car ask-result)
         (:handler-error (values :fail (format nil "~a" (cdr ask-result))))
         (otherwise (progn
                      (setf *eta-serial-device* nil)
                      (values :ok)))))
      (t (values :ok)))))

(defun eta-start-record ()
  "Triggers the recording of data.
Once this command is sent, the ETA will start to send monitor data packages.
So we gotta trigger a read here as well."
  (! *eta-actor* `(:write . ,(eta-pkg:new-start-record-pkg)))
  (! *eta-actor* '(:start-read . nil))
  :ok)

(defun eta-stop-record ()
  (! *eta-actor* '(:stop-read . nil))
  (act:ask-s *eta-actor* `(:write . ,(eta-pkg:new-stop-record-pkg)))
  :ok)

(defun eta-get-state ()
  (act:ask-s *eta-actor* '(:state . nil)))

(defun eta-report-avgs (avg-to-report)
  (! *eta-actor* `(:report-avgs . ,avg-to-report))
  :ok)

(defun eta-make-jobdefinition (fun time-def)
  "Creates and registers the cron job."
  (cron:make-cron-job fun
                      :minute (getf time-def :m :every)
                      :hour (getf time-def :h :every)
                      :day-of-month (getf time-def :dom :every)
                      :day-of-week (getf time-def :dow :every)
                      :hash-key (getf time-def :name)))

;; internal functions

(defun %%find-avg-mon-items (mon-items)
  "Finds monitor items where avg definitions exist in `*eta-avg-items*'."
  (miscutils:filter (lambda (mitem)
                      (member (car mitem) *eta-avg-items* :key #'car :test #'string=))
                    mon-items))

(defun %%find-cadences (mon-item)
  "Finds cadences for the given monitor item in `*eta-avg-items*'."
    (let* ((avg-item (find (car mon-item) *eta-avg-items* :key #'car :test #'string=))
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
                      (string= cadence-name (eta-avg-record-cadence-name avg)))
                    old-avgs))
         (new-avg (if old-avg
                      old-avg
                      (make-eta-avg-record
                       :initial-value mitem-val
                       :initial-time (get-universal-time)
                       :cadence-name cadence-name))))
    (setf (eta-avg-record-current-value new-avg) mitem-val)
    (setf (eta-avg-record-current-time new-avg) (get-universal-time))
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
    (log:info "Avgs: ~a" new-avgs)
    new-avgs))

(defun %calculate-avg (avg-item)
  "Calculates the avg per day of the given avg."
  (let ((day-in-secs (* 24 60 60))
        (name (eta-avg-record-cadence-name avg-item))
        (ival (eta-avg-record-initial-value avg-item))
        (cval (eta-avg-record-current-value avg-item))
        (itime (eta-avg-record-initial-time avg-item))
        (ctime (eta-avg-record-current-time avg-item)))
    (let* ((time-diff-secs (- ctime itime))
           (time-diff-days (float (/ time-diff-secs day-in-secs)))
           (diff-value (- cval ival))
           (per-day (/ diff-value time-diff-days)))
      `(,name . ,per-day))))

;; actor handling

(defun %eta-actor-init ()
  (when (uiop:file-exists-p *eta-state-file*)
    (log:info "State file exists, applying it!")
    (setf *state* (%read-actor-state *eta-state-file*)))
  (dolist (item *eta-avg-items*)
    (let ((cadences (cdr item)))
      (dolist (cadence cadences)
        (let ((cadence-name (car cadence))
              (cadence-timedef (cdr cadence)))
          (eta-make-jobdefinition
           (lambda () (eta-report-avgs cadence-name))
           cadence-timedef))))))

(defun %eta-actor-destroy ()
  (setf *eta-actor* nil
        *eta-serial-proxy-impl* nil))

(defun %eta-handle-init ()
  (setf *eta-serial-port*
        (eta-ser-if:open-serial *eta-serial-proxy-impl* *eta-serial-device*)))

(defun %eta-handle-close ()
  (eta-ser-if:close-serial *eta-serial-proxy-impl* *eta-serial-port*))

(defun %eta-handle-write (data)
  (eta-ser-if:write-serial *eta-serial-proxy-impl* *eta-serial-port* data))

(defun %eta-handle-read (actor)
  (tasks:with-context (actor :tasks)
    (tasks:task-async
     (lambda ()
       (eta-ser-if:read-serial *eta-serial-proxy-impl* *eta-serial-port*))
     :on-complete-fun
     (lambda (result)
       (log:debug "on-complete returned: ~a" result)
       (! actor `(:did-read . ,result)))))
  t)

(defun %eta-handle-did-read (actor state read-data)
  (let ((serial-data (eta-actor-state-serial-data state))
        (avgs (eta-actor-state-avgs state)))
    (let ((new-serial-data
            (handler-case
                (multiple-value-bind (complete data)
                    (eta-pkg:collect-data serial-data read-data)
                  (if complete
                      (let* ((mon-items (%process-complete-pkg data))
                             (new-avgs (%process-avgs mon-items avgs)))
                        (setf (eta-actor-state-avgs state) new-avgs)
                        +eta-new-empty-data+)
                      data))
              (error (c)
                (progn
                  (log:warn "Error collecting data: ~a" c)
                  state)))))
      (setf (eta-actor-state-serial-data state) new-serial-data)
      (when (eta-actor-state-do-read-p state)
        (! actor '(:read . nil)))))
  t)

(defun %eta-handle-start-read (actor state)
  (setf (eta-actor-state-do-read-p state) t)
  (! actor '(:read . nil)))

(defun %eta-handle-stop-read (state)
  (setf (eta-actor-state-do-read-p state) nil)
  t)

(defun %eta-handle-get-state (state)
  state)

(defun %eta-handle-report-avgs (state avg-to-report)
  (let* ((avgs (eta-actor-state-avgs state))
         (filtered (miscutils:filter
                    (lambda (avg)
                      (string= (eta-avg-record-cadence-name avg) avg-to-report))
                    avgs))
         (calculated (mapcar #'%calculate-avg filtered)))
    (flet ((remove-avg (avg-name)
             (delete-if (lambda (avg)
                          (string= avg-name (eta-avg-record-cadence-name avg)))
                        avgs)))
      (dolist (calc calculated)
        (log:info "Posting avg:~a" calc)
        (handler-case
            (let ((avg-name (car calc))
                  (avg-value (cdr calc)))
              (openhab:do-post avg-name avg-value)
              (setf avgs (remove-avg avg-name))
              (setf (eta-actor-state-avgs state) avgs))
          (error (c)
            (log:warn "Error sending avgs: ~a" c)))))
    t))

(defun %eta-serial-actor-receive (msg)
  (let* ((state *state*)
         (self *self*)
         (resp
           (case (car msg)
             (:init (%eta-handle-init))
             (:close (%eta-handle-close))
             (:write (%eta-handle-write (cdr msg)))
             (:read (%eta-handle-read self))
             (:did-read (%eta-handle-did-read self state (cdr msg)))
             (:start-read (%eta-handle-start-read self state))
             (:stop-read (%eta-handle-stop-read state))
             (:state (%eta-handle-get-state state))
             (:report-avgs (%eta-handle-report-avgs state (cdr msg))))))
    (%persist-actor-state state *eta-state-file*)
    resp))


;; -----------------------------------
;; ina219 (zisterne pressure) functions
;; -----------------------------------

(defvar *ina-actor* nil)
(defvar *ina-read-delay-sec* 60 "Seconds delay")
(defvar *ina-read-scheduler-thread* nil)
(defvar *openhab-cistern-currency-item* "ZistSensorCurrency")

(defun ina-init ()
  "High level initialization of `ina'."
  (let ((asys (ensure-initialized)))
    (unless *ina-actor*
      (setf *ina-actor*
            (ac:actor-of asys
                         :name "INA219-cistern-actor"
                         :dispatcher :shared
                         :receive (lambda (msg)
                                    (%ina-actor-receive msg))
                         :destroy (lambda (self)
                                    (declare (ignore self))
                                    (%ina-actor-destroy)))))    
    (! *ina-actor* '(:init . nil)))
  (values :ok))

(defun ina-stop ()
  "High level stop of `ina'. Counterpart of `ina-start'."
  (when *ina-actor*
    (ac:stop (act:context *ina-actor*) *ina-actor*)
    (setf *ina-actor* nil))
  (values :ok))

(defun ina-start-read ()
  (ensure-initialized)
  (! *ina-actor* '(:start-read . nil))
  (values :ok))

;; actor handling

(defun %ina-init ()
  (ina219-if:init)
  t)

(defun %ina-start-read ()
  (unless *ina-read-scheduler-thread*
    (log:info "Creating ina-read-scheduler thread")
    (setf *ina-read-scheduler-thread*
          (bt:make-thread (lambda ()
                            (loop
                              (! *ina-actor* '(:read . nil))
                              (sleep *ina-read-delay-sec*)))
                          :name "ina-read-currency-scheduler")))
  t)

(defun %ina-read ()
  (log:debug "Reading ina currency...")
  (multiple-value-bind (stat currency)
      (ina219-if:read-currency)
    (log:info "Reading ina currency...done, value: ~a" currency)
    (case stat
      (:ok
       (if (numberp currency)
           (openhab:do-post *openhab-cistern-currency-item* currency)
           (log:warn "Currency not a number: ~a" currency)))
      (otherwise
       (log:warn "Read of ina not OK, value: ~a" currency))))
  t)

(defun %ina-actor-receive (msg)
  (case (car msg)
    (:init (%ina-init))
    (:start-read (%ina-start-read))
    (:read (%ina-read))))

(defun %ina-actor-destroy ()
  (when *ina-read-scheduler-thread*
    (bt:destroy-thread *ina-read-scheduler-thread*)
    (setf *ina-read-scheduler-thread* nil))
  (setf *ina-actor* nil))

;; -----------------------------------
;; ina219 (zisterne pressure) functions
;; -----------------------------------

(defvar *solar-actor* nil)
(defvar *solar-read-delay-sec* 30 "Seconds delay")
(defvar *solar-read-scheduler-thread* nil)
(defparameter *solar-totals-cron-key* 'solar-totals-daily)
(defparameter *solar-state-file* #P"solar-actor-state")
(defparameter *openhab-solar-power-item* "SolarPowerMom")
(defparameter *openhab-solar-power-day-total-item* "SolarPowerTotalDay")


(defstruct solar-state
  (total-wh 0 :type integer))

(defun solar-init ()
  (let ((asys (ensure-initialized)))
    (unless *solar-actor*
      (setf *solar-actor*
            (ac:actor-of asys
                         :name "solar-actor"
                         :dispatcher :shared
                         :state (make-solar-state)
                         :receive (lambda (msg)
                                    (%solar-actor-receive msg))
                         :init (lambda (self)
                                 (%solar-actor-init self))
                         :destroy (lambda (self)
                                    (declare (ignore self))
                                    (%solar-actor-destroy)))))
    (! *solar-actor* '(:init . nil)))
  (values :ok))

(defun solar-stop ()
  (when *solar-actor*
    (ac:stop (act:context *solar-actor*) *solar-actor*))
  (values :ok))

(defun solar-start-read ()
  (ensure-initialized)
  (! *solar-actor* '(:start-read . nil))
  (values :ok))

;; internal functions

(defmacro %%read-solar-power ((stat power total) pred &body body)
  `(progn
     (log:debug "Reading solar...")
     (multiple-value-bind (,stat ,power ,total)
         (solar-if:read-power)
       (log:info "Reading solar...done, value: ~a, total: ~a" ,power ,total)
       (case ,stat
         (:ok
          (if ,pred
              (progn
                ,@body)
              (log:warn "Invalid number!")))
         (otherwise
          (log:warn "Read of solar not OK!"))))
     t))

;; actor handling

(defun %solar-init ()
  t)

(defun %solar-start-read ()
  (unless *solar-read-scheduler-thread*
    (log:info "Creating solar-read-scheduler thread")
    (setf *solar-read-scheduler-thread*
          (bt:make-thread (lambda ()
                            (loop
                              (! *solar-actor* '(:read . nil))
                              (sleep *solar-read-delay-sec*)))
                          :name "solar-read-currency-scheduler")))
  t)

(defun %solar-read ()
  (%%read-solar-power (stat power _total)
                      (and (numberp power) (> power 0))
    (let ((rounded (round power)))
      (openhab:do-post *openhab-solar-power-item* rounded))))

(defun %solar-read-total (state)
  (%%read-solar-power (stat _power total)
                      (and (numberp total) (> total 0))
    (let* ((new-rounded-total (round total))
           (old-total (solar-state-total-wh state))
           (new-daily (- new-rounded-total old-total)))
      (setf (solar-state-total-wh state) new-rounded-total)
      (%persist-actor-state state *solar-state-file*)
      (openhab:do-post *openhab-solar-power-day-total-item* new-daily))))

(defun %solar-actor-receive (msg)
  (case (car msg)
    (:init (%solar-init))
    (:start-read (%solar-start-read))
    (:read (%solar-read))
    (:read-total (%solar-read-total *state*))))

(defun %solar-actor-init (actor)
  (when (uiop:file-exists-p *solar-state-file*)
    (log:info "State file exists, applying it!")
    (setf *state* (%read-actor-state *solar-state-file*)))
  (cron:make-cron-job (lambda ()
                        (! actor '(:read-total . nil)))
                      :minute 50
                      :hour 23
                      :day-of-month :every
                      :day-of-week :every
                      :hash-key *solar-totals-cron-key*))

(defun %solar-actor-destroy ()
  (when *solar-read-scheduler-thread*
    (bt:destroy-thread *solar-read-scheduler-thread*)
    (setf *solar-read-scheduler-thread* nil))
  (setf *solar-actor* nil))

;; ---------------------
;; global init functions
;; ---------------------

(defun ensure-initialized ()
  (unless *actor-system*
    (setf *actor-system* (asys:make-actor-system))
    ;; separate dispatcher for tasks
    (asys:register-dispatcher *actor-system*
                              (disp:make-dispatcher *actor-system*
                                                    :tasks
                                                    :workers 4
                                                    :stragety :round-robin)))
  (values *actor-system*))

(defun ensure-shutdown ()
  (when *actor-system*
    (ac:shutdown *actor-system* :wait t))
  (setf *actor-system* nil))

(defun cron-init ()
  (clrhash cron::*cron-jobs-hash*))

(defun cron-start ()
  (cron:start-cron))

(defun cron-stop ()
  (cron:stop-cron)
  (clrhash cron::*cron-jobs-hash*))

(defun cron-show-jobs ()
  (format t "Cron jobs:~%")
  (flet ((print-cron-job (job)
                         (format nil "~%  m:~a,~%  h:~a,~%  dom:~a,~%  dow:~a,~%  month:~a"
                                 (slot-value job 'cron::minute)
                                 (slot-value job 'cron::hour)
                                 (slot-value job 'cron::day-of-month)
                                 (slot-value job 'cron::day-of-week)
                                 (slot-value job 'cron::month))))
        (maphash (lambda (key val)
                   (format t "- ~a: ~a~%" key (print-cron-job val)))
                 cron::*cron-jobs-hash*)))

(defun init-all ()
  (cron-init)
  (eta-init)
  (ina-init)
  (solar-init)
  (cron-start))

(defun start-read-all ()
  (eta-start-record)
  (ina-start-read)
  (solar-start-read))

(defun stop-all ()
  (cron-stop)
  (eta-stop)
  (ina-stop)
  (solar-stop))

(defun help ()
  (format t "~%Welcome to cl-eta version: ~a~%~%"
          (asdf:component-version (asdf:find-system "cl-eta")))
  (format t "Operations:~%")
  (format t "INIT-ALL                    : Initialize all (asys, cron, actors)~%")
  (format t "STOP-ALL                    : Stops actors and cron, not asys~%")
  (format t "START-READ-ALL              : Starts reading values for all sensors~%")
  (format t "CRON-SHOW-JOBS              : Show cron jobs~%"))
