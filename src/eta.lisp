(defpackage :cl-eta.eta
  (:use :cl :sento-user)
  (:nicknames :eta)
  (:export #:init-serial
           #:close-serial
           #:start-record
           #:stop-record
           #:ensure-initialized
           #:ensure-shutdown
           #:*serial-proxy-impl*))

(in-package :cl-eta.eta)

(log:config '(sento) :warn)

(defvar *start-time* (get-universal-time))

(defvar *actor-system* nil)
(defvar *serial-actor* nil)
(defvar *serial-device* nil)
(defvar *serial-port* nil)
(defvar *serial-proxy-impl* nil)

(defvar +new-empty-data+ #())

(defvar *avg-items* nil)

;; this should be part of actor state
(defstruct actor-state
  (serial-data +new-empty-data+)
  (avgs nil)
  (do-read-p nil))

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
    (act:tell actor '(:start-read . nil)))
  :ok)

(defun stop-record ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:tell actor '(:stop-read . nil))
    (act:tell actor `(:write . ,(eta-pkg:new-stop-record-pkg))))
  :ok)

(defun get-state ()
  (multiple-value-bind (actor)
      (ensure-initialized)
    (act:ask-s actor '(:state . nil))))

;; ---------------------
;; actor handling
;; ---------------------

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

(defun %%find-avg-mon-items (mon-items)
  "Finds monitor items where avg definitions exist in `*avg-items*'."
  (utils:filter (lambda (mitem)
                  (member (car mitem) *avg-items* :key #'car))
                mon-items))

(defun %%find-cadences (mon-item)
  "Finds cadences for the given monitor item in `*avg-items*'."
    (let* ((avg-item (find (car mon-item) *avg-items* :key #'car))
           (cadences (cdr avg-item)))
      cadences))

(defun %%make-new-avg (mon-item-val cadence old-avgs)
  "Makes a new 'avg' item taking existing 'avg' item value and new monitor value into account.
Returns alist of cadence name and new avg value."
  (let* ((cadence-name (car cadence))
         (curr-avg (find cadence-name old-avgs :key #'car))
         (curr-avg-val (if curr-avg
                           (cdr curr-avg)
                           0)))
    `(,cadence-name . ,(if (= 0 curr-avg-val)
                           mon-item-val
                           (/ (+ curr-avg-val mon-item-val) 2.0)))))

(defun %%make-new-avgs (mon-items old-avgs)
  "Returns a list of alists cadence-name and avg value in each entry."
  (car
   (let* ((mitems
            (mapcar (lambda (mitem)
                      `(,mitem . ,(%%find-cadences mitem)))
                    (%%find-avg-mon-items mon-items)))
          (new-avgs
            (mapcar (lambda (mitem-with-cadences)
                      (let* ((mitem (car mitem-with-cadences))
                             (cadences (cdr mitem-with-cadences))
                             (new-avg
                               (mapcar (lambda (cadence)
                                         (%%make-new-avg (cdr mitem) cadence old-avgs))
                                       cadences)))
                        new-avg))
                    mitems)))
     new-avgs)))

(defun %process-avgs (mon-items avgs)
  "Calculates new avgs for monitor items."
  (%%make-new-avgs mon-items avgs))

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
  (let ((new-state (copy-structure state)))
    (let ((new-serial-data
            (handler-case
                (multiple-value-bind (complete data)
                    (eta-pkg:collect-data
                     (actor-state-serial-data new-state)
                     (eta-ser-if:read-serial *serial-proxy-impl* *serial-port*))
                  (if complete
                      (let* ((mon-items (%process-complete-pkg data))
                             (new-avgs (%process-avgs mon-items (actor-state-avgs new-state))))
                        (setf (actor-state-avgs new-state) new-avgs)
                        +new-empty-data+)
                      data))
              (error (c)
                (progn
                  (log:warn "Error collecting data: ~a" c)
                  new-state)))))
      (setf (actor-state-serial-data new-state) new-serial-data)
      (when (actor-state-do-read-p new-state)
        (act:tell actor '(:read . nil)))
      (cons t new-state))))

(defun %handle-start-read (actor state)
  (act:tell actor '(:read . nil))
  (let ((new-state (copy-structure state)))
    (setf (actor-state-do-read-p new-state) t)
    (cons t new-state)))

(defun %handle-stop-read (state)
  (let ((new-state (copy-structure state)))
    (setf (actor-state-do-read-p new-state) nil)
    (cons t new-state)))

(defun %handle-get-state (state)
  (cons state state))

(defun %serial-actor-receive (self msg state)
  (let ((resp
          (case (car msg)
            (:init (%handle-init state))
            (:close (%handle-close state))
            (:write (%handle-write (cdr msg) state))
            (:read (%handle-read self state))
            (:start-read (%handle-start-read self state))
            (:stop-read (%handle-stop-read state))
            (:state (%handle-get-state state)))))
    resp))
