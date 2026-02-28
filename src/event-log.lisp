(defpackage :chipi.event-log
  (:use :cl)
  (:nicknames :event-log)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:import-from #:ac
                #:actor-of)
  (:import-from #:act
                #:*state*
                #:*self*)
  (:export #:ensure-event-log
           #:shutdown-event-log))

(in-package :chipi.event-log)

(defvar *event-log* nil)

(defstruct event-log-state
  (log-dir nil)
  (log-file-name "event-log.ndjson")
  (max-file-size (* 10 1024 1024))
  (max-backup-files 5)
  (current-stream nil))

(defun %log-file-path (state)
  (merge-pathnames (event-log-state-log-file-name state)
                   (event-log-state-log-dir state)))

(defun %backup-file-path (state n)
  (merge-pathnames (format nil "~a.~a" (event-log-state-log-file-name state) n)
                   (event-log-state-log-dir state)))

(defun %rotate-files (state)
  "Shift backup files and move current log to .1"
  (let ((max (event-log-state-max-backup-files state)))
    (let ((oldest (%backup-file-path state max)))
      (when (probe-file oldest)
        (delete-file oldest)))
    (loop for i from (1- max) downto 1
          do (let ((src (%backup-file-path state i))
                   (dst (%backup-file-path state (1+ i))))
               (when (probe-file src)
                 (rename-file src dst))))
    (let ((current (%log-file-path state)))
      (when (probe-file current)
        (rename-file current (%backup-file-path state 1))))))

(defun %maybe-rotate (state)
  "Check file size and rotate if needed. Updates current-stream in state."
  (let ((stream (event-log-state-current-stream state)))
    (when (and stream
               (>= (file-position stream)
                    (event-log-state-max-file-size state)))
      (close stream)
      (%rotate-files state)
      (setf (event-log-state-current-stream state)
            (open (%log-file-path state)
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create)))))

(defun %handle-item-changed (event)
  "Write NDJSON line for an item change event."
  (%maybe-rotate *state*)
  (let* ((item (item:item-changed-event-item event))
         (old-value (item:item-changed-event-old-value event))
         (item-state (item:get-item-stateq item))
         (new-value (item:item-state-value item-state))
         (timestamp (item:item-state-timestamp item-state))
         (stream (event-log-state-current-stream *state*)))
    (when stream
      (let ((entry (alexandria:plist-hash-table
                    (list "timestamp" (local-time:format-timestring
                                       nil
                                       (local-time:universal-to-timestamp timestamp))
                          "item" (item:name item)
                          "old_value" (item-ext:item-value-internal-to-ext old-value)
                          "new_value" (item-ext:item-value-internal-to-ext new-value))
                    :test #'equal)))
        (write-string (jzon:stringify entry) stream)
        (terpri stream)
        (force-output stream)))))

(defun %event-log-receive (msg)
  "Handle messages for the event-log actor."
  (cond
    ((typep msg 'item:item-changed-event)
     (%handle-item-changed msg))))

;; --------------------------------
;; public interface
;; --------------------------------

(defun ensure-event-log (&key (max-file-size (* 10 1024 1024))
                              (max-backup-files 5)
                              (log-file-name "event-log.ndjson"))
  "Ensure the event log actor is running. Returns the actor."
  (unless (and *event-log* (act-cell:running-p *event-log*))
    (let* ((log-dir (envi:ensure-runtime-dir "event-log/"))
           (log-path (merge-pathnames log-file-name log-dir))
           (stream (open log-path
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)))
      (setf *event-log*
            (ac:actor-of (isys:ensure-isys)
                         :name "Event log"
                         :init (lambda (self)
                                 (log:info "Starting event log")
                                 (ev:subscribe self self 'item:item-changed-event))
                         :destroy (lambda (self)
                                    (let ((s (event-log-state-current-stream
                                              (act-cell:state self))))
                                      (when (and s (open-stream-p s))
                                        (ignore-errors (close s)))))
                         :state (make-event-log-state
                                 :log-dir log-dir
                                 :log-file-name log-file-name
                                 :max-file-size max-file-size
                                 :max-backup-files max-backup-files
                                 :current-stream stream)
                         :receive #'%event-log-receive))))
  *event-log*)

(defun shutdown-event-log ()
  "Stop the event log actor."
  (when *event-log*
    (ac:stop (act:context *event-log*) *event-log*)
    (setf *event-log* nil)))
