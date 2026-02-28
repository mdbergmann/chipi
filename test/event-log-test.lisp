(defpackage :chipi.event-log-test
  (:use :cl :fiveam :miscutils :chipi.event-log)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.event-log-test)

(def-suite event-log-tests
  :description "Event log tests"
  :in chipi.tests:test-suite)

(in-suite event-log-tests)

(defparameter *test-runtime-dir* "test-runtime/")

(defun %runtime-root ()
  (asdf:system-relative-pathname "chipi" *test-runtime-dir*))

(defun %log-dir ()
  (merge-pathnames "event-log/" (%runtime-root)))

(defun %log-file ()
  (merge-pathnames "event-log.ndjson" (%log-dir)))

(defun %backup-file (n)
  (merge-pathnames (format nil "event-log.ndjson.~a" n) (%log-dir)))

(defun %read-lines (path)
  "Read all lines from a file."
  (when (probe-file path)
    (with-open-file (s path :direction :input)
      (loop :for line = (read-line s nil nil)
            :while line
            :collect line))))

(def-fixture init-destroy-env ()
  (let ((envi::*rel-runtime-dir* *test-runtime-dir*))
    (unwind-protect
         (progn (&body))
      (progn
        (ignore-errors (shutdown-event-log))
        (envi:shutdown-env)
        (uiop:delete-directory-tree
         (uiop:ensure-directory-pathname (%runtime-root))
         :validate t
         :if-does-not-exist :ignore)))))

(test ensure-event-log--creates-actor
  "Ensure event log creates a running actor and is idempotent."
  (with-fixture init-destroy-env ()
    (let ((actor (ensure-event-log)))
      (is-true actor)
      (is-true (act-cell:running-p actor))
      (is (eq actor (ensure-event-log))))))

(test event-log--writes-ndjson-on-item-change
  "Item value change writes a valid NDJSON line to the log file."
  (with-fixture init-destroy-env ()
    (ensure-event-log)
    (let ((item (item:make-item 'test-item :label "Test")))
      (item:set-value item 42)
      (is-true (await-cond 2
                 (let ((lines (%read-lines (%log-file))))
                   (and lines (= 1 (length lines))))))
      (let* ((lines (%read-lines (%log-file)))
             (parsed (jzon:parse (first lines))))
        (is (string= "TEST-ITEM" (gethash "item" parsed)))
        (is (equal 42 (gethash "new_value" parsed)))
        (is (eq t (gethash "old_value" parsed)))
        (is (stringp (gethash "timestamp" parsed)))))))

(test event-log--writes-multiple-changes
  "Multiple item value changes produce multiple NDJSON lines."
  (with-fixture init-destroy-env ()
    (ensure-event-log)
    (let ((item (item:make-item 'test-item :label "Test")))
      (item:set-value item 1)
      (item:set-value item 2)
      (item:set-value item 3)
      (is-true (await-cond 2
                 (let ((lines (%read-lines (%log-file))))
                   (and lines (= 3 (length lines)))))))))

(test event-log--rotates-on-size-threshold
  "Log file is rotated when it exceeds max-file-size."
  (with-fixture init-destroy-env ()
    (ensure-event-log :max-file-size 50)
    (let ((item (item:make-item 'test-item :label "Test")))
      (item:set-value item 1)
      (is-true (await-cond 2
                 (%read-lines (%log-file))))
      (item:set-value item 2)
      (is-true (await-cond 2
                 (probe-file (%backup-file 1)))))))

(test event-log--limits-backup-count
  "Oldest backup is deleted when max-backup-files is exceeded."
  (with-fixture init-destroy-env ()
    (ensure-event-log :max-file-size 50 :max-backup-files 2)
    (let ((item (item:make-item 'test-item :label "Test")))
      (item:set-value item 1)
      (is-true (await-cond 2
                 (%read-lines (%log-file))))
      (item:set-value item 2)
      (is-true (await-cond 2
                 (probe-file (%backup-file 1))))
      (item:set-value item 3)
      (is-true (await-cond 2
                 (probe-file (%backup-file 2))))
      (item:set-value item 4)
      (is-true (await-cond 2
                 (let ((lines (%read-lines (%log-file))))
                   (and lines
                        (let ((parsed (jzon:parse (first lines))))
                          (equal 4 (gethash "new_value" parsed)))))))
      (is-true (probe-file (%backup-file 1)))
      (is-true (probe-file (%backup-file 2)))
      (is-false (probe-file (%backup-file 3))))))

(test shutdown-event-log--stops-actor
  "Shutdown stops the event log actor and clears the global."
  (with-fixture init-destroy-env ()
    (let ((actor (ensure-event-log)))
      (is-true (act-cell:running-p actor))
      (shutdown-event-log)
      (is-false event-log::*event-log*))))
