
;; Overrides make-cron-job to also 'boot' run it if :boot-only is set
(in-package :cl-cron)

(defun make-cron-job (function-symbol
                      &key
                        (minute :every) (step-min 1)
                        (hour :every) (step-hour 1) (day-of-month :every)
                        (step-dom 1) (month :every)
                        (step-month 1) (day-of-week :every)
                        (step-dow 1) (boot-only nil) (hash-key nil))
  "creates a new instance of a cron-job object and appends it to the cron-jobs-list after processing its time. Note that if you wish to use multiple values for each parameter you need to provide a list of numbers or use the gen-list function. You can not have a list of symbols when it comes to month or day-of-week. Please note that as by ANSI Common Lisp for the month variable the possible values are between 1 and 12 inclusive with January=1 and for day of week the possible values are between 0 and 6 with Monday=0. Returns the hash-key"
  (if (eql hash-key nil) (setf hash-key (gensym "cron")))
  (let ((job
          (make-instance 'cron-job
                         :job-minute (get-minutes minute step-min)
                         :job-hour (get-hours hour step-hour)
                         :job-dom (get-days-of-month day-of-month step-dom)
                         :job-month (get-months month step-month)
                         :job-dow (get-days-of-week day-of-week step-dow)
                         :job-@boot boot-only
                         :job-func function-symbol)))
    ;; MB: Added this to run the job if boot-only is set
    ;; MB: This is a bit of a hack, but it works
    ;; MB: If boot-only is set, then the job is not added to the hash to not run it repeatedly
    (format t "Created job: ~a, boot-only: ~a~%" job (job-@boot job))
    (if boot-only
        (run-job-if-boot hash-key job)
        (setf (gethash hash-key *cron-jobs-hash*) job)))
  hash-key)
