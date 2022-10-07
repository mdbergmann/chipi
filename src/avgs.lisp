(defpackage :cl-eta.avgs
  (:use :cl)
  (:nicknames :avgs)
  (:export #:due-p))

(in-package :cl-eta.avgs)

(defun due-p (run-secs cadence-secs)
  "`run-secs': runtime in secs from start
`cadence-secs': cadence in seconds."
  (and (> cadence-secs 0)
       (= (rem run-secs cadence-secs) 0)))
