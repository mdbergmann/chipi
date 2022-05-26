(defpackage :cl-eta.package
  (:use :cl)
  (:nicknames :eta-pkg)
  (:export #:collect-data
           #:extract-pkg
           #:new-start-record-pkg))

(in-package :cl-eta.package)

(defun collect-data (prev-data new-data)
  "Concatenates `prev-data' and `new-data'.
A full package is when it starts with `#\{' and ends with `#\}'.
In this case the return is `(values t <full-package>)'.
If this is a partial package the return is: `(values nil <partial-package>)'."
  nil)

(defun extract-pkg (pkg-data)
  "`pkg-data' is supposed to be a full eta package with starting `#\{' and ending `#\}'.
If it is not a full package, or something happens during extraction the return is:
`(values :fail <reason>)'.
If it is a full package with monitors data the return is:
`(values :monitor <alist-of-monitor-items)' where an item consists of: `(cons <openhab-item-name> <item-value>)'."
  nil)

(defun new-start-record-pkg ()
  "Returns a new `start-record' eta package."
  nil)

