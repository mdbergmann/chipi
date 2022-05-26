(defpackage :cl-eta.package
  (:use :cl)
  (:nicknames :eta-pkg)
  (:export #:collect-data
           #:extract-pkg
           #:new-start-record-pkg))

(in-package :cl-eta.package)

(defun collect-data (prev-data new-data) nil)

(defun extract-pkg (pkg-data))

(defun new-start-record-pkg ())

