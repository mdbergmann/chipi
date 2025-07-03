(defpackage :chipi-ui.ui
  (:use :cl :ui-main)
  (:nicknames :ui)
  (:export #:start))

(in-package :chipi-ui.ui)

(defun start (&key (host "localhost") (port 8080))
  "Initializes the UI system and mounts pages."
  (log:info "Starting UI")
  (ui-main:start-main host port)
  (hab:add-to-shutdown (lambda ()
                         (shutdown)))
  t)

(defun shutdown ()
  (log:info "Shutting down UI")
  (ui-main:shutdown-main))
