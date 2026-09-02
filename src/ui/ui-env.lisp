(defpackage :chipi-ui.ui
  (:use :cl :ui-main)
  (:nicknames :ui)
  (:export #:start))

(in-package :chipi-ui.ui)

(defun start (&key (host "localhost") (port 8080) settings-pin)
  "Initializes the UI system and mounts pages.

SETTINGS-PIN, a string, offers a device lock in the settings view: a locked
device hides the settings gear and asks for this PIN before it shows its
settings again.  Without one there is no lock."
  (log:info "Starting UI")
  (setf ui-settings:*settings-pin* settings-pin)
  (ui-main:start-main host port)
  (hab:add-to-shutdown (lambda ()
                         (shutdown)))
  t)

(defun shutdown ()
  (log:info "Shutting down UI")
  (ui-main:shutdown-main))
