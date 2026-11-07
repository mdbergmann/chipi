(defpackage :chipi-ui.ui
  (:use :cl :ui-main)
  (:nicknames :ui)
  (:export #:start))

(in-package :chipi-ui.ui)

(defun start (&key (host "localhost") (port 8080)
                settings-pin access-key
                ssl-cert-file ssl-key-file)
  "Initializes the UI system and mounts pages.

ACCESS-KEY, a string, puts a login page in front of the whole UI: a browser
gets nothing -- no page, no websocket, no static file -- until it has posted
this key, after which a session cookie lets it in (see `chipi-ui.auth').
Failed attempts are throttled per client address.  Without a key the UI is
open to everyone who can reach it.

SSL-CERT-FILE and SSL-KEY-FILE, PEM files, serve the UI over https; the
session cookie is then marked Secure.  Making the certificate -- and getting
its issuer trusted by the devices -- is the installation's job, see the TLS
section of README-web.md for what the certificate has to satisfy.  Both or
neither must be given.

SETTINGS-PIN, a string, offers a device lock in the settings view: a locked
device hides the settings gear and asks for this PIN before it shows its
settings again.  Without one there is no lock."
  (log:info "Starting UI")
  (setf ui-settings:*settings-pin* settings-pin)
  (ui-auth:configure access-key)
  (setf ui-auth:*secure-cookie* (and ssl-cert-file ssl-key-file t))
  (let ((started nil))
    ;; a failed start (e.g. a missing TLS file) must not leave the gate and
    ;; the Secure flag configured for a server that is not running
    (unwind-protect
         (progn
           (ui-main:start-main host port
                               :ssl-cert-file ssl-cert-file
                               :ssl-key-file ssl-key-file)
           (setf started t))
      (unless started
        (ui-auth:configure nil)
        (setf ui-auth:*secure-cookie* nil))))
  (hab:add-to-shutdown (lambda ()
                         (shutdown)))
  t)

(defun shutdown ()
  (log:info "Shutting down UI")
  (ui-main:shutdown-main))
