(defpackage :chipi-ui.settings
  (:use :cl :clog)
  (:nicknames :ui-settings)
  (:export #:+settings-path+
           #:+home-path-key+
           #:home-path
           #:set-home-path))

(in-package :chipi-ui.settings)

;;; Per-device UI settings.
;;;
;;; These live in the browser's `localStorage', not on the server: which page
;;; the app opens on is a property of the device it is installed on -- the wall
;;; tablet wants the room overview, a phone wants the energy page.  An installed
;;; web app keeps its own storage container, separate from the browser's, so the
;;; setting is made inside the app and stays there.

(defparameter +settings-path+ "/settings"
  "URL path of the built-in settings view.")

(defparameter +home-path-key+ "chipi-home-path"
  "`localStorage' key under which a device stores the path of its home page.")

(defun %stored-value (raw)
  "The usable string in RAW, or `nil'.

`localStorage.getItem' answers JS `null' for a key that was never written,
which arrives here as the string \"null\"; a query that timed out answers
`nil'.  Neither is a path."
  (when (stringp raw)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
      (unless (or (string= "" trimmed)
                  (string-equal "null" trimmed)
                  (string-equal "undefined" trimmed))
        trimmed))))

(defun home-path (body)
  "The home page path this device stored, or `nil' when it has none.

Reading is a round-trip to the browser, so this is only called when rendering
the home view or the settings view -- never per widget."
  (handler-case
      (%stored-value (storage-element (window body) :local +home-path-key+))
    (error (c)
      ;; a device with storage disabled (Safari private mode) must still get a
      ;; dashboard, just always the default one
      (log:warn "Could not read ~a from localStorage: ~a" +home-path-key+ c)
      nil)))

(defun set-home-path (body path)
  "Stores PATH as this device's home page.  A PATH of `nil' removes the setting
again, which returns the device to the default home view."
  (if path
      (setf (storage-element (window body) :local +home-path-key+) path)
      (storage-remove (window body) :local +home-path-key+))
  path)
