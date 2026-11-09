(defpackage :chipi-ui.settings
  (:use :cl :clog)
  (:nicknames :ui-settings)
  (:export #:+settings-path+
           #:+home-path-key+
           #:+lock-key+
           #:*settings-pin*
           #:home-path
           #:set-home-path
           #:lock-available-p
           #:locked-p
           #:set-locked
           #:pin-matches-p
           ;; sections plugins add under /settings/<id>
           #:settings-section
           #:settings-section-p
           #:settings-section-id
           #:settings-section-label
           #:settings-section-render
           #:settings-section-summary
           #:section-path
           #:register-section
           #:unregister-section
           #:clear-sections
           #:get-sections
           #:find-section
           #:find-section-by-path
           #:*section-registered-hook*))

(in-package :chipi-ui.settings)

;;; Per-device UI settings.
;;;
;;; These live in the browser's `localStorage', not on the server: which page
;;; the app opens on is a property of the device it is installed on -- the wall
;;; tablet wants the room overview, a phone wants the energy page.  An installed
;;; web app keeps its own storage container, separate from the browser's, so the
;;; setting is made inside the app and stays there.
;;;
;;; A device can also be locked: the settings gear disappears from the app
;;; header and the settings view asks for a PIN before it shows anything.  The
;;; lock flag is a device setting like the home page; the PIN it asks for is
;;; configured on the server (`*settings-pin*'), so the person holding the
;;; device cannot look it up.

(defparameter +settings-path+ "/settings"
  "URL path of the built-in settings view.")

(defparameter +home-path-key+ "chipi-home-path"
  "`localStorage' key under which a device stores the path of its home page.")

(defparameter +lock-key+ "chipi-settings-locked"
  "`localStorage' key under which a device stores that its settings are locked.")

(defvar *settings-pin* nil
  "The PIN that opens the settings view on a locked device, as a string, or
`nil' when the device lock is not offered at all.  Set through `ui:start's
`:settings-pin'.")

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

(defun %read-stored (body key)
  "The value this device stored under KEY, or `nil' when it has none.

Reading is a round-trip to the browser, so callers read once per view -- never
per widget."
  (handler-case
      (%stored-value (storage-element (window body) :local key))
    (error (c)
      ;; a device with storage disabled (Safari private mode) must still get a
      ;; dashboard, just always the default one
      (log:warn "Could not read ~a from localStorage: ~a" key c)
      nil)))

(defun home-path (body)
  "The home page path this device stored, or `nil' when it has none."
  (%read-stored body +home-path-key+))

(defun set-home-path (body path)
  "Stores PATH as this device's home page.  A PATH of `nil' removes the setting
again, which returns the device to the default home view."
  (if path
      (setf (storage-element (window body) :local +home-path-key+) path)
      (storage-remove (window body) :local +home-path-key+))
  path)

;; ---------------------------------------------------------------------------
;; device lock
;; ---------------------------------------------------------------------------

(defun lock-available-p ()
  "Whether devices can be locked at all, which takes a configured PIN."
  (and *settings-pin* t))

(defun locked-p (body)
  "Whether this device locked its settings.

Always `nil' without a configured PIN, whatever the device stored: dropping
the PIN from the config must not strand locked devices with no way back into
their settings.  This also spares every view the storage round-trip when the
lock is not in use."
  (and (lock-available-p)
       (equal "true" (%read-stored body +lock-key+))))

(defun set-locked (body locked)
  "Locks (LOCKED true) or unlocks this device's settings."
  (if locked
      (setf (storage-element (window body) :local +lock-key+) "true")
      (storage-remove (window body) :local +lock-key+))
  locked)

(defun pin-matches-p (entered)
  "Whether ENTERED, as typed into the PIN prompt, is the configured PIN."
  (and (lock-available-p)
       (stringp entered)
       (string= *settings-pin*
                (string-trim '(#\Space #\Tab #\Newline #\Return) entered))))

;; ---------------------------------------------------------------------------
;; settings sections -- views plugins add under /settings/<id>
;; ---------------------------------------------------------------------------

;;; chipi-ui knows nothing of what a plugin has to show -- a Matter bridge's
;;; pairing code, say -- and must not come to depend on it.  A plugin
;;; registers a section instead: the settings view shows a card for it, and
;;; `/settings/<id>' renders what the plugin renders, behind the same header,
;;; back button and device lock as the settings themselves.

(defstruct settings-section
  "A view a plugin adds to the settings.  RENDER is called with the
navigation context and the element to render into; SUMMARY, when given, is
a function of no arguments returning the one line the section's card shows
in the settings view (\"3 devices, 1 unreachable\")."
  id label render summary)

(defvar *sections* '()
  "The registered sections, in the order they were registered.")

(defvar *section-registered-hook* nil
  "Called with each section as it is registered.  `chipi-ui.main' sets it to
register the section's path as a route, so a deep link or a reload lands in
the section.")

(defun section-path (section)
  "The URL path SECTION is served at: `+settings-path+' followed by its id, in
lowercase."
  (format nil "~a/~(~a~)" +settings-path+ (settings-section-id section)))

(defun register-section (id &key label render summary)
  "Registers the settings section ID, a keyword that also names its path
(`/settings/<id>'), with LABEL (the heading of its card and of its view),
RENDER (a function of the navigation context and the element to render into)
and SUMMARY (a function of no arguments returning the line its card shows, or
`nil').  A section registered under the same id before is replaced, keeping
its place.  Returns the section."
  (check-type id keyword)
  (check-type label string)
  (check-type render (or function (and symbol (not null))))
  (check-type summary (or null function symbol))
  (let ((section (make-settings-section :id id :label label
                                        :render render :summary summary))
        (existing (find id *sections* :key #'settings-section-id)))
    (setf *sections*
          (if existing
              (substitute section existing *sections*)
              (append *sections* (list section))))
    (when *section-registered-hook*
      (funcall *section-registered-hook* section))
    section))

(defun unregister-section (id)
  "Removes the section ID; its card is gone with the next render.  Its CLOG
route stays registered (routes cannot be unregistered), but the path no longer
resolves to a section, so a visit to it shows the not-found view."
  (setf *sections* (remove id *sections* :key #'settings-section-id)))

(defun clear-sections ()
  "Removes every section."
  (setf *sections* '()))

(defun get-sections ()
  "The registered sections, in registration order."
  *sections*)

(defun find-section (id)
  "The section registered as ID, or `nil'."
  (find id *sections* :key #'settings-section-id))

(defun find-section-by-path (path)
  "The section served at PATH (as `page:normalize-path' leaves it), or `nil'."
  (find path *sections* :key #'section-path :test #'string=))
