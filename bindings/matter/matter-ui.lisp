(defpackage :chipi.binding.matter.ui
  (:use :cl :clog)
  (:nicknames :binding-matter-ui)
  (:import-from #:alexandria
                #:when-let)
  (:export #:+section-id+
           #:register-share-section
           #:render-share-section))

(in-package :chipi.binding.matter.ui)

;; The "Share with other apps" card of chipi-ui's settings: what the bridge
;; needs beyond the configuration script, on the wall tablet.
;;
;; chipi-ui knows nothing of Matter.  This file registers a settings
;; section with it (`ui-settings:register-section'), and the section renders
;; three cards from what the binding tells: the pairing code as a QR image
;; and as the number to type, with the pairing window to open for a second
;; app; the apps that added the bridge, each with a Remove; and the bridged
;; devices as the apps see them.  Loading the system `binding-matter/ui'
;; registers the section; the UI shows it from then on.

(defparameter +section-id+ :matter-bridge
  "The id of the settings section, and so its path: /settings/matter-bridge.")

(defparameter *window-minutes* 15
  "How long the pairing window stays open when opened from the card: the most Matter allows.")

(defparameter *vendor-names*
  '((#x1349 . "Apple") (#x6006 . "Google") (#x1217 . "Amazon") (#x10E1 . "Samsung SmartThings"))
  "The apps one meets, by the vendor id their fabric carries.")

(defparameter *device-type-names*
  '((:on-off-light . "On/Off Light") (:on-off-plug-in-unit . "On/Off Plug-in Unit")
    (:contact-sensor . "Contact Sensor") (:temperature-sensor . "Temperature Sensor")
    (:window-covering . "Window Covering") (:thermostat . "Thermostat")))

;; -----------------------------
;; the texts
;; -----------------------------

(defun %format-manual-code (code)
  "The 11 digits of a manual pairing code in the groups the apps print them in: 3497-011-2332."
  (if (= (length code) 11)
      (format nil "~a-~a-~a" (subseq code 0 4) (subseq code 4 7) (subseq code 7))
      code))

(defun %admin-name (admin)
  "What to call an administrator: the label its app gave the fabric, else the vendor's name, else the vendor id."
  (let ((label (getf admin :label))
        (vendor (getf admin :vendor-id)))
    (cond
      ((plusp (length label)) label)
      ((cdr (assoc vendor *vendor-names*)))
      (t (format nil "App with vendor id 0x~4,'0x" vendor)))))

(defun %device-type-name (type)
  (cond
    ((null type) "Unknown device")
    ((cdr (assoc type *device-type-names*)))
    (t (string-capitalize (substitute #\Space #\- (symbol-name type))))))

(defun %duration (seconds)
  (if (< seconds 60)
      (format nil "~d second~:p" seconds)
      (format nil "~d minute~:p" (ceiling seconds 60))))

(defun %window-text (open)
  "The state of the pairing window, as `binding-matter:matter-bridge-window-open-p' tells it."
  (cond
    ((null open) "Pairing window closed")
    ((eq open t) "Pairing window open until the first app has added the bridge")
    (t (format nil "Pairing window open, ~a left" (%duration open)))))

(defun %summary ()
  "The line the settings view shows for the section."
  (if (not (binding-matter:matter-bridge-running-p))
      "Bridge not running"
      (handler-case
          (format nil "~d app~:p, ~d device~:p~:[~;, pairing window open~]"
                  (length (binding-matter:matter-bridge-admins))
                  (length (binding-matter:matter-bridge-devices))
                  (binding-matter:matter-bridge-window-open-p))
        (error (c)
          (format nil "~a" c)))))

;; -----------------------------
;; the cards
;; -----------------------------

(defun %card (container heading)
  "A card titled `HEADING', in the settings' own style; returns the element to fill."
  (let ((card (create-div container :class "page-section")))
    (create-div card :class "page-section-header" :content heading)
    (create-div card :class "page-section-body")))

(defun %hint (container text)
  (create-div container :class "settings-hint" :content text))

(defun %render-pairing-card (container rerender)
  "The pairing code, as a QR image and as the number, and the pairing window with its button. Returns the button; it calls `RERENDER' after opening or closing the window."
  (let ((body (%card container "Pairing code")))
    (multiple-value-bind (manual qr-text) (binding-matter:matter-bridge-pairing-code)
      (let ((qr (create-div body :class "matter-qr"
                                 :content (binding-matter:qr-svg (binding-matter:qr-encode qr-text)))))
        (setf (style qr "max-width") "240px"
              (style qr "margin") "0 auto 0.75rem"))
      (let ((code (create-div body :class "matter-pairing-code"
                                   :content (%format-manual-code manual))))
        (setf (style code "text-align") "center"
              (style code "font-size") "1.5rem"
              (style code "font-weight") "600"
              (style code "letter-spacing") "0.1em"
              (style code "margin-bottom") "0.75rem")))
    (%hint body (format nil "Scan the code with the Home app, or type the number: ~
<i>Add Accessory</i> &rsaquo; <i>More options</i> &rsaquo; <i>My Accessory isn't shown here</i> &rsaquo; <i>Enter code</i>. ~
The bridge presents test credentials, so the app calls it an uncertified accessory: add it anyway. ~
Matter accessories need a home hub, a HomePod or an Apple TV, on the same network."))
    (let* ((open (binding-matter:matter-bridge-window-open-p))
           (row (create-div body :class "settings-option"))
           (detail (progn
                     (create-div row :class "settings-option-label" :content (%window-text open))
                     (create-div row :class "settings-option-detail")))
           (button (create-button detail
                                  :class (if open "btn btn-outline-secondary btn-sm" "btn btn-primary btn-sm")
                                  :content (if open
                                               "Close"
                                               (format nil "Open for ~d minutes" *window-minutes*)))))
      (setf (style row "cursor") "default")
      (set-on-click button
                    (lambda (obj)
                      (declare (ignore obj))
                      ;; on failure keep the card (a re-render would wipe the
                      ;; message) and show the error on the button
                      (handler-case
                          (progn
                            (if open
                                (binding-matter:matter-bridge-close-window)
                                (binding-matter:matter-bridge-open-window :timeout (* 60 *window-minutes*)))
                            (funcall rerender))
                        (error (c)
                          (log:warn "Matter pairing window: ~a" c)
                          (setf (text button) (format nil "Failed: ~a" c))))))
      button)))

(defun %render-admins-card (container rerender)
  "The apps that added the bridge, each with a Remove that wants a second tap. Returns the Remove buttons; one calls `RERENDER' after removing."
  (let ((body (%card container "Apps that added the bridge"))
        (admins (binding-matter:matter-bridge-admins)))
    (if (null admins)
        (progn
          (%hint body "No app has added the bridge yet. Scan the pairing code while the window is open.")
          '())
        ;; mapcar, not loop: each button's handler must keep its own admin
        (mapcar (lambda (admin) (%render-admin-row body admin rerender))
                admins))))

(defun %render-admin-row (body admin rerender)
  "One administrator's row with its Remove button, which is returned."
  (let* ((row (create-div body :class "settings-option"))
         (detail (progn
                   (create-div row :class "settings-option-label"
                                   :content (ui-renderer:escape-html (%admin-name admin)))
                   (create-div row :class "settings-option-detail"
                                   :content (format nil "vendor 0x~4,'0x, fabric ~d &nbsp;"
                                                    (getf admin :vendor-id)
                                                    (getf admin :fabric-index)))))
         (button (create-button detail :class "btn btn-outline-danger btn-sm"
                                       :content "Remove"))
         (armed nil))
    (setf (style row "cursor") "default")
    (set-on-click button
                  (lambda (obj)
                    (declare (ignore obj))
                    (cond
                      (armed
                       (handler-case
                           (progn
                             (binding-matter:matter-bridge-remove-admin (getf admin :fabric-index))
                             (funcall rerender))
                         (error (c)
                           (log:warn "Matter remove admin: ~a" c)
                           (setf armed nil
                                 (text button) (format nil "Failed: ~a" c)))))
                      (t
                       ;; no browser dialog: the second tap is the confirmation
                       (setf armed t
                             (text button) "Tap again to remove")))))
    button))

(defun %render-devices-card (container)
  "The bridged devices as the apps see them: label, type, endpoint and items, or why one is not published."
  (let ((body (%card container "Devices the apps see"))
        (devices (binding-matter:matter-bridge-devices)))
    (if (null devices)
        (%hint body "No item has a <code>matter-bridge</code> binding yet.")
        (dolist (device devices)
          (let ((row (create-div body :class "settings-option"))
                (problem (getf device :problem)))
            (setf (style row "cursor") "default")
            (create-div row :class "settings-option-label"
                            :content (ui-renderer:escape-html (getf device :label)))
            (let ((detail (create-div row :class "settings-option-detail"
                                          :content (if problem
                                                       (format nil "not published: ~a"
                                                               (ui-renderer:escape-html problem))
                                                       (format nil "~a, endpoint ~d: ~{~a~^, ~}"
                                                               (%device-type-name (getf device :device-type))
                                                               (getf device :endpoint)
                                                               (mapcar (lambda (name)
                                                                         (ui-renderer:escape-html (string-downcase name)))
                                                                       (getf device :items)))))))
              (when problem
                (setf (style detail "color") "#dc3545")))
            (create-div row :class "settings-option-check"
                            :content (if (getf device :reachable-p) "&#10003;" "&nbsp;")))))))

(defun render-share-section (ctx container)
  "Renders the section into `CONTAINER', afresh: the pairing code with the window, the apps, the devices -- or, while the bridge is not running, a note. The buttons render it again."
  (declare (ignore ctx))
  (setf (inner-html container) "")
  (flet ((rerender () (render-share-section nil container)))
    (cond
      ((not (binding-matter:matter-bridge-running-p))
       (%hint (%card container "Matter bridge")
              (format nil "The Matter bridge is not running. Start it with ~
<code>(matter-init :bridge ...)</code> in the configuration, see the binding's README.")))
      (t
       (handler-case
           (progn
             (%render-pairing-card container #'rerender)
             (%render-admins-card container #'rerender)
             (%render-devices-card container))
         (error (c)
           (%hint container (format nil "The bridge did not answer: ~a"
                                    (ui-renderer:escape-html (princ-to-string c)))))))))
  container)

(defun register-share-section ()
  "Registers the section with chipi-ui's settings. Loading this file does it; call it again after `ui-settings:clear-sections'."
  (ui-settings:register-section +section-id+
                                :label "Share with other apps"
                                :render #'render-share-section
                                :summary #'%summary))

(register-share-section)
