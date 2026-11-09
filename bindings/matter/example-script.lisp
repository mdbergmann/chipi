(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; we're expecting chipi project root folder to be cwd, and cl-matter as a
  ;; sibling checkout (or in ~/quicklisp/local-projects)
  (let* ((cwd (uiop:getcwd))
         (cl-matter (merge-pathnames "../cl-matter/" cwd))
         (plugin-asd (merge-pathnames "bindings/matter/binding-matter.asd" cwd)))
    (when (probe-file cl-matter)
      (push cl-matter asdf:*central-registry*))
    (asdf:load-asd plugin-asd :name "binding-matter")
    (asdf:load-system :binding-matter)
    ;; with chipi-ui: (asdf:load-system :binding-matter/ui) adds the "Share
    ;; with other apps" card -- pairing QR, window, apps -- to the settings
    ))

(defpackage :my-hab
  (:use :cl :chipi.hab :chipi.house :binding-matter))
(in-package :my-hab)

(defconfig "chipi"
  ;; Chipi as a Matter bridge: Apple Home (or Google Home, Alexa) adds it
  ;; with the pairing code logged at start, as an "uncertified accessory",
  ;; and shows the items below as its devices.
  (matter-init :storage "~/.chipi/matter/"
               :bridge '(:name "Chipi" :passcode 20202021 :discriminator 3840
                         :interface "en0")))

;; a virtual light: switched from the Home app, or from here
(deflight light-virtual "Virtual light"
  (matter-bridge))

;; a light on the KNX bus would be
;;   (deflight light-kitchen "Kitchen"
;;     (knx-binding :ga "1/2/3" :dpt "1.001")
;;     (matter-bridge))
;; the Home app's switch writes to the bus, the wall switch shows in the app

;; a contact: the Home app shows a sensor, open or closed
(defcontact window-bath "Bath window"
  :tags '((:ui-readonly . t))
  (matter-bridge))

;; a temperature: a sensor in degrees
(defheating guest "Guest room"
  :temperature ((matter-bridge)))

;; a heating of two items is one thermostat: the setpoint turned in the Home
;; app lands on the setpoint item (and on the bus, with a knx-binding there)
(defheating bath "Bath"
  :temperature ((matter-bridge :device bath))
  :setpoint    ((matter-bridge :device bath))
  :valve       ())

;; a shutter of three channels is one window covering: Open, Close, Stop and
;; the position slider in the app drive the three items
(defrollershutter blind-living "Blind living room"
  :move     ((matter-bridge :device blind-living))
  :stop     ((matter-bridge :device blind-living))
  :position ((matter-bridge :device blind-living)))

;; an item without a house kind says what it is
(defitem 'garden-pump "Garden pump" 'boolean
  (matter-bridge :device-type :on-off-plug-in-unit))

(defrule "say when the virtual light switches"
    :when-item-change 'light-virtual
    :do (lambda (trigger)
          (let ((item (cdr trigger)))
            (future:fcompleted (item:get-value item)
                (value)
              (format t "Virtual light is now ~a~%" value)))))

;; the codes to type into the Home app, again
;; (matter-bridge-pairing-code)
;; a second app: open the window for 15 minutes, see who added the bridge
;; (matter-bridge-open-window)
;; (matter-bridge-admins)

;; let the window close, the bath warm up, the blind report half way
;; (set-item-value 'window-bath 'item:false)
;; (set-item-value 'bath-temp 21.5)
;; (set-item-value 'blind-living-pos 50)
