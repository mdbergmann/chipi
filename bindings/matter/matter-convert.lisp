(in-package :chipi.binding.matter)

;; House kinds to bridged device types, and item values to cluster
;; attribute values.  Deliberately small: a few tables and a handful of
;; conversions.  The mirror of this, Matter device types to house definers
;; for the controller role, joins it later.

(defparameter *bridged-device-types*
  `((:on-off-light        ,matter:+device-type-on-off-light+        3 :on-off)
    (:dimmable-light      ,matter:+device-type-dimmable-light+      3 :level-control)
    (:on-off-plug-in-unit ,matter:+device-type-on-off-plug-in-unit+ 3 :on-off)
    (:contact-sensor      ,matter:+device-type-contact-sensor+      2 :boolean-state)
    (:temperature-sensor  ,matter:+device-type-temperature-sensor+  2 :temperature-measurement)
    (:window-covering     ,matter:+device-type-window-covering+     2 :window-covering)
    (:thermostat          ,matter:+device-type-thermostat+          2 :thermostat))
  "The device types a bridged device can be published as: (name id revision value-cluster).
VALUE-CLUSTER names the cluster that carries the device's values (a Dimmable Light's Level Control has an On/Off cluster next to it). The revisions are Matter 1.3's Device Library.")

(defparameter *kind-device-types*
  '((:light . :on-off-light)
    (:plug . :on-off-plug-in-unit)
    (:pulse . :on-off-plug-in-unit)
    (:contact . :contact-sensor)
    (:temperature . :temperature-sensor))
  "House kind -- the `:house-kind' tag the house definers give an item -- to the device type that publishes an item of that kind on its own. A push button (`house:defpulse') is a plug that springs back: Matter's own button, the Generic Switch, only reports presses and cannot be pressed from an app.")

(defparameter *composed-device-kinds*
  '((:dimmable-light :light :brightness)
    (:window-covering :shutter-move :shutter-stop :shutter-position :shutter-slat)
    (:thermostat :temperature :setpoint :mode))
  "The device types several items make up together, each with the house kinds of its parts: (type kind...).
A `house:defdimmablelight' is a Dimmable Light, a `house:defrollershutter' a Window Covering, a `house:defheating' a Thermostat (its valve stays at home: Matter's Thermostat has none).")

(defparameter *heating-modes* '(:off 4 :heat 1)
  "What a heating's mode item holds for the Thermostat's two system modes, unless the item's `matter-bridge' gives its own `:modes': the number that switches the heating off and the one that lets it heat. The default is KNX's HVAC mode (DPT 20.102): 4 building protection, 1 comfort.")

(defconstant +system-mode-off+ 0)
(defconstant +system-mode-heat+ 4)

(defun check-heating-modes (modes)
  "`MODES' if it is a plist of the integers `:off' and `:heat', two different ones; an error otherwise."
  (let ((off (getf modes :off))
        (heat (getf modes :heat)))
    (unless (and (integerp off) (integerp heat) (/= off heat))
      (error ":modes must be a plist of two different integers, (:off <n> :heat <m>), not: ~s" modes))
    modes))

(defun item-mode->system-mode (value modes)
  "The Thermostat SystemMode of a mode item: Off while the item holds the `:off' number of `MODES', Heat for any other value -- the modes in between (standby, night) still heat."
  (if (eql value (getf modes :off))
      +system-mode-off+
      +system-mode-heat+))

(defun system-mode->item-mode (mode modes)
  "The mode item value for the Thermostat SystemMode `MODE': the `:off' or the `:heat' number of `MODES'."
  (if (eql mode +system-mode-off+)
      (getf modes :off)
      (getf modes :heat)))

(defun device-type-of-kind (kind)
  "The bridged device type name of an item of house `KIND' on its own, or NIL for one the bridge does not publish."
  (cdr (assoc kind *kind-device-types*)))

(defun device-type-of-kinds (kinds)
  "The bridged device type name of a device whose parts are items of the house `KINDS': the type of one kind on its own, else the composed type of `*composed-device-kinds*' one of the kinds belongs to (a shutter with a position item alone is still a Window Covering, a setpoint alone a Thermostat, a temperature alone a Temperature Sensor, a light alone an On/Off Light), or NIL."
  (or (and (= 1 (length kinds))
           (device-type-of-kind (first kinds)))
      (car (find-if (lambda (spec) (intersection kinds (cdr spec))) *composed-device-kinds*))))

(defun composed-device-kinds (type)
  "The house kinds a device of the composed `TYPE' is made of, or NIL for a type of one item."
  (cdr (assoc type *composed-device-kinds*)))

(defun device-type-spec (name)
  "The device type `NAME' as (values id revision value-cluster); an error for a name not in `*bridged-device-types*'."
  (let ((spec (assoc name *bridged-device-types*)))
    (unless spec
      (error "Unknown bridged device type ~s, one of ~{~s~^, ~} expected"
             name (mapcar #'car *bridged-device-types*)))
    (values-list (cdr spec))))

;; Item values are the symbols `item:true' and `item:false' for booleans and
;; numbers otherwise.

(defun item-boolean->bool (value)
  "T for `item:true', NIL for anything else."
  (eq value 'item:true))

(defun bool->item-boolean (bool)
  "`item:true' for a true `BOOL', `item:false' otherwise."
  (if bool 'item:true 'item:false))

(defun item-contact->state-value (value)
  "The Boolean State of a contact item: a `house:defcontact' is `item:true' while open, a Matter Contact Sensor's StateValue is TRUE while closed."
  (not (eq value 'item:true)))

(defun item-temperature->measured-value (value)
  "The Temperature Measurement MeasuredValue of a temperature item: hundredths of a degree, or :NULL for a value that is not a number."
  (matter:temperature-hundredths value))

;; A light's brightness is a percentage in the house and a level of 1 to
;; 254 on the wire: a Matter light has no level 0, off is the On/Off
;; cluster's business.

(defconstant +min-level+ 1)
(defconstant +max-level+ 254)

(defun item-brightness->level (value)
  "The Level Control CurrentLevel of a brightness item: 1..254 for 0..100 %, or :NULL for a value that is not a number. 0 % is the lowest level: the light is off by its switch, not by its level."
  (if (realp value)
      (max +min-level+ (min +max-level+ (round (* value +max-level+) 100)))
      :null))

(defun level->item-brightness (level)
  "The brightness item value, a percentage of 1..100, of a Level Control level: never 0, which would switch a dimmer off -- that is what the switch item is for."
  (max 1 (min 100 (round (* level 100) +max-level+))))

;; A shutter's position is a percentage, 100 closed, in the house and on
;; the wire; the wire counts hundredths of a percent.

(defun item-position->percent100ths (value)
  "The Window Covering position of a shutter position or slat item: hundredths of a percent, 10000 closed, or :NULL for a value that is not a number."
  (matter:percent-hundredths value))

(defun percent100ths->item-position (percent100ths)
  "The shutter position or slat item value, a percentage, of a Window Covering position."
  (round percent100ths 100))

;; A heating setpoint is degrees in the house and hundredths on the wire,
;; within the limits the Thermostat cluster tells administrators.

(defparameter *heating-setpoint-limits* '(5 . 35)
  "The lowest and highest heating setpoint in degrees the bridge's Thermostats accept: the frost protection of a KNX actuator to a warm bath.")

(defun item-setpoint->heating-setpoint (value)
  "The Thermostat OccupiedHeatingSetpoint of a setpoint item: hundredths of a degree within `*heating-setpoint-limits*'; a value that is not a number is the lower limit."
  (let ((min (* 100 (car *heating-setpoint-limits*)))
        (max (* 100 (cdr *heating-setpoint-limits*)))
        (hundredths (matter:temperature-hundredths value)))
    (if (integerp hundredths)
        (max min (min max hundredths))
        min)))

(defun heating-setpoint->item-setpoint (hundredths)
  "The setpoint item value, degrees as a float, of a Thermostat setpoint."
  (/ hundredths 100.0))
