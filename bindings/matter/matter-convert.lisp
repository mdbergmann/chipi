(in-package :chipi.binding.matter)

;; House kinds to bridged device types, and item values to cluster
;; attribute values.  Deliberately small: a table and a handful of
;; conversions.  The mirror of this, Matter device types to house definers
;; for the controller role, joins it later.

(defparameter *bridged-device-types*
  `((:on-off-light        ,matter:+device-type-on-off-light+        3 :on-off)
    (:on-off-plug-in-unit ,matter:+device-type-on-off-plug-in-unit+ 3 :on-off)
    (:contact-sensor      ,matter:+device-type-contact-sensor+      2 :boolean-state)
    (:temperature-sensor  ,matter:+device-type-temperature-sensor+  2 :temperature-measurement))
  "The device types a bridged item can be published as: (name id revision value-cluster).
VALUE-CLUSTER names the cluster that carries the item's value. The revisions are Matter 1.3's Device Library.")

(defparameter *kind-device-types*
  '((:light . :on-off-light)
    (:plug . :on-off-plug-in-unit)
    (:contact . :contact-sensor)
    (:temperature . :temperature-sensor))
  "House kind -- the `:house-kind' tag the house definers give an item -- to the device type that publishes it.")

(defun device-type-of-kind (kind)
  "The bridged device type name of the house `KIND', or NIL for one the bridge does not publish."
  (cdr (assoc kind *kind-device-types*)))

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
