(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this pulls all the dependencies.
  ;; you may use them in here
  ;; `chipi-api' depends on `chipi'
  ;;(asdf:load-system :chipi-api)
  )

;; we want our own package to be used
;; this allows us to `:use' namespaces we need
(defpackage :chipi-example
  (:use :cl :hab :page))
(in-package :chipi-example)

;; the first thing that needs be dine is setup the environment
;; `DEFCONFIG' sets up the 'runtime' folder within 'chipi' system root folder,
;; it starts actor systems, timers and cron services

(defconfig "chipi"
  ;; at this point we can rely on item system `isys', `timers' and `cr' (cron) to be available
  ;; and that a 'runtime' folder was created

  ;; for chipi-api we need to setup additional things
  
  ;; 1. setup the `api-env' environment, this creates some crucial runtime data
  ;; 2. setup the api-key store
  ;;    we just use a simple file store as there may not be many api-keys (for now)
  ;;    you may change api-key life-time (optional)
  (api-env:init :apikey-store apikey-store:*memory-backend*  ;; (apikey-store:make-simple-file-backend)
                :apikey-lifetime (ltd:duration :day 100))

  ;; maybe create additional api-keys with different access-rights
  ;; see (apikey-store:create-apikey)
  
  ;; 3. start the API server
  ;; This is optional if you don't require an API server. Not required for UI.
  (api:start)

  ;; 4. Initialize and start UI. This starts the UI web server.
  ;; UI is optional as well as API server is optional,
  ;; but apt-env setup (2.) with API keys is mandatory for UI.
  (ui:start :host "localhost" :port 8080)
  )

;; --- demo history for the chart widget --------------------------------------
;; Charts read from a historic persistence (normally influxdb).  This tiny
;; in-memory one keeps all persisted values per item in a list — enough to
;; feed the chart demo below, but not durable.

(defclass mem-persistence (persp:base-historic-persistence)
  ((store :initform (make-hash-table :test #'equal)
          :reader mem-store
          :documentation "item-name -> list of persisted-items, newest first."))
  (:documentation "In-memory historic persistence for demos."))

(defun make-mem-persistence (id)
  (persp::make-persistence id :type 'mem-persistence))

(defmethod persp:initialize ((p mem-persistence)))
(defmethod persp:shutdown ((p mem-persistence)))

(defmethod persp:persist ((p mem-persistence) item)
  (let ((state (item:get-item-stateq item)))
    (push (persp:make-persisted-item
           :value (item:item-state-value state)
           :timestamp (item:item-state-timestamp state))
          (gethash (item:name item) (mem-store p)))))

(defmethod persp:retrieve ((p mem-persistence) item)
  (or (first (gethash (item:name item) (mem-store p)))
      '(:error . "No value persisted")))

(defun %range-boundaries (range)
  "Start/end universal timestamps of RANGE."
  (etypecase range
    (persp:relative-range
     (let ((end (or (persp:end-ts range) (get-universal-time)))
           (span (+ (or (persp:seconds range) 0)
                    (* 60 (or (persp:minutes range) 0))
                    (* 3600 (or (persp:hours range) 0))
                    (* 86400 (or (persp:days range) 0)))))
       (values (- end span) end)))
    (persp:absolute-range
     (values (or (persp:start-ts range) 0)
             (or (persp:end-ts range) (get-universal-time))))))

(defmethod persp:retrieve-range ((p mem-persistence) item range &optional aggregate)
  (declare (ignore aggregate))
  (multiple-value-bind (start end) (%range-boundaries range)
    (sort (remove-if-not (lambda (pitem)
                           (<= start (persp:persisted-item-timestamp pitem) end))
                         (gethash (item:name item) (mem-store p)))
          #'< :key #'persp:persisted-item-timestamp)))

(defpersistence :mem (lambda (id) (make-mem-persistence id)))

;; items here

(defitemgroup 'plugs "Plugs")
(defitemgroup 'lights "Lights" :tags '((:ui-link)))
(defitemgroup 'switches "Switches")
(defitemgroup 'temps "Temperatures" :tags '((:ui-link)))

(defitemgroup 'eg-lights "EG Lights" :tags '((:ui-link)) :group 'lights)
(defitemgroup 'og-lights "OG Lights" :tags '((:ui-link)) :group 'lights)

(defitem 'switch1 "Switch1" 'boolean :initial-value 'item:false
  :group '(eg-lights plugs switches)
  :tags '((:ui-type . "Light")))
(defitem 'light1 "Light1" 'float :initial-value 123.456
  :group '(og-lights))
(defitem 'plug1 "Plug1" 'string :initial-value 'item:false
  :group '(plugs))
(defitem 'outside-temp "Outside temperature" 'integer :initial-value 15
  :group '(temps)
  :persistence '(:id :mem :frequency :every-change))
(defitem 'heizstab-wd1-energy "Heizstab Wendel 1 Energie [Wh]" 'float :initial-value 8.4)
(defitem 'event-1 "Event 1" nil :initial-value "Event occured"
  :tags '((:ext-readonly . t)))
(defitem 'motion-sensor "Motion Sensor" 'boolean :initial-value 'item:true
  :group '(switches)
  :tags '((:ui-readonly . t)))

;; seed ~6h of demo history so the chart shows data right away (writes into
;; the store directly; the persistence actor is idle at load time)
(let ((p (get-persistence :mem))
      (item-name (item:name (get-item 'outside-temp)))
      (now (get-universal-time)))
  (loop :for minutes-ago :from 360 :downto 0 :by 5
        :for ts = (- now (* 60 minutes-ago))
        :do (push (persp:make-persisted-item
                   :value (round (+ 15 (* 8 (sin (/ ts 1800.0)))))
                   :timestamp ts)
                  (gethash item-name (mem-store p)))))

;; and keep it moving: a new value every minute -> the chart appends live
(defrule "Demo: wiggle outside-temp"
  :when-cron '(:minute :every)
  :do (lambda (trigger)
        (declare (ignore trigger))
        (set-item-value 'outside-temp
                        (round (+ 15 (* 8 (sin (/ (get-universal-time) 1800.0))))))))

;; pages (optional): explicitly composed UI pages, each served at its own URL.
;; Without any `defpage' the UI serves the auto-generated itemgroup overview
;; at "/".  Pages and the overview coexist: this example keeps the overview on
;; "/" and adds two extra pages.

(defpage 'wall-panel "Wall panel"
  :path "/wall"
  :title "Wall Panel"
  (section "Lights"
    (toggle 'switch1)
    (value 'motion-sensor :label "Motion"))
  (section "Climate"
    (value 'outside-temp :format "~d °C")
    (setpoint 'outside-temp :min -20 :max 40 :step 1)
    ;; renders a "No history available" placeholder unless a historic
    ;; persistence (e.g. influx) is defined and attached to the item
    (chart 'outside-temp :range '(:hours 6)))
  (section "Energy"
    (slider 'heizstab-wd1-energy :min 0 :max 100 :step 1)
    (text-input 'plug1 :label "Plug name"))
  ;; buttons are not bound to an item: they run a function.  That is what
  ;; momentary commands need -- `set-item-value' is a no-op when the value
  ;; does not change, so a toggle cannot send the same command twice.
  (section "Commands"
    (button-group "Outside temperature"
      (button "Colder" 'demo-colder)
      (button "Warmer" 'demo-warmer))
    ;; the symbol is resolved on every click, so redefining `demo-warmer'
    ;; from the REPL takes effect without re-evaluating the page
    (button "Reset to 15 °C" (lambda () (set-item-value 'outside-temp 15))
            :label "Outside temperature"))
  (page-link 'cellar))

(defun demo-colder ()
  (set-item-value 'outside-temp (1- (get-item-valueq 'outside-temp))))

(defun demo-warmer ()
  (set-item-value 'outside-temp (1+ (get-item-valueq 'outside-temp))))

(defpage 'cellar "Cellar"
  :title "Cellar"
  ;; embeds an existing itemgroup, rendered as on the overview
  (itemgroup-ref 'plugs))
