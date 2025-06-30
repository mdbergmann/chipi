(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this pulls all the dependencies.
  ;; you may use them in here
  ;; `chipi-api' depends on `chipi'
  ;;(asdf:load-system :chipi-api)
  )

;; we want our own package to be used
;; this allows us to `:use' namespaces we need
(defpackage :chipi-example
  (:use :cl :hab))
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
  (api:start)
  
  )

;; items here

(defitemgroup 'plugs "Plugs")
(defitemgroup 'lights "Lights")
(defitemgroup 'switches "Switches")
(defitemgroup 'temps "Temperatures")

(defitem 'switch1 "Switch1" 'boolean :initial-value 'item:false
  :group '(plugs lights switches))
(defitem 'light1 "Light1" 'float :initial-value 123.456
  :group '(lights))
(defitem 'plug1 "Plug1" 'string :initial-value 'item:false
  :group '(plugs))
(defitem 'outside-temp "Outside temperature" 'integer :initial-value -123
  :group '(temps))
(defitem 'event-1 "Event 1" nil :initial-value "Event occured"
  :tags '((:ext-readonly . t)))
