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
  ;; This is optional if you don't require an API server. Not required for UI.
  (api:start)

  ;; 4. Initialize and start UI. This starts the UI web server.
  ;; UI is optional as well as API server is optional,
  ;; but apt-env setup (2.) with API keys is mandatory for UI.
  (ui:start :host "localhost" :port 8080)
  )

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
(defitem 'outside-temp "Outside temperature" 'integer :initial-value -123
  :group '(temps))
(defitem 'heizstab-wd1-energy "Heizstab Wendel 1 Energie [Wh]" 'float :initial-value 8.4)
(defitem 'event-1 "Event 1" nil :initial-value "Event occured"
  :tags '((:ext-readonly . t)))
