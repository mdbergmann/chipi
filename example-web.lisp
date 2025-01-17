(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this pulls all the dependencies.
  ;; you may use them in here
  ;; `chipi-web' depends on `chipi'
  (ql:quickload :chipi-web))

;; we want our own package to be used
;; this allows us to `:use' namespaces we need
(defpackage :chipi-example
  (:use :cl :hab))
(in-package :chipi-example)

;; the first thing that needs be dine is setup the environment
;; `DEFCONFIG' sets up the 'runtime' folder within 'chipi' system root folder,
;; it starts actor systems, timers and cron services

(defconfig
  ;; at this point we can rely on item system `isys', `timers' and `cr' (cron) to be available
  ;; and that a 'runtime' folder was created

  ;; for chipi-web we need to setup additional things
  
  ;; 1. setup the `api-env' environment, this creates some crucial runtime data
  ;; 2. setup the api-key store
  ;;    we just use a simple file store as there may not be many api-keys (for now)
  ;;    you may change api-key life-time (optional)
  (api-env:init :apikey-store (apikey-store:make-simple-file-backend)
                :apikey-lifetime (ltd:duration :days 100))

  ;; maybe create additional api-keys with different access-rights
  ;; see (apikey-store:create-apikey)
  
  ;; 3. start the API server
  (api:start)
  
  )
