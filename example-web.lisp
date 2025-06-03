(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this pulls all the dependencies.
  ;; you may use them in here
  ;; `chipi-web' depends on `chipi'
  (asdf:load-system :chipi-web)
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

  ;; for chipi-web we need to setup additional things
  
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

(defitemgroup 'grp1 "My Group 1")
(defitemgroup 'grp2 "My Group 2")

(defitem 'foo "Foo item" 'boolean :initial-value 'item:false
  :group 'grp1
  :group 'grp2)
(defitem 'foo-1 "Foo item float" 'float :initial-value 10.4
  :group 'grp1)
(defitem 'foo-2 "Foo item string" 'string :initial-value "my string value"
  :group 'grp2)
(defitem 'foo-3 "Foo item integer" 'integer :initial-value -123
  :group 'grp1)
(defitem 'foo-4 "Foo item unknown type" nil :initial-value 12)
