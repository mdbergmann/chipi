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
  
  ;; 1. setup the `api-env' environment, this creates some crucial runtime data, like password salts, etc.
  ;;    this should not be deleted when users were created. Creating a new password salt will
  ;;    invalidate all existing user passwords
  ;; 2. setup the token store
  ;;    we just use an  in `memory' store. Since tokens are short lifed anyway, this is fine.
  ;;    you may change token life-time (optional)
  ;; 3. setup the user store
  ;;    the system is not geared towards many users, maybe just one that uses the API
  ;;    the simple-file-backend will just store users to a file using `cl-marshal'.
  ;;    however, other backends are possible, like a database backend
  (api-env:init :token-store *memory-backend*
                :token-lifetime (ltd:duration :days 30)
                :user-store (user-store:make-simple-file-backend))
  
  ;; 4. we can add a user that primarily should use this API
  (user-store:add-user (user-store:make-user "admin" "the-admin-password"))

  ;; 5. start the API server
  (api:start)
  
  )
