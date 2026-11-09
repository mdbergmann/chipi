(defpackage :chipi.binding.matter
  (:use :cl)
  (:nicknames :binding-matter)
  (:local-nicknames (:matter :cl-matter))
  (:import-from #:alexandria
                #:remove-from-plist
                #:when-let
                #:when-let*)
  (:export #:matter-init
           #:matter-shutdown
           ;; the bridge role: items as bridged devices of Chipi's Matter bridge
           #:matter-bridge
           #:make-matter-bridge-binding
           #:matter-bridge-binding
           #:start-bridge
           #:stop-bridge
           #:matter-bridge-running-p
           #:matter-bridge-pairing-code
           ;; the Matter actors: one per role, polling a cl-matter exchange manager
           #:make-matter-actor
           #:matter-actor-call
           #:matter-actor-run
           #:matter-actor-stop))

(in-package :chipi.binding.matter)

;; The Matter binding plugin.  Chipi plays two Matter roles with it:
;;
;; - as a *bridge* (matter-bridge.lisp) Chipi is a Matter device that Apple
;;   Home, Google Home or Alexa commission, and items -- KNX-bound or
;;   virtual -- are its bridged devices;
;; - as a *controller* (to come) Chipi commissions Matter devices into a
;;   fabric of its own, and they become items.
;;
;; Both roles sit on cl-matter, which is single-threaded and polled; each
;; role owns one Sento actor on a thread of its own that polls its exchange
;; manager (matter-actors.lisp).  The house layer and the primitives know
;; nothing of Matter: `house:deflight' takes a `matter-bridge' form where it
;; takes a `knx-binding'.
