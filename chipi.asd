(defsystem "chipi"
  :version "0.4.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("alexandria"
               (:version "sento" "3.3.0")
               "timer-wheel"
               "cl-cron"
               "yason"
               "drakma"
               "local-time"
               "binding-arrows"
               "parse-float")
  :components ((:module "src"
                :serial t
                :components
                ((:file "cron")
                 (:file "cl-cron-overrides")
                 (:file "timer")
                 (:file "isys")
                 (:file "env")
                 (:file "binding-api")
                 (:file "persistence-api")
                 (:file "item")
                 (:file "itemgroup")
                 (:file "persistence-simple")
                 (:file "persistence-influx")
                 (:file "rule")
                 (:file "persistence")
                 (:module "bindings"
                  :components
                  ((:file "base-binding")))
                 (:file "hab")
                 )))
  :in-order-to ((test-op (test-op "chipi/tests"))))

(defsystem "chipi/tests"
  :author "Manfred Bergmann"
  :depends-on ("chipi"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "test"
                :components
                ((:file "all-tests")
                 (:file "binding-test")
                 (:file "item-test")
                 (:file "itemgroup-test")
                 (:file "rule-test")
                 (:file "persistence-test")
                 (:file "persistence-influx-test")
                 (:file "env-test")
                 (:file "hab-test")
                 )))
  :description "Test system for chipi"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi.tests))))


#|
hab:
OK - test for shutdown-hook
OK - allow `pull-fun` to decide on calling push via returning (values val '(:pull))
OK - allow `pull-fun` to return future which is then just `fcompleted`.
OK - implement itemgroups
- itemgroups: allow group in group
- replace yason code with jzon

- check:
; in: CHIPI.HAB:DEFITEMGROUP 'CHIPI.HAB-TEST::GROUP1
;     (CHIPI.HAB:DEFITEMGROUP 'CHIPI.HAB-TEST::GROUP1 "Group1")
; 
; caught WARNING:
;   undefined variable: #:|old-group-items12|
; 
; caught WARNING:
;   undefined variable: #:|old-group-items45|
; 
; caught WARNING:
;   undefined variable: #:|old-group-items6|

|#
