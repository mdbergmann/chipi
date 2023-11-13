(defsystem "cl-hab"
  :version "0.1.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("alexandria"
               "sento"
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
                 (:file "persistence-simple")
                 (:file "persistence-influx")
                 (:file "rule")
                 (:file "persistence")
                 (:module "bindings"
                  :components
                  ((:file "base-binding")))
                 (:file "hab")
                 )))
  :in-order-to ((test-op (test-op "cl-hab/tests"))))

(defsystem "cl-hab/tests"
  :author "Manfred Bergmann"
  :depends-on ("cl-hab"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "test"
                :components
                ((:file "all-tests")
                 (:file "binding-test")
                 (:file "item-test")
                 (:file "rule-test")
                 (:file "persistence-test")
                 (:file "persistence-influx-test")
                 (:file "hab-test")
                 )))
  :description "Test system for cl-hab"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-hab.tests))))


#|
hab:

- error in push should still persist value

|#
