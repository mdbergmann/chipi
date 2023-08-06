(defsystem "cl-hab"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("alexandria"
               "sento"
               "timer-wheel"
               "cl-cron"
               "yason")
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
                 (:file "persistence-simple")
                 (:file "item")
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
                 (:file "hab-test")
                 )))
  :description "Test system for cl-hab"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-hab.tests))))


#|
hab:

OK - bind-item-delay should set value to all bound items
OK - separate bindings, create bindings folder
OK - place function-binding there
OK - separate item
OK - item should be able to push to binding (item needs reference to binding)
OK - binding can be either pull or push or both
OK - dsl for creating items with or without bindings
OK - control if set-value of 'pull' should be passed through to 'push'.
OK - more pipeline functions: transform of 'retrieved' value
OK - scripts also a dedicated actor with custom dsl for registering to cron and item changes
OK - cron for scripts/rulos?
OK - make item macro to be called on top-level so that it can be evaluated repeatedly and can take changes on runtime info account
OK - do proper cleanup of item with bindings and rules
- execute `exec-pull/push' using tasks?
- binding more abstract, method protocol?
- create http-binding?
OK - persistence based on items: :load :save, :load called on init, :save called on each item value changed
=> - store item value along with timestamp (universal-time)

|#
