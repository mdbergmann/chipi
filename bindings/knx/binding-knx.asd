(defsystem "binding-knx"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi"
               "knx-conn")
  :components ((:file "binding-knx"))
  :in-order-to ((test-op (test-op "binding-knx/tests"))))

(defsystem "binding-knx/tests"
  :author "Manfred Bergmann"
  :depends-on ("binding-knx"
               "fiveam"
               "cl-mock")
  :components ((:file "../binding-test-suite")
               (:file "binding-knx-test-suite")
               (:file "binding-knx-test")
               (:file "binding-knx-integtest"))
  :description "Test system for knx binding"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi.binding.knx-test-suite))))

#|
TODOs:

OK - don't push value when received via listener
OK - implement read request via pull (timed pull)
OK - implement write via push
OK - implement more value types for push
OK - rework pull tests to just work with 'exec-pull'
OK - test initialization of knx

|#
