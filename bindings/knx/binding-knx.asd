(defsystem "binding-knx"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi"
               "knx-conn"
               )
  :components ((:file "binding-knx"))
  :in-order-to ((test-op (test-op "binding-knx/tests"))))

(defsystem "binding-knx/tests"
  :author "Manfred Bergmann"
  :depends-on ("binding-knx"
               "fiveam"
               "cl-mock"
               )
  :components ((:file "binding-knx-test")
               )
  :description "Test system for knx binding"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:binding-knx.tests))))

#|
TODOs:

- don't push value when received via listener
- implement read request via pull (timed pull)
- implement write via push
- test initialization of knx

|#