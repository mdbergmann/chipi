(defsystem "binding-knx"
  :version "0.2.0"
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
               "cl-mock"
               "usocket-server")
  :components ((:file "../binding-test-suite")
               (:file "binding-knx-test-suite")
               (:file "binding-knx-test")
               (:file "binding-knx-integtest"))
  :description "Test system for knx binding"
  :perform (test-op (op c)
                    (symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* '#:test-suite
                                        '#:chipi.binding.knx-test-suite))))

#|
load with:
(let* ((cwd (namestring (uiop:getcwd)))
         (plugin-asd (format nil "~a~a" cwd "bindings/knx/binding-knx.asd")))
    (asdf:load-asd plugin-asd :name "binding-knx")
    (asdf:load-system :binding-knx/tests))

TODOs:

OK - don't push value when received via listener
OK - implement read request via pull (timed pull)
OK - implement write via push
OK - implement more value types for push
OK - rework pull tests to just work with 'exec-pull'
OK - test initialization of knx
OK - more integration tests
OK - shutdown hook?
OK - write value test
OK - documentation
OK - %make-binding-pull-fun return future which is set as is to item.
OK - pull-fun should not set item values itself.
|#
