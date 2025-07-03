(defsystem "chipi-ui"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi-api"
               "clog")
  :components ((:module "src/ui"
                :serial t
                :components
                ((:file "main")
                 (:file "ui-env")
                  )))
  :in-order-to ((test-op (test-op "chipi-ui/tests"))))

(defsystem "chipi-ui/tests"
  :author "Manfred Bergmann"
  :depends-on ("chipi-ui"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "test/ui"
                :components
                ()))
  :description "Test system for chipi-ui."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-ui.tests))))
