(defsystem "chipi-ui"
  :version "0.1.10"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi-api"
               "clog")
  :components ((:module "src/ui"
                :serial t
                :components
                ((:file "page")
                 (:file "settings")
                 (:file "webapp")
                 (:file "page-renderer")
                 (:file "main")
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
                :serial t
                :components
                ((:file "all-ui-tests")
                 (:file "clog-test-utils")
                 (:file "page-test")
                 (:file "main-test")
                 (:file "render-test")
                 (:file "widget-render-test")
                 (:file "settings-test"))))
  :description "Test system for chipi-ui."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-ui.tests))))
