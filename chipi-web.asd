(defsystem "chipi-web"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi")
  :components ((:module "src/web"
                :serial t
                :components
                (;;(:file "cron")
                  
                  )))
  :in-order-to ((test-op (test-op "chipi-web/tests"))))

(defsystem "chipi-web/tests"
  :author "Manfred Bergmann"
  :depends-on ("chipi-web"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "test/web"
                :components
                ((:file "all-tests")
                 )))
  :description "Test system for chipi-web, the web API for chipi."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-web.tests))))


#|
todos:

|#
