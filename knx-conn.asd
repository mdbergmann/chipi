(defsystem "knx-conn"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("alexandria"
               "usocket"
               "babel"
               "log4cl")
  :components ((:module "knx-src"
                :serial t
                :components
                ((:file "utils")
                 (:file "knx-obj")
                 (:file "hpai")
                 (:file "dib")
                 (:file "cri")
                 (:file "crd")
                 (:file "cemi")
                 (:file "descr-info")
                 (:file "connect")
                 (:file "tunneling")
                 (:file "knx-connect")
                 )))
  :in-order-to ((test-op (test-op "knx-conn/tests"))))

(defsystem "knx-conn/tests"
  :author "Manfred Bergmann"
  :depends-on ("knx-conn"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "knx-test"
                :components
                ((:file "test-all")
                 (:file "knx-connect-test")
                 )))
  :description "Test system for knx"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:knx-conn.tests))))
