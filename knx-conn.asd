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
                 (:file "descr-info")
                 (:file "knx-connect")
                 )))
  :in-order-to ((test-op (test-op "knx-conn/tests"))))

(defsystem "knx-conn/tests"
  :author "Manfred Bergmann"
  :depends-on ("knx-conn"
               "try"
               "cl-mock"
               )
  :components ((:module "knx-test"
                :components
                ((:file "knx-connect-test")
                 (:file "test-all")
                 )))
  :description "Test system for knx"
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:knx-conn.test-all '#:test)))
