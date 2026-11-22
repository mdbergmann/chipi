(defsystem "binding-mqtt"
  :version "0.1.0"
  :description "MQTT transport and item bindings for Chipi"
  :license "Apache-2"
  :depends-on ("chipi" "cl-mqtt" "bordeaux-threads" "serapeum" "yason"
               "log4cl-extras" "trivial-types")
  :serial t
  :components ((:file "package") (:file "transport") (:file "binding"))
  :in-order-to ((test-op (test-op "binding-mqtt/tests"))))

(defsystem "binding-mqtt/tests"
  :depends-on ("binding-mqtt" "fiveam" "cl-mock")
  :serial t
  :components ((:file "binding-test"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :fiveam :run! :mqtt-tests)
               (error "MQTT tests failed: ~A" c))))
