(defsystem "binding-matter"
  :version "0.3.2"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "Matter binding for Chipi. The bridge role: Chipi is a Matter bridge whose bridged devices are items, for Apple Home, Google Home and the like. The controller role follows."
  :depends-on ("chipi"
               "cl-matter"
               "babel"
               "bordeaux-threads")
  :components ((:file "package")
               (:file "matter-actors")
               (:file "matter-convert")
               (:file "qr-code")
               (:file "matter-bridge")
               (:file "binding-matter"))
  :in-order-to ((test-op (test-op "binding-matter/tests"))))

(defsystem "binding-matter/ui"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :description "The Matter bridge's card in chipi-ui's settings: pairing code and QR, the pairing window, the apps that added the bridge, the bridged devices. Load it next to chipi-ui."
  :depends-on ("binding-matter"
               "chipi-ui")
  :components ((:file "matter-ui")))

(defsystem "binding-matter/tests"
  :author "Manfred Bergmann"
  :depends-on ("binding-matter"
               "fiveam"
               "cl-mock")
  :components ((:file "../binding-test-suite")
               (:file "binding-matter-test-suite")
               (:file "binding-matter-test")
               (:file "qr-code-test")
               (:file "binding-matter-integtest"))
  :description "Test system for the Matter binding"
  :perform (test-op (op c)
                    (symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* '#:test-suite
                                        '#:chipi.binding.matter-test-suite))))

(defsystem "binding-matter/ui-tests"
  :author "Manfred Bergmann"
  :depends-on ("binding-matter/ui"
               "binding-matter/tests"
               "chipi-ui/tests"
               "fiveam"
               "cl-mock")
  :components ((:file "matter-ui-test"))
  :description "Test system for the Matter bridge's settings card; needs chipi-ui"
  :perform (test-op (op c)
                    (symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* '#:matter-ui-tests
                                        '#:chipi.binding.matter.ui-test))))

#|
cl-matter is not on OCICL. Make it and this plugin visible to ASDF, from the
chipi root, with the sibling checkouts:

(let ((cwd (uiop:getcwd)))
  (push (merge-pathnames "../cl-matter/" cwd) asdf:*central-registry*)
  (asdf:load-asd (merge-pathnames "bindings/matter/binding-matter.asd" cwd) :name "binding-matter")
  (asdf:load-system :binding-matter/tests))

or run `scripts/review/run-tests.sh matter', which also runs the UI tests
(`binding-matter/ui-tests', loading chipi-ui).
|#
