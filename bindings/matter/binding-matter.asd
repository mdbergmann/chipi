(defsystem "binding-matter"
  :version "0.1.0"
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
               (:file "matter-bridge")
               (:file "binding-matter"))
  :in-order-to ((test-op (test-op "binding-matter/tests"))))

(defsystem "binding-matter/tests"
  :author "Manfred Bergmann"
  :depends-on ("binding-matter"
               "fiveam"
               "cl-mock")
  :components ((:file "../binding-test-suite")
               (:file "binding-matter-test-suite")
               (:file "binding-matter-test")
               (:file "binding-matter-integtest"))
  :description "Test system for the Matter binding"
  :perform (test-op (op c)
                    (symbol-call
                     :fiveam :run!
                     (uiop:find-symbol* '#:test-suite
                                        '#:chipi.binding.matter-test-suite))))

#|
cl-matter is not on OCICL. Make it and this plugin visible to ASDF, from the
chipi root, with the sibling checkouts:

(let ((cwd (uiop:getcwd)))
  (push (merge-pathnames "../cl-matter/" cwd) asdf:*central-registry*)
  (asdf:load-asd (merge-pathnames "bindings/matter/binding-matter.asd" cwd) :name "binding-matter")
  (asdf:load-system :binding-matter/tests))

or run `scripts/review/run-tests.sh matter'.
|#
