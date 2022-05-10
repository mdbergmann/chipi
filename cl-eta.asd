(defsystem "cl-eta"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("cl-gserver"
               "libserialport"
               )
  :components ((:module "src"
                :serial t
                :components
                ((:file "eta"))))
  :in-order-to ((test-op (test-op "cl-eta/tests"))))

(defsystem "cl-eta/tests"
  :author "Manfred Bergmann"
  :depends-on ("cl-eta"
               "fiveam"
               "cl-mock")
  :components ((:module "test"
                :components
                ((:file "all-tests")
                 (:file "eta-atest")
                 )))
  :description "Test system for cl-eta"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:cl-eta.tests))))
;; load system
;; (asdf:load-system "cl-eta")
;;
;; test system
;; (asdf:test-system "cl-gserver/tests")
;;

#|

TODOs:

|#

