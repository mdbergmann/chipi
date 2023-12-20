(defsystem "chipi-web"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi"
               "hunchentoot"
               "easy-routes"
               "drakma"
               "cl-ppcre"
               "ironclad/kdf/scrypt"
               "ironclad/mac/hmac"
               "ironclad/digest/sha256"
               "cl-base64")
  :components ((:module "src/web"
                :serial t
                :components
                ((:file "endecode")
                 (:file "cryp")
                 (:file "token-store")
                 (:file "api")
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
                 (:file "token-store-test")
                 (:file "api-integ-test")
                 )))
  :description "Test system for chipi-web, the web API for chipi."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-web.tests))))


#|
todos:

|#
