(defsystem "chipi-web"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("chipi"
               "hunchentoot"
               "drakma"
               "cl-ppcre"
               "ironclad/kdf/scrypt"
               "ironclad/kdf/bcrypt"
               "ironclad/mac/hmac"
               "ironclad/digest/sha256"
               "cl-base64"
               "com.inuoe.jzon"
               "marshal"
               "snooze")
  :components ((:module "src/web"
                :serial t
                :components
                ((:file "endecode")
                 (:file "cryp")
                 (:file "apikey-store")
                 (:file "items-controller")
                 (:file "auth-controller")
                 (:file "api-env")
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
                 (:file "apikey-store-test")
                 (:file "auth-controller-test")
                 (:file "items-controller-test")
                 (:file "api-integ-test")
                 )))
  :description "Test system for chipi-web, the web API for chipi."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-web.tests))))

#|

TODOS:

OK - implement additional 'controller' layer for auth, items, etc.
OK - setup runtime folder in 'system' folder => chipi.envi
  -> (print (asdf:system-relative-pathname "chipi-web" ""))
OK - generate (and read) salt in runtime folder
OK - store users in runtime folder
OK - make sure user storage is thread-safe
  ? (we are not storing many users, it should only be done on bootstrap) 
OK - initialize environment (chipi.env) on startup, if it isn't already
  -> Checkout api-integ-test.lisp for example
OK - make more abstractions in api-env to setup token, user-store, token lifetime, etc.
OK - make tokens more long-lifed => 30d
OK - remove user-store and rework apikey-store to API-Key store
  -> we're only dealing with long-lifed api-keys
OK - add change item value via api
=> - convert to the right value type (string, number, boolean) in API when updating value
- generate the right value type in API when converting item plist to json (boolean in particular)
- validate and length check the parameters for post request
- have a thread, or actor that cleans up expired tokens via scheduler
  -> manage apikey backend via Actor altogether
- hardening token storage by sha256/HMAC
- access-control
- audit log
- pre-flight?
- CORS headers

|#
