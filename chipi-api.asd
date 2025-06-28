(defsystem "chipi-api"
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
               "marshal"
               "snooze")
  :components ((:module "src/api"
                :serial t
                :components
                ((:file "endecode")
                 (:file "cryp")
                 (:file "apikey-store")
                 (:file "items-controller")
                 (:file "itemgroups-controller")
                 (:file "auth-controller")
                 (:file "sse-stream-utils")
                 (:file "sse-manager")
                 (:file "events-controller")
                 (:file "api-env")
                 (:file "api")
                  )))
  :in-order-to ((test-op (test-op "chipi-api/tests"))))

(defsystem "chipi-api/tests"
  :author "Manfred Bergmann"
  :depends-on ("chipi-api"
               "fiveam"
               "cl-mock"
               "split-sequence"
               )
  :components ((:module "test/api"
                :components
                ((:file "all-tests")
                 (:file "sse-fake-stream")
                 (:file "apikey-store-test")
                 (:file "auth-controller-test")
                 (:file "items-controller-test")
                 (:file "itemgroups-controller-test")
                 (:file "sse-manager-test")
                 (:file "sse-stream-utils-test")
                 (:file "events-controller-test")
                 (:file "api-integ-test")
                 )))
  :description "Test system for chipi-api, the web API for chipi."
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:chipi-api.tests))))

(defun add-dev-dirs ()
  (push #P"~/Development/MySources/cl-hab/" asdf:*central-registry*)
  (push #P"~/Development/MySources/cl-gserver/" asdf:*central-registry*)
  )

#|

TODOS:

OK - implement additional 'controller' layer for auth, items, etc.
OK - setup runtime folder in 'system' folder => chipi.envi
  -> (print (asdf:system-relative-pathname "chipi-api" ""))
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
OK - convert to the right value type (string, number, boolean) in API when updating value
OK - generate the right value type in API when converting item plist to json (boolean in particular)
OK - validate and length check the parameters for post request
? have a thread, or actor that cleans up expired tokens via scheduler
  -> manage apikey backend via Actor altogether
OK - provide method to retrieve expired apikeys and revoke api key
OK - hardening token storage by sha256/HMAC
OK - apikey-store should raise condition if apikey signature is not correct, catch in auth-controller
OK - refactor apikey-store return values indicating success or failure
OK - refactor apiley-store to raise condition if structure invalid or wrongly signed key
OK - access-control
- documentation
- audit log
- abstract json to separate package
- pre-flight?
- CORS headers
- replace marshal for json representation

|#
