(defpackage :chipi-web.cryp-test
  (:use :cl :fiveam :chipi-web.cryp)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.cryp-test)

(def-suite cryp-tests
  :description "Tests for crypto helpers"
  :in chipi-web.tests:test-suite)

(in-suite cryp-tests)

(test create-hash--data-as-string--no-salt
  (is (stringp (create-hash "test"))))

(test create-hash--data-as-array--no-salt
  (is (stringp (create-hash (babel:string-to-octets "test")))))

(test create-hash--data--salt-as-string
  (is (stringp (create-hash "test" "salt"))))

(test create-hash--data--salt-as-array
  (is (stringp (create-hash "test" (babel:string-to-octets "test")))))

(test create-random-data
  (is (arrayp (create-random-data 10)))
  (is (= (length (create-random-data 10)) 10)))

(test create-random-string
  (is (stringp (create-random-string 10))))
