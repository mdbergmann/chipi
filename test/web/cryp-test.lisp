(defpackage :chipi-web.cryp-test
  (:use :cl :endecode :fiveam :chipi-web.cryp)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.cryp-test)

(def-suite cryp-tests
  :description "Tests for crypto helpers"
  :in chipi-web.tests:test-suite)

(in-suite cryp-tests)

(test make-hash--data-as-string--no-salt
  (is (stringp (make-hash "test"))))

(test make-hash--data-as-array--no-salt
  (is (stringp (make-hash (string-to-octets "test")))))

(test make-hash--data--salt
  (is (stringp (make-hash "test" (cryp:make-salt)))))

(test make-random-data
  (is (arrayp (make-random-data 10)))
  (is (= (length (make-random-data 10)) 10)))

(test make-random-string
  (is (stringp (make-random-string 10))))

(test hmac-sign--string-data
  (is (stringp (hmac-sign (cryp:make-random-data 10)
                          "test"))))

