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

(test make-hash--data-as-string--no-salt
  (is (stringp (make-hash "test"))))

(test make-hash--data-as-array--no-salt
  (is (stringp (make-hash (babel:string-to-octets "test")))))

(test make-hash--data--salt-as-string
  (is (stringp (make-hash "test" "salt"))))

(test make-hash--data--salt-as-array
  (is (stringp (make-hash "test" (babel:string-to-octets "test")))))

(test make-random-data
  (is (arrayp (make-random-data 10)))
  (is (= (length (make-random-data 10)) 10)))

(test make-random-string
  (is (stringp (make-random-string 10))))
