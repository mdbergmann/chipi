(defpackage :knx-conn.cemi-test
  (:use :cl :fiveam :knx-conn.cemi :address)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.cemi-test)

(def-suite cemi-tests
  :description "CEMI tests"
  :in knx-conn.tests:test-suite)

(in-suite cemi-tests)

(defparameter *l-data* #(41 0 188 208 19 14 4 10 3 0 128 12 104))

(test parse-l-data.ind
  (let ((cut (parse-cemi *l-data*)))
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.ind+))
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equal (cemi-ctrl2 cut) #*11010000))
    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "1.3.14"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/4/10"))
    (is (equalp (cemi-npdu cut) #(0 128 12)))
    (is (equalp (cemi-tpci cut) +tcpi-ucd+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-read-p (cemi-apci cut)))
    (is (null (cemi-data cut)))
    ))
