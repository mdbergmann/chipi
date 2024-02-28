(defpackage :knx-conn.cemi-test
  (:use :cl :fiveam :knx-conn.cemi :address :dpt)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :knx-conn.cemi-test)

(def-suite cemi-tests
  :description "CEMI tests"
  :in knx-conn.tests:test-suite)

(in-suite cemi-tests)

;; CEMI{messageCode=L_DATA_IND, additionalInfo=AdditionalInfo{bytes=}, controlByte1=ControlByte1{standardFrame=true, repeatEnabled=false, broadcastType=NORMAL, priority=LOW, requestAcknowledge=false, errorConfirmation=false}, controlByte2=ControlByte2{addressType=GROUP, hopCount=5, frameFormat=0}, sourceAddress=IndividualAddress{address=1.3.14}, destinationAddress=GroupAddress{address=1034, address(2-level)=0/1034, address(3-level)=0/4/10}, npduLength=3, tpci=UNNUMBERED_PACKAGE, packetNumber=0, apci=GROUP_VALUE_WRITE, data=0x0C 68}

(defparameter *l-data* #(41 0 188 208 19 14 4 10 3 0 128 12 104))

(test parse-l-data.ind
  (let ((cut (parse-cemi *l-data*)))
    (is (typep cut 'cemi-l-data))
    (is (= (cemi-message-code cut) +cemi-mc-l_data.ind+))
    
    (is (equal (cemi-ctrl1 cut) #*10111100))
    (is (equalp `(:standard-frame t
                  :repeat-enabled nil
                  :broadcast-type ,+broadcast-type-normal+
                  :priority ,+priority-low+
                  :ack-requested nil
                  :error-confirmation nil)
                (ctrl1-rep cut)))

    (is (equal (cemi-ctrl2 cut) #*11010000))
    (is (equalp `(:address-type knx-group-address
                  :hop-count 5
                  :frame-format 0)
                (ctrl2-rep cut)))

    (is (typep (cemi-source-addr cut) 'knx-individual-address))
    (is (typep (cemi-destination-addr cut) 'knx-group-address))
    (is (string= (address-string-rep
                  (cemi-source-addr cut)) "1.3.14"))
    (is (string= (address-string-rep
                  (cemi-destination-addr cut)) "0/4/10")) ; Temp_EG_Az

    (is (equalp (cemi-tpci cut) +tcpi-udt+))
    (is (= (cemi-packet-num cut) 0))
    (is (apci-gv-write-p (cemi-apci cut)))
    (is (equalp #(12 104) (cemi-data cut)))
    ))

(test make-cemi--default
  (let ((cemi (make-default-cemi
               :message-code +cemi-mc-l_data.req+
               :dest-address (make-group-address "0/4/10")
               :apci (make-apci-gv-write)
               :dpt (make-dpt1 :switch :on))))
    (is-true cemi)
    (is (typep cemi 'cemi-l-data))))

(test cemi-to-bytes--parse-from-bytes
  (let ((cemi (parse-cemi *l-data*)))
    (print cemi)
    (is (equalp *l-data* (knxobj:to-byte-seq cemi))))
  )

(run! 'cemi-tests)
