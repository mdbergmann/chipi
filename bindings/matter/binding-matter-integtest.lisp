(defpackage :chipi.binding.matter-integtest
  (:use :cl :fiveam :chipi.binding.matter :cl-matter)
  (:import-from #:sento.miscutils
                #:await-cond)
  (:import-from #:chipi.binding.matter-test
                #:bridge-env
                #:%item-value)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.matter-integtest)

(def-suite matter-binding-integtests
  :description "Matter binding: a cl-matter commissioner commissions and controls the bridge over loopback UDP"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-binding-integtests)

;; The bridge runs on its actor, on an ephemeral IPv4 loopback port; the
;; commissioner -- what Apple Home is to the bridge -- is cl-matter's own,
;; polling its manager on the test thread.  No hardware, no network beyond
;; loopback, no mDNS: the commissioner knows the port.

(defun %bridge-port ()
  (nth-value 1 (transport-local-endpoint
                (exchange-manager-transport
                 (binding-matter::bridge-manager binding-matter::*bridge*)))))

(defun %read (model session endpoint cluster attribute)
  (attribute-report-value
   (first (read-attributes model session (list (attribute-path endpoint cluster attribute))))))

(test bridge--commissioned-and-controlled-over-udp
  "A commissioner commissions the bridge, finds the light under the
aggregator, switches it on -- the item follows -- and reads the item's
change back."
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall" (matter-bridge))
    (let* ((transport (make-udp-transport :address "127.0.0.1"))
           (manager (make-exchange-manager :transport transport))
           (channel (make-secure-channel manager))
           (model (make-interaction-model manager))
           (commissioner (make-commissioner channel model :fabric-id #x5A)))
      (unwind-protect
           (let* ((result (commission commissioner "127.0.0.1" (%bridge-port)
                                      :passcode 20202021 :country-code "AT" :timeout 90))
                  (session (getf result :session)))
             (is (eq :case (session-type session)))
             (is (= 1 (length (device-fabrics (binding-matter::bridge-device binding-matter::*bridge*)))))
             ;; the state file was written
             (is-true (probe-file (merge-pathnames "bridge.state" binding-matter::*storage*)))
             ;; the root: a bridge named Chipi
             (is (string= "Chipi" (%read model session 0 +cluster-basic-information+ 5)))
             (is (equal (list (list :device-type +device-type-aggregator+ :revision 1))
                        (%read model session 1 +cluster-descriptor+ 0)))
             ;; the aggregator lists the light, which names itself
             (is (equal '(2) (%read model session 1 +cluster-descriptor+ +attribute-parts-list+)))
             (is (string= "Hall" (%read model session 2 +cluster-bridged-device-basic-information+ 5)))
             (is (eq t (%read model session 2 +cluster-bridged-device-basic-information+ 17)))
             (is (eq nil (%read model session 2 +cluster-on-off+ 0)))
             ;; On from the administrator switches the item
             (invoke-command model session (command-path 2 +cluster-on-off+ 1) nil)
             (is-true (await-cond 3 (eq 'item:true (%item-value 'light-hall))))
             (is (eq t (%read model session 2 +cluster-on-off+ 0)))
             ;; the item switched off at home reads off
             (hab:set-item-value 'light-hall 'item:false)
             (is-true (await-cond 3 (eq nil (%read model session 2 +cluster-on-off+ 0))))
             ;; a device added while commissioned shows up in the parts list
             (house:defplug plug-garden "Garden" (matter-bridge))
             (is-true (await-cond 3 (equal '(2 3) (%read model session 1 +cluster-descriptor+ +attribute-parts-list+))))
             (is (string= "Garden" (%read model session 3 +cluster-bridged-device-basic-information+ 5)))
             ;; a heating of two items is one Thermostat; the administrator writes
             ;; its setpoint and the setpoint item follows
             (house:defheating bath "Bath"
               :temperature ((matter-bridge :device bath))
               :setpoint ((matter-bridge :device bath)))
             (is-true (await-cond 3 (equal '(2 3 4) (%read model session 1 +cluster-descriptor+ +attribute-parts-list+))))
             (is (string= "Bath" (%read model session 4 +cluster-bridged-device-basic-information+ 5)))
             (is (equal (list (list :device-type +device-type-thermostat+ :revision 2)
                              (list :device-type +device-type-bridged-node+ :revision 2))
                        (%read model session 4 +cluster-descriptor+ 0)))
             (let ((statuses (write-attributes model session (list (attribute-write 4 +cluster-thermostat+ #x12 2250)))))
               (is (im-status-success-p (attribute-status-status (first statuses)))))
             (is-true (await-cond 3 (eql 22.5 (%item-value 'bath-setpoint))))
             (is (= 2250 (%read model session 4 +cluster-thermostat+ #x12)))
             (close-session channel session))
        (exchange-manager-close manager)))))
