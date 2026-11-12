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
             ;; the administrator shows up as such, and the window closed with the commissioning
             (let ((admins (matter-bridge-admins)))
               (is (= 1 (length admins)))
               (is (= #x5A (getf (first admins) :fabric-id)))
               (is (= 1 (getf (first admins) :fabric-index))))
             (is-false (matter-bridge-window-open-p))
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

(defun %bridge-side (thunk)
  "`THUNK' in the bridge actor's context."
  (binding-matter::matter-actor-call (binding-matter::bridge-actor binding-matter::*bridge*) thunk))

(test bridge--subscriber-is-kept-informed
  "A hub's subscription: the bridge grants its own, short report interval
whatever the hub asks for, tells the hub a device added later as a whole,
and has a line for the log about who came and went."
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall" (matter-bridge))
    (let* ((transport (make-udp-transport :address "127.0.0.1"))
           (manager (make-exchange-manager :transport transport))
           (channel (make-secure-channel manager))
           (model (make-interaction-model manager))
           (commissioner (make-commissioner channel model :fabric-id #x5A))
           (reports '()))
      (unwind-protect
           (let* ((session (getf (commission commissioner "127.0.0.1" (%bridge-port)
                                             :passcode 20202021 :country-code "AT" :timeout 90)
                                 :session))
                  ;; everything, 0..600 s, as Apple Home's hub subscribes
                  (subscription (subscribe-attributes
                                 model session (list (attribute-path nil nil nil))
                                 :min-interval 0 :max-interval 600
                                 :on-report (lambda (subscription attributes)
                                              (declare (ignore subscription))
                                              (setf reports (append reports attributes))))))
             (is (= 60 binding-matter::*bridge-max-report-interval*))
             (is (= 60 (subscription-max-interval subscription)))
             ;; the bridge's side of it, and what the log says
             (let* ((served (%bridge-side
                             (lambda ()
                               (first (interaction-model-subscriptions
                                       (device-model (binding-matter::bridge-device binding-matter::*bridge*)))))))
                    (peer (binding-matter::%peer-text (server-subscription-session served))))
               (is (= 60 (server-subscription-max-interval served)))
               (is (search "of fabric 000000000000005A at 127.0.0.1" peer))
               (is (string= (format nil "~a subscribed to 1 path, a report at least every 60 s" peer)
                            (binding-matter::%subscription-event-text :subscribed served)))
               (is (string= (format nil "subscription of ~a ended: replaced" peer)
                            (binding-matter::%subscription-event-text :dropped served "replaced")))
               (is (string= (format nil "session opened by ~a" peer)
                            (binding-matter::%session-event-text :opened (server-subscription-session served))))
               (is (string= (format nil "session closed: ~a" peer)
                            (binding-matter::%session-event-text :closed (server-subscription-session served)))))
             ;; a device defined now reaches the subscriber whole, without a read
             (house:defplug plug-garden "Garden" (matter-bridge))
             (flet ((reported (cluster attribute)
                      (find (attribute-path 3 cluster attribute) reports
                            :key #'attribute-report-path :test #'equalp)))
               (is-true (await-cond 5
                          (progn
                            (exchange-manager-poll manager :timeout 0.05)
                            (and (reported +cluster-bridged-device-basic-information+ 5)
                                 (reported +cluster-descriptor+ 0)
                                 (reported +cluster-on-off+ 0)))))
               (is (string= "Garden" (attribute-report-value
                                      (reported +cluster-bridged-device-basic-information+ 5))))
               (is (eq nil (attribute-report-value (reported +cluster-on-off+ 0)))))
             (close-session channel session))
        (exchange-manager-close manager)))))

(test bridge--failed-attempts-have-a-log-line
  (let ((pase (cl-matter::%make-handshake :kind :pase :role :responder))
        (case (cl-matter::%make-handshake :kind :case :role :responder)))
    (is (string= "pairing attempt failed: wrong passcode"
                 (binding-matter::%handshake-failure-text pase "wrong passcode")))
    (is (string= "session attempt failed: no shared root"
                 (binding-matter::%handshake-failure-text case "no shared root")))))
