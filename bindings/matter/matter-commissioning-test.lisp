(in-package :chipi.binding.matter-controller-test)

(def-suite matter-commissioning-tests
  :description "Matter binding: pairing codes, the choice of a device, the commissioning job, definers -- without a node"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-commissioning-tests)

(defpackage :chipi.binding.matter-test-config
  (:use :cl :chipi.hab :chipi.house :chipi.binding.matter)
  (:documentation "What a configuration file is read in: where the suggested definers of a test are read and evaluated."))

(defparameter *manual-code* "3497-011-2332"
  "The manual pairing code of discriminator 3840 (short: 15) and passcode 20202021.")

(defun %payload (&rest args)
  (apply #'matter:make-onboarding-payload (append args '(:discriminator 3840 :passcode 20202021))))

(defun %qr-code (&rest args)
  (matter:encode-qr-code (apply #'%payload args)))

(defun %advert (name &rest plist)
  "An advertisement as `%browse-commissionable' hands it out."
  (list* :name name :addresses '("127.0.0.1") :port 5540 plist))

(defun %await-job (job &optional (timeout 10))
  (sento.miscutils:await-cond timeout (not (commissioning-job-running-p job))))

;; -----------------------------
;; pairing codes
;; -----------------------------

(test matter-decode-pairing-code--a-manual-code
  (let ((code (matter-decode-pairing-code *manual-code*)))
    (is (eq :manual (getf code :kind)))
    (is (= 15 (getf code :short-discriminator)))
    (is (null (getf code :discriminator)))
    (is (null (getf code :vendor-id)))
    (is (null (getf code :product-id)))
    (is (= 20202021 (getf code :passcode))))
  ;; dashes and blanks do not count
  (is (equal (matter-decode-pairing-code *manual-code*)
             (matter-decode-pairing-code " 34970112332 "))))

(test matter-decode-pairing-code--a-qr-code
  (let ((code (matter-decode-pairing-code (%qr-code :vendor-id #xFFF1 :product-id #x8000))))
    (is (eq :qr (getf code :kind)))
    (is (= 3840 (getf code :discriminator)))
    (is (= 15 (getf code :short-discriminator)))
    (is (= #xFFF1 (getf code :vendor-id)))
    (is (= #x8000 (getf code :product-id)))
    (is (= 20202021 (getf code :passcode)))))

(test matter-decode-pairing-code--what-is-no-code
  (signals error (matter-decode-pairing-code "3497-011-2333"))   ; the check digit
  (signals error (matter-decode-pairing-code "1234"))
  (signals error (matter-decode-pairing-code "MT:"))
  (signals error (matter-decode-pairing-code ""))
  (signals error (matter-decode-pairing-code 34970112332)))

;; -----------------------------
;; which device
;; -----------------------------

(defun %choose (payload &rest devices)
  (binding-matter::%choose-device payload devices))

(test choose-device--the-one-that-answers
  (let ((lamp (%advert "Lamp" :discriminator 3840)))
    (is (eq lamp (%choose (%payload) lamp)))
    ;; a manual code knows the upper four bits only
    (is (eq lamp (%choose (binding-matter::%decode-code *manual-code*) lamp)))
    ;; one that advertises no discriminator is not ruled out
    (let ((plain (%advert "Plain")))
      (is (eq plain (%choose (%payload) plain))))))

(test choose-device--none-is-a-closed-window
  (let ((condition (handler-case (%choose (%payload))
                     (error (c) c))))
    (is (typep condition 'error))
    (is (search "pairing window" (princ-to-string condition))))
  ;; one with another discriminator is none
  (signals error (%choose (%payload) (%advert "Other" :discriminator 100)))
  (signals error (%choose (binding-matter::%decode-code *manual-code*)
                          (%advert "Other" :discriminator (ash 9 8)))))

(test choose-device--several-are-told-apart-by-vendor-and-product
  (let ((lamp (%advert "Lamp" :discriminator 3840 :vendor-id #xFFF1 :product-id #x8000))
        (plug (%advert "Plug" :discriminator 3840 :vendor-id #xFFF1 :product-id #x8001))
        (other (%advert "Other" :discriminator 3840 :vendor-id #x1234 :product-id #x8000)))
    (is (eq plug (%choose (%payload :vendor-id #xFFF1 :product-id #x8001) lamp plug other)))
    ;; a manual code of 11 digits has neither: no telling
    (let ((condition (handler-case (%choose (binding-matter::%decode-code *manual-code*) lamp plug)
                       (error (c) c))))
      (is (typep condition 'error))
      (is (search "2 devices" (princ-to-string condition)))
      (is (search "Lamp, Plug" (princ-to-string condition)))
      (is (search ":device" (princ-to-string condition))))
    ;; nor do two of one product
    (signals error (%choose (%payload :vendor-id #xFFF1 :product-id #x8000)
                            lamp (%advert "Lamp 2" :discriminator 3840 :vendor-id #xFFF1 :product-id #x8000)))))

(test present-device--a-name-and-what-it-is
  (let ((device (binding-matter::%present-device
                 (list :device-name "Desk lamp" :device-type #x0100 :vendor-id #xFFF1 :product-id #x8000))))
    (is (string= "Desk lamp" (getf device :name)))
    (is (string= "On/Off Light" (getf device :device-type-name)))
    (is (= #xFFF1 (getf device :vendor-id))))
  (let ((device (binding-matter::%present-device (list :vendor-id #xFFF1 :product-id #x8000))))
    (is (string= "Vendor #xFFF1 product #x8000" (getf device :name)))
    (is (null (getf device :device-type-name))))
  (is (string= "Matter device" (getf (binding-matter::%present-device '()) :name))))

(test operational-node-id--of-our-fabric-only
  (is (= #x2A (binding-matter::%operational-node-id
               "87E1B004E235A130-000000000000002A._matter._tcp.local" "87E1B004E235A130")))
  (is (null (binding-matter::%operational-node-id
             "0000000000000001-000000000000002A._matter._tcp.local" "87E1B004E235A130")))
  (is (null (binding-matter::%operational-node-id "printer._matter._tcp.local" "87E1B004E235A130"))))

;; -----------------------------
;; the job
;; -----------------------------

(test step-state--the-steps-of-the-flow-as-states
  (is (equal '(:pairing :pairing :pairing :pairing :attesting :attesting :attesting
               :credentials :credentials :credentials :completing :completing nil)
             (mapcar #'binding-matter::%step-state
                     '(:pase :arm-fail-safe :read-basic-information :regulatory-config :dac :pai :attestation
                       :csr :add-root :add-noc :case :complete :done)))))

(test commissioning-state-text--a-line-per-state
  (is (equal '(:discovering :pairing :attesting :credentials :completing :reading :labeling)
             (mapcar #'car *commissioning-states*)))
  (is (string= "finding the device" (matter-commissioning-state-text :discovering)))
  (is (string= "done" (matter-commissioning-state-text :done))))

(test commissioning-job--prints-without-its-passcode
  (let ((job (binding-matter::%make-commissioning-job :label "lamp" :passcode 20202021)))
    (is (string= "#<COMMISSIONING-JOB \"lamp\" :discovering>" (princ-to-string job)))
    (is-true (commissioning-job-running-p job))))

(defun %failure (state cause)
  (princ-to-string (make-condition 'matter-commissioning-error :state state :cause cause)))

(test commissioning-error--says-it-plainly
  (flet ((flow-error (message)
           (make-condition 'matter:commissioning-error :step :arm-fail-safe :message message)))
    ;; what the search says is said as it is
    (is (string= "No device" (%failure :discovering (make-condition 'simple-error :format-control "No device"))))
    (is (search "another app"
                (%failure :pairing (flow-error "ArmFailSafe failed with CommissioningError 4"))))
    (is (search "no room"
                (%failure :credentials (flow-error "AddNOC failed with NodeOperationalCertStatus 5"))))
    (is (search "identity"
                (%failure :attesting (make-condition 'matter:attestation-error :reason :chain :message "no PAA"))))
    (let ((text (%failure :pairing (make-condition 'matter:interaction-error
                                                   :status matter:+im-status-timeout+ :message "nothing"))))
      (is (search "did not answer while establishing the pairing session" text))
      (is (search "pairing window" text)))
    (is (null (search "pairing window"
                      (%failure :reading (make-condition 'matter:interaction-error
                                                         :status matter:+im-status-timeout+ :message "nothing")))))
    (is (string= "It failed while issuing credentials: commissioning (ARM-FAIL-SAFE): odd"
                 (%failure :credentials (flow-error "odd"))))))

(test matter-start-commissioning--what-is-signalled-at-once
  ;; no controller
  (signals error (matter-start-commissioning *manual-code*))
  (with-fixture controller-env ()
    ;; no code
    (signals error (matter-start-commissioning "3497-011-2333"))
    ;; no label
    (signals error (matter-start-commissioning *manual-code* :label ""))
    ;; the code of another device
    (let ((condition (handler-case
                         (matter-start-commissioning *manual-code*
                                                     :device (%advert "Plug" :discriminator (ash 9 8)))
                       (error (c) c))))
      (is (typep condition 'error))
      (is (search "another device" (princ-to-string condition)))
      (is (search "15" (princ-to-string condition)))
      (is (search "Plug advertises 9" (princ-to-string condition))))
    ;; none of these left a job behind
    (is (null (matter-commissioning-job)))))

(test matter-start-commissioning--no-device-fails-the-job
  (with-fixture controller-env ()
    (let ((states '())
          (results '()))
      (cl-mock:with-mocks ()
        (cl-mock:answer binding-matter::%browse-commissionable '())
        (let ((job (matter-start-commissioning *manual-code* :label "lamp"
                                                             :on-step (lambda (job)
                                                                        (push (commissioning-job-state job) states))
                                                             :on-complete (lambda (result) (push result results)))))
          (is (typep job 'commissioning-job))
          (is-true (%await-job job))
          (is (eq :failed (commissioning-job-state job)))
          (is (eq job (matter-commissioning-job)))
          (let ((error (commissioning-job-error job)))
            (is (typep error 'matter-commissioning-error))
            (is (eq :discovering (matter-commissioning-error-state error)))
            (is (search "pairing window" (princ-to-string error)))
            (is (equal (list error) results)))
          (is (equal '(:failed) states))
          (is (null (commissioning-job-node job)))
          (is (string= "lamp" (commissioning-job-label job)))
          ;; the search was by the code's short discriminator
          (is (equal '(:short-discriminator 15)
                     (subseq (first (cl-mock:invocations 'binding-matter::%browse-commissionable)) 1 3)))
          ;; `matter-commission' signals what the job failed with
          (signals matter-commissioning-error (matter-commission *manual-code* :label "lamp" :wait 10))
          (is (null (matter-nodes))))))))

(test matter-start-commissioning--a-device-without-address-fails-the-job
  (with-fixture controller-env ()
    (let ((job (matter-start-commissioning *manual-code* :device (list :name "Lamp" :discriminator 3840))))
      (is-true (%await-job job))
      (is (eq :failed (commissioning-job-state job)))
      (is (search "no address" (princ-to-string (commissioning-job-error job)))))))

(test matter-start-commissioning--a-task-that-does-not-start-frees-the-controller
  (with-fixture controller-env ()
    (cl-mock:with-mocks ()
      (cl-mock:answer binding-matter::%run-task (error "no task"))
      (signals error (matter-start-commissioning *manual-code* :label "lamp"))
      (let ((job (matter-commissioning-job)))
        (is-true job)
        (is-true (%await-job job))
        (is (eq :failed (commissioning-job-state job)))))
    ;; another commissioning is accepted: no "under way"
    (let ((job (matter-start-commissioning *manual-code*
                                           :device (list :name "Lamp" :discriminator 3840))))
      (is-true (%await-job job))
      (is (search "no address" (princ-to-string (commissioning-job-error job)))))))

(test stop-controller--fails-a-commissioning-that-runs
  (with-fixture controller-env ()
    (let* ((controller binding-matter::*controller*)
           (results '())
           (job (binding-matter::%make-commissioning-job :label "lamp"
                                                         :on-complete (lambda (result) (push result results)))))
      (matter-actor-call (binding-matter::controller-actor controller)
                         (lambda () (binding-matter::%claim-job controller job)))
      ;; one at a time
      (signals error (matter-start-commissioning *manual-code* :device (%advert "Lamp")))
      (stop-controller)
      (is (eq :failed (commissioning-job-state job)))
      (is (= 1 (length results)))
      (is (search "stopped" (princ-to-string (first results)))))))

;; -----------------------------
;; requests to a node that is none
;; -----------------------------

(test node-requests--fail-for-a-node-that-is-not-commissioned
  (with-fixture controller-env ()
    (dolist (future (list (matter-set-label "nobody" "somebody")
                          (matter-decommission "nobody")
                          (matter-decommission "nobody" :force t)
                          (matter-open-window "nobody")))
      (is (typep (%await future) 'error))
      (is-true (future:error-p future)))
    (signals error (matter-set-label "nobody" ""))
    (signals error (matter-describe "nobody"))
    (signals error (matter-suggestions "nobody"))
    (signals error (matter-items-of "nobody"))
    (signals error (matter-await (matter-open-window "nobody")))))

(test matter-await--the-value-or-the-condition
  (with-fixture controller-env ()
    (is (equal '() (matter-await (matter-actor-future (binding-matter::controller-actor binding-matter::*controller*)
                                                      (lambda (resolve) (funcall resolve '()))))))
    (is (= 42 (matter-await (matter-actor-future (binding-matter::controller-actor binding-matter::*controller*)
                                                 (lambda (resolve) (funcall resolve 42))))))
    ;; one that does not come
    (signals error (matter-await (matter-actor-future (binding-matter::controller-actor binding-matter::*controller*)
                                                      (lambda (resolve) (declare (ignore resolve))))
                                 :timeout 0.2))))

(test matter-discover--the-open-windows-without-a-controller
  (cl-mock:with-mocks ()
    (cl-mock:answer matter:discover-commissionable-nodes
      (list (list :instance "A" :addresses '("fe80::1") :port 5540 :discriminator 3840
                  :commissioning-mode 1 :device-name "Lamp" :device-type #x0100)
            ;; extended discovery: there, but its window is shut
            (list :instance "B" :addresses '("fe80::2") :port 5540 :discriminator 7 :commissioning-mode 0)
            (list :instance "C" :addresses '("fe80::3") :port 5541 :discriminator 9
                  :commissioning-mode 2 :vendor-id #x1234)))
    (let ((found (matter-discover :timeout 1)))
      (is (equal '("Lamp" "Vendor #x1234") (mapcar (lambda (d) (getf d :name)) (getf found :commissionable))))
      (is (equal '("On/Off Light" nil)
                 (mapcar (lambda (d) (getf d :device-type-name)) (getf found :commissionable))))
      (is (equal '("fe80::1") (getf (first (getf found :commissionable)) :addresses)))
      (is (null (getf found :operational)))
      (is (null (getf found :silent))))))

;; -----------------------------
;; device types and definers
;; -----------------------------

(test device-types--names-and-suggestions
  (is (string= "On/Off Light" (binding-matter::device-type-name matter:+device-type-on-off-light+)))
  (is (string= "Root Node" (binding-matter::device-type-name matter:+device-type-root-node+)))
  (is (string= "Device type #x7777" (binding-matter::device-type-name #x7777)))
  (is (eq :light (binding-matter::device-type-suggestion matter:+device-type-on-off-light+)))
  (is (eq :plug (binding-matter::device-type-suggestion matter:+device-type-on-off-plug-in-unit+)))
  (is (eq :dimmable-light (binding-matter::device-type-suggestion matter:+device-type-dimmable-light+)))
  (is (eq :contact (binding-matter::device-type-suggestion matter:+device-type-contact-sensor+)))
  (is (eq :temperature (binding-matter::device-type-suggestion matter:+device-type-temperature-sensor+)))
  (is (eq :shutter (binding-matter::device-type-suggestion matter:+device-type-window-covering+)))
  (is (eq :heating (binding-matter::device-type-suggestion matter:+device-type-thermostat+)))
  (is (null (binding-matter::device-type-suggestion matter:+device-type-root-node+)))
  (is (null (binding-matter::device-type-suggestion #x7777))))

(test item-name-of-label--a-name-a-symbol-can-have
  (is (string= "kitchen-lamp-2" (binding-matter::item-name-of-label "Kitchen lamp 2")))
  (is (string= "eve-energy" (binding-matter::item-name-of-label " Eve  Energy! ")))
  (is (string= "lamp" (binding-matter::item-name-of-label "lamp")))
  (is (string= "node" (binding-matter::item-name-of-label "---"))))

(test suggest-definer--a-light-as-text
  (is (string= (format nil "(deflight lamp \"Lamp\"~%  (matter-binding :node \"lamp\" :endpoint 1 :cluster :on-off))")
               (binding-matter::suggest-definer :light "lamp" "Lamp" "lamp" 1)))
  (is (null (binding-matter::suggest-definer :nothing "lamp" "Lamp" "lamp" 1)))
  ;; a button is a remark, not a form
  (is (eql 0 (search ";;" (binding-matter::suggest-definer :switch "button" "Button" "button" 1)))))

(defun %define-suggested (kind name)
  "Reads and evaluates the definer suggested for `KIND' as a configuration file would; the form's operator."
  (let* ((text (binding-matter::suggest-definer kind name "Thing" "thing" 3))
         (form (let ((*package* (find-package :chipi.binding.matter-test-config)))
                 (read-from-string text))))
    (eval form)
    (first form)))

(defun %config-item (name)
  (hab:get-item (intern (string-upcase name) :chipi.binding.matter-test-config)))

(defun %kind (name)
  (cdr (assoc :house-kind (item:tags (%config-item name)))))

(test suggest-definer--every-suggestion-defines-its-items
  "The text is ready to paste: read in a configuration's package it defines the items, bound to the node."
  (with-fixture controller-env (:init nil)
    (flet ((node-bindings (name)
             (remove-if-not (lambda (binding) (typep binding 'binding-matter::matter-binding))
                            (item:bindings (%config-item name)))))
      (is (eq 'house:deflight (%define-suggested :light "s-light")))
      (is (eq :light (%kind "s-light")))
      (is (= 1 (length (node-bindings "s-light"))))
      (is (string= "thing" (binding-matter::matter-binding-node (first (node-bindings "s-light")))))
      (is (= 3 (binding-matter::matter-binding-endpoint (first (node-bindings "s-light")))))

      (is (eq 'house:defplug (%define-suggested :plug "s-plug")))
      (is (eq :plug (%kind "s-plug")))

      (is (eq 'house:defdimmablelight (%define-suggested :dimmable-light "s-dimmer")))
      (is (eq :light (%kind "s-dimmer")))
      (is (eq :brightness (%kind "s-dimmer-brightness")))
      (is (equal '(:fields :move-to-level-with-on-off (:level (:scale 127/50) :transition-time 0))
                 (binding-matter::matter-binding-command (first (node-bindings "s-dimmer-brightness")))))

      (is (eq 'house:defcontact (%define-suggested :contact "s-contact")))
      (is (eq :contact (%kind "s-contact")))
      (is (equal '(:ui-readonly . t) (assoc :ui-readonly (item:tags (%config-item "s-contact")))))

      (is (eq 'hab:defitem (%define-suggested :temperature "s-temp")))
      (is (= 1/100 (binding-matter::matter-binding-scale (first (node-bindings "s-temp")))))
      (is (eq 'hab:defitem (%define-suggested :humidity "s-humidity")))
      (is (equal '(3 #x0405 0) (binding-matter::binding-path (first (node-bindings "s-humidity")))))
      (is (eq 'hab:defitem (%define-suggested :occupancy "s-motion")))
      (is (equal '(3 #x0406 0) (binding-matter::binding-path (first (node-bindings "s-motion")))))

      (is (eq 'house:defrollershutter (%define-suggested :shutter "s-blind")))
      (is (eq :shutter-move (%kind "s-blind")))
      (is (eq :shutter-stop (%kind "s-blind-stop")))
      (is (eq :shutter-position (%kind "s-blind-pos")))
      (is (equal '(:switch :up-or-open :down-or-close)
                 (binding-matter::matter-binding-command (first (node-bindings "s-blind")))))

      (is (eq 'house:defheating (%define-suggested :heating "s-heating")))
      (is (eq :temperature (%kind "s-heating-temp")))
      (is (eq :setpoint (%kind "s-heating-setpoint"))))))

(defparameter *lamp-plist*
  (list :node-id 2 :label "Desk lamp"
        :endpoints (list (list :id 0 :device-types (list (cons matter:+device-type-root-node+ 3))
                               :server-clusters (list matter:+cluster-basic-information+ #xFC01))
                         (list :id 1 :device-types (list (cons matter:+device-type-on-off-light+ 3))
                               :server-clusters (list matter:+cluster-on-off+)))))

(test node-suggestions--what-each-endpoint-is
  (let ((suggestions (binding-matter::node-suggestions *lamp-plist* '())))
    (is (equal '(0 1) (mapcar (lambda (s) (getf s :endpoint)) suggestions)))
    (is (equal '(("Root Node") ("On/Off Light")) (mapcar (lambda (s) (getf s :device-types)) suggestions)))
    (is (equal '("basic-information" "#xFC01") (getf (first suggestions) :clusters)))
    (is (equal '("on-off") (getf (second suggestions) :clusters)))
    (is (null (getf (first suggestions) :definer)))
    (is (null (getf (second suggestions) :items)))
    (is (eql 0 (search "(deflight desk-lamp \"Desk lamp\"" (getf (second suggestions) :definer))))
    (is (search ":node \"Desk lamp\" :endpoint 1" (getf (second suggestions) :definer)))))

(test node-suggestions--parts-of-one-device-get-names-of-their-own
  (let* ((plist (list :node-id 3 :label "strip"
                      :endpoints (list (list :id 1 :device-types (list (cons matter:+device-type-on-off-plug-in-unit+ 3)))
                                       (list :id 2 :device-types (list (cons matter:+device-type-on-off-plug-in-unit+ 3))))))
         (suggestions (binding-matter::node-suggestions plist '())))
    (is (eql 0 (search "(defplug strip-1 " (getf (first suggestions) :definer))))
    (is (eql 0 (search "(defplug strip-2 " (getf (second suggestions) :definer))))))
