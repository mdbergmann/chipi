(defpackage :chipi.binding.knx-test
  (:use :cl :fiveam :cl-mock :chipi.binding.knx)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.knx-test)

(def-suite knx-binding-tests
  :description "KNX binding tests")

(in-suite knx-binding-tests)

(test make-knx-binding
  "Tests creating a knx binding"
  (with-mocks ()
    ;; binding should register listener function
    (answer knx-client:add-tunnelling-request-listener t)
    (let ((cut (knx-binding
                :ga "1/2/3"
                :dpt "9.001"
                :initial-delay nil)))
      (is-true cut)
      (is (address:knx-group-address-p (binding-knx::group-address cut)))
      (is (eq 'dpt:dpt-9.001 (binding-knx::dpt-type cut)))
      (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener)))))))

(defun %make-test-tun-req (ga mc apci &optional (dpt (dpt:make-dpt1 'dpt:dpt-1.001 :on)))
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 0
   :cemi (cemi:make-default-cemi
          :message-code mc
          :dest-address (address:make-group-address ga)
          :apci apci
          :dpt dpt)))

(test binding-listens-on-ga-changes
  (with-mocks ()
    (let ((item-set-called-with nil)
          (listener-fun-registered nil))
      ;; binding should register listener function
      (answer (knx-client:add-tunnelling-request-listener fun)
        (progn
          (setf listener-fun-registered fun)
          t))
      ;; binding should call `set-value' on bound items
      (answer (item:set-value _ value :push nil)
        (progn
          (setf item-set-called-with value)
          t))

      (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
        (binding:bind-item cut :foo-item)

        (is (functionp listener-fun-registered))
        ;; we call the registered fun manually
        ;; in production this is done automatically
        (funcall listener-fun-registered
                 (%make-test-tun-req "1/2/3"
                                     cemi:+cemi-mc-l_data.ind+
                                     (cemi:make-apci-gv-write)))

        (is (equal 'item:true item-set-called-with))
        ))))

(test binding-listens-on-ga-changes--check-errors
  (with-mocks ()
    (let ((item-set-called-with nil)
          (listener-fun-registered nil))
      (answer (knx-client:add-tunnelling-request-listener fun)
        (progn
          (setf listener-fun-registered fun)
          t))

      (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay nil)))
        (binding:bind-item cut :foo-item)

        (flet ((assert-no-processing (req)
                 (funcall listener-fun-registered req)
                 (is (eq nil item-set-called-with))))
          ;; wrong ga
          (assert-no-processing
           (%make-test-tun-req "9/8/7"
                               cemi:+cemi-mc-l_data.ind+
                               (cemi:make-apci-gv-write)))
          ;; wrong mc
          (assert-no-processing
           (%make-test-tun-req "1/2/3"
                               cemi:+cemi-mc-l_data.con+
                               (cemi:make-apci-gv-write)))
          ;; wrong apci
          (assert-no-processing
           (%make-test-tun-req "1/2/3"
                               cemi:+cemi-mc-l_data.ind+
                               (cemi:make-apci-gv-read))))))))

(test binding-listens-on-ga-changes--other-value-than-1.001
  "1.001 must be converted from :on/:off to 'item:true/'item:false"
  (with-mocks ()
    (let ((item-set-called-with nil)
          (listener-fun-registered nil))
      (answer (knx-client:add-tunnelling-request-listener fun)
        (progn
          (setf listener-fun-registered fun)
          t))

      (answer (item:set-value _ value :push nil)
        (progn
          (setf item-set-called-with value)
          t))

      (let ((cut (knx-binding :ga "1/2/3" :dpt "5.001" :initial-delay nil)))
        (binding:bind-item cut :foo-item)

        (is (functionp listener-fun-registered))
        ;; we call the registered fun manually
        ;; in production this is done automatically
        (funcall listener-fun-registered
                 (%make-test-tun-req "1/2/3"
                                     cemi:+cemi-mc-l_data.ind+
                                     (cemi:make-apci-gv-write)
                                     (dpt:make-dpt5 'dpt:dpt-5.001 11)))

        (is (equal 11 item-set-called-with))
        ))))

(test binding-can-pull--initially
  "Pull is done after initializing the binding (and only once)."
  (with-mocks ()
    (let ((item-set-called-with nil))
      (answer knx-client:add-tunnelling-request-listener t)

      (answer (knxc:request-value _ 'dpt:dpt-1.001)
        (future:with-fut :on))

      (answer (item:set-value _ value :push nil)
        (progn
          (setf item-set-called-with value)
          t))

      (binding:bind-item
       (knx-binding :ga "1/2/3" :dpt "1.001" :initial-delay 1)
       :foo-item)

      (is-true (miscutils:await-cond 2.5
                 (eq item-set-called-with 'item:true)))
      )))

(defun binding-can-push-value (dpt-type-str push-value expected-value)
  (with-mocks ()
    (answer knx-client:add-tunnelling-request-listener t)
    (let ((cut (knx-binding :ga "1/2/3" :dpt dpt-type-str))
          (pushed-value nil))
      (answer (knxc:write-value _ _ push-value)
        (setf pushed-value push-value))
      (binding:exec-push cut push-value)
      (is (equalp pushed-value expected-value)))))

(test binding-can-push-on-item-value-change
  (binding-can-push-value "1.001" 'item:false nil))

(test binding-can-push-on-item-value-change--uint
  (binding-can-push-value "5.010" 123 123))

(test knx-defconfig--initializes-knx
  (with-mocks ()
    (answer knxc:knx-conn-init t)
    (knx-defconfig :gw-host "foo.bar" :gw-port 3671)
    (is (= 1 (length (invocations 'knxc:knx-conn-init))))))

(test shutdown-knx--closes-knx
  (with-mocks ()
    (answer knxc:knx-conn-destroy t)
    (knx-shutdown)
    (is (= 1 (length (invocations 'knxc:knx-conn-destroy))))))
