(defpackage :chipi.binding.knx-test
  (:use :cl :fiveam :cl-mock :chipi.binding.knx)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.knx-test)

(def-suite knx-binding-tests
  :description "KNX binding tests")

(in-suite knx-binding-tests)

;; your test code here

(test make-knx-binding
  "Tests creating a knx binding"
  (with-mocks ()
    ;; binding should register listener function
    (answer knx-client:add-tunnelling-request-listener t)
    (let ((cut (knx-binding
                :ga "1/2/3"
                :dpt "9.001")))
      (is-true cut)
      (is (address:knx-group-address-p (group-address cut)))
      (is (eq 'dpt:dpt-9.001 (dpt-type cut)))
      (is (= 1 (length (invocations 'knx-client:add-tunnelling-request-listener)))))))

(defun %make-test-tun-req ()
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 0
   :cemi (cemi:make-default-cemi
          :message-code cemi:+cemi-mc-l_data.ind+
          :dest-address (address:make-group-address "1/2/3")
          :apci (cemi:make-apci-gv-write)
          :dpt (dpt:make-dpt1 'dpt:dpt-1.001 :on))))

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
      (answer (item:set-value _ value)
        (progn
          (setf item-set-called-with value)
          t))

      (let ((cut (knx-binding :ga "1/2/3" :dpt "1.001")))
        (binding:bind-item cut :foo-item)

        (is (functionp listener-fun-registered))
        ;; we call the registered fun manually
        ;; in production this is done automatically
        (funcall listener-fun-registered (%make-test-tun-req))

        (is (equal 'item:true item-set-called-with))
        ))))


(run! 'knx-binding-tests)
