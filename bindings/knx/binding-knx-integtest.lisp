(defpackage :chipi.binding.knx-integtest
  (:use :cl :fiveam :cl-mock :chipi.binding.knx :hab)
  (:import-from #:sento.miscutils
                #:await-cond)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding.knx-integtest)

(def-suite knx-binding-integtests
  :description "Integration tests for KNX binding"
  :in chipi.binding.knx-test-suite:test-suite)

(in-suite knx-binding-integtests)

(defun %make-test-tun-req (ga mc apci
                           &optional (dpt (dpt:make-dpt1 'dpt:dpt-1.001 :on)))
  (tunnelling:make-tunnelling-request
   :channel-id 1
   :seq-counter 0
   :cemi (cemi:make-default-cemi
          :message-code mc
          :dest-address (address:make-group-address ga)
          :apci apci
          :dpt dpt)))

;; ------------------------------
;; KNXNet simulator
;; ------------------------------

(defvar *sim-thread-and-socket* nil)
(defvar *conn-host* nil)
(defvar *conn-port* nil)
(defvar *tunnel-established* nil)
(defvar *last-received-tun-req* nil)

(defun %send-knx-msg (knx-msg)
  (let ((socket (second *sim-thread-and-socket*))
        (buffer (knxobj:to-byte-seq knx-msg)))
    (usocket:socket-send socket
                         buffer
                         (length buffer)
                         :port *conn-port*
                         :host *conn-host*)))

(defun start-knxnet-sim ()
  (flet ((handler-fun (buf)
           (format t "KNXNet server: received message.~%")
           (setf *conn-host* usocket:*remote-host*)
           (setf *conn-port* usocket:*remote-port*)
           (let ((received-knx-obj (knxobj:parse-root-knx-object buf)))
             (format t "KNXNet server: knxobj received: ~a~%" received-knx-obj)
             (let ((response-obj
                     (etypecase received-knx-obj
                       (connect:knx-connect-request
                        (format t "KNXNet server: generating connect-response...~%")
                        (prog1
                            (connect:make-connect-response 1 "127.0.0.1" 3671)
                          (setf *tunnel-established* t)))
                       (tunnelling:knx-tunnelling-request
                        (format t "KNXNet server: tunnel-req")
                        (setf *last-received-tun-req* received-knx-obj)
                        (%send-knx-msg (tunnelling:make-tunnelling-ack received-knx-obj))
                        (%make-test-tun-req "1/2/3"
                                            cemi:+cemi-mc-l_data.ind+
                                            (cemi:make-apci-gv-response)
                                            (dpt:make-dpt1 'dpt:dpt-1.001 :off)))
                       (tunnelling:knx-tunnelling-ack
                        (format t "KNXNet server: tunnel-ack"))
                       (connect:knx-disconnect-request
                        (format t "KNXNet server: disconnect-request")
                        (prog1
                            (connect:make-disconnect-response 1 0)
                          (setf *tunnel-established* nil))))))
               (when response-obj
                 (knxobj:to-byte-seq response-obj))))))
    (setf *sim-thread-and-socket*
          (multiple-value-list
           (usocket:socket-server "127.0.0.1" 3671
                                  #'handler-fun nil
                                  :in-new-thread t
                                  :protocol :datagram)))
    (format t "KNXNet server started~%")))

(defun stop-knxnet-sim ()
  (when *sim-thread-and-socket*
    (usocket:socket-close (second *sim-thread-and-socket*))
    (bt2:destroy-thread (car *sim-thread-and-socket*))
    (setf *sim-thread-and-socket* nil)))

;; -------------------------------
;; tests
;; -------------------------------

(def-fixture simulator ()
  (unwind-protect
       (progn
         (setf *tunnel-established* nil)
         (setf *conn-port* nil)
         (setf *conn-host* nil)
         (setf *last-received-tun-req* nil)
         
         (start-knxnet-sim)
         (sleep .5)
         
         (defconfig
           (knx-init :gw-host "127.0.0.1"))
         (is-true (await-cond 2.0
                    *tunnel-established*))
         (&body))
    (progn
      (ignore-errors
       (shutdown))
      (ignore-errors
       (stop-knxnet-sim)))))

(test bus-events-update-item-value
  (with-fixture simulator ()
    (let ((item
            (defitem 'foo "KNX item" '(unsigned-byte 8)
              (knx-binding :ga "1/2/3"
                           :dpt "5.010"
                           :initial-delay nil))))
      (is-true item)
      (%send-knx-msg
       (%make-test-tun-req "1/2/3"
                           cemi:+cemi-mc-l_data.ind+
                           (cemi:make-apci-gv-write)
                           (dpt:make-dpt5 'dpt:dpt-5.010 123)))
      (is-true (await-cond 5.0
                 (let ((item-value (item:get-value item)))
                   (eql 123 (future:fawait item-value :timeout 1))))))))

(test retrieve-ga-value-initially
  (with-fixture simulator ()
    (let ((item
            (defitem 'foo "KNX item" 'boolean
              (knx-binding :ga "1/2/3"
                           :dpt "1.001"
                           :initial-delay 0.1)
              :initial-value 'item:true)))
      (is-true item)
      (is-true (await-cond 3.0
                 (let ((item-value (item:get-value item)))
                   (eql 'item:false (future:fawait item-value :timeout 1))))))))

(test write-value-to-ga-on-item-update
  (with-fixture simulator ()
    (let ((item
            (defitem 'foo "KNX item" 'boolean
              (knx-binding :ga "1/2/3"
                           :dpt "1.001"
                           :initial-delay nil
                           :call-push-p t)
              :initial-value 'item:true)))
      (item:set-value item 'item:false)
      (is-true (await-cond 3.0
                 (let* ((cemi (tunnelling:tunnelling-request-cemi *last-received-tun-req*))
                        (ga (cemi:cemi-destination-addr cemi))
                        (ga-str (address:address-string-rep ga))
                        (mc (tunnelling:tunnelling-cemi-message-code *last-received-tun-req*))
                        (apci (cemi:cemi-apci cemi))
                        (cemi-data (cemi:cemi-data cemi)))
                   (and (typep *last-received-tun-req* 'tunnelling:knx-tunnelling-request)
                        (string= ga-str "1/2/3")
                        (eql mc cemi:+cemi-mc-l_data.req+)
                        (cemi:apci-gv-write-p apci)
                        (equalp cemi-data #(0)))))))))
