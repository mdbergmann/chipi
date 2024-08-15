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

;; ------------------------------
;; KNXNet simulator
;; ------------------------------

(defvar *tunnel-established* nil)
(defvar *conn-host* nil)
(defvar *conn-port* nil)

(defvar *sim-thread-and-socket* nil)

(defun start-knxnet-sim ()
  (flet ((handler-fun (buf)
           (format t "KNXNet server: received message.~%")
           (setf *conn-host* usocket:*remote-host*)
           (setf *conn-port* usocket:*remote-port*)
           (let ((knx-obj (knxobj:parse-root-knx-object buf)))
             (format t "KNXNet server: knxobj received: ~a~%" knx-obj)
             (let ((response-obj
                     (etypecase knx-obj
                       (connect:knx-connect-request
                        (format t "KNXNet server: generating connect-response...~%")
                        (prog1
                            (connect:make-connect-response 1 "127.0.0.1" 3671)
                          (setf *tunnel-established* t)))
                       (tunnelling:knx-tunnelling-ack
                        (format t "KNXNet server: tunnel-ack"))
                       (connect:knx-disconnect-request
                        (format t "KNXNet server: disconnect-request")
                        ;; TODO: send disconnect response, or we'll wait longer than necessary on client side
                        (setf *tunnel-established* nil)))))
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

(def-fixture simulator (host)
  (unwind-protect
       (progn
         (setf *tunnel-established* nil)
         (setf *conn-port* nil)
         (setf *conn-host* nil)
         
         (start-knxnet-sim)
         (sleep .5)
         
         (defconfig
           (knx-init :gw-host host))
         (is-true (await-cond 2.0
                    *tunnel-established*))
         (&body))
    (progn
      (ignore-errors
       (knx-shutdown))
      (ignore-errors
       (shutdown))
      (ignore-errors
       (stop-knxnet-sim)))))

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

(defun %send-knx-msg (knx-msg)
  (let ((socket (second *sim-thread-and-socket*))
        (buffer (knxobj:to-byte-seq knx-msg)))
    (usocket:socket-send socket
                         buffer
                         (length buffer)
                         :port *conn-port*
                         :host *conn-host*)))

(test bus-events-update-item-value
  (with-fixture simulator ("127.0.0.1")
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
