(defpackage :knx-conn.knx-connect-test
  (:use :cl :cl-mock :fiveam
   :knxutil :knxobj :descr-info :connect :tunnelling
        :crd :cemi :address :dib :knxc))

(in-package :knx-conn.knx-connect-test)

(def-suite knx-connect-tests
  :description "Tests for KNX connection handling"
  );;:in projname.tests:test-suite)

(in-suite knx-connect-tests)

(log:config :debug)

;; --------------------------------------
;; description request/response
;; --------------------------------------

(setf knxc::*conn* 'foo)

(defparameter *descr-response-data*
  #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
    80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
    0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test retrieve-descr-info--receive-response--check-package
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *descr-response-data*)

    (let ((result (retrieve-descr-info)))
      (is (typep result 'knx-descr-response))
      ;; check knx-header
      (let ((header (package-header result)))
        (is (typep header 'knx-header))
        (is (eql knxobj::+knx-header-len+ (header-len header)))
        (is (eql descr-info::+knx-descr-response+ (header-type header)))
        (is (eql knxobj::+knx-netip-version+ (header-knxnetip-version header)))
        (is (eql (- 84 knxobj::+knx-header-len+) (header-body-len header))))
      ;; check knx-body
      (let ((body (package-body result)))
        (is-true (not (null body)))
        (is (eql 78 (length body))))
      ;; check dibs
      (is (typep (descr-response-device-hardware result) 'dib))
      (is (typep (descr-response-device-hardware result) 'dib-device-info))
      (is (typep (descr-response-supp-svc-families result) 'dib))
      (is (typep (descr-response-supp-svc-families result) 'dib-supp-svc-families))
      (is (typep (descr-response-other-dib-info result) 'dib-list))
      (is-true (not (endp (descr-response-other-dib-info result)))))

    (is (eql 1 (length (invocations 'usocket:socket-send))))
    (is (eql 1 (length (invocations 'usocket:socket-receive))))))

(defparameter *raw-descr-request*
  (make-array 14
              :element-type '(unsigned-byte 8)
              :initial-contents
              '(#x06 #x10
                #x02 #x03
                #x00 #x0e
                ;; HPAI
                #x08
                #x01                ;; udp
                #x00 #x00 #x00 #x00 ;; unbound address
                #x00 #x00
                )))

(test descr-request--compare-to-raw
  (is (equalp *raw-descr-request*
              (byte-seq-to-byte-array
               (to-byte-seq
                (make-descr-request hpai:*hpai-unbound-addr*))))))

(defparameter *raw-descr-request-2*
  (make-array 14
              :element-type '(unsigned-byte 8)
              :initial-contents
              '(#x06 #x10
                #x02 #x03
                #x00 #x0e
                ;; HPAI
                #x08
                #x01 ;; udp
                192 168 50 100
                195 180)))

(test descr-request--compare-to-raw-2
  (is (equalp *raw-descr-request-2*
              (byte-seq-to-byte-array
               (to-byte-seq
                (make-descr-request
                 (hpai:make-hpai "192.168.50.100" 50100)))))))

;; --------------------------------------
;; connect request/response
;; --------------------------------------

(defparameter *connect-response-data-ok*
  #(6 16 2 6 0 20 78 0 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test connect-ok
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-ok*)

    (multiple-value-bind (response err)
        (connect-to-endpoint)
      (is (eq nil err))
      (is (typep response 'knx-connect-response))
      ;; check knx-header
      (let ((header (package-header response)))
        (is (typep header 'knx-header)))
      ;; check connect response body
      (is (eql +connect-status-no-error+ (connect-response-status response)))
      (is (eql 78 (connect-response-channel-id response)))
      (is (equal (address-string-rep
                  (crd:crd-individual-address
                   (connect-response-crd response)))
                 "14.14.255"))
      )
    
    (is (eql 1 (length (invocations 'usocket:socket-send))))
    (is (eql 1 (length (invocations 'usocket:socket-receive))))))

(defparameter *connect-response-data-err*
  #(6 16 2 6 0 20 78 34 8 1 0 0 0 0 0 0 4 4 238 255 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  "Connect response with error status")

(test connect-err
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *connect-response-data-err*)

    (multiple-value-bind (response err)
        (connect-to-endpoint)
      (is (null response))
      (is (equal (format nil "~a" err)
                 (format nil
                         "Error condition: Connect response status, args: (~a)"
                         +connect-status-err-conn-type+))))
    
    (is (eql 1 (length (invocations 'usocket:socket-send))))
    (is (eql 1 (length (invocations 'usocket:socket-receive))))))


;; --------------------------------------
;; tunneling request receival
;; --------------------------------------

(defparameter *tunnelling-request-data*
  #(6 16 4 32 0 23 4 76 0 0 41 0 188 208 19 14 4 10 3 0 128 12 104
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(test tunneling-request-ok ()
  (with-mocks ()
    (answer usocket:socket-receive *tunnelling-request-data*)

    (multiple-value-bind (request err)
        (receive-knx-request)
      (is (not (null request)))
      (is (typep request 'knx-tunnelling-request))
      (let ((conn-header (tunnelling-request-conn-header request)))
        (is (= (conn-header-channel-id conn-header) 76))
        (is (= (conn-header-seq-counter conn-header) 0)))
      (let ((cemi (tunnelling-request-cemi request)))
        (is (typep cemi 'cemi-l-data))
        (is (= (cemi-message-code cemi) +cemi-mc-l_data.ind+))
        (is (equal (cemi-ctrl1 cemi) #*10111100))
        (is (equal (cemi-ctrl2 cemi) #*11010000))
        (is (typep (cemi-source-addr cemi) 'knx-individual-address))
        (is (typep (cemi-destination-addr cemi) 'knx-group-address))
        (is (string= (address-string-rep
                      (cemi-source-addr cemi)) "1.3.14"))
        (is (string= (address-string-rep
                      (cemi-destination-addr cemi)) "0/4/10"))
        (is (equalp (cemi-npdu cemi) #(0 128 12)))
        ))
    
    (is (= 1 (length (invocations 'usocket:socket-receive))))))

(run! 'knx-connect-tests)
