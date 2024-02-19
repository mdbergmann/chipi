(defpackage :knx-conn.knx-connect-test
  (:use :cl :cl-mock :try :knxutil :knxobj :descr-info :dib :knxc))

(in-package :knx-conn.knx-connect-test)

(log:config :debug)

;; --------------------------------------
;; description request/response
;; --------------------------------------

(setf knxc::*conn* 'foo)

(deftest descr-info-suite ()
  (retrieve-descr-info--receive-response--check-package)
  (descr-request--compare-to-raw)
  (descr-request--compare-to-raw-2))

(defparameter *descr-response-data*
  #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
    80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
    0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(deftest retrieve-descr-info--receive-response--check-package ()
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *descr-response-data*)

    (let ((result (retrieve-descr-info)))
      (is (typep result 'knx-descr-response))
      ;; check knx-header
      (let ((header (package-header result)))
        (is (typep header 'knx-header))
        (is (= knxobj::+knx-header-len+ (header-len header)))
        (is (= descr-info::+knx-descr-response+ (header-type header)))
        (is (= knxobj::+knx-netip-version+ (header-knxnetip-version header)))
        (is (= (- 84 knxobj::+knx-header-len+) (header-body-len header))))
      ;; check knx-body
      (let ((body (package-body result)))
        (is (not (null body)))
        (is (= 78 (length body))))
      ;; check dibs
      (is (typep (descr-response-device-hardware result) 'dib))
      (is (typep (descr-response-device-hardware result) 'dib-device-info))
      (is (typep (descr-response-supp-svc-families result) 'dib))
      (is (typep (descr-response-supp-svc-families result) 'dib-supp-svc-families))
      (is (typep (descr-response-other-dib-info result) 'dib-list))
      (is (not (endp (descr-response-other-dib-info result)))))

    (is (= 1 (length (invocations 'usocket:socket-send))))
    (is (= 1 (length (invocations 'usocket:socket-receive))))))

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

(deftest descr-request--compare-to-raw ()
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

(deftest descr-request--compare-to-raw-2 ()
  (is (equalp *raw-descr-request-2*
              (byte-seq-to-byte-array
               (to-byte-seq
                (make-descr-request
                 (hpai:make-hpai "192.168.50.100" 50100)))))))

;; --------------------------------------
;; connect request/response
;; --------------------------------------

(deftest connect-suite ()
  )

(deftest suite ()
  (descr-info-suite)
  (connect-suite)
  )
