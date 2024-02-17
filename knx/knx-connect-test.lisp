(defpackage :chipi.knx-connect-test
  (:use :cl :cl-mock :try :chipi.knx-connect))

(in-package :chipi.knx-connect-test)

(defparameter *descr-response-data*
  #(6 16 2 4 0 84 54 1 2 0 17 1 0 0 0 1 0 53 81 241 0 0 0 0 0 14 140 0 107 180 73
    80 32 73 110 116 101 114 102 97 99 101 32 78 49 52 56 0 0 0 0 0 0 0 0 0 0 0 0
    0 12 2 2 1 3 2 4 1 7 1 8 1 12 254 0 1 8 0 255 241 115 255 148 75 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(deftest suite ()
  (send-descr-request--receive-response--check-package))

(deftest send-descr-request--receive-response--check-package ()
  (with-mocks ()
    (answer usocket:socket-send t)
    (answer usocket:socket-receive *descr-response-data*)

    (let ((result (send-descr-request)))
      (is (typep result 'knx-descr-response))
      (let ((header (package-header result)))
        (is (typep header 'knx-header))
        (is (= knxc::+knx-header-len+ (header-len header)))
        (is (= knxc::+knx-descr-response+ (header-type header)))
        (is (= knxc::+knx-netip-version+ (header-knxnetip-version header)))
        (is (= (- 84 knxc::+knx-header-len+) (header-body-len header))))
      (let ((body (package-body result)))
        (is (not (null body))))
      (is (typep (descr-response-device-hardware result) 'dib))
      (is (typep (descr-response-supp-svc-families result) 'dib))
      (is (typep (descr-response-other-dev-info result) 'dib-list))
      (is (not (endp (descr-response-other-dev-info result))))
      )

    (is (= 1 (length (invocations 'usocket:socket-send))))
    (is (= 1 (length (invocations 'usocket:socket-receive))))))

(try 'suite)
