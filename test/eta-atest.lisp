(defpackage :cl-eta.eta-atest
  (:use :cl :fiveam :cl-eta.eta)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.eta-atest)

(def-suite eta-atests
  :description "Acceptance tests"
  :in cl-eta.tests:test-suite)

(in-suite eta-atests)

(log:config :error)

(defparameter +record-data-pkg-payload+
  '(#x18 0 53 0 99                      ; Betriebsstunden (99 hours)
    ))

(defun new-record-data-pkg ()
  (eta-pkg:make-pkg `(,(char-code #\{)
                      ,(char-code #\M) ,(char-code #\D)
                      ,(length +record-data-pkg-payload+)
                      ,(eta-pkg:check-sum +record-data-pkg-payload+)
                      ,+record-data-pkg-payload+
                      ,(char-code #\}))))

(defmethod eta-ser-if:open-serial ((impl (eql :atest-start-record)) device)
  (declare (ignore impl device))
  t)
(defmethod eta-ser-if:write-serial ((impl (eql :atest-start-record)) port data)
  (declare (ignore impl port data))
  (length (eta-pkg:new-start-record-pkg)))
(defmethod eta-ser-if:read-serial ((impl (eql :atest-start-record)) port &optional timeout)
  (declare (ignore impl port timeout))
  (new-record-data-pkg))

(defparameter *stop-record-written-serial* 0)
(defmethod eta-ser-if:open-serial ((impl (eql :atest-stop-record)) device)
  (declare (ignore impl device))
  t)
(defmethod eta-ser-if:write-serial ((impl (eql :atest-stop-record)) port data)
  (declare (ignore impl port data))
  (setf *stop-record-written-serial* (length (eta-pkg:new-stop-record-pkg))))
(defmethod eta-ser-if:read-serial ((impl (eql :atest-stop-record)) port &optional timeout)
  (declare (ignore impl port timeout))
  #())

(defparameter *http-server* nil)

(defparameter *item-name* "")
(defparameter *raw-data* "")

(def-fixture init-destroy ()
  (unwind-protect
       (progn
         (setf openhab:*openhab-base-url* "http://127.0.0.1:10456/rest/items/")
         (setf *http-server*
               (make-instance 'easy-routes:easy-routes-acceptor
                              :port 10456
                              :address "127.0.0.1"))
         (hunchentoot:start *http-server*)
         (eta:ensure-initialized)
         (eta:eta-init)
         (&body))
    (progn
      (when *http-server*
        (hunchentoot:stop *http-server*)
        (setf *http-server* nil))
      (eta:ensure-shutdown))))

;; (test start-record--success
;;   "Sends the record ETA interface package that will result in receiving data packages."
;;   (with-fixture init-destroy ()
;;     (setf eta:*eta-serial-proxy-impl* :atest-start-record)
;;     ;; the `start-record' function is the trigger for the boiler to send monitor data,
;;     ;; which is eventually forwarded to openHAB.
;;     ;; So we can expect that after calling `start-record' an http call will go out
;;     ;; to openHAB with data we expect to be sent.
;;     (is (eq :ok (eta-start-record)))
;;     (is-true (miscutils:assert-cond (lambda ()
;;                                   (and (string= *item-name* "HeatingETAOperatingHours")
;;                                        (string= *raw-data* "99.0")))
;;                                 0.2))))

;; (test stop-record--success
;;   "Sends the serial command to stop sending record packages."
;;   (with-fixture init-destroy ()
;;     (setf eta:*eta-serial-proxy-impl* :atest-stop-record)
;;     ;; the `stop-record' function is counterpart of the `start-record' function.
;;     ;; It will instruct the ETA to stop sending monitor data.
;;     (is (eq :ok (eta-stop-record)))
;;     (is-true (miscutils:assert-cond (lambda ()
;;                                   (> *stop-record-written-serial* 0))
;;                                 0.2))))

;; ------------------- server routes --------------------

(easy-routes:defroute rest-items ("/rest/items/:item" :method :post) ()
  (let ((raw-content (hunchentoot:raw-post-data :force-text t)))
    (setf *item-name* item)
    (setf *raw-data* raw-content)
    (log:debug "item: ~a, params: ~a" item raw-content))
  "")
