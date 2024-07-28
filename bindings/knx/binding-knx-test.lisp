(defpackage :chipi.binding.knx-test
  (:use :cl :fiveam :chipi.binding.knx)
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
  (let ((cut (knx-binding
              :ga "1/2/3"
              :dpt "9.001")))
    (is-true cut)
    (is (address:knx-group-address-p (group-address cut)))
    (is (eq 'dpt:dpt-9.001 (dpt-type cut)))))

;; (test binding-listens-on-ga-changes
;;   (let ((cut (knx-binding
;;               :ga "1/2/3"
;;               :dpt "9.001")))
;;     ()
;;   )


(run! 'knx-binding-tests)
