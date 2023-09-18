(defpackage :cl-hab.influx-persistence-test
  (:use :cl :fiveam :cl-hab.persistence :cl-hab.influx-persistence)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-hab.influx-persistence-test)

(def-suite influx-persistence-tests
  :description "Persistence tests for influxdb"
  :in cl-hab.tests:test-suite)

(in-suite influx-persistence-tests)

(def-fixture init-destroy-env ()
  (unwind-protect
       (progn 
         (&body))
    (progn
      (envi:shutdown-env))))

(test make-persistence--influx
  "Make a `influx' persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence :persp-influx
                                        :base-url "http://picellar:8086"
                                        :token "A005mInE0uPMoW6l-kHmsxX1l8XC14Uw0UyAjV20GDq7qev0M1-kaGy77M7JH7wsIrc3-rTm1hRoHZ735Q4tHw=="
                                        :org "mabe"
                                        :bucket "test"
                                        :precision "s"
                                        )))
      (print cut)
      (is-true cut)
      (is (typep cut 'influx-persistence)))))

;; (test simple-persistence--store-and-fetch
;;   "Store a value in a `simple` persistence."
;;   (with-fixture init-destroy-env ()
;;     (let ((cut (make-simple-persistence :persp-map
;;                                         :storage-root-path #P"/tmp/cl-hab/persistence-test"))
;;           (item (item:make-item 'foo)))
;;       (item:set-value item "foobar")
;;       (persp:store cut item)
;;       (is-true (miscutils:await-cond 0.5
;;                  (uiop:file-exists-p #P"/tmp/cl-hab/persistence-test/FOO.store")))
;;       (let ((fetched (persp:fetch cut item)))
;;         (is-true (miscutils:await-cond 0.5
;;                    (let ((resolved (future:fresult fetched)))
;;                      (and (not (equal resolved :not-ready))
;;                           (equal (persisted-item-value resolved) "foobar"))))))
;;       )))
