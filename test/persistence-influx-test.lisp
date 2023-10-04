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
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url "http://picellar:8086"
                :token "some"
                :org "mabe"
                :bucket "test")))
      (print cut)
      (is-true cut)
      (is (typep cut 'influx-persistence)))))

;; --------------------------------------
;; !!! tests below require a running influxdb instance
;; --------------------------------------

(defun make-cut ()
  (make-influx-persistence
   :persp-influx
   :base-url "http://picellar:8086"
   :token "A005mInE0uPMoW6l-kHmsxX1l8XC14Uw0UyAjV20GDq7qev0M1-kaGy77M7JH7wsIrc3-rTm1hRoHZ735Q4tHw=="
   :org "mabe"
   :bucket "test"))

(test influx-persistence--store-and-fetch--string-type
  "Store a string value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'stringfield 'string)))
      (item:set-value item "foobar")
      (persp:store cut item)
      (sleep 1)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (equal (persisted-item-value resolved) "foobar")))))))))

(test influx-persistence--store-and-fetch--int-type
  "Store an integer value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'intfield 'integer)))
      (item:set-value item 1234)
      (persp:store cut item)
      (sleep 1)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (= (persisted-item-value resolved) 1234)))))))))

(test influx-persistence--store-and-fetch--float-type
  "Store a float value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'floatfield 'float)))
      (item:set-value item 1234.0)
      (persp:store cut item)
      (sleep 1)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (= (persisted-item-value resolved) 1234.0)))))))))

(test influx-persistence--store-and-fetch--bool-type-t
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'boolfield 'boolean)))
      (item:set-value item 'item:true)
      (persp:store cut item)
      (sleep 1)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (eq (persisted-item-value resolved) 'item:true)))))))))

(test influx-persistence--store-and-fetch--bool-type-nil
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'boolfield 'boolean)))
      (item:set-value item 'item:false)
      (persp:store cut item)
      (sleep 1)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (eq (persisted-item-value resolved) 'item:false)))))))))
