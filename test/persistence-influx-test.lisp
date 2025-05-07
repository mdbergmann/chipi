(defpackage :chipi.influx-persistence-test
  (:use :cl :fiveam :chipi.persistence :chipi.influx-persistence)
  (:import-from #:chipi.persistence-test
                #:assert-fetch-error)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.influx-persistence-test)

(def-suite influx-persistence-tests
  :description "Persistence tests for influxdb"
  :in chipi.tests:test-suite)

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

(defmacro test-type-value (type value itemid)
  `(let ((cut (make-cut))
         (item (item:make-item ,itemid :type-hint ,type)))
     (item:set-value item ,value)
     (persp:store cut item)
     (sleep 1)
     (let ((fetched (persp:fetch cut item)))
       (is-true (miscutils:await-cond 5.0
                  (let ((resolved (future:fresult fetched)))
                    (and (not (eq resolved :not-ready))
                         (equal (persisted-item-value resolved) ,value))))))))

(test influx-persistence--store-and-fetch--string-type
  "Store a string value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'string "some string" 'stringfield)))

(test influx-persistence--store-and-fetch--int-type
  "Store an integer value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'integer 1234 'intfield)))

(test influx-persistence--store-and-fetch--float-type
  "Store a float value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'float 1234.567 'floatfield)))

(test influx-persistence--store-and-fetch--bool-type-t
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'boolean 'item:true 'boolfield)))

(test influx-persistence--store-and-fetch--bool-type-nil
  "Store a bool value in a `influx` persistence and fetch the last one."
  (with-fixture init-destroy-env ()
    (test-type-value 'boolean 'item:false 'boolfield)))

(test influx-persistence--fetch-with-error--unknown-host
  "Fetch raises error, i.e. unknown host."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url "http://notexist:8086"
                :token ""
                :org "mabe"
                :bucket "test"))
          (item (item:make-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item)))
        (assert-fetch-error fetched)))))


(test influx-persistence--fetch-with-error--api-response-4xy
  "Fetch raises error, i.e. api response 4xy."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url "http://picellar:8086"
                :token "wrong-token"
                :org "mabe"
                :bucket "test"))
          (item (item:make-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item)))
        (assert-fetch-error fetched)))))
  
;; --------------------------------------
;; history api tests
;; --------------------------------------

(test influx-persistence--range
  "Retrieves item value history for time frame."
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'history :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      (dotimes (x 10)
        (item:set-value item x)
        (sleep 1))
      (sleep 1)
      (flet ((assert-fetched (fetched-range)
               (is-true (miscutils:await-cond 2.0 (future:complete-p fetched-range)))
               (let ((fetched-items (future:fresult fetched-range)))
                 (is (= (length fetched-items) 10))
                 (is (= (persisted-item-value (car fetched-items)) 0))
                 (is (= (persisted-item-value (car (last fetched-items))) 9))
                 (is (= (reduce #'+ (mapcar #'persisted-item-value fetched-items) :initial-value 0) 45)))))
        ;; pure relative
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-relative-range :seconds 20))))
          (assert-fetched fetched-range))
        ;; implicit absolute
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-relative-range
                                           :seconds 20 :end-ts (get-universal-time)))))
          (assert-fetched fetched-range))
        ;; absolute range
        (let ((fetched-range (persp:fetch cut item
                                          (persp:make-absolute-range
                                           (- (get-universal-time) 20)
                                           (get-universal-time)))))
          (assert-fetched fetched-range))          
        ))))

(test influx-persistence--range--fetch-with-error--api-response-4xy
  "Fetch raises error, i.e. api response 4xy."
  (with-fixture init-destroy-env ()
    (let ((cut (make-influx-persistence
                :persp-influx
                :base-url "http://picellar:8086"
                :token "wrong-token"
                :org "mabe"
                :bucket "test"))
          (item (item:make-item 'someid :type-hint 'string)))
      (let ((fetched (persp:fetch cut item (persp:make-relative-range :seconds 20))))
        (assert-fetch-error fetched)))))

;; --------------------------------------
;; aggregate API tests
;; --------------------------------------

(test influx-persistence--aggregate--influx-fn
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'avgfield :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      ;; push values 1,2,3,4,5 one per second
      (dotimes (i 5)
        (item:set-value item (1+ i))
        (sleep 1))
      (sleep 1)
      ;; fetch over last 20 seconds
      (flet ((assert-fetched (aggregate-fun value)
               (let ((fut (persp:fetch cut item
                                       (persp:make-relative-range :seconds 20)
                                       aggregate-fun)))
                 (is-true (miscutils:await-cond 2.0 (future:complete-p fut)))
                 (let ((result (future:fresult fut)))
                   (is (= (length result) 1))
                   (is (= (persisted-item-value (car result)) value))))))
        (assert-fetched :avg 3)
        (assert-fetched :min 1)
        (assert-fetched :max 5)
        (assert-fetched :sum 15)
        (assert-fetched :count 5)
        (assert-fetched :median 3)
        ))))

(test influx-persistence--aggregate--error--bad-aggregate-parameter
  (with-fixture init-destroy-env ()
    (let ((cut (make-cut))
          (item (item:make-item 'avgfield :type-hint 'integer)))
      (item:add-persistence item cut :frequency :every-change)
      (let ((fut (persp:fetch cut item
                              (persp:make-relative-range :seconds 20)
                              :not-supported)))
        (is-true (miscutils:await-cond 2.0 (future:complete-p fut)))
        (let ((result (future:fresult fut)))
          (is (eq (car result) :error)))))))
