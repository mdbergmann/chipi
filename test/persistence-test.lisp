(defpackage :cl-hab.persistence-test
  (:use :cl :fiveam :cl-hab.persistence)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-hab.persistence-test)

(def-suite persistence-tests
  :description "Persistence tests"
  :in cl-hab.tests:test-suite)

(in-suite persistence-tests)

(def-fixture init-destroy-env ()
  (unwind-protect
       (progn 
         (&body))
    (envi:shutdown-env)))

(test make-persistence--simple
  "Make a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-persistence :persp-map :simple :every-change)))
      (print cut)
      (is-true cut)
      (is (typep cut 'persp::simple-persistence)))))

(test simple-persistence--store-and-fetch
  "Store a value in a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-persistence :persp-map :simple :every-change))
          (item (item:make-item 'foo)))
      (item:set-value item "foobar")
      (persp:store cut item)
      (sleep 0.3)
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 0.5
                   (equal (future:fresult fetched) "foobar"))))
      )))
