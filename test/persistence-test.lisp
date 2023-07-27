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

(test make-persistence--map
  "Make a `map` persistence"
  (with-fixture init-destroy-env ()
    (let ((cut (make-persistence :persp-map :map :every-change)))
      (is-true cut)
      (is (typep cut 'persp::map-persistence))
      )))
