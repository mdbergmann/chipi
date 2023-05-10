(defpackage :cl-eta.hab-test
  (:use :cl :fiveam :cl-eta.hab :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.hab-test)

(def-suite hab-tests
  :description "House automation bus"
  :in cl-eta.tests:test-suite)

(in-suite hab-tests)

(test define-items
  (envi:ensure-isys)
  (unwind-protect
       (progn
         (defitems
           (item 'tempA "Temperatur A")
           (item 'tempB "Temperatur B"))
         
         (print *items*)
         (is (= 4 (length hab:*items*)))
         (is (typep (getf *items* 'tempA) 'item:item))
         (is (typep (getf *items* 'tempB) 'item:item))
         )
    (progn
      (envi:shutdown-isys))))

(run! 'hab-tests)
