(defpackage :cl-hab.hab-test
  (:use :cl :fiveam :cl-hab.hab :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-hab.hab-test)

(def-suite hab-tests
  :description "House automation bus"
  :in cl-hab.tests:test-suite)

(in-suite hab-tests)

(def-fixture clean-after ()
  (unwind-protect
       (progn
         (&body))
       (shutdown)))

(test define-items
  "Tests defining items."  
  (with-fixture clean-after ()
    (defconfig ())
    (item 'temp-a "Temperatur A")
    ;; define item second time, first will be removed
    (item 'temp-a "Temperatur A")
    (item 'temp-b "Temperatur B"
      (binding
       :initial-delay 0.1
       :pull (lambda () 1)))

    (is (= 2 (hash-table-count *items*)))
    (is (typep (gethash 'temp-a *items*) 'item:item))
    (is (typep (gethash 'temp-b *items*) 'item:item)))
  (is (= 0 (hash-table-count *items*))))

(test define-rules
  "Tests defining rules."
  (with-fixture clean-after ()
    (defconfig ())
    (rule "example foo"
          :when-cron '(:minute 10 :hour 0)
          :when-item-change 'temp-a
          :when-item-change 'temp-b
          :do (lambda (trigger)
                (format t "My rule code: ~a~%" trigger)))
    ;; make sure we can evaluate the same rule again
    (rule "example foo")
    (is (= 1 (hash-table-count *rules*)))
    (is (typep (gethash "example foo" *rules*) 'rule:rule)))
  (is (= 0 (hash-table-count *rules*))))

;; (test define-config
;;   (with-fixture clean-after ()
;;     (defconfig
;;       (defrules
;;           (rule "example foo"
;;                 :when-cron '(:minute 10 :hour 0)
;;                 :when-item-change 'temp-a
;;                 :when-item-change 'temp-b
;;                 :do (lambda (trigger)
;;                       (format t "My rule code: ~a~%" trigger))))
;;       )
;;     (print *rules*)
;;     (is (= 1 (hash-table-count *rules*)))
;;     (is (typep (gethash "example foo" *rules*) 'rule:rule))
;;     )
;;   (is (= 0 (hash-table-count *rules*)))
;;   )
