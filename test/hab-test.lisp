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

(test define-config
  (with-fixture clean-after ()
    (defconfig
      (defitems
        (item 'temp-a "Temperatur A")
        (item 'temp-b "Temperatur B"
          (binding
           :initial-delay 0.1
           :pull (lambda ()
                   (format t "Calling pull.~%")
                   1 ;; return 1
                   ))))

      (defrules
          (rule "example foo"
                :when-cron '(:minute 10 :hour 0)
                :when-item-change 'temp-a
                :when-item-change 'temp-b
                :do (lambda (trigger)
                      (format t "My rule code: ~a~%" trigger))))
      )
    (print *items*)
    (is (= 2 (hash-table-count hab:*items*)))
    (is (typep (gethash 'temp-a *items*) 'item:item))
    (is (typep (gethash 'temp-b *items*) 'item:item))

    (print *rules*)
    (is (= 1 (hash-table-count *rules*)))
    (is (typep (gethash "example foo" *rules*) 'rule:rule))
    )
  (is (= 0 (hash-table-count *items*)))
  (is (= 0 (hash-table-count *rules*)))
  )
