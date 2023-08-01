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

(defun delete-folder (path)
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname path)
   :validate t
   :if-does-not-exist :ignore))

(def-fixture clean-after ()
  (unwind-protect
       (progn
         (&body))
    (progn 
      (shutdown)
      (delete-folder #P"/tmp/hab-test"))))

;; test for `CONFIG'

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

(test define-persistence
  "Tests defining persistence."
  (with-fixture clean-after ()
    (defconfig ())
    (persistence :default
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    (persistence :foo
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    ;; redefining same persistence to check if possible
    (persistence :default
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    (item 'temp-a "Temperatur A"
      :persistence '(:id :default :frequency :every-change)
      :persistence '(:id :foo :frequency :every-3minutes))

    (is (= 2 (hash-table-count *persistences*)))
    (is (typep (gethash :default *persistences*) 'persp:persistence))

    ;; item contains persistence
    (is (= 2 (length (item::persistences (gethash 'temp-a *items*)))))))

