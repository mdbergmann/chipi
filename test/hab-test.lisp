(defpackage :chipi.hab-test
  (:use :cl :fiveam :chipi.hab :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.hab-test)

(def-suite hab-tests
  :description "House automation bus"
  :in chipi.tests:test-suite)

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

(test define-itemgroups
  "Tests creating itemgroups."
  (with-fixture clean-after ()
    (defconfig "chipi")
    (defitemgroup 'group1 "Group1")
    (itemgroup:add-item (get-itemgroup 'group1) (defitem 'item1 "Item1" nil))
    ;; plus 1 for default group
    (is (= 2 (hash-table-count *itemgroups*)))
    ;; recreate group should preserve the added item
    (defitemgroup 'group1 "Group1")
    (is (= 2 (Hash-table-count *itemgroups*)))
    (is (string= "ITEM1" (item:name (car (get-items-on-group 'group1))))))
  (is (= 0 (hash-table-count *itemgroups*))))

(test define-items
  "Tests defining items."
  (labels ((get-items-of-groups (group-ids)
             (alexandria:flatten
              (mapcar #'get-items-on-group group-ids)))
           (assert-item-in-groups (item-id group-ids)
             (is (= (length group-ids)
                    (length (filter (lambda (x) (equal (act-cell:name x)
                                                  (symbol-name item-id)))
                                    (get-items-of-groups
                                     (mapcar #'itemgroup:id (get-itemgroups))))))))
           (assert-groupitems (item-id group-ids)
             (is (= (length group-ids)
                    (length (get-items-of-groups group-ids))))
             (is (member (get-item item-id)
                         (get-items-of-groups group-ids)))))
    (with-fixture clean-after ()
      (defconfig "chipi")
      (defitemgroup 'group2 "Group2")
      (defitemgroup 'group3 "Group3")
      (defitem 'temp-a "Temperatur A" nil
        ;; two groups
        :group '(group2 group3))
      (assert-groupitems 'temp-a '(group2 group3))
      (assert-item-in-groups 'temp-a '(group2 group3))
      ;; define item second time, first will be removed
      (defitem 'temp-a "Temperatur A" nil
        ;; one group, item should be removed from group3
        :group '(group2))
      (assert-groupitems 'temp-a '(group2))
      (assert-item-in-groups 'temp-a '(group2))

      (defitem 'temp-b "Temperatur B" nil
        (binding
         :initial-delay 0.1
         :pull (lambda () 1)))
      (defitem 'temp-c "Temperatur C" 'integer
        :initial-value 1)

      (is (= 3 (hash-table-count *items*)))
      (is (typep (gethash 'temp-a *items*) 'item:item))
      (is (typep (gethash 'temp-b *items*) 'item:item))
      (is (typep (gethash 'temp-c *items*) 'item:item))
      (is (= 1 (item:item-state-value (item:get-item-stateq (gethash 'temp-c *items*))))))
    (is (= 0 (hash-table-count *items*)))))

(test define-rules
  "Tests defining rules."
  (with-fixture clean-after ()
    (defconfig "chipi")
    (defrule "example foo"
      :when-cron '(:minute 10 :hour 0)
      :when-item-change 'temp-a
      :when-item-change 'temp-b
      :do (lambda (trigger)
            (format t "My rule code: ~a~%" trigger)))
    ;; make sure we can evaluate the same rule again
    (defrule "example foo")
    (is (= 1 (hash-table-count *rules*)))
    (is (typep (gethash "example foo" *rules*) 'rule:rule)))
  (is (= 0 (hash-table-count *rules*))))

(test define-items-with-tags
  "Tests defining items with tags."
  (with-fixture clean-after ()
    (defconfig "chipi")
    (defitemgroup 'sensors "Sensors")
    
    ;; Define item with tags
    (defitem 'temp-sensor "Temperature Sensor" 'float
      :initial-value 20.0
      :group '(sensors)
      :tags '((:ui-readonly . t)
              (:unit . "celsius")
              (:category . "sensor")))
    
    ;; Define item without tags
    (defitem 'humidity-sensor "Humidity Sensor" 'float
      :initial-value 50.0
      :group '(sensors))
    
    ;; Verify tags are set correctly
    (let ((temp-item (get-item 'temp-sensor))
          (humidity-item (get-item 'humidity-sensor)))
      (is (equal '((:ui-readonly . t)
                   (:unit . "celsius")
                   (:category . "sensor"))
                 (item:tags temp-item)))
      (is (null (item:tags humidity-item))))
    
    ;; Redefine item with different tags
    (defitem 'temp-sensor "Temperature Sensor" 'float
      :initial-value 20.0
      :group '(sensors)
      :tags '((:ui-readonly . nil)
              (:unit . "fahrenheit")))
    
    ;; Verify tags were updated
    (let ((temp-item (get-item 'temp-sensor)))
      (is (equal '((:ui-readonly . nil)
                   (:unit . "fahrenheit"))
                 (item:tags temp-item))))))

(test define-persistence
  "Tests defining persistence."
  (with-fixture clean-after ()
    (defconfig "chipi")
    (defpersistence :default
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    (defpersistence :foo
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    ;; redefining same persistence
    (defpersistence :default
                 (lambda (id)
                   (simple-persistence:make-simple-persistence
                    id :storage-root-path #P"/tmp/hab-test")))
    (defitem 'temp-a "Temperatur A" 'float
      :persistence '(:id :default :frequency :every-change)
      :persistence '(:id :foo :frequency :every-3minutes)) ;; `every-3minutes' doesn't exist

    (is (= 2 (hash-table-count *persistences*)))
    (is (typep (gethash :default *persistences*) 'persp:persistence))

    ;; item contains persistence
    (is (= 2 (length (item::persistences (gethash 'temp-a *items*)))))))

(test shutdown-calls-shutdown-hooks
  (let ((called nil))
    (add-to-shutdown (lambda () (setf called t)))
    (shutdown)
    (is-true called)))

(test shutdown-calls-shutdown-hooks--and-continous-on-error
  (let ((called nil))
    (add-to-shutdown (lambda () (setf called t)))
    (add-to-shutdown (lambda () (error "Foo err")))
    (shutdown)
    (is-true called)))
