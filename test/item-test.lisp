(defpackage :chipi.item-test
  (:use :cl :fiveam :cl-mock :miscutils :chipi.item :chipi.simple-persistence)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.item-test)

(def-suite item-tests
  :description "Item tests"
  :in chipi.tests:test-suite)

(in-suite item-tests)

(defun delete-folder (path)
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname path)
   :validate t
   :if-does-not-exist :ignore))

(def-fixture init-destroy-env ()
  (unwind-protect
       (progn 
         (&body))
    (progn 
      (envi:shutdown-env)
      (delete-folder #P"/tmp/chipi"))))

(test make-item
  (unwind-protect
       (let ((item (make-item 'my-item :label "label")))
         (is-true item)
         (is-true (typep item 'item))
         (is-false (tags item))
         (is (eq (item:item-state-value (item:get-item-stateq item)) t)))
    (envi:shutdown-env)))

(test make-item--with-initial-value
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item :label "label" :initial-value 12345)))
      (is-true item)
      (is-true (typep item 'item))
      (is (eq (item:item-state-value (item:get-item-stateq item)) 12345)))))

(test make-item--with-tags
  "Test creating item with tags."
  (with-fixture init-destroy-env ()
    ;; Test with various tag formats
    (let ((item (make-item 'my-item 
                          :label "label" 
                          :tags '((:ui-readonly . nil)
                                  (:foo . "whatever")
                                  (:priority . 5)))))
      (is-true item)
      (is (equal '((:ui-readonly . nil) 
                   (:foo . "whatever")
                   (:priority . 5)) 
                 (tags item))))))

(test make-item--with-state--get--set
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item)))
      (is-true item)
      (let ((item-value (get-value item))
            (item-timestamp))
        (flet ((get-timestamp (item)
                 (item-state-timestamp (get-item-stateq item))))
          (is-true (await-cond 2
                   (eq (future:fresult item-value) t)))
          (setf item-timestamp (get-timestamp item))
          (is-true item-timestamp)
          (sleep 1)
          (is-true (set-value item 123))
          (is-true (await-cond 2
                     (let ((item-value (get-value item)))
                       (await-cond 0.5 
                         (eq (future:fresult item-value) 123)))))
          (is (> (get-timestamp item) item-timestamp)))))))

(test item--does-not-change-value-when-setting-same-value
  "Tests that item does not change value when same value is set."
  ;; potentially rewrite to not have to wait for 1.5 seconds
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item :initial-value 12345)))
      (let ((timestamp (item-state-timestamp (get-item-stateq item))))
        (sleep 1)
        (set-value item 12345)
        (sleep 0.5)
        (is (eq (item-state-timestamp (get-item-stateq item)) timestamp))))))

(test item--set-value-pushes-to-binding--with-passthrough
  "Tests that set-value pushes to binding."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item))
           (pushed-value)
           (binding (binding:make-function-binding
                     :pull (lambda ())
                     :push (lambda (value) (setf pushed-value value)))))
      (add-binding item binding)
      (set-value item "Foo")
      (is-true (await-cond 0.5
                 (equal pushed-value "Foo"))))))

(test item--set-value-does-no-push
  "Tests that set-value does not push to binding if :push key is false."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item))
           (pushed-value)
           (binding (binding:make-function-binding
                     :pull (lambda ())
                     :push (lambda (value) (setf pushed-value value)))))
      (add-binding item binding)
      (set-value item "Foo" :push nil)
      (sleep 0.3)
      (is-false (equal pushed-value "Foo")))))

(test item--set-value-does-no-push-even-with-passthrough--push-has-precedence
  "Tests that set-value does not push to binding if :push key is false."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item))
           (pushed-value)
           (binding (binding:make-function-binding
                     :pull (lambda ())
                     :call-push-p t  ;; default t
                     :push (lambda (value) (setf pushed-value value)))))
      (add-binding item binding)
      (set-value item "Foo" :push nil)
      (sleep 0.3)
      (is-false (equal pushed-value "Foo")))))

(test item--set-value-does-no-push-when-value-is-unchanged
  "Tests that set-value on an unchanged value does not reach the binding."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (add-binding item binding)
      (set-value item "Foo")
      (sleep 0.3)
      (is-false pushed-values))))

(test item--set-value-push-always--pushes-when-value-is-unchanged
  "Tests that `:push :always' reaches the binding every time, also when the
item already holds the value."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (add-binding item binding)
      (set-value item "Foo" :push :always)
      (set-value item "Foo" :push :always)
      (is-true (await-cond 0.5
                 (equal pushed-values '("Foo" "Foo")))))))

(test item--set-value-push-always--sets-value-when-changed
  "Tests that `:push :always' with a new value is an ordinary set-value: the
item is updated and the value pushed once."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (add-binding item binding)
      (set-value item "Bar" :push :always)
      (is-true (await-cond 0.5
                 (equal pushed-values '("Bar"))))
      (is (equal "Bar" (item-state-value (get-item-stateq item))))
      (sleep 0.3)
      (is (equal pushed-values '("Bar"))))))

(test item--set-value-push-always--keeps-state-and-raises-no-event-when-unchanged
  "Tests that `:push :always' with an unchanged value only pushes: the item
state, its timestamp and the subscribers are left alone."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item :initial-value "Foo"))
           (timestamp (item-state-timestamp (get-item-stateq item)))
           (pushed-values)
           (events)
           (binding (binding:make-function-binding
                     :push (lambda (value) (push value pushed-values)))))
      (ac:actor-of (isys:ensure-isys)
                   :init (lambda (self)
                           (ev:subscribe self self 'item-changed-event))
                   :receive (lambda (msg)
                              (when (typep msg 'item-changed-event)
                                (push msg events))))
      (add-binding item binding)
      (sleep 1)
      (set-value item "Foo" :push :always)
      (is-true (await-cond 0.5
                 (equal pushed-values '("Foo"))))
      (sleep 0.3)
      (is-false events)
      (is (eql timestamp (item-state-timestamp (get-item-stateq item)))))))

(test item--set-value-push-always--binding-call-push-p-has-precedence
  "Tests that a binding without `call-push-p' is not pushed to by `:push :always'."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item :initial-value "Foo"))
           (pushed-values)
           (binding (binding:make-function-binding
                     :call-push-p nil
                     :push (lambda (value) (push value pushed-values)))))
      (add-binding item binding)
      (set-value item "Foo" :push :always)
      (set-value item "Bar" :push :always)
      (is-true (await-cond 0.5
                 (equal "Bar" (item-state-value (get-item-stateq item)))))
      (sleep 0.3)
      (is-false pushed-values))))

(test item-raises-changed-event-when-changed
  "When item value is changed it should raise event."
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item))
          (ev-received))
      (ac:actor-of (isys:ensure-isys)
                   :init (lambda (self)
                           (ev:subscribe self self 'item-changed-event))
                   :receive (lambda (msg)
                              (format t "received event...~%")
                              (cond
                                ((typep msg 'item-changed-event)
                                 (setf ev-received msg)))))
      (set-value item 1)
      (is-true (await-cond 0.5 ev-received))
      (is (eq (item-changed-event-item ev-received) item))
      (is (eq (item-changed-event-old-value ev-received) t))
      (is (= 1 (item::item-state-value (act-cell:state item)))))))

(test item--pull-value-from-added-binding
  "Binding that provides the value updates to the item."
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item))
          (binding (binding:make-function-binding
                    :pull (lambda () 1)
                    :initial-delay 0.1)))
      (add-binding item binding)
      (is-true (await-cond 2
                 (let ((item-value (get-value item)))
                   (await-cond 0.3
                     (eq (future:fresult item-value) 1))))))))

(test item--cleanup-on-destroy
  "Tests that item cleans up bindings on destroy."
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'my-item))
          (binding (binding:make-function-binding
                    :pull (lambda () 1)
                    :initial-delay 0.1))
          (persp (make-simple-persistence :foo
                  :storage-root-path #P"/tmp/chipi/persistence-test")))
      (add-binding item binding)
      (add-persistence item persp
                       :frequency :every-1s)  ;; triggers adding a timer
      (with-mocks ()
        (answer (binding:destroy _) t)
        (answer (timer:cancel _) t)
        (destroy item)
        (is (= 1 (length (invocations 'binding:destroy))))
        (is (= 1 (length (invocations 'timer:cancel))))))))

(test item--with-simple-persistence
  "Tests that item persists its value."
  (with-fixture init-destroy-env ()
    (let* ((persp (make-simple-persistence
                   :foo
                   :storage-root-path #P"/tmp/chipi/persistence-test"))
           (item (make-item 'my-item)))
      (add-persistence item persp
                       :frequency :every-change)  ;; default
      (set-value item 1)
      (is-true (await-cond 2
                 (uiop:file-exists-p
                  (merge-pathnames #P"MY-ITEM.store"
                                   #P"/tmp/chipi/persistence-test/"))))
      ;; make load item on startup
      (destroy item)
      (let ((item (make-item 'my-item)))
        (add-persistence item persp :load-on-start t)
        (is-true (await-cond 2
                   (let ((item-value (get-value item)))
                     (await-cond 0.3
                       (equal (future:fresult item-value) 1)))))))))

(test item--with-persistence--freq-every-x
  "Tests that item perists it's value with frequency :every-x."
  (with-fixture init-destroy-env ()
    (let* ((persp (make-simple-persistence
                   :foo
                   :storage-root-path #P"/tmp/chipi/persistence-test"))
           (item (make-item 'my-item)))
      (add-persistence item persp
                       :frequency :every-1s)

      (with-mocks ()
        (answer persp:store t)
        (set-value item 1)
        (is-true (await-cond 3
                   (= 2 (length (invocations 'persp:store)))))))))

(test item--parse-persistence-frequency
  "Tests the parsing of frequency that can be specified on the item."
  (signals error (item::%parse-frequency "foo-bar"))
  (is (= 12 (item::%parse-frequency :every-12s)))
  (is (= (* 12 60) (item::%parse-frequency :every-12m)))
  (is (= (* 30 60) (item::%parse-frequency :every-30m)))
  (is (= (* 12 60 60) (item::%parse-frequency :every-12h))))

(test item--with-persistence--should-not-persist-on-load-on-start
  "Test that when two persistences are defined, one with `:load-on-start',
that when loading the value the second (or any more) persistence does not persit it."
  (with-fixture init-destroy-env ()
    (let* ((persp1 (make-simple-persistence
                    :foo
                    :storage-root-path #P"/tmp/chipi/persistence-test"))
           (persp2 (make-simple-persistence
                    :bar
                    :storage-root-path #P"/tmp/chipi/persistence-test"))
           (item (make-item 'my-item :initial-value 0)))
      (with-mocks ()
        (answer persp:fetch (future:with-fut
                              (persp:make-persisted-item
                               :value 1
                               :timestamp (get-universal-time))))
        (answer persp:store (error "No call to store should be made!"))
        (add-persistence item persp2
                         :frequency :every-change)
        (add-persistence item persp1
                         :load-on-start t
                         :frequency :every-change)
        (is-true (await-cond 2
                   (= 1 (length (invocations 'persp:fetch)))))
        (sleep 0.5)
        (is (= 0 (length (invocations 'persp:store)))))
      )
    )
  )


(test item--tags-remain-unchanged
  "Test that tags remain unchanged through item lifecycle."
  (with-fixture init-destroy-env ()
    (let* ((initial-tags '((:ui-readonly . t) (:room . "living-room")))
           (item (make-item 'my-item 
                           :initial-value 42
                           :tags initial-tags)))
      ;; Change item value
      (set-value item 100)
      (is-true (await-cond 0.5
                 (let ((item-value (get-value item)))
                   (await-cond 0.3
                     (= (future:fresult item-value) 100)))))
      
      ;; Tags should remain exactly the same
      (is (equal initial-tags (tags item))))))

(defclass fail-fetch-persistence (simple-persistence:simple-persistence) ())
(defmethod persp:retrieve ((persistence fail-fetch-persistence) item)
  '(:error . "Failed to fetch."))

(test item--simple-persistence--dont-update-on-fetch-error
  "Tests that item persists its value."
  (with-fixture init-destroy-env ()
    (let* ((persp (persp::make-persistence :foo :type 'fail-fetch-persistence))
           (item (make-item 'my-item)))
      ;; make load item on startup
      (destroy item)
      (let ((item (make-item 'my-item))
            (fetched-item-value))
        (add-persistence item persp :load-on-start t)
        (sleep 2)
        (let ((item-value (get-value item)))
          (sleep 1)
          (setf fetched-item-value (future:fresult item-value)))
        (format t "fetched-item-value: ~a~%" fetched-item-value)
        (is-true (eq t fetched-item-value))))))

(defclass fail-persist-persistence (simple-persistence:simple-persistence) ())
(defmethod persp:persist ((persistence fail-persist-persistence) item)
  (format t "persisting...~%")
  (error "Failed to persist."))

(test item--push-even-on-persist-err
  "Tests that item value is pushed even if persistence fails."
  (with-fixture init-destroy-env ()
    (let* ((persp (persp::make-persistence :foo :type 'fail-persist-persistence))
           (item (make-item 'my-item))
           (pushed-value)
           (binding (binding:make-function-binding
                     :push (lambda (value) (setf pushed-value value))
                     :call-push-p t)))
      (add-binding item binding)
      (add-persistence item persp)
      (sleep 0.5)
      (set-value item 1)
      (is-true (await-cond 2 (equal pushed-value 1))))))

(test item--pushes-on-all-bindings-even-if-previous-raises-err--with-order-check
  "Tests that item value is pushed to all bindings even if previous fails with error.
This also tests the error handling in item calling `exec-push' of the binding."
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'my-item))
           (pushed-value)
           (binding1 (binding:make-function-binding
                      :push (lambda (value)
                              (push value pushed-value)
                              (error "on push!"))
                      :call-push-p t))
           (binding2 (binding:make-function-binding
                      :push (lambda (value)
                              (push (1+ value) pushed-value))
                      :call-push-p t)))
      (add-binding item binding1)
      (add-binding item binding2)
      (set-value item 1)
      (is-true (await-cond 2 (equalp
                              pushed-value (list 2 1)))))))

(test table-value--validation-and-snapshots
  (with-fixture init-destroy-env ()
    (let* ((key (copy-seq "MixedCase"))
           (text (copy-seq "original"))
           (table (alexandria:plist-hash-table (list key text) :test #'equal))
           (item (make-item 'table-item :type-hint 'table :initial-value table)))
      (setf (char key 0) #\x (char text 0) #\x)
      (is (equal "original" (gethash "MixedCase" (item-state-value (get-item-stateq item)))))
      (let ((snapshot (item-state-value (get-item-stateq item))))
        (setf (char (gethash "MixedCase" snapshot) 0) #\z)
        (is (equal "original" (gethash "MixedCase" (item-state-value (get-item-stateq item))))))
      (dolist (bad (list nil t (vector 1) (list 1)))
        (signals invalid-table-value (set-value item bad)))
      (dolist (bad (list nil t 'cl:null (list 1) #(1) (make-hash-table)))
        (signals invalid-table-value
          (set-value item (alexandria:plist-hash-table (list "bad" bad)))))
      (signals invalid-table-value
        (set-value item (alexandria:plist-hash-table (list :bad 1))))
      (is (equal "original" (gethash "MixedCase" (item-state-value (get-item-stateq item))))))))

(test table-value--default-and-explicit-nil
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'empty-table :type-hint 'table)))
      (is (= 0 (hash-table-count (item-state-value (get-item-stateq item))))))
    (signals invalid-table-value
      (make-item 'invalid-table :type-hint 'table :initial-value nil))))

(test table-value--equal-snapshots-and-always-push
  (with-fixture init-destroy-env ()
    (let* ((table (alexandria:plist-hash-table '("A" 1 "a" item:false) :test #'equal))
           (item (make-item 'equal-table :type-hint 'table :initial-value table))
           (events nil)
           (subscribed nil)
           (pushes 0)
           (binding (binding:make-function-binding :pull (lambda ())
                       :push (lambda (value) (declare (ignore value)) (incf pushes))))
           (timestamp (item-state-timestamp (get-item-stateq item))))
      (ac:actor-of (isys:ensure-isys)
                   :init (lambda (self)
                           (ev:subscribe self self 'item-changed-event)
                           (setf subscribed t))
                   :receive (lambda (msg)
                              (when (typep msg 'item-changed-event)
                                (push msg events))))
      (is-true (await-cond 1 subscribed))
      (add-binding item binding)
      (set-value item (copy-table-value table) :timestamp (1+ timestamp))
      (let ((barrier (get-value item)))
        (is-true (await-cond 1 (hash-table-p (future:fresult barrier)))))
      (is (= 0 pushes))
      (is (= timestamp (item-state-timestamp (get-item-stateq item))))
      (set-value item table :push :always :timestamp (1+ timestamp))
      (is-true (await-cond 1 (= 1 pushes)))
      (sleep 0.1)
      (is-false events)
      (is (= timestamp (item-state-timestamp (get-item-stateq item)))))))

(test table-value--enqueue-and-future-isolation
  (with-fixture init-destroy-env ()
    (let* ((item (make-item 'queued-table :type-hint 'table))
           (key (copy-seq "Field"))
           (value (copy-seq "text"))
           (table (alexandria:plist-hash-table (list key value) :test #'equal)))
      (set-value item table)
      (setf (char key 0) #\x (char value 0) #\x)
      (clrhash table)
      (let ((future (get-value item))
            (snapshot nil))
        (is-true (await-cond 1
                   (hash-table-p (setf snapshot (future:fresult future)))))
        (is (equal "text" (gethash "Field" snapshot)))
        (setf (char (gethash "Field" snapshot) 0) #\x)
        (is (equal "text" (gethash "Field" (item-state-value (get-item-stateq item)))))))))

(test table-value--finite-real-fields
  (dolist (value (list 12 1/3 2.5 3.5d0 "text" 'item:true 'item:false))
    (let ((table (alexandria:plist-hash-table (list "value" value))))
      (is (equal value (gethash "value" (copy-table-value table))))))
  (dolist (value (list #C(1 2)
                      #+sbcl sb-ext:double-float-positive-infinity
                      #+sbcl sb-ext:double-float-negative-infinity))
    (signals invalid-table-value
      (copy-table-value (alexandria:plist-hash-table (list "value" value))))))

(test table-value--publishes-complete-state-before-notification
  (with-fixture init-destroy-env ()
    (let* ((value (alexandria:plist-hash-table '("field" 1) :test #'equal))
           (item (make-item 'atomic-table :type-hint 'table :initial-value value))
           (old-state (act-cell:state item))
           (old-timestamp (item-state-timestamp old-state))
           (new-timestamp (1+ old-timestamp))
           (observed nil))
      (with-mocks ()
        (answer ev:publish
          (setf observed (get-item-stateq item)))
        (set-value item (alexandria:plist-hash-table '("field" 2) :test #'equal)
                   :timestamp new-timestamp)
        (is-true (await-cond 1 observed))
        (is (= 2 (gethash "field" (item-state-value observed))))
        (is (= new-timestamp (item-state-timestamp observed)))
        (is (= 1 (gethash "field" (item-state-value old-state))))
        (is (= old-timestamp (item-state-timestamp old-state)))
        (is (not (eq old-state (act-cell:state item))))))))

(test table-type-hint--package-independent
  (is-true (table-type-hint-p 'table))
  (is-true (table-type-hint-p (make-symbol "TABLE")))
  (is-false (table-type-hint-p nil))
  (is-false (table-type-hint-p "TABLE"))
  (is-false (table-type-hint-p 'cl:hash-table))
  (with-fixture init-destroy-env ()
    (let ((item (make-item 'foreign-table-hint :type-hint (make-symbol "TABLE"))))
      (is (= 0 (hash-table-count (item-state-value (get-item-stateq item)))))
      (signals invalid-table-value (set-value item nil)))))
