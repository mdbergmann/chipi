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
         (is-false (group item))
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

(test make-item--with-groups
  "Test creating item with tags."
  (with-fixture init-destroy-env ()
    ;; Test with various tag formats
    (let ((item (make-item 'my-item 
                          :label "label" 
                          :group '(group1 group2))))
      (is-true item)
      (is (equal '(group1 group2)
                 (group item))))))

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
                     :call-push-p t
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
                     :call-push-p t
                     :push (lambda (value) (setf pushed-value value)))))
      (add-binding item binding)
      (set-value item "Foo" :push nil)
      (sleep 0.3)
      (is-false (equal pushed-value "Foo")))))

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
