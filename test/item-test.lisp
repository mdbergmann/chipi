(defpackage :cl-hab.item-test
  (:use :cl :fiveam :cl-mock :miscutils :cl-hab.item :cl-hab.simple-persistence)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-hab.item-test)

(def-suite item-tests
  :description "Item tests"
  :in cl-hab.tests:test-suite)

(in-suite item-tests)

(defun delete-folder (path)
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname path)
   :validate t
   :if-does-not-exist :ignore))

(def-fixture init-destroy-isys ()
  (unwind-protect
       (progn 
         (&body))
    (progn 
      (envi:shutdown-env)
      (delete-folder #P"/tmp/cl-hab"))))

(test make-item
  (unwind-protect
       (let ((item (make-item 'my-item "label")))
         (is-true item)
         (is-true (typep item 'item)))
    (envi:shutdown-env)))

(test make-item--with-state--get--set
  (with-fixture init-destroy-isys ()
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

(test item--set-value-pushes-to-binding--with-passthrough
  "Tests that set-value pushes to binding."
  (with-fixture init-destroy-isys ()
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
  (with-fixture init-destroy-isys ()
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
  (with-fixture init-destroy-isys ()
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
  (with-fixture init-destroy-isys ()
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
  (with-fixture init-destroy-isys ()
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
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item))
          (binding (binding:make-function-binding
                    :pull (lambda () 1)
                    :initial-delay 0.1))
          (persp (make-simple-persistence :foo
                  :storage-root-path #P"/tmp/cl-hab/persistence-test")))
      (add-binding item binding)
      (add-persistence item persp)
      (with-mocks ()
        (answer (binding:destroy _) t)
        (destroy item)
        (is (= 1 (length (invocations 'binding:destroy))))))))

(test item--with-simple-persistence
  "Tests that item persists its value."
  (with-fixture init-destroy-isys ()
    (let* ((persp (make-simple-persistence :foo
                   :storage-root-path #P"/tmp/cl-hab/persistence-test"))
           (item (make-item 'my-item)))
      (add-persistence item persp
                       :frequency :every-change)  ;; default
      (set-value item 1)
      (is-true (await-cond 2
                 (uiop:file-exists-p
                  (merge-pathnames #P"MY-ITEM.store"
                                   #P"/tmp/cl-hab/persistence-test/"))))
      ;; make load item on startup
      (destroy item)
      (let ((item (make-item 'my-item)))
        (add-persistence item persp :load-on-start t)
        (is-true (await-cond 2
                   (let ((item-value (get-value item)))
                     (await-cond 0.3
                       (equal (future:fresult item-value) 1)))))))))
  
