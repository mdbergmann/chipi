(defpackage :chipi.binding-test
  (:use :cl :fiveam :cl-mock :miscutils :chipi.binding)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.binding-test)

(def-suite binding-tests
  :description "Tests for bindings"
  :in chipi.tests:test-suite)

(in-suite binding-tests)

(def-fixture init-destroy-timer ()
  (unwind-protect
       (progn
         (&body))
    (envi:shutdown-env)))

(test binding--bind-item
  (let ((binding (make-function-binding)))
    (bind-item binding 'fake-item)
    (is (eq 'fake-item (car (binding::bound-items binding))))))

(test binding--pull-and-push
  "Test that binding can pull and push when both functions are given."
  (with-fixture init-destroy-timer ()
    (let* ((pull-called)
           (push-called)
           (binding (make-function-binding
                     :pull (lambda () (setf pull-called t))
                     :push (lambda (value) (setf push-called value)))))
      (exec-pull binding)
      (is-true pull-called)
      (exec-push binding "Foo")
      (is (equal "Foo" push-called)))))

(test binding--call-push-after-setting-value
  "Test that bindings is eventually passed through to push (after transformations)."
  (with-fixture init-destroy-timer ()
    (let* ((item (item:make-item 'my-item))
           (push-value)
           (binding (make-function-binding
                     :pull (lambda () 123)
                     :push (lambda (value) (setf push-value value))
                     :call-push-p t)))
      (item:add-binding item binding)
      (exec-pull binding)
      (is-true (await-cond 0.5 (eq 123 push-value))))))

(test binding--allow-binding-pull-to-control-calling-push
  "Test that bindings pull function can control whether to do a push (still :call-push-p needed)."
  (with-fixture init-destroy-timer ()
    (let* ((item (item:make-item 'my-item))
           (push-value)
           (binding (make-function-binding
                     :pull (lambda () (values 123 '(:push nil)))
                     :push (lambda (value) (setf push-value value))
                     :call-push-p t)))
      (item:add-binding item binding)
      (exec-pull binding)
      (sleep .5)
      (is-false push-value))))

(test binding--allow-pull-to-return-future
  "Test that bindings pull function can return a future."
  (with-fixture init-destroy-timer ()
    (let* ((item (item:make-item 'my-item))
           (push-value)
           (binding (make-function-binding
                     :pull (lambda () (values (future:with-fut 123) '(:push t)))
                     :push (lambda (value) (setf push-value value))
                     :call-push-p t)))
      (item:add-binding item binding)
      (exec-pull binding)
      (is-true (await-cond 0.5 (eq 123 push-value))))))

(test binding--does-not-call-push-after-on-pull-err
  "Test that bindings is not passed through to push if pull fails."
  (with-fixture init-destroy-timer ()
    (let* ((item (item:make-item 'my-item))
           (push-value)
           (binding (make-function-binding
                     :pull (lambda () (error "foo"))
                     :push (lambda (value) (setf push-value value))
                     :call-push-p t)))
      (item:add-binding item binding)
      (exec-pull binding)
      (sleep 0.5)
      (is-false push-value))))

(test binding--transform-between-after-pull
  "Tests that binding calls transform function to transform the pulled value."
  (with-fixture init-destroy-timer ()
    (let* ((item (item:make-item 'my-item))
           (push-value)
           (binding (make-function-binding
                     :pull (lambda () 123)
                     :transform (lambda (value) (1+ value))
                     :push (lambda (value) (setf push-value value))
                     :call-push-p t)))
      (item:add-binding item binding)
      (exec-pull binding)
      (is-true (await-cond 2.0 (eq 124 push-value))))))

(test binding--initial-delay->0--execute-pull
  "`initial-delay' > 0 means execute `pull' function after bind."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :pull (lambda () 123)
                      :initial-delay 0.1)))
        (answer (item:set-value _ value :push _)
          (assert (= value 123)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.5
                   (= 1 (length (invocations 'item:set-value)))))))))

(test binding--initial-delay-nil--no-execute-pull
  "`initial-delay' = nil means don't execute `pull' function after bind."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :pull (lambda () 123)
                      :initial-delay nil)))
        (bind-item binding 'my-fake-item)
        (sleep 0.5)
        (is (= 0 (length (invocations 'item:set-value))))))))

(test binding--delay-recurring
  "`delay' to reperatedly execute `pull'."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let* ((call-count 0)
             (binding (make-function-binding
                       :pull (lambda () (incf call-count))
                       :delay 0.1)))
        (answer item:name "my-fake-item")
        (answer (item:set-value _ value :push _)
          (assert (>= value 0)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 1.5
                   (>= call-count 3)))))))

(test binding--delay-calls-to-all-bound-items
  "`pulled' value should be set on all bound items."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((called-items nil)
            (binding (make-function-binding
                      :pull (lambda () 123)
                      :delay 0.2)))
        (answer item:name "my-fake-item")
        (answer (item:set-value item _ :push _)
          (setf called-items (cons item called-items)))
        (bind-item binding 'my-fake-item)
        (bind-item binding 'my-fake-item2)
        (is-true (await-cond 1.0
                   (>= (length called-items) 2)))
        (print called-items)
        (is-true (and (member 'my-fake-item called-items)
                      (member 'my-fake-item2 called-items)))))))

(test binding--destroy-cancels-timers
  "Destroying a binding should cancel all timers."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :pull (lambda () 123)
                      :initial-delay 0.1
                      :delay 0.1)))
        (answer item:name "my-fake-item")
        (answer (item:set-value _ _ :push _) t)
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.5
                   (>= 1 (length (invocations 'item:set-value)))))
        (answer (timer:cancel _) t)
        (destroy binding)
        (is-true (await-cond 0.5
                   (= 2 (length (invocations 'timer:cancel)))))))))
