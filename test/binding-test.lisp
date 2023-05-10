(defpackage :cl-eta.binding-test
  (:use :cl :fiveam :cl-mock :miscutils :cl-eta.binding)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.binding-test)

(def-suite binding-tests
  :description "Tests for bindings"
  :in cl-eta.tests:test-suite)

(in-suite binding-tests)

(def-fixture init-destroy-timer ()
  (unwind-protect
       (progn
         (&body))
    (hab:shutdown-timer)))

(test binding--bind-item
  (let ((binding (make-function-binding)))
    (bind-item binding 'fake-item)
    (is (eq 'fake-item (car (binding::bound-items binding))))))

(test binding--pull-and-push
  "Test that binding can pull and push when both functions are given."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let* ((pull-called)
             (push-called)
             (binding (make-function-binding
                       :pull (lambda () (setf pull-called t))
                       :push (lambda (value) (setf push-called value)))))
        (exec-pull binding)
        (is-true pull-called)
        (exec-push binding "Foo")
        (is (equal "Foo" push-called))))))

(test binding--initial-delay->0--execute-pull
  "`initial-delay' >= 0 means execute `pull' function after bind."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :pull (lambda () 123)
                      :initial-delay 0.1)))
        (answer (item::set-value--internal _ value)
          (assert (= value 123)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.5
                   (= 1 (length (invocations 'item::set-value--internal)))))))))

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
        (answer (item::set-value--internal _ value)
          (assert (>= value 0)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.3
                   (>= 2 (length (invocations 'item::set-value--internal)))))))))

(test binding--delay-calls-to-all-bound-items
  "`pulled' value should be set on all bound items."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((called-items nil)
            (binding (make-function-binding
                      :pull (lambda () 123)
                      :delay 0.2)))
        (answer (item::set-value--internal item _)
          (setf called-items (cons item called-items)))
        (bind-item binding 'my-fake-item)
        (bind-item binding 'my-fake-item2)
        (is-true (await-cond 1.0
                   (>= (length called-items) 2)))
        (print called-items)
        (is-true (and (member 'my-fake-item called-items)
                      (member 'my-fake-item2 called-items)))))))
