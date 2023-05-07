(defpackage :cl-eta.hab-test
  (:use :cl :fiveam :cl-mock :cl-eta.hab :miscutils)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.hab-test)

(def-suite hab-tests
  :description "House automation bus"
  :in cl-eta.tests:test-suite)

(in-suite hab-tests)

(def-fixture init-destroy-isys ()
  (unwind-protect
       (&body))
    (shutdown-isys))

(test make-item
  (unwind-protect
       (let ((item (make-item 'my-item)))
         (is-true item)
         (is-true (typep item 'item)))
    (hab:shutdown-isys)))

(test make-item--with-state--get--set
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item)))
      (is-true item)
      (let ((item-value (get-value item)))
        (is-true (await-cond 2
                   (eq (future:fresult item-value) t)))
        (is-true (set-value item 123))
        (setf item-value (get-value item))
        (is-true (await-cond 2
                   (eq (future:fresult item-value) 123)))        
        ))))

(test item-raises-changed-event-when-changed
  "When item value is changed it should raise event."
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item))
          (ev-received))
      (ac:actor-of hab::*isys*
                   :init (lambda (self)
                           (ev:subscribe self self 'hab:item-changed-event))
                   :receive (lambda (msg)
                              (format t "received event...~%")
                              (cond
                                ((typep msg 'hab:item-changed-event)
                                 (setf ev-received msg)))))
      (set-value item 1)
      (is-true (await-cond 0.5
                 ev-received))
      (is (eq (item-changed-event-item ev-received) item))
      (is (= 1 (hab::item-state-value (act-cell:state item)))))))


(def-fixture init-destroy-timer ()
  (unwind-protect
       (progn
         (hab::ensure-timer)
         (&body))
    (shutdown-timer)))

(test binding--bind-item
  (let ((binding (make-function-binding :retrieve nil)))
    (bind-item binding 'fake-item)
    (is (eq 'fake-item (car (hab::bound-items binding))))))

(test binding--initial-delay->0--execute-retrieve
  "`initial-delay' >= 0 means execute `retrieve' function after bind."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :retrieve (lambda () 123)
                      :initial-delay 0.1)))
        (answer (hab:set-value _ value)
          (assert (= value 123)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.5
                   (= 1 (length (invocations 'hab:set-value)))))))))

(test binding--initial-delay-nil--no-execute-retrieve
  "`initial-delay' = nil means don't execute `retrieve' function after bind."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let ((binding (make-function-binding
                      :retrieve (lambda () 123)
                      :initial-delay nil)))
        (bind-item binding 'my-fake-item)
        (sleep 0.5)
        (is (= 0 (length (invocations 'hab:set-value))))))))

(test binding--delay-recurring
  "`delay' to reperatedly execute `retrieve'."
  (with-fixture init-destroy-timer ()
    (with-mocks ()
      (let* ((call-count 0)
             (binding (make-function-binding
                       :retrieve (lambda () (incf call-count)
                                   :delay 0.1))))
        (answer (hab:set-value _ value)
          (assert (>= value 0)))
        (bind-item binding 'my-fake-item)
        (is-true (await-cond 0.3
                   (>= 2 (length (invocations 'hab:set-value)))))))))
