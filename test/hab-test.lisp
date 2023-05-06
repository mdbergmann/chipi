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
       (&body)
    (shutdown-isys)))

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

(test make-item--with-function-binding
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item
                           :binding (make-function-binding
                                     :retrieve nil))))
      (is-true (binding item)))))

(test make-item--initial-delay-0--execute-retrieve
  "`initial-delay' = 0 means execute `retrieve' function after bind."
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item
                           :binding (make-function-binding
                                     :retrieve (lambda () 123)
                                     :initial-delay 0))))
      (is-true (binding item))
      (sleep 0.1)  ;; otherwise we'll get the initial value
      (let ((item-value (get-value item)))
        (is-true (await-cond 2
                   (eq (future:fresult item-value) 123)))))))

(test make-item--initial-delay-nil--no-execute-retrieve
  "`initial-delay' = nil means don't execute `retrieve' function after bind."
  (with-fixture init-destroy-isys ()
    (let ((item (make-item 'my-item
                           :binding (make-function-binding
                                     :retrieve (lambda () 123)
                                     :initial-delay nil))))
      (is-true (binding item))
      (sleep 0.1)  ;; otherwise we'll get the initial value
      (let ((item-value (get-value item)))
        (sleep 0.5)
        (is-true (await-cond 2
                   (eq (future:fresult item-value) t)))))))

(test item-delay-recuring
  "`delay' to reperatedly execute `retrieve'."
  (with-fixture init-destroy-isys ()
    (let* ((call-count 0)
           (item (make-item 'my-item
                            :binding (make-function-binding
                                      :retrieve (lambda () (incf call-count))
                                      :delay 0.3))))
      (sleep 1.0)
      (let ((item-value (get-value item)))
        (is-true (await-cond 2
                   (let ((result (future:fresult item-value)))
                     (case result
                       (:not-ready nil)
                       (otherwise (> result 1))))))))))

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
      (is (eq (hab:item-changed-event-item ev-received) item))
      (is (= 1 (hab::item-state-value (act-cell:state item)))))))

(run! 'hab-tests)
