(defpackage :cl-eta.item-test
  (:use :cl :fiveam :cl-mock :miscutils :cl-eta.item)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-eta.item-test)

(def-suite item-tests
  :description "Item tests"
  :in cl-eta.tests:test-suite)

(in-suite item-tests)

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
      (ac:actor-of (hab:ensure-isys)
                   :init (lambda (self)
                           (ev:subscribe self self 'item-changed-event))
                   :receive (lambda (msg)
                              (format t "received event...~%")
                              (cond
                                ((typep msg 'item-changed-event)
                                 (setf ev-received msg)))))
      (set-value item 1)
      (is-true (await-cond 0.5
                 ev-received))
      (is (eq (item-changed-event-item ev-received) item))
      (is (= 1 (hab::item-state-value (act-cell:state item)))))))
