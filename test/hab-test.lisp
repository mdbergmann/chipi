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
                                     :retrieve (lambda () 123)
                                     :onbind t))))
      (is-true (binding item))
      (sleep 0.1)  ;; otherwise we'll get the initial value
      (let ((item-value (get-value item)))
        (is-true (await-cond 2
                   (eq (future:fresult item-value) 123)))))))

;; add binding test for initial delay, delay (if recurring)
;; add item test for raising event about changed item.

(run! 'hab-tests)
