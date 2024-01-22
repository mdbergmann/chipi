(defpackage :chipi-web.items-controller-test
  (:use :cl :fiveam :cl-mock :chipi-web.items-controller)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi-web.items-controller-test)

(def-suite items-controller-tests
  :description "Test for items controller"
  :in chipi-web.tests:test-suite)

(in-suite items-controller-tests)

(def-fixture with-isys ()
  (unwind-protect
       (progn
         ;; setup a partial environment
         (let ((hab:*items* (make-hash-table)))
           (isys:ensure-isys)
           (&body)))
    (isys:shutdown-isys)))

(test retrieve-items--empty
  (with-mocks ()
    (answer hab:get-items '())
    (is (= (length (retrieve-items)) 0))
    (is (= (length (invocations 'hab:get-items)) 1))))

(test retrieve-items--non-empty
  (with-fixture with-isys ()
    (with-mocks ()
      (answer hab:get-items
        `(,(hab:defitem 'foo1 "foo1-label" nil :initial-value 1)
          ,(hab:defitem 'foo2 "foo2-label" nil :initial-value 2)))
      (let ((items (retrieve-items)))
        (is (= (length items) 2))
        (is (= (length (invocations 'hab:get-items)) 1))
        (is (equalp '((:name "foo1" :label "foo1-label" :value 1)
                      (:name "foo2" :label "foo2-label" :value 2))
                    items))
        ))))
