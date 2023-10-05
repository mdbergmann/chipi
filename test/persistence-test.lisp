(defpackage :cl-hab.persistence-test
  (:use :cl :fiveam :cl-hab.persistence :cl-hab.simple-persistence)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-hab.persistence-test)

(def-suite persistence-tests
  :description "Persistence tests for simple persistence."
  :in cl-hab.tests:test-suite)

(in-suite persistence-tests)

(def-fixture init-destroy-env ()
  (unwind-protect
       (progn 
         (&body))
    (progn
      (envi:shutdown-env)
      (uiop:delete-directory-tree
       (uiop:ensure-directory-pathname #P"/tmp/cl-hab")
       :validate t
       :if-does-not-exist :ignore))))

(test make-persistence--simple
  "Make a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/cl-hab/persistence-test")))
      (print cut)
      (is-true (miscutils:await-cond 0.5
                 (uiop:directory-exists-p #P"/tmp/cl-hab/persistence-test")))
      (is-true cut)
      (is (typep cut 'simple-persistence)))))

(test simple-persistence--store-and-fetch
  "Store a value in a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/cl-hab/persistence-test"))
          (item (item:make-item 'foo 'string)))
      (item:set-value item "foobar")
      (persp:store cut item)
      (is-true (miscutils:await-cond 0.5
                 (uiop:file-exists-p #P"/tmp/cl-hab/persistence-test/FOO.store")))
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 0.5
                   (let ((resolved (future:fresult fetched)))
                     (and (not (equal resolved :not-ready))
                          (equal (persisted-item-value resolved) "foobar")))))))))

(test simple-persistence--error-on-fetch--file-not-found
  "Fetch a non-existent file from a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/cl-hab/persistence-test"))
          (item (item:make-item 'foo 'string)))
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 2.0
                   (let ((resolved (future:fresult fetched)))
                     (and (not (eq resolved :not-ready))
                          (consp resolved)
                          (equal (car resolved) :error)))))))))
