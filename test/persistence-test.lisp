(defpackage :chipi.persistence-test
  (:use :cl :fiveam :chipi.persistence :chipi.simple-persistence)
  (:export #:run!
           #:all-tests
           #:nil
           #:assert-fetch-error))
(in-package :chipi.persistence-test)

(def-suite persistence-tests
  :description "Persistence tests for simple persistence."
  :in chipi.tests:test-suite)

(in-suite persistence-tests)

(defun cleanup-tmp-dir ()
  (uiop:delete-directory-tree
   (uiop:ensure-directory-pathname #P"/tmp/chipi")
   :validate t
   :if-does-not-exist :ignore))

(def-fixture init-destroy-env ()
  (cleanup-tmp-dir)
  (unwind-protect
       (progn 
         (&body))
    (progn
      (envi:shutdown-env)
      (cleanup-tmp-dir))))

(defmacro assert-fetch-error (fetched)
  `(is-true (miscutils:await-cond 2.0
              (let ((resolved (future:fresult ,fetched)))
                (format t "resolved: ~a~%" resolved)
                (and (not (eq resolved :not-ready))
                     (consp resolved)
                     (equal (car resolved) :error))))))

(test make-persistence--simple
  "Make a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/chipi/persistence-test")))
      (print cut)
      (is-true (miscutils:await-cond 0.5
                 (uiop:directory-exists-p #P"/tmp/chipi/persistence-test")))
      (is-true cut)
      (is (typep cut 'simple-persistence)))))

(test simple-persistence--store-and-fetch
  "Store a value in a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/chipi/persistence-test"))
          (item (item:make-item 'foo)))
      (item:set-value item "foobar")
      (persp:store cut item)
      (is-true (miscutils:await-cond 0.5
                 (uiop:file-exists-p #P"/tmp/chipi/persistence-test/FOO.store")))
      (let ((fetched (persp:fetch cut item)))
        (is-true (miscutils:await-cond 0.5
                   (let ((resolved (future:fresult fetched)))
                     (and (not (eq resolved :not-ready))
                          (equal (persisted-item-value resolved) "foobar")))))))))

(test simple-persistence--error-on-use-of-range
  "Simple persistence does'nt implement retrieving history."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/chipi/persistence-test"))
          (item (item:make-item 'foo)))
      (let ((fetched (persp:fetch cut item (make-relative-range :seconds 1))))
        (assert-fetch-error fetched)))))

(test simple-persistence--error-on-fetch--file-not-found
  "Fetch a non-existent file from a `simple` persistence."
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :persp-map
                                        :storage-root-path #P"/tmp/chipi/persistence-test"))
          (item (item:make-item 'foo)))
      (let ((fetched (persp:fetch cut item)))
        (assert-fetch-error fetched)))))
