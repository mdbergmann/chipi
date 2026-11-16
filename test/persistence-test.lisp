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

(test retrieve--file-table-roundtrip-and-restart
  (with-fixture init-destroy-env ()
    (let* ((cut (make-simple-persistence :table-files
                                        :storage-root-path #P"/tmp/chipi/table-files/"))
           (original (alexandria:plist-hash-table
                      '("Count" 12 "count" 2.5 "temperature" 29.08d0
                        "ratio" 1/3 "text" "ok" "yes" item:true "no" item:false)
                      :test #'equal))
           (item (item:make-item 'table-file :type-hint 'table :initial-value original))
           (value (item:item-state-value (item:get-item-stateq item))))
      (is-true (miscutils:await-cond 1 (uiop:directory-exists-p #P"/tmp/chipi/table-files/")))
      (persp:persist cut item)
      (let ((fetched (persp:retrieve cut item)))
        (is (equalp value (persp:persisted-item-value fetched)))
        (is (typep (gethash "Count" (persp:persisted-item-value fetched)) 'single-float)))
      (let* ((timestamp (item:item-state-timestamp (item:get-item-stateq item)))
             (path #P"/tmp/chipi/table-files/TABLE-FILE.store")
             (file-text (uiop:read-file-string path))
             (pushes 0))
        (item:destroy item)
        (setf item (item:make-item 'table-file :type-hint 'table))
        (item:add-binding item (binding:make-function-binding
                               :push (lambda (value) (declare (ignore value)) (incf pushes))))
        (item:add-persistence item cut :load-on-start t)
        (is-true (miscutils:await-cond 1
                   (equalp value (item:item-state-value (item:get-item-stateq item)))))
        (is (= timestamp (item:item-state-timestamp (item:get-item-stateq item))))
        (is (= 0 pushes))
        (is (string= file-text (uiop:read-file-string path))))
      (item:set-value item (make-hash-table :test #'equal) :persist nil)
      (is-true (miscutils:await-cond 1
                 (= 0 (hash-table-count (item:item-state-value (item:get-item-stateq item))))))
      (persp:persist cut item)
      (is (= 0 (hash-table-count (persp:persisted-item-value (persp:retrieve cut item))))))))

(test %fetch-persisted-value--equal-empty-file-table-is-no-op
  (with-fixture init-destroy-env ()
    (let ((cut (make-simple-persistence :empty-table-files
                                        :storage-root-path #P"/tmp/chipi/empty-table/"))
           (item (item:make-item 'empty-file :type-hint 'table))
           (events nil)
           (subscribed nil)
           (timestamp 3900000000)
           (initial-timestamp nil)
           (enqueued nil)
           (original-set-value (symbol-function 'item:set-value)))
      (setf initial-timestamp (item:item-state-timestamp (item:get-item-stateq item)))
      (is-true (miscutils:await-cond 1 (uiop:directory-exists-p #P"/tmp/chipi/empty-table/")))
      (with-open-file (stream #P"/tmp/chipi/empty-table/EMPTY-FILE.store"
                              :direction :output :if-exists :supersede)
        (com.inuoe.jzon:stringify
         (item-ext:item-state-to-ht
          (item:make-item-state :value (make-hash-table :test #'equal) :timestamp timestamp))
         :stream stream))
      (ac:actor-of (isys:ensure-isys)
                   :init (lambda (self)
                           (ev:subscribe self self 'item:item-changed-event)
                           (setf subscribed t))
                   :receive (lambda (msg)
                              (when (typep msg 'item:item-changed-event)
                                (push msg events))))
      (is-true (miscutils:await-cond 1 subscribed))
      (cl-mock:with-mocks ()
        (cl-mock:answer (item:set-value target value :push push :timestamp ts :persist persist)
          (prog1 (funcall original-set-value target value :push push :timestamp ts :persist persist)
            (setf enqueued t)))
        (item:add-persistence item cut :load-on-start t)
        (is-true (miscutils:await-cond 1 enqueued))
        (let ((barrier (item:get-value item)))
          (is-true (miscutils:await-cond 1 (hash-table-p (future:fresult barrier))))))
      (is (= initial-timestamp (item:item-state-timestamp (item:get-item-stateq item))))
      (is (= 0 (hash-table-count (item:item-state-value (item:get-item-stateq item)))))
      (sleep 0.1)
      (is-false events))))
