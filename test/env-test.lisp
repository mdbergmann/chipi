(defpackage :chipi.env-test
  (:use :cl :fiveam :chipi.env)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :chipi.env-test)

(def-suite env-tests
  :description "Tests for envi"
  :in chipi.tests:test-suite)

(in-suite env-tests)

(test ensure-runtime-dir-creates-folder--default
  (let ((dir (ensure-runtime-dir)))
    (format t "~a~%" dir)
    (is (str:ends-with-p "runtime/" (namestring dir)))
    (is (uiop:directory-exists-p dir))
    (uiop:delete-directory-tree dir :validate t)))

(test ensure-runtime-dir-creates-folder--custom
  (let* ((envi::*rel-runtime-dir* "test-dir/")
         (dir (ensure-runtime-dir)))
    (format t "~a~%" dir)
    (is (str:ends-with-p "test-dir/" (namestring dir)))
    (is (uiop:directory-exists-p dir))
    (uiop:delete-directory-tree dir :validate t)))

(test ensure-runtime-dir-creates-folder--custom--with-subfolder
  (let* ((envi::*rel-runtime-dir* "test-dir/")
         (root (ensure-runtime-dir))
         (dir (ensure-runtime-dir "subfolder/")))
    (format t "~a~%" dir)
    (is (str:ends-with-p "test-dir/subfolder/" (namestring dir)))
    (is (uiop:directory-exists-p dir))
    (uiop:delete-directory-tree root :validate t)))
