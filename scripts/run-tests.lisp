(require :asdf)
(let ((setup (or (uiop:getenv "CHIPI_LISP_SETUP")
                 (namestring (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))))
  (load setup))
;; A Qlot setup can register a searcher for its owning project. This checkout
;; must supply Chipi, even when its dependencies come from another Qlot tree.
(setf asdf:*system-definition-search-functions*
      (remove-if (lambda (fun)
                   (and (symbolp fun) (string= (symbol-name fun) "PROJECT-SYSTEM-SEARCHER")))
                 asdf:*system-definition-search-functions*))
(asdf:initialize-source-registry
 `(:source-registry (:directory ,(uiop:getenv "CHIPI_TEST_ROOT"))
   :ignore-inherited-configuration))
(ql:quickload "chipi/tests")
(assert (equal (truename (asdf:system-source-directory "chipi"))
               (truename (uiop:getenv "CHIPI_TEST_ROOT"))))
(log:config :warn)
(let* ((mode (uiop:getenv "CHIPI_TEST_SUITE"))
       (ok (if (string= mode "unit")
               (fiveam:run!
                (mapcar (lambda (pair) (uiop:find-symbol* (second pair) (first pair)))
                        '(("CHIPI.ITEM-TEST" "ITEM-TESTS")
                          ("CHIPI.ITEM-EXT-TEST" "ITEM-EXT-TESTS")
                          ("CHIPI.PERSISTENCE-TEST" "PERSISTENCE-TESTS")
                          ("CHIPI.HAB-TEST" "HAB-TESTS")
                          ("CHIPI.INFLUX-PERSISTENCE-UNIT-TEST" "INFLUX-PERSISTENCE-UNIT-TESTS"))))
               (fiveam:run! (if (string= mode "all")
                                'chipi.tests:test-suite
                                'chipi.influx-persistence-test::influx-persistence-tests)))))
  (when (member mode '("all" "unit") :test #'string=)
    (ql:quickload '("chipi-api/tests" "chipi-ui/tests"))
    (setf ok (and (uiop:symbol-call :fiveam :run! (uiop:find-symbol* "TEST-SUITE" "CHIPI-API.TESTS")) ok))
    (setf ok (and (uiop:symbol-call :fiveam :run! (uiop:find-symbol* "TEST-SUITE" "CHIPI-UI.TESTS")) ok)))
  (uiop:quit (if ok 0 1)))
