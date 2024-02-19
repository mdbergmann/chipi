(defpackage :knx-conn.test-all
  (:use :cl :try))
(in-package :knx-conn.test-all)

(deftest test-all ()
  (knx-conn.knx-connect-test::suite)
  )

(defun test ()
  (try 'test-all))
