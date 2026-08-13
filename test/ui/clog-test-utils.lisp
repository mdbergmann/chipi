(defpackage :chipi-ui.test-utils
  (:use :cl)
  (:export #:+conn-id+
           #:*captured*
           #:with-captured-clog
           #:captured-js
           #:js~
           #:make-body
           #:make-item-ht
           #:invalidate-connection))

(in-package :chipi-ui.test-utils)

(defparameter +conn-id+ "chipi-ui-render-test-conn")

(defvar *captured* nil
  "List of JS command strings the stubbed CLOG connection received (reversed).")

(defmacro with-captured-clog (&body body)
  "Run BODY with clog-connection:execute/query stubbed so that every command
CLOG emits is pushed onto *CAPTURED* instead of going to a browser.  A
connection-data hash is registered for +CONN-ID+ so event binding
(set-on-click / set-on-change) takes the live path rather than no-opping, and
a stub connection is registered so `clog:validp' reports the connection as
alive."
  (let ((save-exec (gensym "EXEC")) (save-query (gensym "QUERY")))
    `(let ((*captured* nil)
           (,save-exec (fdefinition 'clog-connection:execute))
           (,save-query (fdefinition 'clog-connection:query)))
       (unwind-protect
            (progn
              (setf (fdefinition 'clog-connection:execute)
                    (lambda (connection-id message)
                      (declare (ignore connection-id))
                      (push message *captured*)
                      nil))
              (setf (fdefinition 'clog-connection:query)
                    (lambda (connection-id script &key default-answer)
                      (declare (ignore connection-id script))
                      default-answer))
              (setf (gethash +conn-id+ clog-connection::*connection-data*)
                    (make-hash-table :test #'equal))
              (setf (gethash +conn-id+ clog-connection::*connection-ids*) :stub)
              ,@body)
         (setf (fdefinition 'clog-connection:execute) ,save-exec)
         (setf (fdefinition 'clog-connection:query) ,save-query)
         (remhash +conn-id+ clog-connection::*connection-data*)
         (remhash +conn-id+ clog-connection::*connection-ids*)))))

(defun invalidate-connection ()
  "Makes the stub connection report as dead (`clog:validp' => nil)."
  (remhash +conn-id+ clog-connection::*connection-ids*))

(defun captured-js ()
  "All captured commands joined into one string for substring assertions."
  (format nil "~{~a~^~%~}" (reverse *captured*)))

(defun js~ (substr)
  "True if SUBSTR appears anywhere in the captured JS."
  (and (search substr (captured-js)) t))

(defun make-body ()
  (clog::make-clog-body +conn-id+))

(defun make-item-ht (&key (label "Temperature") (name "sensor.temp")
                          (type-hint "FLOAT") (value 23.5) (timestamp 0) tags)
  "A mock item hash-table shaped like item-ext:item-to-ht output."
  (let ((item  (make-hash-table :test #'equal))
        (state (make-hash-table :test #'equal)))
    (setf (gethash "value" state)     value
          (gethash "timestamp" state) timestamp)
    (setf (gethash "label" item)      label
          (gethash "name" item)       name
          (gethash "type-hint" item)  type-hint
          (gethash "item-state" item) state
          (gethash "tags" item)       tags)
    item))
