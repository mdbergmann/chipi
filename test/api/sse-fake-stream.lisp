(defpackage :chipi-api.sse-fake-stream
  (:use :cl)
  (:nicknames :sse-fake-stream)
  (:export #:make-test-stream
           #:get-test-stream-output
           #:test-stream-closed-p
           #:close-test-stream
           #:clear-test-stream-output
           #:setup-fake-streams))

(in-package :chipi-api.sse-fake-stream)

;; Use regular string streams for testing but track them
(defvar *test-streams* (make-hash-table :test 'eq))

(defun setup-fake-streams ()
  "Initialize/reset the fake streams hashtable for testing"
  (setf *test-streams* (make-hash-table :test 'eq)))

(defun make-test-stream ()
  "Create a test stream that works with write-sequence of octets (like Hunchentoot chunked streams)"
  (let* ((binary-stream (flexi-streams:make-in-memory-output-stream))
         (flexi-stream (flexi-streams:make-flexi-stream
                        binary-stream
                        :external-format :utf-8
                        :element-type '(unsigned-byte 8))))
    (setf (gethash flexi-stream *test-streams*)
          (list :closed nil :output "" :binary-stream binary-stream))
    flexi-stream))

(defun get-test-stream-output (stream)
  "Get output from test stream as string"
  (let ((binary-stream (getf (gethash stream *test-streams*) :binary-stream)))
    (if binary-stream
        (flexi-streams:octets-to-string
         (flexi-streams:get-output-stream-sequence binary-stream)
         :external-format :utf-8)
        "")))

(defun test-stream-closed-p (stream)
  "Check if test stream is marked as closed"
  (getf (gethash stream *test-streams*) :closed))

(defun close-test-stream (stream)
  "Mark test stream as closed"
  (when (gethash stream *test-streams*)
    (setf (getf (gethash stream *test-streams*) :closed) t)))

(defun clear-test-stream-output (stream)
  "Reset test stream"
  (when (gethash stream *test-streams*)
    (close stream)
    (let ((new-stream (make-string-output-stream)))
      (setf (gethash new-stream *test-streams*)
            (gethash stream *test-streams*))
      (remhash stream *test-streams*)
      new-stream)))

;; Override stream methods for testing
(defmethod open-stream-p :around (stream)
  (if (gethash stream *test-streams*)
      (not (test-stream-closed-p stream))
      (call-next-method)))
