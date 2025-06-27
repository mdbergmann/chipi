(defpackage :chipi-api.sse-utils
  (:use :cl)
  (:nicknames :sse-utils)
  (:local-nicknames (#:jz #:com.inuoe.jzon))
  (:import-from #:alexandria
                #:plist-hash-table)
  (:export #:write-sse-message
           #:write-sse-data
           #:write-sse-heartbeat
           #:write-sse-connection
           #:stream-closed-error))

(in-package :chipi-api.sse-utils)

(define-condition stream-closed-error (simple-condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Stream is closed!"))))

(defun write-sse-message (stream message-type data &key id event retry)
  "Write a generic SSE message to stream with optional fields.
   Returns T on success, NIL on failure."
  (when (not (open-stream-p stream))
    (error 'stream-closed-error))
  (handler-case
      (let* ((message (format nil "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
                              id event retry data))
             (message-bytes (flexi-streams:string-to-octets
                             message :external-format :utf-8)))
        (write-sequence message-bytes stream)
        (force-output stream)
        t)
    (error (e)
      (log:warn "Error writing SSE message (~a): ~a" message-type e)
      nil)))

(defun write-sse-data (stream data-string)
  "Write SSE data message to stream. Returns T on success, NIL on failure."
  (write-sse-message stream "data" data-string))

(defun write-sse-heartbeat (stream timestamp)
  "Write SSE heartbeat message to stream. Returns T on success, NIL on failure."
  (let ((heartbeat-data (jz:stringify
                         (plist-hash-table
                          (list "type" "heartbeat"
                                "timestamp" timestamp)
                          :test #'equal))))
    (write-sse-message stream "heartbeat" heartbeat-data)))

(defun write-sse-connection (stream message)
  "Write SSE connection message to stream. Returns T on success, NIL on failure."
  (let ((connection-data (jz:stringify
                          (plist-hash-table
                           (list "type" "connection"
                                 "message" message)
                           :test #'equal))))
    (write-sse-message stream "connection" connection-data)))
