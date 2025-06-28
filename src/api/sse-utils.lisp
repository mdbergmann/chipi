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

(defun write-sse-message (stream data &key id event retry)
  "Write a JSON message to stream without SSE formatting.
   Returns T on success, NIL on failure."
  (declare (ignore id event retry))
  (when (not (open-stream-p stream))
    (error 'stream-closed-error))
  (let* ((parsed-data (jz:parse data))
         (json-wrapper (jz:stringify
                        (plist-hash-table
                         (list "data" parsed-data)
                         :test #'equal)))
         (message-bytes (flexi-streams:string-to-octets
                         (concatenate 'string json-wrapper (string #\Newline))
                         :external-format :utf-8)))
    (write-sequence message-bytes stream)
    (force-output stream)
    t))

(defun write-sse-data (stream data-string)
  "Write SSE data message to stream. Returns T on success, NIL on failure."
  (write-sse-message stream data-string))

(defun write-sse-heartbeat (stream timestamp)
  "Write SSE heartbeat message to stream. Returns T on success, NIL on failure."
  (let ((heartbeat-data (jz:stringify
                         (plist-hash-table
                          (list "type" "heartbeat"
                                "timestamp" timestamp)
                          :test #'equal))))
    (write-sse-message stream heartbeat-data)))

(defun write-sse-connection (stream message)
  "Write SSE connection message to stream. Returns T on success, NIL on failure."
  (let ((connection-data (jz:stringify
                          (plist-hash-table
                           (list "type" "connection"
                                 "message" message)
                           :test #'equal))))
    (write-sse-message stream connection-data)))
