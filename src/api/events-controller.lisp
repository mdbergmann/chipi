(defpackage :chipi-api.events-controller
  (:use :cl)
  (:nicknames :eventsc)
  (:local-nicknames (#:jz #:com.inuoe.jzon))
  (:import-from #:chipi-api.sse-manager
                #:add-client
                #:remove-client)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:export #:handle-sse-connection
           #:*heartbeat-sleep-time-s*
           #:*max-heartbeats*))

(in-package :chipi-api.events-controller)

(defvar *heartbeat-sleep-time-s* 30
  "Sleep time in seconds between heartbeat messages")

(defvar *max-heartbeats* nil
  "Maximum number of heartbeats to send (nil = unlimited, used for testing)")

(defun handle-sse-connection (stream)
  "Handle SSE connection for a given stream (server-agnostic)"
  
  ;; Send initial connection message
  (format stream "data: ~a~%~%"
          (jz:stringify
           (plist-hash-table
            (list "type" "connection"
                  "message" "Connected to item events")
            :test #'equal)))
  (force-output stream)
  
  ;; Add client to SSE manager
  (let ((client-id (add-client stream)))
    (log:info "SSE client ~a connected" client-id)
    
    ;; Keep connection alive
    (handler-case
        (let ((heartbeat-count 0))
          (loop
            (when (and *max-heartbeats* (>= heartbeat-count *max-heartbeats*))
              (return))
            (sleep *heartbeat-sleep-time-s*) ; Send heartbeat based on dynamic variable
            (format stream "data: ~a~%~%"
                    (jz:stringify
                     (plist-hash-table
                      (list "type" "heartbeat"
                            "timestamp" (get-universal-time))
                      :test #'equal)))
            (force-output stream)
            (incf heartbeat-count)))
      (condition (c)
        (log:info "SSE client ~a disconnected: ~a" client-id c)
        (remove-client client-id)))))
