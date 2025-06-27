(defpackage :chipi-api.events-controller
  (:use :cl)
  (:nicknames :eventsc)
  (:local-nicknames (#:jz #:com.inuoe.jzon))
  (:import-from #:chipi-api.sse-manager
                #:add-client
                #:remove-client)
  (:import-from #:chipi-api.sse-utils
                #:write-sse-connection
                #:write-sse-heartbeat)
  (:import-from #:alexandria
                #:plist-hash-table)
  (:export #:handle-sse-connection
           #:*heartbeat-sleep-time-s*
           #:*max-heartbeats*))

(in-package :chipi-api.events-controller)

(defparameter *heartbeat-sleep-time-s* 30
  "Sleep time in seconds between heartbeat messages")

(defparameter *max-heartbeats* nil
  "Maximum number of heartbeats to send (nil = unlimited, used for testing)")

(defun handle-sse-connection (stream)
  "Handle SSE connection for a given stream (server-agnostic)"
  (log:info "Handle sse-connection...")

  ;; Send initial connection message
  (write-sse-connection stream "Connected to item events")
  
  ;; Add client to SSE manager
  (let ((client-id)
        (client-id-fut (add-client stream)))
    (future:fcompleted client-id-fut
        (cid)
      (log:info "SSE client ~a connected" cid)
      (setf client-id cid))
    
    ;; Keep connection alive
    (handler-case
        (let ((heartbeat-count 0))
          (loop
            (when (and *max-heartbeats* (>= heartbeat-count *max-heartbeats*))
              (return))
            (sleep *heartbeat-sleep-time-s*) ; Send heartbeat based on dynamic variable
            (log:info "Sending heartbeat...")
            (write-sse-heartbeat stream (get-universal-time))
            (when *max-heartbeats*
              (incf heartbeat-count))))
      (condition (c)
        (log:info "SSE client ~a disconnected: ~a" client-id c)
        (remove-client client-id)))))
