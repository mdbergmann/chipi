(defpackage :chipi-api.sse-manager
  (:use :cl)
  (:nicknames :sse-manager)
  (:import-from #:ac
                #:actor-of)
  (:import-from #:act
                #:!
                #:?
                #:reply
                #:*state*
                #:*self*)
  (:export #:make-sse-manager
           #:ensure-sse-manager
           #:add-client
           #:remove-client))

(in-package :chipi-api.sse-manager)

(defstruct sse-client
  stream
  api-key
  access-rights)

(defstruct sse-manager-state
  (clients (make-hash-table :test 'equal))
  (client-counter 0))

(defun make-sse-manager ()
  "Create a new SSE manager actor"
  (ac:actor-of (isys:ensure-isys)
               :name "SSE manager"
               :init (lambda (self)
                       (log:info "Starting SSE manager")
                       (ev:subscribe self self 'item:item-changed-event))
               :state (make-sse-manager-state)
               :receive #'sse-manager-receive))

(defun sse-manager-receive (msg)
  "Handle messages for SSE manager"
  (cond
    ((typep msg 'item:item-changed-event)
     (%handle-item-changed msg))
    ((listp msg)
     (case (car msg)
       (:add-client
        (%handle-add-client (getf (cdr msg) :stream)
                            (getf (cdr msg) :api-key)
                            (getf (cdr msg) :access-rights)))
       (:remove-client
        (%handle-remove-client (getf (cdr msg) :client-id)))))))

(defun %handle-item-changed (event)
  "Handle item change events and broadcast to connected clients"
  (let ((item (item:item-changed-event-item event)))
    (when item
      (let ((item-data (%serialize-item-for-sse item)))
        (%broadcast-to-clients item-data)))))

(defun %handle-add-client (stream api-key access-rights)
  "Add a new SSE client connection"
  (let* ((client-id (format nil "client-~a" 
                            (incf (sse-manager-state-client-counter *state*))))
         (client (make-sse-client :stream stream
                                  :api-key api-key
                                  :access-rights access-rights)))
    (setf (gethash client-id (sse-manager-state-clients *state*)) client)
    (log:info "Added SSE client: ~a with access rights: ~a" client-id access-rights)
    (reply client-id)))

(defun %handle-remove-client (client-id)
  "Remove an SSE client connection"
  (when (remhash client-id (sse-manager-state-clients *state*))
    (log:info "Removed SSE client: ~a" client-id)))

(defun %serialize-item-for-sse (item)
  "Convert item to JSON format for SSE transmission using item-ext"
  (let ((item-ht (item-ext:item-to-ht item)))
    (com.inuoe.jzon:stringify
     (alexandria:plist-hash-table
      (list "type" "item-change"
            "data" item-ht)
      :test #'equal))))

(defun %broadcast-to-clients (data)
  "Broadcast data to all connected SSE clients with appropriate access rights"
  (let ((clients-to-remove '()))
    (maphash (lambda (client-id client)
               (when (member :read (sse-client-access-rights client))
                 (if (%send-sse-data (sse-client-stream client) data)
                     (log:debug "Sent SSE data to client: ~a" client-id)
                     (progn
                       (log:info "Failed to send to client ~a, marking for removal" client-id)
                       (push client-id clients-to-remove)))))
             (sse-manager-state-clients *state*))
    ;; Remove dead clients
    (dolist (client-id clients-to-remove)
      (%handle-remove-client client-id))))

(defun %send-sse-data (stream data)
  "Send SSE formatted data to stream, return T on success, NIL on failure"
  (handler-case
      (progn
        (format stream "data: ~a~%~%" data)
        (force-output stream)
        t)
    (error (e)
      (log:warn "Error sending SSE data: ~a" e)
      nil)))


;; Global SSE manager instance
(defparameter *sse-manager* nil)

(defun ensure-sse-manager ()
  "Ensure SSE manager is running"
  (unless (and *sse-manager* (act-cell:running-p *sse-manager*))
    (setf *sse-manager* (make-sse-manager)))
  *sse-manager*)

(defun add-client (stream api-key access-rights)
  "Add a client to the SSE manager"
  (? (ensure-sse-manager) `(:add-client :stream ,stream 
                                        :api-key ,api-key 
                                        :access-rights ,access-rights)))

(defun remove-client (client-id)
  "Remove a client from the SSE manager"
  (when *sse-manager*
    (! *sse-manager* `(:remove-client :client-id ,client-id))))
