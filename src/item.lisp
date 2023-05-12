(defpackage :cl-eta.item
  (:use :cl)
  (:nicknames :item)
  (:import-from #:envi
                #:ensure-isys)
  (:import-from #:act
                #:*self*
                #:*state*
                #:reply
                #:!
                #:?)
  (:export #:make-item
           #:item
           #:get-value
           #:set-value
           #:add-binding
           ;; events
           #:item-changed-event
           #:item-changed-event-item))

(in-package :cl-eta.item)

(defstruct item-changed-event item)

(defclass item (act:actor)
  ((label :initform nil
          :reader label
          :documentation "An explanatory label.")
   (bindings :initform '()
             :reader bindings
             :documentation "The items bindings")))
(defstruct item-state
  (value t))

(defun make-item (id &optional (label nil))
  (let* ((isys (ensure-isys))
         (item (ac:actor-of
                isys
                :name (symbol-name id)
                :type 'item
                :state (make-item-state)
                :receive (lambda (msg)
                           (log:debug "Received msg: " msg)
                           (let ((new-value (cdr msg))
                                 (self *self*))
                             (flet ((apply-new-value ()
                                      (prog1
                                          (setf (item-state-value *state*) new-value)
                                        (ev:publish self (make-item-changed-event
                                                          :item self))))
                                    (push-to-bindings ()
                                      (with-slots (bindings) self
                                        (log:debug "Processing ~a binding(s)." (length bindings))
                                        (dolist (binding bindings)
                                          (when (binding:pull-passthrough binding)
                                            (binding:exec-push binding new-value))))))
                               (case (car msg)
                                 (:get-state
                                  (reply (item-state-value *state*)))
                                 (:set-state
                                  (progn
                                    (log:debug "set-state: ~a" new-value)
                                    (apply-new-value)
                                    (push-to-bindings))))))))))
    (setf (slot-value item 'label) label)
    item))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (format stream "name: ~a, label: ~a, value: ~a"
              (act-cell:name obj)
              (label obj)
              (act-cell:state obj))
      (get-output-stream-string string-stream))))

(defun get-value (item)
  (? item '(:get-state . nil)))

(defun set-value (item value)
  "Updates item value with push to bindings."
  (! item `(:set-state . ,value)))

(defun add-binding (item binding)
  (with-slots (bindings) item
    (setf bindings (cons binding bindings))
    (binding:bind-item binding item))
  item)
