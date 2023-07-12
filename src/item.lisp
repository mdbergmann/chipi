(defpackage :cl-hab.item
  (:use :cl)
  (:nicknames :item)
  (:import-from #:isys
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
           #:destroy
           ;; events
           #:item-changed-event
           #:item-changed-event-item))

(in-package :cl-hab.item)

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
                           (let ((self *self*))
                             (flet ((apply-new-value (new-value)
                                      (prog1
                                          (setf (item-state-value *state*) new-value)
                                        (ev:publish self (make-item-changed-event
                                                          :item self))))
                                    (push-to-bindings (new-value push) 
                                      (with-slots (bindings) self
                                        (log:debug "Processing ~a binding(s)." (length bindings))
                                        (dolist (binding bindings)
                                          (let ((eff-push (if push 
                                                              (binding:pull-passthrough binding)
                                                              nil)))
                                            (when eff-push
                                              (binding:exec-push binding new-value)))))))
                               (case (car msg)
                                 (:get-state
                                  (reply (item-state-value *state*)))
                                 (:set-state
                                  (let ((val (cadr msg))
                                        (push (cddr msg)))
                                    (log:debug "set-state: ~a" val)
                                    (apply-new-value val)
                                    (push-to-bindings val push)))))))
                :destroy (lambda (self)
                           (with-slots (bindings) self
                             (dolist (binding bindings)
                               (binding:destroy binding)))))))
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

(defun set-value (item value &key (push t))
  "Updates item value with push to bindings.
If PUSH is non-nil, bindings will be pushed regardsless of :pull-passthrough."
  (! item `(:set-state . (,value . ,push))))

(defun add-binding (item binding)
  (with-slots (bindings) item
    (setf bindings (cons binding bindings))
    (binding:bind-item binding item))
  item)

(defun destroy (item)
  (ac:stop (act:context item) item :wait t))
