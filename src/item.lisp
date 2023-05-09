(defpackage :cl-eta.item
  (:use :cl)
  (:nicknames :item)
  (:import-from #:hab
                #:ensure-isys)
  (:import-from #:act
                #:*self*
                #:*state*
                #:!
                #:?)
  (:export #:make-item
           #:item
           #:get-value
           #:set-value
           ;; events
           #:item-changed-event
           #:item-changed-event-item))

(in-package :cl-eta.item)

(defstruct item-changed-event item)

(defclass item (act:actor) ())
(defstruct item-state
  (value t))

(defun make-item (id)
  (let* ((isys (ensure-isys))
         (item (ac:actor-of
                isys
                :name (symbol-name id)
                :type 'item
                :state (make-item-state)
                :receive (lambda (msg)
                           (log:debug "Received msg: " msg)
                           (case (car msg)
                             (:get-state
                              (act:reply (slot-value *state* 'value)))
                             (:set-state
                              (prog1
                                  (setf (slot-value *state* 'value) (cdr msg))
                                (let ((self *self*))
                                  (ev:publish self (make-item-changed-event
                                                    :item self))))))))))
    item))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (format stream "name: ~a, value: ~a"
              (act-cell:name obj)
              (act-cell:state obj))
      (get-output-stream-string string-stream))))

(defun get-value (item)
  (? item '(:get-state . nil)))

(defun set-value (item value)
  (! item `(:set-state . ,value)))
