(defpackage :cl-eta.hab
  (:use :cl)
  (:nicknames :hab)
  (:import-from #:act
                #:*self*
                #:*state*
                #:!
                #:?)
  (:export #:shutdown-isys
           #:shutdown-timer
           ;; item
           #:make-item
           #:item
           #:get-value
           #:set-value
           ;; events
           #:item-changed-event
           #:item-changed-event-item)
  )

(in-package :cl-eta.hab)

(defvar *isys* nil)
(defvar *timer* nil)

(defun ensure-isys ()
  (or *isys*
      (prog1
          (setf *isys* (asys:make-actor-system))
        ;; separate dispatcher for tasks
        (asys:register-dispatcher *isys*
                                  (disp:make-dispatcher
                                   *isys*
                                   :tasks
                                   :workers 4
                                   :stragety :round-robin))
        (setf tasks:*task-context* *isys*
              tasks:*task-dispatcher* :tasks))))

(defun shutdown-isys ()
  (when *isys*
    (ac:shutdown *isys* :wait t)
    (setf *isys* nil
          tasks:*task-context* nil
          tasks:*task-dispatcher* nil)
    t))

(defun ensure-timer ()
  (or *timer*
      (setf *timer* (wt:make-wheel-timer :max-size 300 :resolution 100))))

(defun shutdown-timer ()
  (when *timer*
    (wt:shutdown-wheel-timer *timer*)
    (setf *timer* nil))
  t)

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
                                (ev:publish *self* (make-item-changed-event
                                                    :item *self*)))))))))
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
