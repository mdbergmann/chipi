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
           #:binding
           ;; events
           #:item-changed-event
           #:item-changed-event-item
           ;; binding
           #:make-function-binding
           #:bind-item)
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

;; -------------------------
;; bindings
;; -------------------------

;; bindings should be able to attach to multiple items

(defclass binding ()
  ((bound-items :initform '()
                :reader bound-items
                :documentation "The bound items. On operation the value will be updated to each item.")
   (retrieve-fun :initarg :retrieve-fun
                 :initform (error "Must be set!")
                 :type function
                 :documentation "The function that retrieves the value.")
   (initial-delay :initarg :initial-delay
                  :initform nil
                  :documentation "Initial delay in seconds where `RETRIEVE-FUN' is executed. `NIL' means disabled.")
   (delay :initarg :delay
          :initform nil
          :documentation "Recurring delay. Calls `RETRIEVE-FUN' repeatedly. `NIL' means disabled.")))

(defmethod print-object ((obj binding) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (with-slots (initial-delay delay) obj
        (format stream "initial-delay: ~a, delay: ~a" initial-delay delay))
      (get-output-stream-string string-stream))))

(defun make-function-binding (&key retrieve (initial-delay nil) (delay nil))
  (make-instance 'binding
                 :retrieve-fun retrieve
                 :initial-delay initial-delay
                 :delay delay))

(defun bind-item (binding item)
  (with-slots (bound-items retrieve-fun initial-delay delay delay-thread) binding
    (setf bound-items (cons item bound-items))
    (let ((timer-fun (lambda ()
                       ;; maybe execute with using tasks
                       (let ((result (funcall retrieve-fun)))
                         (set-value item result)))))
      (when initial-delay
        (sched:schedule initial-delay timer-fun))
      
      (when delay
        (let (recurring-timer-fun)
          (setf recurring-timer-fun
                (lambda ()
                  (funcall timer-fun)
                  (wt:schedule *timer* delay recurring-timer-fun)))
          (sched:schedule delay recurring-timer-fun))))))
