(defpackage :cl-eta.hab
  (:use :cl :sento-user)
  (:nicknames :hab)
  (:export #:shutdown-isys
           ;; item
           #:make-item
           #:item
           #:get-value
           #:set-value
           #:binding
           ;; binding
           #:make-function-binding)
  )

(in-package :cl-eta.hab)

(defvar *isys* nil)

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

(defclass item (act:actor)
  ((binding :initform nil
            :reader binding
            :documentation "The binding of the item. This is where the item value comes from.")))

(defstruct item-state
  (value t))

(defun make-item (id &key (binding nil))
  (let* ((isys (ensure-isys))
         (item (ac:actor-of
                isys
                :name (symbol-name id)
                :type 'item
                :state (make-item-state)
                :destroy (lambda (self)
                           (when (binding self)
                             (with-slots (delay-thread) (binding self)
                               (when delay-thread
                                 (bt:destroy-thread delay-thread)))))
                :receive (lambda (msg)
                           (log:debug "Received msg: " msg)
                           (case (car msg)
                             (:get-state
                              (act:reply (slot-value act:*state* 'value)))
                             (:set-state
                              (setf (slot-value act:*state* 'value) (cdr msg))))))))
    (when binding
      (setf (slot-value item 'binding) binding)
      (bind-item binding item))
    item))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (format stream "name: ~a, binding: ~a"
              (act-cell:name obj)
              (binding obj))
      (get-output-stream-string string-stream))))

(defun get-value (item)
  (act:? item '(:get-state . nil)))

(defun set-value (item value)
  (act:! item `(:set-state . ,value)))

;; -------------------------
;; bindings
;; -------------------------

;; bindings should be able to attach to multiple items

(defclass binding ()
  ((bound-items :initform '()
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
          :documentation "Recurring delay. Calls `RETRIEVE-FUN' repeatedly. `NIL' or `0' means disabled.")
   (delay-thread :initform nil)))

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
    (when initial-delay
      (make-scheduler-thread (lambda ()
                               (sleep initial-delay)
                               (let ((result (funcall retrieve-fun)))
                                 (set-value item result)))))
    (when (and delay (> delay 0))
      (setf delay-thread
            (make-scheduler-thread (lambda ()
                                     (loop
                                       (sleep delay)
                                       (let ((result (funcall retrieve-fun)))
                                         (set-value item result)))))))))

(defun make-scheduler-thread (fun)
  ;; todo: replace with scheduler
  (bt:make-thread fun
                  :name "Binding: scheduler thread"))
