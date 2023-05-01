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
                                   :stragety :round-robin)))))

(defun shutdown-isys ()
  (when *isys*
    (ac:shutdown *isys* :wait t)
    (setf *isys* nil)
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
                :receive (lambda (msg)
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
                 :documentation "The function that retrieves the value.")
   (onbind :initarg :onbind
           :initform t
           :documentation "Flag indicating that `retrieve-fun' should be called and the item state updated immediately after binding to the item.")))

(defmethod print-object ((obj binding) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (format stream "binding")
      (get-output-stream-string string-stream))))

(defun make-function-binding (&key retrieve (onbind t))
  (make-instance 'binding
                 :retrieve-fun retrieve
                 :onbind onbind))

(defun bind-item (binding item)
  (with-slots (bound-items retrieve-fun onbind) binding
    (setf bound-items (cons item bound-items))
    (when onbind
      (tasks:with-context (*isys* :tasks)
        (tasks:task-async retrieve-fun
                          :on-complete-fun (lambda (result)
                                             (set-value item result)))))))
  
