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
           #:add-persistence
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
             :documentation "The items bindings")
   (persistences :initform '()
                 :reader persistences
                 :documentation "The items persistences")))
(defstruct item-state
  (value t))

(defstruct item-persistence
  (persp nil :type (or null persp:persistence))
  (frequency :every-change)
  (load-on-start t))

(defun %filter-frequency (list freq)
  (remove-if-not
   (lambda (p)
     (eq (item-persistence-frequency p) freq))
   list))

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
                                              (binding:exec-push binding new-value))))))
                                    (apply-persistences ()
                                      (with-slots (persistences) self
                                        (dolist (persp (%filter-frequency persistences :every-change))
                                          (persp:store
                                           (item-persistence-persp persp)
                                           self)))))
                               (case (car msg)
                                 (:get-state
                                  (reply (item-state-value *state*)))
                                 (:set-state
                                  (let ((val (cadr msg))
                                        (push (cddr msg)))
                                    (log:debug "set-state: ~a" val)
                                    (apply-new-value val)
                                    (push-to-bindings val push)
                                    (apply-persistences)))))))
                :destroy (lambda (self)
                           (with-slots (bindings persistences) self
                             (dolist (binding bindings)
                               (ignore-errors
                                (binding:destroy binding))))))))
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
    (push binding bindings)
    (binding:bind-item binding item))
  item)

(defun add-persistence (item persistence &rest other-args)
  ;; unwrap a list in list
  (when (listp (car other-args))
    (setf other-args (car other-args)))
  (with-slots (persistences) item
    (let ((item-persp (make-item-persistence
                       :persp persistence
                       :frequency (getf other-args :frequency)
                       :load-on-start (getf other-args :load-on-start))))
      (push item-persp persistences)
      (when (item-persistence-load-on-start item-persp)
        (log:debug "Loading item value from persp: ~a" persistence)
        (future:fcompleted (persp:fetch persistence item)
            (result)
          (set-value item result :push nil))))))

(defun destroy (item)
  (ac:stop (act:context item) item :wait t))
