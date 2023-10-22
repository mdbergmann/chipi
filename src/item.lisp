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
           #:value-type-hint
           #:get-value
           #:set-value
           #:get-item-stateq
           #:add-binding
           #:add-persistence
           #:destroy
           #:item-state
           #:item-state-value
           #:item-state-timestamp
           ;; boolean value types
           #:true
           #:false
           ;; events
           #:item-changed-event
           #:item-changed-event-item))

(in-package :cl-hab.item)

(defstruct item-changed-event item)

(defclass item (act:actor)
  ((label :initform nil
          :reader label
          :documentation "An explanatory label.")
   (value-type-hint :initform nil
                    :reader value-type-hint
                    :documentation "A type hint for the item value.
This is in particular important for persistences that are type specific, like influxdb (within a measurement).")
   (bindings :initform '()
             :reader bindings
             :documentation "The items bindings")
   (persistences :initform '()
                 :reader persistences
                 :documentation "The items persistences")))
(defstruct item-state
  (value t)
  (timestamp (get-universal-time)))

(deftype true ())
(deftype false ())

(defstruct item-persistence
  (persp nil :type (or null persp:persistence))
  (frequency :every-change)
  (load-on-start t))

(defun %filter-frequency (list freq)
  (remove-if-not
   (lambda (p)
     (eq (item-persistence-frequency p) freq))
   list))

(defun make-item (id &key (label nil) (type-hint nil))
  (log:info "Creating item: ~a, label: ~a, type-hint: ~a" id label type-hint)
  (let* ((isys (ensure-isys))
         (item (ac:actor-of
                isys
                :name (symbol-name id)
                :type 'item
                :state (make-item-state)
                :receive (lambda (msg)
                           (log:debug "Received msg: ~a, item: ~a" msg id)
                           (let ((self *self*))
                             (flet ((apply-new-value (new-value timestamp)
                                      (let ((timestamp (or timestamp (get-universal-time))))
                                        (prog1
                                            (setf (item-state-value *state*) new-value
                                                  (item-state-timestamp *state*) timestamp)
                                          (ev:publish self (make-item-changed-event
                                                            :item self)))))
                                    (push-to-bindings (new-value push) 
                                      (with-slots (bindings) self
                                        (log:debug "Processing ~a binding(s)." (length bindings))
                                        (dolist (binding bindings)
                                          (let ((eff-push (if push 
                                                              (binding:call-push-p binding)
                                                              nil)))
                                            (when eff-push
                                              (binding:exec-push binding new-value))))))
                                    (apply-persistences ()
                                      (with-slots (persistences) self
                                        (log:debug "Processing ~a persistence(s)." (length persistences))
                                        (dolist (persp (%filter-frequency persistences :every-change))
                                          (persp:store
                                           (item-persistence-persp persp)
                                           self)))))
                               (case (car msg)
                                 (:get-state
                                  (let ((state-value (item-state-value *state*)))
                                    (log:debug "Current state value: ~a, item: ~a" state-value id)
                                    (reply state-value)))
                                 (:set-state
                                  (let ((val (getf (cdr msg) :value))
                                        (push (getf (cdr msg) :push))
                                        (timestamp (getf (cdr msg) :timestamp)))
                                    (log:debug "set-state: ~a on item: ~a" val id)
                                    (apply-new-value val timestamp)
                                    (push-to-bindings val push)
                                    (apply-persistences)))))))
                :destroy (lambda (self)
                           (with-slots (bindings) self
                             (dolist (binding bindings)
                               (ignore-errors
                                (binding:destroy binding))))
                           (log:info "Item '~a' destroyed!" id)))))
    (setf (slot-value item 'label) label
          (slot-value item 'value-type-hint) type-hint)
    item))

(defmethod print-object ((obj item) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (format stream "name: ~a, label: ~a, value: ~a, type-hint: ~a"
              (act-cell:name obj)
              (label obj)
              (act-cell:state obj)
              (value-type-hint obj))
      (get-output-stream-string string-stream))))

(defun get-value (item)
  (? item '(:get-state . nil)))

(defun set-value (item value &key (push t) (timestamp nil))
  "Updates item value with push to bindings.
If PUSH is non-nil, bindings will be pushed regardsless of :do-push."
  (! item `(:set-state . (:value ,value :push ,push :timestamp ,timestamp))))

(defun get-item-stateq (item)
  "Returns the item state `item-state'."
  (act-cell:state item))

(defun add-binding (item binding)
  (log:info "Adding binding: ~a to item: ~a" binding item)
  (with-slots (bindings) item
    (push binding bindings)
    (binding:bind-item binding item))
  item)

(defun add-persistence (item persistence &rest other-args)
  ;; unwrap a list in list
  (log:info "Adding persistence: ~a to item: ~a" persistence item)
  (when (listp (car other-args))
    (setf other-args (car other-args)))
  (with-slots (persistences) item
    (let ((item-persp (make-item-persistence
                       :persp persistence
                       :frequency (getf other-args :frequency)
                       :load-on-start (getf other-args :load-on-start))))
      (push item-persp persistences)
      (when (item-persistence-load-on-start item-persp)
        (%fetch-persisted-value persistence item)))))

(defun %fetch-persisted-value (persistence item)
  (log:debug "Loading item value from persp: ~a" persistence)
  (future:fcompleted (persp:fetch persistence item)
      (result)
    (cond
      ((and (consp result) (eq (car result) :error))
       (log:warn "Error loading item (~a) value from persp: ~a" (act-cell:name item) persistence))
      (t
       (set-value item (persp:persisted-item-value result)
                  :push nil
                  :timestamp (persp:persisted-item-timestamp result))))))

(defun destroy (item)
  (ac:stop (act:context item) item :wait t))
