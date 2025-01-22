(defpackage :chipi.item
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
           #:name
           #:label
           #:add-binding
           #:add-persistence
           #:destroy
           #:make-item-state
           #:item-state
           #:item-state-value
           #:item-state-timestamp
           ;; boolean value types
           #:true
           #:true-p
           #:false
           #:false-p
           ;; events
           #:item-changed-event
           #:item-changed-event-item))

(in-package :chipi.item)

(defstruct item-changed-event item)

(defclass item (act:actor)
  ((label :initarg :label
          :initform nil
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

(defstruct (item-state
	    (:print-function
	     (lambda (struct stream depth)
	       (declare (ignore depth))
	       (format stream "(item-state value:~a, timestamp:~a)"
		       (item-state-value struct)
		       (local-time:universal-to-timestamp (item-state-timestamp struct))))))
  (value t)
  (timestamp (get-universal-time)))

(defun true-p (val)
  (and (stringp val)
       (string= val "true")))
(defun false-p (val)
  (and (stringp val)
       (string= val "false")))

(deftype true ()
  `(satisfies true-p))
(deftype false ()
  `(satisfies false-p))

(defstruct item-persistence
  (persp nil :type (or null persp:persistence))
  (frequency :every-change)
  (load-on-start t)
  (timer-sig (gensym "item-persistence-timer-sig")))

(defun %filter-frequency (list freq)
  (remove-if-not
   (lambda (p)
     (eq (item-persistence-frequency p) freq))
   list))

(defun %item-receive (msg id)
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
               (dolist (binding (reverse bindings))
                 (handler-case
                     (let ((effective-push (if push 
                                               (binding:call-push-p binding)
                                               nil)))
                       (when effective-push
                         (binding:exec-push binding new-value)))
                   (error (c) (log:warn "Error in binding: ~a, error: ~a" binding c))))))
           (apply-persistences ()
             (with-slots (persistences) self
               (let ((every-change-persps (%filter-frequency persistences :every-change)))
                 (log:debug "Processing 'every-change' ~a persistence(s)." (length every-change-persps))
                 (dolist (persp every-change-persps)
                   (persp:store
                    (item-persistence-persp persp)
                    self))))))
      (case (car msg)
        (:get-state
         (let ((state-value (item-state-value *state*)))
           (log:debug "Current state value: ~a, item: ~a" state-value id)
           (reply state-value)))
        (:set-state
         (let ((old-val (item-state-value *state*))
               (val (getf (cdr msg) :value))
               (push (getf (cdr msg) :push-p))
               (persist (getf (cdr msg) :persist-p t))
               (timestamp (getf (cdr msg) :timestamp)))
           (unless (equal old-val val)
             (log:debug "set-state: ~a on item: ~a" val id)
             (apply-new-value val timestamp)
             (when persist
               (apply-persistences))
             (push-to-bindings val push))))))))

(defun make-item (id &key (label nil) (type-hint nil) (initial-value t))
  (log:info "Creating item: ~a, label: ~a, type-hint: ~a, value: ~a"
            id label type-hint initial-value)
  (let* ((isys (ensure-isys))
         (item (ac:actor-of
                isys
                :name (symbol-name id)
                :type 'item
                :state (make-item-state :value initial-value)
                :receive (lambda (msg) (%item-receive msg id))
                :destroy (lambda (self)
                           (with-slots (bindings persistences) self
                             (dolist (binding bindings)
                               (ignore-errors
                                (binding:destroy binding)))
                             (dolist (persp persistences)
                               (ignore-errors
                                (timer:cancel
                                 (item-persistence-timer-sig persp)))))
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

(defun name (item)
  (act-cell:name item))

(defun get-value (item)
  "Returns `future:future'."
  (? item '(:get-state . nil)))

(defun set-value (item value &key (push t) (timestamp nil) (persist t))
  "Updates item value with push to bindings.
If `PUSH' is non-nil, bindings will be pushed regardsless of `CALL-PUSH-P' on binding definition.
`TIMESTAMP': can be used to define a custom timestamp (universal-time). If `NIL' a timestamp is created.
`PERSIST': if non-nil, persistences will be applied."
  (! item `(:set-state . (:value ,value
                          :push-p ,push
                          :timestamp ,timestamp
                          :persist-p ,persist))))

(defun get-item-stateq (item)
  "Returns the item state `item-state'."
  (act-cell:state item))

(defun add-binding (item binding)
  (log:info "Adding binding: ~a to item: ~a" binding item)
  (with-slots (bindings) item
    (pushnew binding bindings)
    (binding:bind-item binding item))
  item)

(defun add-persistence (item persistence &rest other-args)
  "Adds persistence to item.
`PERSISTENCE' is a `persp:persistence' object.
`OTHER-ARGS' are key arguments:
`:frequency': which by default is
  `:every-change' denoting that every change to the iten should be stored.
  `:every-N<s|m|h>' denoting a number N specified as `s' (seconds), `m' (minutes) or `h' (hours)
  when the item should be stored recurringly."
  ;; unwrap a list in list
  (log:info "Adding persistence: ~a to item: ~a" persistence item)
  (when (listp (car other-args))
    (setf other-args (car other-args)))
  (with-slots (persistences) item
    (let ((item-persp (make-item-persistence
                       :persp persistence
                       :frequency (getf other-args :frequency :every-change)
                       :load-on-start (getf other-args :load-on-start))))
      (push item-persp persistences)
      (let ((load-on-start (item-persistence-load-on-start item-persp))
            (frequency (item-persistence-frequency item-persp))
            (timer-sig (format nil "~a-~a"
			       (item-persistence-timer-sig item-persp)
			       (name item)))
            (persp (item-persistence-persp item-persp)))
        (when load-on-start
          (%fetch-persisted-value persistence item))
        (when (not (eq :every-change frequency))
          (let ((freq-in-secs (%parse-frequency frequency)))
            (log:info "Scheduling persistence: ~a, frequency: ~a secs" persistence freq-in-secs)
            (timer:schedule-recurring freq-in-secs
                                      (lambda ()
                                        (persp:store persp item))
                                      timer-sig)))))))

(defun %fetch-persisted-value (persistence item)
  (log:debug "Loading item value from persp: ~a" persistence)
  (future:fcompleted (persp:fetch persistence item)
      (result)
    (cond
      ((and (consp result) (eq (car result) :error))
       (log:warn "Error loading item (~a) value from persp: ~a"
                 (name item) persistence))
      (t
       (progn
         (log:debug "Setting loaded value to item: ~a" item)
         (set-value item (persp:persisted-item-value result)
                    :push nil
                    :timestamp (persp:persisted-item-timestamp result)
                    :persist nil ; loaded values should not be persisted
                    ))))))

(defun %parse-frequency (freq)
  (let* ((freq-str (symbol-name freq))
         (timing (second (str:split "EVERY-" freq-str))))
    (multiple-value-bind (start end ar1 ar2)
        (ppcre:scan "([0-9]*)([S|M|H])" timing)
      (declare (ignore end))
      (when (null start)
        (log:warn "Invalid frequency: ~a" freq)
        (error "Invalid frequency: ~a" freq))
      (let ((num (subseq timing (elt ar1 0) (elt ar1 1)))
            (unit (subseq timing (elt ar2 0) (elt ar2 1))))
        (cond ((string= unit "S")
               (parse-integer num))
              ((string= unit "M")
               (* (parse-integer num) 60))
              ((string= unit "H")
               (* (parse-integer num) 3600)))))))

(defun destroy (item)
  (ac:stop (act:context item) item :wait t))
