(in-package :chipi.binding)

(defclass binding ()
  ((bound-items :initform '()
                :reader bound-items
                :documentation "The bound items. On operation the value will be updated to each item.")
   (pull-fun :initarg :pull-fun
             :initform nil
             :accessor pull-fun
             :documentation "0-arity function that retrieves the value.")
   (push-fun :initarg :push-fun
             :initform nil
             :accessor push-fun
             :documentation "Function to push the item values to some receiver.
Beware that `pull' will only call `push' when `call-push-p' is set.
Make sure you do the right thing in `pull-fun'.")
   (transform-fun :initarg :transform-fun
                  :initform nil
                  :documentation "A function transforming an input value to an output value.
The input value comes from `pull-fun'.
The output value will be set on the item, should an item be attached.")
   (call-push-p :initarg :call-push-p
                :initform nil
                :reader call-push-p
                :documentation "defines whether setting a new value (`item:set-value') will be passed through to 'push', after the transformation chain has been executed.")
   (initial-delay :initarg :initial-delay
                  :initform nil
                  :documentation "Initial delay in seconds where `PULL-FUN' is executed. `NIL' means disabled and `PULL-FUN' is not called.")
   (delay :initarg :delay
          :initform nil
          :documentation "Recurring delay in seconds. Calls `PULL-FUN' repeatedly. `NIL' means disabled.")
   (timers :initform (make-hash-table :test #'eq)
           :documentation "The timers that are scheduled for this binding.")))

(defmethod print-object ((obj binding) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (with-slots (initial-delay delay call-push-p bound-items) obj
        (format stream "initial-delay: ~a, delay: ~a, call-push-p: ~a, bound-items: ~a"
                initial-delay delay call-push-p bound-items))
      (get-output-stream-string string-stream))))

(defun make-function-binding (&key
                                (pull nil)
                                (push nil)
                                (transform nil)
                                (initial-delay nil)
                                (delay nil)
                                (call-push-p nil))
  (make-instance 'binding
                 :pull-fun pull
                 :push-fun push
                 :transform-fun transform
                 :initial-delay initial-delay
                 :delay delay
                 :call-push-p call-push-p))

(defun exec-pull (binding)
  (log:debug "Pulling value from: " binding)
  (with-slots (bound-items pull-fun transform-fun) binding
    ;; maybe execute using tasks
    (when pull-fun
      (handler-case
          (let ((result (funcall pull-fun)))
            (log:debug "Pulling value from: " binding ", result: " result)
            (when transform-fun
              (setf result (funcall transform-fun result)))
            (dolist (item bound-items)
              (item:set-value item result)))
        (error (c)
          (log:warn "Error pulling value from: ~a, error: ~a" binding c))))))

(defun exec-push (binding value)
  (log:debug "Pushing value: " value " to: " binding)
  (with-slots (push-fun) binding
    ;; maybe execute using tasks
    (when push-fun
      (funcall push-fun value))))

(defun bind-item (binding item)
  (with-slots (bound-items initial-delay delay) binding
    (log:info "Binding item: ~a, on: ~a" item binding)
    (setf bound-items (cons item bound-items))
    (let ((timer-fun (lambda () (exec-pull binding))))
      (when initial-delay
        (log:info "Scheduling initial delay: " initial-delay " on: " binding)
        (%add-timer binding
                    (timer:schedule-once initial-delay timer-fun)
                    :initial-delay))
      ;; only on binding the first item we schedule
      (when (and delay (not (second bound-items)))
        (log:info "Scheduling recurring delay: " delay " on: " binding)
        (%add-timer binding
                    (timer:schedule-recurring delay timer-fun)
                    :delay)))))

(defun %add-timer (binding timer timer-type)
  "`TIMER' is a just a signature."
  (with-slots (timers) binding
    (log:debug "Adding timer: " timer " of type: " timer-type " to: " binding)
    (setf (gethash timer-type timers) timer)))

(defun destroy (binding)
  (log:info "Destroying binding: " binding)
  (with-slots (timers) binding
    (maphash (lambda (key timer)
               (declare (ignore key))
               (log:debug "Cancelling timer: " timer " on: " binding)
               (timer:cancel timer))
             timers)
    (clrhash timers)))
