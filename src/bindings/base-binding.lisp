(in-package :cl-hab.binding)

(defclass binding ()
  ((bound-items :initform '()
                :reader bound-items
                :documentation "The bound items. On operation the value will be updated to each item.")
   (pull-fun :initarg :pull-fun
             :initform nil
             :documentation "The function that retrieves the value.")
   (push-fun :initarg :push-fun
             :initform nil
             :documentation "Function to push the item values to some receiver.
Beware that `pull' will implicitly call `push'. Make sure you do the right thing in `pull-fun'.")
   (transform-fun :initarg :transform-fun
                  :initform nil
                  :documentation "A function transforming an input value to an output value.
The input value comes from `pull-fun'.
The output value will be set on the item, should an item be attached.")
   (pull-passthrough :initarg :pull-passthrough
                     :initform nil
                     :reader pull-passthrough
                     :documentation "defines whether the value of 'pull' will be passed through to 'push', after the transformation chain has been executed and the value been set to the item.")
   (initial-delay :initarg :initial-delay
                  :initform nil
                  :documentation "Initial delay in seconds where `RETRIEVE-FUN' is executed. `NIL' means disabled.")
   (delay :initarg :delay
          :initform nil
          :documentation "Recurring delay in seconds. Calls `RETRIEVE-FUN' repeatedly. `NIL' means disabled.")))

(defmethod print-object ((obj binding) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((string-stream (make-string-output-stream)))
      (with-slots (initial-delay delay pull-passthrough) obj
        (format stream "initial-delay: ~a, delay: ~a, pull-passthrough: ~a"
                initial-delay delay pull-passthrough))
      (get-output-stream-string string-stream))))

(defun make-function-binding (&key
                                (pull nil)
                                (push nil)
                                (transform nil)
                                (initial-delay nil)
                                (delay nil)
                                (pull-passthrough nil))
  (make-instance 'binding
                 :pull-fun pull
                 :push-fun push
                 :transform-fun transform
                 :initial-delay initial-delay
                 :delay delay
                 :pull-passthrough pull-passthrough))

(defun exec-pull (binding)
  (log:debug "Retrieving value...")
  (with-slots (bound-items pull-fun transform-fun) binding
    ;; maybe execute using tasks
    (when pull-fun
      (let ((result (funcall pull-fun)))
        (when transform-fun
          (setf result (funcall transform-fun result)))
        (dolist (item bound-items)
          (item:set-value item result))))))

(defun exec-push (binding value)
  (log:debug "Pushing value: " value)
  (with-slots (push-fun) binding
    ;; maybe execute using tasks
    (when push-fun
      (funcall push-fun value))))

(defun bind-item (binding item)
  (with-slots (bound-items initial-delay delay) binding
    (log:info "Binding item: " item)
    (setf bound-items (cons item bound-items))
    (let ((timer-fun (lambda () (exec-pull binding))))
      (when initial-delay
        (log:info "Scheduling initial delay: " initial-delay)
        (sched:schedule initial-delay timer-fun))

      ;; only on binding the first item we schedule
      (when (and delay (not (second bound-items)))
        (log:info "Scheduling delay: " delay)
        (let (recurring-timer-fun)
          (setf recurring-timer-fun
                (lambda ()
                  (funcall timer-fun)
                  (sched:schedule delay recurring-timer-fun)))
          (sched:schedule delay recurring-timer-fun))))))
