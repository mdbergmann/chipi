(in-package :chipi.binding)

(eval-when (:compile-toplevel)
  (shadowing-import '(future:fcompleted
                      future:frecover
                      future:futurep
                      future:fmap
                      future:with-fut
                      binding-arrows:->)))

(defclass binding ()
  ((bound-items :initform '()
                :reader bound-items
                :documentation "The bound items. On operation the value will be updated to each item.")
   (pull-fun :initarg :pull-fun
             :initform nil
             :accessor pull-fun
             :documentation "0-arity function that retrieves the value.
The function must return a value representing the new item value.
The returned value can be wrapped in a `future`, if the pull is performed asynchronously.
To allow control over the subsequent call chain it is possible to return `values' with options indicating whether a subsequent 'push' should be performed.
I.e. `(values <value> '(:push [nil|t]))`.
If `:push' is `NIL' then no push will be performed.
The default, if omitted and also when a single value is returned is to push.")
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
  "Executes the `pull-fun' as defined on the binding.
Beware that `pull-fun' is executed by the thread that calls `exec-pull'. This may differ. I.e.when called from timer when `:initial-delay' or `:delay' is defined.
In order to not unwind/collapse a timer stack execution of `pull-fun' is error checked (handler-case).
`exec-pull' will set pulled value to bound items, or if a `transform-fun' is defined will call that first."
  (log:debug "Pulling value from: ~a" binding)
  (with-slots (bound-items pull-fun transform-fun) binding
    (when pull-fun
      (handler-case
          (multiple-value-bind (pull-result opts)
              (funcall pull-fun)
            (log:debug "Pulled value: ~a, opts: ~a" pull-result opts)
            (-> (if (futurep pull-result) pull-result (with-fut pull-result))
              (fmap (result)
                  (if transform-fun
                      (prog1 (funcall transform-fun result)
                        (log:debug "Transformed value to: ~a" result))
                      result))
              (fcompleted (result)
                  (progn
                    (log:debug "Setting on items (~a)" (length bound-items))
                    (dolist (item bound-items)
                      (item:set-value item result
                                      :push
                                      (getf (if (listp opts)
                                                opts
                                                nil)
                                            :push t)))))
              (frecover
               (error (c)
                      (log:error "Error on handling pull future: ~a" c)))))
        (error (c)
          (log:warn "Error pulling value from: ~a, error: ~a" binding c))))))

(defun exec-push (binding value)
  "`exec-push' is called after an item value change. It executed `push-fun' as defined on the binding.
The caller of `exec-push' is responsible for unwind protection. No error handling is done here.
This is to have the higher-level module decide how to handle the error."
  (with-slots (push-fun) binding
    (when push-fun
      (log:debug "Pushing value: ~a to: ~a" value binding)
      (funcall push-fun value))))

(defun %add-timer (binding timer timer-type)
  "`TIMER' is just a signature."
  (with-slots (timers) binding
    (log:debug "Adding timer: " timer " of type: " timer-type " to: " binding)
    (setf (gethash timer-type timers) timer)))

(defun bind-item (binding item)
  (with-slots (bound-items initial-delay delay) binding
    (log:info "Binding item: ~a, on: ~a" item binding)
    (setf bound-items (cons item bound-items))
    (let ((timer-fun (lambda () (exec-pull binding))))
      (when initial-delay
        (log:info "Scheduling initial delay: " initial-delay " on: " binding)
        (assert (> initial-delay 0) nil "Initial delay must be > 0, use 0.1 for a small value!")
        (%add-timer binding
                    (timer:schedule-once initial-delay timer-fun)
                    :initial-delay))
      ;; only on binding the first item we schedule
      (when (and delay (not (second bound-items)))
        (log:info "Scheduling recurring delay: " delay " on: " binding)
        (let ((timer-sig (format nil "~a-~a"
                                 (gensym "binding-timer-")
                                 (item:name item))))
          (%add-timer binding
                      (timer:schedule-recurring delay timer-fun timer-sig)
                      :delay))))))

(defun destroy (binding)
  (log:info "Destroying binding: " binding)
  (with-slots (timers) binding
    (maphash (lambda (key timer)
               (declare (ignore key))
               (log:debug "Cancelling timer: " timer " on: " binding)
               (timer:cancel timer))
             timers)
    (clrhash timers)))
