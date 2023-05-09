(defpackage :cl-eta.binding
  (:use :cl)
  (:nicknames :binding)
  (:export #:make-function-binding
           #:bind-item))

(in-package :cl-eta.binding)

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
          :documentation "Recurring delay in seconds. Calls `RETRIEVE-FUN' repeatedly. `NIL' means disabled.")))

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
  (with-slots (bound-items retrieve-fun initial-delay delay) binding
    (log:info "Binding item: " item)
    (setf bound-items (cons item bound-items))
    (let ((timer-fun (lambda ()
                       ;; maybe execute with using tasks
                       (let ((result (funcall retrieve-fun)))
                         (dolist (item (bound-items binding))
                           (item:set-value item result))))))
      (when initial-delay
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
