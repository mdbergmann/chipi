(defpackage :cl-hab.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:*persistences*
           #:defconfig
           #:defitem
           #:binding
           #:defrule
           #:defpersistence
           #:shutdown)
  )

(in-package :cl-hab.hab)

(defvar *items* nil "All items")
(defvar *rules* nil "All rules")
(defvar *persistences* nil "All persistences")

(defmacro defconfig (&body body)
  `(progn
     (envi:ensure-env)
     (unless *items*
       (setf *items* (make-hash-table)))
     (unless *rules*
       (setf *rules* (make-hash-table :test #'equal)))
     (unless *persistences*
       (setf *persistences* (make-hash-table :test #'eq)))
     ,@body))

(defun shutdown ()
  (envi:shutdown-env)
  (clrhash *items*)
  (clrhash *rules*)
  (clrhash *persistences*))

(defmacro defitem (id label &body body)
  (let ((item (gensym "item"))
        (old-item (gensym "old-item"))
        (bindings (gensym "bindn"))
        (binding (gensym "bind"))
        (p-rep (gensym "p-rep"))
        (p-reps (gensym "p-reps"))
        (persp (gensym "persp")))
    `(progn
       (when (and *items* (gethash ,id *items*))
         (log:info "Cleaning old item: " ,id)
         (let ((,old-item (gethash ,id *items*)))
           (item:destroy ,old-item)
           (remhash ,id *items*)))
       (let ((,item (item:make-item ,id ,label))
             (,bindings (loop :for x :in (list ,@body)
                              :if (typep x 'binding::binding)
                                :collect x))
             (,p-reps (loop :for (k v) :on (list ,@body)
                            :if (eq k :persistence)
                              :collect v)))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding))
         (dolist (,p-rep ,p-reps)
           (let ((,persp (gethash (getf ,p-rep :id) *persistences*)))
             (when ,persp
               (item:add-persistence ,item ,persp ,p-rep))))
         (setf (gethash ,id *items*) ,item)))))

(defmacro binding (&rest args)
  `(binding:make-function-binding ,@args))

(defmacro defrule (name &rest args)
  (let ((rule (gensym "rule"))
        (old-rule (gensym "old-rule")))
    `(progn
       (when (and *rules* (gethash ,name *rules*))
         (log:info "Cleaning old rule: " ,name)
         (let ((,old-rule (gethash ,name *rules*)))
           (rule:destroy ,old-rule)
           (remhash ,name *rules*)))
       (let ((,rule (rule:make-rule ,name ,@args)))
         (setf (gethash ,name *rules*) ,rule)))))

(defmacro defpersistence (id factory)
  (let ((persistence (gensym "persistence"))
        (old-persistence (gensym "old-persistence")))
    `(progn
       (when (and *persistences* (gethash ,id *persistences*))
         (log:info "Cleaning old persistence: " ,id)
         (let ((,old-persistence (gethash ,id *persistences*)))
           (persp:destroy ,old-persistence)
           (remhash ,id *persistences*)))
       (let ((,persistence (funcall ,factory ,id)))
         (setf (gethash ,id *persistences*) ,persistence)))))
