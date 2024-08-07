(defpackage :chipi.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:*persistences*
           #:get-item
           #:get-items
           #:get-persistence
           #:get-rule
           #:defconfig
           #:defitem
           #:binding
           #:defrule
           #:defpersistence
           #:shutdown)
  )

(in-package :chipi.hab)

(defvar *items* nil "All items")
(defvar *rules* nil "All rules")
(defvar *persistences* nil "All persistences")

(defun get-item (id)
  "Returns the item with the given id from the created items."
  (when *items*
    (gethash id *items*)))

(defun get-items ()
  "Returns all items."
  (when *items*
    (loop :for item :being :the :hash-value :of *items*
          :collect item)))

(defun get-persistence (id)
  "Returns the persistence with the given id from the created persistences."
  (when *persistences*
    (gethash id *persistences*)))

(defun get-rule (name)
  "Returns the rule with the given name from the created rules."
  (when *rules*
    (gethash name *rules*)))

(defmacro defconfig (&body body)
  "Defines a configuration for the environment.
It will start the environment if it is not already started.
It is re-entrant, so it can be called multiple times.
But if environment is already configured/started it does nothing.
It also will setup items, rules and persistences storages."
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
  "Shuts down the environment and cleans all items rules and persistences."
  (envi:shutdown-env)
  (when *items* (clrhash *items*))
  (when *rules* (clrhash *rules*))
  (when *persistences* (clrhash *persistences*))
  :ok)

(defmacro defitem (id label type-hint &rest body)
  "Defines an item.
It will create the item if it does not exist.
It will clean and re-create the item if it already exists.
Cleaning means all attached bindings are re-created and persistence are re-attached.
An `:initial-value' can be used to specify the initial value of the item.
Bindings can be defined as a list of `binding's.
The `binding' arguments are passed to `binding:make-function-binding'.
Persistences are references via `:persistence' key.
`persistence' key allows to define a plist of `:id' and `:frequency' configuration.
`:id' specifies the persistence id.
`:frequency' specifies the persistence frequency. Currently only `:every-change' exists.
See `hab-test.lisp' and `item' for more examples."
  (let ((item (gensym "item"))
        (old-item (gensym "old-item"))
        (bindings (gensym "bindn"))
        (binding (gensym "bind"))
        (p-rep (gensym "p-rep"))
        (p-reps (gensym "p-reps"))
        (persp (gensym "persp"))
        (initial-value (gensym "initial-value")))
    `(progn
       (when (get-item ,id)
         (log:info "Cleaning old item: " ,id)
         (let ((,old-item (get-item ,id)))
           (item:destroy ,old-item)
           (remhash ,id *items*)))
       (let* ((,bindings (loop :for x :in (list ,@body)
                               :if (typep x 'binding::binding)
                                 :collect x))
              (,p-reps (loop :for (k v) :on (list ,@body)
                             :if (eq k :persistence)
                               :collect v))
              (,initial-value (loop :for (k v) :on (list ,@body)
                                    :if (eq k :initial-value)
                                      :return v))
              (,item (item:make-item ,id
                                     :label ,label
                                     :type-hint ,type-hint
                                     :initial-value ,initial-value)))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding))
         (dolist (,p-rep ,p-reps)
           (let ((,persp (get-persistence (getf ,p-rep :id))))
             (when ,persp
               (item:add-persistence ,item ,persp ,p-rep))))
         (setf (gethash ,id *items*) ,item)))))

(defmacro binding (&rest args)
  "Creates a binding.
See `binding:make-function-binding' for more information and arguments."
  `(binding:make-function-binding ,@args))

(defmacro defrule (name &rest args)
  "Defines a rule.
It will create the rule if it does not exist.
It will clean and re-create the rule if it already exists.
See `rule:make-rule' for more information and arguments."
  (let ((rule (gensym "rule"))
        (old-rule (gensym "old-rule")))
    `(progn
       (when (get-rule ,name)
         (log:info "Cleaning old rule: " ,name)
         (let ((,old-rule (get-rule ,name)))
           (rule:destroy ,old-rule)
           (remhash ,name *rules*)))
       (let ((,rule (rule:make-rule ,name ,@args)))
         (setf (gethash ,name *rules*) ,rule)))))

(defmacro defpersistence (id factory)
  "Defines a persistence.
Persistence generally represents a storage that items use for persisting their values.
It will create the persistence if it does not exist.
It will clean and re-create the persistence if it already exists.
The factory function is called with the persistence id as argument and allows to create required persistence type.
See `hab-test.lisp' and `persistence' for more examples."
  (let ((persistence (gensym "persistence"))
        (old-persistence (gensym "old-persistence")))
    `(progn
       (when (get-persistence ,id)
         (log:info "Cleaning old persistence: " ,id)
         (let ((,old-persistence (get-persistence ,id)))
           (persp:destroy ,old-persistence)
           (remhash ,id *persistences*)))
       (let ((,persistence (funcall ,factory ,id)))
         (setf (gethash ,id *persistences*) ,persistence)))))
