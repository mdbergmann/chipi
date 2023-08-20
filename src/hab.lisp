(defpackage :cl-hab.hab
  (:use :cl)
  (:nicknames :hab)
  (:export #:*items*
           #:*rules*
           #:*persistences*
           #:get-item
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

(defun get-item (id)
  "Returns the item with the given id."
  (gethash id *items*))

(defmacro defconfig (&body body)
  "Defines a configuration for the environment.
It will start the environment if it is not already started.
It is re-entrant, so it can be called multiple times.
It will setup items, rules and persistences storages."
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
  (clrhash *items*)
  (clrhash *rules*)
  (clrhash *persistences*))

(defmacro defitem (id label &body body)
  "Defines an item.
It will create the item if it does not exist.
It will clean and re-create the item if it already exists.
Cleaning means all attached bindings are re-created and persistence are re-attached.
Bindings can be defined as a list of `binding's.
The `binding' arguments are passed to `binding:make-function-binding'.
Persistences are references via `:persistence' key.
`persistence' key allows to define a plist of `:id' and `:frequency' configuration.
`:id' specifies the persistence id.
`:frequency' specifies the persistence frequency. Currently only `:every-change' exists. 
See `hab-test.lisp' for more examples."
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
       (when (and *rules* (gethash ,name *rules*))
         (log:info "Cleaning old rule: " ,name)
         (let ((,old-rule (gethash ,name *rules*)))
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
Currently only `simple-persistence' exists."
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

;; TODOs:
;; - add 'get-item' function to retrieve item by id
