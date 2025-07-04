(defpackage :chipi.hab
  (:use :cl)
  (:nicknames :hab)
  (:import-from #:alexandria
                #:with-gensyms
                #:hash-table-values
                #:when-let)
  (:export #:*items*
           #:*itemgroups*
           #:*rules*
           #:*persistences*
           #:get-itemgroup
           #:get-items-on-group
           #:get-item
           #:get-items
           #:get-itemgroups
           #:get-persistence
           #:get-rule
           #:set-item-value
           #:get-item-valueq
           #:defconfig
           #:defitemgroup
           #:defitem
           #:binding
           #:defrule
           #:defpersistence
           #:shutdown
           #:add-to-shutdown))

(in-package :chipi.hab)

(defvar *itemgroups* nil "All itemgroups")
(defvar *items* nil "All items")
(defvar *rules* nil "All rules")
(defvar *persistences* nil "All persistences")
(defvar *shutdown-hooks* nil "Shutdown hooks for plugins")

(defun get-itemgroup (id)
  "Returns the itemgroup with `id' if exists, `nil' otherwise."
  (when *itemgroups*
    (gethash id *itemgroups*)))

(defun get-items-on-group (id)
  "Returns the items of group as list"
  (when *itemgroups*
    (itemgroup:get-items (get-itemgroup id))))

(defun get-item (id)
  "Returns the item with the given id from the created items."
  (when *items*
    (gethash id *items*)))

(defun get-items ()
  "Returns all items."
  (when *items*
    (hash-table-values *items*)))

(defun get-itemgroups ()
  "Returns all itemgroups."
  (when *itemgroups*
    (hash-table-values *itemgroups*)))

(defun get-persistence (id)
  "Returns the persistence with the given id from the created persistences."
  (when *persistences*
    (gethash id *persistences*)))

(defun get-rule (name)
  "Returns the rule with the given name from the created rules."
  (when *rules*
    (gethash name *rules*)))

(defun set-item-value (item-sym value &key (push t pushp)
                                        (timestamp nil timestamp-p)
                                        (persist t persistp))
  (let ((args (list (get-item item-sym) value)))
    (if pushp (setf args (append args (list :push push))))
    (if timestamp-p (setf args (append args (list :timestamp timestamp))))
    (if persistp (setf args (append args (list :persist persist))))
    (apply #'item:set-value args)))

(defun get-item-valueq (item-sym)
  (let ((item-state (item:get-item-stateq (get-item item-sym))))
    (values
     (item:item-state-value item-state)
     (item:item-state-timestamp item-state))))

(defun delete-item (item-id group-id)
  (let ((item (get-item item-id))
        (group (or (get-itemgroup group-id) (get-itemgroup 'ch-default))))
    (when item
      (item:destroy item)
      (remhash item-id *items*))
    (when group
      (itemgroup:remove-item group item-id))))

(defmacro defconfig (system &body body)
  "Defines a configuration for the environment.
It will start the environment if it is not already started.
It is re-entrant, so it can be called multiple times.
But if environment is already configured/started it does nothing.
It also will setup items, rules and persistences storages.
The parameter `system' defines the CL system root folder for runtime files."
  `(progn
     (log:info "Using system: ~a as root" ,system)
     (envi:ensure-env ,system)
     (unless *itemgroups*
       (setf *itemgroups* (make-hash-table)))
     (unless *items*
       (setf *items* (make-hash-table)))
     (unless *rules*
       (setf *rules* (make-hash-table :test #'equal)))
     (unless *persistences*
       (setf *persistences* (make-hash-table :test #'eq)))
     (setf *shutdown-hooks* nil)
     (defitemgroup 'ch-default "Default")
     ,@body))

(defun shutdown ()
  "Shuts down the environment, calls shutdown hooks and cleans all items rules and persistences."
  (log:info "Executing shutdown hooks (~a)..." (length *shutdown-hooks*))
  (dolist (hook *shutdown-hooks*)
    (multiple-value-bind (ret condition)
        (ignore-errors
         (funcall hook))
      (declare (ignore ret))
      (when condition
        (log:info "Condition on shutdown hook: ~a" condition))))
  (envi:shutdown-env)
  (when *itemgroups* (clrhash *itemgroups*))
  (when *items* (clrhash *items*))
  (when *rules* (clrhash *rules*))
  (when *persistences* (clrhash *persistences*))
  (when *shutdown-hooks* (setf *shutdown-hooks* nil))
  :ok)

(defun add-to-shutdown (fun)
  (push fun *shutdown-hooks*))

(defmacro defitemgroup (id label)
  "Defines an itemgroup.
Itemgroups are containers for items."
  (with-gensyms (group old-group groupitem old-group-items)
    `(let* ((,old-group (get-itemgroup ,id))
            (,old-group-items (if ,old-group
                                  (itemgroup:get-items ,old-group)
                                  nil)))
       (let ((,group (itemgroup:make-itemgroup ,id :label ,label)))
         (dolist (,groupitem ,old-group-items)
           (itemgroup:add-item ,group ,groupitem))
         (setf (gethash ,id *itemgroups*) ,group)))))

(defmacro defitem (id label type-hint &rest body)
  "Defines an item.
It will create the item if it does not exist.
It will clean and re-create the item if it already exists.
Cleaning means all attached bindings are re-created and persistence are re-attached.
An `:initial-value' can be used to specify the initial value of the item.
A `:group' key can used to specify to which `itemgroup' the `item' should belong.
It will then be added to the group.
A `:tags' key can be used to specify tags as an alist of (key . value) pairs.

Bindings can be defined as a list of `binding's.
The `binding' arguments are passed to `binding:make-function-binding'.
Persistences are references via `:persistence' key.

`persistence' key allows to define a plist of `:id' and `:frequency' configuration.
`:id' specifies the persistence id.
`:frequency' specifies the persistence frequency.
  - can be `:every-change' or
  - a notation of the form: `:every-N<s|m|h>' where N is the number, s (seconds), m (minutes) and h (hours).

See `hab-test.lisp' and `item' for more examples."
  (with-gensyms
      (body-forms
       item old-item bindings binding
       p-rep p-reps persp initial-value
       itemgroups ig tags)
    `(progn
       (when-let ((,old-item (get-item ,id)))
         (log:info "Cleaning from itemgroups...")
         (dolist (,ig (item:group ,old-item))
           (itemgroup:remove-item (get-itemgroup ,ig) ,id))
         ;;(setf ,ig nil)
         (log:info "Cleaning old item: " ,id)
         (item:destroy ,old-item)
         (remhash ,id *items*))
       (let* ((,body-forms (list ,@body))
              (,bindings (loop :for x :in ,body-forms
                               :if (typep x 'binding::binding)
                                 :collect x))
              (,p-reps (loop :for (k v) :on ,body-forms
                             :if (eq k :persistence)
                               :collect v))
              (,initial-value (loop :for (k v) :on ,body-forms
                                    :if (eq k :initial-value)
                                      :return v))
              (,itemgroups (loop :for (k v) :on ,body-forms
                                :if (eq k :group)
                                  :return v))
              (,tags (loop :for (k v) :on ,body-forms
                           :if (eq k :tags)
                             :return v))
              (,item (item:make-item ,id
                                     :label ,label
                                     :type-hint ,type-hint
                                     :initial-value ,initial-value
                                     :tags ,tags
                                     :group ,itemgroups)))
         (if ,itemgroups
             ;; adding the new item will replace the previous item
             ;; because use of hash-table where key is the item-id
             (dolist (,ig ,itemgroups)
               (itemgroup:add-item (get-itemgroup ,ig) ,item))
             (itemgroup:add-item (get-itemgroup 'ch-default) ,item))
         (dolist (,binding ,bindings)
           (item:add-binding ,item ,binding))
         (dolist (,p-rep ,p-reps)
           (when-let ((,persp (get-persistence (getf ,p-rep :id))))
             (item:add-persistence ,item ,persp ,p-rep)))
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
  (with-gensyms (rule old-rule)
    `(progn
       (when-let ((,old-rule (get-rule ,name)))
         (log:info "Cleaning old rule: " ,name)
         (rule:destroy ,old-rule)
         (remhash ,name *rules*))
       (let ((,rule (rule:make-rule ,name ,@args)))
         (setf (gethash ,name *rules*) ,rule)))))

(defmacro defpersistence (id factory)
  "Defines a persistence.
Persistence generally represents a storage that items use for persisting their values.
It will create the persistence if it does not exist.
It will clean and re-create the persistence if it already exists.
The factory function is called with the persistence id as argument and allows to create required persistence type.
See `hab-test.lisp' and `persistence' for more examples."
  (with-gensyms (persistence old-persistence)
    `(progn
       (when-let ((,old-persistence (get-persistence ,id)))
         (log:info "Cleaning old persistence: " ,id)
         (persp:destroy ,old-persistence)
         (remhash ,id *persistences*))
       (let ((,persistence (funcall ,factory ,id)))
         (setf (gethash ,id *persistences*) ,persistence)))))
