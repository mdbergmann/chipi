(in-package :cl-hab.persistence)

(shadowing-import '(act:!
                    act:?
                    act:reply))

(defun make-persistence (id &rest other-args &key type &allow-other-keys)
  "Creates a persistence actor with the given `id' and `other-args'.
The type of the persistence actor can be specified with the
`:type' keyword.
`other-args' can be used by sub-classes to pass additional arguments to the underlying actor
which may be necessary to configure the persistence, i.e.: in `act:pre-start'.
See `simple-persistence' as example.
This constructor is not public, subclasses should provide their own constructor but call this one."
  (let ((isys (isys:ensure-isys)))
    (ac:actor-of isys
                 :name id
                 :type type
                 :receive (lambda (msg)
                            (log:debug "Received: ~a, msg: ~a" (car msg) msg)
                            (case (car msg)
                              (:store (persist act:*self* (cadr msg) (cddr msg)))
                              (:fetch (reply
                                       (multiple-value-bind (value timestamp)
                                           (retrieve act:*self* (cdr msg))
                                         `(,value . ,timestamp))))))
                 :init (lambda (self)
                         (initialize self))
                 :destroy (lambda (self)
                            (shutdown self))
                 :other-args other-args)))

(defun store (persistence item)
  "Triggers the 'store' procedure of the persistence actor.
The actual persistence method called as a result is `persp:persist'."
  (future:fcompleted
      (item:get-value item)
      (result)
    (! persistence `(:store . (,item . ,result))))
  t)

(defun fetch (persistence item)
  "Triggers the 'fetch' procedure of the persistence actor.
The actual persistence method called as a result is `persp:retrieve'.
Returns a pair with value and timestamp."
  (? persistence `(:fetch . ,item)))

(defun destroy (persistence)
  "Destroys the persistence."
  (ac:stop (act:context persistence) persistence :wait t))
