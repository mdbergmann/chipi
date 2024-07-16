(in-package :chipi.persistence)

(eval-when (:compile-toplevel)
  (shadowing-import '(act:!
                      act:?
                      act:reply
                      act:*self*)))

(defun make-persistence (id &rest other-args &key type &allow-other-keys)
  "Creates a persistence actor with the given `id' and `other-args'.
The type of the persistence actor can be specified with the
`:type' keyword.
`other-args' can be used by sub-classes to pass additional arguments to the underlying actor.
See `simple-persistence' as example.
This constructor is not public, subclasses should provide their own constructor but call this one."
  (let ((isys (isys:ensure-isys)))
    (apply #'ac:actor-of isys
           :name id
           :type type
           :receive (lambda (msg)
                      (log:debug "Received: ~a, msg: ~a" (car msg) msg)
                      (case (car msg)
                        (:store (persist *self* (cadr msg)))
                        (:fetch
                         (let ((item (cadr msg))
                               (range (cddr msg)))
                           (handler-case
                               (if range
                                   (reply (retrieve-range *self* item range))
                                   (reply (retrieve *self* item)))
                             (error (e)
                               (log:warn "Error on retrieving persisted data: ~a" e)
                               (reply `(:error . ,e))))))))
           :init (lambda (self)
                   (initialize self))
           :destroy (lambda (self)
                      (shutdown self))
           other-args)))

(defun make-relative-range (&key
                              (seconds nil)
                              (minutes nil)
                              (hours nil)
                              (days nil))
  "Creates a relative range with the given `seconds', `minutes', `hours' and `days'.
Specify only one."
  (make-instance 'relative-range
                 :seconds seconds
                 :minutes minutes
                 :hours hours
                 :days days))

(defun store (persistence item)
  "Triggers the 'store' procedure of the persistence actor.
The actual persistence method called as a result is `persp:persist'."
  (future:fcompleted
      (item:get-value item)
      (result)
    (! persistence `(:store . (,item . ,result))))
  t)

(defun fetch (persistence item &optional range)
  "Triggers the 'fetch' procedure of the persistence actor.
The actual persistence method called as a result is `persp:retrieve'.
Optionally specify a `range' to retrieve a list of values that satisfy the `range'
See `item:range' for more details of range. Currently only `relative-range' is supported.
Returns a `persisted-item' instance or a list of `persisten-item's if range is specified."
  (? persistence `(:fetch . (,item . ,range))))

(defun destroy (persistence)
  "Destroys the persistence."
  (ac:stop (act:context persistence) persistence :wait t))
