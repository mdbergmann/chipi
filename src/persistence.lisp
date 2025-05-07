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
           :receive (lambda (msg) (%persp-receive msg))
           :init (lambda (self)
                   (initialize self))
           :destroy (lambda (self)
                      (shutdown self))
           other-args)))

(defun %persp-receive (msg)
  (log:debug "Received: ~a, msg: ~a" (car msg) msg)
  (case (car msg)
    (:store (persist *self* (cadr msg)))
    (:fetch
     (let ((item (second msg))
           (range (third msg))
           (aggregate (fourth msg)))
       (handler-case
         (cond
          ((not range) (reply (retrieve *self* item)))
          (aggregate (reply (retrieve-range *self* item range aggregate)))
          (t (reply (retrieve-range *self* item range))))
         (error (e)
           (log:warn "Error on retrieving persisted data: ~a" e)
           (reply `(:error . ,e))))))))

(defun make-relative-range (&key
                              (seconds nil)
                              (minutes nil)
                              (hours nil)
                              (days nil)
                              (end-ts nil))
  "Creates a relative range with the given `seconds', `minutes', `hours' and `days'. Specify only one.
`end-ts' is the end timestamp boundary. Specified values for relative time are substracted from `end-ts' for a start boundary. If `end-ts' is `nil' then end-time boundary is 'now'."
  (make-instance 'relative-range
                 :seconds seconds
                 :minutes minutes
                 :hours hours
                 :days days
                 :end-ts end-ts))

(defun make-absolute-range (start-ts end-ts)
  "Creates an absolute range with given `start-ts' and `end-ts' as universal time(stamps)."
  (make-instance 'absolute-range
                 :start-ts start-ts
                 :end-ts end-ts))

(defun store (persistence item)
  "Triggers the 'store' procedure of the persistence actor.
The actual persistence method called as a result is `persp:persist'."
  (future:fcompleted
      (item:get-value item)
      (result)
    (log:debug "Tell persist item: ~a, persp: ~a"
               (item:name item)
               (act-cell:name persistence))
    (! persistence `(:store . (,item . ,result))))
  t)

(defun fetch (persistence item &optional range aggregate)
  "Triggers the 'fetch' procedure of the persistence actor.
The actual persistence method called as a result is:
 • `persp:retrieve'            when no RANGE is given
 • `persp:retrieve-range'      when only RANGE is given
RANGE must be a `relative-range' or `absolute-range'.
AGGREGATE, if non-`nil', should be one of `:avg`, `:min` or `:max`."
  (? persistence `(:fetch ,item ,range ,aggregate)))

(defun destroy (persistence)
  "Destroys the persistence."
  (ac:stop (act:context persistence) persistence :wait t))
