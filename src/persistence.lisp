(in-package :cl-hab.persistence)

(shadowing-import '(act:!
                    act:?
                    act:reply))

(defun make-persistence (id &rest other-args &key type &allow-other-keys)
  (let ((isys (isys:ensure-isys)))
    (ac:actor-of isys
                 :name id
                 :type type
                 :receive (lambda (msg)
                            (log:debug "Received: ~a, msg: ~a" (car msg) msg)
                            (case (car msg)
                              (:store (persist act:*self* (cadr msg) (cddr msg)))
                              (:fetch (reply (retrieve act:*self* (cdr msg))))))
                 :init (lambda (self)
                         (initialize self))
                 :destroy (lambda (self)
                            (shutdown self))
                 :other-args other-args)))

(defun store (persistence item)
  (future:fcompleted
      (item:get-value item)
      (result)
    (! persistence `(:store . (,item . ,result))))
  t)

(defun fetch (persistence item)
  (? persistence `(:fetch . ,item)))

(defun destroy (persistence)
  (ac:stop (act:context persistence) persistence :wait t))
