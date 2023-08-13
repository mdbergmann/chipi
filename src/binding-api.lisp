(defpackage :cl-hab.binding
  (:use :cl)
  (:nicknames :binding)
  (:export #:make-function-binding
           #:bind-item
           #:exec-pull
           #:exec-push
           #:call-push-p
           #:destroy))
