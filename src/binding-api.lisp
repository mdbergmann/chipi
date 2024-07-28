(defpackage :chipi.binding
  (:use :cl)
  (:nicknames :binding)
  (:export #:make-function-binding
           #:binding
           #:bind-item
           #:exec-pull
           #:exec-push
           #:call-push-p
           #:destroy))
