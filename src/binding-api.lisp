(defpackage :cl-eta.binding
  (:use :cl)
  (:nicknames :binding)
  (:export #:make-function-binding
           #:bind-item
           #:exec-pull
           #:exec-push
           #:pull-passthrough))
