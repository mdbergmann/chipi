(defpackage :cl-eta.binding
  (:use :cl)
  (:nicknames :binding)
  (:export #:make-function-binding
           #:bind-item
           #:exec-retrieve
           #:exec-push))
