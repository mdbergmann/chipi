(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; we're expecting chipi project root folder to be cwd
  (let* ((cwd (namestring (uiop:getcwd)))
         (plugin-asd (format nil "~a~a" cwd "bindings/knx/binding-knx.asd")))
    (asdf:load-asd plugin-asd :name "binding-knx")
    (asdf:load-system :binding-knx)))

(defpackage :my-hab
  (:use :cl :chipi.hab :binding-knx))
(in-package :my-hab)

(defconfig
  (knx-init :gw-host "192.168.50.40"))

(defitem 'az-light "Office main light" 'boolean
  (knx-binding :ga "0/0/4"
               :dpt "1.001"
               :call-push-p t
               :initial-delay 2))

(defrule "do when office light switches on"
    :when-item-change 'az-light
    :do (lambda (trigger)
          (format t "Trigger: ~a~%" trigger)
          (let ((item (cdr trigger)))
            (future:fcompleted (item:get-value item)
                (value)
              (when (eq value 'item:true)
                (format t "Office light switched on.~%"))))))
