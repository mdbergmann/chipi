(defpackage :chipi.binding.knx
  (:use :cl :binding :knxc :knx-conn.address :knx-conn.dpt)
  (:nicknames :binding-knx)
  (:export #:knx-binding
           #:group-address
           #:dpt-type))

(in-package :chipi.binding.knx)

(defclass knx-binding (binding)
  ((ga :initarg :ga
       :initform (error "Group-address must exist!")
       :reader group-address)
   (dpt :initarg :dpt
        :initform (error "DPT type required!")
        :reader dpt-type)))

(defun %make-knx-binding (&key ga dpt)
  (make-instance 'knx-binding
                 :ga (make-group-address ga)
                 :dpt (value-type-string-to-symbol dpt)))

(defmacro knx-binding (&rest args)
  `(%make-knx-binding ,@args))
