(defpackage :chipi.binding.knx
  (:use :cl :binding :knxc :knx-conn.address :knx-conn.dpt :knx-conn.knx-obj)
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

(defun %make-listener-fun (binding ga dpt-type)
  (flet ((assert-ga (req requested-ga)
           (let* ((cemi (tunnelling:tunnelling-request-cemi req))
                  (ga (cemi:cemi-destination-addr cemi)))
             (log:debug "Received request for ga: ~a, required: ~a" ga requested-ga)
             (unless (equalp ga requested-ga)
               (error "Not required ga!"))))
         (assert-mc (req mc-type)
           (unless (eql (tunnelling:tunnelling-cemi-message-code req) mc-type)
             (error "Not required mc!")))
         (assert-apci (req apci-type)
           (let ((cemi (tunnelling:tunnelling-request-cemi req)))
             (unless (typep (cemi:cemi-apci cemi) apci-type)
               (error "Not required apci!"))))
         (coerce-dpt (req dpt-type)
           (let* ((cemi (tunnelling:tunnelling-request-cemi req))
                  (cemi-data (cemi:cemi-data cemi))
                  (dpt (etypecase cemi-data
                         (dpt cemi-data)
                         ((vector octet) (parse-to-dpt dpt-type cemi-data))))
                  (value (dpt:dpt-value dpt)))
             (log:info "Write value: ~a for ga: ~a" dpt ga)
             value))
         (convert-1.001-to-item-bool (value dpt-type)
           (cond
             ((eq dpt-type 'dpt:dpt-1.001)
              (case value
                (:on 'item:true)
                (:off 'item:false)))
             (t value))))
    (lambda (req)
      (handler-case
          (progn
            (assert-ga req ga)
            (assert-mc req cemi:+cemi-mc-l_data.ind+)
            (assert-apci req 'cemi:apci-gv-write)
            (let ((value (convert-1.001-to-item-bool
                          (coerce-dpt req dpt-type) dpt-type)))
              (log:debug "Setting on items...")
              (dolist (item (binding::bound-items binding))
                (log:debug "Setting on item: ~a" item)
                (item:set-value item value :push nil))))
        (error (e)
          (log:error "Error in listener-fun: ~a" e))))))

(defun %make-knx-binding (&key ga dpt)
  (let* ((ga-obj (make-group-address ga))
         (dpt-type (value-type-string-to-symbol dpt))
         (binding (make-instance 'knx-binding
                                 :ga ga-obj
                                 :dpt dpt-type)))
    (knx-client:add-tunnelling-request-listener
     (%make-listener-fun binding ga-obj dpt-type))
    binding))

(defmacro knx-binding (&rest args)
  `(%make-knx-binding ,@args))
