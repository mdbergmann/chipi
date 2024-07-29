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
  (lambda (req)
    (let* ((cemi (tunnelling:tunnelling-request-cemi req))
           (req-ga (cemi:cemi-destination-addr cemi)))
      (log:debug "Received request for ga: ~a, required: ~a" req-ga ga)
      (when (and (equalp req-ga ga)
                 (eql (tunnelling:tunnelling-cemi-message-code req)
                      cemi:+cemi-mc-l_data.ind+)
                 (cemi:apci-gv-write-p (cemi:cemi-apci cemi)))
        (handler-case
            (let* ((cemi-data (cemi:cemi-data cemi))
                   (dpt (etypecase cemi-data
                          (dpt
                           cemi-data)
                          ((vector octet)
                           (parse-to-dpt dpt-type
                                         cemi-data))))
                   (value (dpt:dpt-value dpt)))
              (log:info "Write value: ~a for ga: ~a"
                        dpt ga)
              ;; set item value
              (let ((converted-value
                      (cond
                        ((eq dpt-type 'dpt:dpt-1.001)
                         (cond
                           ((eq :on value) 'item:true)
                           ((eq :off value) 'item:false)))
                        (t value))))
                (loop :for item :in (binding::bound-items binding)
                      :do (progn
                            (log:debug "Setting on item: ~a" item)
                            (item:set-value item converted-value)))))
          (error (e)
            (log:error "Error in listener-fun: ~a" e)))))))

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
