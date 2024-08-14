(defpackage :chipi.binding.knx
  (:use :cl :binding :knxc :knx-conn.address :knx-conn.dpt :knx-conn.knx-obj)
  (:nicknames :binding-knx)
  (:export #:knx-binding
           #:knx-init
           #:knx-shutdown))

(in-package :chipi.binding.knx)

(defclass knx-binding (binding)
  ((ga :initarg :ga
       :initform (error "Group-address must exist!")
       :reader group-address)
   (dpt :initarg :dpt
        :initform (error "DPT type required!")
        :reader dpt-type)))

(defun %convert-1.001-to-item-bool (value dpt-type)
  (cond
    ((eq dpt-type 'dpt:dpt-1.001)
     (case value
       (:on 'item:true)
       (:off 'item:false)))
    (t value)))

(defun %convert-item-bool-to-1.001 (value dpt-type)
  "Converts between `item:true'/`item:false' and `:on'/`:off' as `knxc:write-value' wants it."
  (cond
    ((eq dpt-type 'dpt:dpt-1.001)
     (cond 
       ((eq value 'item:true) t)
       ((eq value 'item:false) nil)))
    (t value)))

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
             (log:info "Indicated value: ~a for ga: ~a" dpt ga)
             value)))
    (lambda (req)
      (handler-case
          (progn
            (log:debug "KNX tunnel listener received: ~a" req)
            (assert-ga req ga)
            (assert-mc req cemi:+cemi-mc-l_data.ind+)
            (assert-apci req 'cemi:apci-gv-write)
            (let ((value (%convert-1.001-to-item-bool
                          (coerce-dpt req dpt-type) dpt-type))
                  (items (binding::bound-items binding)))
              (log:debug "Setting on items (~a)..." (length items))
              (dolist (item items)
                (log:debug "Setting on item: ~a" item)
                (item:set-value item value :push nil))))
        (error (e)
          (log:error "Error in listener-fun: ~a" e))))))

(defun %make-binding-pull-fun (binding ga-obj dpt-type)
  (lambda ()
    (future:fcompleted
        (knxc:request-value (address:address-string-rep ga-obj)
                            dpt-type)
        (result)
      (cond
        ((null result)
         (log:warn "Timed out!"))
        ((typep result 'error)
         (log:warn "Error: ~a" result))
        (t
         (let ((value (%convert-1.001-to-item-bool result dpt-type)))
           (log:info "Received value: ~a" value)
           (dolist (item (binding::bound-items binding))
             (log:debug "Setting on item: ~a" item)
             (item:set-value item value :push nil)))))
      )))

(defun %make-binding-push-fun (ga-obj dpt-type)
  (lambda (value)
    (let ((converted-value (%convert-item-bool-to-1.001 value dpt-type)))
      (log:info "Writing value: ~a to: ~a" value ga-obj)
      ;; wants `t' and `nil' for 1.001
      (knxc:write-value ga-obj dpt-type converted-value))))

(defun %make-knx-binding (&rest other-args &key ga dpt &allow-other-keys)
  (let* ((ga-obj (make-group-address ga))
         (dpt-type (value-type-string-to-symbol dpt))
         (binding (make-instance 'knx-binding
                                 :ga ga-obj
                                 :dpt dpt-type
                                 :initial-delay (getf other-args :initial-delay 2))))
    (assert ga-obj nil "Unable to make group-address object!")
    (assert dpt-type nil "Unable to parse dpt-type!")
    (setf (binding::pull-fun binding)
          (%make-binding-pull-fun binding ga-obj dpt-type))
    (setf (binding::push-fun binding)
          (%make-binding-push-fun ga-obj dpt-type))
    (knx-client:add-tunnelling-request-listener
     (%make-listener-fun binding ga-obj dpt-type))
    binding))

;; -----------------------------
;; Public API
;; -----------------------------

(defmacro knx-binding (&rest other-args &key ga dpt &allow-other-keys)
  "Creates a knx-binding.

Relevant arguments:
- `ga': group-address in string representation like '1/2/3'
- `dpt': dpt-type string, i.e. '1.001'

Creating the binding expects an initialized knx-conn environment.
The binding will pull the value from the ga initially with a 2 seconds delay.
Delay can be overriden by specifying `:initial-delay' in full seconds."
  `(progn
     (assert (typep ,ga 'string) nil "Parameter ga must be string!")
     (assert (typep ,dpt 'string) nil "Parameter dpt must be string!")
     (log:info "Make knx binding...")
     (%make-knx-binding :ga ,ga :dpt ,dpt ,@other-args)))

(defun knx-init (&key gw-host (gw-port 3671))
  "Config and initialize KNX binding.
This should be as part of `hab:defconfig'.

`gw-host': host or UP address to a KNXNet/IP router/gateway.
`gw-port': port to the gateway, default 3671."
  (knxc:knx-conn-init gw-host
                      :port gw-port))

(defun knx-shutdown ()
  "Shutdown KNX binding and release/clean all resources."
  (knxc:knx-conn-destroy))
