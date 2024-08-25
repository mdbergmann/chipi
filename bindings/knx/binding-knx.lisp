(defpackage :chipi.binding.knx
  (:use :cl :binding :knxc :knx-conn.address :knx-conn.dpt :knx-conn.knx-obj)
  (:nicknames :binding-knx)
  (:import-from #:alexandria
                #:remove-from-plist)
  (:export #:knx-binding
           #:knx-init
           #:knx-shutdown))

(in-package :chipi.binding.knx)

(defclass knx-binding (binding)
  ((ga-read :initarg :ga-read
            :initform (error "Read group-address required!")
            :reader group-address-read)
   (ga-write :initarg :ga-write
             :initform (error "Write group-address required!")
             :reader group-address-write)
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

(defun %make-ind-write-listener-fun (binding ga dpt-type)
  (flet ((assert-ga (req requested-ga)
           (let* ((cemi (tunnelling:tunnelling-request-cemi req))
                  (ga (cemi:cemi-destination-addr cemi)))
             (log:debug "Received request for ga: ~a, required: ~a" ga requested-ga)
             (unless (equalp ga requested-ga)
               (error "GA not of required value!"))))
         (assert-mc (req mc-type)
           (unless (eql (tunnelling:tunnelling-cemi-message-code req) mc-type)
             (error "MC not of required value!")))
         (assert-apci (req apci-type)
           (let ((cemi (tunnelling:tunnelling-request-cemi req)))
             (unless (typep (cemi:cemi-apci cemi) apci-type)
               (error "APCI not of required value!"))))
         (coerce-dpt (req dpt-type)
           (let* ((cemi (tunnelling:tunnelling-request-cemi req))
                  (cemi-data (cemi:cemi-data cemi))
                  (dpt (etypecase cemi-data
                         (dpt cemi-data)
                         ((vector octet) (parse-to-dpt dpt-type cemi-data))))
                  (value (dpt:dpt-value dpt)))
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
              (log:info "Indicated value: ~a for ga: ~a" value ga)
              (log:debug "Setting on items (~a)..." (length items))
              (dolist (item items)
                (log:debug "Setting on item: ~a" item)
                (item:set-value item value :push nil))))
        (error (e)
          (log:debug "From listener-fun: ~a" e))))))

(defun %make-binding-pull-fun (ga-obj dpt-type)
  (lambda ()
    (let ((fut
            (future:fmap
                (knxc:request-value (address:address-string-rep ga-obj)
                                    dpt-type)
                (result)
              (cond
                ((null result)
                 (error "Timed out!"))
                ((typep result 'error)
                 (error "Error: ~a" result))
                (t
                 (let ((value (%convert-1.001-to-item-bool result dpt-type)))
                   (log:info "Received value: ~a" value)
                   value))))))
      (values 
       fut
       '(:push nil)))))

(defun %make-binding-push-fun (ga-obj dpt-type)
  (lambda (value)
    (let ((converted-value (%convert-item-bool-to-1.001 value dpt-type)))
      (log:info "Writing to bus, value: ~a ga: ~a, dpt: ~a" value ga-obj dpt-type)
      ;; wants `t' and `nil' for 1.001
      (knxc:write-value (address:address-string-rep ga-obj) dpt-type converted-value))))

(defun %make-knx-binding (&rest other-args &key ga dpt &allow-other-keys)
  (let* ((ga-read-obj (if (stringp ga)
                          (make-group-address ga)
                          (make-group-address (getf ga :read))))
         (ga-write-obj (if (stringp ga)
                           (make-group-address ga)
                           (make-group-address (getf ga :write))))
         (dpt-type (value-type-string-to-symbol dpt))
         (rest-args (remove-from-plist other-args :ga :dpt))
         (binding (apply #'make-instance 'knx-binding
                         :ga-read ga-read-obj
                         :ga-write ga-write-obj
                         :dpt dpt-type
                         :initial-delay (getf other-args :initial-delay 2)
                         :push-fun (%make-binding-push-fun ga-write-obj dpt-type)
                         :pull-fun (%make-binding-pull-fun ga-read-obj dpt-type)
                         rest-args)))
    (assert (and ga-read-obj ga-write-obj) nil "Unable to make group-address objects!")
    (assert dpt-type nil "Unable to parse dpt-type!")
    (knx-client:add-tunnelling-request-listener
     (%make-ind-write-listener-fun binding ga-read-obj dpt-type))
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
Delay can be overriden by specifying `:initial-delay' in full seconds.

`other-args' will be forwarded to the `base-binding' constructor. So things like `:call-push-p' and `:delay' also work here. However, be careful with `:push' and `:pull'. Using them redefine the behavior of the knx-binding.
In particular, `:call-push-p' allows to forward item value changes which come from other places than the KNX bus
to push to the bus."
  `(progn
     (assert (or (stringp ,ga)
                 (listp ,ga)) nil "Parameter ga must be string or a plist with :read and :write keys!")
     (assert (typep ,dpt 'string) nil "Parameter dpt must be string!")
     (log:info "Make knx binding...")
     (%make-knx-binding :ga ,ga :dpt ,dpt ,@other-args)))

(defun knx-init (&key gw-host (gw-port 3671))
  "Config and initialize KNX binding.
This should be as part of `hab:defconfig'.
A shutdown hook is added via `hab:add-to-shutdown' which calls `knx-shutdown'.

`gw-host': host or UP address to a KNXNet/IP router/gateway.
`gw-port': port to the gateway, default 3671."
  (hab:add-to-shutdown #'knx-shutdown)
  (knxc:knx-conn-init gw-host
                      :port gw-port))

(defun knx-shutdown ()
  "Shutdown KNX binding and release/clean all resources.
Be aware that the global shutdown function (`hab:shutdown') will also call this so this usually doesn't need to be called manually except in test setups."
  (knxc:knx-conn-destroy))
