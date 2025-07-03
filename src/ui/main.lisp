(defpackage :chipi-ui.main
  (:use :cl :clog)
  (:nicknames :ui-main)
  (:import-from #:itemsc
                #:update-item-value)
  (:import-from #:itemgroupsc
                #:retrieve-itemgroups)
  ;;(:export #:)
  )

(in-package :chipi-ui.main)

(defvar *item-value-form-update-funs* (make-hash-table :test #'equal))

(defun set-on-value-update (item-name fun)
  (setf (gethash item-name *item-value-form-update-funs*) fun))

(defun call-item-value-update-fun (item-name updated-value)
  (funcall (gethash item-name *item-value-form-update-funs*) updated-value))

(defun on-main (body)
  (log:info "Rendering main...")
  (load-css (html-document body)
            "/custom-styles.css")
  (load-script (html-document body)
               "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

  (let ((container (create-div body :class "container")))
    (create-div container :class "header-line" 
                          :content "Chipi Home Automation Dashboard")
    (let ((itemgroups-container (create-div container :class "itemgroups-container")))
      (map nil (lambda (ig) (render-itemgroup ig itemgroups-container)) (retrieve-itemgroups)))))

(defun render-itemgroup (itemg parent)
  (let* ((col-div (create-div parent :class "itemgroup-column"))
         (card-div (create-div col-div :class "itemgroup-card")))
    (create-div card-div :class "itemgroup-header"
                         :content (gethash "label" itemg))
    
    (let ((items-container (create-div card-div :class "items-container")))
      (map nil (lambda (item)
                 (render-item item items-container))
           (gethash "items" itemg)))))

(defun render-item (item parent)
  (let* ((item-div (create-div parent :class "item-container"))
         (item-state (gethash "item-state" item))
         (item-name (gethash "name" item))
         (type-hint (gethash "type-hint" item)))
    
    (create-div item-div :class "item-name"
                         :content item-name)
    
    (create-div item-div :class (format nil "item-type-badge ~a" (string-downcase type-hint))
                         :content (format-type-hint type-hint))
    
    (render-item-value item-name (gethash "value" item-state) type-hint item-div)

    (create-div item-div :class "item-timestamp-display"
                         :content (format-timestamp (gethash "timestamp" item-state)))))

(defun render-item-value (item-name item-value type-hint parent)
  (cond
    ((string= "BOOLEAN" type-hint)
     (let* ((form-check (create-div parent :class "item-value-boolean"))
            (toggle-input (create-form-element form-check "checkbox"
                                               :role "switch"
                                               :class "item-value-boolean-input")))
       (set-on-value-update item-name
                            (lambda (updated-value)
                              (log:debug "Setting value: ~a on component: ~a"
                                         updated-value toggle-input)
                              (setf (checkedp toggle-input) updated-value) nil t))
       (setf (checkedp toggle-input) item-value)
       (set-on-change toggle-input
                      (lambda (obj)
                        (let ((current-state (checkedp obj)))
                          (format t "Toggled (change): ~a = ~a~%" obj current-state)
                          (update-item-value item-name current-state)
                          )))))
    (t
     (create-div parent :class "item-value-display"
                        :content (format-value item-value type-hint)))))

(defun format-timestamp (timestamp)
  (local-time:format-rfc1123-timestring nil (local-time:unix-to-timestamp timestamp)))

(defun format-value (value type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) (if value "On" "Off"))
    ((string= "FLOAT" type-hint) (format nil "~f" value))
    ((string= "INTEGER" type-hint) (format nil "~a" value))
    ((string= "STRING" type-hint) (if value value ""))
    (t (if value (format nil "~a" value) ""))))


(defun format-type-hint (type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) "Switch")
    ((string= "FLOAT" type-hint) "Decimal Number")
    ((string= "INTEGER" type-hint) "Whole Number")
    ((string= "STRING" type-hint) "String")
    (t "Undefined type")))

(defvar *item-change-listener* nil)

(defun item-listener-receive (msg)
  (typecase msg
    (item:item-changed-event
     (let ((item (item:item-changed-event-item msg)))
       (format t "Item changed: ~a~%" item)
       (call-item-value-update-fun
        (item:name item)
        (gethash "value" (gethash "item-state" (item-ext:item-to-ht item)))))
       )))

(defun start-main ()
  (let ((system-root (merge-pathnames "ui/static-files/"
                                      (asdf:system-source-directory :chipi))))
    (format t "Root: ~a~%" system-root)

    (unless *item-change-listener*
      (setf *item-change-listener*
            (ac:actor-of (isys:ensure-isys)
                         :name "ui-item-change-listener"
                         :init (lambda (self)
                                 (ev:subscribe self self 'item:item-changed-event))
                         :receive (lambda (msg) (item-listener-receive msg)))))

    (setf *item-value-form-update-funs* (make-hash-table :test #'equal))
    
    (initialize 'on-main
                :static-root system-root
                :extended-routing t)))
