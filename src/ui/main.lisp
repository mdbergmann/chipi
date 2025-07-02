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

(defun on-main (body)
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

(defun start-main ()
  (let ((system-root (merge-pathnames "ui/static-files/"
                                      (asdf:system-source-directory :chipi))))
    (format t "Root: ~a~%" system-root)
    (initialize 'on-main
                :static-root system-root
                :extended-routing t)))
