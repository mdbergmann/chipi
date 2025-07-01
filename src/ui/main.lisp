(defpackage :chipi-ui.main
  (:use :cl :clog)
  (:nicknames :ui-main)
  (:import-from #:itemgroupsc
                #:retrieve-itemgroups)
  ;;(:export #:)
  )

(in-package :chipi-ui.main)

(defun on-main (body)
  (load-css (html-document body)
            "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css")
  (load-css (html-document body)
            "/custom-styles.css")
  (load-script (html-document body)
               "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

  (let ((container (create-div body :class "container")))
    (create-div container :class "display-4 fw-light text-center mb-4 text-dark" 
                :content "Chipi Home Automation Dashboard")
    (let ((itemgroups-container (create-div container :class "row g-3")))
      (map nil (lambda (ig) (render-itemgroup ig itemgroups-container)) (retrieve-itemgroups)))))

(defun render-itemgroup (itemg parent)
  (let* ((col-div (create-div parent :class "col-lg-4 col-md-6 col-sm-12"))
         (card-div (create-div col-div :class "card h-100")))
    (create-div card-div :class "card-header bg-primary text-white"
                :content (gethash "label" itemg))
    
    (let ((items-container (create-div card-div :class "list-group list-group-flush")))
      (map nil (lambda (item)
                 (render-item item items-container))
           (gethash "items" itemg)))))

(defun render-item (item parent)
  (let* ((item-div (create-div parent :class "list-group-item position-relative"))
         (item-state (gethash "item-state" item))
         (item-name (gethash "name" item))
         (type-hint (gethash "type-hint" item)))
    
    (create-div item-div :class "item-name mb-2"
                         :content item-name)
    
    (create-div item-div :class (format nil "badge item-type ~a" (string-downcase type-hint))
                         :content (format-type-hint type-hint))
    
    (render-item-value item-name (gethash "value" item-state) type-hint item-div)

    (create-div item-div :class "item-timestamp text-muted small"
                         :content (format-timestamp (gethash "timestamp" item-state)))))

(defun render-item-value (item-name item-value type-hint parent)
  (cond
    ((string= "BOOLEAN" type-hint)
     (let* ((form-check (create-div parent :class "form-check form-switch mb-2"))
            (toggle-input (create-form-element form-check "checkbox"
                                               :role "switch"
                                               :class "form-check-input")))
       (setf (checkedp toggle-input) item-value)
       (set-on-change toggle-input
                      (lambda (obj)
                        (let ((current-state (checkedp obj)))
                          (format t "Toggled (change): ~a = ~a~%" obj current-state)
                          (itemsc:update-item-value item-name current-state)
                          )))))
    (t
     (create-div parent :class "item-value mb-2"
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
