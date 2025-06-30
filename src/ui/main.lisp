(defpackage :chipi-ui.main
  (:use :cl :clog)
  (:nicknames :ui-main)
  (:import-from #:itemgroupsc
                #:retrieve-itemgroups)
  ;;(:export #:)
  )

(in-package :chipi-ui.main)

(defun on-main (body)
  (load-css (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css")
  (load-css (html-document body) "/custom-styles.css")
  (load-script (html-document body) "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js")
  (load-script (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")

  ;; Add Bootstrap container and header
  (let ((container (create-div body :class "container-fluid mt-4")))
    (create-div container :class "row mb-4"
                :content "<div class=\"col-12\"><h1 class=\"display-4 text-center\">Home Automation Dashboard</h1></div>")
    (let ((itemgroups-row (create-div container :class "row")))
      (map nil (lambda (ig) (render-itemgroup ig itemgroups-row)) (retrieve-itemgroups)))))

(defun render-itemgroup (itemg parent)
  (let* ((col (create-div parent :class "col-lg-6 col-xl-4 mb-4"))
         (card (create-div col :class "card h-100 shadow-sm"))
         (card-header (create-div card :class "card-header bg-primary text-white"))
         (card-body (create-div card :class "card-body p-0")))
    
    (create-element card-header "h5" :class "card-title mb-0"
                    :content (gethash "label" itemg))
    
    (let ((items-list (create-div card-body :class "list-group list-group-flush")))
      (map nil (lambda (item)
                 (render-item item items-list))
           (gethash "items" itemg)))))

(defun render-item (item parent)
  (let* ((list-item (create-div parent :class "list-group-item list-group-item-action"))
         (item-state (gethash "item-state" item))
         (type-hint (gethash "type-hint" item)))
    
    ;; Item header with name and type badge
    (let ((item-header (create-div list-item :class "d-flex justify-content-between align-items-start mb-2")))
      (create-element item-header "h6" :class "mb-1 font-weight-bold text-monospace"
                      :content (gethash "name" item))
      (create-element item-header "span" 
                      :class (format nil "badge ~a" (get-type-badge-class type-hint))
                      :content (format-type-hint type-hint)))
    
    ;; Item value - prominent display
    (let ((value-container (create-div list-item :class "mb-2")))
      (create-element value-container "div" :class "h4 mb-1 text-success font-weight-bold"
                      :content (format-value (gethash "value" item-state) type-hint)))
    
    ;; Timestamp - small and muted
    (create-element list-item "small" :class "text-muted"
                    :content (format nil "🕒 ~a" 
                                   (format-timestamp (gethash "timestamp" item-state))))))

(defun format-timestamp (timestamp)
  (local-time:format-rfc1123-timestring nil (local-time:unix-to-timestamp timestamp)))

(defun format-value (value type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) (if value "On" "Off"))
    ((string= "FLOAT" type-hint) (format nil "~f" value))
    ((string= "INTEGER" type-hint) (format nil "~a" value))
    ((string= "STRING" type-hint) (if value value ""))
    (t (if value (format nil "~a" value) ""))))

(defun get-type-badge-class (type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) "badge-success")
    ((string= "FLOAT" type-hint) "badge-info")
    ((string= "INTEGER" type-hint) "badge-warning")
    ((string= "STRING" type-hint) "badge-secondary")
    (t "badge-light")))

(defun format-type-hint (type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) "Switch")
    ((string= "FLOAT" type-hint) "Decimal Number")
    ((string= "INTEGER" type-hint) "Whole Number")
    ((string= "STRING" type-hint) "String")
    (t "Undefined type")))

(defun start-main ()
  (initialize 'on-main
              :static-root (merge-pathnames "./ui/static-files/")
              :extended-routing t))
