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

  (let ((container (create-div body :class "container")))
    (create-element container "h1" :content "Home Automation Dashboard")
    (let ((itemgroups-container (create-div container :class "itemgroups")))
      (map nil (lambda (ig) (render-itemgroup ig itemgroups-container)) (retrieve-itemgroups)))))

(defun render-itemgroup (itemg parent)
  (let ((itemgroup-div (create-div parent :class "itemgroup")))
    (create-element itemgroup-div "h2" :class "itemgroup-title"
                    :content (gethash "label" itemg))
    
    (let ((items-container (create-div itemgroup-div :class "items")))
      (map nil (lambda (item)
                 (render-item item items-container))
           (gethash "items" itemg)))))

(defun render-item (item parent)
  (let* ((item-div (create-div parent :class "item"))
         (item-state (gethash "item-state" item))
         (type-hint (gethash "type-hint" item)))
    
    (create-element item-div "div" :class "item-name"
                    :content (gethash "name" item))
    
    (create-element item-div "span" :class (format nil "item-type ~a" (string-downcase type-hint))
                    :content (format-type-hint type-hint))
    
    (create-element item-div "div" :class "item-value"
                    :content (format-value (gethash "value" item-state) type-hint))
    
    (create-element item-div "p" :class "item-timestamp"
                    :content (format-timestamp (gethash "timestamp" item-state)))))

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
  (initialize 'on-main
              :static-root (merge-pathnames "./ui/static-files/")
              :extended-routing t))
