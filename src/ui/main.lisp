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
  (load-css (html-document body) "/custom-styles.css") ; Add your custom CSS
  (load-script (html-document body) "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js")
  (load-script (html-document body) "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js")

  (map nil (lambda (ig) (render-itemgroup ig body)) (retrieve-itemgroups)))

(defun render-itemgroup (itemg parent)
  (let ((itemg-panel (create-div parent :class "itemgroup"
                                        :content (gethash "label" itemg))))
    (map nil (lambda (item)
               (render-item item itemg-panel))
         (gethash "items" itemg))))

(defun render-item (item parent)
  (let ((item-panel (create-div parent
                                :class "item"
                                :content (gethash "label" item))))
    (create-div item-panel :class "item-name"
                           :content (gethash "name" item))
    (create-div item-panel :class "item-type"
                           :content (format-type-hint (gethash "type-hint" item)))
    (let ((item-state (gethash "item-state" item))
          (item-state-panel (create-div item-panel
                                        :class "item-state")))
      (create-div item-state-panel :class "item-value"
                                   :content
                                   (format-value (gethash "value" item-state)
                                                 (gethash "type-hint" item)))
      (create-div item-state-panel :class "item-timestamp"
                                   :content
                                   (format-timestamp
                                    (gethash "timestamp" item-state))))))

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
