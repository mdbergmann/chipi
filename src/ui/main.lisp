(defpackage :chipi-ui.main
  (:use :cl :clog)
  (:nicknames :ui-main)
  (:import-from #:itemsc
                #:update-item-value)
  (:import-from #:itemgroupsc
                #:retrieve-itemgroups
                #:retrieve-top-level-itemgroups)
  (:export #:start-main
           #:shutdown-main)
  )

(in-package :chipi-ui.main)

(defvar *item-value-form-update-funs* (make-hash-table :test #'equal))

(defun set-on-value-update (item-name fun)
  (setf (gethash item-name *item-value-form-update-funs*) fun))

(defun call-item-value-update-fun (item-name updated-item-state)
  (let ((update-fun (gethash item-name *item-value-form-update-funs*)))
    (if update-fun
        (funcall update-fun updated-item-state)
        (log:warn "No update function registered for: ~a" item-name))))

(defun on-main (body)
  "The main page."
  (log:info "Rendering main...")
  (load-css (html-document body)
            "/custom-styles.css")
  (load-script (html-document body)
               "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js")

  (let ((container (create-div body :class "container")))
    (%show-overview container)))

(defun %show-overview (container)
  "Renders the overview page with all itemgroups."
  (setf (inner-html container) "")
  (clrhash *item-value-form-update-funs*)
  (create-div container :class "header-line"
                        :content "Chipi Home Automation Dashboard")
  (let* ((groups (retrieve-top-level-itemgroups))
         (link-groups (remove-if-not #'%itemgroup-link-p groups))
         (card-groups (remove-if #'%itemgroup-link-p groups)))
    ;; Render link groups as a list if any exist
    (when link-groups
      (let ((links-container (create-div container :class "itemgroup-links-container")))
        (dolist (ig link-groups)
          (%render-itemgroup-link ig links-container container
                                  (lambda () (%show-overview container))))))
    ;; Render card groups in the grid
    (when card-groups
      (let ((itemgroups-container (create-div container :class "itemgroups-container")))
        (dolist (ig card-groups)
          (%render-itemgroup ig itemgroups-container))))))

(defun %itemgroup-link-p (itemg)
  "Returns T if the itemgroup should render as a link (has :ui-link tag)."
  (let ((tags (gethash "tags" itemg)))
    (when (and tags (hash-table-p tags))
      (multiple-value-bind (val present-p)
          (gethash :ui-link tags)
        (declare (ignore val))
        present-p))))

(defun %render-itemgroup-link (itemg parent container back-fn)
  "Renders an itemgroup as a clickable link."
  (let ((link-div (create-div parent :class "itemgroup-link"
                                     :content (gethash "label" itemg))))
    (set-on-click link-div
                  (lambda (obj)
                    (declare (ignore obj))
                    (%show-itemgroup-detail itemg container back-fn)))))

(defun %show-itemgroup-detail (itemg container back-fn)
  "Shows a detail page for a single itemgroup with a back button.
Shows child groups (as links or cards) above direct items."
  (setf (inner-html container) "")
  (clrhash *item-value-form-update-funs*)
  (let ((back-btn (create-button container
                                 :class "back-button"
                                 :content "&larr; Back")))
    (set-on-click back-btn
                  (lambda (obj)
                    (declare (ignore obj))
                    (funcall back-fn))))
  ;; Show child groups if any
  (let* ((children (coerce (gethash "children" itemg) 'list))
         (link-children (remove-if-not #'%itemgroup-link-p children))
         (card-children (remove-if #'%itemgroup-link-p children)))
    (when link-children
      (let ((links-container (create-div container :class "itemgroup-links-container")))
        (dolist (child link-children)
          (%render-itemgroup-link child links-container container
                                  (lambda ()
                                    (%show-itemgroup-detail itemg container back-fn))))))
    (when card-children
      (let ((itemgroups-container (create-div container :class "itemgroups-container")))
        (dolist (child card-children)
          (%render-itemgroup child itemgroups-container)))))
  ;; Show direct items as card (only if there are items)
  (let ((items (gethash "items" itemg)))
    (when (and items (> (length items) 0))
      (let ((itemgroups-container (create-div container :class "itemgroups-container")))
        (%render-itemgroup itemg itemgroups-container)))))

(defun %render-itemgroup (itemg parent)
  (let* ((col-div (create-div parent :class "itemgroup-column"))
         (card-div (create-div col-div :class "itemgroup-card")))
    (create-div card-div :class "itemgroup-header"
                         :content (gethash "label" itemg))
    
    (let ((items-container (create-div card-div :class "items-container")))
      (map nil (lambda (item)
                 (%render-item item items-container))
           (gethash "items" itemg)))))

(defun %render-item (item parent)
  (let* ((item-div (create-div parent :class "item-container"))
         (item-label (gethash "label" item))
         (item-name (gethash "name" item))
         (item-state (gethash "item-state" item))
         (type-hint (gethash "type-hint" item)))
    
    (create-div item-div :class "item-label"
                         :content item-label)

    (create-div item-div :class (format nil "item-type-badge ~a" (string-downcase type-hint))
                         :content (%format-type-hint type-hint))

    (create-div item-div :class "item-name"
                         :content item-name)
        
    (%render-item-value item-name (gethash "value" item-state) type-hint item-div)

    (let ((ts-div
            (create-div item-div :class "item-timestamp-display"
                                 :content (%format-timestamp (gethash "timestamp" item-state)))))
      (set-on-value-update item-name
                           (lambda (updated-item-state)
                             (setf (text ts-div)
                                   (%format-timestamp
                                    (gethash "timestamp" updated-item-state))))))))

(defun %render-item-value (item-name item-value type-hint parent)
  (cond
    ((string= "BOOLEAN" type-hint)
     (let* ((form-check (create-div parent :class "item-value-boolean"))
            (toggle-input (create-form-element form-check "checkbox"
                                               :role "switch"
                                               :class "item-value-boolean-input")))
       (set-on-value-update item-name
                            (lambda (updated-item-state)
                              (let ((updated-value (gethash "value" updated-item-state)))
                                (log:debug "Setting value: ~a on component: ~a"
                                           updated-value toggle-input)
                                (setf (checkedp toggle-input) updated-value))))
       (setf (checkedp toggle-input) item-value)
       (set-on-change toggle-input
                      (lambda (obj)
                        (let ((current-state (checkedp obj)))
                          (log:debug "Toggled (change): ~a = ~a" obj current-state)
                          (update-item-value item-name
                                             (item-ext:item-value-ext-to-internal current-state))
                          )))))
    (t
     (let ((value-div (create-div parent :class "item-value-display"
                                         :content (%format-value item-value type-hint))))
       (set-on-value-update item-name
                            (lambda (updated-value)
                              (log:debug "Setting value: ~a on component: ~a"
                                         updated-value value-div)
                              (log:debug "Current value: ~a" (text value-div))
                              (setf (text value-div)
                                    (%format-value updated-value type-hint))))))))

(defun %format-timestamp (timestamp)
  (local-time:format-rfc1123-timestring nil (local-time:unix-to-timestamp timestamp)))

(defun %format-value (value type-hint)
  (cond
    ((string= "FLOAT" type-hint) (format nil "~f" value))
    ((string= "INTEGER" type-hint) (format nil "~a" value))
    ((string= "STRING" type-hint) (if value value ""))
    (t (if value (format nil "~a" value) ""))))


(defun %format-type-hint (type-hint)
  (cond
    ((string= "BOOLEAN" type-hint) "Switch")
    ((string= "FLOAT" type-hint) "Decimal Number")
    ((string= "INTEGER" type-hint) "Whole Number")
    ((string= "STRING" type-hint) "Text")
    (t "Undefined type")))

(defvar *item-change-listener* nil)

(defun %item-listener-receive (msg)
  (typecase msg
    (item:item-changed-event
     (let ((item (item:item-changed-event-item msg)))
       (log:debug "Item changed: ~a" item)
       (call-item-value-update-fun
        (item:name item)
        (gethash "item-state" (item-ext:item-to-ht item))))
       )))

(defun start-main (host port)
  "Starts the CLOG UI and sets up handlers for the pages."
  (let ((system-root (merge-pathnames "ui/static-files/"
                                      (asdf:system-source-directory :chipi))))
    (log:info "UI static root: ~a" system-root)

    (unless *item-change-listener*
      (setf *item-change-listener*
            (ac:actor-of (isys:ensure-isys)
                         :name "ui-item-change-listener"
                         :init (lambda (self)
                                 (ev:subscribe self self 'item:item-changed-event))
                         :receive (lambda (msg) (%item-listener-receive msg)))))

    (setf *item-value-form-update-funs* (make-hash-table :test #'equal))
    
    (initialize 'on-main
                :static-root system-root
                :host host
                :port port
                :extended-routing t)))

(defun shutdown-main ()
  "Shuts down and cleans up resources."
  (shutdown)
  (when *item-change-listener*
    (ac:stop (isys:ensure-isys) *item-change-listener*)
    (setf *item-change-listener* nil)))
