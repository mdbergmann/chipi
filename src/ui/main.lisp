(defpackage :chipi-ui.main
  (:use :cl :clog)
  (:nicknames :ui-main)
  (:import-from #:ui-renderer
                #:make-nav-context
                #:nav-context-body
                #:nav-context-depth
                #:render-page
                #:render-home
                #:render-settings
                #:render-not-found
                #:call-item-value-update-fun)
  (:export #:start-main
           #:shutdown-main))

(in-package :chipi-ui.main)

(defun on-main (body)
  "Entry handler for every new connection.  Dispatches the requested URL path
to a `page:defpage'd page, to the auto-generated itemgroup overview (root path
when no page claims it), or to a not-found view.  Rendering is a pure function
of the current URL path: in-app navigation pushes history entries and browser
back/forward re-renders via the popstate handler."
  (log:info "Rendering main, path: ~a" (path-name (location body)))
  (load-css (html-document body)
            "/custom-styles.css")
  (load-css (html-document body)
            "/page-styles.css")
  ;; from static-files rather than from a CDN: installed as a web app the
  ;; dashboard has to come up on a LAN that has no way out to the internet
  (load-css (html-document body)
            "/vendor/uPlot.min.css")
  (load-script (html-document body)
               "/vendor/bootstrap.bundle.min.js")
  (load-script (html-document body)
               "/vendor/uPlot.iife.min.js")

  (let* ((container (create-div body :class "container"))
         (ctx (make-nav-context :body body :container container)))
    ;; browser back/forward fires popstate with the URL already updated;
    ;; re-render whatever the current path addresses
    (set-on-pop-state (window body)
                      (lambda (obj)
                        (declare (ignore obj))
                        (setf (nav-context-depth ctx)
                              (max 0 (1- (nav-context-depth ctx))))
                        (%dispatch-path ctx)))
    (%dispatch-path ctx)))

(defun %dispatch-path (ctx)
  "Renders the view matching the connection's current URL path.

The root path is resolved before the page registry is consulted: it is the
app's home slot, and which view fills it is the device's setting (see
`chipi-ui.settings') rather than a property of a page."
  (let ((path (page:normalize-path
               (path-name (location (nav-context-body ctx))))))
    (cond
      ((string= ui-settings:+settings-path+ path) (render-settings ctx))
      ((string= "/" path) (render-home ctx))
      (t (let ((matching-page (page:find-page-by-path path)))
           (if matching-page
               (render-page matching-page ctx)
               (render-not-found ctx path)))))))

(defun %register-page-route (page)
  "Registers PAGE's URL path as a CLOG route so that a direct browser visit
(deep link) to it serves the boot file and lands in `on-main'."
  (when (string= ui-settings:+settings-path+ (page:page-path page))
    (log:warn "Page ~a claims ~a, which the built-in settings view serves; ~
the page will not be reachable."
              (page:page-id page) (page:page-path page)))
  (unless (string= "/" (page:page-path page))
    (log:debug "Registering page route: ~a" (page:page-path page))
    (set-on-new-window 'on-main
                       :path (page:page-path page)
                       :boot-file "/boot.html")))

(defparameter *reconnect-delay* 120
  "Seconds the server keeps a closed connection's session alive so that the
browser can reconnect to it.

Installed as a web app the UI is suspended whenever the user switches away, and
the websocket dies with it; the session has to outlive that absence, or coming
back would find the page rendered but dead -- CLOG's client reconnects with the
old connection id, and a server that has already dropped it just closes the
socket again.  The boot page reloads when it detects that, but a reload
re-renders the whole view, so the session is kept for a couple of minutes
first.  Costs one parked thread per closed connection for the duration.")

(defvar *item-change-listener* nil)

(defun %item-listener-receive (msg)
  (typecase msg
    (item:item-changed-event
     (let ((item (item:item-changed-event-item msg)))
       (log:debug "Item changed: ~a" item)
       (call-item-value-update-fun
        (item:name item)
        (gethash "item-state" (item-ext:item-to-ht item)))))))

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

    (setf clog-connection:*reconnect-delay* *reconnect-delay*)
    (setf ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal))

    (initialize 'on-main
                :static-root system-root
                :host host
                :port port
                :extended-routing t
                :lack-middleware-list (list #'ui-webapp:manifest-middleware))

    ;; the settings view is built in rather than a `defpage', so its route is
    ;; registered here and not by the page hook below
    (set-on-new-window 'on-main
                       :path ui-settings:+settings-path+
                       :boot-file "/boot.html")

    ;; routes for pages defined before the UI started ...
    (dolist (p (page:get-pages))
      (%register-page-route p))
    ;; ... and for pages defined (or re-defined) afterwards
    (setf page:*page-registered-hook* #'%register-page-route)))

(defun shutdown-main ()
  "Shuts down and cleans up resources."
  (setf page:*page-registered-hook* nil)
  (shutdown)
  (when *item-change-listener*
    (ac:stop (isys:ensure-isys) *item-change-listener*)
    (setf *item-change-listener* nil)))
