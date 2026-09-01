(defpackage :chipi-ui.webapp
  (:use :cl)
  (:nicknames :ui-webapp)
  (:export #:+app-name+
           #:+manifest-path+
           #:+manifest+
           #:manifest-middleware))

(in-package :chipi-ui.webapp)

;;; What makes the UI installable as a web app -- on an iPhone or iPad via
;;; "Add to Home Screen", on a Mac via Safari's "Add to Dock", on Android via
;;; Chrome's install prompt.
;;;
;;; The manifest's `start_url' is always "/", never the configured home page:
;;; the launch URL is baked into the install, so a manifest naming a concrete
;;; page would freeze that choice until the app is installed again.  Instead the
;;; app always launches "/" and the server decides what "/" shows, which makes
;;; the setting in `chipi-ui.settings' take effect on the next launch.

(defparameter +app-name+ "Chipi"
  "The installed app's name: its home screen label and the UI's brand.")

(defparameter +manifest-path+ "/manifest.webmanifest"
  "URL path the web app manifest is served at.")

(defparameter +manifest+
  "{
  \"id\": \"/\",
  \"name\": \"Chipi\",
  \"short_name\": \"Chipi\",
  \"description\": \"Chipi home automation dashboard\",
  \"start_url\": \"/\",
  \"scope\": \"/\",
  \"display\": \"standalone\",
  \"orientation\": \"any\",
  \"background_color\": \"#ffffff\",
  \"theme_color\": \"#0d6efd\",
  \"icons\": [
    { \"src\": \"/icons/icon-192.png\", \"sizes\": \"192x192\", \"type\": \"image/png\", \"purpose\": \"any\" },
    { \"src\": \"/icons/icon-512.png\", \"sizes\": \"512x512\", \"type\": \"image/png\", \"purpose\": \"any\" },
    { \"src\": \"/icons/icon-512.png\", \"sizes\": \"512x512\", \"type\": \"image/png\", \"purpose\": \"maskable\" }
  ]
}
"
  "The web app manifest, served as a document rather than as a static file:
`trivial-mimes' has no type for the .webmanifest extension and would hand the
browser application/octet-stream, which is not a manifest.")

(defun manifest-middleware (app)
  "Lack middleware answering `+manifest-path+' with `+manifest+'.

Wraps CLOG's own handler (which serves the boot file, the websocket route and
the static files) and passes everything else straight through."
  (lambda (env)
    (if (string= +manifest-path+ (getf env :path-info))
        (list 200
              (list :content-type "application/manifest+json"
                    :cache-control "no-cache")
              (list +manifest+))
        (funcall app env))))
