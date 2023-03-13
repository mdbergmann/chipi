(defpackage :cl-eta.openhab
  (:use :cl)
  (:nicknames :openhab)
  (:export #:do-post
           #:*openhab-base-url*))

(in-package :cl-eta.openhab)
(defparameter *openhab-base-url* "http://mini.local:8080/rest/items/")

(defun do-post (resource data)
  (log:debug "posting item: ~a, value: ~a" resource data)
  (drakma:http-request (format nil "~a~a" *openhab-base-url* resource)
                       :method :post
                       :content (write-to-string data)
                       :content-type "text/plain")
  :ok)
