(defpackage :cl-eta.solar-if
  (:use :cl)
  (:nicknames :solar-if)
  (:export #:read-power))

(in-package :cl-eta.solar-if)

(defparameter *solar-url* "http://192.168.50.222/rpc")

(defun read-power ()
  (multiple-value-bind (body stat)
      (drakma:http-request
       *solar-url* 
       :method :post
       :content "{\"id\":1,\"method\":\"Switch.GetStatus\",\"params\":{\"id\":0}}")
    (case stat
      (200 (let* ((json (com.inuoe.jzon:parse (babel:octets-to-string body)))
                  (apower (gethash "apower" (gethash "result" json))))
             (values :ok apower)))
      (otherwise (values :nok stat)))))
