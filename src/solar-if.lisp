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
      (200 (let* ((json (yason:parse (babel:octets-to-string body)))
                  (result (gethash "result" json))
                  (apower (gethash "apower" result))
                  (total (gethash "total" (gethash "aenergy" result))))
             (values :ok apower total)))
      (otherwise (values :nok stat)))))
