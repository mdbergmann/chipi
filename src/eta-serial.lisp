(defpackage :etaconnector.serial
  (:use :cl)
  (:nicknames :eta-ser)
  (:export #:*serial*
           #:open-serial
           #:close-serial
           #:configure-serial
           #:write-serial
           #:read-serial))

(in-package :etaconnector.serial)

(defparameter *shell* nil)

(defun write-shell (command)
  (let* ((out-stream (uiop:process-info-input *shell*))
         (written (write-line command out-stream)))
    (force-output out-stream)
    written))

(defun read-shell ()
  (coerce (loop
            :with stream = (uiop:process-info-output *shell*)
            :while (listen stream)
            :collect (read-char stream))
          'string))

(defun make-shell ()
  (setf *shell* (uiop:launch-program "bash" :input :stream :output :stream)))

(defun close-shell ()
  (uiop:close-streams *shell*))

(defmacro with-shell (shell &body body)
  `(let ((*shell* ,shell))
    ,@body))

;; serial

;; (defparameter *serial-rid* nil)
;; (defparameter *serial-wid* nil)

;; (defun open-serial (read-id write-id)
;;   (setf *serial-rid* read-id)
;;   (setf *serial-wid* write-id)
;;   (write-shell (format nil "exec ~a<~a" *serial-rid* *serial*))
;;   (write-shell (format nil "exec ~a>~a" *serial-wid* *serial*)))

;; (defun close-serial ()
;;   (write-shell (format nil "exec ~a<&-" *serial-rid*))
;;   (write-shell (format nil "exec ~a<&-" *serial-wid*)))

;; (defun configure-serial (&optional (speed 19200))
;;   (write-shell (format nil "stty -f ~a raw speed ~a" *serial* speed)))

;; (defun write-serial (text)
;;   (write-shell (format nil "echo -n '~a' >&~a" text *serial-wid*)))

;; (defun read-serial (&optional (timeout 2.0))
;;   (write-shell (format nil "IFS= read -u ~a -d '}' -r; echo -n $REPLY" *serial-rid*))
;;   (loop :with start-time = (get-universal-time)
;;         :for out-string = (read-shell)
;;         :for curr-time = (get-universal-time)
;;         :for duration = (- curr-time start-time)
;;         :until (or
;;                 (> (length out-string) 0)
;;                 (> duration timeout))
;;         :do (progn
;;               ;;(format t "len: ~a, dur: ~a~%" (length out-string) duration)
;;               (sleep .2))
;;         :finally (return out-string)))

;;; libserialport

(defparameter *serial* "/dev/cu.usbserial-143340")
(defparameter *serport* nil)
(defun open-serial (&optional (speed 19200))
  (setf *serport*
        (libserialport:open-serial-port
         *serial*
         :baud speed :bits 8 :stopbits 1 :parity :sp-parity-none
         :rts :sp-rts-off
         :flowcontrol :sp-flowcontrol-none)))

(defun read-serial ()
  (libserialport:serial-read-octets-until
   *serport*
   #\}
   :timeout 2000))

(defun close-serial ()
  (libserialport:shutdown-serial-port *serport*))

(defun write-serial (data)
  (libserialport:serial-write-data *serport* data))

(defun receive (actor msg state)
  ;;(lf:lwarn "received: ~a" msg)
  (case (car msg)
    (:init
     (progn
       (open-serial)))
    (:read
     (progn
       (let ((read (read-serial)))
         (when (> (length read) 0)
           (format t "read: ~a~%" read)
           (format t "read string: ~a~%" (babel:octets-to-string read))))
       (act:tell actor msg)))
    (:write
     (write-serial (cdr msg)))
    (:close
     (progn
       (close-serial))))
  (cons nil state))

(defvar *asys* (asys:make-actor-system))
(defparameter *serial-act* (ac:actor-of
                            *asys*
                            :receive (lambda (a b c) (receive a b c))))


#|

CL-USER> (ql:quickload :libserialport)
To load "libserialport":
  Load 1 ASDF system:
    libserialport
; Loading "libserialport"

(:LIBSERIALPORT)
CL-USER> (ql:quickload :cl-gserver)
To load "cl-gserver":
  Load 1 ASDF system:
    cl-gserver
; Loading "cl-gserver"

(:CL-GSERVER)
;Compiling "/Users/mbergmann/Development/MySources/cl-eta/src/eta-serial.lisp"...
CL-USER> (in-package :eta-ser)
#<Package "ETACONNECTOR.SERIAL">
SERIAL> *serial*
"/dev/cu.usbserial-143340"
SERIAL> (*serial-act*)
; Debugger entered on #<CCL::UNDEFINED-FUNCTION-CALL #x3020033BDEFD>
[1] SERIAL> 
; Evaluation aborted on #<CCL::UNDEFINED-FUNCTION-CALL #x30200324860D>
SERIAL> *serial-act*
#<ACTOR path: /user/actor-27289, cell: #<ACTOR actor-27289, running: T, state: NIL, message-box: #<MESSAGE-BOX/DP mesgb-27290, processed messages: 0, max-queue-size: 0, queue: #<QUEUE-UNBOUNDED #x302002FDA20D>>>>
SERIAL> (act:tell *serial-act* '(:init . nil))
T
SERIAL> (act:tell *serial-act* '(:write . "Hello World"))
T
SERIAL> (act:tell *serial-act* '(:read . nil))
T
SERIAL> 
read: #(13)
read string: 
read: #(104 101 108)
read string: hel
read: #(108 111 32 102 114 111 109 32 116 101 114 109)
read string: lo from term
read: #(105 110 97 108)
read string: inal
read: #(125)
read string: }
; No values
SERIAL> 
read: #(104 97 108 108)
read string: hall
; No values
read: #(111)
read string: o
SERIAL> 
read: #(125)
read string: }
; No values
SERIAL> 
; No values
read: #(100 102 103)
read string: dfg
SERIAL> 
; No values
SERIAL> 
; No values
SERIAL> 
; No values
SERIAL> 
; No values
SERIAL> 
read: #(100 103)
read string: dg
; No values
SERIAL> 
read: #(100 103 125)
read string: dg}
; No values
SERIAL>

|#
