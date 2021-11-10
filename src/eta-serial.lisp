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

;; serial

(defparameter *serial* "/dev/cu.usbserial-143340")

(defun open-serial ()
  (write-shell (format nil "exec 3<>~a" *serial*)))

(defun close-serial ()
  (write-shell "exec 3<&-"))

(defun configure-serial ()
  (write-shell (format nil "stty -f ~a raw speed 19200" *serial*)))

(defun write-serial (text)
  (write-shell (format nil "echo -n '~a' > ~a" text *serial*)))

(defun read-serial (&optional (timeout 2.0))
  (write-shell (format nil "IFS= read -u 3 -d '}' -r; echo -n $REPLY"))
  (loop :with start-time = (get-universal-time)
        :for out-string = (read-shell)
        :for curr-time = (get-universal-time)
        :for duration = (- curr-time start-time)
        :until (or
                (> (length out-string) 0)
                (> duration timeout))
        :do (progn
              (format t "len: ~a, dur: ~a~%" (length out-string) duration)
              (sleep .2))
        :finally (return out-string)))
