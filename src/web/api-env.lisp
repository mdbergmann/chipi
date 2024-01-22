(defpackage :chipi-web.api-env
  (:use :cl)
  (:nicknames :api-env)
  (:export #:init))

(in-package :chipi-web.api-env)

(defun %init-user-pw-salt ()
  (let ((pw-salt (envi:ensure-runtime-dir "user-pw-salt")))
    (if (not (uiop:file-exists-p pw-salt))
        (progn
          (log:info "Generating new password salt")
          (with-open-file (file pw-salt
                                :direction :output
                                :if-exists :supersede)
            (prin1 user-store::*scrypt-salt* file)))
        (progn
          (log:info "Loading password salt")
          (with-open-file (file pw-salt
                                :direction :input)
            (setf user-store::*scrypt-salt* (read file))))))
  t)

(defun init ()
  "Initialize the API environment.
This should be called very early in the application startup process.
Preferably in or with `hab:defconfig'."
  (%init-user-pw-salt)
  t)
