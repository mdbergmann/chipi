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
            (with-standard-io-syntax
              (prin1 user-store::*crypt-salt* file))))
        (progn
          (log:info "Loading password salt")
          (with-open-file (file pw-salt
                                :direction :input)
            (setf user-store::*crypt-salt*
                  (coerce (read file) '(simple-array (unsigned-byte 8) (*))))))))
  t)

(defun init ()
  "Initialize the API environment.
This should be called very early in the application startup process.
Preferably in or with `hab:defconfig'."
  (%init-user-pw-salt)
  t)
