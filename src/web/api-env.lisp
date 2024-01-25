(defpackage :chipi-web.api-env
  (:use :cl)
  (:nicknames :api-env)
  (:import-from #:token-store
                #:*token-store-backend*
                #:*token-life-time-duration*)
  (:import-from #:ltd
                #:duration)
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
            (with-standard-io-syntax
              (setf user-store::*crypt-salt*
                    (coerce (read file)
                            '(simple-array (unsigned-byte 8) (*)))))))))
  t)

(defun %init-token-store (token-store-backend)
  "Initialize the token store backend."
  (setf token-store:*token-store-backend*
        token-store-backend))

(defun %init-token-lifetime (token-lifetime-duration)
  "Initialize the token lifetime duration."
  (setf token-store:*token-life-time-duration*
        token-lifetime-duration))

(defun init (&key token-store (token-lifetime (duration :day 30)))
  "Initialize the API environment.
This should be called very early in the application startup process.
Preferably in or with `hab:defconfig'."
  (%init-user-pw-salt)
  (%init-token-store token-store)
  (%init-token-lifetime token-lifetime)
  t)
