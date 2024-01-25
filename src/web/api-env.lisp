(defpackage :chipi-web.api-env
  (:use :cl)
  (:nicknames :api-env)
  (:import-from #:apikey-store
                #:*apikey-store-backend*
                #:*apikey-life-time-duration*)
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

(defun %init-apikey-store (apikey-store-backend)
  "Initialize the token store backend."
  (setf apikey-store:*apikey-store-backend*
        apikey-store-backend))

(defun %init-apikey-lifetime (apikey-lifetime-duration)
  "Initialize the apikey lifetime duration."
  (setf apikey-store:*apikey-life-time-duration*
        apikey-lifetime-duration))

(defun init (&key apikey-store (apikey-lifetime *apikey-life-time-duration*))
  "Initialize the API environment.
This should be called very early in the application startup process.
Preferably in or with `hab:defconfig'."
  (%init-user-pw-salt)
  (%init-apikey-store apikey-store)
  (%init-apikey-lifetime apikey-lifetime)
  t)
