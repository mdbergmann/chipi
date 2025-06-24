(defpackage :chipi-api.api-env
  (:use :cl)
  (:nicknames :api-env)
  (:import-from #:apikey-store
                #:*apikey-store-backend*
                #:*apikey-life-time-duration*)
  (:import-from #:ltd
                #:duration)
  (:export #:init))

(in-package :chipi-api.api-env)

(defun %init-apikey-sign-key ()
  (let ((apikey-sign-key (envi:ensure-runtime-dir "apikey-sign-key")))
    (if (not (uiop:file-exists-p apikey-sign-key))
        (progn
          (log:info "Generating new apikey sign key")
          (with-open-file (file apikey-sign-key
                                :direction :output
                                :if-exists :supersede)
            (with-standard-io-syntax
              (prin1 apikey-store::*sign-key* file))))
        (progn
          (log:info "Loading apikey sign key")
          (with-open-file (file apikey-sign-key
                                :direction :input)
            (with-standard-io-syntax
              (setf apikey-store::*sign-key*
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
  (%init-apikey-sign-key)
  (%init-apikey-store apikey-store)
  (%init-apikey-lifetime apikey-lifetime)
  t)
