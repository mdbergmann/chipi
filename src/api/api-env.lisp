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
              ;; print as a plain list of octets: a specialized vector would
              ;; print as #A(...) on sbcl/cmucl, an extension other
              ;; implementations cannot read; a list is portable
              (prin1 (coerce apikey-store::*sign-key* 'list) file))))
        (progn
          (log:info "Loading apikey sign key")
          (with-open-file (file apikey-sign-key
                                :direction :input)
            (with-standard-io-syntax
              ;; coerce accepts any sequence: new-style list files as well as
              ;; legacy vector files (#A.../#...) written by older versions
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
  (log:info "Initializing API environment")
  (%init-apikey-sign-key)
  (%init-apikey-store apikey-store)
  (%init-apikey-lifetime apikey-lifetime)
  t)
