(defpackage :chipi-api.api-env-test
  (:use :cl :fiveam))

(in-package :chipi-api.api-env-test)

(def-suite api-env-tests
  :description "Tests for the API environment initialization,
in particular the portable serialization of the apikey sign key."
  :in chipi-api.tests:test-suite)

(in-suite api-env-tests)

(defun %sign-key-path ()
  (envi:ensure-runtime-dir "apikey-sign-key"))

(def-fixture preserve-sign-key ()
  "Runs the body with the sign-key file removed, restoring the previous file
content and `*sign-key*' value afterwards -- tests must not destroy a live
key."
  (let* ((path (%sign-key-path))
         (original-content (when (uiop:file-exists-p path)
                             (uiop:read-file-string path)))
         (original-key apikey-store::*sign-key*))
    (unwind-protect
         (progn
           (uiop:delete-file-if-exists path)
           (&body))
      (progn
        (setf apikey-store::*sign-key* original-key)
        (if original-content
            (with-open-file (file path :direction :output
                                       :if-exists :supersede)
              (write-string original-content file))
            (uiop:delete-file-if-exists path))))))

(test init-sign-key--generates-portable-list-file
  "A fresh sign-key file must contain a plain list of octets: a specialized
vector would print as #A(...) on sbcl, an extension other implementations
(e.g. clamiga) cannot read."
  (with-fixture preserve-sign-key ()
    (api-env::%init-apikey-sign-key)
    (let ((content (string-trim '(#\Space #\Newline)
                                (uiop:read-file-string (%sign-key-path)))))
      (is (char= #\( (char content 0)))
      (let ((octets (read-from-string content)))
        (is (listp octets))
        (is (= 20 (length octets)))
        (is-true (every (lambda (o) (typep o '(unsigned-byte 8))) octets))))))

(test init-sign-key--loads-back-what-it-wrote
  (with-fixture preserve-sign-key ()
    (api-env::%init-apikey-sign-key)
    (let ((written apikey-store::*sign-key*))
      (setf apikey-store::*sign-key* nil)
      (api-env::%init-apikey-sign-key)
      (is (typep apikey-store::*sign-key*
                 '(simple-array (unsigned-byte 8) (*))))
      (is (equalp written apikey-store::*sign-key*)))))

(test init-sign-key--reads-legacy-specialized-vector-file
  "Sign-key files written by older versions contain the raw specialized
vector (#A form on sbcl); loading them must still work."
  (with-fixture preserve-sign-key ()
    (let ((legacy-key (make-array 20 :element-type '(unsigned-byte 8)
                                     :initial-contents
                                     (loop :for i :from 1 :to 20 :collect i))))
      (with-open-file (file (%sign-key-path) :direction :output
                                             :if-exists :supersede)
        (with-standard-io-syntax
          (prin1 legacy-key file)))
      (api-env::%init-apikey-sign-key)
      (is (equalp legacy-key apikey-store::*sign-key*)))))
