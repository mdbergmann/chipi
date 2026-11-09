(in-package :chipi.binding.matter)

;; Configuration: `matter-init' in `hab:defconfig', `matter-shutdown' from
;; the shutdown hook it registers.

(defvar *storage* nil
  "The directory of the Matter state files, from `matter-init'.")

(defun %expand-home (namestring)
  "`NAMESTRING' with a leading `~/' replaced by the home directory; Lisp does not expand it by itself."
  (if (and (>= (length namestring) 2)
           (char= #\~ (char namestring 0))
           (char= #\/ (char namestring 1)))
      (namestring (merge-pathnames (subseq namestring 2) (user-homedir-pathname)))
      namestring))

(defun %storage-directory (storage)
  "The directory `STORAGE' names (a string or pathname, `~/' understood), or `matter/' in chipi's runtime directory when it is NIL; created when missing."
  (let ((dir (if storage
                 (uiop:ensure-directory-pathname
                  (if (stringp storage) (%expand-home storage) storage))
                 (envi:ensure-runtime-dir "matter/"))))
    (ensure-directories-exist dir)
    dir))

(defun matter-init (&key storage bridge)
  "Configures and starts the Matter binding; to be called in `hab:defconfig'. A shutdown hook that calls `matter-shutdown' is added with `hab:add-to-shutdown'.

`STORAGE': the directory for the state files, a string (`~/' understood) or pathname; default `matter/' in chipi's runtime directory. Created when missing.
`BRIDGE': NIL, or T or a plist to run Chipi as a Matter bridge, whose bridged devices are the items with a `matter-bridge' binding. The plist's keys are those of `start-bridge': `:name' (\"Chipi\"), `:passcode' (20202021), `:discriminator' (3840), `:port' (5540), `:interface', `:advertise' (T).

  (defconfig \"my-home\"
    (matter-init :storage \"~/.chipi/matter/\"
                 :bridge '(:name \"Chipi\" :passcode 20202021 :discriminator 3840)))

The bridge's pairing codes are logged at start and `matter-bridge-pairing-code' returns them."
  (hab:add-to-shutdown #'matter-shutdown)
  (setf *storage* (%storage-directory storage))
  (log:info "Matter: state files in ~a" *storage*)
  (when bridge
    (apply #'start-bridge *storage* (if (eq bridge t) '() bridge)))
  t)

(defun matter-shutdown ()
  "Stops the Matter binding: the bridge, if it runs. `hab:shutdown' calls this through the hook `matter-init' registered."
  (stop-bridge)
  (%clear-bridge-bindings)
  (setf *storage* nil)
  t)
