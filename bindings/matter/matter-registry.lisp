(in-package :chipi.binding.matter)

;; The node registry of the controller role: one entry per Matter node
;; that Chipi commissioned into its fabric, kept in the file `nodes' of the
;; storage directory, next to the commissioner's own state file.
;;
;; The label is the name the configuration uses for a node: it survives a
;; node that is commissioned again and comes back under another node id.
;; What the node said about itself -- vendor, product, its endpoints with
;; their device types and clusters -- is what the devices view and the
;; suggested definers work on later.
;;
;; Nothing here touches the network or an actor: entries are plain data,
;; and the controller actor is the one that holds and changes them.

(defstruct (node-entry (:constructor %make-node-entry))
  "A commissioned node: its NODE-ID on Chipi's fabric and the LABEL it was
given; VENDOR-ID, PRODUCT-ID, VENDOR-NAME, PRODUCT-NAME, SERIAL-NUMBER and
UNIQUE-ID from its Basic Information; its ENDPOINTS, a list of plists
(:id :device-types :server-clusters), device types as (id . revision), from
the Descriptor of each; the ADDRESSES (strings) and PORT it was last
reached at; COMMISSIONED-AT and LAST-SEEN, universal times."
  node-id label
  vendor-id product-id vendor-name product-name serial-number unique-id
  (endpoints '())
  (addresses '())
  port
  commissioned-at last-seen)

(defmethod print-object ((entry node-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "~s node ~d~@[, ~a~]~@[ ~a~]"
            (node-entry-label entry) (node-entry-node-id entry)
            (node-entry-vendor-name entry) (node-entry-product-name entry))))

(defparameter *node-entry-fields*
  '((:node-id . node-entry-node-id)
    (:label . node-entry-label)
    (:vendor-id . node-entry-vendor-id)
    (:product-id . node-entry-product-id)
    (:vendor-name . node-entry-vendor-name)
    (:product-name . node-entry-product-name)
    (:serial-number . node-entry-serial-number)
    (:unique-id . node-entry-unique-id)
    (:endpoints . node-entry-endpoints)
    (:addresses . node-entry-addresses)
    (:port . node-entry-port)
    (:commissioned-at . node-entry-commissioned-at)
    (:last-seen . node-entry-last-seen))
  "The fields of a `node-entry' as the keys of its plist, each with its reader.")

(defun make-node-entry (&rest plist &key node-id label &allow-other-keys)
  "A `node-entry' from its plist. `NODE-ID' must be an integer and `LABEL' a string that is not empty."
  (unless (integerp node-id)
    (error "A node's id is an integer, not: ~s" node-id))
  (unless (and (stringp label) (plusp (length label)))
    (error "A node's label is a string that is not empty, not: ~s" label))
  (apply #'%make-node-entry plist))

(defun node-entry-plist (entry)
  "`ENTRY' as a fresh plist of `*node-entry-fields*': what the registry file holds and what `matter-nodes' hands out."
  (loop :for (key . reader) :in *node-entry-fields*
        :collect key
        :collect (copy-tree (funcall reader entry))))

(defun find-node-entry (entries designator)
  "The entry of `ENTRIES' that `DESIGNATOR' names: a label (a string) or a node id (an integer); NIL when there is none."
  (etypecase designator
    (string (find designator entries :key #'node-entry-label :test #'string=))
    (integer (find designator entries :key #'node-entry-node-id))))

(defun unique-node-label (entries label)
  "`LABEL' when no entry of `ENTRIES' has it, else `LABEL' with the first of -2, -3, ... that makes it free."
  (if (find-node-entry entries label)
      (loop :for n :from 2
            :for candidate := (format nil "~a-~d" label n)
            :unless (find-node-entry entries candidate)
              :return candidate)
      label))

;; -----------------------------
;; the file
;; -----------------------------

(defun %registry-file (storage)
  (merge-pathnames "nodes" storage))

(defun read-registry (storage)
  "The node entries kept in the directory `STORAGE', oldest first; none when there is no file yet."
  (mapcar (lambda (plist) (apply #'make-node-entry plist))
          (getf (matter:read-state-file (%registry-file storage)) :nodes)))

(defun write-registry (storage entries)
  "Writes `ENTRIES' to the registry file in the directory `STORAGE', replacing what is there."
  (matter:write-state-file (list :format matter:+state-format+
                                 :nodes (mapcar #'node-entry-plist entries))
                           (%registry-file storage)))

;; -----------------------------
;; what a node says about itself
;; -----------------------------

(defconstant +attribute-device-type-list+ 0)
(defconstant +attribute-server-list+ 1)

(defun %set-basic-information (entry attribute value)
  "Sets the slot of `ENTRY' that the Basic Information attribute id `ATTRIBUTE' fills; an attribute the registry does not keep is left out."
  (case attribute
    (1 (setf (node-entry-vendor-name entry) value))
    (2 (setf (node-entry-vendor-id entry) value))
    (3 (setf (node-entry-product-name entry) value))
    (4 (setf (node-entry-product-id entry) value))
    (15 (setf (node-entry-serial-number entry) value))
    (18 (setf (node-entry-unique-id entry) value))))

(defun describe-node-entry (entry reports)
  "Fills `ENTRY' from `REPORTS', the attribute reports of a read of the node's Basic Information and of the Descriptor of every endpoint: who made it, what it is, its endpoints with their device types and server clusters, in the order of their ids. A report that carries a status instead of a value is skipped; with no Descriptor among the reports the endpoints stay as they are. Returns `ENTRY'."
  (let ((endpoints '()))
    (flet ((endpoint (id)
             (or (find id endpoints :key (lambda (e) (getf e :id)))
                 (first (push (list :id id :device-types '() :server-clusters '()) endpoints)))))
      (dolist (report reports)
        (let ((path (matter:attribute-report-path report)))
          (multiple-value-bind (value kind) (matter:attribute-report-value report)
            (unless (eq kind :status)
              (let ((endpoint-id (matter:attribute-path-endpoint path))
                    (cluster (matter:attribute-path-cluster path))
                    (attribute (matter:attribute-path-attribute path)))
                (cond ((and (eql cluster matter:+cluster-basic-information+) (eql endpoint-id 0))
                       (%set-basic-information entry attribute value))
                      ((eql cluster matter:+cluster-descriptor+)
                       (let ((endpoint (endpoint endpoint-id)))
                         (cond ((eql attribute +attribute-device-type-list+)
                                (setf (getf endpoint :device-types)
                                      (mapcar (lambda (type) (cons (getf type :device-type) (getf type :revision)))
                                              value)))
                               ((eql attribute +attribute-server-list+)
                                (setf (getf endpoint :server-clusters) (copy-list value)))))))))))))
    (when endpoints
      (setf (node-entry-endpoints entry)
            (sort endpoints #'< :key (lambda (e) (getf e :id)))))
    entry))
