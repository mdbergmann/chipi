(defpackage :chipi.binding.matter-controller-test
  (:use :cl :fiveam :chipi.binding.matter)
  (:local-nicknames (:matter :cl-matter))
  (:import-from #:chipi.binding.matter-test
                #:%temp-storage
                #:%delete-storage)
  (:export #:run!
           #:all-tests
           #:nil
           #:*test-controller-args*
           #:%await))
(in-package :chipi.binding.matter-controller-test)

(def-suite matter-controller-tests
  :description "Matter binding: the controller role without a node -- registry, futures, start and stop"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-controller-tests)

;; -----------------------------
;; helpers and fixtures
;; -----------------------------

(defparameter *test-controller-args*
  '(:family :ipv4 :address "127.0.0.1")
  "A controller on an ephemeral IPv4 loopback port: what SBCL's backend offers, and all a test needs.")

(defun %await (future &key (timeout 5))
  "The result of `FUTURE' -- the condition it failed with, for one that failed -- waiting up to `TIMEOUT' seconds; `:not-ready' when it does not come."
  (multiple-value-bind (result fut)
      (future:fawait future :timeout timeout :sleep-time 0.01)
    (if (future:complete-p fut)
        result
        :not-ready)))

(def-fixture controller-env (&key (init t))
  "A chipi environment with the Matter controller running on loopback, its storage in a fresh directory bound to `storage'. With `:init nil' the controller is not started; the test does it."
  (let ((storage (%temp-storage)))
    (unwind-protect
         (progn
           (hab:defconfig "chipi")
           (when init
             (matter-init :storage storage :controller *test-controller-args*))
           (&body))
      (progn
        ;; a controller the test started by itself has no shutdown hook
        (matter-shutdown)
        (hab:shutdown)
        (%delete-storage storage)))))

(defun %entry (&rest plist)
  (apply #'binding-matter::make-node-entry plist))

;; -----------------------------
;; the registry
;; -----------------------------

(test make-node-entry--checks-id-and-label
  (signals error (%entry :label "lamp"))
  (signals error (%entry :node-id 2))
  (signals error (%entry :node-id 2 :label ""))
  (signals error (%entry :node-id "2" :label "lamp"))
  (let ((entry (%entry :node-id 2 :label "lamp")))
    (is (= 2 (binding-matter::node-entry-node-id entry)))
    (is (string= "lamp" (binding-matter::node-entry-label entry)))
    (is (search "\"lamp\" node 2" (princ-to-string entry)))))

(test node-entry-plist--a-fresh-plist-of-every-field
  (let* ((entry (%entry :node-id 2 :label "lamp" :vendor-id #xFFF1 :vendor-name "ACME"
                        :endpoints (list (list :id 1 :device-types (list (cons #x100 3)) :server-clusters (list 3 6)))
                        :addresses (list "fe80::1") :port 5540 :commissioned-at 100 :last-seen 200))
         (plist (binding-matter::node-entry-plist entry)))
    (is (equal '(:node-id :label :vendor-id :product-id :vendor-name :product-name :serial-number :unique-id
                 :endpoints :addresses :port :commissioned-at :last-seen)
               (loop :for key :in plist :by #'cddr :collect key)))
    (is (= 2 (getf plist :node-id)))
    (is (string= "ACME" (getf plist :vendor-name)))
    (is (null (getf plist :product-name)))
    (is (equal '((:id 1 :device-types ((#x100 . 3)) :server-clusters (3 6))) (getf plist :endpoints)))
    ;; changing what was handed out does not reach the entry
    (setf (getf (first (getf plist :endpoints)) :id) 9)
    (is (= 1 (getf (first (binding-matter::node-entry-endpoints entry)) :id)))))

(test find-node-entry--by-label-and-by-id
  (let* ((lamp (%entry :node-id 2 :label "lamp"))
         (plug (%entry :node-id 3 :label "plug"))
         (entries (list lamp plug)))
    (is (eq lamp (binding-matter::find-node-entry entries "lamp")))
    (is (eq plug (binding-matter::find-node-entry entries 3)))
    (is (null (binding-matter::find-node-entry entries "Lamp")))
    (is (null (binding-matter::find-node-entry entries 4)))
    (signals error (binding-matter::find-node-entry entries :lamp))))

(test unique-node-label--the-first-free-suffix
  (let ((entries (list (%entry :node-id 2 :label "lamp") (%entry :node-id 3 :label "lamp-2"))))
    (is (string= "plug" (binding-matter::unique-node-label entries "plug")))
    (is (string= "lamp-3" (binding-matter::unique-node-label entries "lamp")))
    (is (string= "lamp" (binding-matter::unique-node-label '() "lamp")))))

(test registry--written-and-read-back
  (let ((storage (%temp-storage)))
    (unwind-protect
         (progn
           (is (null (binding-matter::read-registry storage)))
           (binding-matter::write-registry
            storage
            (list (%entry :node-id 2 :label "lamp" :vendor-id #xFFF1 :product-id #x8000
                          :vendor-name "ACME" :product-name "Lamp" :serial-number "S1" :unique-id "U1"
                          :endpoints (list (list :id 0 :device-types (list (cons #x16 3)) :server-clusters (list 29 40))
                                           (list :id 1 :device-types (list (cons #x100 3)) :server-clusters (list 3 6 29)))
                          :addresses (list "fe80::1" "fd00::1") :port 5540
                          :commissioned-at 3970000000 :last-seen 3970000100)
                  (%entry :node-id 3 :label "plug")))
           (is-true (probe-file (merge-pathnames "nodes" storage)))
           (let ((entries (binding-matter::read-registry storage)))
             (is (equal '("lamp" "plug") (mapcar #'binding-matter::node-entry-label entries)))
             (is (equal (list :node-id 2 :label "lamp" :vendor-id #xFFF1 :product-id #x8000
                              :vendor-name "ACME" :product-name "Lamp" :serial-number "S1" :unique-id "U1"
                              :endpoints '((:id 0 :device-types ((#x16 . 3)) :server-clusters (29 40))
                                           (:id 1 :device-types ((#x100 . 3)) :server-clusters (3 6 29)))
                              :addresses '("fe80::1" "fd00::1") :port 5540
                              :commissioned-at 3970000000 :last-seen 3970000100)
                        (binding-matter::node-entry-plist (first entries))))
             (is (null (binding-matter::node-entry-endpoints (second entries)))))
           ;; writing replaces
           (binding-matter::write-registry storage '())
           (is (null (binding-matter::read-registry storage))))
      (%delete-storage storage))))

;; -----------------------------
;; names to ids
;; -----------------------------

(test designators--names-and-ids
  (is (= matter:+cluster-on-off+ (binding-matter::%cluster-id :on-off)))
  (is (= #x1234 (binding-matter::%cluster-id #x1234)))
  (signals error (binding-matter::%cluster-id :no-such-cluster))
  (is (= 0 (binding-matter::%attribute-id :on-off :on-off)))
  (is (= 5 (binding-matter::%attribute-id :basic-information :node-label)))
  (is (= 7 (binding-matter::%attribute-id #x1234 7)))
  (signals error (binding-matter::%attribute-id :on-off :no-such-attribute))
  (is (= 1 (binding-matter::%command-id :on-off :on)))
  (is (= 2 (binding-matter::%command-id :on-off :toggle)))
  (is (= 9 (binding-matter::%command-id #x1234 9)))
  (signals error (binding-matter::%command-id :on-off :no-such-command))
  ;; fields: none, or a plist through the command's definition
  (is (null (binding-matter::%command-fields :on-off :on nil)))
  (is (matter:tlv-element-p
       (binding-matter::%command-fields :on-off :off-with-effect '(:effect-identifier 0 :effect-variant 0))))
  (let ((tlv (matter:command-fields :on-off :off-with-effect '(:effect-identifier 0 :effect-variant 0))))
    (is (eq tlv (binding-matter::%command-fields :on-off :off-with-effect tlv)))))

;; -----------------------------
;; a future out of a Matter actor
;; -----------------------------

(test matter-actor-future--resolves-fails-and-catches
  (with-fixture controller-env ()
    (let ((actor (binding-matter::controller-actor binding-matter::*controller*))
          (test-thread (bt:current-thread))
          (ran-on nil)
          (kept nil))
      ;; a value, resolved in actor context
      (is (= 42 (%await (matter-actor-future actor (lambda (resolve)
                                                     (setf ran-on (bt:current-thread))
                                                     (funcall resolve 42))))))
      (is-true ran-on)
      (is-false (eq test-thread ran-on))
      ;; NIL is a value too
      (let ((future (matter-actor-future actor (lambda (resolve) (funcall resolve nil)))))
        (is (null (%await future)))
        (is-true (future:complete-p future))
        (is-false (future:error-p future)))
      ;; resolved later, by another turn of the actor
      (let ((future (matter-actor-future actor (lambda (resolve) (setf kept resolve)))))
        (is (eq :not-ready (%await future :timeout 0.2)))
        (matter-actor-run actor (lambda () (funcall kept :later)))
        (is (eq :later (%await future))))
      ;; a condition fails the future, and so does an error in the function
      (let ((future (matter-actor-future actor (lambda (resolve)
                                                 (funcall resolve (make-condition 'simple-error
                                                                                  :format-control "no answer"))))))
        (is (typep (%await future) 'simple-error))
        (is-true (future:error-p future)))
      (let* ((mapped nil)
             (future (matter-actor-future actor (lambda (resolve)
                                                  (declare (ignore resolve))
                                                  (error "broken"))))
             (recovered (future:frecover
                         (future:fmap future (value)
                           (setf mapped value))
                         (error (c) (princ-to-string c)))))
        (is (string= "broken" (%await recovered)))
        (is-true (future:error-p future))
        (is (null mapped))))))

;; -----------------------------
;; start and stop
;; -----------------------------

(defun %fabric-id ()
  (matter:fabric-fabric-id
   (matter:commissioner-fabric (binding-matter::controller-commissioner binding-matter::*controller*))))

(test matter-init--starts-the-controller-at-once
  (with-fixture controller-env (:init nil)
    (is-false (matter-controller-running-p))
    (matter-init :storage storage :controller (append '(:fabric-id #xC1) *test-controller-args*))
    (is-true (matter-controller-running-p))
    (is-false (matter-bridge-running-p))
    (is (= #xC1 (%fabric-id)))
    (is (null (matter-nodes)))
    (is (search "fabric 00000000000000C1, 0 nodes" (princ-to-string binding-matter::*controller*)))
    ;; the fabric is on file from the start
    (is-true (probe-file (merge-pathnames "commissioner.state" storage)))
    ;; the configuration loaded again leaves the running controller alone
    (let ((controller binding-matter::*controller*))
      (matter-init :storage storage :controller (append '(:fabric-id #xC2) *test-controller-args*))
      (is (eq controller binding-matter::*controller*)))
    (signals error (start-controller storage))
    (matter-shutdown)
    (is-false (matter-controller-running-p))
    (is-false (stop-controller))))

(test matter-init--controller-t-starts-with-the-defaults
  (with-fixture controller-env (:init nil)
    (cl-mock:with-mocks ()
      (cl-mock:answer binding-matter::start-controller t)
      (matter-init :storage storage :controller t)
      ;; the storage only, no keys: start-controller's own defaults apply
      (is (equal (list (list 'binding-matter::start-controller
                             (binding-matter::%storage-directory storage)))
                 (cl-mock:invocations 'binding-matter::start-controller))))))

(test matter-init--no-controller-unless-asked
  (with-fixture controller-env (:init nil)
    (matter-init :storage storage)
    (is-false (matter-controller-running-p))
    (signals error (matter-nodes))
    (signals error (matter-read "lamp" 1 :on-off :on-off))
    (signals error (matter-invoke "lamp" 1 :on-off :on))))

(test start-controller--the-kept-fabric-stays
  (with-fixture controller-env (:init nil)
    (signals error (apply #'start-controller storage :attestation :whatever *test-controller-args*))
    (is-false (matter-controller-running-p))
    (apply #'start-controller storage :fabric-id #xC1 *test-controller-args*)
    (let ((root (matter:fabric-root-public-key
                 (matter:commissioner-fabric (binding-matter::controller-commissioner binding-matter::*controller*)))))
      (stop-controller)
      ;; another fabric id configured: the fabric on file is the one the nodes are on
      (apply #'start-controller storage :fabric-id #xC2 *test-controller-args*)
      (is (= #xC1 (%fabric-id)))
      (is (equalp root
                  (matter:fabric-root-public-key
                   (matter:commissioner-fabric
                    (binding-matter::controller-commissioner binding-matter::*controller*))))))))

(test start-controller--the-registry-is-read
  (with-fixture controller-env (:init nil)
    (binding-matter::write-registry storage (list (%entry :node-id 2 :label "lamp" :vendor-name "ACME")))
    (apply #'start-controller storage *test-controller-args*)
    (let ((nodes (matter-nodes)))
      (is (= 1 (length nodes)))
      (is (= 2 (getf (first nodes) :node-id)))
      (is (string= "lamp" (getf (first nodes) :label)))
      (is (string= "ACME" (getf (first nodes) :vendor-name))))
    ;; and written back when the controller stops
    (stop-controller)
    (is (equal '("lamp") (mapcar #'binding-matter::node-entry-label (binding-matter::read-registry storage))))))

(test stop-controller--fails-a-read-waiting-for-a-session
  (with-fixture controller-env (:init nil)
    ;; nothing listens at the node's address: the handshake never ends
    (binding-matter::write-registry
     storage (list (%entry :node-id 2 :label "lamp" :addresses '("127.0.0.1") :port 9)))
    (apply #'start-controller storage *test-controller-args*)
    (let ((future (matter-read "lamp" 1 :on-off :on-off)))
      (is (eq :not-ready (%await future :timeout 0.3)))
      (stop-controller)
      (is (search "was stopped" (princ-to-string (%await future))))
      (is-true (future:error-p future)))))

(test stop-controller--closes-the-socket-when-a-state-file-fails
  (with-fixture controller-env (:init nil)
    (apply #'start-controller storage *test-controller-args*)
    (let ((port (nth-value 1 (matter:transport-local-endpoint
                              (matter:exchange-manager-transport
                               (binding-matter::controller-manager binding-matter::*controller*)))))
          (blocker (string-right-trim "/" (namestring storage))))
      ;; the files cannot be written: the storage directory is gone, a file in its place
      (%delete-storage storage)
      (with-open-file (out blocker :direction :output :if-does-not-exist :create)
        (write-line "x" out))
      (unwind-protect
           (progn
             (is-true (stop-controller))
             (is-false (matter-controller-running-p))
             ;; the port is free again, so the manager was closed
             (let ((manager (matter:make-exchange-manager
                             :transport (matter:make-udp-transport :family :ipv4 :address "127.0.0.1"
                                                                   :port port))))
               (matter:exchange-manager-close manager)
               (pass)))
        ;; the fixture deletes a directory
        (delete-file blocker)
        (ensure-directories-exist storage)))))

;; -----------------------------
;; requests that cannot go out
;; -----------------------------

(test matter-read--fails-for-a-node-that-is-not-commissioned
  (with-fixture controller-env ()
    (let ((future (matter-read "lamp" 1 :on-off :on-off)))
      (is (search "No Matter node \"lamp\" is commissioned" (princ-to-string (%await future))))
      (is-true (future:error-p future)))
    (let ((future (matter-invoke 7 1 :on-off :on)))
      (is (search "No Matter node 7 is commissioned" (princ-to-string (%await future))))
      (is-true (future:error-p future)))))

(test matter-read--fails-for-a-node-without-an-address
  (with-fixture controller-env (:init nil)
    (binding-matter::write-registry storage (list (%entry :node-id 2 :label "lamp")))
    (apply #'start-controller storage *test-controller-args*)
    (let ((first (matter-read "lamp" 1 :on-off :on-off))
          (second (matter-invoke "lamp" 1 :on-off :on)))
      (is (search "No address is known" (princ-to-string (%await first))))
      (is-true (future:error-p first))
      ;; the next request tries again, and fails by itself
      (is (search "No address is known" (princ-to-string (%await second))))
      (is-true (future:error-p second)))))

(test matter-read--fails-for-a-name-the-cluster-does-not-have
  (with-fixture controller-env (:init nil)
    (binding-matter::write-registry storage (list (%entry :node-id 2 :label "lamp")))
    (apply #'start-controller storage *test-controller-args*)
    (let ((future (matter-read "lamp" 1 :on-off :no-such-attribute)))
      (is (search "has no attribute" (princ-to-string (%await future))))
      (is-true (future:error-p future)))
    (let ((future (matter-invoke "lamp" 1 :on-off :no-such-command)))
      (is (search "has no command" (princ-to-string (%await future))))
      (is-true (future:error-p future)))))

(test commission-at--wants-a-label-that-is-free
  (with-fixture controller-env (:init nil)
    (binding-matter::write-registry storage (list (%entry :node-id 2 :label "lamp")))
    (apply #'start-controller storage *test-controller-args*)
    (signals error (binding-matter::%commission-at "127.0.0.1" 5540 :passcode 20202021))
    (signals error (binding-matter::%commission-at "127.0.0.1" 5540 :passcode 20202021 :label ""))
    (let ((future (binding-matter::%commission-at "127.0.0.1" 5540 :passcode 20202021 :label "lamp")))
      (is (search "is commissioned already" (princ-to-string (%await future))))
      (is-true (future:error-p future)))))
