(defpackage :chipi.binding.matter.ui-test
  (:use :cl :fiveam :cl-mock :chipi-ui.test-utils)
  (:local-nicknames (:matter :cl-matter)
                    (:ui :chipi.binding.matter.ui))
  (:import-from #:chipi.binding.matter-test
                #:bridge-env
                #:%in-bridge)
  (:export #:matter-ui-tests))
(in-package :chipi.binding.matter.ui-test)

(def-suite matter-ui-tests
  :description "The bridge's card in chipi-ui's settings, rendered into a stubbed CLOG connection"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite matter-ui-tests)

(defun %ctx (body)
  (ui-renderer:make-nav-context :body body
                                :container (clog:create-div body :class "container")
                                :depth 0))

(defun %render ()
  "Renders the section into a fresh container of the stubbed connection; returns the container."
  (let* ((body (make-body))
         (container (clog:create-div body)))
    (ui:render-share-section (%ctx body) container)
    container))

(defun %install-test-fabric (label vendor-id)
  "A fabric on the bridge's device, as commissioning would leave one, without the network."
  (let ((fabric (matter:make-test-fabric :fabric-id #x1D :vendor-id vendor-id)))
    (setf (matter:fabric-label fabric) label)
    (%in-bridge (lambda ()
                  (matter::%install-fabric (binding-matter::bridge-device binding-matter::*bridge*) fabric)))
    fabric))

;; -----------------------------
;; the texts
;; -----------------------------

(test share-section--registered-with-the-settings-on-load
  (let ((section (ui-settings:find-section-by-path "/settings/matter-bridge")))
    (is-true section)
    (is (eq ui:+section-id+ (ui-settings:settings-section-id section)))
    (is (string= "Share with other apps" (ui-settings:settings-section-label section)))
    (is (eq #'ui:render-share-section (ui-settings:settings-section-render section)))))

(test texts--manual-code-admin-name-device-type-window
  (is (string= "3497-011-2332" (ui::%format-manual-code "34970112332")))
  (is (string= "123" (ui::%format-manual-code "123")))
  (is (string= "Apple Home" (ui::%admin-name '(:label "Apple Home" :vendor-id #x1349))))
  (is (string= "Apple" (ui::%admin-name '(:label "" :vendor-id #x1349))))
  (is (string= "Google" (ui::%admin-name '(:label "" :vendor-id #x6006))))
  (is (string= "App with vendor id 0xBEEF" (ui::%admin-name '(:label "" :vendor-id #xBEEF))))
  (is (string= "On/Off Light" (ui::%device-type-name :on-off-light)))
  (is (string= "Unknown device" (ui::%device-type-name nil)))
  (is (string= "Some Thing" (ui::%device-type-name :some-thing)))
  (is (string= "Pairing window closed" (ui::%window-text nil)))
  (is (string= "Pairing window open until the first app has added the bridge" (ui::%window-text t)))
  (is (string= "Pairing window open, 14 minutes left" (ui::%window-text 790)))
  (is (string= "Pairing window open, 1 minute left" (ui::%window-text 60)))
  (is (string= "Pairing window open, 59 seconds left" (ui::%window-text 59))))

(test summary--without-and-with-the-bridge
  (with-fixture bridge-env (:init nil)
    (is (string= "Bridge not running" (ui::%summary))))
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall" (binding-matter:matter-bridge))
    (is (string= "0 apps, 1 device, pairing window open" (ui::%summary)))
    (%install-test-fabric "Apple Home" #x1349)
    (binding-matter:matter-bridge-close-window)
    (is (string= "1 app, 1 device" (ui::%summary)))))

;; -----------------------------
;; the cards
;; -----------------------------

(test render--without-the-bridge-says-so
  (with-fixture bridge-env (:init nil)
    (with-captured-clog
      (%render)
      (is-true (js~ "not running"))
      (is-false (js~ "Pairing code")))))

(test render--shows-code-qr-window-apps-and-devices
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall <b>" (binding-matter:matter-bridge))
    (house:defheating bath "Bath"
      :temperature ((binding-matter:matter-bridge :device bath))
      :setpoint ((binding-matter:matter-bridge :device bath)))
    (hab:defitem 'nothing "Nothing" 'boolean (binding-matter:matter-bridge))
    (with-captured-clog
      (%render)
      (is-true (js~ "Pairing code"))
      (is-true (js~ (ui::%format-manual-code (binding-matter:matter-bridge-pairing-code))))
      (is-true (js~ "<svg"))
      ;; no app yet: the window is open, so the button closes it
      (is-true (js~ "open until the first app"))
      (is-true (js~ ">Close<"))
      (is-true (js~ "No app has added the bridge yet"))
      ;; the devices: labels escaped, type and items named, the unpublishable one with its problem
      (is-true (js~ "Hall &lt;b&gt;"))
      (is-true (js~ "On/Off Light, endpoint 2: light-hall"))
      (is-true (js~ "Thermostat, endpoint 3: bath-temp, bath-setpoint"))
      (is-true (js~ "not published:"))
      (is (= 2 (count-js "&#10003;"))))))

(test render--window-button-closes-and-opens-the-window
  (with-fixture bridge-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (container (clog:create-div body))
             (rerendered 0)
             (rerender (lambda () (incf rerendered)))
             (button (ui::%render-pairing-card container rerender)))
        (is (eq t (binding-matter:matter-bridge-window-open-p)))
        (is-true (fire-click button))
        (is-false (binding-matter:matter-bridge-window-open-p))
        (is (= 1 rerendered))
        (let ((button (ui::%render-pairing-card container rerender)))
          (is-true (js~ "Pairing window closed"))
          (is-true (fire-click button))
          (let ((left (binding-matter:matter-bridge-window-open-p)))
            (is (integerp left))
            (is (<= 890 left 900)))
          (is (= 2 rerendered)))))))

(test render--admins-listed-and-removed-on-the-second-tap
  (with-fixture bridge-env ()
    (%install-test-fabric "" #x1349)
    (%install-test-fabric "Google Home" #x6006)
    (with-captured-clog
      (let* ((body (make-body))
             (container (clog:create-div body))
             (rerendered 0)
             (buttons (ui::%render-admins-card container (lambda () (incf rerendered)))))
        (is (= 2 (length buttons)))
        (is-true (js~ "Apple"))
        (is-true (js~ "Google Home"))
        (is-true (js~ "vendor 0x1349, fabric 1"))
        ;; the first tap arms, the second removes
        (is-true (fire-click (first buttons)))
        (is-true (js~ "Tap again to remove"))
        (is (= 2 (length (binding-matter:matter-bridge-admins))))
        (is (= 0 rerendered))
        (is-true (fire-click (first buttons)))
        (is (= 1 rerendered))
        (let ((admins (binding-matter:matter-bridge-admins)))
          (is (= 1 (length admins)))
          (is (string= "Google Home" (getf (first admins) :label))))))))

(test render--click-on-a-stopped-bridge-shows-the-error-instead-of-escaping
  (with-fixture bridge-env ()
    (%install-test-fabric "" #x1349)
    (with-captured-clog
      (let* ((body (make-body))
             (container (clog:create-div body))
             (rerendered 0)
             (rerender (lambda () (incf rerendered)))
             (window-button (ui::%render-pairing-card container rerender))
             (remove-button (first (ui::%render-admins-card container rerender))))
        (with-mocks ()
          (answer binding-matter:matter-bridge-close-window (error "bridge <gone>"))
          (answer binding-matter:matter-bridge-remove-admin (error "bridge <gone>"))
          (finishes (fire-click window-button))
          (is-true (js~ "Failed: bridge <gone>"))
          (is (= 0 rerendered))
          (finishes (fire-click remove-button))   ; arms
          (finishes (fire-click remove-button))   ; removes, fails
          (is (= 0 rerendered))
          (is (= 2 (count-js "Failed: bridge <gone>"))))))))

(test render--a-bridge-that-does-not-answer-is-reported
  (with-fixture bridge-env ()
    (with-captured-clog
      (with-mocks ()
        (answer binding-matter:matter-bridge-pairing-code (error "actor <gone>"))
        (finishes (%render))
        (is-true (js~ "did not answer: actor &lt;gone&gt;"))))))

(test render-settings--shows-the-card-with-the-summary
  (with-fixture bridge-env ()
    (house:deflight light-hall "Hall" (binding-matter:matter-bridge))
    (with-captured-clog
      (let ((body (make-body)))
        (ui-renderer:render-settings (%ctx body))
        (is-true (js~ "Share with other apps"))
        (is-true (js~ "0 apps, 1 device, pairing window open"))))))
