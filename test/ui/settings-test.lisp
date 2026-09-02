(defpackage :chipi-ui.settings-test
  (:use :cl :fiveam :cl-mock :chipi-ui.test-utils))

(in-package :chipi-ui.settings-test)

(def-suite ui-settings-tests
  :description "Tests for the per-device UI settings, the home view they
select, and the app header that reaches them."
  :in chipi-ui.tests:test-suite)

(in-suite ui-settings-tests)

(def-fixture settings-env ()
  (let ((ui-renderer:*item-value-form-update-funs* (make-hash-table :test #'equal)))
    (unwind-protect
         (progn (&body))
      (page:clear-pages))))

(defun make-ctx (body &optional (depth 0))
  (ui-renderer:make-nav-context :body body
                                :container (clog:create-div body :class "container")
                                :depth depth))

;; ----------------------------------------------------------------------------
;; %stored-value -- what localStorage answers is not always a path.
;; ----------------------------------------------------------------------------

(test %stored-value--plain-path
  (is (string= "/rooms" (ui-settings::%stored-value "/rooms"))))

(test %stored-value--trims-surrounding-whitespace
  (is (string= "/rooms" (ui-settings::%stored-value "  /rooms  "))))

(test %stored-value--js-null-is-no-value
  ;; getItem answers JS null for a key that was never written; it arrives as
  ;; the string "null" and must not be taken for a path
  (is-false (ui-settings::%stored-value "null"))
  (is-false (ui-settings::%stored-value "undefined")))

(test %stored-value--empty-and-non-string-are-no-value
  (is-false (ui-settings::%stored-value ""))
  (is-false (ui-settings::%stored-value "   "))
  ;; a query that timed out answers NIL
  (is-false (ui-settings::%stored-value nil)))

;; ----------------------------------------------------------------------------
;; home-path / set-home-path -- the browser-side store.
;; ----------------------------------------------------------------------------

(test set-home-path--writes-the-path-to-local-storage
  (with-captured-clog
    (let ((body (make-body)))
      (ui-settings:set-home-path body "/rooms")
      (is-true (js~ "localStorage.setItem('chipi-home-path','/rooms')")))))

(test set-home-path--nil-removes-the-setting
  (with-captured-clog
    (let ((body (make-body)))
      (ui-settings:set-home-path body nil)
      (is-true (js~ "localStorage.removeItem('chipi-home-path')"))
      (is-false (js~ "setItem")))))

(defmacro with-query-answer ((answer) &body body)
  "Runs BODY with the stubbed CLOG connection answering every query with
ANSWER, which is what the browser sends back for a `storage-element' read."
  (let ((saved (gensym "QUERY")))
    `(let ((,saved (fdefinition 'clog-connection:query)))
       (unwind-protect
            (progn
              (setf (fdefinition 'clog-connection:query)
                    (lambda (connection-id script &key default-answer)
                      (declare (ignore connection-id script default-answer))
                      ,answer))
              ,@body)
         (setf (fdefinition 'clog-connection:query) ,saved)))))

(test home-path--reads-what-the-browser-stored
  (with-captured-clog
    (let ((body (make-body)))
      (with-query-answer ("/rooms")
        (is (string= "/rooms" (ui-settings:home-path body))))
      ;; a device that never stored one: getItem answers JS null
      (with-query-answer ("null")
        (is-false (ui-settings:home-path body)))
      ;; a query that timed out answers the default answer, NIL
      (is-false (ui-settings:home-path body)))))

;; ----------------------------------------------------------------------------
;; home-page -- resolving a stored path against the page registry.
;; ----------------------------------------------------------------------------

(test home-page--stored-path-wins
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/")
    (page:defpage 'rooms "Rooms" :path "/rooms")
    (is (eq (page:get-page 'rooms) (ui-renderer:home-page "/rooms")))))

(test home-page--without-a-stored-path-falls-back-to-the-root-page
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/")
    (is (eq (page:get-page 'start) (ui-renderer:home-page nil)))))

(test home-page--stale-stored-path-falls-back-to-the-root-page
  ;; the page a device points at can be renamed or dropped while the device
  ;; keeps its setting -- that must not strand it on a 'page not found'
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/")
    (is (eq (page:get-page 'start) (ui-renderer:home-page "/gone")))))

(test home-page--nil-means-the-generated-overview
  (with-fixture settings-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms")
    ;; nothing stored and no page claiming "/"
    (is-false (ui-renderer:home-page nil))
    ;; stored path gone and no page claiming "/"
    (is-false (ui-renderer:home-page "/gone"))))

;; ----------------------------------------------------------------------------
;; render-home -- the view behind the root path.
;; ----------------------------------------------------------------------------

(test render-home--renders-the-root-page-when-nothing-is-stored
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/" :title "Overview page")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-home ctx)
        (is-true (js~ "Overview page"))))))

(test render-home--falls-back-to-the-itemgroup-overview
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (with-mocks ()
          (answer itemgroupsc:retrieve-top-level-itemgroups nil)
          (ui-renderer:render-home ctx)
          (is-true (js~ "Chipi Home Automation Dashboard")))))))

;; ----------------------------------------------------------------------------
;; app header -- the only navigation an installed web app has.
;; ----------------------------------------------------------------------------

(test app-header--carries-brand-home-and-settings
  (with-fixture settings-env ()
    (page:defpage 'wall "Wall panel" :path "/wall")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-page (page:get-page 'wall) ctx)
        (is-true (js~ "app-header"))
        (is-true (js~ "app-brand"))
        (is-true (js~ "Chipi"))
        (is-true (js~ "Home"))
        (is-true (js~ "Settings"))
        ;; entry view: nothing to go back to
        (is-false (js~ "Back"))))))

(test app-header--back-button-appears-once-navigated
  (with-fixture settings-env ()
    (page:defpage 'wall "Wall panel" :path "/wall")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body 1)))
        (ui-renderer:render-page (page:get-page 'wall) ctx)
        (is-true (js~ "Back"))))))

;; ----------------------------------------------------------------------------
;; navigate-home / navigate-to-settings -- the Home/Settings header buttons'
;; click handlers, mirroring navigate-to-page's own test.
;; ----------------------------------------------------------------------------

(test navigate-home--pushes-root-path-and-renders-home-view
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/" :title "Overview page")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer::navigate-home ctx)
        (is (= 1 (ui-renderer:nav-context-depth ctx)))
        (is-true (js~ "history.pushState({},'','/')"))
        (is-true (js~ "Overview page"))))))

(test navigate-to-settings--pushes-settings-path-and-renders-settings-view
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer::navigate-to-settings ctx)
        (is (= 1 (ui-renderer:nav-context-depth ctx)))
        (is-true (js~ (format nil "history.pushState({},'','~a')"
                              ui-settings:+settings-path+)))
        (is-true (js~ "Home page"))))))

;; ----------------------------------------------------------------------------
;; settings view.
;; ----------------------------------------------------------------------------

(test render-settings--lists-default-and-every-page
  (with-fixture settings-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms")
    (page:defpage 'energy "Energy" :path "/energy")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-settings ctx)
        (is-true (js~ "Settings"))
        (is-true (js~ "Home page"))
        (is-true (js~ "Default"))
        (is-true (js~ "Rooms"))
        (is-true (js~ "/rooms"))
        (is-true (js~ "Energy"))
        (is-true (js~ "/energy"))
        ;; default option + one per page
        (is (= 3 (count-js "settings-option-label")))))))

(test render-settings--default-option-names-what-it-resolves-to
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        ;; no page claims the root path
        (ui-renderer:render-settings ctx)
        (is-true (js~ "Itemgroup overview"))))
    (page:defpage 'start "Start" :path "/")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-settings ctx)
        (is-true (js~ "Start (page at /)"))))))

(test render-settings--root-page-not-duplicated-as-its-own-row
  ;; a page claiming "/" is already reachable via "Default" -- listing it a
  ;; second time under its own path would give it two rows whose selected
  ;; state disagrees depending on whether the setting was stored via
  ;; "Default" (path nil) or the page's own row (path "/")
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/")
    (page:defpage 'rooms "Rooms" :path "/rooms")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (ui-renderer:render-settings ctx)
        ;; default option + "Rooms" only -- "Start" gets no row of its own
        (is (= 2 (count-js "settings-option-label")))))))

(test render-settings--marks-the-stored-page-as-selected
  (with-fixture settings-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        (with-mocks ()
          (answer ui-settings:home-path "/rooms")
          (ui-renderer:render-settings ctx)
          (is (= 1 (count-js "settings-option selected"))))))))

(test settings-option--click-stores-the-path
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body))
             (row (ui-renderer::%render-home-option
                   ctx body "Rooms" "/rooms" "/rooms" nil)))
        (is-true (fire-click row))
        (is-true (js~ "localStorage.setItem('chipi-home-path','/rooms')"))))))

(test settings-option--click-on-default-clears-the-setting
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body))
             (row (ui-renderer::%render-home-option
                   ctx body "Default" "Itemgroup overview" nil "/rooms")))
        (is-true (fire-click row))
        (is-true (js~ "localStorage.removeItem('chipi-home-path')"))))))

;; ----------------------------------------------------------------------------
;; path dispatch -- the settings path and the root path are served by the UI
;; itself, not by the page registry.
;; ----------------------------------------------------------------------------

(test dispatch-path--root-renders-the-home-view
  (with-fixture settings-env ()
    (page:defpage 'start "Start" :path "/" :title "The start page")
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body)))
        ;; the stubbed connection answers the location query with NIL, which
        ;; normalizes to the root path
        (ui-main::%dispatch-path ctx)
        (is-true (js~ "The start page"))))))

;; ----------------------------------------------------------------------------
;; device lock -- a locked device hides the settings gear and asks for the PIN
;; configured on the server before it shows its settings.
;; ----------------------------------------------------------------------------

(defmacro with-pin ((pin) &body body)
  `(let ((ui-settings:*settings-pin* ,pin))
     ,@body))

(defun fire-enter (clog-obj)
  "Invokes the key-down handler bound on CLOG-OBJ with an Enter key event, in
the wire format `clog:parse-keyboard-event' reads.  Matches what a real
browser sends for Enter: CLOG's `keyboard-event-script' computes :key-code
client-side as `e.key.charCodeAt(0)', which for \"Enter\" is 69 (the code of
`E'), not the standard numeric key code 13.  Returns t if bound."
  (let ((handler (gethash (format nil "~a:keydown" (clog:html-id clog-obj))
                          (clog:connection-data clog-obj))))
    (when handler
      (funcall handler "69:0:false:false:false:false:Enter")
      t)))

(test lock-available-p--takes-a-configured-pin
  (with-pin (nil) (is-false (ui-settings:lock-available-p)))
  (with-pin ("4711") (is-true (ui-settings:lock-available-p))))

(test locked-p--reads-what-the-browser-stored
  (with-pin ("4711")
    (with-captured-clog
      (let ((body (make-body)))
        (with-query-answer ("true")
          (is-true (ui-settings:locked-p body)))
        (with-query-answer ("null")
          (is-false (ui-settings:locked-p body)))
        ;; a query that timed out
        (is-false (ui-settings:locked-p body))))))

(test locked-p--never-without-a-configured-pin
  ;; the PIN dropped from the config again must not strand a locked device
  ;; with no way into its settings
  (with-pin (nil)
    (with-captured-clog
      (let ((body (make-body)))
        (with-query-answer ("true")
          (is-false (ui-settings:locked-p body)))))))

(test set-locked--writes-and-removes-the-flag
  (with-captured-clog
    (let ((body (make-body)))
      (ui-settings:set-locked body t)
      (is-true (js~ "localStorage.setItem('chipi-settings-locked','true')"))
      (ui-settings:set-locked body nil)
      (is-true (js~ "localStorage.removeItem('chipi-settings-locked')")))))

(test pin-matches-p--compares-the-trimmed-entry-with-the-configured-pin
  (with-pin ("4711")
    (is-true (ui-settings:pin-matches-p "4711"))
    (is-true (ui-settings:pin-matches-p " 4711 "))
    (is-false (ui-settings:pin-matches-p "0000"))
    (is-false (ui-settings:pin-matches-p ""))
    ;; the field's value query timed out
    (is-false (ui-settings:pin-matches-p nil)))
  (with-pin (nil)
    (is-false (ui-settings:pin-matches-p "4711"))))

(test app-header--locked-device-has-no-settings-gear
  (with-fixture settings-env ()
    (page:defpage 'wall "Wall panel" :path "/wall")
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (with-mocks ()
            (answer ui-settings:locked-p t)
            (ui-renderer:render-page (page:get-page 'wall) ctx)
            (is-true (js~ "Home"))
            (is-false (js~ "&#9881;"))
            (is-false (js~ "Settings"))))))))

(test brand--five-taps-open-the-settings-on-a-locked-device
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body))
             (brand (ui-renderer::%render-brand ctx body t)))
        (loop :repeat 4 :do (fire-click brand))
        (is-false (js~ "history.pushState"))
        (fire-click brand)
        (is-true (js~ (format nil "history.pushState({},'','~a')"
                              ui-settings:+settings-path+)))))))

(test brand--is-inert-on-an-unlocked-device
  (with-fixture settings-env ()
    (with-captured-clog
      (let* ((body (make-body))
             (ctx (make-ctx body))
             (brand (ui-renderer::%render-brand ctx body nil)))
        (is-false (fire-click brand))))))

(test %set-on-secret-taps--only-taps-within-the-window-count
  (with-captured-clog
    (let* ((body (make-body))
           (div (clog:create-div body))
           (now 0)
           (fired 0))
      (ui-renderer::%set-on-secret-taps div (lambda () (incf fired))
                                        :clock (lambda () now))
      (loop :repeat 4 :do (fire-click div))
      ;; a pause longer than the window forgets the taps so far
      (incf now (* 10 internal-time-units-per-second))
      (fire-click div)
      (is (= 0 fired))
      (loop :repeat 3 :do (fire-click div))
      (is (= 0 fired))
      (fire-click div)
      (is (= 1 fired)))))

(test render-settings--locked-device-gets-the-pin-prompt
  (with-fixture settings-env ()
    (page:defpage 'rooms "Rooms" :path "/rooms")
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (with-mocks ()
            (answer ui-settings:locked-p t)
            (ui-renderer:render-settings ctx)
            (is-true (js~ "Device locked"))
            (is-true (js~ "settings-pin-input"))
            ;; none of the settings themselves, and no gear either
            (is-false (js~ "settings-option-label"))
            (is-false (js~ "Rooms"))
            (is-false (js~ "&#9881;"))))))))

(test render-settings--unlocked-connection-sees-the-locked-devices-settings
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (setf (ui-renderer:nav-context-settings-unlocked ctx) t)
          (with-mocks ()
            (answer ui-settings:locked-p t)
            (ui-renderer:render-settings ctx)
            (is-true (js~ "Home page"))
            (is-true (js~ "Unlock this device"))
            (is-false (js~ "settings-pin-input"))
            ;; the device is still locked, so the gear stays hidden
            (is-false (js~ "&#9881;"))))))))

(test render-settings--offers-the-lock-only-with-a-configured-pin
  (with-fixture settings-env ()
    (with-pin (nil)
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (ui-renderer:render-settings ctx)
          (is-true (js~ "Home page"))
          (is-false (js~ "Device lock")))))
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (ui-renderer:render-settings ctx)
          (is-true (js~ "Device lock"))
          (is-true (js~ "Lock this device")))))))

(test pin-prompt--the-right-pin-unlocks-the-connection
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (with-mocks ()
            (answer ui-settings:locked-p t)
            (multiple-value-bind (input button)
                (ui-renderer::%render-pin-prompt ctx (clog:create-div body))
              (declare (ignore input))
              ;; the browser answers the field's value query with the entry
              (with-query-answer ("4711")
                (is-true (fire-click button)))
              (is-true (ui-renderer:nav-context-settings-unlocked ctx))
              (is-true (js~ "Unlock this device")))))))))

(test pin-prompt--enter-in-the-field-tries-the-pin
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (with-mocks ()
            (answer ui-settings:locked-p t)
            (multiple-value-bind (input button)
                (ui-renderer::%render-pin-prompt ctx (clog:create-div body))
              (declare (ignore button))
              (with-query-answer ("4711")
                (is-true (fire-enter input)))
              (is-true (ui-renderer:nav-context-settings-unlocked ctx)))))))))

(test pin-prompt--a-wrong-pin-says-so-and-keeps-the-lock
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (multiple-value-bind (input button)
              (ui-renderer::%render-pin-prompt ctx (clog:create-div body))
            (declare (ignore input))
            (with-query-answer ("0000")
              (fire-click button))
            (is-false (ui-renderer:nav-context-settings-unlocked ctx))
            (is-true (js~ "Wrong PIN"))
            (is-false (js~ "Home page"))))))))

(test lock-row--click-locks-the-device-and-keeps-this-connection-in
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body))
               (row (ui-renderer::%render-lock-section
                     ctx (clog:create-div body) nil)))
          (is-true (fire-click row))
          (is-true (js~ "localStorage.setItem('chipi-settings-locked','true')"))
          (is-true (ui-renderer:nav-context-settings-unlocked ctx)))))))

(test lock-row--click-on-a-locked-device-unlocks-it
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body))
               (row (ui-renderer::%render-lock-section
                     ctx (clog:create-div body) t)))
          (is-true (fire-click row))
          (is-true (js~ "localStorage.removeItem('chipi-settings-locked')")))))))

(test dispatch-path--typed-settings-url-meets-the-pin-prompt-on-a-locked-device
  ;; a plain browser has an address bar; typing /settings into it must not get
  ;; around the lock
  (with-fixture settings-env ()
    (with-pin ("4711")
      (with-captured-clog
        (let* ((body (make-body))
               (ctx (make-ctx body)))
          (with-mocks ()
            (answer ui-settings:locked-p t)
            ;; the stubbed connection answers the location query with the path
            (with-query-answer ("/settings")
              (ui-main::%dispatch-path ctx))
            (is-true (js~ "Device locked"))
            (is-false (js~ "settings-option-label"))))))))

;; ----------------------------------------------------------------------------
;; manifest -- what makes the UI installable.
;; ----------------------------------------------------------------------------

(test manifest-middleware--serves-the-manifest-as-json
  (let* ((inner (lambda (env) (declare (ignore env)) :passed-through))
         (app (ui-webapp:manifest-middleware inner))
         (response (funcall app (list :path-info "/manifest.webmanifest"))))
    (is (= 200 (first response)))
    (is (string= "application/manifest+json"
                 (getf (second response) :content-type)))
    (let ((manifest (first (third response))))
      (is-true (search "\"name\": \"Chipi\"" manifest))
      ;; the launch URL is baked into the install, so it must stay the root
      ;; path -- the device's setting decides what the root path shows
      (is-true (search "\"start_url\": \"/\"" manifest))
      (is-true (search "\"display\": \"standalone\"" manifest)))))

(test manifest-middleware--passes-other-paths-through
  (let* ((inner (lambda (env) (declare (ignore env)) :passed-through))
         (app (ui-webapp:manifest-middleware inner)))
    (is (eq :passed-through (funcall app (list :path-info "/rooms"))))
    (is (eq :passed-through (funcall app (list :path-info "/"))))))
