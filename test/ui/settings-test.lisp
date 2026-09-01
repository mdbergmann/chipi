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
