(defpackage :chipi-ui.page-test
  (:use :cl :fiveam :chipi-ui.page))

(in-package :chipi-ui.page-test)

(def-suite ui-page-tests
  :description "Tests for the page/widget model and the defpage DSL."
  :in chipi-ui.tests:test-suite)

(in-suite ui-page-tests)

(def-fixture clean-pages ()
  (unwind-protect
       (progn (&body))
    (clear-pages)))

;; ----------------------------------------------------------------------------
;; defpage -- registration, redefinition, path/title keys.
;; ----------------------------------------------------------------------------

(test defpage--registers-page-with-defaults
  (with-fixture clean-pages ()
    (let ((page (defpage 'wall-panel "Wall panel")))
      (is (eq page (get-page 'wall-panel)))
      (is (string= "Wall panel" (page-label page)))
      ;; path defaults to the downcased id, title to nil
      (is (string= "/wall-panel" (page-path page)))
      (is-false (page-title page))
      (is-false (page-children page)))))

(test defpage--path-and-title-keys
  (with-fixture clean-pages ()
    (let ((page (defpage 'wall-panel "Wall panel"
                  :path "/wall"
                  :title "Ground floor")))
      (is (string= "/wall" (page-path page)))
      (is (string= "Ground floor" (page-title page))))))

(test defpage--collects-widgets-in-order
  (with-fixture clean-pages ()
    (let ((page (defpage 'p "P"
                  (toggle 'switch1)
                  :title "With widgets"
                  (value 'temp1))))
      (is (= 2 (length (page-children page))))
      (is (typep (first (page-children page)) 'toggle))
      (is (typep (second (page-children page)) 'value-display))
      ;; the :title key is scanned, not collected as a widget
      (is (string= "With widgets" (page-title page))))))

(test defpage--redefinition-replaces
  (with-fixture clean-pages ()
    (defpage 'p "First")
    (defpage 'p "Second")
    (is (string= "Second" (page-label (get-page 'p))))
    (is (= 1 (length (get-pages))))))

(test defpage--widgets-composable-programmatically
  (with-fixture clean-pages ()
    (let ((page (defpage 'p "P"
                  (apply #'section "Temps"
                         (mapcar (lambda (id) (value id))
                                 '(temp1 temp2 temp3))))))
      (let ((sec (first (page-children page))))
        (is (typep sec 'section))
        (is (= 3 (length (section-children sec))))))))

;; ----------------------------------------------------------------------------
;; find-page-by-path -- path normalization and duplicate handling.
;; ----------------------------------------------------------------------------

(test find-page-by-path--normalizes-trailing-slash
  (with-fixture clean-pages ()
    (defpage 'wall "Wall" :path "/wall")
    (is (eq (get-page 'wall) (find-page-by-path "/wall")))
    (is (eq (get-page 'wall) (find-page-by-path "/wall/")))
    (is-false (find-page-by-path "/other"))))

(test find-page-by-path--root-path
  (with-fixture clean-pages ()
    (defpage 'home "Home" :path "/")
    (is (eq (get-page 'home) (find-page-by-path "/")))
    ;; the empty path addresses the root
    (is (eq (get-page 'home) (find-page-by-path "")))))

(test find-page-by-path--duplicate-path-newest-wins
  (with-fixture clean-pages ()
    (defpage 'first "First" :path "/same")
    (defpage 'second "Second" :path "/same")
    (is (eq (get-page 'second) (find-page-by-path "/same")))))

(test normalize-path--variants
  (is (string= "/" (normalize-path "/")))
  (is (string= "/" (normalize-path "")))
  (is (string= "/" (normalize-path nil)))
  (is (string= "/a" (normalize-path "/a/")))
  (is (string= "/a/b" (normalize-path "/a/b"))))

;; ----------------------------------------------------------------------------
;; widget constructors.
;; ----------------------------------------------------------------------------

(test widget-constructors--slider-defaults-and-overrides
  (let ((s (slider 'dimmer)))
    (is (= 0 (slider-min s)))
    (is (= 100 (slider-max s)))
    (is (= 1 (slider-step s))))
  (let ((s (slider 'dimmer :min 5 :max 50 :step 5 :label "Dimmer")))
    (is (= 5 (slider-min s)))
    (is (= 50 (slider-max s)))
    (is (= 5 (slider-step s)))
    (is (string= "Dimmer" (item-widget-label s)))))

(test widget-constructors--setpoint
  (let ((s (setpoint 'target-temp :min 15.0 :max 25.0 :step 0.5)))
    (is (= 15.0 (setpoint-min s)))
    (is (= 25.0 (setpoint-max s)))
    (is (= 0.5 (setpoint-step s)))))

(test widget-constructors--selection-choices
  (let ((s (selection 'mode :choices '(("auto" . "Automatic") ("off" . "Off")))))
    (is (= 2 (length (selection-choices s))))
    (is (string= "auto" (car (first (selection-choices s)))))))

(test widget-constructors--value-with-format
  (let ((v (value 'temp :format "~,1f °C")))
    (is (typep v 'value-display))
    (is (string= "~,1f °C" (value-display-format v)))))

(test widget-constructors--page-link-and-itemgroup-ref
  (let ((link (page-link 'cellar :label "Cellar")))
    (is (eq 'cellar (page-link-page-id link)))
    (is (string= "Cellar" (page-link-label link))))
  (let ((ref (itemgroup-ref 'lights)))
    (is (eq 'lights (itemgroup-ref-group-id ref)))))

;; ----------------------------------------------------------------------------
;; chart constructor -- range coercion.
;; ----------------------------------------------------------------------------

(test chart--range-plist-coerced-to-relative-range
  (let ((c (chart 'temp :range '(:hours 6))))
    (is (typep (chart-range c) 'persp:relative-range))
    (is (= 6 (persp:hours (chart-range c))))))

(test chart--range-object-passed-through
  (let* ((range (persp:make-relative-range :days 2))
         (c (chart 'temp :range range)))
    (is (eq range (chart-range c)))))

(test chart--range-defaults-to-last-day
  (let ((c (chart 'temp)))
    (is (typep (chart-range c) 'persp:relative-range))
    (is (= 1 (persp:days (chart-range c))))))

(test chart--type-and-persistence
  (let ((c (chart 'temp :type :bar :persistence :influx)))
    (is (eq :bar (chart-type c)))
    (is (eq :influx (chart-persistence c)))))
