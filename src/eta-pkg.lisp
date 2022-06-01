(defpackage :cl-eta.package
  (:use :cl)
  (:nicknames :eta-pkg)
  (:export #:collect-data
           #:extract-pkg
           #:check-sum
           #:new-start-record-pkg
           #:new-stop-record-pkg))

(in-package :cl-eta.package)

(defparameter +monitor-items+
  '((19 . ("EtaAbgas" . 10.0))
    (22 . ("EtaTempAussen" . 10.0))
    (53 . ("HeatingETAOperatingHours" . 1.0))
    (21 . ("EtaBoiler" . 10.0))
    (167 . ("EtaBoilerUnten" . 10.0))
    (145 . ("EtaBoilerUntSolar" . 10.0)) ; node #x20
    (20 . ("EtaKessel" . 10.0))
    (112 . ("EtaKesselRuecklauf" . 10.0))
    (75 . ("EtaKollektor" . 10.0)) ; node #x20
    (77 . ("EtaPufferOben" . 10.0))
    (78 . ("EtaPufferUnten" . 10.0))
    (23 . ("EtaVorlaufMK0" . 10.0))
    (107 . ("HeatingETAIgnitionCount" . 1.0))
    ))

(defvar +monitor-size+ 5)

(defparameter +start-record-pkg-payload+
  '(30                                  ; 30 seconds refresh time
    #x18 0 19                           ; Abgass
    #x18 0 22                           ; Aussen
    #x18 0 53                           ; Betriebsstunden
    #x18 0 21                           ; Boiler
    #x18 0 167                          ; BoilerUnten
    #x20 0 145                          ; BoilerUntSolar
    #x18 0 20                           ; Kessel
    #x18 0 112                          ; Kesselrücklauf
    #x20 0 75                           ; Kollektor
    #x18 0 77                           ; Puffer oben
    #x18 0 78                           ; Puffer unten
    #x18 0 23                           ; Vorlauf MK0
    #x18 0 107                          ; Zündzähler
    ))

(defun new-start-record-pkg ()
  (coerce (alexandria:flatten
           `(#\{
             #\M #\C
             ,(length +start-record-pkg-payload+)
             ,(check-sum +start-record-pkg-payload+)
             ,+start-record-pkg-payload+
             #\}))
          'vector))

(defun new-stop-record-pkg ()
  #(#\{
    #\M #\E
    0 0
    #\}))

(defun collect-data (prev-data new-data)
  "Concatenates `prev-data' and `new-data'.
A full package is when it starts with `#\{' and ends with `#\}'.
In this case the return is `(values t <full-package>)'.
If this is a partial package the return is: `(values nil <partial-package>)'."
  (let* ((data (concatenate 'vector prev-data new-data))
         (data-len (length data)))
    (values
     (if (> data-len 0)
         (let ((first (elt data 0))
               (last (elt data (1- data-len))))
           (and (characterp first)
                (characterp last)
                (char= #\{ first)
                (char= #\} last)))
         nil)
     data)))

(defun check-sum (seq)
  (mod (reduce #'+ seq) 256))

(defun %to-int (upper lower)
  (+ (ash upper 8) lower))

(defun %to-vec (int)
  (vector (ash (mask-field (byte 16 8) int) -8)
          (mask-field (byte 8 0) int)))

(defun %id-to-item-name (mid)
  (cadr (find mid +monitor-items+ :key #'car :test #'=)))

(defun %id-to-item-divisor (mid)
  (cddr (find mid +monitor-items+ :key #'car :test #'=)))

(defun %monitor-payload-size-nok-p (monitor-data)
  (> (mod (length monitor-data) +monitor-size+) 0))

(defun %process-monitors (monitor-data)
  (if (%monitor-payload-size-nok-p monitor-data)
      `(:fail "Wrong payload size!")
      `(:ok ,(let ((monitors (/ (length monitor-data) +monitor-size+)))
               (loop :for i :to (1- monitors)
                     :for m = (subseq monitor-data (* i +monitor-size+))
                     :for node-id = (elt m 0)
                     :for m-id = (%to-int (elt m 1) (elt m 2))
                     :for m-val = (%to-int (elt m 3) (elt m 4))
                     :for item-name = (%id-to-item-name m-id)
                     :for item-div = (%id-to-item-divisor m-id)
                     :collect `(,item-name . ,(/ m-val item-div)))))))

(defun %undersized-pkg-p (pkg-data)
  (< (length pkg-data) 6))

(defun extract-pkg (pkg-data)
  "`pkg-data' is a full eta package with starting `#\{' and ending `#\}'.
If something happens during extraction the return is:
`(values :fail <reason>)'.
If it is a full package with monitors data the return is:
`(values :eta-monitor <alist-of-monitor-items)' where an item consists of: `(cons <openhab-item-name> <item-value>)'."
  (if (%undersized-pkg-p pkg-data)
      (values :fail "Undersized package!")
      (let ((sid (coerce `#(,(elt pkg-data 1) ,(elt pkg-data 2)) 'string))
            (payload-len (elt pkg-data 3))
            (checksum (elt pkg-data 4))
            (payload (subseq pkg-data 5 (1- (length pkg-data)))))
        (let ((monitor-data (%process-monitors payload)))
          (case (car monitor-data)
            (:ok (values :eta-monitor (cadr (%process-monitors payload))))
            (:fail (values :fail (cadr monitor-data))))))))
      
