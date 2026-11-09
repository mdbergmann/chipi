(in-package :chipi.binding.matter)

;; A QR code encoder, for the bridge's onboarding payload on the UI's
;; "Share with other apps" card.
;;
;; ISO/IEC 18004, the part a pairing code needs: byte mode, versions 1 to
;; 10 (up to 271 characters at level L), the four error correction levels,
;; and the mask chosen by the penalty rules, so any phone reads it.  The
;; result is a square bit array of modules, 1 dark; `qr-svg' draws it.
;; Nothing here is Matter-specific.

(defparameter *qr-ec-blocks*
  '((1  :l (7 (1 19))          :m (10 (1 16))         :q (13 (1 13))         :h (17 (1 9)))
    (2  :l (10 (1 34))         :m (16 (1 28))         :q (22 (1 22))         :h (28 (1 16)))
    (3  :l (15 (1 55))         :m (26 (1 44))         :q (18 (2 17))         :h (22 (2 13)))
    (4  :l (20 (1 80))         :m (18 (2 32))         :q (26 (2 24))         :h (16 (4 9)))
    (5  :l (26 (1 108))        :m (24 (2 43))         :q (18 (2 15) (2 16))  :h (22 (2 11) (2 12)))
    (6  :l (18 (2 68))         :m (16 (4 27))         :q (24 (4 19))         :h (28 (4 15)))
    (7  :l (20 (2 78))         :m (18 (4 31))         :q (18 (2 14) (4 15))  :h (26 (4 13) (1 14)))
    (8  :l (24 (2 97))         :m (22 (2 38) (2 39))  :q (22 (4 18) (2 19))  :h (26 (4 14) (2 15)))
    (9  :l (30 (2 116))        :m (22 (3 36) (2 37))  :q (20 (4 16) (4 17))  :h (24 (4 12) (4 13)))
    (10 :l (18 (2 68) (2 69))  :m (26 (4 43) (1 44))  :q (24 (6 19) (2 20))  :h (28 (6 15) (2 16))))
  "Per version, per error correction level: the error correction codewords per block, then the block groups as (blocks data-codewords) -- table 9 of the standard.")

(defparameter *qr-alignment-positions*
  '((1) (2 6 18) (3 6 22) (4 6 26) (5 6 30) (6 6 34) (7 6 22 38) (8 6 24 42) (9 6 26 46) (10 6 28 50))
  "Per version, the row and column coordinates the alignment patterns are centred on.")

(defparameter *qr-level-bits* '(:l 1 :m 0 :q 3 :h 2)
  "The two bits of each error correction level in the format information.")

(defun %qr-ec-spec (version level)
  "The error correction codewords per block and the block groups of `VERSION' at `LEVEL'."
  (let ((spec (getf (rest (assoc version *qr-ec-blocks*)) level)))
    (values (first spec) (rest spec))))

(defun %qr-data-codewords (version level)
  (loop :for (blocks data) :in (nth-value 1 (%qr-ec-spec version level))
        :sum (* blocks data)))

(defun %qr-count-bits (version)
  "The bits of the character count in byte mode."
  (if (< version 10) 8 16))

(defun %qr-version-for (length level)
  "The smallest version whose data codewords hold `LENGTH' bytes at `LEVEL', or an error when none does."
  (or (loop :for version :from 1 :to 10
            :when (<= (+ 4 (%qr-count-bits version) (* 8 length))
                      (* 8 (%qr-data-codewords version level)))
              :return version)
      (error "~d bytes do not fit a QR code of version 10 at level ~a" length level)))

;; -----------------------------
;; Reed-Solomon over GF(256)
;; -----------------------------

(defun %gf-multiply (x y)
  "The product in GF(2^8) with the standard's polynomial x^8 + x^4 + x^3 + x^2 + 1."
  (let ((z 0))
    (loop :for i :from 7 :downto 0
          :do (setf z (logxor (ash z 1) (* (ldb (byte 1 7) z) #x11D)))
              (when (logbitp i y)
                (setf z (logxor z x))))
    z))

(defun %rs-generator (degree)
  "The coefficients of the generator polynomial of `DEGREE', highest power first, leading 1 left out."
  (let ((result (make-array degree :element-type '(unsigned-byte 8) :initial-element 0))
        (root 1))
    (setf (aref result (1- degree)) 1)
    (dotimes (i degree)
      (loop :for j :below degree
            :do (setf (aref result j)
                      (logxor (%gf-multiply (aref result j) root)
                              (if (< (1+ j) degree) (aref result (1+ j)) 0))))
      (setf root (%gf-multiply root 2)))
    result))

(defun %rs-remainder (data generator)
  "The error correction codewords of `DATA' for `GENERATOR'."
  (let ((result (make-array (length generator) :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop :for b :across data
          :do (let ((factor (logxor b (aref result 0))))
                (replace result result :start2 1)
                (setf (aref result (1- (length result))) 0)
                (loop :for j :below (length result)
                      :do (setf (aref result j)
                                (logxor (aref result j) (%gf-multiply (aref generator j) factor))))))
    result))

;; -----------------------------
;; the data codewords
;; -----------------------------

(defun %qr-data-bits (octets version level)
  "The data codewords of `OCTETS' in byte mode: mode, count, data, terminator, padding."
  (let* ((capacity (* 8 (%qr-data-codewords version level)))
         (bits (make-array capacity :element-type 'bit :initial-element 0))
         (count 0))
    ;; no fill pointers on specialised arrays: not every Lisp has them
    (flet ((append-bits (value n)
             (loop :for i :from (1- n) :downto 0
                   :do (setf (aref bits count) (ldb (byte 1 i) value))
                       (incf count))))
      (append-bits #b0100 4)
      (append-bits (length octets) (%qr-count-bits version))
      (loop :for b :across octets :do (append-bits b 8))
      (append-bits 0 (min 4 (- capacity count)))
      (append-bits 0 (mod (- count) 8))
      (loop :for pad := #xEC :then (if (= pad #xEC) #x11 #xEC)
            :while (< count capacity)
            :do (append-bits pad 8)))
    (let ((codewords (make-array (/ capacity 8) :element-type '(unsigned-byte 8))))
      (dotimes (i (length codewords) codewords)
        (setf (aref codewords i)
              (loop :for j :below 8
                    :sum (ash (aref bits (+ (* 8 i) j)) (- 7 j))))))))

(defun %qr-codewords (data version level)
  "The final sequence of codewords: the data of every block and then the error correction of every block, interleaved."
  (multiple-value-bind (ec-count groups) (%qr-ec-spec version level)
    (let* ((generator (%rs-generator ec-count))
           (blocks (loop :with start := 0
                         :for (count size) :in groups
                         :append (loop :repeat count
                                       :collect (prog1 (subseq data start (+ start size))
                                                  (incf start size)))))
           (ecs (mapcar (lambda (block) (%rs-remainder block generator)) blocks))
           (longest (reduce #'max blocks :key #'length))
           (result (make-array (+ (length data) (* ec-count (length blocks)))
                               :element-type '(unsigned-byte 8)))
           (k 0))
      (flet ((out (codeword)
               (setf (aref result k) codeword)
               (incf k)))
        (dotimes (i longest)
          (dolist (block blocks)
            (when (< i (length block))
              (out (aref block i)))))
        (dotimes (i ec-count)
          (dolist (ec ecs)
            (out (aref ec i)))))
      result)))

;; -----------------------------
;; the symbol
;; -----------------------------

(defstruct (qr (:constructor %make-qr))
  "A QR symbol being drawn: VERSION, SIZE, the MODULES (1 dark) and which of them are FUNCTION patterns, not data."
  version size modules function)

(defun %qr-set (qr x y dark &key (function t))
  "Sets the module in column `X', row `Y' of `QR'; a `FUNCTION' module is kept from the data and the mask."
  (setf (aref (qr-modules qr) y x) (if dark 1 0))
  (when function
    (setf (aref (qr-function qr) y x) 1)))

(defun %qr-function-p (qr x y)
  (= 1 (aref (qr-function qr) y x)))

(defun %qr-draw-finder (qr cx cy)
  "The finder pattern centred on (`CX', `CY') with its separator."
  (loop :for dy :from -4 :to 4
        :do (loop :for dx :from -4 :to 4
                  :do (let ((x (+ cx dx)) (y (+ cy dy))
                            (distance (max (abs dx) (abs dy))))
                        (when (and (<= 0 x (1- (qr-size qr))) (<= 0 y (1- (qr-size qr))))
                          (%qr-set qr x y (and (/= distance 2) (/= distance 4))))))))

(defun %qr-draw-alignment (qr cx cy)
  (loop :for dy :from -2 :to 2
        :do (loop :for dx :from -2 :to 2
                  :do (%qr-set qr (+ cx dx) (+ cy dy) (/= 1 (max (abs dx) (abs dy)))))))

(defun %qr-format-bits (level mask)
  "The 15 bits of the format information: level and mask with their BCH code, masked."
  (let* ((data (logior (ash (getf *qr-level-bits* level) 3) mask))
         (remainder data))
    (dotimes (i 10)
      (setf remainder (logxor (ash remainder 1) (* (ldb (byte 1 9) remainder) #x537))))
    (logxor (logior (ash data 10) remainder) #x5412)))

(defun %qr-draw-format (qr level mask)
  "The format information, twice, and the dark module."
  (let ((bits (%qr-format-bits level mask))
        (size (qr-size qr)))
    (flet ((dark-p (i) (logbitp i bits)))
      (loop :for i :from 0 :to 5 :do (%qr-set qr 8 i (dark-p i)))
      (%qr-set qr 8 7 (dark-p 6))
      (%qr-set qr 8 8 (dark-p 7))
      (%qr-set qr 7 8 (dark-p 8))
      (loop :for i :from 9 :to 14 :do (%qr-set qr (- 14 i) 8 (dark-p i)))
      (loop :for i :from 0 :to 7 :do (%qr-set qr (- size 1 i) 8 (dark-p i)))
      (loop :for i :from 8 :to 14 :do (%qr-set qr 8 (+ (- size 15) i) (dark-p i)))
      (%qr-set qr 8 (- size 8) t))))

(defun %qr-version-bits (version)
  "The 18 bits of the version information of `VERSION' (7 and up)."
  (let ((remainder version))
    (dotimes (i 12)
      (setf remainder (logxor (ash remainder 1) (* (ldb (byte 1 11) remainder) #x1F25))))
    (logior (ash version 12) remainder)))

(defun %qr-draw-version (qr)
  (when (>= (qr-version qr) 7)
    (let ((bits (%qr-version-bits (qr-version qr)))
          (size (qr-size qr)))
      (dotimes (i 18)
        (let ((a (+ (- size 11) (mod i 3)))
              (b (floor i 3))
              (dark (logbitp i bits)))
          (%qr-set qr a b dark)
          (%qr-set qr b a dark))))))

(defun %qr-draw-function-patterns (qr level)
  "Finders, timing, alignment, and the format and version areas, reserved with mask 0 for now."
  (let ((size (qr-size qr)))
    (loop :for i :below size
          :do (%qr-set qr 6 i (evenp i))
              (%qr-set qr i 6 (evenp i)))
    (%qr-draw-finder qr 3 3)
    (%qr-draw-finder qr (- size 4) 3)
    (%qr-draw-finder qr 3 (- size 4))
    (let* ((positions (rest (assoc (qr-version qr) *qr-alignment-positions*)))
           (last (car (last positions))))
      (dolist (cx positions)
        (dolist (cy positions)
          (unless (or (and (= cx 6) (= cy 6))
                      (and (= cx 6) (= cy last))
                      (and (= cx last) (= cy 6)))
            (%qr-draw-alignment qr cx cy)))))
    (%qr-draw-format qr level 0)
    (%qr-draw-version qr)))

(defun %qr-draw-codewords (qr codewords)
  "Places the bits of `CODEWORDS' in the zigzag the standard walks, skipping function modules."
  (let ((size (qr-size qr))
        (i 0)
        (total (* 8 (length codewords))))
    (loop :for right :from (1- size) :downto 1 :by 2
          :do (when (= right 6) (setf right 5))
              (dotimes (vert size)
                (dotimes (j 2)
                  (let* ((x (- right j))
                         (upward (zerop (logand (1+ right) 2)))
                         (y (if upward (- size 1 vert) vert)))
                    (when (and (not (%qr-function-p qr x y)) (< i total))
                      (%qr-set qr x y (logbitp (- 7 (logand i 7)) (aref codewords (ash i -3))) :function nil)
                      (incf i))))))))

(defun %qr-mask-p (mask x y)
  "Whether the mask pattern `MASK' flips the module in column `X', row `Y'."
  (zerop (ecase mask
           (0 (mod (+ x y) 2))
           (1 (mod y 2))
           (2 (mod x 3))
           (3 (mod (+ x y) 3))
           (4 (mod (+ (floor x 3) (floor y 2)) 2))
           (5 (+ (mod (* x y) 2) (mod (* x y) 3)))
           (6 (mod (+ (mod (* x y) 2) (mod (* x y) 3)) 2))
           (7 (mod (+ (mod (+ x y) 2) (mod (* x y) 3)) 2)))))

(defun %qr-apply-mask (qr mask)
  "Flips the data modules `MASK' selects; applying it twice undoes it."
  (let ((size (qr-size qr)))
    (dotimes (y size)
      (dotimes (x size)
        (when (and (not (%qr-function-p qr x y)) (%qr-mask-p mask x y))
          (setf (aref (qr-modules qr) y x) (- 1 (aref (qr-modules qr) y x))))))))

(defun %qr-penalty (qr)
  "The penalty score of the symbol as masked: runs, blocks, finder-like patterns, and the balance of dark and light."
  (let* ((size (qr-size qr))
         (modules (qr-modules qr))
         (score 0)
         (dark 0))
    (flet ((line-penalty (at)
             ;; AT maps an index to a module; runs of five and more, and
             ;; the 1:1:3:1:1 finder pattern with four light modules beside it
             (let ((run-color -1) (run-length 0) (window 0))
               (dotimes (i size)
                 (let ((module (funcall at i)))
                   (cond ((= module run-color) (incf run-length)
                          (when (= run-length 5) (incf score 3))
                          (when (> run-length 5) (incf score 1)))
                         (t (setf run-color module run-length 1)))
                   (setf window (logior (ash (logand window #x3FF) 1) module))
                   (when (and (>= i 10) (or (= window #x5D0) (= window #x05D)))
                     (incf score 40)))))))
      (dotimes (y size)
        (line-penalty (lambda (x) (aref modules y x))))
      (dotimes (x size)
        (line-penalty (lambda (y) (aref modules y x)))))
    (dotimes (y (1- size))
      (dotimes (x (1- size))
        (let ((color (aref modules y x)))
          (when (and (= color (aref modules y (1+ x)))
                     (= color (aref modules (1+ y) x))
                     (= color (aref modules (1+ y) (1+ x))))
            (incf score 3)))))
    (dotimes (y size)
      (dotimes (x size)
        (incf dark (aref modules y x))))
    (let* ((total (* size size))
           (k (1- (ceiling (abs (- (* dark 20) (* total 10))) total))))
      (incf score (* 10 (max k 0))))
    score))

(defun qr-encode (string &key (level :m) mask)
  "The QR code of `STRING' (UTF-8, byte mode) as a square bit array of modules, 1 for dark, in the smallest version from 1 to 10 that holds it at error correction `LEVEL' (:l, :m, :q or :h). The mask pattern with the lowest penalty is used unless `MASK' (0 to 7) names one. Second value: the version."
  (check-type level (member :l :m :q :h))
  (let* ((octets (babel:string-to-octets string :encoding :utf-8))
         (version (%qr-version-for (length octets) level))
         (size (+ 17 (* 4 version)))
         (qr (%make-qr :version version :size size
                       :modules (make-array (list size size) :element-type 'bit :initial-element 0)
                       :function (make-array (list size size) :element-type 'bit :initial-element 0))))
    (%qr-draw-function-patterns qr level)
    (%qr-draw-codewords qr (%qr-codewords (%qr-data-bits octets version level) version level))
    (let ((chosen (or mask
                      (loop :with best := nil :with best-score := nil
                            :for candidate :from 0 :to 7
                            :do (%qr-apply-mask qr candidate)
                                (%qr-draw-format qr level candidate)
                                (let ((score (%qr-penalty qr)))
                                  (when (or (null best-score) (< score best-score))
                                    (setf best candidate best-score score)))
                                (%qr-apply-mask qr candidate)
                            :finally (return best)))))
      (%qr-apply-mask qr chosen)
      (%qr-draw-format qr level chosen)
      (values (qr-modules qr) version))))

(defun qr-svg (modules &key (quiet-zone 4) (dark "#000") (light "#fff"))
  "`MODULES', as `qr-encode' returns them, as an SVG image of unit modules with `QUIET-ZONE' light modules around, scaling to its container."
  (let* ((size (array-dimension modules 0))
         (total (+ size (* 2 quiet-zone))))
    (with-output-to-string (out)
      (format out "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~d ~d\" shape-rendering=\"crispEdges\" role=\"img\" aria-label=\"QR code\">"
              total total)
      (format out "<rect width=\"~d\" height=\"~d\" fill=\"~a\"/>" total total light)
      (format out "<path fill=\"~a\" d=\"" dark)
      (dotimes (y size)
        (dotimes (x size)
          (when (= 1 (aref modules y x))
            (format out "M~d ~dh1v1h-1z" (+ x quiet-zone) (+ y quiet-zone)))))
      (format out "\"/></svg>"))))

(defun qr-string (modules &key (dark #\#) (light #\.))
  "`MODULES' as lines of `DARK' and `LIGHT' characters, for the REPL and the tests."
  (let ((size (array-dimension modules 0)))
    (with-output-to-string (out)
      (dotimes (y size)
        (dotimes (x size)
          (write-char (if (= 1 (aref modules y x)) dark light) out))
        (terpri out)))))
