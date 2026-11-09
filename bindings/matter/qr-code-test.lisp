(in-package :chipi.binding.matter-test)

(def-suite qr-code-tests
  :description "The QR code encoder, against symbols node-qrcode made of the same texts"
  :in chipi.binding.matter-test-suite:test-suite)

(in-suite qr-code-tests)

;; The expected symbols come from node-qrcode 1.x (`QRCode.create' with a
;; byte-mode segment, the level and, where given, the mask pattern), one
;; line per row, # dark.

(defparameter *hello-world-m-mask-0*
  "#######..##.#.#######
#.....#.##..#.#.....#
#.###.#.....#.#.###.#
#.###.#...##..#.###.#
#.###.#.##..#.#.###.#
#.....#..#..#.#.....#
#######.#.#.#.#######
..........###........
#.#.#.#..#.#....#..#.
#.#..#...##...##...#.
#...#.#####.##.######
#.##...####.....#..#.
#.##..###...#####.#..
........####.#....##.
#######...##...##.###
#.....#..####..#....#
#.###.#.####..#.#.#..
#.###.#....#..###.##.
#.###.#.#.#.#.#.#.#.#
#.....#...##....#..#.
#######.##.##.##..###
")

(defparameter *hello-world-q*
  "#######.####..#######
#.....#..####.#.....#
#.###.#..##...#.###.#
#.###.#...#.#.#.###.#
#.###.#.#.#.#.#.###.#
#.....#.##.##.#.....#
#######.#.#.#.#######
.....................
.#######.##.#..##...#
#####..#.#.#####.##..
..###.#.#..####..###.
.#.##...####.#..###..
.##...#..#.#.#....#.#
........####.....#...
#######.#.#...#...##.
#.....#.###..#.#.####
#.###.#.#...#..#..#.#
#.###.#.#.#.######...
#.###.#.##..#..#..#..
#.....#.#...##..###..
#######..#.##...#.##.
")

(defparameter *pairing-payload-m*
  "#######..####.#...#######
#.....#..####.##..#.....#
#.###.#.##......#.#.###.#
#.###.#.#.#..#.##.#.###.#
#.###.#.##.####...#.###.#
#.....#.#.#....#..#.....#
#######.#.#.#.#.#.#######
........#..#...#.........
#.#####...###.###.#####..
##.#...##.##......##.###.
.##..####.##.#######.##.#
.#.#.#.###.#..##.....#..#
#.#.####.###.#.#.##..####
#....#...##.#.###.##..###
#.#..#####.....###.....##
#.#..#....#.#........#.##
#...######..##..#####.##.
........#.#...###...##..#
#######....##..##.#.#####
#.....#.##.#....#...##...
#.###.#.##.#...######.###
#.###.#.##..##.#..###..##
#.###.#.#....###.#...#..#
#.....#.....##..#.#.#...#
#######.#.#.#.##..#.##.##
")

(defparameter *hundred-forty-a-m*
  "#######.##....#.####...##....###...##...#.#######
#.....#.#.####..####...#.##..#..#..#.####.#.....#
#.###.#....###.###....##.#..#..#..#..#.##.#.###.#
#.###.#.##..#..##.##....##...###...###.#..#.###.#
#.###.#.....#.##.#..#.######...###........#.###.#
#.....#...#.#####....##...#.#..#..#..##...#.....#
#######.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#######
........#.#....##...#.#...#..#..#..#..#..........
#.##.###..##.#..####..########...###....#.#..#.##
.##.....###..##.####.#.###...###...###...###.#..#
..#######....#.##.####.#..#..#..#..#..##.#..##...
.#.#.#.#..##.#...#.#.##..#..#..#..#..#..#..#.###.
.....######.#.###.#..#.###...###...###.#.###.#..#
#...##.#.#....##..##.##..###...###...###...##..#.
#.....#..#.#.##...###....#..#..#..#..#.##..#.###.
..###...###..#..#.###.#.#..#..#..#..#..#..#....##
#.#.#.###.....#.#.#.#....####..###...##....##..#.
.####...###......#....##.....#...###...###....#..
....#.#....#..#..#.#....#...#.#..#..#.....#....##
#..#.#..####.####.#.##.#.#...#..#..#..#..#..##...
#...#####.##.#######..##.#####...###....##....#..
#.#.##...#.#..#.##.#.#.####..###...###...###.#..#
##.########........##.#####..#..#..#..########...
#...#...#..#..##.#.#.##...#.#..#..#..#..#...####.
..#.#.#.###..#.......##.#.#..###...###.##.#.##..#
##.##...##.#...#.#.####...##...###...##.#...#..#.
#..########...###.....#####.#..#..#..#..########.
..##...#..####.#.##..#..#..#..#..#..#..##.#....##
....####........#.###..#..##...###...##.##..#..#.
##.....#...##..#.#.#...##..###...###....##....#..
.#....#..#......##.#.##..#.#..#..#..#..#.###...##
##..#..#.###.##.##..#...#.#..#..#..#..#.##..##.#.
..#...###...####.#.#..#..#..##...###.......#..###
#.#..#..#.#........#.#####...###...###.#.###.#..#
...##.####.#...#.#.##....##..#..#..#..#....###...
.#####.####..#.....#.....#..#..#..#..#.....#.###.
##..#.#..#.##...###..#..#....###...###.##.#..#..#
....#..#.###..##...##.#..###...###...##....##..#.
.#...###.###.##..#.....#....#..#..#..#..##...###.
.###...#.#.#..#.#....#..#..#..#..#..#..##.#....##
###...#..#.#....##..#.######...###...##.#####..#.
........##.#.#..###...#...####...###...##...#.#..
#######.#..#.#..#.##..#.#.##..#..#..#..##.#.#..##
#.....#.####...#..#.#.#...#..#..#..#..#.#...##...
#.###.#..#....##..##.#########...###....#####.#..
#.###.#.#..#..########..#....###...###.#..#..#..#
#.###.#.###....#.##......##..#..#..#..##...###.##
#.....#.....#.##.##.####.#..#..#..#..#.###...##..
#######.##...##.........##...###...###.#..#..#.##
")

(test qr-encode--matches-the-reference-symbols
  "The symbols node-qrcode makes of the same texts, module for module: the pairing payload at version 2 with the mask chosen by the penalty rules, a version 1 with the mask given and with the mask chosen, and a version 8 with version information."
  (multiple-value-bind (modules version) (qr-encode "HELLO WORLD" :level :m :mask 0)
    (is (= 1 version))
    (is (string= *hello-world-m-mask-0* (qr-string modules))))
  (multiple-value-bind (modules version) (qr-encode "HELLO WORLD" :level :q)
    (is (= 1 version))
    (is (string= *hello-world-q* (qr-string modules))))
  (multiple-value-bind (modules version) (qr-encode "MT:-24J0AFN00KA0648G00")
    (is (= 2 version))
    (is (string= *pairing-payload-m* (qr-string modules))))
  (multiple-value-bind (modules version) (qr-encode (make-string 140 :initial-element #\A) :level :m)
    (is (= 8 version))
    (is (string= *hundred-forty-a-m* (qr-string modules)))))

(test qr-encode--version-grows-with-the-text-and-the-level
  (flet ((version (length level)
           (nth-value 1 (qr-encode (make-string length :initial-element #\x) :level level))))
    ;; version 1 holds 19 data codewords at L, 16 at M; mode and count take two of them
    (is (= 1 (version 17 :l)))
    (is (= 2 (version 18 :l)))
    (is (= 1 (version 14 :m)))
    (is (= 2 (version 15 :m)))
    (is (= 1 (version 7 :h)))
    (is (= 2 (version 8 :h)))
    ;; version 10 takes a 16-bit count
    (is (= 10 (version 271 :l)))
    (signals error (version 272 :l))
    (signals error (qr-encode "x" :level :x))))

(test qr-encode--utf-8-is-byte-mode
  (multiple-value-bind (modules version) (qr-encode "Grüße" :level :l)
    (is (= 1 version))
    (is (= 21 (array-dimension modules 0)))))

(test qr-format-and-version-bits--the-standard's-values
  ;; table C.1 of ISO/IEC 18004: the format information of each level with mask 0, and M with mask 1
  (is (= #x5412 (binding-matter::%qr-format-bits :m 0)))
  (is (= #x77C4 (binding-matter::%qr-format-bits :l 0)))
  (is (= #x355F (binding-matter::%qr-format-bits :q 0)))
  (is (= #x1689 (binding-matter::%qr-format-bits :h 0)))
  (is (= #x5125 (binding-matter::%qr-format-bits :m 1)))
  ;; table D.1: the version information of versions 7 and 10
  (is (= #x07C94 (binding-matter::%qr-version-bits 7)))
  (is (= #x0A4D3 (binding-matter::%qr-version-bits 10))))

(test qr-svg--a-square-of-unit-modules-with-a-quiet-zone
  (let ((modules (make-array '(2 2) :element-type 'bit :initial-contents '((1 0) (0 1)))))
    (let ((svg (qr-svg modules)))
      (is (string= "<svg" (subseq svg 0 4)))
      (is-true (search "viewBox=\"0 0 10 10\"" svg))
      (is-true (search "M4 4h1v1h-1z" svg))
      (is-true (search "M5 5h1v1h-1z" svg))
      (is-false (search "M5 4h1v1h-1z" svg)))
    (let ((svg (qr-svg modules :quiet-zone 0 :dark "#123")))
      (is-true (search "viewBox=\"0 0 2 2\"" svg))
      (is-true (search "fill=\"#123\"" svg))
      (is-true (search "M0 0h1v1h-1z" svg)))))

(test qr-string--rows-of-characters
  (let ((modules (make-array '(2 2) :element-type 'bit :initial-contents '((1 0) (0 1)))))
    (is (string= (format nil "#.~%.#~%") (qr-string modules)))
    (is (string= (format nil "X ~% X~%") (qr-string modules :dark #\X :light #\Space)))))
