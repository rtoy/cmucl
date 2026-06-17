;;; -*- Mode: Lisp; Package: UNICODE-COLLATION-TESTS -*-
;;;
;;; Conformance tests for the UTS #10 collation implementation in
;;; code/unicode-collation.lisp, run against the UCA conformance data
;;; file CollationTest_SHIFTED.txt.

(defpackage :unicode-collation-tests
  (:use :cl :lisp-unit))

(in-package "UNICODE-COLLATION-TESTS")

(defvar *collation-allkeys* "target:i18n/allkeys.txt")
(defvar *collation-shifted-test*
  "target:i18n/CollationTest/CollationTest_SHIFTED.txt")
(defvar *collation-non-ignorable-test*
  "target:i18n/CollationTest/CollationTest_NON_IGNORABLE.txt")

(defvar *ducet* nil
  "The Default Unicode Collation Element Table, loaded on first use.")

(defun ducet ()
  "Return the DUCET, built from the collation section of unidata.bin on
first use."
  (or *ducet*
      (setf *ducet* (lisp::unidata-ducet))))

(defun collation-hex-list (string)
  "Parse all space-separated hexadecimal numbers in STRING into a list of
integers, in order.  Non-hex runs are skipped."
  (let ((result nil)
        (i 0)
        (n (length string)))
    (loop
      ;; Skip any non-hexadecimal characters.
      (loop while (and (< i n)
                       (null (digit-char-p (char string i) 16)))
            do (incf i))
      (when (>= i n) (return))
      ;; Accumulate one hexadecimal number.  PARSE-INTEGER is avoided
      ;; here because it conses, and this runs several times per line
      ;; over hundreds of thousands of conformance lines; the values are
      ;; 16-bit and fit in a fixnum.
      (let ((val 0)
            (d nil))
        (loop while (and (< i n)
                         (setf d (digit-char-p (char string i) 16)))
              do (setf val (+ (* val 16) d))
                 (incf i))
        (push val result)))
    (nreverse result)))

(defun collation-split-on-bar (string)
  "Split STRING into a list of substrings on the #\\| character."
  (let ((result nil) (start 0))
    (loop
      (let ((pos (position #\| string :start start)))
        (push (subseq string start (or pos (length string))) result)
        (if pos (setf start (1+ pos)) (return))))
    (nreverse result)))

(defun collation-parse-expected-key (comment)
  "Parse the trailing [L1 | L2 | L3 | L4 |] sort key from the comment
portion of a CollationTest line.  Returns four values: the L1, L2, L3
and L4 weight lists."
  (let ((lb (position #\[ comment :from-end t))
        (rb (position #\] comment :from-end t)))
    (if (and lb rb (< lb rb))
        (let* ((inner (subseq comment (1+ lb) rb))
               (parts (collation-split-on-bar inner)))
          (values (collation-hex-list (or (nth 0 parts) ""))
                  (collation-hex-list (or (nth 1 parts) ""))
                  (collation-hex-list (or (nth 2 parts) ""))
                  (collation-hex-list (or (nth 3 parts) ""))))
        (values nil nil nil nil))))

(defun collation-parse-test-line (line)
  "Parse a CollationTest data LINE.  Returns five values -- the codepoint
list and the four expected weight levels -- or NIL for comment or blank
lines."
  (when (and (plusp (length line))
             (not (char= (char line 0) #\#))
             (not (char= (char line 0) #\@)))
    (let ((semi (position #\; line)))
      (when semi
        (let ((cps (collation-hex-list (subseq line 0 semi))))
          (when cps
            (multiple-value-bind (l1 l2 l3 l4)
                (collation-parse-expected-key (subseq line (1+ semi)))
              (values cps l1 l2 l3 l4))))))))

(defun collation-test-string (codepoints)
  "Build a string from a list of CODEPOINTS, encoding codepoints outside
the BMP as UTF-16 surrogate pairs."
  (let ((out (make-array 10 :fill-pointer 0 :element-type 'character)))
    (dolist (cp codepoints)
      (if (> cp #xffff)
          (let ((s (lisp::codepoints-string (list cp))))
            (vector-push-extend (aref s 0) out)
            (vector-push-extend (aref s 1) out))
          (vector-push-extend (code-char cp) out)))
    (coerce out 'simple-string)))

(defun run-collation-conformance (ducet file weighting)
  "Check every line of the UCA conformance FILE: the four sort-key levels
produced by LISP::COLLATION-WEIGHTS under WEIGHTING must match the
expected key parsed from the line's comment.  Each line is a separate
LISP-UNIT assertion.

This is a plain function rather than inline in the DEFINE-TESTs below
because a DEFINE-TEST body is stored as source and run interpreted; the
per-line work over a quarter-million lines must run compiled, so it
lives here and the tests just call it."
  (with-open-file (s file :direction :input :external-format :utf-8)
    (loop for line = (read-line s nil nil)
          while line
          do
             (multiple-value-bind (cps e1 e2 e3 e4)
                 (collation-parse-test-line line)
               (when cps
                 (multiple-value-bind (g1 g2 g3 g4)
                     (lisp::collation-weights ducet (collation-test-string cps)
                                              weighting)
                   ;; For :NON-IGNORABLE the comment has no fourth level
                   ;; and COLLATION-WEIGHTS returns NIL for L4, so the
                   ;; same four-level comparison serves both options.
                   (assert-equalp (list e1 e2 e3 e4)
                                  (list g1 g2 g3 g4)
                                  cps)))))))

(define-test unicode.collation-shifted
  "Test UTS #10 collation sort keys against the UCA SHIFTED conformance
data.  For each line, the four sort-key levels produced by
LISP::COLLATION-WEIGHTS must match the expected key in the line's
comment."
  (:tag :unicode)
  (run-collation-conformance (ducet) *collation-shifted-test* :shifted))

(define-test unicode.collation-non-ignorable
  "Test UTS #10 collation sort keys against the UCA NON_IGNORABLE
conformance data.  Under the Non-ignorable option variable elements keep
their weights and there is no fourth level, so for each line the three
weight levels produced by LISP::COLLATION-WEIGHTS with :NON-IGNORABLE
must match the expected key in the line's comment."
  (:tag :unicode)
  (run-collation-conformance (ducet) *collation-non-ignorable-test*
                             :non-ignorable))

;; A DEFINE-TEST body is stored as source and run interpreted, and the
;; test runner (tests/run-tests.lisp) loads this file as source, so its
;; functions would otherwise run interpreted.  The per-line parsing and
;; string building run on every one of several hundred thousand
;; conformance lines, so interpreted they make the suite about ten times
;; slower.  Compile the hot functions on load.
(eval-when (:load-toplevel :execute)
  (dolist (name '(collation-hex-list
                  collation-split-on-bar
                  collation-parse-expected-key
                  collation-parse-test-line
                  collation-test-string
                  run-collation-conformance))
    (compile name)))
