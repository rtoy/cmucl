;;; -*- Mode: Lisp; Package: Lisp -*-
;;;
;;; Parser and in-memory representation of the Default Unicode Collation
;;; Element Table (DUCET, allkeys.txt) for UTS #10 collation.
;;;
;;; This is the data layer only: it reads allkeys.txt into memory and
;;; provides lookup.  Sort-key construction and serialization into
;;; unidata.bin are separate, later steps.

(in-package "LISP")

;;; A collation element is (primary secondary tertiary . variable-p),
;;; packed into a fixnum: primary in bits 17-32, secondary in bits 9-16,
;;; tertiary in bits 1-8, variable flag in bit 0.  Primaries and
;;; secondaries are 16-bit in the DUCET (max #xFFFD); tertiaries are
;;; small (<= #xFF).  We keep primary full-width and pack the rest.
;;;
;;; Layout (low to high):
;;;   bit 0       : variable-p
;;;   bits 1-8    : tertiary  (8 bits)
;;;   bits 9-24   : secondary (16 bits)
;;;   bits 25-40  : primary   (16 bits)
;;; That needs 41 bits; on a 32-bit build that is a bignum.  To stay in
;;; fixnum range we instead store each element as a simple-vector
;;; #(primary secondary tertiary variable-p).  Sort-key construction
;;; reads the levels separately anyway, so a vector is clearer and
;;; avoids bignum boxing.

(defstruct (collation-element
            (:constructor make-ce (primary secondary tertiary variable-p))
            (:conc-name ce-))
  (primary 0 :type (unsigned-byte 16))
  (secondary 0 :type (unsigned-byte 16))
  (tertiary 0 :type (unsigned-byte 16))
  (variable-p nil :type (member t nil)))

;;; An implicit-weight range: codepoints in [START, END] derive their
;;; collation elements algorithmically (UTS #10 section 10.1), based at
;;; the primary BASE given by the @implicitweights directive.
(defstruct (implicit-range (:constructor make-implicit-range (start end base)))
  (start 0 :type codepoint)
  (end 0 :type codepoint)
  (base 0 :type (unsigned-byte 16))
  ;; The origin from which the BBBB offset is measured: the smallest
  ;; START among all ranges that share this BASE.  Ranges with a common
  ;; base (e.g. Tangut and Tangut Supplement, both FB00) form one
  ;; continuous offset space, so the offset is (code - base-origin), not
  ;; (code - start).  Filled in by LOAD-DUCET after all ranges are read.
  (base-origin 0 :type codepoint))

;;; The whole table.
(defstruct (ducet (:conc-name ducet-))
  (version nil)
  ;; Map from a key (a simple-vector of codepoints) to a simple-vector
  ;; of collation-elements.  Single characters and contractions share
  ;; this table; the key length distinguishes them.
  (map (make-hash-table :test 'equalp) :type hash-table)
  ;; Fast paths derived from MAP at load time.  SINGLE maps a single
  ;; codepoint (a fixnum) directly to its collation elements, avoiding an
  ;; EQUALP probe with a one-element key for the common case.  STARTERS
  ;; holds every codepoint that begins some multi-codepoint key; a
  ;; codepoint not in STARTERS can neither begin a contraction nor be the
  ;; base of a discontiguous match, so the contraction machinery is
  ;; skipped for it entirely.
  (single (make-hash-table :test 'eql) :type hash-table)
  (starters (make-hash-table :test 'eql) :type hash-table)
  ;; Implicit-weight ranges, in file order.
  (implicit-ranges nil :type list)
  ;; Longest key (in codepoints) present in MAP; bounds the contraction
  ;; match during lookup.
  (max-key-length 1 :type fixnum)
  ;; Largest primary weight on any variable (*) element.  Needed for the
  ;; "variable weighting" step of sort-key construction.
  (max-variable-primary 0 :type (unsigned-byte 16)))

(defun parse-collation-element (str start end)
  "Parse one collation element [.pppp.ssss.tttt] or [*pppp.ssss.tttt]
from STR between START and END (the brackets), returning a
COLLATION-ELEMENT."
  ;; STR[start] is #\[ ; STR[start+1] is #\. or #\* ; then three
  ;; period-separated hex fields, then #\].
  (let* ((variable-p (char= (char str (1+ start)) #\*))
         (p1 (+ start 2))
         (dot1 (position #\. str :start p1 :end end))
         (dot2 (position #\. str :start (1+ dot1) :end end))
         (primary (parse-integer str :start p1 :end dot1 :radix 16))
         (secondary (parse-integer str :start (1+ dot1) :end dot2 :radix 16))
         (tertiary (parse-integer str :start (1+ dot2) :end (1- end)
                                  :radix 16)))
    (make-ce primary secondary tertiary variable-p)))

(defun parse-collation-elements (str)
  "Parse the run of [....] collation elements in STR (the part of a
line after the semicolon, comment already stripped), returning a
simple-vector of COLLATION-ELEMENTs."
  (let ((result (make-array 4 :fill-pointer 0 :adjustable t))
        (i 0)
        (n (length str)))
    (loop
      (let ((open (position #\[ str :start i :end n)))
        (when (null open) (return))
        (let ((close (position #\] str :start open :end n)))
          (when (null close) (return))
          (vector-push-extend (parse-collation-element str open (1+ close))
                              result)
          (setf i (1+ close)))))
    (coerce result 'simple-vector)))

(defun parse-codepoint-key (str)
  "Parse the left-hand side of an allkeys.txt entry (space-separated
hex codepoints) into a simple-vector of codepoints."
  (let ((result (make-array 2 :fill-pointer 0 :adjustable t))
        (i 0)
        (n (length str)))
    (loop
      ;; Skip spaces.
      (loop while (and (< i n) (char= (char str i) #\Space)) do (incf i))
      (when (>= i n) (return))
      (let ((j i))
        (loop while (and (< j n)
                         (digit-char-p (char str j) 16))
              do (incf j))
        (when (= j i) (return))
        (vector-push-extend (parse-integer str :start i :end j :radix 16)
                            result)
        (setf i j)))
    (coerce result 'simple-vector)))

(defun parse-implicit-weights-directive (line)
  "Parse an @implicitweights directive line, e.g.
   @implicitweights 17000..18AFF; FB00 # Tangut ...
returning an IMPLICIT-RANGE, or NIL if it does not parse."
  ;; After the keyword: RANGE-START..RANGE-END ; BASE [# comment]
  (let* ((body (string-trim " " (subseq line (length "@implicitweights"))))
         (semi (position #\; body))
         (range (string-trim " " (subseq body 0 semi)))
         (dots (search ".." range))
         (start (parse-integer range :start 0 :end dots :radix 16))
         (end (parse-integer range :start (+ dots 2) :radix 16))
         (rest (subseq body (1+ semi)))
         (hash (position #\# rest))
         (base (parse-integer (string-trim " " (subseq rest 0 hash))
                              :radix 16)))
    (make-implicit-range start end base)))

(defun strip-comment (line)
  "Return LINE with any trailing # comment removed."
  (let ((hash (position #\# line)))
    (if hash (subseq line 0 hash) line)))

(defun load-ducet (pathname)
  "Read the DUCET (allkeys.txt) at PATHNAME into a DUCET structure."
  (let ((d (make-ducet)))
    (with-open-file (s pathname :direction :input :external-format :utf-8)
      (loop for line = (read-line s nil) while line do
        (let ((line (string-right-trim '(#\Return) line)))
          (cond
            ;; Blank line.
            ((zerop (length (string-trim " " line))))
            ;; @version directive.
            ((and (>= (length line) 8) (string= line "@version" :end1 8))
             (setf (ducet-version d)
                   (string-trim " " (subseq line 8))))
            ;; @implicitweights directive.
            ((and (>= (length line) 16)
                  (string= line "@implicitweights" :end1 16))
             (push (parse-implicit-weights-directive line)
                   (ducet-implicit-ranges d)))
            ;; Other @ directives: ignore for now.
            ((char= (char line 0) #\@))
            ;; Comment line.
            ((char= (char line 0) #\#))
            ;; Data line: KEY ; ELEMENTS # comment
            (t
             (let* ((nocmt (strip-comment line))
                    (semi (position #\; nocmt)))
               (when semi
                 (let* ((key (parse-codepoint-key (subseq nocmt 0 semi)))
                        (ces (parse-collation-elements (subseq nocmt (1+ semi)))))
                   (when (plusp (length key))
                     (setf (gethash key (ducet-map d)) ces)
                     (when (> (length key) (ducet-max-key-length d))
                       (setf (ducet-max-key-length d) (length key)))
                     ;; Track the largest variable primary.
                     (loop for ce across ces
                           when (and (ce-variable-p ce)
                                     (> (ce-primary ce)
                                        (ducet-max-variable-primary d)))
                           do (setf (ducet-max-variable-primary d)
                                    (ce-primary ce))))))))))))
    (setf (ducet-implicit-ranges d) (nreverse (ducet-implicit-ranges d)))
    ;; Build the fast-path tables: SINGLE for one-codepoint keys, and
    ;; STARTERS for the first codepoint of every multi-codepoint key.
    (maphash (lambda (key ces)
               (if (= (length key) 1)
                   (setf (gethash (aref key 0) (ducet-single d)) ces)
                   (setf (gethash (aref key 0) (ducet-starters d)) t)))
             (ducet-map d))
    ;; Compute, for each base, the smallest range start sharing it, and
    ;; record it as the BBBB offset origin on every range with that base.
    (let ((origin (make-hash-table)))
      (dolist (r (ducet-implicit-ranges d))
        (let ((b (implicit-range-base r)))
          (when (or (null (gethash b origin))
                    (< (implicit-range-start r) (gethash b origin)))
            (setf (gethash b origin) (implicit-range-start r)))))
      (dolist (r (ducet-implicit-ranges d))
        (setf (implicit-range-base-origin r)
              (gethash (implicit-range-base r) origin))))
    d))

;;; Lookup ------------------------------------------------------------

(defun ducet-lookup (d key)
  "Look up KEY (a simple-vector of one or more codepoints) in the DUCET
table D.  Returns the simple-vector of collation elements, or NIL if
KEY is not present (no contraction/implicit handling here -- that is
the caller's job during sort-key construction)."
  (gethash key (ducet-map d)))

(defun ducet-implicit-range-for (d code)
  "If CODE falls in one of the DUCET's @implicitweights ranges, return
that IMPLICIT-RANGE; otherwise NIL."
  (dolist (r (ducet-implicit-ranges d) nil)
    (when (<= (implicit-range-start r) code (implicit-range-end r))
      (return r))))

;;; -------------------------------------------------------------------
;;; Sort-key construction (UTS #10 Section 7, with the Shifted option
;;; for variable weighting).
;;;
;;; Pipeline: normalize to NFD (S1.1), produce the collation element
;;; array (S2), then form the sort key (S3).
;;;
;;; NOTE ON COMPLETENESS: this implements the contiguous longest-match
;;; rule (S2.1).  The discontiguous match for unblocked non-starters
;;; (S2.1.1-S2.1.3) is NOT yet implemented; it requires canonical
;;; combining-class lookup and only affects inputs where a base+mark
;;; contraction is interrupted by an intervening mark of lower combining
;;; class.  The conformance run will reveal whether any test cases need
;;; it; if so, it is the next piece to add.
;;; -------------------------------------------------------------------

(defun string-to-collation-codepoints (string)
  "Normalize STRING to NFD (UTS #10 step S1.1) and decode it into a
simple-vector of codepoints, collapsing UTF-16 surrogate pairs."
  (let* ((nfd (string-to-nfd string))
         (n (length nfd))
         (out (make-array (max 1 n) :fill-pointer 0 :adjustable t))
         (i 0))
    (loop while (< i n) do
      (multiple-value-bind (cp widep) (codepoint nfd i)
        (vector-push-extend cp out)
        (incf i (if (eql widep 1) 2 1))))
    (coerce out 'simple-vector)))

(defun derive-implicit-elements (d code)
  "Synthesize the collation elements for CODE when it has no explicit
DUCET entry (UTS #10 Section 10.1.3).  Returns a list of two
COLLATION-ELEMENTs."
  (let ((range (ducet-implicit-range-for d code)))
    (if range
        ;; @implicitweights range: AAAA = base; BBBB = (code - origin)|8000
        ;; where origin is shared across ranges with the same base.
        (let ((aaaa (implicit-range-base range))
              (bbbb (logior (- code (implicit-range-base-origin range)) #x8000)))
          (list (make-ce aaaa #x20 #x02 nil)
                (make-ce bbbb #x00 #x00 nil)))
        ;; Default implicit weights for Han and other code points
        ;; (Section 10.1).  The base depends on whether CODE is an
        ;; assigned unified ideograph: core ideographs use FB40,
        ;; ideographs in the extension blocks use FB80, and everything
        ;; else -- including unassigned code points that merely fall
        ;; inside an ideograph block -- uses FBC0.  Assignment is tested
        ;; via UNICODE-CATEGORY (zero = unassigned).
        (let* ((assigned (not (zerop (unicode-category code))))
               (base (cond ((and assigned
                                 (or (<= #x4E00 code #x9FFF)
                                     (<= #xF900 code #xFAFF)))
                            #xFB40)
                           ((and assigned
                                 (or (<= #x3400 code #x4DBF)
                                     (<= #x20000 code #x3FFFF)))
                            #xFB80)
                           (t #xFBC0)))
               (aaaa (+ base (ash code -15)))
               (bbbb (logior (logand code #x7FFF) #x8000)))
          (list (make-ce aaaa #x20 #x02 nil)
                (make-ce bbbb #x00 #x00 nil))))))

(defun collation-extend-key (key code)
  "Return a fresh codepoint key: the codepoints of KEY followed by CODE."
  (let* ((kl (length key))
         (new (make-array (1+ kl))))
    (dotimes (j kl) (setf (aref new j) (aref key j)))
    (setf (aref new kl) code)
    new))

(defun ducet-element-array (d cps)
  "Produce the collation element array for the (NFD-normalized) codepoint
vector CPS, per UTS #10 step S2, deriving implicit elements for
codepoints with no entry.  Returns a list of COLLATION-ELEMENTs in
order.

Implements the contiguous longest match (S2.1) and the discontiguous
match for unblocked non-starters (S2.1.1-S2.1.3).  After a match S, the
following non-starters are processed in order: a non-starter C is
unblocked when no still-present codepoint between S and C has a
canonical combining class greater than or equal to C's (a starter, ccc
0, blocks absolutely).  If C is unblocked and S+C is in the table, C is
folded into S and removed; otherwise C remains and, if unblocked, raises
the blocking threshold for later non-starters."
  (let ((result nil)
        (consumed (make-array (max 1 (length cps)) :initial-element nil))
        (i 0)
        (n (length cps))
        (maxlen (ducet-max-key-length d))
        (map (ducet-map d))
        (single (ducet-single d))
        (starters (ducet-starters d)))
    (loop while (< i n) do
      (cond
        ((aref consumed i) (incf i))    ; folded in by a discontiguous match
        (t
         (let* ((cp (aref cps i))
                (starter (gethash cp starters))
                (best-key nil) (best-len 0) (best-ces nil))
           ;; S2.1: longest contiguous match starting at I.  Only a
           ;; codepoint that begins some multi-codepoint key (a STARTER)
           ;; can match a contraction, so the multi-length scan is done
           ;; only for those; every other codepoint takes the SINGLE
           ;; fast path.  The scan rejects any span containing a position
           ;; already consumed by an earlier discontiguous fold.
           (when starter
             (loop for len from (min maxlen (- n i)) downto 2 do
               (when (loop for j from i below (+ i len)
                           never (aref consumed j))
                 (let* ((key (subseq cps i (+ i len)))
                        (ces (gethash key map)))
                   (when ces
                     (setf best-key key best-len len best-ces ces)
                     (return))))))
           (unless best-ces
             (let ((ces (gethash cp single)))
               (when ces
                 (setf best-ces ces best-len 1)
                 ;; BEST-KEY is needed only to extend by discontiguous
                 ;; folds, which apply only to starters.
                 (when starter
                   (setf best-key (make-array 1 :initial-element cp))))))
           (cond
             (best-ces
              ;; S2.1.1-S2.1.3: extend by unblocked following non-starters.
              ;; A codepoint that is not a STARTER can begin no multi-key,
              ;; so no fold is possible and the scan is skipped entirely.
              ;; MAXCCC is the highest combining class among still-present
              ;; codepoints passed since the end of the contiguous match.
              (when (and starter best-key)
                (let ((maxccc 0))
                  (loop for k from (+ i best-len) below n do
                    (unless (aref consumed k)
                      (let ((ccc (lisp::unicode-combining-class (aref cps k))))
                        (cond
                          ((zerop ccc)
                           (return))      ; starter blocks all further
                          ((> ccc maxccc) ; unblocked candidate
                           (let* ((cand (collation-extend-key
                                         best-key (aref cps k)))
                                  (ces (gethash cand map)))
                             (if ces
                                 ;; Fold C in and remove it; MAXCCC
                                 ;; unchanged since C is no longer present.
                                 (setf best-key cand
                                       best-ces ces
                                       (aref consumed k) t)
                                 ;; No match: C stays and becomes a blocker.
                                 (setf maxccc ccc))))
                          (t
                           ;; Blocked (ccc <= maxccc): C stays; MAXCCC
                           ;; already covers it.
                           nil)))))))
              (loop for ce across best-ces do (push ce result))
              (incf i best-len))
             (t
              ;; No entry: derive implicit weights (Section 10.1).
              (dolist (ce (derive-implicit-elements d cp))
                (push ce result))
              (incf i)))))))
    (nreverse result)))

(defun collation-weights (d string &optional (variable-weighting :shifted))
  "Return four values -- the level-1, level-2, level-3 and level-4 weight
lists for STRING under DUCET D.  VARIABLE-WEIGHTING selects the UTS #10
Section 4 option:

  :SHIFTED (the default) -- a variable element contributes nothing at
    levels 1-3 and its primary at level 4; a non-variable element takes
    a level-4 weight of FFFF when it carries level-2 or level-3 content,
    but none when it is a primary-only continuation element
    ([.XXXX.0000.0000], e.g. the second half of an implicit weight pair
    or an expansion tail); a completely ignorable element
    ([.0000.0000.0000]) contributes nothing anywhere; and a primary-
    ignorable element that follows a variable element is shifted away
    entirely.

  :NON-IGNORABLE -- variable elements are not treated specially: every
    element contributes its non-zero weights at levels 1-3 just like any
    other element, and there is no fourth level (the returned L4 is
    always NIL)."
  (ecase variable-weighting
    (:shifted (collation-weights-shifted d string))
    (:non-ignorable (collation-weights-non-ignorable d string))))

(defun collation-weights-shifted (d string)
  "Compute the four weight levels for STRING under the Shifted option.
See COLLATION-WEIGHTS."
  (let ((ces (ducet-element-array d (string-to-collation-codepoints string)))
        (l1 nil) (l2 nil) (l3 nil) (l4 nil)
        (after-variable nil))
    (dolist (ce ces)
      (let ((p (ce-primary ce))
            (s (ce-secondary ce))
            (te (ce-tertiary ce)))
        (cond
          ((ce-variable-p ce)
           (push p l4)
           (setf after-variable t))
          ((and (zerop p) (zerop s) (zerop te))
           ;; Completely ignorable: nothing, and does not reset the
           ;; after-variable state.
           )
          ((zerop p)
           ;; Primary-ignorable.
           (unless after-variable
             (unless (zerop s) (push s l2))
             (unless (zerop te) (push te l3))
             (push #xFFFF l4)))
          (t
           ;; Non-variable element with a non-zero primary.
           (push p l1)
           (unless (zerop s) (push s l2))
           (unless (zerop te) (push te l3))
           ;; Shifted quaternary: FFFF marks an element carrying level-2
           ;; or level-3 content.  A continuation element of the form
           ;; [.XXXX.0000.0000] -- the second element of an implicit
           ;; weight pair, or an expansion tail -- has only a primary and
           ;; takes no quaternary weight.
           (unless (and (zerop s) (zerop te))
             (push #xFFFF l4))
           (setf after-variable nil)))))
    (values (nreverse l1) (nreverse l2) (nreverse l3) (nreverse l4))))

(defun collation-weights-non-ignorable (d string)
  "Compute the weight levels for STRING under the Non-ignorable option.
Variable elements are treated exactly like ordinary elements, and there
is no fourth level.  See COLLATION-WEIGHTS."
  (let ((ces (ducet-element-array d (string-to-collation-codepoints string)))
        (l1 nil) (l2 nil) (l3 nil))
    (dolist (ce ces)
      (let ((p (ce-primary ce))
            (s (ce-secondary ce))
            (te (ce-tertiary ce)))
        ;; Every element -- variable or not -- contributes its non-zero
        ;; weights at each level; zero weights are passed over.
        (unless (zerop p) (push p l1))
        (unless (zerop s) (push s l2))
        (unless (zerop te) (push te l3))))
    (values (nreverse l1) (nreverse l2) (nreverse l3) nil)))

(defun collation-sort-key (d string &optional (variable-weighting :shifted)
                                              (strength :tertiary))
  "Compute the UTS #10 sort key for STRING under DUCET D.  Returns a
(simple-array (unsigned-byte 16) (*)) holding the weight levels separated
by 0000: level 1, level 2, level 3, and -- under the :SHIFTED option --
level 4.  Binary comparison of two such keys yields the collation order
of their strings.  VARIABLE-WEIGHTING is as in COLLATION-WEIGHTS.

STRENGTH bounds the levels included in the key, and hence the
distinctions the comparison makes: :PRIMARY (base letters only),
:SECONDARY (also accents), :TERTIARY (also case; the default), or
:QUATERNARY (also the level-4 weights, which exist only under the
:SHIFTED option and otherwise add nothing).  A lower strength makes
more strings compare equal; for example :SECONDARY ignores case."
  (multiple-value-bind (l1 l2 l3 l4)
      (collation-weights d string variable-weighting)
    (let* ((weights (ecase strength
                      (:primary l1)
                      (:secondary (append l1 (list 0) l2))
                      (:tertiary (append l1 (list 0) l2 (list 0) l3))
                      (:quaternary
                       (append l1 (list 0) l2 (list 0) l3 (list 0) l4))))
           (key (make-array (length weights)
                            :element-type '(unsigned-byte 16))))
      (loop for w in weights
            for k from 0
            do (setf (aref key k) w))
      key)))

(defun collation-compare (d s1 s2 &optional (variable-weighting :shifted)
                                            (strength :tertiary))
  "Compare strings S1 and S2 under DUCET D.  Returns -1, 0, or 1 like a
three-way comparison: negative if S1 sorts before S2, zero if equal, 1
if after.  VARIABLE-WEIGHTING and STRENGTH are as in COLLATION-SORT-KEY."
  (let ((k1 (collation-sort-key d s1 variable-weighting strength))
        (k2 (collation-sort-key d s2 variable-weighting strength)))
    (let ((n (min (length k1) (length k2))))
      (dotimes (i n)
        (let ((a (aref k1 i)) (b (aref k2 i)))
          (cond ((< a b) (return-from collation-compare -1))
                ((> a b) (return-from collation-compare 1)))))
      (cond ((< (length k1) (length k2)) -1)
            ((> (length k1) (length k2)) 1)
            (t 0)))))


;;; -------------------------------------------------------------------
;;; Public collation API (UNICODE package): the Unicode-aware
;;; equivalents of the COMMON-LISP string comparison functions.
;;;
;;; These compare strings by the Unicode Collation Algorithm rather than
;;; by code-point order, so the result reflects linguistic sort order
;;; (after NFD normalization, with contractions, expansions and the
;;; chosen variable-weighting option).  The Default Unicode Collation
;;; Element Table is loaded lazily on first use.
;;;
;;; Unlike the COMMON-LISP functions, these return a generalized boolean
;;; (T or NIL) rather than a mismatch index: the comparison is performed
;;; on sort keys derived from the whole normalized string, so there is
;;; no meaningful character index of the first difference to return.
;;; -------------------------------------------------------------------


;;; -------------------------------------------------------------------
;;; Building the runtime DUCET from the collation section of
;;; unidata.bin.  The resulting table is structurally identical to one
;;; built by LOAD-DUCET from allkeys.txt -- the same MAP / SINGLE /
;;; STARTERS hashes and implicit ranges -- so the sort-key construction
;;; code uses it unchanged.  This replaces the runtime use of LOAD-DUCET
;;; (which is kept for regenerating data and for cross-checking).
;;; -------------------------------------------------------------------

(defun unidata-ducet ()
  "Build a DUCET from the collation section of unidata.bin, loading the
section first if necessary."
  (unless (unidata-collation *unicode-data*)
    (load-collation))
  (let* ((c (unidata-collation *unicode-data*))
	 (primv (collation-primv c))
	 (secv (collation-secv c))
	 (terv (collation-terv c))
	 (contractions (collation-contractions c))
	 (ranges (collation-ranges c))
	 (d (make-ducet :version (format nil "~D.~D.~D"
					 +unicode-major-version+
					 +unicode-minor-version+
					 +unicode-update-version+)))
	 (maxvar 0)
	 (maxkey 1))
    (flet ((ces-at (packed)
	     ;; Slice the parallel arrays into a simple-vector of
	     ;; collation-elements for the packed (offset << 6) | count.
	     (let* ((off (ash packed -6))
		    (n (logand packed #x3f))
		    (v (make-array n)))
	       (dotimes (i n)
		 (let* ((j (+ off i))
			(te (aref terv j))
			(var (logbitp 7 te))
			(p (aref primv j)))
		   (when (and var (> p maxvar))
		     (setf maxvar p))
		   (setf (aref v i)
			 (make-ce p (aref secv j) (logand te #x7f) var))))
	       v)))
      ;; Single-codepoint entries: walk the codepoint space and pull the
      ;; non-zero values out of the index trie.  (Many keys are astral,
      ;; so the walk must cover the full range, not just the BMP.)
      (dotimes (cp #x110000)
	(let ((packed (qref32 c cp)))
	  (unless (zerop packed)
	    (let ((ces (ces-at packed)))
	      (setf (gethash cp (ducet-single d)) ces)
	      (setf (gethash (make-array 1 :initial-element cp) (ducet-map d))
		    ces)))))
      ;; Contractions: four 32-bit words each.
      (loop for i from 0 below (length contractions) by 4 do
	(let* ((cp1 (aref contractions i))
	       (cp2 (aref contractions (+ i 1)))
	       (cp3 (aref contractions (+ i 2)))
	       (packed (aref contractions (+ i 3)))
	       (key (if (= cp3 #xFFFFFFFF)
			(make-array 2 :initial-contents (list cp1 cp2))
			(make-array 3 :initial-contents (list cp1 cp2 cp3)))))
	  (setf (gethash key (ducet-map d)) (ces-at packed))
	  (setf (gethash cp1 (ducet-starters d)) t)
	  (setf maxkey (max maxkey (length key)))))
      ;; Implicit-weight ranges: four 32-bit words each (start, end,
      ;; base, base-origin).
      (let ((rl nil))
	(loop for i from 0 below (length ranges) by 4 do
	  (let ((r (make-implicit-range (aref ranges i)
					(aref ranges (+ i 1))
					(aref ranges (+ i 2)))))
	    (setf (implicit-range-base-origin r) (aref ranges (+ i 3)))
	    (push r rl)))
	(setf (ducet-implicit-ranges d) (nreverse rl)))
      (setf (ducet-max-key-length d) maxkey
	    (ducet-max-variable-primary d) maxvar)
      d)))

(in-package "UNICODE")

(defvar *collation-table-path* "ext-formats:allkeys.txt"
  "Pathname of the DUCET data file (allkeys.txt) from which the default
collation table is loaded on first use.")

(defvar *collation-table* nil
  "The default Unicode collation table, or NIL if it has not yet been
loaded.  Loaded lazily from *COLLATION-TABLE-PATH* the first time a
collation function needs it.  Set to NIL to force a reload.")

(defun collation-table ()
  "Return the default Unicode collation table, building it from the
collation section of unidata.bin on first use."
  (or *collation-table*
      (setf *collation-table* (lisp::unidata-ducet))))

(defun %collation-compare (string1 string2 start1 end1 start2 end2
                           variable-weighting strength)
  "Three-way collation comparison of the designated substrings of
STRING1 and STRING2: returns a negative integer, zero, or a positive
integer as the first sorts before, equal to, or after the second."
  (let ((s1 (string string1))
        (s2 (string string2)))
    (when (or (/= start1 0) end1)
      (setf s1 (subseq s1 start1 end1)))
    (when (or (/= start2 0) end2)
      (setf s2 (subseq s2 start2 end2)))
    (lisp::collation-compare (collation-table) s1 s2
                             variable-weighting strength)))

(defmacro %def-collation-predicate (name test default-strength docstring)
  "Define a collation comparison predicate NAME whose result is (TEST c)
where c is the three-way comparison of the two string arguments.
DEFAULT-STRENGTH is the default value of the STRENGTH keyword."
  `(defun ,name (string1 string2 &key (start1 0) end1 (start2 0) end2
                                      (variable-weighting :shifted)
                                      (strength ,default-strength))
     ,docstring
     (let ((c (%collation-compare string1 string2
                                  start1 end1 start2 end2
                                  variable-weighting strength)))
       (,test c))))

(%def-collation-predicate string= zerop :tertiary
  "Return true if STRING1 and STRING2 collate as equal under the Unicode
Collation Algorithm.  Note that this is collation equality, not
code-point identity: canonically equivalent strings, and strings that
differ only in collation-ignorable ways, compare equal.  START1, END1,
START2 and END2 bound the substrings compared.  VARIABLE-WEIGHTING is
:SHIFTED (the default) or :NON-IGNORABLE.  STRENGTH is :PRIMARY,
:SECONDARY, :TERTIARY (the default), or :QUATERNARY, as in
LISP::COLLATION-SORT-KEY; a lower strength makes more strings compare
equal -- :SECONDARY, for instance, ignores case.")

(%def-collation-predicate string/= (lambda (c) (not (zerop c))) :tertiary
  "Return true if STRING1 and STRING2 do not collate as equal.  See
UNICODE:STRING= for the meaning of the keyword arguments.")

(%def-collation-predicate string< minusp :tertiary
  "Return true if STRING1 collates before STRING2 under the Unicode
Collation Algorithm.  See UNICODE:STRING= for the meaning of the keyword
arguments.")

(%def-collation-predicate string> plusp :tertiary
  "Return true if STRING1 collates after STRING2 under the Unicode
Collation Algorithm.  See UNICODE:STRING= for the meaning of the keyword
arguments.")

(%def-collation-predicate string<= (lambda (c) (not (plusp c))) :tertiary
  "Return true if STRING1 collates before or equal to STRING2 under the
Unicode Collation Algorithm.  See UNICODE:STRING= for the meaning of the
keyword arguments.")

(%def-collation-predicate string>= (lambda (c) (not (minusp c))) :tertiary
  "Return true if STRING1 collates after or equal to STRING2 under the
Unicode Collation Algorithm.  See UNICODE:STRING= for the meaning of the
keyword arguments.")

;;; The case-insensitive comparison functions, the Unicode analogs of
;;; the COMMON-LISP -EQUAL/-LESSP/... family.  They default to :SECONDARY
;;; strength, which drops the tertiary level where case is encoded, so
;;; they ignore case (and other tertiary distinctions, such as width)
;;; while remaining sensitive to base letters and accents.  This is the
;;; closest collation analog of case-folded comparison; the Unicode
;;; Collation Algorithm has no operation that folds case alone.

(%def-collation-predicate string-equal zerop :secondary
  "Return true if STRING1 and STRING2 collate as equal ignoring case,
under the Unicode Collation Algorithm.  Like UNICODE:STRING= but
defaulting to :SECONDARY strength; see it for the keyword arguments.")

(%def-collation-predicate string-not-equal (lambda (c) (not (zerop c))) :secondary
  "Return true if STRING1 and STRING2 do not collate as equal ignoring
case.  See UNICODE:STRING-EQUAL for the keyword arguments.")

(%def-collation-predicate string-lessp minusp :secondary
  "Return true if STRING1 collates before STRING2 ignoring case, under
the Unicode Collation Algorithm.  See UNICODE:STRING-EQUAL for the
keyword arguments.")

(%def-collation-predicate string-greaterp plusp :secondary
  "Return true if STRING1 collates after STRING2 ignoring case.  See
UNICODE:STRING-EQUAL for the keyword arguments.")

(%def-collation-predicate string-not-greaterp (lambda (c) (not (plusp c))) :secondary
  "Return true if STRING1 collates before or equal to STRING2 ignoring
case.  See UNICODE:STRING-EQUAL for the keyword arguments.")

(%def-collation-predicate string-not-lessp (lambda (c) (not (minusp c))) :secondary
  "Return true if STRING1 collates after or equal to STRING2 ignoring
case.  See UNICODE:STRING-EQUAL for the keyword arguments.")
