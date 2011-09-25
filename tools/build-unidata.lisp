;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment "$Header: src/tools/build-unidata.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Build the Unicode data file
;;;
;;; This can be run from an 8-bit build just fine.  The Unicode data
;;; file is read and written in portable way so it can be used on any
;;; platform.
;;;
;;; What to do: Run (build-unidata) then (write-unidata <path>)
;;;
;;; This only needs to be run if the Unicode database changes and we
;;; need a new Unicode data file.

(in-package "COMMON-LISP-USER")

(defstruct unidata
  range
  name+
  name
  category
  scase
  numeric
  decomp
  combining
  bidi
  name1+
  name1
  qc-nfd
  qc-nfkd
  qc-nfc
  qc-nfkc
  comp-exclusions
  full-case-lower
  full-case-title
  full-case-upper
  case-fold-full
  case-fold-simple
  word-break
  )

(defvar *unicode-data* (make-unidata))

;; The magic number for the unidata.bin file.  (It's "*UCD", in
;; big-endian order.)
(defconstant +unicode-magic-number+ #x2A554344)

;; The expected Unicode version
(defconstant +unicode-major-version+ 6)
(defconstant +unicode-minor-version+ 0)
(defconstant +unicode-update-version+ 0)

;;; These need to be synched with code/unidata.lisp

(defstruct dictionary
  (cdbk (ext:required-argument) :read-only t :type simple-vector)
  (keyv (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 8) (*)))
  (keyl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 8) (*)))
  (codev (ext:required-argument) :read-only t
	 :type (simple-array (signed-byte 32) (*)))
  (nextv (ext:required-argument) :read-only t
	 :type (simple-array (unsigned-byte 32) (*)))
  (namev (ext:required-argument) :read-only t
	 :type (simple-array (unsigned-byte 32) (*))))

(defstruct range
  (codes (ext:required-argument) :read-only t
	 :type (simple-array (unsigned-byte 32) (*))))

(defstruct ntrie
  (split (ext:required-argument) :read-only t
	 :type (unsigned-byte 8))
  (hvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*)))
  (mvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (ntrie1 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array bit (*))))

(defstruct (ntrie2 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 2) (*))))

(defstruct (ntrie4 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 4) (*))))

(defstruct (ntrie8 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 8) (*))))

(defstruct (ntrie16 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (ntrie32 (:include ntrie))
  (lvec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 32) (*))))


(defstruct (scase (:include ntrie32))
  (svec (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (decomp (:include ntrie32))
  (tabl (ext:required-argument) :read-only t
	;; This has type simple-string in unidata.lisp, but we can treat
	;; it as a vector of integers here
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (full-case (:include ntrie32))
  (tabl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (case-folding (:include ntrie32))
  (tabl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defstruct (bidi (:include ntrie16))
  (tabl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defconstant +decomposition-type+
  '(nil "<compat>" "<initial>" "<medial>" "<final>" "<isolated>"
    "<super>" "<sub>" "<fraction>" "<font>" "<noBreak>"
    "<vertical>" "<wide>" "<narrow>" "<small>" "<square>" "<circle>"))

(defconstant +bidi-class+
  '("L" "LRE" "LRO" "R" "AL" "RLE" "RLO" "PDF" "EN" "ES" "ET" "AN" "CS"
    "NSM" "BN" "B" "S" "WS" "ON"))


;; Codebooks for encoding names.  These are fairly arbitrary.  Order isn't
;; important, except that if A is an initial substring of B, B must come
;; first (or A will never be used), but since they're searched in order it's
;; nice if more-common strings come before less-common ones.  The entries
;; don't have to be complete words; but may not contain spaces or hyphens,
;; and there must be a single-character string for every valid character in
;; the set of names you want to look up (unless some character only ever
;; occurs in some other entry).
;;
;; *CODEBOOK* is for the modern character names, *CODEBOOK-1* is for
;; the Unicode 1.0 names.
(defvar *codebook*
  #(" " "LETTER" "NA" "SMALL" "CAPITAL" "MATHEMATICAL" "RADICAL" "ARABIC"
    "HANGUL" "LATIN" "SYLLABLE" "-" "WITH" "RA" "YI" "CJK" "FORM" "LIGATURE"
    "FOR" "PATTERN" "SIGN" "MARK" "GREEK" "AND" "YA" "BOLD" "COMPATIBILITY"
    "OLD" "KATAKANA" "SYMBOL" "ITALIC" "CUNEIFORM" "TAI" "UP" "U" "LINEAR"
    "SYLLABICS" "ETHIOPIC" "ABOVE" "2" "Z" "CANADIAN" "DOTS" "LINE"
    "HALFWIDTH" "X" "KANGXI" "MUSICAL" "VOWEL" "Q" "IDEOGRAPHIC" "DOT"
    "RIGHTWARDS" "PARENTHESIZED" "LAM" "TIMES" "HALF" "FINAL" "ORIYA" "SERIF"
    "DIGIT" "ARROW" "IDEOGRAPH" "RIGHT" "PA" "TAG" "CIRCLED" "MALAYALAM" "9"
    "HAH" "SANS" "CIRCLE" "YEH" "Y" "OR" "DASIA" "WHITE" "KANNADA" "VARIATION"
    "CYRILLIC" "1" "8" "SQUARE" "VARIA" "NOTATION" "TONE" "0" "3" "5" "LIGHT"
    "BRAILLE" "DA" "ONE" "KA" "COMBINING" "4" "ISOLATED" "EQUAL" "NOT"
    "SELECTOR" "VAI" "TO" "DOUBLE" "GEORGIAN" "OF" "BYZANTINE" "MODIFIER"
    "HAMZA" "6" "ANGLE" "LEFTWARDS" "FOUR" "CARRIER" "VERTICAL" "TIBETAN"
    "STROKE" "BELOW" "INITIAL" "MEEM" "PLUS" "LEFT" "7" "DOWN" "NUMBER" "TWO"
    "KHMER" "OVER" "JEEM" "BARB" "SCRIPT" "MACRON" "LAO" "WEST" "W" "HOOK"
    "TAMIL" "EIGHT" "COPTIC" "ALEF" "BAR" "E" "IDEOGRAM" "TA" "I" "RUNIC"
    "DRAWINGS" "BLACK" "FIVE" "HORIZONTAL" "MONGOLIAN" "TILE" "GLAGOLITIC"
    "LOWER" "NUMERIC" "SIX" "CREE" "BOX" "MYANMAR" "THREE" "PSILI" "LOW"
    "ACUTE" "HARPOON" "BRACKET" "DEVANAGARI" "MAKSURA" "LIMBU" "FULLWIDTH"
    "VOCALIC" "V" "JONGSEONG" "THAN" "HA" "CHARACTER" "BALINESE" "LI" "MA"
    "NINE" "SEVEN" "DOMINO" "FRAKTUR" "HEBREW" "TILDE" "CHOSEONG" "LAGAB"
    "BENGALI" "MEDIAL" "ARMENIAN" "HEAVY" "GUJARATI" "NEW" "TELUGU" "LA"
    "CHAM" "OMEGA" "DESERET" "KHAROSHTHI" "SIOS" "HIRAGANA" "POINTING" "P"
    "FUNCTIONAL" "APL" "THAI" "CHEROKEE" "LUE" "GURMUKHI" "F" "DIAERESIS"
    "D" "SAURASHTRA" "BOPOMOFO" "B" "JUNGSEONG" "TETRAGRAM" "HIGH" "LONG"
    "CIRCUMFLEX" "C" "GRAVE" "SINHALA" "ALPHA" "KHAH" "K" "TELEGRAPH" "LESS"
    "OPEN" "HEXAGRAM" "H" "SYRIAC" "ACCENT" "LEPCHA" "L" "TRIANGLE" "GREATER"
    "STRUCK" "OXIA" "O" "J" "REVERSED" "AEGEAN" "A" "SHORT" "TURNED" "T" "GA2"
    "G" "MONOSPACE" "STOP" "S" "M" "NKO" "NUMERAL" "RIEUL" "R" "N"))

(defvar *codebook-1*
  #(" " "LETTER" "RA" "OR" "ARABIC" "SMALL" "CAPITAL" "LATIN" "GLYPH" "HANGUL"
    "RIGHT" "IE" "LEFT" "CYRILLIC" "ONE" "SQUARED" "HACEK" "SQUARE" "DOUBLE"
    "END" "FORMS" "WITH" "AND" "ON" "FOR" "LIGHT" "SPACING" "GEORGIAN" "EN"
    "NON" "GE" "CIRCLED" "UPPER" "DOWN" "WHITE" "SO" "THAI" "ABOVE" "BRACKET"
    "HALFWIDTH" "GREEK" "VERTICAL" "ISOLATE" "POINTING" "DIGIT" "HALF" "TEN"
    "FINAL" "SCRIPT" "BELOW" "ACUTE" "POINT" "HEAVY" "-" "OPENING" "TE"
    "GRAPHIC" "CLOSING" "HAMZAH" "OPEN" "KAERITEN" "MACRON" "BLACK" "TONOS"
    "YA" "DOTS" "RING" "ANGLE" "SINGLE" "HAA" "LESS" "PERIOD" "KA" "UPSILON"
    "CIRCUMFLEX" "BARB" "INITIAL" "ALEF" "FOUR" "DIAERESIS" "THAN" "SIGN"
    "HOOK" "MEDIAL" "GRAVE" "PARENTHESIZED" "LIEUL" "OF" "REVERSED" "TWO" "UP"
    "HORIZONTAL" "CEDILLA" "INVERSE" "SANS" "MU" "VOWEL" "ARROWS" "THREE" "FA"
    "HARPOON" "BIEUB" "OVERLAY" "V" "X" "LAM" "EQUAL" "SERIF" "TIBETAN" "HA"
    "DASHED" "COMMA" "GREATER" "MARK" "BARRED" "ARROW" "UNDERSCORE" "TRIANGLE"
    "NUMBER" "SIOS" "FULLWIDTH" "DASH" "WAW" "LINE" "OVERSCORE" "UNDER" "U"
    "TORTOISE" "BAR" "QUOTATION" "PARENTHESIS" "Q" "GIYEOG" "G" "EUM" "SARA"
    "DOTLESS" "FRACTION" "OVER" "BREVE" "NOT" "YOGH" "LOWER" "DOT" "Y" "II"
    "TAA" "EIGHT" "OGONEK" "SSANG" "CURLY" "JIEUJ" "MAI" "TILDE" "J" "KHO"
    "DESCENDER" "HEADED" "CURL" "RETROFLEX" "R" "BAA" "SUPERSCRIPT" "TONE"
    "NIEUN" "FIVE" "ZHE" "MODIFIER" "DIGEUD" "CORNER" "SUBSCRIPT" "TO" "INDIC"
    "EASTERN" "LIGATURE" "CENTER" "PIEUP" "Z" "TAH" "SLASH" "WAVY" "NINE" "W"
    "HIEUH" "DEVICE" "CAF" "ARMENIAN" "EPSILON" "FEATHERED" "SIX" "THO" "A"
    "IOTA" "KEY" "NOR" "CONTROL" "BAN" "LENTICULAR" "L" "SEVEN" "B" "DAL"
    "TAIL" "CHE" "OMEGA" "O" "HEBREW" "EQUIVALENT" "E" "SHORT" "TURNED"
    "NEITHER" "CHI" "INVERTED" "MIEUM" "SEPARATOR" "TIEUT" "DHAH" "NOON" "N"
    "PAIRED" "FEED" "KHAA" "SHELL" "HIGH" "TABULATION" "CHARACTER"
    "IDEOGRAPHS" "I" "DAMMAH" "START" "MADDAH" "C" "TRIPLE" "STRUCK" "HORN"
    "H" "K" "DELETE" "D" "TATWEEL" "STROKE" "MARBUTAH" "SHIFT" "TIP" "FROM"
    "SHADOWED" "S" "THAA" "F" "M" "TRANSMISSION" "P" "TRANSFINITE" "T" "2"))



(defun encode-name (string codebook)
  (let ((p 0)
	(res '()))
    (loop while (< p (length string)) do
      (dotimes (i (length codebook)
		  (error "\"~C\" is not in the codebook." (char string p)))
	(let ((code (aref codebook i)))
	  (when (and (<= (length code) (- (length string) p))
		     (string= string code :start1 p :end1 (+ p (length code))))
	    (push i res)
	    (incf p (length code))
	    (return)))))
    (nreverse (coerce res 'vector))))

(defun name-lookup (name codebook keyv keyl nextv)
  (let* ((current 0)
	 (posn 0))
    (loop
      (let ((keyp (ash (aref nextv current) -18)))
	(dotimes (i (aref keyl keyp)
		    (return-from name-lookup nil))  ; shouldn't happen
	  (let* ((str (aref codebook (aref keyv (+ keyp i))))
		 (len (length str)))
	    (when (and (>= (length name) (+ posn len))
		       (string= name str :start1 posn :end1 (+ posn len)))
	      (setq current
		  (+ (logand (aref nextv current) #x3FFFF) i))
	      (if (= (incf posn len) (length name))
		  (return-from name-lookup current)
		  (return)))))))))	; from DOTIMES - do outer LOOP again

(defun build-dictionary (codebook entries)
  (let ((khash (make-hash-table :test 'equalp))
	(thash (make-hash-table))
	(top 0)
	(keyl (make-array 0 :element-type '(unsigned-byte 8)))
	(keyv (make-array 0 :element-type '(unsigned-byte 8)))
	vec1 vec2 vec3)
    (labels ((add-to-trie (trie name codepoint)
	       (loop for ch across (encode-name name codebook) do
		 (let ((sub (cdr (assoc ch (rest trie)))))
		   (if sub
		       (setq trie sub)
		       (setq trie (cdar (push (cons ch (cons nil nil))
					      (rest trie)))))))
	       (unless (or (null (car trie)) (= (car trie) codepoint))
		 (error "Codepoints #x~4,'0X and #x~4,'0X are both named ~S."
			(car trie) codepoint name))
	       (setf (car trie) codepoint))
	     (key (trie)
	       (map '(simple-array (unsigned-byte 8) (*)) #'car (rest trie)))
	     (pass1 (trie depth)
	       (setf (rest trie) (sort (rest trie) #'< :key #'car))
	       (setf (gethash trie thash)
		   (list depth (1- (incf top)) (length (rest trie))))
	       (setf (gethash (key trie) khash) t)
	       (mapc (lambda (x) (pass1 (cdr x) (1+ depth))) (rest trie)))
	     (pass2 (trie)
	       (let* ((x (gethash (gethash trie thash) thash))
		      (n (car x)))
		 (setf (aref vec1 n) (if (first trie) (first trie) -1)
		       (aref vec2 n) (logior (ash (gethash (key trie) khash)
						  18)
					     (cdr x))))
	       (mapc (lambda (x) (pass2 (cdr x))) (rest trie))))
      (format t "~&  Initializing...~%")
      (let ((trie (cons nil nil)))
	(loop for (name . code) in entries do (add-to-trie trie name code))
	(format t "~&  Pass 1...~%")
	(pass1 trie 0)
	(format t "~&  Sorting...~%")
	(dolist (key (sort (loop for k being the hash-keys of khash
			      collect k)
			   #'> :key #'length))
	  (let ((pos -1))
	    (loop
	      (setq pos (search key keyv :start2 (1+ pos)))
	      (when (and pos (zerop (aref keyl pos)))
		(setf (aref keyl pos) (length key)))
	      (when (and pos (= (aref keyl pos) (length key)))
		(setf (gethash key khash) pos)
		(return))
	      (when (null pos)
		(setf (gethash key khash) (length keyv))
		(setf keyl (adjust-array keyl (+ (length keyv) (length key))))
		(setf (aref keyl (length keyv)) (length key))
		(setf keyv (concatenate '(simple-array (unsigned-byte 8) (*))
					keyv key))
		(return)))))
	(loop with off = 1
	      for key in (sort (loop for x being the hash-values of thash
				  collect x)
			       (lambda (a b) (if (= (first a) (first b))
						 (< (second a) (second b))
						 (< (first a) (first b)))))
	      as i upfrom 0
	  do (setf (gethash key thash) (cons i off) off (+ off (third key))))
	(setq vec1 (make-array top :element-type '(signed-byte 32))
	      vec2 (make-array top :element-type '(unsigned-byte 32))
	      vec3 (make-array top :element-type '(unsigned-byte 32)))
	(format t "~&  Pass 2...~%")
	(pass2 trie)
	(format t "~&  Finalizing~%")
	(dotimes (i top)
	  (let ((xxx (aref vec2 i)))
	    (dotimes (j (aref keyl (ash xxx -18)))
	      (setf (aref vec3 (+ (logand xxx #x3FFFF) j)) i))))
	(loop for (name . code) in entries do
	  (let ((n (name-lookup name codebook keyv keyl vec2)))
	    (unless n (error "Codepoint not found for ~S." name))
	    (setf (ldb (byte 14 18) (aref vec3 n)) (length name))))))
    (make-dictionary :cdbk codebook
		   :keyv keyv :keyl keyl
		   :codev vec1 :nextv vec2 :namev vec3)))

(defun pack (ucd range fn default bits split)
  (let* ((lbits (1+ (logand split 15)))
	 (mbits (1+ (ash split -4)))
	 (upos 0)
	 (base 0)
	 (len (ash 1 mbits))
	 (posn len)
	 (hvec (make-array (1+ (ash #x110000 (- (+ mbits lbits))))
			   :element-type '(unsigned-byte 16)))
	 (ttop (1+ (ash #x110000 (- lbits))))
	 (tvec (make-array ttop :element-type '(unsigned-byte 16)))
	 (lvec (make-array 0 :element-type (list 'unsigned-byte bits))))
    ;; pass 1 - build lvec and temporary tvec
    (loop
      ;;(format t "~&Pass 1: ~6,'0X~%" (ash base lbits))
      (let ((vec (make-array (ash 1 lbits) :initial-element default
			     :element-type (list 'unsigned-byte bits)))
	    (empty t))
	(loop while (and (< upos (length ucd))
			 (< (ucdent-code (aref ucd upos))
			    (ash (1+ base) lbits)))
	  do
	    (let* ((min (ucdent-code (aref ucd upos)))
		   (max (let ((x (position min (range-codes range))))
			  (if (and x (evenp x))
			      (aref (range-codes range) (1+ x))
			      min)))
		   (i (max (- min (ash base lbits)) 0))
		   (j (min (- max (ash base lbits)) (1- (ash 1 lbits)))))
	      (loop for i from i to j do
	        (setf (aref vec i) (funcall fn (aref ucd upos)))
		(unless (= (aref vec i) default)
		  (setq empty nil)))
	      (if (< max (ash (1+ base) lbits))
		  (incf upos)
		  (return))))
	(if empty
	    (setf (aref tvec base) #xFFFF)
	    (let ((idx (search vec lvec)))
	      (when (null idx)
		(setq idx (length lvec))
		(setf lvec (adjust-array lvec (+ idx (ash 1 lbits))))
		(replace lvec vec :start1 idx))
	      (setf (aref tvec base) idx))))
      (incf base)
      (when (> (ash base lbits) #x10FFFF)
	(return)))
    ;; pass 2 - build hvec and transform tvec into mvec
    (setf base 1 (aref hvec 0) 0)
    (loop
      ;;(format t "~&Pass 2: ~6,'0X~%" (ash base (+ mbits lbits)))
      (let ((empty t))
	(loop for i from posn below (+ posn len)
	  when (/= (aref tvec i) #xFFFF) do (setq empty nil)
	  while empty)
	(if empty
	    (progn
	      (setf (aref hvec base) #xFFFF)
	      (replace tvec tvec :start1 posn :start2 (+ posn len))
	      (decf ttop len))
	    (let ((idx (search tvec tvec
			       :start1 posn :end1 (+ posn len)
			       :end2 ttop))
		  (dl len) (ds posn) (pinc len))
	      (when (< idx posn)
		(when (> (+ idx len) posn)
		  (setq dl (- posn idx) ds (+ idx len)))
		(decf pinc dl)
		(replace tvec tvec :start1 ds :start2 (+ ds dl))
		(decf ttop dl))
	      (incf posn pinc)
	      (setf (aref hvec base) idx))))
      (incf base)
      (when (> (ash base (+ mbits lbits)) #x10FFFF)
	(return)))
    (values hvec (lisp::shrink-vector tvec ttop) lvec)))



(defun write-unidata (path)
  (labels ((write16 (n stm)
	     (write-byte (ldb (byte 8 8) n) stm)
	     (write-byte (ldb (byte 8 0) n) stm))
	   (write32 (n stm)
	     (write16 (ldb (byte 16 16) n) stm)
	     (write16 (ldb (byte 16 0) n) stm))
	   (write-ntrie (split hvec mvec lvec stm)
	     (write-byte split stm)
	     (write16 (length hvec) stm)
	     (write16 (length mvec) stm)
	     (write16 (length lvec) stm)
	     (write-vector hvec stm :endian-swap :network-order)
	     (write-vector mvec stm :endian-swap :network-order)
	     (write-vector lvec stm :endian-swap :network-order))
	   (write-ntrie32 (data stm)
	     (write-ntrie (ntrie32-split data)
			  (ntrie32-hvec data)
			  (ntrie32-mvec data)
			  (ntrie32-lvec data)
			  stm))
	   (write-ntrie16 (data stm)
	     (write-ntrie (bidi-split data)
			  (bidi-hvec data)
			  (bidi-mvec data)
			  (bidi-lvec data)
			  stm))
	   (write-ntrie8 (data stm)
	     (write-ntrie (ntrie8-split data)
			  (ntrie8-hvec data)
			  (ntrie8-mvec data)
			  (ntrie8-lvec data)
			  stm))
	   (write-ntrie4 (data stm)
	     (write-ntrie (ntrie4-split data)
			  (ntrie4-hvec data)
			  (ntrie4-mvec data)
			  (ntrie4-lvec data)
			  stm))
	   (write-ntrie2 (data stm)
	     (write-ntrie (ntrie2-split data)
			  (ntrie2-hvec data)
			  (ntrie2-mvec data)
			  (ntrie2-lvec data)
			  stm))
	   (write-ntrie1 (data stm)
	     (write-ntrie (ntrie1-split data)
			  (ntrie1-hvec data)
			  (ntrie1-mvec data)
			  (ntrie1-lvec data)
			  stm))
	   (write-dict (data stm)
	     (write-byte (1- (length (dictionary-cdbk data))) stm)
	     (write16 (length (dictionary-keyv data)) stm)
	     (write32 (length (dictionary-codev data)) stm)
	     (let ((codebook (dictionary-cdbk data)))
	       (dotimes (i (length codebook))
		 (write-byte (length (aref codebook i)) stm)
		 (dotimes (j (length (aref codebook i)))
		   (write-byte (char-code (char (aref codebook i) j)) stm))))
	     (write-vector (dictionary-keyv data) stm :endian-swap :network-order)
	     (write-vector (dictionary-keyl data) stm :endian-swap :network-order)
	     (write-vector (dictionary-codev data) stm :endian-swap :network-order)
	     (write-vector (dictionary-nextv data) stm :endian-swap :network-order)
	     (write-vector (dictionary-namev data) stm :endian-swap :network-order))
	   (update-index (val array)
	     (let ((result (vector-push val array)))
	       (unless result
		 (error "Index array too short for the data being written")))))
    (with-open-file (stm path :direction :io :if-exists :rename-and-delete
			 :element-type '(unsigned-byte 8))
      ;; The length of the index array is the number of sections to be
      ;; written.  See below for each section.
      (let ((index (make-array 19 :fill-pointer 0)))
	;; File header
	(write32 +unicode-magic-number+ stm)	; identification "magic"
	;; File format version 
	(write-byte 0 stm)
	;; Unicode version
	(write-byte +unicode-major-version+ stm)
	(write-byte +unicode-minor-version+ stm)
	(write-byte +unicode-update-version+  stm)
	(dotimes (i (array-dimension index 0))
	  (write32 0 stm))		; space for indices
	(write32 0 stm)			; end marker
	;; 0. Range data
	(let ((data (unidata-range *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write32 (length (range-codes data)) stm)
	  (write-vector (range-codes data) stm :endian-swap :network-order))
	;; 1. Character name data
	(update-index (file-position stm) index)
	(write-dict (unidata-name+ *unicode-data*) stm)
	;; 2. Codepoint-to-name mapping
	(let ((data (unidata-name *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm))
	;; 3. Codepoint-to-category table
	(let ((data (unidata-category *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie8 data stm))
	;; 4. Simple case mapping table
	(let ((data (unidata-scase *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm)
	  (write-byte (length (scase-svec data)) stm)
	  (write-vector (scase-svec data) stm :endian-swap :network-order))
	;; 5. Numeric data
	(let ((data (unidata-numeric *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm))
	;; 6. Decomposition data
	(let ((data (unidata-decomp *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm)
	  (write16 (length (decomp-tabl data)) stm)
	  (write-vector (decomp-tabl data) stm :endian-swap :network-order))
	;; 7. Combining classes
	(let ((data (unidata-combining *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie8 data stm))
	;; 8. Bidi data
	(let ((data (unidata-bidi *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie16 data stm)
	  (write-byte (length (bidi-tabl data)) stm)
	  (write-vector (bidi-tabl data) stm :endian-swap :network-order))
	;; 9. Unicode 1.0 names
	(update-index (file-position stm) index)
	(write-dict (unidata-name1+ *unicode-data*) stm)
	;; 10. Codepoint to unicode-1.0 name
	(let ((data (unidata-name1 *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm))
	;; 11. Normalization quick-check data
	(update-index (file-position stm) index)
	(let ((data (unidata-qc-nfd *unicode-data*)))
	  (write-ntrie1 data stm))
	(let ((data (unidata-qc-nfkd *unicode-data*)))
	  (write-ntrie1 data stm))
	(let ((data (unidata-qc-nfc *unicode-data*)))
	  (write-ntrie2 data stm))
	(let ((data (unidata-qc-nfkc *unicode-data*)))
	  (write-ntrie2 data stm))
	;; 12. Write composition exclusion table
	(let ((data (unidata-comp-exclusions *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write16 (length data) stm)
	  (write-vector data stm :endian-swap :network-order))
	(flet ((dump-full-case (data)
		 (update-index (file-position stm) index)
		 (write-ntrie32 data stm)
		 (write16 (length (full-case-tabl data)) stm)
		 (write-vector (full-case-tabl data) stm :endian-swap :network-order)))
	  ;; 13. Write full-case lower data
	  (dump-full-case (unidata-full-case-lower *unicode-data*))
	  ;; 14. Write full-case title data
	  (dump-full-case (unidata-full-case-title *unicode-data*))
	  ;; 15. Write full-case upper data
	  (dump-full-case (unidata-full-case-upper *unicode-data*)))
	;; 16. Write case-folding simple data
	(let ((data (unidata-case-fold-simple *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm))
	;; 17. Write case-folding full data
	(let ((data (unidata-case-fold-full *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie32 data stm)
	  (write16 (length (case-folding-tabl data)) stm)
	  (write-vector (case-folding-tabl data) stm :endian-swap :network-order))
	;; 18. Word-break
	(let ((data (unidata-word-break *unicode-data*)))
	  (update-index (file-position stm) index)
	  (write-ntrie4 data stm))
	;; All components saved. Patch up index table now.
	(file-position stm 8)
	(dotimes (i (length index))
	  (write32 (aref index i) stm)))))
  nil)



(defstruct (ucdent
	     (:constructor make-ucdent (code name cat comb bidi decomp
					     num1 num2 num3 mirror
					     name1 comment
					     upper lower title)))
  code name cat comb bidi decomp num1 num2 num3 mirror name1 comment
  upper lower title
  aliases
  mcode
  norm-qc
  comp-exclusion
  full-case-lower
  full-case-title
  full-case-upper
  case-fold-full
  case-fold-simple
  word-break
  ;; ...
  )

;; ucd-directory should be the directory where UnicodeData.txt is
;; located.
(defun foreach-ucd (name ucd-directory fn)
   (format t "~&  ~A~%" name)
  (with-open-file (s (make-pathname :name name :type "txt"
				    :defaults ucd-directory))
    (cond
      ((string= name "Unihan")
       (loop for line = (read-line s nil) while line do
	     (when (char= (char line 0) #\U)
	       (let* ((tab1 (position #\Tab line))
		      (tab2 (position #\Tab line :start (1+ tab1))))
		 (funcall fn
			  (parse-integer line :radix 16 :start 2 :end tab1)
			  (subseq line (1+ tab1) tab2)
			  (subseq line (1+ tab2)))))))
      ((string= name "CompositionExclusions")
       (loop for line = (read-line s nil) while line do
	     (let ((code (parse-integer line :radix 16 :junk-allowed t)))
	       (when code
		 (funcall fn code)))))
      (t
       (loop with save = 0
	     for line = (read-line s nil) as end = (position #\# line)
	     while line do
	     (loop while (and end (plusp end)
			      (char= (char line (1- end)) #\Space))
	       do (decf end))
	     (when (position #\; line :end end)
	       (let* ((split (loop for i = 0 then (1+ j)
				   as j = (position #\; line :start i :end end)
				   collect (string-trim '(#\Space #\Tab)
							(subseq line i (or j end)))
				   while j))
		      (first (first split))
		      (second (second split))
		      (length (length second))
		      (brk (search ".." first))
		      hi lo)
		 (if brk
		     (setq lo (parse-integer first :radix 16 :junk-allowed t)
			   hi (parse-integer first :radix 16 :start (+ brk 2)))
		     (setq lo (parse-integer first :radix 16)
			   hi lo))
		 (cond ((and (> length 8)
			     (string= second ", First>" :start1 (- length 8)))
			(setq save lo))
		       ((and (> length 7)
			     (string= second ", Last>" :start1 (- length 7)))
			(apply fn save hi
			       (concatenate 'string
					    (subseq second 0 (- length 7)) ">")
			       (rest (rest split))))
		       (t
			(apply fn lo hi (rest split)))))))))))

(defun parse-decomposition (string)
  (let ((type nil) (n 0))
    (when (char= (char string 0) #\<)
      (setq n (1+ (position #\Space string)))
      (setq type (subseq string 0 (1- n))))
    (list* type
	   (loop while (< n (length string))
	     collect (multiple-value-bind (a b)
			 (parse-integer string  :radix 16
			      :start n :end (position #\Space string :start n))
		       (setq n (1+ b))
		       a)))))

(declaim (ftype (function (&optional t) (values simple-vector range)) read-data))
(defun read-data (&optional (ucd-directory #p"target:i18n/"))
  (let ((vec (make-array 50000))
	(vec-hash (make-hash-table :size 15000))
	(pos 0)
	(range (make-array 50 :element-type '(unsigned-byte 32)))
	(rpos 0))
    (flet ((cat (x) (dpb (position (char x 0) "CZMPNLS") (byte 3 4)
			 (position (char x 1) "nsifepkcdmulto")))
	   (num (x) (if (string= x "") nil (read-from-string x)))
	   (chr (x) (if (string= x "") nil (parse-integer x :radix 16)))
	   (str (x) (if (string= x "") nil x))
	   (bool (x) (string= x "Y"))
	   (decomp (x) (if (string= x "") nil (parse-decomposition x))))
      (foreach-ucd "UnicodeData" ucd-directory
	(lambda (min max name cat comb bidi decomp num1 num2 num3
		 mirror name1 comment upper lower title)
	  (when (> max min)
	    (setf (aref range rpos) min (aref range (1+ rpos)) max)
	    (incf rpos 2))
	  (when (position #\( name1)
	    (setq name1 (subseq name1 0 (1- (position #\( name1)))))
	  (setf (aref vec pos)
	      (make-ucdent min name (cat cat) (num comb) (str bidi)
			   (decomp decomp)
			   (num num1) (num num2) (num num3)
			   (bool mirror) (str name1) (str comment)
			   (chr upper) (chr lower) (chr title)))
	  (incf pos))))
    (setf vec (lisp::shrink-vector vec pos))
    (setf range (lisp::shrink-vector range rpos))
    (flet ((find-ucd (key)
	     (let ((index (gethash key vec-hash)))
	       (if index
		   (aref vec index)
		   nil))))
      ;; Using FIND is rather slow, especially for the derived
      ;; normalization props.  We create a hash table that maps a
      ;; codepoint to the index to VEC that contains the desired entry.
      ;; This is much faster.  We do it this way to keep the structure
      ;; as close as possible to the original.
      (loop for k from 0 below pos
	 do (setf (gethash (ucdent-code (aref vec k)) vec-hash) k))
    
      (foreach-ucd "NameAliases"
	  ucd-directory
	(lambda (min max alias)
	  (declare (ignore max))
	  (push alias (ucdent-aliases (find-ucd min)))))

      (foreach-ucd "NormalizationCorrections"
	  ucd-directory
	(lambda (min max bad good &rest junk)
	  (declare (ignore max bad junk))
	  (setf (ucdent-decomp (find-ucd min))
		(parse-decomposition good))))

      (foreach-ucd "BidiMirroring"
	  ucd-directory
	(lambda (min max mirror)
	  (declare (ignore max))
	  (setf (ucdent-mcode (find-ucd min))
		(parse-integer mirror :radix 16 :junk-allowed t))))

      (foreach-ucd "DerivedNormalizationProps"
	  ucd-directory
	(lambda (min max prop &optional value)
	  (cond ((string= prop "NFD_QC")
		 (loop for i from min to max
		    as ent = (find-ucd i) do
		    (when ent
		      (setf (getf (ucdent-norm-qc ent) :nfd)
			    (intern value "KEYWORD")))))
		((string= prop "NFKD_QC")
		 (loop for i from min to max
		    as ent = (find-ucd i) do
		    (when ent
		      (setf (getf (ucdent-norm-qc ent) :nfkd)
			    (intern value "KEYWORD")))))
		((string= prop "NFC_QC")
		 (loop for i from min to max
		    as ent = (find-ucd i) do
		    (when ent
		      (setf (getf (ucdent-norm-qc ent) :nfc)
			    (intern value "KEYWORD")))))
		((string= prop "NFKC_QC")
		 (loop for i from min to max
		    as ent = (find-ucd i) do
		    (when ent
		      (setf (getf (ucdent-norm-qc ent) :nfkc)
			    (intern value "KEYWORD"))))))))

      (foreach-ucd "CompositionExclusions"
	  ucd-directory
	(lambda (min)		 
	  (let ((entry (find-ucd min)))
	    (setf (ucdent-comp-exclusion entry) t))))

      (foreach-ucd "SpecialCasing"
	  ucd-directory
	(lambda (min max lower title upper &rest condition)
	  (declare (ignore max))
	  (when (string= (car condition) "")
	    (flet ((parse-casing (string)
		     (rest (parse-decomposition string))))
	      (let ((ent (find-ucd min)))
		(setf (ucdent-full-case-lower ent) (parse-casing lower))
		(setf (ucdent-full-case-title ent) (parse-casing title))
		(setf (ucdent-full-case-upper ent) (parse-casing upper)))))))

      (foreach-ucd "CaseFolding"
	  ucd-directory
	(lambda (min max mode expansion &rest info)
	  (declare (ignore max info))
	  (flet ((parse-folding (string)
		   (rest (parse-decomposition string))))
	    (let ((ent (find-ucd min)))
	      (cond ((string= mode "T")
		     ;; We ignore these language-specific foldings.
		     )
		    ((string= mode "F")
		     ;; Full case folding
		     (setf (ucdent-case-fold-full ent) (parse-folding expansion)))
		    (t
		     ;; Simple case folding (C or S)
		     (setf (ucdent-case-fold-simple ent) (car (parse-folding expansion)))))))))

      (foreach-ucd "WordBreakProperty"
	  ucd-directory
	(lambda (min max prop)
	  (let ((code (intern (string-upcase prop) "KEYWORD")))
	    (loop for i from min to max
		  as ent = (find-ucd i) do
		  (when ent
		    (setf (ucdent-word-break ent) code))))))
      (values vec (make-range :codes range)))))



(defun pack-name (ucdent name1 dictionary)
  (let* ((cdbk (dictionary-cdbk dictionary))
	 (keyv (dictionary-keyv dictionary))
	 (keyl (dictionary-keyl dictionary))
	 (nextv (dictionary-nextv dictionary))
	 (name (if name1 (ucdent-name1 ucdent) (ucdent-name ucdent))))
    (or (and name (name-lookup name cdbk keyv keyl nextv)) 0)))

(defun pack-case (ucdent svec)
  (let* ((uo (if (ucdent-upper ucdent)
		 (- (ucdent-code ucdent) (ucdent-upper ucdent))
		 0))
	 (lo (if (ucdent-lower ucdent)
		 (- (ucdent-code ucdent) (ucdent-lower ucdent))
		 0))
	 (to (if (ucdent-title ucdent)
		 (- (ucdent-code ucdent) (ucdent-title ucdent))
		 0))
	 pu pl pt)
    (unless (setq pu (position (abs uo) svec))
      (setq pu (fill-pointer svec))
      (vector-push-extend (abs uo) svec))
    (unless (setq pl (position (abs lo) svec))
      (setq pl (fill-pointer svec))
      (vector-push-extend (abs lo) svec))
    (unless (setq pt (position (abs to) svec))
      (setq pt (fill-pointer svec))
      (vector-push-extend (abs to) svec))
    (logior (ash (if (minusp to) (logior 128 pt) pt) 16)
	    (ash (if (minusp lo) (logior 128 pl) pl) 8)
	    (if (minusp uo) (logior 128 pu) pu))))

(defun pack-numeric (ucdent)
  (let* ((n3 (ucdent-num3 ucdent))
	 (fl (cond ((ucdent-num1 ucdent) #x3800000)
		   ((ucdent-num2 ucdent) #x1800000)
		   (n3 #x800000)
		   (t 0)))
	 (neg (if (and n3 (minusp n3)) #x900000 0))
	 (num (if n3 (ash (abs (numerator n3)) 3) 0))
	 (den (if n3 (1- (denominator n3)) 0)))
    (logior fl neg num den)))

(defun pack-decomp (ucdent tabl)
  (if (not (ucdent-decomp ucdent))
      0
      (let* ((d (loop for i in (rest (ucdent-decomp ucdent))
		  if (<= i #xFFFF) collect i
		  else collect (logior (ldb (byte 10 10) (- i #x10000)) #xD800)
		   and collect (logior (ldb (byte 10 0) (- i #x10000)) #xDC00)))
	     (l (length d))
	     (n (search d tabl)))
	(unless n
	  (setq n (fill-pointer tabl))
	  (dolist (x d) (vector-push-extend x tabl)))
	;; high 5 bits: decomp type
	;; next 5 bits: unused
	;; next 6 bits: length, in code units
	;; low 16 bits: index into tabl
	(logior n (ash l 16)
		(ash (position (first (ucdent-decomp ucdent))
			       +decomposition-type+ :test #'equalp)
		     27)))))

(defun pack-full-case (ucdent tabl entry)
  (if (not (funcall entry ucdent))
      0
      (let* ((d (loop for i in (funcall entry ucdent)
		  if (<= i #xFFFF) collect i
		  else collect (logior (ldb (byte 10 10) (- i #x10000)) #xD800)
		   and collect (logior (ldb (byte 10 0) (- i #x10000)) #xDC00)))
	     (l (length d))
	     (n (search d tabl)))
	(unless n
	  (setq n (fill-pointer tabl))
	  (dolist (x d) (vector-push-extend x tabl)))
	;; next 6 bits: length, in code units
	;; low 16 bits: index into tabl
	(logior n (ash l 16)))))

(defun pack-bidi (ucdent tabl)
  (logior (position (ucdent-bidi ucdent) +bidi-class+ :test #'string=)
	  (if (ucdent-mirror ucdent) #x20 #x00)
	  (if (ucdent-mcode ucdent)
	      (let* ((n (- (ucdent-mcode ucdent) (ucdent-code ucdent)))
		     (x (abs n)))
		(logior
		 (ash (if (< x #x10)
			  x
			  (let ((k (position x tabl)))
			    (if k k (prog1 (fill-pointer tabl) (vector-push-extend x tabl)))))
		      6)
		 (if (< x #x10) #x000 #x800)
		 (if (minusp n) #x400 #x000)))
	      0)))

(defun pack-case-folding-simple (ucdent)
  (or (ucdent-case-fold-simple ucdent)
      0))

(defun pack-case-folding-full (entry tabl)
  (if (not (ucdent-case-fold-full entry))
      0
      (let* ((d (loop for i in (ucdent-case-fold-full entry)
		  if (<= i #xFFFF) collect i
		  else collect (logior (ldb (byte 10 10) (- i #x10000)) #xD800)
		   and collect (logior (ldb (byte 10 0) (- i #x10000)) #xDC00)))
	     (l (length d))
	     (n (search d tabl)))
	(unless n
	  (setq n (fill-pointer tabl))
	  (dolist (x d) (vector-push-extend x tabl)))
	;; next 6 bits: length, in code units
	;; low 16 bits: index into tabl
	(logior n (ash l 16)))))

(defun pack-word-break (ucdent)
  ;; The code is the index in the list.  :OTHER is a dummy value and
  ;; used to represent the default case.
  (or (position (ucdent-word-break ucdent)
		'(:other :cr :lf :newline :extend :format
		  :katakana :aletter :midnumlet :midletter :midnum
		  :numeric :extendnumlet))
      0))

;; ucd-directory should be the directory where UnicodeData.txt is
;; located.
(defun build-unidata (&optional (ucd-directory "target:i18n/"))
  (format t "~&Reading data from ~S~%" (probe-file ucd-directory))
  (multiple-value-bind (ucd range) (read-data ucd-directory)
    (setf (unidata-range *unicode-data*) range)
    (format t "~&Building character name tables~%")
    (let* ((data (loop for ent across ucd
		   when (char/= (char (ucdent-name ent) 0) #\<)
		     collect (cons (ucdent-name ent) (ucdent-code ent))
		   when (ucdent-aliases ent)
		     nconc (loop for name in (ucdent-aliases ent)
			     collect (cons name (ucdent-code ent)))))
	   (dict (build-dictionary *codebook* data)))
      (setf (unidata-name+ *unicode-data*) dict)
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-name x nil dict)) 0 32 #x54)
	(setf (unidata-name *unicode-data*)
	    (make-ntrie32 :split #x54 :hvec hvec :mvec mvec :lvec lvec))))

    (format t "~&Building Unicode 1.0 character name tables~%")
    (let* ((data (loop for ent across ucd
		   when (plusp (length (ucdent-name1 ent)))
		     collect (cons (ucdent-name1 ent) (ucdent-code ent))))
	   (dict (build-dictionary *codebook-1* data)))
      (setf (unidata-name1+ *unicode-data*) dict)
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-name x t dict)) 0 32 #x54)
      (setf (unidata-name1 *unicode-data*)
	  (make-ntrie32 :split #x54 :hvec hvec :mvec mvec :lvec lvec))))

    (format t "~&Building general category table~%")
    (multiple-value-bind (hvec mvec lvec)
	(pack ucd range #'ucdent-cat 0 8 #x53)
      (setf (unidata-category *unicode-data*)
	  (make-ntrie8 :split #x53 :hvec hvec :mvec mvec :lvec lvec)))

    (format t "~&Building simple case-conversion table~%")
    (let ((svec (make-array 100 :element-type '(unsigned-byte 16)
			    :fill-pointer 0 :adjustable t)))
      (vector-push-extend 0 svec)
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-case x svec))
		0 32 #x62)
	(setf (unidata-scase *unicode-data*)
	    (make-scase :split #x62 :hvec hvec :mvec mvec :lvec lvec
			:svec (copy-seq svec)))))

    (format t "~&Building numeric-values table~%")
    (multiple-value-bind (hvec mvec lvec)
	(pack ucd range #'pack-numeric 0 32 #x63)
      (setf (unidata-numeric *unicode-data*)
	  (make-ntrie32 :split #x63 :hvec hvec :mvec mvec :lvec lvec)))

    (format t "~&Building decomposition table~%")
    (let ((tabl (make-array 6000 :element-type '(unsigned-byte 16)
			    :fill-pointer 0 :adjustable t)))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-decomp x tabl))
		0 32 #x62)
	(setf (unidata-decomp *unicode-data*)
	    (make-decomp :split #x62 :hvec hvec :mvec mvec :lvec lvec
			 :tabl (copy-seq tabl)))))

    (format t "~&Building combining-class table~%")
    (multiple-value-bind (hvec mvec lvec)
	(pack ucd range #'ucdent-comb 0 8 #x64)
      (setf (unidata-combining *unicode-data*)
	  (make-ntrie8 :split #x64 :hvec hvec :mvec mvec :lvec lvec)))

    (format t "~&Building bidi information table~%")
    (let ((tabl (make-array 10 :element-type '(unsigned-byte 16)
			    :fill-pointer 0 :adjustable t)))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-bidi x tabl))
		0 16 #x62)
	(setf (unidata-bidi *unicode-data*)
	    (make-bidi :split #x62 :hvec hvec :mvec mvec :lvec lvec
		       :tabl (copy-seq tabl)))))

    (format t "~&Building normalization quick-check tables~%")
    (progn
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x)
			    (ecase (getf (ucdent-norm-qc x) :nfd :y)
			      (:y 0) (:n 1)))
		0 1 #x47)
	(setf (unidata-qc-nfd *unicode-data*)
	      (make-ntrie1 :split #x47 :hvec hvec :mvec mvec :lvec lvec)))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x)
			    (ecase (getf (ucdent-norm-qc x) :nfkd :y)
			      (:y 0) (:n 1)))
		0 1 #x47)
	(setf (unidata-qc-nfkd *unicode-data*)
	      (make-ntrie1 :split #x47 :hvec hvec :mvec mvec :lvec lvec)))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x)
			    (ecase (getf (ucdent-norm-qc x) :nfc :y)
			      (:y 0) (:m 1) (:n 2)))
		0 2 #x56)
	(setf (unidata-qc-nfc *unicode-data*)
	      (make-ntrie2 :split #x56 :hvec hvec :mvec mvec :lvec lvec)))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x)
			    (ecase (getf (ucdent-norm-qc x) :nfkc :y)
			      (:y 0) (:m 1) (:n 2)))
		0 2 #x55)
	(setf (unidata-qc-nfkc *unicode-data*)
	      (make-ntrie2 :split #x55 :hvec hvec :mvec mvec :lvec lvec))))

    (format t "~&Building composition exclusion table~%")
    (let ((exclusions (make-array 1 :element-type '(unsigned-byte 32)
				  :adjustable t
				  :fill-pointer 0)))
      (loop for ent across ucd do
	   (when (ucdent-comp-exclusion ent)
	     (vector-push-extend (ucdent-code ent) exclusions)))
      (setf (unidata-comp-exclusions *unicode-data*) (copy-seq exclusions)))

    (format t "~&Building full case mapping tables~%")
    (progn
      (format t "~&  Lower...~%")
      (let ((tabl (make-array 100 :element-type '(unsigned-byte 16)
			      :fill-pointer 0 :adjustable t))
	    (split #x65))
	(multiple-value-bind (hvec mvec lvec)
	    (pack ucd range (lambda (x) (pack-full-case x tabl #'ucdent-full-case-lower))
		  0 32 split)
	  (setf (unidata-full-case-lower *unicode-data*)
		(make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
				:tabl (copy-seq tabl)))))
      (format t "~&  Title...~%")
      (let ((tabl (make-array 100 :element-type '(unsigned-byte 16)
			      :fill-pointer 0 :adjustable t))
	    (split #x65))
	(multiple-value-bind (hvec mvec lvec)
	    (pack ucd range (lambda (x) (pack-full-case x tabl #'ucdent-full-case-title))
		  0 32 split)
	  (setf (unidata-full-case-title *unicode-data*)
		(make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
				:tabl (copy-seq tabl)))))
      (format t "~&  Upper...~%")
      (let ((tabl (make-array 100 :element-type '(unsigned-byte 16)
			      :fill-pointer 0 :adjustable t))
	    (split #x65))
	(multiple-value-bind (hvec mvec lvec)
	    (pack ucd range (lambda (x) (pack-full-case x tabl #'ucdent-full-case-upper))
		  0 32 split)
	  (setf (unidata-full-case-upper *unicode-data*)
		(make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
				:tabl (copy-seq tabl))))))

    (format t "~&Building case-folding tables~%")
    (progn
      (format t "~&  Simple...~%")
      (let ((split #x54))
	(multiple-value-bind (hvec mvec lvec)
	    (pack ucd range (lambda (x) (pack-case-folding-simple x))
		  0 32 split)
	  (setf (unidata-case-fold-simple *unicode-data*)
		(make-ntrie32 :split split :hvec hvec :mvec mvec :lvec lvec))))
      (format t "~&  Full...~%")
      (let ((tabl (make-array 100 :element-type '(unsigned-byte 16)
			      :fill-pointer 0 :adjustable t))
	    (split #x65))
	(multiple-value-bind (hvec mvec lvec)
	    (pack ucd range (lambda (x) (pack-case-folding-full x tabl))
		  0 32 split)
	  (setf (unidata-case-fold-full *unicode-data*)
		(make-case-folding :split split :hvec hvec :mvec mvec :lvec lvec
				   :tabl (copy-seq tabl))))))

    (format t "~&Building word-break table~%")
    (let ((split #x66))
      (multiple-value-bind (hvec mvec lvec)
	  (pack ucd range (lambda (x) (pack-word-break x))
		0 4 split)
	(setf (unidata-word-break *unicode-data*)
	      (make-ntrie4 :split split :hvec hvec :mvec mvec :lvec lvec))))
    nil))
