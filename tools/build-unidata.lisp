;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/build-unidata.lisp,v 1.1.2.3 2009/04/15 14:41:56 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Build the Unicode data file
;;;
;;; This can be run from an 8-bit build just fine.  The Unicode data
;;; file is read and written in portable way so it can be used on any
;;; platform.
;;;
;;; What to do: Run (build-unidata "src/i18n/") then (write-unidata
;;; #p"src/pcl/simple-streams/external-formats/unidata.bin")
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
  name1+
  name1
  )

(defvar *unicode-data* (make-unidata))

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
	:type (simple-array (unsigned-byte 16) (*))))


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
      (format t "~&Initializing...~%")
      (let ((trie (cons nil nil)))
	(loop for (name . code) in entries do (add-to-trie trie name code))
	(format t "~&Pass 1...~%")
	(pass1 trie 0)
	(format t "~&Sorting...~%")
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
	(format t "~&Pass 2...~%")
	(pass2 trie)
	(format t "~&Finalizing~%")
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
	     (write16 (ldb (byte 16 0) n) stm)))
    (with-open-file (stm path :direction :io :if-exists :rename-and-delete
			 :element-type '(unsigned-byte 8))
      (let ((index (make-array 7 :fill-pointer 0)))
	;; File header
	(write32 #x2A554344 stm)	; identification "magic"
	(write-byte 0 stm)		; file format version
	(write-byte 5 stm)		; Unicode version major
	(write-byte 1 stm)		; Unicode version minor
	(write-byte 0 stm)		; Unicode version subminor
	(dotimes (i (array-dimension index 0))
	  (write32 0 stm))		; space for indices
	(write32 0 stm)			; end marker
	;; Range data
	(let ((data (unidata-range *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write32 (length (range-codes data)) stm)
	  (write-vector (range-codes data) stm :endian-swap :network-order))
	;; Character name data
	(let ((data (unidata-name+ *unicode-data*)))
	  (vector-push (file-position stm) index)
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
	;; Codepoint-to-name mapping
	(let ((data (unidata-name *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (ntrie32-split data) stm)
	  (write16 (length (ntrie32-hvec data)) stm)
	  (write16 (length (ntrie32-mvec data)) stm)
	  (write16 (length (ntrie32-lvec data)) stm)
	  (write-vector (ntrie32-hvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie32-mvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie32-lvec data) stm :endian-swap :network-order))
	;; Codepoint-to-category table
	(let ((data (unidata-category *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (ntrie8-split data) stm)
	  (write16 (length (ntrie8-hvec data)) stm)
	  (write16 (length (ntrie8-mvec data)) stm)
	  (write16 (length (ntrie8-lvec data)) stm)
	  (write-vector (ntrie8-hvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie8-mvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie8-lvec data) stm :endian-swap :network-order))
	;; Simple case mapping table
	(let ((data (unidata-scase *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (scase-split data) stm)
	  (write-byte (length (scase-svec data)) stm)
	  (write16 (length (scase-hvec data)) stm)
	  (write16 (length (scase-mvec data)) stm)
	  (write16 (length (scase-lvec data)) stm)
	  (write-vector (scase-hvec data) stm :endian-swap :network-order)
	  (write-vector (scase-mvec data) stm :endian-swap :network-order)
	  (write-vector (scase-lvec data) stm :endian-swap :network-order)
	  (write-vector (scase-svec data) stm :endian-swap :network-order))
	;; Numeric data
	(let ((data (unidata-numeric *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (ntrie32-split data) stm)
	  (write16 (length (ntrie32-hvec data)) stm)
	  (write16 (length (ntrie32-mvec data)) stm)
	  (write16 (length (ntrie32-lvec data)) stm)
	  (write-vector (ntrie32-hvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie32-mvec data) stm :endian-swap :network-order)
	  (write-vector (ntrie32-lvec data) stm :endian-swap :network-order))
	;; Decomposition data
	(let ((data (unidata-decomp *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (decomp-split data) stm)
	  (write16 (length (decomp-hvec data)) stm)
	  (write16 (length (decomp-mvec data)) stm)
	  (write16 (length (decomp-lvec data)) stm)
	  (write16 (length (decomp-tabl data)) stm)
	  (write-vector (decomp-hvec data) stm :endian-swap :network-order)
	  (write-vector (decomp-mvec data) stm :endian-swap :network-order)
	  (write-vector (decomp-lvec data) stm :endian-swap :network-order)
	  (write-vector (decomp-tabl data) stm :endian-swap :network-order))
	;; Patch up index
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
  ;; ...
  )

(defun foreach-ucd (name fn)
  (with-open-file (s (make-pathname :name name :type "txt"
				    :defaults #p"ext-formats:"))
    (if (string= name "Unihan")
	(loop for line = (read-line s nil) while line do
	  (when (char= (char line 0) #\U)
	    (let* ((tab1 (position #\Tab line))
		   (tab2 (position #\Tab line :start (1+ tab1))))
	      (funcall fn
		       (parse-integer line :radix 16 :start 2 :end tab1)
		       (subseq line (1+ tab1) tab2)
		       (subseq line (1+ tab2))))))
	(loop with save = 0
	       for line = (read-line s nil) as end = (position #\# line)
	  while line do
	    (when (position #\; line :end end)
	      (let* ((end (position #\# line))
		     (split (loop for i = 0 then (1+ j)
				   as j = (position #\; line :start i :end end)
			       collect (subseq line i j) while j))
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
		       (apply fn lo hi (rest split))))))))))

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

(declaim (ftype (function () (values simple-vector range)) read-data))
(defun read-data ()
  (let ((vec (make-array 50000))
	(pos 0)
	(range (make-array 50 :element-type '(unsigned-byte 32)))
	(rpos 0))
    (flet ((cat (x) (dpb (position (char x 0) "CLMNPSZ") (byte 3 4)
			 (position (char x 1) "ncdefiklmopstu")))
	   (num (x) (if (string= x "") nil (read-from-string x)))
	   (chr (x) (if (string= x "") nil (parse-integer x :radix 16)))
	   (str (x) (if (string= x "") nil x))
	   (bool (x) (string= x "Y"))
	   (decomp (x) (if (string= x "") nil (parse-decomposition x))))
      (foreach-ucd "UnicodeData"
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
    (lisp::shrink-vector vec pos)
    (lisp::shrink-vector range rpos)
    (values vec (make-range :codes range))))



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

(defconstant +decomposition-type+
  '(nil "<compat>" "<initial>" "<medial>" "<final>" "<isolated>"
    "<super>" "<sub>" "<fraction>" "<font>" "<noBreak>"
    "<vertical>" "<wide>" "<narrow>" "<small>" "<square>" "<circle>"))

(defun pack-decomp (ucdent tabl)
  (if (not (ucdent-decomp ucdent))
      0
      (let* ((d (loop for i in (rest (ucdent-decomp ucdent))
		  if (<= i #xFFFF) collect i
		  else collect (logior (ldb (byte 10 10) i) #xD800)
		   and collect (logior (ldb (byte 10 0) i) #xDC00)))
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

(defun build-unidata ()
  (format t "~&Reading data~%")
  (multiple-value-bind (ucd range) (read-data)
    (foreach-ucd "NameAliases"
      (lambda (min max alias)
	(declare (ignore max))
	(push alias (ucdent-aliases (find min ucd :key #'ucdent-code)))))
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
	  (make-ntrie32 :split #x72 :hvec hvec :mvec mvec :lvec lvec))))
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
    nil))
