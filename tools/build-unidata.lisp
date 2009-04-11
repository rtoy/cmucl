;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/build-unidata.lisp,v 1.1.2.1 2009/04/11 12:04:27 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Build the Unicode data file.
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
  name1+
  name1
  category
  scase
  )

(defvar *unicode-data* (make-unidata))

;;; These need to be synched with code/unidata.lisp

(defstruct nametrie
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

(defstruct codeset
  (code (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 32) (*))))

(defstruct rangeset
  (min (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*)))
  (max (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*))))

(defstruct (name (:include codeset))
  (low (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 8) (*))))

(defstruct (category (:include rangeset)))

(defstruct (scase (:include codeset))
  (ext (ext:required-argument) :read-only t
       :type (simple-array (unsigned-byte 32) (*))))


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
;; the Unicode 1.0 names, and *CODEBOOK-E* is for HTML entity names.
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

;; *codebook-e* ??



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

(defun build-nametrie (codebook entries)
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
	(format t "~&Finalizing...~%")
	(dotimes (i top)
	  (let ((xxx (aref vec2 i)))
	    (dotimes (j (aref keyl (ash xxx -18)))
	      (setf (aref vec3 (+ (logand xxx #x3FFFF) j)) i))))
	(loop for (name . code) in entries do
	  (let ((n (name-lookup name codebook keyv keyl vec2)))
	    (unless n (error "Codepoint not found for ~S." name))
	    (setf (ldb (byte 14 18) (aref vec3 n)) (length name))))))
    (make-nametrie :cdbk codebook
		   :keyv keyv :keyl keyl
		   :codev vec1 :nextv vec2 :namev vec3)))



(defun build-names (data trie)		; (name . code)
  (let ((cdbk (nametrie-cdbk trie))
	(keyv (nametrie-keyv trie))
	(keyl (nametrie-keyl trie))
	(nextv (nametrie-nextv trie))
	(vec1 (make-array (length data) :element-type '(unsigned-byte 32)))
	(vec2 (make-array (length data) :element-type '(unsigned-byte 8)))
	(indx 0))
    (loop for (name . code) in data do
      (let ((id (name-lookup name cdbk keyv keyl nextv)))
	(when id
	  (setf (aref vec1 indx) (logior code (ash (ash id -8) 21))
		(aref vec2 indx) (logand id #xFF))
	  (incf indx))))
    (lisp::shrink-vector vec1 indx)
    (lisp::shrink-vector vec2 indx)
    (make-name :code vec1 :low vec2)))

(defun build-categories (data)		; (category . (min . max))
  (let ((vec1 (make-array (length data) :element-type '(unsigned-byte 32)))
	(vec2 (make-array (length data) :element-type '(unsigned-byte 32))))
    (loop for (cat . (min . max)) in data as indx upfrom 0 do
      (setf (aref vec1 indx) (logior (ash cat 21) min)
	    (aref vec2 indx) max))
    (make-category :min vec1 :max vec2)))

(defun build-scase (data)		; (code upper lower title)
  (let ((vec1 (make-array (length data) :element-type '(unsigned-byte 32)))
	(vec2 (make-array (length data) :element-type '(unsigned-byte 32))))
    (loop for (code upper lower title) in data as indx upfrom 0 do
      (let ((flag (if (= code upper) #.(ash 1 31) 0))
	    (case (if (= code upper) lower upper))
	    (ttl1 (ash (logand title #x7FF) 21))
	    (ttl2 (ash (logand title #x1FF800) 10)))
	(setf (aref vec1 indx) (logior code ttl2 flag)
	      (aref vec2 indx) (logior case ttl1))))
    (make-scase :code vec1 :ext vec2)))



(defun write-unidata (path)
  (labels ((write16 (n stm)
	     (write-byte (ldb (byte 8 8) n) stm)
	     (write-byte (ldb (byte 8 0) n) stm))
	   (write32 (n stm)
	     (write16 (ldb (byte 16 16) n) stm)
	     (write16 (ldb (byte 16 0) n) stm)))
    (with-open-file (stm path :direction :io :if-exists :rename-and-delete
			 :element-type '(unsigned-byte 8))
      (let ((index (make-array 5 :fill-pointer 0)))
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
	  (write32 (length (rangeset-min data)) stm)
	  (write-vector (rangeset-min data) stm :endian-swap :network-order)
	  (write-vector (rangeset-max data) stm :endian-swap :network-order))
	;; Character name data
	(let ((data (unidata-name+ *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write-byte (1- (length (nametrie-cdbk data))) stm)
	  (write16 (length (nametrie-keyv data)) stm)
	  (write32 (length (nametrie-codev data)) stm)
	  (let ((codebook (nametrie-cdbk data)))
	    (dotimes (i (length codebook))
	      (write-byte (length (aref codebook i)) stm)
	      (dotimes (j (length (aref codebook i)))
		(write-byte (char-code (char (aref codebook i) j)) stm))))
	  (write-vector (nametrie-keyv data) stm :endian-swap :network-order)
	  (write-vector (nametrie-keyl data) stm :endian-swap :network-order)
	  (write-vector (nametrie-codev data) stm :endian-swap :network-order)
	  (write-vector (nametrie-nextv data) stm :endian-swap :network-order)
	  (write-vector (nametrie-namev data) stm :endian-swap :network-order))
	;; Codepoint-to-name mapping
	(let ((data (unidata-name *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write32 (length (name-code data)) stm)
	  (write-vector (name-code data) stm :endian-swap :network-order)
	  (write-vector (name-low data) stm :endian-swap :network-order))
	;; Codepoint-to-category table
	(let ((data (unidata-category *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write32 (length (category-min data)) stm)
	  (write-vector (category-min data) stm :endian-swap :network-order)
	  (write-vector (category-max data) stm :endian-swap :network-order))
	;; Simple case mapping table
	(let ((data (unidata-scase *unicode-data*)))
	  (vector-push (file-position stm) index)
	  (write32 (length (scase-code data)) stm)
	  (write-vector (scase-code data) stm :endian-swap :network-order)
	  (write-vector (scase-ext data) stm :endian-swap :network-order))
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

(defun foreach-ucd (name fn &optional (defaults #p"target:unidata/"))
  (with-open-file (s (make-pathname :name name :type "txt"
				    :defaults defaults))
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

(defun read-data (&optional (defaults #p"target:unidata/"))
  (let ((vec (make-array 50000 :fill-pointer 0))
	(pos 0)
	(rmin (make-array 50 :element-type '(unsigned-byte 32)))
	(rmax (make-array 50 :element-type '(unsigned-byte 32)))
	(rpos 0))
    (flet ((cat (x) (dpb (position (char x 0) "CLMNPSZ") (byte 3 4)
			 (position (char x 1) "cdefiklmnopstu")))
	   (num (x) (if (string= x "") nil (read-from-string x)))
	   (chr (x) (if (string= x "") nil (parse-integer x :radix 16)))
	   (bool (x) (string= x "Y")))
      (foreach-ucd "UnicodeData"
	(lambda (min max name cat comb bidi decomp num1 num2 num3
		 mirror name1 comment upper lower title)
	  (when (> max min)
	    (setf (aref rmin rpos) min (aref rmax rpos) max)
	    (incf rpos))
	  (when (position #\( name1)
	    (setq name1 (subseq name1 0 (1- (position #\( name1)))))
	  (vector-push (make-ucdent min name (cat cat) (num comb) bidi decomp
				    (num num1) (num num2) (num num3) (bool mirror)
				    name1 comment (chr upper) (chr lower) (chr title))
		       vec)
	  (incf pos))
	defaults))
    #+nil
    (progn
      (lisp::shrink-vector vec pos)
      (lisp::shrink-vector rmin rpos)
      (lisp::shrink-vector rmax rpos))
    (values (map 'vector #'identity vec)
	    (map '(simple-array (unsigned-byte 32) (*)) #'identity rmin)
	    (map '(simple-array (unsigned-byte 32) (*)) #'identity rmax))))



(defun build-unidata (&optional (defaults #p"target:unidata/"))
  (format t "~&Reading data~%")
  (multiple-value-bind (ucd rmin rmax)
      (read-data defaults)
    (foreach-ucd "NameAliases"
      (lambda (min max alias)
	(declare (ignore max))
	(push alias (ucdent-aliases (find min ucd :key #'ucdent-code))))
      defaults)
    (setf (unidata-range *unicode-data*) (make-rangeset :min rmin :max rmax))
    (format t "~&Building character name tables~%")
    (let* ((data (loop for ent across ucd
		   when (char/= (char (ucdent-name ent) 0) #\<)
		     collect (cons (ucdent-name ent) (ucdent-code ent))
		   when (ucdent-aliases ent)
		     nconc (loop for name in (ucdent-aliases ent)
			     collect (cons name (ucdent-code ent)))))
	   (trie (build-nametrie *codebook* data)))
      (setf (unidata-name+ *unicode-data*) trie)
      (setf (unidata-name *unicode-data*) (build-names data trie)))
    (format t "~&Building Unicode 1.0 character name tables~%")
    (let* ((data (loop for ent across ucd
		   when (plusp (length (ucdent-name1 ent)))
		     collect (cons (ucdent-name1 ent) (ucdent-code ent))))
	   (trie (build-nametrie *codebook-1* data)))
      (setf (unidata-name1+ *unicode-data*) trie)
      (setf (unidata-name1 *unicode-data*) (build-names data trie)))
    (format t "~&Building category table~%")
    (let ((data (loop with data = '() for ent across ucd
		  do (unless (eql (ucdent-cat ent) (caar data))
		       (push (cons (ucdent-cat ent)
				   (cons (ucdent-code ent) (ucdent-code ent)))
			     data))
		     (let ((n (position (ucdent-code ent) rmin)))
		       (setf (cddar data)
			   (if n (aref rmax n) (ucdent-code ent))))
		  finally (return (nreverse data)))))
      (setf (unidata-category *unicode-data*) (build-categories data)))
    (format t "~&Building simple case-conversion table~%")
    (let ((data (loop for ent across ucd
		  when (or (ucdent-upper ent)
			   (ucdent-lower ent)
			   (ucdent-title ent))
		    collect (list (ucdent-code ent)
				  (or (ucdent-upper ent) (ucdent-code ent))
				  (or (ucdent-lower ent) (ucdent-code ent))
				  (or (ucdent-title ent) (ucdent-code ent))))))
      (setf (unidata-scase *unicode-data*) (build-scase data)))
    ;; ...
    ))

