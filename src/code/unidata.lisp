;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: LISP -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;; 
(ext:file-comment "$Header: src/code/unidata.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Unicode Database access
;;;
;;; See tools/build-unidata.lisp for the instructions and code to
;;; create the unicode database.

(in-package "LISP")
(intl:textdomain "cmucl")

(export '(string-to-nfd string-to-nfkc string-to-nfkd string-to-nfc
	  unicode-complete unicode-complete-name
	  unicode-full-case-lower
	  unicode-full-case-upper
	  unicode-full-case-title
	  unicode-category
	  +unicode-category-lower+
	  +unicode-category-other+
	  +unicode-category-graphic+
	  +unicode-category-upper+
	  +unicode-category-title+
	  load-all-unicode-data))

(defvar *unidata-path* #p"ext-formats:unidata.bin")

(defvar *unidata-version* "$Revision: 1.29 $")

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
  case-fold-simple
  case-fold-full
  word-break
  )

(defvar *unicode-data* (make-unidata))

;; The magic number for the unidata.bin file.  (It's "*UCD", in
;; big-endian order).
(defconstant +unicode-magic-number+ #x2A554344)

;; The format version for the unidata.bin file.
(defconstant +unicode-format-version+ 0)

;; The expected Unicode version.  This needs to be synced with
;; build-unidata.lisp.
(defconstant +unicode-major-version+ 6)
(defconstant +unicode-minor-version+ 2)
(defconstant +unicode-update-version+ 0)

;;; These need to be synched with tools/build-unidata.lisp

;;; ==== DICTIONARY ====
;;
;; A Dictionary is a mapping from names to codepoints.
;; 
;; Imagine we have the following structure:
;; 
;;    node = (value . ((char1 . node1) (char2 . node2) ...))
;; 
;; To look up a string, we start at the root node (representing the
;; empty string), and look up the first character in the alist that is
;; (cdr node); assuming the character is found, repeat using the
;; associated node and the next character of the string.  After the
;; last character, the current node's value is the value associated
;; with that string (i.e., the codepoint).  If at any point the next
;; character is not present in the node, or you reach the end and the
;; value in the current node is NIL, the string is not in the
;; dictionary.
;; 
;; As an example, let's have a dictionary containing three words:
;;   "cat" = 1, "dog" = 2, "cow" = 3
;; 
;; then we have
;; 
;;   (nil . (#\c . (nil . (#\a . (nil . (#\t . (1 . nil))))
;;                        (#\o . (nil . (#\w . (3 . nil))))))
;;          (#\d . (nil . (#\o . (nil . (#\g . (2 . nil)))))))
;; 
;; or, broken down to make it easier to read,
;; 
;;    root = (nil . (#\c . node1) (#\d . node2))
;;   node1 = (nil . (#\a . node3) (#\o . node4))
;;   node2 = (nil . (#\o . node5))
;;   node3 = (nil . (#\t . node6))
;;   node4 = (nil . (#\w . node7))
;;   node5 = (nil . (#\g . node8))
;;   node6 = (1 . nil)
;;   node7 = (3 . nil)
;;   node8 = (2 . nil)
;; 
;; To look up "dog", start with the root node;
;;   look for #\d in the alist at (cdr root)  - it maps to node2
;;   look for #\o in the alist at (cdr node2) - it maps to node5
;;   look for #\g in the alist at (cdr node5) - it maps to node8
;;   no more characters, so return the value in node8, which is 2
;; 
;; A Dictionary is just a more compact version of that:
;;
;; There are two pairs of vectors: NEXTV and CODEV is one pair, KEYV
;; and KEYL are the second (ignore NAMEV for now).
;;
;; The "current node" is represented by an index into the NEXTV/CODEV
;; pair.  Given node i (initially 0, the root), NEXTV[i] is split into
;; two numbers: x (14 bits) and y (18 bits).  n = KEYL[x], and
;; KEYV[x:x+n] is the set of characters that occur in the alist of
;; node i.  In the example above, when i=0, KEYL[x] will be 2 and
;; KEYV[x:x+2] will be "cd".  If the character you're searching for
;; occurs at x+m, then y+m is the index of the corresponding node.
;; CODEV[i] is the value associated with each node (or -1 for none).
;; 
;; The above example could be rendered as:
;; 
;;   keyv = c  d  a  o  t  w  g
;;   keyl = 2  0  2  1  1  1  1
;; 
;;   nextv = [0:1][2:3][3:5][4:6][5:7][6:8][1:0][1:0][1:0]
;;   codev =   -1   -1   -1   -1   -1   -1   1    3    2
;; 
;; To look up "dog", start with i=0
;;   nextv[i] = [0:1]   i.e., x=0, y=1
;;   n = keyl[x] = 2
;;   keyv[x:x+n] = "cd"
;;   m = (position #\d "cd") = 1
;;   i = y+m = 2
;; 
;;   nextv[i] = [3:5]   i.e., x=3, y=5
;;   n = keyl[x] = 1
;;   keyv[x:x+n] = "o"
;;   m = (position #\o "o") = 0
;;   i = y+m = 5
;; 
;;   nextv[i] = [6:8]   i.e., x=6, y=8
;;   n = keyl[x] = 1
;;   keyv[x:x+n] = "g"
;;   m = (position #\g "g") = 0
;;   i = y+m = 8
;; 
;;   codev[i] = 2
;; 
;; 
;; But rather than using only the restricted set of characters that
;; occur in Unicode character names, we encode the names using a
;; codebook of up to 256 strings, turning, for example, "LATIN SMALL
;; LETTER A" into "ABCBDBE" (where "A" encodes "LATIN", "B" encodes
;; space, and so on), so we only have to go through 7 nodes to reach
;; the end, instead of 20, reducing the size of the tables.
;; 
;; The NAMEV vector is used to walk backwards through the trie,
;; reconstructing the string from the final node index.  Again, each
;; entry consists of a 14 bit and an 18 bit number.  In the example
;; above, we would have
;; 
;;   namev = [?:0][?:0][?:0][?:1][?:1][?:2][3:3][3:4][3:5]      ? = don't care
;; 
;; The high 14 bits give the length of the string; the low 18 bits
;; give the previous index.  Having looked up "dog", the final node
;; was 8:
;; 
;;   i = 8
;;   namev[i] = [3:5]       -- the string is 3 characters long
;;   nextv[5] = [6:8]   i.e., x=6, y=8
;;   z = i-y = 0
;;   keyv[x+z] = "g"        -- the string is "__g"
;;   i = 5
;; 
;;   namev[i] = [?:2]
;;   nextv[2] = [3:5]   i.e., x=3, y=5
;;   z = i-y = 0
;;   keyv[x+z] = "o"        -- the string is "_og"
;;   i = 2
;; 
;;   namev[i] = [?:0]
;;   nextv[0] = [0:1]   i.e., x=0, y=1
;;   z = i-y = 1
;;   keyv[x+z] = "d"        -- the string is "dog"
;;   i = 0  (finished)

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


;;; ==== NTRIE ====
;; 
;; An NTrie is a mapping from codepoints to integers.
;; 
;; The codepoint is split into three pieces, x:y:z where x+y+z = 21
;; bits.  The value of the SPLIT slot encodes y-1 in the upper 4 bits
;; and z-1 in the lower 4 bits.  I.e., if SPLIT is #x63, the split is
;; 10:7:4
;; 
;; p = HVEC[x] is either an index into MVEC or #xFFFF representing no
;; value.  q = MVEC[y+p] is either an index into LVEC or #xFFFF
;; representing no value.  LVEC[z+q] is the value associated with the
;; codepoint.
;; 
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
  (tabl (ext:required-argument) :read-only t :type simple-string))

(defstruct (full-case (:include ntrie32))
  (tabl (ext:required-argument) :read-only t :type simple-string))

(defstruct (case-fold-full (:include decomp)))

(defstruct (bidi (:include ntrie16))
  (tabl (ext:required-argument) :read-only t
	:type (simple-array (unsigned-byte 16) (*))))

(defconstant +decomposition-type+
  '(nil "<compat>" "<initial>" "<medial>" "<final>" "<isolated>"
    "<super>" "<sub>" "<fraction>" "<font>" "<noBreak>"
    "<vertical>" "<wide>" "<narrow>" "<small>" "<square>" "<circle>"))

(defconstant +bidi-class+
  #("L" "LRE" "LRO" "R" "AL" "RLE" "RLO" "PDF" "EN" "ES" "ET" "AN" "CS"
    "NSM" "BN" "B" "S" "WS" "ON"))


(defconstant +hangul-choseong+		; U+1100..U+1112
  #("G" "GG" "N" "D" "DD" "R" "M" "B" "BB" "S" "SS" "" "J" "JJ" "C" "K" "T" "P"
    "H"))

(defconstant +hangul-jungseong+		; U+1161..U+1175
  #("A" "AE" "YA" "YAE" "EO" "E" "YEO" "YE" "O" "WA" "WAE" "OE" "YO" "U" "WEO"
    "WE" "WI" "YU" "EU" "YI" "I"))

(defconstant +hangul-jongseong+		; U+11A8..U+11C2
  #("G" "GG" "GS" "N" "NJ" "NH" "D" "L" "LG" "LM" "LB" "LS" "LT" "LP" "LH" "M"
    "B" "BS" "S" "SS" "NG" "J" "C" "K" "T" "P" "H"))



(defun search-dictionary (string dictionary &optional (current 0) (posn 0))
  "Search the Unicode name dictionary for the longest entry that
  matches STRING.  STRING must be in Unicode name format.  That is, it
  must be upper case with spaces separating each word.

  Two values are returned.  The first value is index into the codebook
  that continues the string..  The second value is the length of the
  substring of string that matches the codebook. "
  
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type string string) (type dictionary dictionary)
	   (type (unsigned-byte 32) current) (type lisp::index posn))
  (let* ((codebook (dictionary-cdbk dictionary))
	 (stack '()))
    (loop
       (let ((keyv (ash (aref (dictionary-nextv dictionary) current) -18)))
	 (dotimes (i (aref (dictionary-keyl dictionary) keyv)
		   (if stack
		       (let ((next (pop stack)))
			 (setq posn (car next) current (cdr next)))
		       (return-from search-dictionary nil)))
	   (let* ((str (aref codebook (aref (dictionary-keyv dictionary)
					    (+ keyv i))))
		  (len (length str)))
	     (declare (type simple-base-string str))
	     (cond ((and (>= (length string) (+ posn len))
			 (string= string str :start1 posn :end1 (+ posn len)))
		    (setq current
			  (+ (logand (aref (dictionary-nextv dictionary) current)
				     #x3FFFF)
			     i))
		    (when (= (incf posn len) (length string))
		      (return-from search-dictionary (values current posn)))
		    (return))		; from DOTIMES - loop again
		   ((and (< (length string) (+ posn len))
			 (string= string str :start1 posn :end2 (- (length string) posn)))
		    (return-from search-dictionary (values current posn))))
	     (when (or (string= str " ") (string= str "-"))
	       (push (cons posn
			   (+ (logand (aref (dictionary-nextv dictionary)
					    current)
				      #x3FFFF)
			      i))
		     stack))))))))

;; Like SEARCH-DICTIONARY, but we don't try to do partial matches.  We
;; do an exact match on the given string.
(defun exact-match-dictionary (string dictionary)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type string string) (type dictionary dictionary))
  (let* ((codebook (dictionary-cdbk dictionary))
	 (current 0)
	 (posn 0)
	 (stack '()))
    (declare (type (unsigned-byte 32) current) (type lisp::index posn))
    (loop
      (let ((keyv (ash (aref (dictionary-nextv dictionary) current) -18)))
	(dotimes (i (aref (dictionary-keyl dictionary) keyv)
		    (if stack
			(let ((next (pop stack)))
			  (setq posn (car next) current (cdr next)))
			(return-from exact-match-dictionary nil)))
	  (let* ((str (aref codebook (aref (dictionary-keyv dictionary)
					   (+ keyv i))))
		 (len (length str)))
	    (declare (type simple-base-string str))
	    (when (and (>= (length string) (+ posn len))
		       (string= string str :start1 posn :end1 (+ posn len)))
	      (setq current
		  (+ (logand (aref (dictionary-nextv dictionary) current)
			     #x3FFFF)
		     i))
	      (when (= (incf posn len) (length string))
		(return-from exact-match-dictionary current))
	      (return))			; from DOTIMES - loop again
	    (when (or (string= str " ") (string= str "-"))
	      (push (cons posn
			  (+ (logand (aref (dictionary-nextv dictionary)
					   current)
				     #x3FFFF)
			     i))
		    stack))))))))

(defun search-range (code range)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code) (type range range))
  (let* ((set (range-codes range))
	 (min 0)
	 (max (length set)))
    (declare (type lisp::index min max))
    (loop for n of-type lisp::index = (logand #xFFFFFE (ash (+ min max) -1)) do
      (let* ((dmin (logand #x1FFFFF (aref set n)))
	     (dmax (logand #x1FFFFF (aref set (1+ n)))))
	(cond ((< code dmin)
	       (when (= (the lisp::index (setq max n)) min) (return nil)))
	      ((> code dmax)
	       (when (= (setq min (+ n 2)) max) (return nil)))
	      (t (return (ash n -1))))))))

(declaim (inline qref qref1 qref2 qref4 qref8 qref16 qref32))
(defun qref (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie ntrie) (type codepoint code))
  (let* ((mbits (1+ (ash (ntrie-split ntrie) -4)))
	 (lbits (1+ (logand (ntrie-split ntrie) 15)))
	 (hvec (ntrie-hvec ntrie))
	 (mvec (ntrie-mvec ntrie))
	 (hi (aref hvec (ash code (- (+ mbits lbits)))))
	 (md (logand (ash code (- lbits)) (lognot (ash -1 mbits))))
	 (lo (logand code (lognot (ash -1 lbits)))))
    (declare (type (simple-array (unsigned-byte 16) (*)) hvec mvec))
    (if (= hi #xFFFF)
	nil
	(let ((md (aref mvec (+ hi md))))
	  (if (= md #xFFFF)
	      nil
	      (+ md lo))))))

(defun qref1 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie1 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie1-lvec ntrie) n) 0)))

(defun qref2 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie2 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie2-lvec ntrie) n) 0)))

(defun qref4 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie4 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie4-lvec ntrie) n) 0)))

(defun qref8 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie8 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie8-lvec ntrie) n) 0)))

(defun qref16 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type ntrie16 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie16-lvec ntrie) n) 0)))

(defun qref32 (ntrie code)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (ext:inhibit-warnings 3)) ;; shut up about boxing return
	   (type ntrie32 ntrie) (type codepoint code))
  (let ((n (qref ntrie code)))
    (if n (aref (ntrie32-lvec ntrie) n) 0)))



(defun unidata-locate (stream index)
  (labels ((read16 (stm)
	     (logior (ash (read-byte stm) 8) (read-byte stm)))
	   (read32 (stm)
	     (logior (ash (read16 stm) 16) (read16 stm))))
    (unless (and (= (read32 stream) +unicode-magic-number+)
		 (= (read-byte stream) +unicode-format-version+))
      (error (intl:gettext "The Unicode data file is broken.")))
    (let ((a (read-byte stream))
	  (b (read-byte stream))
	  (c (read-byte stream)))
      (unless (and (= a +unicode-major-version+)
		   (= b +unicode-minor-version+)
		   (= c +unicode-update-version+))
	(warn (intl:gettext "Unicode data file is for Unicode ~D.~D.~D") a b c)))
    (dotimes (i index)
      (when (zerop (read32 stream))
	(return-from unidata-locate nil)))
    (let ((n (read32 stream)))
      (and (plusp n) (file-position stream n)))))

;; List of all defined defloaders
(defvar *defloaders* nil)

(defmacro defloader (name (stm locn) &body body)
  `(progn
     (push ',name *defloaders*)
     (defun ,name ()
       (labels ((read16 (stm)
		  (logior (ash (read-byte stm) 8) (read-byte stm)))
		(read32 (stm)
		  (logior (ash (read16 stm) 16) (read16 stm)))
		(read-ntrie (bits stm)
		  (let* ((split (read-byte stm))
			 (hlen (read16 stm))
			 (mlen (read16 stm))
			 (llen (read16 stm))
			 (hvec (make-array hlen
					   :element-type '(unsigned-byte 16)))
			 (mvec (make-array mlen
					   :element-type '(unsigned-byte 16)))
			 (lvec (make-array llen
					   :element-type (list 'unsigned-byte bits))))
		    (read-vector hvec stm :endian-swap :network-order)
		    (read-vector mvec stm :endian-swap :network-order)
		    (read-vector lvec stm :endian-swap :network-order)
		    (values split hvec mvec lvec))))
	 (declare (ignorable #'read16 #'read32 #'read-ntrie))
	 (with-open-file (,stm *unidata-path* :direction :input
					      :element-type '(unsigned-byte 8))
	   (unless (unidata-locate ,stm ,locn)
	     (error (intl:gettext "No data in file.")))
	   ,@body)))))

(defloader load-range (stm 0)
  (let* ((n (read32 stm))
	 (codes (make-array n :element-type '(unsigned-byte 32))))
    (read-vector codes stm :endian-swap :network-order)
    (setf (unidata-range *unicode-data*)
	(make-range :codes codes))))

(defloader load-names (stm 1)
  (let* ((cb (1+ (read-byte stm)))
	 (kv (read16 stm))
	 (cv (read32 stm))
	 (codebook (make-array cb))
	 (keyv (make-array kv :element-type '(unsigned-byte 8)))
	 (keyl (make-array kv :element-type '(unsigned-byte 8)))
	 (codev (make-array cv :element-type '(signed-byte 32)))
	 (nextv (make-array cv :element-type '(unsigned-byte 32)))
	 (namev (make-array cv :element-type '(unsigned-byte 32))))
    (dotimes (i cb)
      (let* ((n (read-byte stm))
	     (s (make-string n)))
	(setf (aref codebook i) s)
	(dotimes (i n) (setf (char s i) (code-char (read-byte stm))))))
    (read-vector keyv stm :endian-swap :network-order)
    (read-vector keyl stm :endian-swap :network-order)
    (read-vector codev stm :endian-swap :network-order)
    (read-vector nextv stm :endian-swap :network-order)
    (read-vector namev stm :endian-swap :network-order)
    (setf (unidata-name+ *unicode-data*)
	(make-dictionary :cdbk codebook :keyv keyv :keyl keyl
			 :codev codev :nextv nextv :namev namev))))

(defloader load-name (stm 2)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 32 stm)
    (setf (unidata-name *unicode-data*)
	(make-ntrie32 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-categories (stm 3)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 8 stm)
    (setf (unidata-category *unicode-data*)
	(make-ntrie8 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-scase (stm 4)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 32 stm)
    (let* ((slen (read-byte stm))
	   (svec (make-array slen :element-type '(unsigned-byte 16))))
      (read-vector svec stm :endian-swap :network-order)
      (setf (unidata-scase *unicode-data*)
	  (make-scase :split split :hvec hvec :mvec mvec :lvec lvec
		      :svec svec)))))

(defloader load-numerics (stm 5)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 32 stm)
    (setf (unidata-numeric *unicode-data*)
	(make-ntrie32 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-decomp (stm 6)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 32 stm)
    (let* ((tlen (read16 stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-decomp *unicode-data*)
	  (make-decomp :split split :hvec hvec :mvec mvec :lvec lvec
		       :tabl (map 'simple-string #'code-char tabl))))))

(defloader load-combining (stm 7)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 8 stm)
    (setf (unidata-combining *unicode-data*)
	(make-ntrie8 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-bidi (stm 8)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 16 stm)
    (let* ((tlen (read-byte stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-bidi *unicode-data*)
	  (make-bidi :split split :hvec hvec :mvec mvec :lvec lvec
		     :tabl tabl)))))

(defloader load-1.0-names (stm 9)
  (let* ((cb (1+ (read-byte stm)))
	 (kv (read16 stm))
	 (cv (read32 stm))
	 (codebook (make-array cb))
	 (keyv (make-array kv :element-type '(unsigned-byte 8)))
	 (keyl (make-array kv :element-type '(unsigned-byte 8)))
	 (codev (make-array cv :element-type '(signed-byte 32)))
	 (nextv (make-array cv :element-type '(unsigned-byte 32)))
	 (namev (make-array cv :element-type '(unsigned-byte 32))))
    (dotimes (i cb)
      (let* ((n (read-byte stm))
	     (s (make-string n)))
	(setf (aref codebook i) s)
	(dotimes (i n) (setf (char s i) (code-char (read-byte stm))))))
    (read-vector keyv stm :endian-swap :network-order)
    (read-vector keyl stm :endian-swap :network-order)
    (read-vector codev stm :endian-swap :network-order)
    (read-vector nextv stm :endian-swap :network-order)
    (read-vector namev stm :endian-swap :network-order)
    (setf (unidata-name1+ *unicode-data*)
	(make-dictionary :cdbk codebook :keyv keyv :keyl keyl
			 :codev codev :nextv nextv :namev namev))))

(defloader load-1.0-name (stm 10)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 32 stm)
    (setf (unidata-name1 *unicode-data*)
	(make-ntrie32 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-normalization-qc (stm 11)
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 1 stm)
    (setf (unidata-qc-nfd *unicode-data*)
	(make-ntrie1 :split split :hvec hvec :mvec mvec :lvec lvec)))
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 1 stm)
    (setf (unidata-qc-nfkd *unicode-data*)
	(make-ntrie1 :split split :hvec hvec :mvec mvec :lvec lvec)))
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 2 stm)
    (setf (unidata-qc-nfc *unicode-data*)
	(make-ntrie2 :split split :hvec hvec :mvec mvec :lvec lvec)))
  (multiple-value-bind (split hvec mvec lvec) (read-ntrie 2 stm)
    (setf (unidata-qc-nfkc *unicode-data*)
	(make-ntrie2 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-composition-exclusions (stm 12)
  (let* ((len (read16 stm))
	 (ex (make-array len :element-type '(unsigned-byte 32))))
    (read-vector ex stm :endian-swap :network-order)
    (setf (unidata-comp-exclusions *unicode-data*) ex)))

(defloader load-full-case-lower (stm 13)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 32 stm)
    (let* ((tlen (read16 stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-full-case-lower *unicode-data*)
	    (make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
			    :tabl (map 'simple-string #'code-char tabl))))))

(defloader load-full-case-title (stm 14)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 32 stm)
    (let* ((tlen (read16 stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-full-case-title *unicode-data*)
	    (make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
			    :tabl (map 'simple-string #'code-char tabl))))))

(defloader load-full-case-upper (stm 15)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 32 stm)
    (let* ((tlen (read16 stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-full-case-upper *unicode-data*)
	    (make-full-case :split split :hvec hvec :mvec mvec :lvec lvec
			    :tabl (map 'simple-string #'code-char tabl))))))

(defloader load-case-fold-simple (stm 16)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 32 stm)
    (setf (unidata-case-fold-simple *unicode-data*)
	  (make-ntrie32 :split split :hvec hvec :mvec mvec :lvec lvec))))

(defloader load-case-fold-full (stm 17)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 32 stm)
    (let* ((tlen (read16 stm))
	   (tabl (make-array tlen :element-type '(unsigned-byte 16))))
      (read-vector tabl stm :endian-swap :network-order)
      (setf (unidata-case-fold-full *unicode-data*)
	    (make-case-fold-full :split split :hvec hvec :mvec mvec :lvec lvec
				 :tabl (map 'simple-string #'code-char tabl))))))

(defloader load-word-break (stm 18)
  (multiple-value-bind (split hvec mvec lvec)
      (read-ntrie 4 stm)
    (setf (unidata-word-break *unicode-data*)
	  (make-ntrie4 :split split :hvec hvec :mvec mvec :lvec lvec))))

;;; Accessor functions.

(defvar *reverse-hangul-choseong*)
(defvar *reverse-hangul-jungseong*)
(defvar *reverse-hangul-jongseong*)

(declaim (inline cjk-ideograph-p hangul-syllable-p))
(defun cjk-ideograph-p (code)
  ;; Search src/i18n/UnicodeData.txt for "CJK Ideograph" to find the
  ;; values here.
  (or (<= #x3400 code #x4DB5)		; CJK Ideograph Extension A
      (<= #x4E00 code #x9FCB)		; CJK Ideograph
      (<= #x20000 code #x2A6D6)		; CJK Ideograph Extension B
      (<= #X2A700 code #X2B734)))	; CJK Ideograph Extension C

(defun hangul-syllable-p (code)
  ;; Search src/i18n/UnicodeData.txt for "Hangule Syllable" to find
  ;; the values here.
  (<= #xAC00 code #xD7A3))

(defun initialize-reverse-hangul-tables ()
  (unless (boundp '*reverse-hangul-choseong*)
    (setq *reverse-hangul-choseong*
	  (sort (coerce (loop for x across +hangul-choseong+
			   as i upfrom 0 by 588
			   collect (cons x i))
			'vector)
		#'> :key (lambda (x) (length (car x)))))
    (setq *reverse-hangul-jungseong*
	  (sort (coerce (loop for x across +hangul-jungseong+
			   as i upfrom 0 by 28
			   collect (cons x i))
			'vector)
		#'> :key (lambda (x) (length (car x)))))
    (setq *reverse-hangul-jongseong*
	  (sort (coerce (loop for x across +hangul-jongseong+
			   as i upfrom 1
			   collect (cons x i))
			'vector)
		#'> :key (lambda (x) (length (car x)))))))

(defun unicode-name-to-codepoint (name)
  (declare (type string name))
  (cond ((and (> (length name) 22)
	      (or (string= name "CJK UNIFIED" :end1 11)
		  (string= name "CJKUNIFIED" :end1 10)))
	 (let* ((x (search "IDEOGRAPH" name))
		(n (and x (position-if (lambda (x) (digit-char-p x 16)) name
				       :start (+ x 9))))
		(code (and n (values (parse-integer name :start n :radix 16)))))
	   
	   (when (and code (cjk-ideograph-p code))
	     code)))
	((and (> (length name) 15)
	      (or (string= name "HANGUL SYLLABLE" :end1 15)
		  (string= name "HANGULSYLLABLE" :end1 14)))
	 (let* ((x (search "SYLLABLE" name))
		(n (position-if (lambda (x) (alpha-char-p x)) name
				:start (+ x 8)))
		(ll nil) (vv nil) (tt 0))
	   (unless n (return-from unicode-name-to-codepoint nil))
	   (initialize-reverse-hangul-tables)
	   (loop for (x . y) across *reverse-hangul-choseong*
	     when (and (<= (+ n (length x)) (length name))
		       (string= name x :start1 n :end1 (+ n (length x))))
	      do (incf n (length x))
		 (setq ll y)
		 (return))
	   (loop for (x . y) across *reverse-hangul-jungseong*
	     when (and (<= (+ n (length x)) (length name))
		       (string= name x :start1 n :end1 (+ n (length x))))
	      do (incf n (length x))
		 (setq vv y)
		 (return))
	   (when (< n (length name))
	     (loop for (x . y) across *reverse-hangul-jongseong*
	       when (and (<= (+ n (length x)) (length name))
			 (string= name x :start1 n :end1 (+ n (length x))))
		do (incf n (length x))
		   (setq tt y)
		   (return)))
	   (if (and ll vv (= n (length name)))
	       (+ ll vv tt #xAC00)
	       nil)))
	(t
	 (unless (unidata-name+ *unicode-data*) (load-names))
	 (let* ((names (unidata-name+ *unicode-data*))
		(n (exact-match-dictionary name names)))
	   (when n
	     (let ((cp (aref (dictionary-codev names) n)))
	       (if (minusp cp) nil cp)))))))

(defun unicode-1.0-name-to-codepoint (name)
  (declare (type string name))
  (unless (unidata-name1+ *unicode-data*) (load-1.0-names))
  (let* ((names (unidata-name1+ *unicode-data*))
	 (n (exact-match-dictionary name names)))
      (when n
	(let ((cp (aref (dictionary-codev names) n)))
	  (if (minusp cp) nil cp)))))

(defun unicode-name+ (code ntrie dict)
  (declare (optimize (speed 3) (space 0) (safety 0)
		     (ext:inhibit-warnings 3))
	   (type codepoint code)
	   (type ntrie32 ntrie) (type dictionary dict))
  (let ((n (qref32 ntrie code)))
    (when (plusp n)
      (let* ((codebook (dictionary-cdbk dict))
	     (namev (dictionary-namev dict))
	     (nextv (dictionary-nextv dict))
	     (keyv (dictionary-keyv dict))
	     (p (ash (aref namev n) -18))
	     (s (make-string p)))
	(loop while (plusp n) do
	  (let* ((prev (logand (aref namev n) #x3FFFF))
		 (temp (aref nextv prev))
		 (base (logand temp #x3FFFF))
		 (str (aref codebook
			    (aref keyv (+ (ash temp -18) (- n base))))))
	    (declare (type simple-base-string str))
	    (setq p (- p (length str)) n prev)
	    (replace s str :start1 p)))
	s))))

(defun unicode-name (code)
  (cond ((cjk-ideograph-p code)
	 (format nil "CJK UNIFIED IDEOGRAPH-~4,'0X" code))
	((hangul-syllable-p code)	; Hangul Syllable
	 (apply #'concatenate 'string "HANGUL SYLLABLE "
		(loop for ch across (unicode-decomp code)
		       as code = (char-code ch)
		  collect (cond ((<= #x1100 code #x1112)
				 (aref +hangul-choseong+ (- code #x1100)))
				((<= #x1161 code #x1175)
				 (aref +hangul-jungseong+ (- code #x1161)))
				((<= #x11A8 code #x11C2)
				 (aref +hangul-jongseong+ (- code #x11A8)))))))
	(t
	 (unless (unidata-name+ *unicode-data*) (load-names))
	 (unless (unidata-name *unicode-data*) (load-name))
	 (unicode-name+ code (unidata-name *unicode-data*)
			(unidata-name+ *unicode-data*)))))

(defun unicode-1.0-name (code)
  (unless (unidata-name1+ *unicode-data*) (load-1.0-names))
  (unless (unidata-name1 *unicode-data*) (load-1.0-name))
  (unicode-name+ code (unidata-name1 *unicode-data*)
		 (unidata-name1+ *unicode-data*)))

(declaim (inline unicode-category))
(defun unicode-category (code)
  (declare (type codepoint code))
  (unless (unidata-category *unicode-data*) (load-categories))
  (qref8 (the ntrie8 (unidata-category *unicode-data*)) code))

(defun unicode-category-string (code)
  (let ((n (unicode-category code))
	(s (make-string 2)))
    (setf (schar s 0) (schar "CZMPNLS?????????" (ldb (byte 4 4) n))
	  (schar s 1) (schar "nsifepkcdmulto??" (ldb (byte 4 0) n)))
    s))

(declaim (inline unicode-assigned-codepoint-p))
(defun unicode-assigned-codepoint-p (code)
  (not (zerop (unicode-category code))))

;; Look at unicode-category-string to see how we get these numbers.
;; +unicode-category-graphic+ is the first graphic character
;; +unicode-category-letter+ is the first letter
;; +unicode-category-upper+, +unicode-category-lower+, +unicode-category-title+
;; are uppercase, lowercase, and titlecase letters respectively.
(defconstant +unicode-category-graphic+ #x30)
(defconstant +unicode-category-letter+ #x50)
(defconstant +unicode-category-upper+ #x5a)
(defconstant +unicode-category-lower+ #x5b)
(defconstant +unicode-category-title+ #x5c)
(defconstant +unicode-category-other+ #x5d)

(defun unicode-upper (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((scase (unidata-scase *unicode-data*))
	 (n (logand (qref32 scase code) #xFF)))
    (if (zerop n)
	code
	(let* ((m (aref (scase-svec scase) (logand n #x7F))))
	  (if (logbitp 7 n) (+ code m) (- code m))))))

(defun unicode-lower (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((scase (unidata-scase *unicode-data*))
	 (n (logand (ash (qref32 scase code) -8) #xFF)))
    (if (zerop n)
	code
	(let ((m (aref (scase-svec scase) (logand n #x7F))))
	  (if (logbitp 7 n) (+ code m) (- code m))))))

(defun unicode-title (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-scase *unicode-data*) (load-scase))
  (let* ((scase (unidata-scase *unicode-data*))
	 (n (logand (ash (qref32 scase code) -16) #xFF)))
    (if (zerop n)
	code
	(let ((m (aref (scase-svec scase) (logand n #x7F))))
	  (if (logbitp 7 n) (+ code m) (- code m))))))

(defun unicode-num1 (code)
  (declare (type codepoint code))
  (unless (unidata-numeric *unicode-data*) (load-numerics))
  (let ((n (qref32 (unidata-numeric *unicode-data*) code)))
    (if (logbitp 25 n) (logand (ash n -3) #xF) nil)))

(defun unicode-num2 (code)
  (declare (type codepoint code))
  (unless (unidata-numeric *unicode-data*) (load-numerics))
  (let ((n (qref32 (unidata-numeric *unicode-data*) code)))
    (if (logbitp 24 n) (logand (ash n -3) #xF) nil)))

(defun unicode-num3 (code)
  (declare (type codepoint code))
  (unless (unidata-numeric *unicode-data*) (load-numerics))
  (let ((n (qref32 (unidata-numeric *unicode-data*) code)))
    (if (logbitp 23 n)
	(let ((num (/ (logand (ash n -3) #x1FFFF) (1+ (logand n 7)))))
	  (if (logbitp 20 n) (- num) num))
	nil)))

(defun unicode-decomp (code &optional (compatibility t))
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (if (hangul-syllable-p code)
      ;; Hangul syllables.  (See
      ;; http://www.unicode.org/reports/tr15/#Hangul for the
      ;; algorithm.)
      (multiple-value-bind (q1 r1)
	  (truncate (- code #xAC00) 588)
	(declare (type (integer 0 18) q1) (type (integer 0 587) r1))
	(multiple-value-bind (q2 r2)
	    (truncate r1 28)
	  (declare (type (integer 0 20) q2) (type (integer 0 27) r2))
	  (let ((decomp (make-string (if (zerop r2) 2 3))))
	    (setf (schar decomp 0) (code-char (+ #x1100 q1))
		  (schar decomp 1) (code-char (+ #x1161 q2)))
	    (unless (zerop r2)
	      (setf (schar decomp 2) (code-char (+ #x11A7 r2))))
	    decomp)))
      (progn
	(unless (unidata-decomp *unicode-data*) (load-decomp))
	(let* ((decomp (unidata-decomp *unicode-data*))
	       (n (qref32 decomp code))
	       (type (ldb (byte 5 27) n)))
	  (if (= n 0)
	      nil
	      (if (or compatibility (zerop type))
		  (let ((off (logand n #xFFFF))
			(len (ldb (byte 6 16) n)))
		    (values (subseq (decomp-tabl decomp) off (+ off len))
			    type))
		  nil))))))

(declaim (ftype (function (codepoint) (unsigned-byte 8))
		unicode-combining-class))
(defun unicode-combining-class (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-combining *unicode-data*) (load-combining))
  (the (unsigned-byte 8) (qref8 (unidata-combining *unicode-data*) code)))

(defun unicode-bidi-class (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-bidi *unicode-data*) (load-bidi))
  (logand (qref16 (unidata-bidi *unicode-data*) code) #x1F))

(defun unicode-bidi-class-string (code)
  (aref +bidi-class+ (unicode-bidi-class code)))

(defun unicode-bidi-mirror-p (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-bidi *unicode-data*) (load-bidi))
  (logbitp 5 (qref16 (unidata-bidi *unicode-data*) code)))

(defun unicode-mirror-codepoint (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-bidi *unicode-data*) (load-bidi))
  (let* ((d (unidata-bidi *unicode-data*))
	 (x (ash (qref16 d code) -6))
	 (i (logand x #x0F))
	 (n (if (logbitp 5 x) (aref (bidi-tabl d) i) i)))
    (cond ((= x 0) nil)
	  ((logbitp 4 x) (- code n))
	  (t (+ code n)))))

(declaim (inline unicode-nfc-qc unicode-nfkc-qc
		 unicode-nfd-qc unicode-nfkd-qc))

(defun unicode-nfc-qc (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-qc-nfc *unicode-data*) (load-normalization-qc))
  (ecase (qref2 (unidata-qc-nfc *unicode-data*) code)
    (0 :Y) (1 :M) (2 :N)))

(defun unicode-nfkc-qc (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-qc-nfkc *unicode-data*) (load-normalization-qc))
  (ecase (qref2 (unidata-qc-nfkc *unicode-data*) code)
    (0 :Y) (1 :M) (2 :N)))

(defun unicode-nfd-qc (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-qc-nfd *unicode-data*) (load-normalization-qc))
  (ecase (qref1 (unidata-qc-nfd *unicode-data*) code)
    (0 :Y) (1 :N)))

(defun unicode-nfkd-qc (code)
  (declare (optimize (speed 3) (space 0) (safety 0))
	   (type codepoint code))
  (unless (unidata-qc-nfkd *unicode-data*) (load-normalization-qc))
  (ecase (qref1 (unidata-qc-nfkd *unicode-data*) code)
    (0 :Y) (1 :N)))

(defun unicode-composition-exclusions ()
  (unless (unidata-comp-exclusions *unicode-data*)
    (load-composition-exclusions))
  (unidata-comp-exclusions *unicode-data*))

(defun %unicode-full-case (code data default)
  (let* ((n (qref32 data code)))
    (if (= n 0)
	(multiple-value-bind (hi lo)
	    (surrogates (funcall default code))
	  (let ((s (make-string (if lo 2 1))))
	    (setf (schar s 0) hi)
	    (when lo
	      (setf (schar s 1) lo))
	    s))
	(let ((off (logand n #xffff))
	      (len (ldb (byte 6 16) n)))
	  (subseq (full-case-tabl data) off (+ off len))))))

(defun unicode-full-case-lower (code)
  (unless (unidata-full-case-lower *unicode-data*)
    (load-full-case-lower))
  (%unicode-full-case code (unidata-full-case-lower *unicode-data*) #'unicode-lower))

(defun unicode-full-case-title (code)
  (unless (unidata-full-case-title *unicode-data*)
    (load-full-case-title))
  (%unicode-full-case code (unidata-full-case-title *unicode-data*) #'unicode-title))
  
(defun unicode-full-case-upper (code)
  (unless (unidata-full-case-upper *unicode-data*)
    (load-full-case-upper))
  (%unicode-full-case code (unidata-full-case-upper *unicode-data*) #'unicode-upper))

(defun unicode-case-fold-simple (code)
  (unless (unidata-case-fold-simple *unicode-data*)
    (load-case-fold-simple))
  (let* ((data (unidata-case-fold-simple *unicode-data*))
	 (n (qref32 data code)))
    (if (= n 0)
	code
	n)))

(defun unicode-case-fold-full (code)
  (unless (unidata-case-fold-full *unicode-data*)
    (load-case-fold-full))
  (let* ((data (unidata-case-fold-full *unicode-data*))
	 (n (qref32 data code)))
    (if (= n 0)
	(string (code-char (unicode-case-fold-simple code)))
	(let ((off (logand n #xffff))
	      (len (ldb (byte 6 16) n)))
	  (subseq (case-fold-full-tabl data) off (+ off len))))))
  

(declaim (inline composition-table-key))
(defun composition-table-key (c1 c2)
  (declare (type codepoint c1 c2))
  ;; Compute the key for the composition table from two code points.
  ;; Note that each codepoint is 21 bits long.  We just cat the
  ;; codepoints together to create the key.  Based on tests with
  ;; Unicode 5.2.0 this is good enough because the low 29 bits are
  ;; unique, so each key will be in its own bucket.
  (logior (ash c1 21) c2))

;; Build the composition pair table.
(defun build-composition-table ()
  (let ((table (make-hash-table)))
    (dotimes (cp #x10ffff)
      ;; Ignore Hangul characters, which can be done algorithmically.
      (unless (<= #xac00 cp #xd7a3)
	(let ((decomp (unicode-decomp cp nil)))
	  ;; Also ignore any characters whose canonical decomposition
	  ;; consists of a sequence of characters, the first of which
	  ;; has a non-zero combining class, or if the decomposition
	  ;; consists of a single codepoint.
	  (when (and decomp
		     (zerop (unicode-combining-class (codepoint decomp 0))))
	    (multiple-value-bind (c1 widep)
		(codepoint decomp 0)
	      (setf widep (if widep 2 1))
	      (when (> (length decomp) widep)
		(let ((c2 (codepoint decomp widep)))
		  (setf (gethash (composition-table-key c1 c2) table) cp))))))))
    ;; Remove any in the exclusion list
    (loop for cp across (unicode-composition-exclusions)
       do
       (let ((decomp (unicode-decomp cp nil)))
	 (when decomp
	   (multiple-value-bind (c1 widep)
	       (codepoint decomp 0)
	     (when (> (length decomp) (if widep 2 1))
	       (let ((c2 (codepoint decomp (if widep 2 1))))
		 (remhash (composition-table-key c1 c2) table)))))))
    (values table)))

(defvar *composition-pair-table* nil)

;; Based on the sample code from
;; http://www.unicode.org/reports/tr15/#Hangul
(declaim (inline compose-hangul))
(defun compose-hangul (c1 c2)
  (declare (type codepoint c1 c2)
	   (optimize (speed 3)))
  (let ((index-l (- c1 #x1100)))
    (cond ((and (<= 0 index-l)
		(< index-l 19))
	   (let ((index-v (- c2 #x1161)))
	     (when (and (<= 0 index-v)
			(< index-v 21))
	       (+ #xac00 (* 28 (+ (* index-l 21) index-v))))))
	  (t
	   (let ((index-s (- c1 #xac00)))
	     (when (and (<= 0 index-s)
			(< index-s 11172)
			(zerop (rem index-s 28)))
	       (let ((index-t (- c2 #x11a7)))
		 (when (and (plusp index-t)
			    (< index-t 28))
		   (+ c1 index-t)))))))))

(defun unicode-pairwise-composition (c1 c2)
  (declare (type codepoint c1 c2)
	   (optimize (speed 3)))
  (unless *composition-pair-table*
    (setf *composition-pair-table* (build-composition-table)))
  (cond ((compose-hangul c1 c2))
	(t
	 (gethash (composition-table-key c1 c2) *composition-pair-table* nil))))

(defun unicode-word-break-code (code)
  (unless (unidata-word-break *unicode-data*)
    (load-word-break))
  (let* ((data (unidata-word-break *unicode-data*))
	 (n (qref4 data code)))
    n))

(defun unicode-word-break (code)
  ;; The order of the array here MUST match the order used in
  ;; pack-word-break in tools/build-unidata.lisp!
  (aref #(:other :cr :lf :newline :extend :format
	  :katakana :aletter :midnumlet :midletter :midnum
	  :numeric :extendnumlet :regional_indicator)
	(unicode-word-break-code code)))

;; Support for character name completion for slime.
;;
;; Code written by Paul Foley, with some modifications by Raymond Toy.
;;

;; These hold dictionaries for the Hangul syllables and the CJK
;; unified ideographs.  Note that these could be stored in
;; unidata.bin, but that adds almost a megabyte to the size of
;; unidata.bin.  That seems way to much bloat for something that is
;; probably not used that much.  However, this incurs a runtime cost
;; the first time it needs to be accessed.  On a 450 MHz sparc, it
;; takes 55 sec for the cjk dictionary and 9 sec for the Hangul
;; dictionary.  A bit long but not too bad.  On a 2 GHz mac mini, it
;; takes 5 sec and .8 sec, respectively.  This seems reasonable,
;; especially since the intent is for character completion, which
;; doesn't have to be too fast.
(defvar *hangul-syllable-dictionary* nil
  "Dictionary of Hangul syllables")
(defvar *cjk-unified-ideograph-dictionary* nil
  "Dictionary of CJK Unified ideographs")

;; Convert the string into the form we want for character names.
;; Basically the Unicode name has spaces replaced by underscores, and
;; the result is capitalized.
(declaim (inline %str %strx))
(defun %str (x)
  (nsubstitute #\_ #\Space (string-capitalize x)))

(defun %strx (x)
  (%str (car x)))

(declaim (inline %match))
#+(or)
(defun %match (part prefix posn)
  (and (>= (length part) (- (length prefix) posn))
       (string= part prefix :start2 posn :end1 (- (length prefix) posn))))

#+(or)
(defun %match (part prefix posn)
  (let ((s1 (search part prefix :start2 posn))
	(s2 (search prefix part :start1 posn)))
    (or (and s1 (= s1 posn))
	(and s2 (zerop s2)))))

;; Test if the string PART matches the string PREFIX starting from
;; position POSN.  Basically test that the initial parts of the
;; strings match each other exactly.  For if the prefix is "BO", then
;; both "B" and "BOX" should match.  (This is needed to get the
;; completion of "cjk_radical_bo" to match "cjk_radical_box" as well
;; as "cjk_radical_bone" and others because at one point in the
;; algorithm the part is "B", which we do want to match "BO" so that
;; we can get the possible completions BONE" and "BOLT OF CLOTH".
(defun %match (part prefix posn)
  (let ((len (min (length part)
		  (- (length prefix) posn))))
    (string= part prefix :end1 len :start2 posn :end2 (+ posn len))))


(defun unicode-complete-name (prefix
			      &optional (dict (unidata-name+
					       *unicode-data*)))
  "Try to complete the string Prefix using the dictionary in Dict.
  Three values are returned: (1) The best match of prefix, (2) a list
  of possible completions, (3) a boolean indicating whether the best
  match is a complete unicode name. "

  (unless dict
    ;; Load the names dictionary, if needed.
    (unless (unidata-name+ *unicode-data*)
      (load-names))
    (setf dict (unidata-name+ *unicode-data*)))
  (let ((prefix (nsubstitute #\Space #\_ (string-upcase prefix)))
	completep)
    (multiple-value-bind (n p)
	(search-dictionary prefix dict)
      (when n
	(setq completep (> (aref (dictionary-codev dict) n) -1)))
      #+(or debug-uc)
      (progn
	(format t "n,p,complete = ~S ~S ~S~%" n p completep)
	(when n (format t "match = ~S~%" (subseq prefix 0 p))))
      (cond ((not p)
	     (values (%str prefix) nil nil))
	    ((= p (length prefix))
	     ;; The prefix is an exact match to something in the code
	     ;; book.  Try to find possible completions of this
	     ;; prefix.
	     (let ((x (node-next n dict))
		   (suffix ""))
	       #+(or debug-uc)
	       (format t "init x = ~S~%" x)
	       (when (= (length x) 1)
		 ;; There was only one possible extension.  Try to
		 ;; extend from there.
		 #+(or debug-uc)
		 (format t "extending~%")
		 (setq suffix (caar x)
		       n (cdar x)
		       x (node-next (cdar x) dict)))
	       #+(or debug-uc)
	       (progn
		 (format t "x = ~S~%" x)
		 (format t "suffix = ~S~%" suffix))
	       (when (<= (length x) 1)
		 (setq prefix (concatenate 'string prefix suffix))
		 (setf suffix ""))
	       (values (%str prefix)
		       (sort (mapcar #'(lambda (e)
					 (%str (concatenate 'string suffix (car e))))
				     x)
			     #'string<)
		       (or (> (aref (dictionary-codev dict) n) -1)
			   completep))))
	    (t
	     ;; The prefix was not an exact match of some entry in the
	     ;; codebook. Try to find some completions from there.
	     (let* ((nodex (node-next n dict))
		    (x (remove-if-not (lambda (x)
					(%match (car x) prefix p))
				      nodex)))
	       #+(or debug-uc)
	       (progn
		 (format t "nodex = ~S~%" nodex)
		 (format t "x = ~S~%" x))
	       (setq prefix (subseq prefix 0 p))
	       (cond ((= (length x) 1)
		      ;; Only one possible completion.  Try to extend
		      ;; the completions from there.
		      (setq prefix (concatenate 'string prefix (caar x))
			    n (cdar x)
			    x (node-next (cdar x) dict))
		      (values (%str prefix)
			      (sort (mapcar #'%strx x) #'string<)
			      (> (aref (dictionary-codev dict) n) -1)))
		     (t
		      ;; There's more than one possible completion.
		      ;; Try to extend each of those completions one
		      ;; more step, but we still want to keep the
		      ;; original completions.
		      (let* ((p (append (mapcar #'car x)
					(mapcan #'(lambda (ex)
						    (let ((next (node-next (cdr ex) dict)))
						      (if next
							  (mapcar #'(lambda (n)
								      (concatenate 'string (car ex) (car n)))
								  (node-next (cdr ex) dict))
							  (list (car ex)))))
						x)))
			     (q (%mip p)))
			(setq prefix (concatenate 'string prefix q))
			
			(do ((tmp p (cdr tmp)))
			    ((endp tmp))
			  (setf (car tmp) (subseq (car tmp) (length q))))
			(values (%str prefix)
				(sort (mapcar #'%str p) #'string<)
				nil))))))))))

;; Like unicode-complete-name, but we also try to handle the names
;; that can be computed algorithmically like the Hangul syllables and
;; the CJK Unified Ideographs.
(defun unicode-complete (prefix
			 &optional (dict (unidata-name+ *unicode-data*)))
  "Search the dictionary in Dict and return a list of the possible
  completions starting with Prefix.  If there is no match, NIL is
  returned."
  (let (names)
    (multiple-value-bind (prefix-match next completep)
	(unicode-complete-name prefix dict)
      (loop for x in next
	 do (push (concatenate 'string prefix-match x) names))
      (when completep
	(push prefix-match names))
      (flet ((han-or-cjk-completion (prefix-match prefix dictionary)
	       (let* ((prefix-tail (subseq prefix-match
					   (min (length prefix)
						(length prefix-match))))
		      (full-prefix (concatenate 'string prefix prefix-tail)))
		 (multiple-value-bind (m suffixes)
		     (unicode-complete-name prefix-tail dictionary)
		   (declare (ignore m))
		   (if suffixes
		       (loop for n in suffixes
			  do (push (concatenate 'string full-prefix n) names))
		       ;; No suffixes.  So either the prefix is the
		       ;; only possible completion or it's not valid.
		       ;; Figure that out.  If it's valid, add it to
		       ;; names.
		       (when (search-dictionary (string-upcase prefix-tail) dictionary)
			 (push prefix-match names)))))))
	;; Match prefix for Hangul syllables or CJK unified ideographs.
	(cond ((char= (char prefix-match 0) #\H)
	       ;; Add "Hangul_Syllable_" as possible completion for
	       ;; anything beginning with "H".
	       (push "Hangul_Syllable_" names)
	       (when (<= (length names) 1)
		 ;; Hangul_Syllable is the only match, so let's extend it.
		 (unless *hangul-syllable-dictionary*
		   (initialize-reverse-hangul-tables)
		   (build-hangul-syllable-dictionary))
		 (han-or-cjk-completion prefix-match "Hangul_Syllable_"
					*hangul-syllable-dictionary*)))
	      ((char= (char prefix-match 0) #\C)
	       ;; Add "Cjk_Unified_Ideograph-" as possible completion
	       ;; for anything beginning with "C".
	       (push "Cjk_Unified_Ideograph-" names)
	       (when (<= (length names) 1)
		 (unless *cjk-unified-ideograph-dictionary*
		   (build-cjk-unified-ideograph-dictionary))
		 (han-or-cjk-completion prefix-match "Cjk_Unified_Ideograph-"
					*cjk-unified-ideograph-dictionary*)
		 ))))
      (setf names (mapcar #'string-capitalize names))
      ;;(format t "Final names = ~S~%" names)
      names)))

;; Find the longest initial substring of the STRINGS.
(defun %mip (strings)
  (let* ((first (first strings))
	 (posn (length first)))
    (dolist (string (rest strings))
      (setq posn (or (mismatch first string :end1 posn) posn)))
    (subseq first 0 posn)))

(defun node-next (i &optional (dict (unidata-name+ *unicode-data*)))
  (let* ((j (aref (dictionary-nextv dict) i))
	 (x (ldb (byte 14 18) j))
	 (y (ldb (byte 18 0) j)))
    (loop for i from 0 below (aref (dictionary-keyl dict) x)
       collect (close-node (cons (aref (dictionary-cdbk dict)
				       (aref (dictionary-keyv dict) (+ x i)))
				 (+ y i))
			   dict))))

(defun close-node (i &optional (dict (unidata-name+ *unicode-data*)))
  (loop
     (if (> (aref (dictionary-codev dict) (cdr i)) -1)
	 (return i)
	 (let* ((j (aref (dictionary-nextv dict) (cdr i)))
		(x (ldb (byte 14 18) j))
		(y (ldb (byte 18 0) j)))
	   (if (> (aref (dictionary-keyl dict) x) 1)
	       (return i)
	       (let ((k (aref (dictionary-cdbk dict)
			      (aref (dictionary-keyv dict) x))))
		 (setf (car i) (concatenate 'string (car i) k)
		       (cdr i) y)))))))

(defun build-hangul-syllable-dictionary ()
  "Build the dictionary for Hangul syllables"
  (format t "~&Building Hangul Syllable dictionary.  Please wait...~%")
  (force-output)
  (initialize-reverse-hangul-tables)
  (let ((hangul-codebook
	 ;; For our codebook, combine all the choseong, jungseong, and
	 ;; jonseong syllables, but removing empty strings (there's at
	 ;; least one).  Then sort these according to length.  This
	 ;; ensures that if A is an initial substring of B, then B
	 ;; must come before A (or A will never be used).  (See
	 ;; tools/build-unidata.lisp, *codebook*.)
	 (sort (map 'vector #'car
		    (delete ""
			    (concatenate 'vector
					 *reverse-hangul-choseong*
					 *reverse-hangul-jungseong*
					 *reverse-hangul-jongseong*)
			    :test #'string= :key #'car))
	       #'> :key #'length))
	(names
	 (loop for codepoint from 0 below codepoint-limit
	    when (hangul-syllable-p codepoint)
	    collect (cons (subseq (format nil "~A"
					  (string-upcase (char-name (code-char codepoint))))
				  16)
			  codepoint))))
    
    (setf *hangul-syllable-dictionary*
	  (build-dictionary hangul-codebook names))
    (format t "~&Done.~%")
    (force-output)
    (values)))

(defun build-cjk-unified-ideograph-dictionary ()
  "Build the dictionary for CJK Unified Ideographs"
  (format t "~&Building CJK Unified Ideographs dictionary.  Please wait...~%")
  (force-output)
  (let ((codebook (coerce (loop for k from 0 to 15
			     collect (format nil "~X" k))
			  'vector))
	(names (loop for codepoint from 0 below codepoint-limit
		    when (cjk-ideograph-p codepoint)
		  collect (cons (format nil "~X" codepoint)
				codepoint))))
    (setf *cjk-unified-ideograph-dictionary*
	  (build-dictionary codebook names))
    (format t "~&Done.~%")
    (force-output)
    (values)))

;; The definitions of BUILD-DICTIONARY, NAME-LOOKUP, and ENCODE-NAME
;; were taken from build-unidata.lisp.
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
		  (return)))))))))

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

;; This is primarily intended for users who what to create a core
;; image that contains all of the unicode data.  By doing this, the
;; resulting image no longer needs unidata.bin anymore.  This is
;; useful for an executable image.
(defun load-all-unicode-data ()
  "Load all unicode data and set *UNIDATA-PATH* to NIL.
Normally, the unicode data is loaded as needed.  This loads all of the
data, which is useful for creating a core that no longer needs
unidata.bin."
  (dolist (loader (reverse *defloaders*))
    (funcall loader))
  t)

;; CHeck to see if all of the unicode data has been loaded.
(defun unicode-data-loaded-p ()
  ;; FIXME: Would be nice to be able to do this automatically from the
  ;; structure without having to list every slot here.
  (and (unidata-range *unicode-data*)
       (unidata-name+ *unicode-data*)
       (unidata-name *unicode-data*)
       (unidata-category *unicode-data*)
       (unidata-scase *unicode-data*)
       (unidata-numeric *unicode-data*)
       (unidata-decomp *unicode-data*)
       (unidata-combining *unicode-data*)
       (unidata-bidi *unicode-data*)
       (unidata-name1+ *unicode-data*)
       (unidata-name1 *unicode-data*)
       (unidata-qc-nfd *unicode-data*)
       (unidata-qc-nfkd *unicode-data*)
       (unidata-qc-nfc *unicode-data*)
       (unidata-qc-nfkc *unicode-data*)
       (unidata-comp-exclusions *unicode-data*)
       (unidata-full-case-lower *unicode-data*)
       (unidata-full-case-title *unicode-data*)
       (unidata-full-case-upper *unicode-data*)
       (unidata-case-fold-simple *unicode-data*)
       (unidata-case-fold-full *unicode-data*)
       (unidata-word-break *unicode-data*)
       t))
