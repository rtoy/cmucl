;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/fndb.lisp,v 1.12 1990/10/11 17:32:59 ram Exp $
;;;
;;;    This file defines all the standard functions to be known functions.
;;; Each function has type and side-effect information, and may also have IR1
;;; optimizers.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "C")

(in-package "LISP")
(import '(
	  %aset
	  %bitset
	  %charset
	  %primitive
	  %put
	  %rplaca
	  %rplacd
	  %sbitset
	  %scharset
	  %set-documentation
	  %set-fdefinition
	  %set-fill-pointer
	  %set-row-major-aref
	  %setelt
	  %setnth
	  %sp-set-definition
	  %sp-set-plist
	  %standard-char-p
	  %svset
	  %typep
	  array-header-p
	  base-char-p
	  double-float-p
	  long-float-p
	  short-float-p
	  single-float-p
	  string<*
	  string>*
	  string<=*
	  string>=*
	  string=*
	  string/=*
	  %sp-string-compare
	  )
	"C")

(in-package "KERNEL")

(export '(%caller-frame-and-pc))

(in-package 'c)


;;;; Information for known functions:

(defknown coerce (t type-specifier) t
	  (movable foldable)			  ; Is defined to signal errors. 
  #|:derive-type 'type-spec-arg2|#)

(defknown type-of (t) t (foldable flushable))


;;;; In the "Predicates" chapter:

(defknown typep (t type-specifier) boolean (foldable flushable))
(defknown subtypep (type-specifier type-specifier) (values boolean boolean)
	  (foldable flushable))

(defknown (null symbolp atom consp listp numberp integerp rationalp floatp
		complexp characterp stringp bit-vector-p vectorp
		simple-vector-p simple-string-p simple-bit-vector-p arrayp
		packagep functionp compiled-function-p not)
  (t) boolean (movable foldable flushable))


(defknown (eq eql) (t t) boolean (movable foldable flushable))
(defknown (equal equalp) (t t) boolean (foldable flushable))


;;;; In the "Control Structure" chapter:

;;; Not flushable, since required to signal an error if unbound.
(defknown (symbol-value symbol-function) (symbol) t ())

(defknown boundp (symbol) boolean (flushable))
(defknown fboundp ((or symbol cons)) boolean (flushable))
(defknown special-form-p (symbol) t (movable foldable flushable)) ; They never change...
(defknown set (symbol t) t (unsafe)
  #|:derive-type 'result-type-arg2|#)
(defknown fdefinition ((or symbol cons)) function)
(defknown %set-fdefinition ((or symbol cons) function) function)
(defknown makunbound (symbol) symbol)
(defknown fmakunbound ((or symbol cons)) (or symbol cons))
(defknown (get-setf-method get-setf-method-multiple-value)
  ((or list symbol) &optional lexical-environment)
  (values list list list form form)
  (flushable))
(defknown apply (callable t &rest t) *) ; ### Last arg must be List...
(defknown funcall (callable &rest t) *)

(defknown (mapcar maplist mapcan mapcon) (callable list &rest list) list
  (call))

(defknown (mapc mapl) (callable list &rest list) list (foldable call))

(defknown values (&rest t) * (movable foldable flushable unsafe))
(defknown values-list (list) * (movable foldable flushable))


;;;; In the "Macros" chapter:

(defknown macro-function (symbol) (or function null) (flushable))
(defknown (macroexpand macroexpand-1)
  (t &optional lexical-environment) (values form &optional boolean))


;;;; In the "Declarations" chapter:

(defknown proclaim (list) void)


;;;; In the "Symbols" chapter:

(defknown get (symbol t &optional t) t (flushable))
(defknown remprop (symbol t) t)
(defknown symbol-plist (symbol) list (flushable))
(defknown getf (list t &optional t) t (foldable flushable))
(defknown get-properties (list list) (values t t list) (foldable flushable))
(defknown symbol-name (symbol) simple-string (movable foldable flushable))
(defknown make-symbol (string) symbol (flushable))
(defknown copy-symbol (symbol &optional t) symbol (flushable))
(defknown gensym (&optional (or string unsigned-byte)) symbol ())
(defknown gentemp (&optional string package) symbol)
(defknown symbol-package (symbol) (or package null) (flushable))
(defknown keywordp (t) boolean (flushable))	  ; If someone uninterns it...


;;;; In the "Packages" chapter:

(deftype packagelike () '(or stringlike package))
(deftype symbols () '(or list symbol))
(defknown make-package (stringlike &key (use list) (nicknames list)
				   ;; ### Extensions...
				   (internal-symbols index) (external-symbols index))
	  package)
(defknown in-package (stringlike &key (nicknames list) (use list)) void)
(defknown find-package (stringlike) (or package null) (flushable))
(defknown package-name (package) simple-string (flushable))
(defknown package-nicknames (package) list (flushable))
(defknown rename-package (package stringlike &optional list) void)
(defknown package-use-list (package) list (flushable))
(defknown package-used-by-list (package) list (flushable))
(defknown package-shadowing-symbols (package) list (flushable))
(defknown list-all-packages () list (flushable))
(defknown intern (string &optional packagelike)
  (values symbol (member :internal :external :inherited))
  ())
(defknown find-symbol (string &optional packagelike)
	  (values symbol (member :internal :external :inherited nil))
	  (flushable))
(defknown (export import) (symbols &optional packagelike) truth)
(defknown unintern (symbol &optional packagelike) boolean)
(defknown unexport (symbols &optional packagelike) truth)
(defknown (shadowing-import shadow) (symbols &optional packagelike) truth)
(defknown (use-package unuse-package) ((or list packagelike) &optional packagelike) truth)
(defknown find-all-symbols (stringlike) list (flushable))


;;;; In the "Numbers" chapter:

(defknown zerop (number) boolean (movable foldable flushable))
(defknown (plusp minusp) (real) boolean (movable foldable flushable))
(defknown (oddp evenp) (integer) boolean (movable foldable flushable))
(defknown (= /=) (number &rest number) boolean (movable foldable flushable))
(defknown (< > <= >=) (real &rest real) boolean (movable foldable flushable))
(defknown (max min) (real &rest real) real (movable foldable flushable))
(defknown + (&rest number) number (movable foldable flushable))
(defknown - (number &rest number) number (movable foldable flushable))
(defknown * (&rest number) number (movable foldable flushable))
(defknown / (number &rest number) number (movable foldable flushable))
(defknown (1+ 1-) (number) number (movable foldable flushable))
(defknown conjugate (number) number (movable foldable flushable))
(defknown gcd (&rest integer) unsigned-byte (movable foldable flushable)
  #|:derive-type 'boolean-result-type|#)
(defknown lcm (&rest integer) unsigned-byte (movable foldable flushable))

(defknown exp (number) irrational (movable foldable flushable))
(defknown expt (number real) number (movable foldable flushable))
(defknown log (number &optional real) irrational (movable foldable flushable))
(defknown sqrt (number) irrational (movable foldable flushable))
(defknown isqrt (unsigned-byte) unsigned-byte (movable foldable flushable))
(defknown (abs phase signum) (number) number (movable foldable flushable))
(defknown cis (real) (complex float) (movable foldable flushable))
(defknown atan (number &optional real) irrational (movable foldable flushable))
(defknown (sin cos tan asin acos sinh cosh tanh asinh acosh atanh)
  (number) irrational (movable foldable flushable))
(defknown float (real &optional float) float (movable foldable flushable))
(defknown (rational rationalize) (real) rational (movable foldable flushable))
(defknown (numerator denominator) (rational) integer
  (movable foldable flushable))
(defknown (floor ceiling truncate round)
  (real &optional real) (values integer real) (movable foldable flushable))
(defknown (mod rem) (real real) real (movable foldable flushable))
(defknown (ffloor fceiling fround ftruncate)
  (real &optional real) (values float float) (movable foldable flushable))



(defknown decode-float (float) (values float float-exponent float)
  (movable foldable flushable))
(defknown scale-float (float float-exponent) float
  (movable foldable flushable))
(defknown float-radix (float) float-radix (movable foldable flushable))
(defknown float-sign (float &optional float) float
  (movable foldable flushable))
(defknown (float-digits float-precision) (float) float-digits
  (movable foldable flushable))
(defknown integer-decode-float (float)
	  (values integer float-exponent (member -1 1))
	  (movable foldable flushable))
(defknown complex (real &optional real) number (movable foldable flushable))
(defknown (realpart imagpart) (number) real (movable foldable flushable))

(defknown (logior logxor logand logeqv lognand lognor logandc1 logandc2 logorc1
		  logorc2)
  (&rest integer) t
  (movable foldable flushable))

(defknown boole (t t boole-code) integer (movable foldable flushable))
(defknown lognot (integer) t (movable foldable flushable))
(defknown logtest (t integer) boolean (movable foldable flushable))
(defknown logbitp (bit-index integer) boolean (movable foldable flushable))
(defknown ash (integer ash-index) integer (movable foldable flushable))
(defknown (logcount integer-length) (integer) bit-index
  (movable foldable flushable))
(defknown byte (bit-index bit-index) byte-specifier
  (movable foldable flushable))
(defknown (byte-size byte-position) (byte-specifier) bit-index
  (movable foldable flushable)) 
(defknown ldb (byte-specifier integer) integer (movable foldable flushable))
(defknown ldb-test (byte-specifier integer) boolean
  (movable foldable flushable))
(defknown mask-field (byte-specifier integer) integer
  (movable foldable flushable))
(defknown dpb (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown deposit-field (integer byte-specifier integer) integer
  (movable foldable flushable))
(defknown random (real &optional random-state) real ())
(defknown make-random-state (&optional (or (member nil t) random-state))
  random-state (flushable))
(defknown random-state-p (t) boolean (movable foldable flushable))

;;; In "Characters" chapter:
(defknown (standard-char-p graphic-char-p alpha-char-p
			   upper-case-p lower-case-p both-case-p alphanumericp)
  (character) boolean (movable foldable flushable))

(defknown digit-char-p (character &optional unsigned-byte)
  (or (integer 0 35) null) (movable foldable flushable))

(defknown (char= char/= char< char> char<= char>= char-equal char-not-equal
		 char-lessp char-greaterp char-not-greaterp char-not-lessp)
  (character &rest character) boolean (movable foldable flushable))

(defknown character (t) character (movable foldable flushable))
(defknown char-code (character) char-code (movable foldable flushable))
(defknown code-char (char-code) base-character (movable foldable flushable))
(defknown (char-upcase char-downcase) (character) character
  (movable foldable flushable))
(defknown digit-char (integer &optional integer char-bits)
  (or character null) (movable foldable flushable))
(defknown char-int (character) char-code (movable foldable flushable))
(defknown char-name (character) (or simple-string null)
  (movable foldable flushable))
(defknown name-char (stringable) (or character null)
  (movable foldable flushable))


;;;; In the "Sequences" chapter:

(defknown elt (sequence index) t (foldable flushable))

(defknown subseq (sequence index &optional sequence-end) consed-sequence
  (foldable flushable) #|:derive-type 'result-type-arg1|#)

(defknown copy-seq (sequence) consed-sequence (foldable flushable)
  #|:derive-type 'result-type-arg1|#)

(defknown length (sequence) index (foldable flushable))

(defknown reverse (sequence) consed-sequence (foldable flushable)
  #|:derive-type 'result-type-arg1|#)

(defknown nreverse (sequence) sequence ()
  #|:derive-type 'result-type-arg1|#)

(defknown make-sequence (type-specifier index &key (initial-element t)) consed-sequence
  (movable flushable unsafe) #|:derive-type 'type-spec-arg1|#)

(defknown concatenate (type-specifier &rest sequence) consed-sequence
  (foldable flushable) #|:derive-type 'type-spec-arg1|#)

(defknown map (type-specifier callable sequence &rest sequence) consed-sequence
  (flushable call)
;  :derive-type 'type-spec-arg1  Nope... (map nil ...) returns null, not nil.
  )

;;; Returns predicate result... 
(defknown some (callable sequence &rest sequence) t
  (foldable flushable call))

(defknown (every notany notevery) (callable sequence &rest sequence) boolean
  (foldable flushable call))

;;; Unsafe for :Initial-Value...
(defknown reduce (callable sequence &key (from-end t) (start index)
			   (end sequence-end) (initial-value t))
  t
  (foldable flushable call unsafe))

(defknown fill (sequence t &key (start index) (end sequence-end)) sequence
  (unsafe)
  #|:derive-type 'result-type-arg1|#)

(defknown replace (sequence sequence &key (start1 index) (end1 sequence-end)
			    (start2 index) (end2 sequence-end))
  consed-sequence ()
  #|:derive-type 'result-type-arg1|#)

(defknown remove
  (t sequence &key (from-end t) (test callable)
     (test-not callable) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  consed-sequence
  (flushable call)
  #|:derive-type 'result-type-arg2|#)

(defknown substitute
  (t t sequence &key (from-end t) (test callable)
     (test-not callable) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  consed-sequence
  (flushable call)
  #|:derive-type 'result-type-arg3|#)

(defknown (remove-if remove-if-not)
  (callable sequence &key (from-end t) (start index) (end sequence-end)
	    (count sequence-end) (key callable))
  consed-sequence
  (flushable call)
  #|:derive-type 'result-type-arg2|#)

(defknown (substitute-if substitute-if-not)
  (t callable sequence &key (from-end t) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  consed-sequence
  (flushable call)
  #|:derive-type 'result-type-arg3|#)

(defknown delete
  (t sequence &key (from-end t) (test callable)
     (test-not callable) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  sequence
  (flushable call)
  #|:derive-type 'result-type-arg2|#)

(defknown nsubstitute
  (t t sequence &key (from-end t) (test callable)
     (test-not callable) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  sequence
  (flushable call)
  #|:derive-type 'result-type-arg3|#)

(defknown (delete-if delete-if-not)
  (callable sequence &key (from-end t) (start index) (end sequence-end)
	    (count sequence-end) (key callable))
  sequence
  (flushable call)
  #|:derive-type 'result-type-arg2|#)

(defknown (nsubstitute-if nsubstitute-if-not)
  (t callable sequence &key (from-end t) (start index) (end sequence-end)
     (count sequence-end) (key callable))
  sequence
  (flushable call)
  #|:derive-type 'result-type-arg3|#)

(defknown remove-duplicates
  (sequence &key (test callable) (test-not callable) (start index) (from-end t)
	    (end sequence-end) (key callable))
  consed-sequence
  (flushable call)
  #|:derive-type 'result-type-arg1|#)

(defknown delete-duplicates
  (sequence &key (test callable) (test-not callable) (start index) (from-end t)
	    (end sequence-end) (key callable))
  sequence
  (flushable call)
  #|:derive-type 'result-type-arg1|#)

(defknown find (t sequence &key (test callable) (test-not callable)
		  (start index) (from-end t) (end sequence-end) (key callable))
  t
  (foldable flushable call))

(defknown (find-if find-if-not)
  (callable sequence &key (from-end t) (start index) (end sequence-end)
	    (key callable))
  t
  (foldable flushable call))

(defknown position (t sequence &key (test callable) (test-not callable)
		      (start index) (from-end t) (end sequence-end)
		      (key callable))
  (or index null)
  (foldable flushable call))

(defknown (position-if position-if-not)
  (callable sequence &key (from-end t) (start index) (end sequence-end)
	    (key callable))
  (or index null)
  (foldable flushable call))

(defknown count (t sequence &key (test callable) (test-not callable)
		      (start index) (from-end t) (end sequence-end)
		      (key callable))
  index
  (foldable flushable call))

(defknown (count-if count-if-not)
  (callable sequence &key (from-end t) (start index) (end sequence-end)
	    (key callable))
  index
  (foldable flushable call))

(defknown (mismatch search)
  (sequence sequence &key (from-end t) (test callable) (test-not callable)
	    (start1 index) (end1 sequence-end) (start2 index) (end2 sequence-end)
	    (key callable))
  (or index null)
  (foldable flushable call))

;;; Not flushable, since vector sort guaranteed in-place...
(defknown (stable-sort sort) (sequence callable &key (key callable)) sequence
  (call)
  #|:derive-type 'result-type-arg1|#)

(defknown merge (type-specifier sequence sequence callable
				&key (key callable))
  sequence
  (flushable call)
  #|:derive-type 'type-spec-arg1|#)


;;;; In the "Manipulating List Structure" chapter:

(defknown (car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
	       cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar
	       cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	       first second third fourth fifth sixth seventh eighth ninth tenth
	       rest)
  (list) t (foldable flushable))

(defknown cons (t t) cons (movable flushable unsafe))

(defknown tree-equal (t t &key (test callable) (test-not callable)) boolean
  (foldable flushable call))
(defknown endp (t) boolean (foldable flushable movable))
(defknown list-length (list) (or index null) (foldable flushable))
(defknown (nth nthcdr) (index list) t (foldable flushable))
(defknown last (list) list (foldable flushable))
(defknown list (&rest t) list (movable flushable unsafe))
(defknown list* (t &rest t) t (movable flushable unsafe))
(defknown make-list (index &key (initial-element t)) list
  (movable flushable unsafe))

;;;
;;; All but last must be list...
(defknown append (&rest t) t (flushable))

(defknown copy-list (list) list (flushable))
(defknown copy-alist (list) list (flushable))
(defknown copy-tree (t) t (flushable))
(defknown revappend (list t) t (flushable))
(defknown nconc (&rest list) list ())
(defknown nreconc (list list) list ())
(defknown butlast (list &optional index) list (flushable))
(defknown nbutlast (list &optional index) list ())
(defknown ldiff (list list) list (flushable))
(defknown (rplaca rplacd) (cons t) list (unsafe))

(defknown (nsubst subst) (t t t &key (key callable) (test callable)
			    (test-not callable))
  list (flushable unsafe call))

(defknown (subst-if subst-if-not nsubst-if nsubst-if-not)
	  (t t t &key (key callable))
  list (flushable unsafe call))

(defknown (sublis nsublis) (list t &key (key callable) (test callable)
				 (test-not callable))
  list (flushable unsafe call))

(defknown member (t list &key (key callable) (test callable)
		    (test-not callable))
  list (foldable flushable call))
(defknown (member-if member-if-not) (callable list &key (key callable))
  list (foldable flushable call))

(defknown tailp (list list) boolean (foldable flushable))

(defknown adjoin (t list &key (key callable) (test callable)
		    (test-not callable))
  list (foldable flushable unsafe call))

(defknown (union intersection set-difference set-exclusive-or)
	  (list list &key (key callable) (test callable) (test-not callable))
  list
  (foldable flushable call))

(defknown (nunion nintersection nset-difference nset-exclusive-or)
	  (list list &key (key callable) (test callable) (test-not callable))
  list
  (foldable flushable call))

(defknown subsetp 
	  (list list &key (key callable) (test callable) (test-not callable))
  boolean
  (foldable flushable call))

(defknown acons (t t t) list (movable flushable unsafe))
(defknown pairlis (t t &optional t) list (flushable unsafe))

(defknown (rassoc assoc)
	  (t list &key (key callable) (test callable) (test-not callable))
  list (foldable flushable call))
(defknown (assoc-if-not assoc-if rassoc-if rassoc-if-not)
	  (callable list) list (foldable flushable call))


;;;; In the "Hash Tables" chapter:

(defknown make-hash-table
  (&key (test callable) (size index) (rehash-size (or (integer (0)) (float (1.0))))
	(rehash-threshold (or (integer (0)) (float (0.0) (1.0)))))
  hash-table
  (flushable unsafe))
(defknown hash-table-p (t) boolean (movable foldable flushable))
(defknown gethash (t hash-table &optional t) (values t boolean)
  (foldable flushable unsafe))
(defknown remhash (t hash-table) boolean ())
(defknown maphash (callable hash-table) null (foldable flushable call))
(defknown clrhash (hash-table) hash-table ())
(defknown hash-table-count (hash-table) fixnum (foldable flushable))

(deftype non-negative-fixnum () `(integer 0 ,most-positive-fixnum))
(defknown sxhash (t) non-negative-fixnum (foldable flushable))


;;;; In the "Arrays" chapter:

(defknown make-array ((or index list) &key (element-type type-specifier)
		      (initial-element t) (initial-contents list) (adjustable t)
		      (fill-pointer t) (displaced-to (or array null))
		      (displaced-index-offset index))
  array (flushable unsafe))

(defknown vector (&rest t) simple-vector (flushable unsafe))

(defknown aref (array &rest index) t (foldable flushable))
(defknown row-major-aref (array index) t (foldable flushable))

(defknown array-element-type (array) type-specifier (foldable flushable))
(defknown array-rank (array) array-rank (foldable flushable))
(defknown array-dimension (array array-rank) index (foldable flushable))
(defknown array-dimensions (array) list (foldable flushable))
(defknown array-in-bounds-p (array &rest index) boolean (foldable flushable))
(defknown array-row-major-index (array &rest index) array-total-size
  (foldable flushable)) 
(defknown array-total-size (array) array-total-size (foldable flushable))
(defknown adjustable-array-p (array) boolean (movable foldable flushable))

(defknown svref (simple-vector index) t (foldable flushable))
(defknown bit ((array bit) &rest index) bit (foldable flushable))
(defknown sbit ((simple-array bit) &rest index) bit (foldable flushable))

(defknown (bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2
		   bit-orc1 bit-orc2)
  ((array bit) (array bit) &optional (or (array bit) (member t)))
  (array bit)
  (foldable)
  #|:derive-type 'result-type-arg1|#)

(defknown bit-not ((array bit) &optional (or (array bit) (member t)))
  (array bit)
  (foldable)
  #|:derive-type 'result-type-arg1|#)

(defknown array-has-fill-pointer-p (array) boolean (movable foldable flushable))
(defknown fill-pointer (vector) index (foldable flushable))
(defknown vector-push (t vector) (or index null) ())
(defknown vector-push-extend (t vector &optional index) index ())
(defknown vector-pop (vector) t ())

(defknown adjust-array
  (array (or index list) &key (element-type type-specifier)
	 (initial-element t) (initial-contents list) (adjustable t)
	 (fill-pointer t) (displaced-to (or array null))
	 (displaced-index-offset index))
  array (unsafe))
;  :derive-type 'result-type-arg1) Not even close...


;;;; In the "Strings" chapter:

(defknown char (string index) character (foldable flushable))
(defknown schar (simple-string index) character (foldable flushable))

(defknown (string= string-equal)
  (stringlike stringlike &key (start1 index) (end1 sequence-end)
	      (start2 index) (end2 sequence-end))
  boolean
  (foldable flushable))

(defknown (string< string> string<= string>= string/= string-lessp
		   string-greaterp string-not-lessp string-not-greaterp
		   string-not-equal)
  (stringlike stringlike &key (start1 index) (end1 sequence-end)
	      (start2 index) (end2 sequence-end))
  (or index null)
  (foldable flushable))

(defknown make-string (index &key (initial-element character))
  simple-string (flushable))

(defknown (string-trim string-left-trim string-right-trim)
  (sequence stringlike) simple-string (flushable))

(defknown (string-upcase string-downcase string-capitalize)
  (stringlike &key (start index) (end sequence-end))
  simple-string (flushable))

(defknown (nstring-upcase nstring-downcase nstring-capitalize)
  (string &key (start index) (end sequence-end))
  string ())

(defknown string ((or character string symbol)) string (flushable))


;;; Internal non-keyword versions of string predicates:

(defknown (string<* string>* string<=* string>=* string/=*)
  (stringlike stringlike index sequence-end index sequence-end)
  (or index null)
  (foldable flushable))

(defknown string=*
  (stringlike stringlike index sequence-end index sequence-end)
  boolean
  (foldable flushable))


;;;; In the "Eval" chapter:

(defknown eval (t) *)
(defknown constantp (t) boolean (foldable flushable))


;;;; In the "Streams" chapter:

(defknown make-synonym-stream (symbol) stream (flushable))
(defknown make-broadcast-stream (&rest stream) stream (flushable))
(defknown make-concatenated-stream (&rest stream) stream (flushable))
(defknown make-two-way-stream (stream stream) stream (flushable))
(defknown make-echo-stream (stream stream) stream (flushable))
(defknown make-string-input-stream (string &optional index index) stream (flushable unsafe))
(defknown make-string-output-stream () stream (flushable))
(defknown get-output-stream-string (stream) simple-string ())
(defknown streamp (t) boolean (movable foldable flushable))
(defknown stream-element-type (stream) type-specifier (movable foldable flushable))
(defknown (output-stream-p input-stream-p) (stream) boolean (movable foldable
								     flushable))
(defknown close (stream &key (abort t)) stream ())


;;;; In the "Input/Output" chapter:

;;; The I/O functions are currently given effects ANY under the theory that
;;; code motion over I/O operations is particularly confusing and not very
;;; important for efficency.

(defknown copy-readtable (&optional (or readtable null) readtable) readtable
  ())
(defknown readtablep (t) boolean (movable foldable flushable))

(defknown set-syntax-from-char
  (character character &optional (or readtable null) readtable) void
  ())

(defknown set-macro-character (character callable &optional t readtable) void
  (unsafe))
(defknown get-macro-character (character &optional readtable)
  (values callable boolean) (flushable))

(defknown make-dispatch-macro-character (character &optional t readtable)
  void ())
(defknown set-dispatch-macro-character
  (character character callable &optional readtable) void
  (unsafe))
(defknown get-dispatch-macro-character
  (character character &optional readtable) callable
  (flushable))

;;; May return any type due to eof-value...
(defknown (read read-preserving-whitespace read-char read-char-no-hang)
  (&optional streamlike t t t) t)

(defknown read-delimited-list (character &optional streamlike t) t)
(defknown read-line (&optional streamlike t t t) (values t boolean))
(defknown unread-char (character &optional streamlike) t)
(defknown peek-char (&optional (or character (member nil t)) streamlike t t t) t)
(defknown listen (&optional streamlike) boolean (flushable))

(defknown clear-input (&optional stream) null)

(defknown read-from-string
  (string &optional t t &key (start index) (end sequence-end)
	  (preserve-whitespace t))
  t)
(defknown parse-integer
  (string &key (start index) (end sequence-end) (radix (integer 2 36))
	  (junk-allowed t)) 
  (or integer null ()))

(defknown read-byte (stream &optional t t) t)

(defknown write
  (t &key (stream streamlike) (escape t) (radix t) (base (integer 2 36))
     (circle t) (pretty t) (level (or unsigned-byte null))
     (length (or unsigned-byte null)) (case t) (array t) (gensym t)) t
  (any)
  #|:derive-type 'result-type-arg1|#)

(defknown (prin1 print princ) (t &optional streamlike) t (any)
  #|:derive-type 'result-type-arg1|#)

;;; xxx-TO-STRING not foldable because they depend on the dynamic environment. 
(defknown write-to-string
  (t &key (stream streamlike) (escape t) (radix t) (base (integer 2 36))
     (circle t) (pretty t) (level (or unsigned-byte null))
     (length (or unsigned-byte null)) (case t) (array t) (gensym t))
  simple-string
  (foldable flushable))

(defknown (prin1-to-string princ-to-string) (t) simple-string (flushable))

(defknown write-char (character &optional streamlike) character)
(defknown (write-string write-line)
  (string &optional streamlike &key (start index) (end sequence-end))
  string)

(defknown (terpri finish-output force-output clear-output)
  (&optional streamlike) null)

(defknown fresh-line (&optional streamlike) boolean)

(defknown write-byte (integer stream) integer)

(defknown format ((or streamlike string) string &rest t) (or string null))

(defknown (y-or-n-p yes-or-no-p) (&optional string &rest t) boolean)


;;;; In the "File System Interface" chapter:

(defknown pathname (pathnamelike) pathname (foldable flushable))
(defknown truename (pathnamelike) pathname ())

(defknown parse-namestring
  (pathnamelike &optional (or string null) pathnamelike  &key (start index)
		(end sequence-end) (junk-allowed t))
  (values (or pathname null) index)
  ())

(defknown merge-pathnames (pathnamelike &optional pathnamelike) pathname
  (foldable flushable))

(defknown make-pathname
 (&key (defaults pathnamelike) (host pathname-host) (device pathname-device)
       (directory (or pathname-directory string)) (name pathname-name)
       (type pathname-type) (version pathname-version))
  pathname (foldable flushable))

(defknown pathnamep (t) boolean (movable foldable flushable))

(defknown pathname-host (pathnamelike) pathname-host (foldable flushable))
(defknown pathname-device (pathnamelike) pathname-device (foldable flushable))
(defknown pathname-directory (pathnamelike) pathname-directory
  (foldable flushable))
(defknown pathname-name (pathnamelike) pathname-name (foldable flushable))
(defknown pathname-type (pathnamelike) pathname-type (foldable flushable))
(defknown pathname-version (pathnamelike) pathname-version
  (foldable flushable))

(defknown (namestring file-namestring directory-namestring host-namestring)
  (pathnamelike) simple-string
  (foldable flushable))

(defknown enough-namestring (pathnamelike &optional pathnamelike)
  simple-string
  (foldable flushable))

(defknown user-homedir-pathname (&optional t) pathname (flushable))

(defknown open
  (pathnamelike &key (direction (member :input :output :io :probe))
		(element-type type-specifier)
		(if-exists (member :error :new-version :rename
				   :rename-and-delete :overwrite :append
				   :supersede nil))
		(if-does-not-exist (member :error :create nil)))
  (or stream null))

(defknown rename-file (pathnamelike filename) (values pathname pathname pathname))
(defknown delete-file (pathnamelike) t)
(defknown probe-file (pathnamelike) (or pathname null) (flushable))
(defknown file-write-date (pathnamelike) (or unsigned-byte null) (flushable))
(defknown file-author (pathnamelike) (or simple-string null) (flushable))

(defknown file-position (stream &optional
				(or unsigned-byte (member :start :end)))
  (or unsigned-byte (member t nil)))
(defknown file-length (stream) (or unsigned-byte null) (flushable))

(defknown load
  ((or filename stream)
   &key (verbose t) (print t) (if-does-not-exist (member :error :create nil)))
  t)

(defknown directory (pathnamelike &key) list (flushable))


;;;; In the "Errors" chapter:

(defknown error (t &rest t) nil) ; Never returns...
(defknown cerror (string t &rest t) null)
(defknown warn (t &rest t) null)
(defknown break (&optional t &rest t) null)


;;;; In the "Miscellaneous" Chapter.

;;; ### Compiler interface non-standard...
(defknown compile (symbol &optional (or list function null))
  (values (or function symbol) boolean boolean))
(defknown compile-file
  ((or filename list) &key (output-file filename) (error-file filename)
   (trace-file filename) (error-output t) (load t) (block-compile t))
  (values (or pathname null) boolean boolean))
(defknown disassemble (callable &optional stream) void)

(defknown documentation (symbol (member variable function structure type setf))
  (or string null)
  (flushable))

(defknown describe (t &optional stream) (values))
(defknown inspect (t) (values))

(defknown room (&optional (member t nil) t) void)
(defknown ed (&optional filename) t)
(defknown dribble (&optional filename &key (if-exists t)) t)

(defknown apropos (stringlike &optional packagelike t) (values))
(defknown apropos-list (stringlike &optional packagelike t) list (flushable))

(defknown get-decoded-time ()
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational 0 (24)))
  (flushable))

(defknown get-universal-time () unsigned-byte (flushable))

(defknown decode-universal-time (unsigned-byte &optional (integer 0 23))
  (values (integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
	  (integer 1 12) unsigned-byte (integer 0 6) boolean (rational 0 (24)))
  (flushable))

(defknown encode-universal-time
  ((integer 0 59) (integer 0 59) (integer 0 23) (integer 1 31)
   (integer 1 12) unsigned-byte &optional (rational 0 (24)))
  unsigned-byte
  (flushable))

(defknown (get-internal-run-time get-internal-real-time)
  () internal-time (flushable))

(defknown sleep ((or (rational 0) (float 0.0))) null)

(defknown (lisp-implementation-type
	   lisp-implementation-version machine-type machine-version
	   machine-instance software-type software-version short-site-name
	   long-site-name)
  () simple-string (flushable))

(defknown identity (t) t (movable foldable flushable unsafe)
  #|:derive-type 'result-type-arg1|#)


;;;; Magical compiler frobs:

(defknown %typep (t type-specifier) boolean)
(defknown %special-bind (t t) void)
(defknown %special-unbind (t) void)
(defknown %listify-rest-args (t t) list (flushable))
(defknown %more-arg-context (t t) (values t fixnum) (flushable))
(defknown %more-arg (t t) t)
(defknown %verify-argument-count (t t) nil)
(defknown %argument-count-error (t) nil)
(defknown %unknown-values () *)
(defknown %catch (t t) void)
(defknown %unwind-protect (t t) void)
(defknown (%catch-breakup %unwind-protect-breakup) () void)
(defknown %lexical-exit-breakup (t) void)
(defknown %continue-unwind (t t t) nil)
(defknown %throw (t &rest t) nil); This is MV-called.
(defknown %nlx-entry (t) *)
(defknown %%primitive (t t &rest t) *)
(defknown %pop-values (t) void)
(defknown %type-check-error (t t) nil)
(defknown %odd-keyword-arguments-error () nil)
(defknown %unknown-keyword-argument-error (t) nil)
(defknown (%ldb %mask-field) (bit-index bit-index integer) unsigned-byte
  (movable foldable flushable))
(defknown (%dpb %deposit-field) (integer bit-index bit-index integer) integer
  (movable foldable flushable))
(defknown %negate (number) number (movable foldable flushable))
(defknown %check-bound (array index fixnum) index (movable foldable flushable))
(defknown data-vector-ref (array index) t (foldable flushable))
(defknown data-vector-set (array index t) t (unsafe))
(defknown kernel:%caller-frame-and-pc () (values t t) (flushable))


;;; Structure slot accessors or setters are magically "known" to be these
;;; functions, although the var remains the Slot-Accessor describing the actual
;;; function called.
;;;
(defknown %slot-accessor (t) t (foldable flushable))
(defknown %slot-setter (t t) t (unsafe))


;;;; Setf inverses:

(defknown %aset (array &rest t) t (unsafe))
(defknown %set-row-major-aref (array index t) t (unsafe))
(defknown %rplaca (cons t) t (unsafe))
(defknown %rplacd (cons t) t (unsafe))
(defknown %put (symbol t t) t (unsafe))
(defknown %setelt (sequence index t) t (unsafe))
(defknown %svset (simple-vector index t) t (unsafe))
(defknown %bitset (bit-vector index bit) bit (unsafe))
(defknown %sbitset (simple-bit-vector index bit) bit (unsafe))
(defknown %charset (string index character) character (unsafe))
(defknown %scharset (simple-string index character) character (unsafe))
(defknown %sp-set-definition (symbol function) function (unsafe))
(defknown %sp-set-plist (symbol t) t (unsafe))
(defknown %set-documentation
	  (symbol (member variable function structure type setf)
		  (or string null))
  ())
(defknown %setnth (index list t) t (unsafe))
(defknown %set-fill-pointer (vector index) (unsafe))


;;;; Internal type predicates:
;;;
;;;    Simple typep uses that don't have any standard predicate are translated
;;; into non-standard unary predicates.

(defknown (fixnump bignump ratiop short-float-p single-float-p double-float-p
	   long-float-p base-char-p %standard-char-p structurep
	   array-header-p)
  (t) boolean (movable foldable flushable))


;;;; Miscellaneous "sub-primitives":

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))
