;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Storage purifier for Spice Lisp.
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
;;;    The function Purify, defined herein, puts as much of the Lisp system as
;;; possible into Read-Only and Static spaces so that subsequent garbage
;;; collections are quicker.  This is done by frobbing the free-pointers for
;;; spaces so that new objects are put in static or read-only space as
;;; appropiate, then doing a GC.
;;;
;;;    We also transport all of the dynamic symbols in Lisp code so we
;;; can do clever things that improve locality in the resulting Lisp. 
;;; Some constant conses and g-vectors are also transported in macrocode
;;; so that we can put them in read-only space.
;;;
(in-package 'lisp)

(defun purify (&key root-structures)
  (declare (special lisp-environment-list))
  (setq lisp-environment-list NIL)
  (write-string "[Doing purification: ")
  (force-output)
  (setq *already-maybe-gcing* t)
  ;;
  ;; Move symbols to static space, constants to read-only space.
  (localify root-structures)
  ;;
  ;; Move everything else to either static or read-only space, depending
  ;; on type.
  (let ((fixups (gc-grovel-stack)))
    (%primitive clear-registers)
    (%primitive purify)
    (gc-fixup-stack fixups))

  (setq *already-maybe-gcing* nil)
  (setq *need-to-collect-garbage* nil)
  (write-line "done]")
  nil)

;;;; Localify

(eval-when (compile eval)
;;; Peek, Poke  --  Internal
;;;
;;;    Read or write the cell at a location without doing any type-checking or
;;; anything silly like that.
;;; 
(defmacro peek (x)
  `(%primitive read-control-stack ,x))
(defmacro poke (x val)
  `(%primitive write-control-stack ,x ,val))

;;; Symbol-Bits  --  Internal
;;;
;;;    There is a whole 32 bits at the end of every symbol, which until
;;; now, was unused.  We will use the low 16 to annotate some stuff about
;;; how symbols are referenced.
;;;
(defmacro symbol-bits (sym)
  `(get ,sym 'purify-symbol-bits 0))

(defsetf symbol-bits (sym) (val)
  `(let ((space (%primitive get-allocation-space)))
     (%primitive set-allocation-space %dynamic-space)
     (prog1 (setf (get ,sym 'purify-symbol-bits) ,val)
	    (%primitive set-allocation-space space))))

(defconstant marked-bit		#b001)
(defconstant worthwhile-bit	#b010)
(defconstant referenced-bit	#b100)

;;; Do-Allocated-Symbols  --  Internal
;;;
;;;    Iterate over all the symbols allocated in some space.
;;;
(defmacro do-allocated-symbols ((symbol space) &body forms)
  `(let* ((old-alloc-space (%primitive get-allocation-space)))
     (%primitive set-allocation-space %dynamic-space)
     (let* ((index (+ (ash %symbol-type %alloc-ref-type-shift)
		      (ash ,space %alloc-ref-space-shift)))
	    (alloc-table (int-sap %fixnum-alloctable-address))
	    (end (+ (logior (%primitive 16bit-system-ref alloc-table (1+ index))
			    (ash (logand %type-space-mask
					 (%primitive 16bit-system-ref alloc-table index))
				 16))
		    (ash ,space %space-shift))))
       (declare (fixnum end))
       (do ((base (ash ,space %space-shift) (+ base %symbol-length)))
	   ((= base end))
	 (declare (fixnum base))
	 (let ((,symbol (%primitive make-immediate-type base %symbol-type)))
	   (%primitive set-allocation-space old-alloc-space)
	   ,@forms
	   (%primitive set-allocation-space %dynamic-space))))
     (%primitive set-allocation-space old-alloc-space)))

;;; Inlinep  --  Internal
;;;
;;;    Return true if symbol appears to be the name of a function likely
;;; to be coded inline.
;;;
(defmacro inlinep (sym)
  `(or (info function source-transform ,sym)
       (let ((info (info function info ,sym)))
	 (and info
	      (or (c::function-info-templates info)
		  (c::function-info-ir2-convert info))))))


;;; Next-Symbol, Next-Cons  --  Internal
;;;
;;;    Return the object allocated after the supplied one.
;;;
(defmacro next-symbol (sym)
  `(%primitive make-immediate-type (+ (%primitive make-fixnum ,sym) %symbol-length)
	       %symbol-type))
(defmacro next-cons (cons)
  `(%primitive make-immediate-type (+ (%primitive make-fixnum ,cons) %cons-length)
	       %list-type))

;;; Purep  --  Internal
;;;
;;;    True if Obj is either not dynamic or has already been transported.
;;;
(defmacro purep (obj)
  `(or (>= (%primitive get-space ,obj) %static-space)
       (let ((type (%primitive get-type ,obj)))
	 (declare (fixnum type))
	 (or (< type %first-pointer-type)
	     (> type %last-pointer-type)
	     (= (%primitive get-type (peek ,obj)) %gc-forward-type)))))

;;; Free-Pointer-Location  --  Internal
;;;
;;;    Return the SAP which points to the location of the free-pointer
;;; for the specifed type and space in the alloc table.
;;;
(defmacro free-pointer-location (type space)
  `(+ %fixnum-alloctable-address
      (%primitive lsh ,type (1+ %alloc-ref-type-shift))
      (%primitive lsh ,space (1+ %alloc-ref-space-shift))))

;;; Transport-Symbol  --  Internal
;;;
;;;    If Sym is impure, copy it into static space and put a GC forward in the
;;; old symbol.  Return True only if we actually did something.
;;; 
(defmacro transport-symbol (sym)
  `(unless (purep ,sym)
     (let ((new-sym (%primitive alloc-symbol (symbol-name ,sym))))
       (when (boundp ,sym)
	 (setf (symbol-value new-sym) (symbol-value ,sym)))
       (when (fboundp ,sym)
	 (setf (symbol-function new-sym) (symbol-function ,sym)))
       (setf (symbol-plist new-sym) (symbol-plist ,sym))
       (%primitive set-package new-sym (symbol-package ,sym))
       (poke ,sym (%primitive make-immediate-type new-sym %gc-forward-type))
       t)))

;;; Copy-G-Vector  --  Internal
;;;
;;;    Copy a G-Vector into the current allocation space, and forward
;;; the old object.  Return the new object.  If an EQ hashtable,
;;; change the subtype, otherwise preserve it.
;;;
(defmacro copy-g-vector (object)
  `(let* ((len (length ,object))
	  (new (%primitive alloc-g-vector len nil))
	  (st (%primitive get-vector-subtype ,object)))
     (dotimes (i len)
       (setf (svref new i) (svref ,object i)))
     (%primitive set-vector-subtype new
		 (case st
		   ((2 3) 4)
		   (t st)))
     (poke ,object (%primitive make-immediate-type new %gc-forward-type))
     new))


;;; Scavenge-Symbols  --  Internal
;;;
;;;    Scan through static symbol space doing a Transport-Function on
;;; the definition of every Fbound symbol between the free pointer
;;; and our clean pointer.  The free pointer can move during the process
;;; due to symbols being transported.
;;;
(defmacro scavenge-symbols ()
  `(do ((free-ptr (peek free-ptr-loc) (peek free-ptr-loc)))
       ((eq clean-ptr free-ptr))
     (when (fboundp clean-ptr)
       (transport-function (symbol-function clean-ptr)))
     (setq clean-ptr (next-symbol clean-ptr))))
); eval-when (compile eval)

;;; Mark-Function  --  Internal
;;;
;;;    Set the referenced bit in any symbol constants, and call
;;; Mark-If-Worthwhile on any which are not marked.
;;;
(defun mark-function (fun)
  (let ((len (%primitive header-length fun)))
    (do ((i %function-constants-offset (1+ i)))
	((= i len))
      (let ((el (%primitive header-ref fun i)))
	(when (symbolp el)
	  (let ((bits (symbol-bits el)))
	    (setf (symbol-bits el) (logior referenced-bit bits))
	    (when (zerop (logand marked-bit bits))
	      (mark-if-worthwhile el))))))))


;;; Mark-If-Worthwhile  --  Internal
;;;
;;;    Mark the symbol if it is not already marked.  If it is appears to
;;; be a symbol likely to be used at runtime, we set the worthwhile
;;; bit as well.
;;;
(defun mark-if-worthwhile (sym)
  (when (zerop (logand (symbol-bits sym) marked-bit))
    ;;
    ;; Mark it so we know we have been here...
    (setf (symbol-bits sym) (logior marked-bit (symbol-bits sym)))
    ;;
    ;; If fbound and not an open-coded function, walk the function.
    (when (and (fboundp sym) (not (inlinep sym)))
      (setf (symbol-bits sym)
	    (logior worthwhile-bit (symbol-bits sym)))
      (mark-function (symbol-function sym)))
    ;;
    ;; If bound and not a inline constant, or neither bound nor fbound, 
    ;; but has a plist, mark as worthwhile.
    (when (if (boundp sym)
	      (not (and (constantp sym)
			(let ((val (symbol-value sym)))
			  (or (characterp val) (numberp val) (eq sym val)))))
	      (and (not (fboundp sym))
		   (not (null (cddr (symbol-plist sym))))))
      (setf (symbol-bits sym)
	    (logior worthwhile-bit (symbol-bits sym))))))


;;; Transport-And-Scavenge  --  Internal
;;;
;;;    Transport a symbol and then scavenge to completion.
;;;
(defun transport-and-scavenge (symbol)
  (let* ((free-ptr-loc (free-pointer-location %symbol-type %static-space))
	 (clean-ptr (peek free-ptr-loc)))
    (transport-symbol symbol)
    (scavenge-symbols)))


;;; Transport-Function  --  Internal
;;;
;;;    Grovel the constants of a function object, transporting things
;;; that look useful.  If a symbol has the worthwhile bit set, we move it.  We
;;; transport conses and g-vectors here so that they can go into read-only
;;; space.  If a constant is a compiled function, we recurse on it.
;;;
(defun transport-function (fun)
  (unless (purep fun)
    (let ((def (ecase (%primitive get-vector-subtype fun)
		 (#.%function-entry-subtype
		  (transport-function-object fun)
		  (%primitive header-ref fun %function-entry-constants-slot))
		 (#.%function-closure-subtype
		  (let ((entry (%primitive header-ref fun
					   %function-name-slot)))
		    (transport-function-object entry)
		    (%primitive header-ref entry
				%function-entry-constants-slot)))
		 (#.%function-funcallable-instance-subtype
		  nil))))
      (when (and def (not (purep def)))
	(let ((length (%primitive header-length def)))
	  (transport-function-object def)
	  (do ((i %function-constants-constants-offset (1+ i)))
	      ((= i length))
	    (let ((const (%primitive header-ref def i)))
	      (typecase const
		(symbol
		 (unless (zerop (logand worthwhile-bit (symbol-bits const)))
		   (transport-symbol const)))
		(cons
		 (transport-cons const))
		(compiled-function
		 (transport-function const))
		(simple-vector
		 (transport-g-vector const))))))))))


;;; TRANSPORT-FUNCTION-OBJECT  --  Internal
;;;
;;;    Copy a function object into read-only space.  This only moves the
;;; function (entry or constants) object itself, and lets GC scavenge.
;;;
(defun transport-function-entry (fun)
  (%primitive set-allocation-space %read-only-space)
  (let* ((len (%primitive header-length fun))
	 (res (%primitive alloc-function len)))
    (%primitive set-vector-subtype res (%primitive get-vector-subtype fun))
    (dotimes (i len)
      (%primitive header-set res i (%primitive header-ref fun i)))
    (poke fun (%primitive make-immediate-type res %gc-forward-type)))
  (%primitive set-allocation-space %static-space))
 

;;; Transport-Cons  --  Internal
;;;
;;;    Transport a cons and any list structure attached to it into read-only
;;; space and scavenge to completion.
;;;
(defun transport-cons (cons)
  (unless (purep cons)
    (%primitive set-allocation-space %read-only-space)
    (let* ((free-ptr-loc (free-pointer-location %list-type %read-only-space))
	   (clean-ptr (peek free-ptr-loc)))
      (loop
	(loop
	  (let ((new (cons (car cons) (cdr cons))))
	    (poke cons (%primitive make-immediate-type new %gc-forward-type))
	    (setq cons (cdr cons))
	    (when (or (atom cons) (purep cons)) (return nil))))
	(let ((free-ptr (peek free-ptr-loc)))
	  (loop
	    (when (eq clean-ptr free-ptr)
	      (%primitive set-allocation-space %static-space)
	      (return-from transport-cons nil))
	    (setq cons (car clean-ptr))
	    (setq clean-ptr (next-cons clean-ptr))
	    (unless (or (atom cons) (purep cons)) (return nil))))))))

;;; Transport-G-Vector  --  Internal
;;;
;;;    Transport a G-Vector into static space.  We only bother with
;;; the top level, and leave the rest to GC.
;;;
(defun transport-g-vector (vec &optional read-only)
  (unless (purep vec)
    (when read-only
      (%primitive set-allocation-space %read-only-space))
    (copy-g-vector vec)
    (when read-only
      (%primitive set-allocation-space %static-space))))

;;; Transport-Root  --  Internal
;;;
;;;    Descend into lists, simple-vectors and compiled functions, transporting 
;;; any useful symbols we run into, and scavenging to completion after each.  We
;;; transport simple-vectors now so that we don't lose on circular or highly
;;; shared structures.
;;;
(defun transport-root (object)
  (unless (purep object)
    (typecase object
      (symbol
       (unless (zerop (logand worthwhile-bit (symbol-bits object)))
	 (transport-and-scavenge object)))
      (simple-vector
       (let ((new (copy-g-vector object)))
	 (dotimes (i (length new))
	   (transport-root (svref new i)))))
      (cons
       (transport-root (car object))
       (transport-root (cdr object)))
      (compiled-function
       (transport-function object)))))

;;; Localify  --  Internal
;;;
;;;    This function goes GC-Like stuff at lisp level to try to increase 
;;; the locality in a purified core image.  The basic idea is to do a
;;; breadth-first walk of the function objects, moving interesting symbols
;;; into static space.
;;;
(defun localify (root-structures)
  (%primitive set-allocation-space %static-space)
  ;;
  ;; Mark interesting symbols, and those referenced by their definitions.
  (do-allocated-symbols (sym %dynamic-space)
    (setf (symbol-bits sym) 0))
  (do-allocated-symbols (sym %dynamic-space)
    (mark-if-worthwhile sym))
  ;;
  ;; Move interesting symbols referenced by the root structures.
  (dolist (x root-structures)
    (transport-root x))
  ;;
  ;; Treat interesting unreferenced symbols as roots...
  (do-allocated-symbols (sym %dynamic-space)
    (unless (purep sym)
      (let ((bits (symbol-bits sym)))
	(when (and (zerop (logand referenced-bit bits))
		   (not (zerop (logand worthwhile-bit bits))))
	  (transport-and-scavenge sym)))))
  ;;
  ;; Treat referenced symbols as roots...
  (do-allocated-symbols (sym %dynamic-space)
    (unless (or (purep sym)
		(zerop (logand referenced-bit (symbol-bits sym))))
      (transport-and-scavenge sym)))
  ;;
  ;; Do anything else that wants to be done...
  (do-allocated-symbols (sym %dynamic-space)
    ;;
    ;; Move some types of variable value...
    (when (boundp sym)
      (let ((val (symbol-value sym)))
	(cond ((purep val))
	      ((eq (info variable kind sym) :constant)
	       (typecase val
		 (cons (transport-cons val))
		 (simple-vector (transport-g-vector val t)))))))
    ;;
    ;; Move any interned symbol that's left...
    (unless (or (purep sym) (not (symbol-package sym)))
      (transport-and-scavenge sym)))

  ;;
  ;; Reset the bits...
  (remprop nil 'purify-symbol-bits)

  (do-allocated-symbols (sym %static-space)
    (remprop sym 'purify-symbol-bits))

  (do-allocated-symbols (sym %dynamic-space)
    (remprop sym 'purify-symbol-bits))

  (%primitive set-allocation-space %dynamic-space))
); Compiler-Let

;;;; Save-Stand-Alone-Lisp
;;;
;;;    A stand-alone is a lisp that has had everything that doesn't pertain
;;; to a particular application GC'ed away.  This can result in a drastic
;;; size reduction, but tends make the Lisp unusable for anything else and
;;; hard to debug in.  We do this by blowing away all symbols not directly
;;; referenced and doing a GC.  We also blow away random debug info.


;;; Save-Stand-Alone-Lisp  --  Public
;;;
(defun save-stand-alone-lisp (file root-function)
  "Write into File a core file which contains only objects referenced
  by Root-Function or needed for the basic system.  Root-Function
  is called when the core file is resumed.  Root-Function should be
  a symbol rather than an actual function object."
  (let ((all-packages (list-all-packages)))
    (fresh-line)
    (write-string "[Nuking useless stuff")
    (force-output)
    ;;
    ;; Mark all external symbols so that we can find them later...
    (dolist (p all-packages)
      (do-external-symbols (s p)
	(setf (symbol-bits s) 1)))
    ;;
    ;; Nuke all hashtables in packages...
    (dolist (p all-packages)
      (make-package-hashtable 10 (package-internal-symbols p))
      (make-package-hashtable 10 (package-external-symbols p)))
    #|
    ;;
    ;; Nuke random garbage on all symbols...
    (do-allocated-symbols (s %dynamic-space)
      ;;
      ;; Nuke arglists on functions...
      (when (fboundp s)
	(let ((fun (symbol-function s)))
	  (cond ((compiled-function-p fun)
		 (%primitive header-set fun %function-arg-names-slot ()))
		((and (consp fun) (compiled-function-p (cdr fun)))
		 (%primitive header-set (cdr fun) %function-arg-names-slot
			     ()))))
    
      ;;
      ;; Nuke unnecessary properties...
      (when (symbol-plist s)
	(dolist (p garbage-properties)
	  (when (get s p)
	    (remprop s p))))))
    |#
      
    (write-string "]
[GC'ing it away")
    (force-output)
    ;;
    ;; GC it away....
    (gc nil)
    (write-string "]")
    ;;
    ;; Rebuild packages...
    (write-string "]
[Rebuilding packages")
    (force-output)
    (do-allocated-symbols (s %dynamic-space)
      (let ((p (symbol-package s)))
	(cond ((null p))
	      ((zerop (symbol-bits s))
	       (add-symbol (package-internal-symbols p) s))
	      (t
	       (add-symbol (package-external-symbols p) s)
	       (setf (symbol-bits s) 0)))
	(remprop s 'purify-symbol-bits)))
    (do-allocated-symbols (s %static-space)
      (let ((p (symbol-package s)))
	(cond ((null p))
	      ((zerop (symbol-bits s))
	       (add-symbol (package-internal-symbols p) s))
	      (t
	       (add-symbol (package-external-symbols p) s)
	       (setf (symbol-bits s) 0)))
	(remprop s 'purify-symbol-bits)))
    (write-line "]")
    (purify :root-structures (list root-function))
    (if (save file)
	(quit)
	(funcall root-function))))
