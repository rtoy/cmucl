;;; -*- Package: DISASSEM -*-
;;;
;;; **********************************************************************
;;; This code was written for the use of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/disassem.lisp,v 1.8 1992/04/21 03:17:11 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Machine independent disassembler for CMU Common Lisp
;;;
;;; Written by Miles Bader <miles@cogsci.ed.ac.uk>
;;;

(in-package 'disassem)

(export '(;; for defining the instruction set
	  set-disassem-params
	  gen-inst-format-decl-form gen-field-type-decl-form gen-inst-decl-form
	  specialize

	  ;; main user entry-points
	  disassemble
	  disassemble-memory
	  disassemble-function
	  disassemble-code-component

	  ;; some variables to set
	  *opcode-column-width*
	  *note-column*

	  ;; slightly lower level entry-points
	  create-dstate set-dstate-segment
	  get-function-segments get-code-segments label-segments
	  disassemble-segments disassemble-segment
	  set-address-printing-range
	  add-hook add-note-hook add-comment-hook

	  ;; segment type
	  segment seg-start seg-length seg-debug-function

	  ;; for printing instructions
	  print-field
	  print-inst-using
	  print-inst

	  ;; decoding a bit-pattern
	  sap-ref-dchunk
	  get-inst-space
	  find-inst

	  ;; getting at the dstate (usually from mach-dep code)
	  disassem-state dstate-curpos dstate-nextpos dstate-code
	  dstate-segment-start dstate-segment-length dstate-segment-sap
	  dstate-get-prop

	  ;; random types
	  params inst inst-format field-type

	  ;; 
	  arg-value
	  sign-extend

	  ;; making handy margin notes
	  note
	  note-code-constant
	  maybe-note-nil-indexed-symbol-slot-ref
	  maybe-note-nil-indexed-object
	  maybe-note-assembler-routine
	  maybe-note-single-storage-ref
	  maybe-note-associated-storage-ref
	  handle-break-args

	  ;; taking over and printing...
	  print-notes-and-newline
	  print-current-address
	  print-bytes print-words
	  prin1-short
	  prin1-quoted-short
	  ))

;;; This file implements a retargetable disassembler, that uses simple hooks
;;; in the compiler-backend's instruction definitions to learn about a
;;; machine's instruction set.
;;;
;;; Most of these hooks are actually called by the macros defined by
;;; assembler.lisp, and only add a few new keyword arguments.  These are
;;; described below.
;;;
;;; All of the symbols described here except for DEFINE-INSTRUCTION,
;;; DEFINE-FORMAT, and DEFINE-ARGUMENT-TYPE (which are from assembler.lisp)
;;; are interned in the DISASSEM package, which is not normally USEd by the
;;; instruction definition files, and so would have "DISASSEM:" prepended in
;;; normal use.
;;;
;;; The first thing that should be in the instruction definition file is a
;;; call to SET-DISASSEM-PARAMS, where the possible keyword arguments are:
;;;
;;;  :INSTRUCTION-ALIGNMENT	Minimum alignment of instructions, in bytes.
;;;  :ADDRESS-SIZE		Size of a machine address, in bits.
;;;  :OPCODE-COLUMN-WIDTH	Width of the column used for printing the opcode
;;;  				portion of the instruction, or NIL.
;;;  :STORAGE-CLASS-SETS	A list of mappings (SET-NAME SC-NAME*), which
;;;				defines which generic storage class [SET-NAME]
;;;				the compilers SCs map into.
;;;
;;; The assembler definition macros should be in the following order:
;;;  DEFINE-ARGUMENT-TYPE, DEFINE-FORMAT, DEFINE-INSTRUCTION.
;;;
;;;
;;; *** New keyword arguments to DEFINE-ARGUMENT-TYPE:
;;;
;;;  :DISASSEM-PRINTER	Used to print an instruction field of this type.  Can
;;;			be a string, which is used as a format-string, with
;;;			the field's value as the only argument; a vector,
;;;			which is indexed using the field's value, and printed
;;;			using PRINC; T, meaning the field's value is printed
;;;			verbatim, or a function, which is called with three
;;;			arguments: the value, the output stream, and a
;;;			structure of type DISASSEM-STATE (which can be used
;;;			in various ways; see below).  Typically you should
;;;			make sure that whatever you print or supply to be
;;;			printed should respect *PRINT-CASE* (so, e.g., if you
;;;			use an array of register names, they should probably
;;;			be symbols instead of strings).
;;;  :SIGN-EXTEND	Non-NIL if this field should be sign-extended when it's
;;;			extracted from the instruction.
;;;  :USE-LABEL		Indicates that this field should have an address
;;;			label associated with it (which is then indicated at
;;;			the point of definition, and printed in place of the
;;;			field).  The value can be either T, in which case the
;;;			value of the field is used directly, or a function
;;;			taking two arguments, the value and a DISASSEM-STATE
;;;			structure, which should return the proper value to
;;;			use.
;;;
;;; The DISASSEM-STATE structure contains several fields of interest, notably
;;; DSTATE-CURPOS, which is the address of the current instruction being
;;; disassembled, and DSTATE-NEXTPOS, which is the address of the following
;;; instruction, and is needed for calling several auxilarly functions;
;;; notably at this point:
;;;
;;;  MAYBE-NOTE-ASSOCIATED-STORAGE-REF	Called with the an offset, a general
;;;			storage-class set (as defined in SET-DISASSEM-
;;;			PARAMS), a associated name, and a DISASSEM-STATE
;;;			object.  This checks to see if there's a source
;;;			variable mapped to OFFSET in the storage-class at
;;;			this point in the function, and if so, makes a note
;;;			saying that it's associated with the name you gave.
;;;
;;;
;;; *** New keyword arguments to DEFINE-FIXUP-TYPE:  The same three as for
;;;  DEFINE-ARGUMENT-TYPE. 
;;;
;;;
;;; *** New keyword arguments to DEFINE-FORMAT:
;;;  DEFINE-FORMAT takes two kinds of keyword arguments, one kind in the
;;;  initial header, after the name, and one kind in each field description.
;;;  To the header arguments is added:
;;;
;;;   :DISASSEM-PRINTER	Describes how instructions with this format should be
;;;			printed unless otherwise specified.  It can be either
;;;			a PRINTER-DESCRIPTION-LIST (described below), or a
;;;			function called four arguments: the actual bits
;;;			making it up, an instruction object, a stream and a
;;;			disassem-state [functions are not normally used].
;;;   :DISASSEM-CONTROL	A function that is called after the instruction has
;;;			been entirely printed, to allow for other side
;;;			effects; this function is called both during actual
;;;			disassembly, and during the initial pass when labels
;;;			are being calculated, at which time the STREAM
;;;			argument is NIL (so you can test it and avoid doing
;;;			things that only should happen during output).  Four
;;;			arguments are supplied: a DCHUNK, which contains the
;;;			actual bits of the instruction, and is usually just
;;;			passed to other functions, an INST is a structure
;;;			describing the instruction, a STREAM being currently
;;;			output to (or NIL if no output is being done), and a
;;;			DISASSEM-STATE.  Typically you use this function to
;;;			do things like printing handy margin notes (functions
;;;			to do which are described below).
;;;
;;;  To the field descriptions, only :DEFAULT-TYPE is added, which is
;;;   analogous to :ARGUMENT in DEFINE-INSTRUCTION, and simply supplies a
;;;   default for when the type is not specified in the instruction.  This is
;;;   important because sometimes the disassembler wants to print a constant
;;;   field as if it were a normal argument, and it needs to know the type to
;;;   do it properly.
;;;
;;; *** PRINTER-DESCRIPTION-LISTs:
;;;  These are a handy way to describe how an instruction should be printed,
;;;   and are used as a value of :DISASSEM-PRINTER in both DEFINE-FORMAT and
;;;   DEFINE-INSTRUCTION.  They are essentially a list, where each field is
;;;   one of:
;;;
;;;    A string		Printed as if by PRINC.
;;;    Something quoted	Printed as if by PRINC (so 'FOO respects
;;;			*PRINT-CASE*, which "FOO" does not).
;;;    :NAME		The name of the instruction is printed as if by
;;;			PRINC.
;;;    :TAB		The output is tabbed to the "argument column".
;;;    An atom		Must be the name of a field in this instruction, which
;;;			is printed according to its type.
;;;    A list		The contents contents are printed as a PDL.
;;;    (:CHOOSE choice1 choice2 ...)
;;;			The choices are gone through in turn, and the first
;;;			one in which *every* field reference (atom) is valid,
;;;			is printed as a PDL (this includes a choice of NIL, if a
;;;			default of nothing is wanted).  **All :CHOOSE operators
;;;			are executed before anything else is done, and no other
;;;    			grammar is paid attention to while this is being
;;;     		done.**  This means you can use a :CHOOSE clause in
;;;     		places where it might seem ungrammatical, such as the
;;;     		field in a test-clause (see below).
;;;    (:UNLESS test ...) If test is *not* true, then the rest of the list is
;;;			printed as a PDL.  See below for a description of tests
;;;    (:COND (test ...) ...)
;;;			Analogous to lisp COND, the first clause in which the
;;;			test is true has its cdr printed as a PDL.
;;;
;;;  Tests in :UNLESS and :COND are of either of the form:
;;;
;;;   T			Always true.
;;;   ([field] :AND test ...)	True if all the sub-tests are true.
;;;   ([field] :OR test ...)	True if any the sub-tests are true.
;;;   ([field] :NOT test)	True if the subtest isn't true.
;;;   ([field] :CONSTANT [value])
;;;			True if field is a constant, (and when value is
;;;			supplied, equal to it).
;;;   ([field] :SAME-AS [other-field])
;;;			True if field is constrained to be the same as some
;;;			other field (which must be other-field if it is
;;;			supplied).
;;;
;;;   Any test which ONLY has one element, may leave the parentheses out.
;;;
;;;   Note that the field in each is optional; if it is left out, then the
;;;    field from the nearest ancestor in the whole test which supplied one
;;;    is used.  If the top-most ancestor didn't, then the first atom in the
;;;    PDL accompanying the test in the :UNLESS or :COND is used-- this can
;;;    be very handy; a very commom idiom is:
;;;	(:unless (:constant 0) ", " field)
;;;    which prints ", <field contents>" unless the field is
;;;    specified to be zero.  An example of inheriting otherwise is:
;;;	(:unless (field1 :or (:constant 0) :same-as) field2)
;;;    which prints field2 if field1 is either specified to be a constant 0,
;;;    or to be the same as some other field (*not* just field2, though).
;;;
;;;   Also note that when a test does something based on whether a field is
;;;    constant or the same as another one, this is *only* a *static* test--
;;;    i.e., it means that an instruction which is specified with that
;;;    constraint [(:constant something) or (:same-as something)] will be
;;;    printed that way, but *not* cases where no instruction was defined
;;;    with such a constraint, but where it just happened to be true.  So if
;;;    you use a printer with a test of :constant 0 against a particular
;;;    field but never define an instruction flavor with :constant 0 as the
;;;    field description, then this test will never be true, even if
;;;    instructions get disassembled where that particular field *is* zero.
;;;
;;;  Two examples:
;;;
;;;   (:name :tab
;;;	     (:unless (:same-as dest-reg) source-reg-1 ", ")
;;;	     (:choose source-reg-2 immed) ", "
;;;	     dest-reg))
;;;
;;;   (:name :tab
;;;	     (:choose (source-reg-1 (:unless (:constant 0) "+" immed))
;;;		      (:cond ((source-reg-2 :constant 0) source-reg-1)
;;;		    	     ((source-reg-1 :constant 0) source-reg-2)
;;;		      	     (t source-reg-1 "+" source-reg-2)))
;;;	     (:unless (:constant 0) ", " dest-reg)))
;;;
;;;
;;; *** New keyword arguments to DEFINE-INSTRUCTION:
;;;  DEFINE-INSTRUCTION takes two kinds of keyword arguments, one kind in the
;;;  initial header, after the name, and one kind in each field description.
;;;  To the header arguments is added the same two as DEFINE-FORMAT, which,
;;;  if specified, override the default ones in this instruction's format.
;;;
;;;  To the individual field instructions are added:
;;;
;;;   :TYPE		Can be used to specify the type of this field when
;;;			it's not done by :ARGUMENT (e.g., when it's a
;;;			:CONSTANT).
;;;   :MASK		Takes an integer constant.  For use with an :ARGUMENT
;;;			field, specifies that only part of the field is
;;;			actually variable-- those bits that are 1 in the
;;;			mask.  The rest of the bits in the field are assumed
;;;			to be constant 0.  This is needed in certain cases to
;;;			make sure the instructions are unambiguous.
;;;
;;;
;;; *** Handy functions that can be called from :DISASSEM-PRINTER or
;;; :DISASSEM-CONTROL functions:
;;;
;;;  ARG-VALUE field-name dchunk inst
;;;			Returns the value of the given field in the
;;;			instruction.
;;;  NOTE note dstate	Records a note to be printed in the end-of-line
;;;			comments.  The note can be either a string, which is
;;;			printed by PRINC, or a function taking a stream
;;;			argument.
;;;  MAYBE-NOTE-NIL-INDEXED-SYMBOL-SLOT-REF integer-offset dstate
;;;			If the offset added to NIL points to a valid slot in
;;;			a symbol, a note describing this is added to the
;;;			end-of-line comments.
;;;  MAYBE-NOTE-CODE-CONSTANT integer-offset dstate
;;;			If the offset from the function's code-object refers
;;;			to a valid constant-slot, a note describing this is
;;;			added to the end-of-line comments.
;;;  MAYBE-NOTE-SINGLE-STORAGE-REF integer-offset gen-storage-class-name dstate
;;;			If there's a currently valid source-variable mapped
;;;			at this offset in the storage class specified, a note
;;;			to this effect is made for the end-of-line comments.
;;;
;;;
;;; *** How to SPECIALIZE an instruction:
;;;  Often it's desirable to have a different :DISASSEM-PRINTER only in
;;;  certain cases, e.g., when a certain register field refers to a certain
;;;  register.  One way you can do this is to define the instruction using a
;;;  normal printer, and use the SPECIALIZE macro to define modified versions
;;;  with different printer or control functions.  The syntax is like:
;;;
;;;   SPECIALIZE (inst-name [:DISASSEM-PRINTER printer]
;;;			    [:DISASSEM-CONTROL control]
;;;			    [:NAME new-name])
;;;     	 field-constraint*
;;;
;;;  The keyword args are just like for DEFINE-FORMAT and
;;;  DEFINE-INSTRUCTION.  The field-constraints are either just field names,
;;;  which mean that the specialize operation should only affect flavors of
;;;  the instruction that contain that field, or a field-name and a
;;;  constraint, like:
;;;
;;;   (field-name :CONSTANT const-val)
;;;  or
;;;   (field-name :SAME-AS other-field-name)
;;;
;;;  Any instruction flavor that has all the specified fields will either
;;;  have its printer, control, or name arguments modified, if all its
;;;  constraints match those in the specialize, or will have a clone made
;;;  with the field constraints tightened to match, and the clone will have
;;;  the new printer, control, or name arguments as specified.  Also see the
;;;  documentation for the SPECIALIZE macro.
;;;
;;;
;;; *** Debugging hints:
;;;  Because the disassembler caches the search tree that it builds to find
;;;  instructions, if you use the disassemble command and subsequently
;;;  re-execute some of the instruction-definitions with changes, it won't
;;;  see the changes until you flush the cache, like:
;;;  
;;;   (setf (disassem::params-inst-space
;;;	     (c:backend-disassem-params c:*target-backend*))
;;;    	    nil)
;;;  

;;; ----------------------------------------------------------------

(defun req () (error "Required argument missing"))

;;; ----------------------------------------------------------------

(defvar *opcode-column-width* nil
  "The width of the column in which instruction-names are printed.
  NIL means use the default.  A value of zero gives the effect of not
  aligning the arguments at all.")
(defvar *note-column* 45
  "The column in which end-of-line comments for notes are started.")

(defconstant default-opcode-column-width 6)
(defconstant default-address-column-width 8)
(defconstant label-column-width 7)

;;; ----------------------------------------------------------------

(defstruct (params (:print-function %print-params))
  (field-types (make-hash-table :test #'eq) :type hash-table)
  (inst-formats (make-hash-table :test #'eq) :type hash-table)
  (instructions (make-hash-table :test #'eq) :type hash-table)
  (inst-space nil :type (or null inst-space))
  (instruction-alignment vm:word-bytes :type fixnum)
  (address-column-width default-address-column-width :type fixnum)
  (opcode-column-width default-opcode-column-width :type (or null fixnum))
  (storage-class-sets nil :type list)
  (backend (req) :type c::backend)	; for convenience
  )

(defun %print-params (params stream level)
  (declare (ignore level))
  (format stream "#<Disassem parameters for ~a>"
	  (and (params-backend params)
	       (c:backend-name (params-backend params)))))

(defmacro set-disassem-params (&key instruction-alignment
				    address-size
				    (opcode-column-width nil opcode-column-width-p)
				    storage-class-sets)
  "Specify global disassembler params for C:*TARGET-BACKEND*.  Currently
  includes:

  :INSTRUCTION-ALIGNMENT	Minimum alignment of instructions, in bits.
  :ADDRESS-SIZE			Size of a machine address, in bits.
  :OPCODE-COLUMN-WIDTH		Width of the column used for printing the opcode
  				portion of the instruction, or NIL.
  :STORAGE-CLASS-SETS		A list of mappings (SET-NAME SC-NAME*), which
				defines which generic storage class the
  				compilers SCs map into."
  `(let ((params
	  (or (c:backend-disassem-params c:*target-backend*)
	      (setf (c:backend-disassem-params c:*target-backend*)
		    (make-params :backend c:*target-backend*)))))
     ,(when instruction-alignment
	`(multiple-value-bind (bytes bits)
	     (truncate ,instruction-alignment vm:byte-bits)
	   (unless (zerop bits)
	     (error "Instruction alignment not a multiple of ~s" vm:byte-bits))
	   (setf (params-instruction-alignment params) bytes)))
     ,(when address-size
	`(setf (params-address-column-width params)
	       (* 2 ,address-size)))
     ,(when opcode-column-width-p
	`(setf (params-opcode-column-width params) ,opcode-column-width))
     ,(when storage-class-sets
	`(setf (params-storage-class-sets params) ,storage-class-sets))
     (values)))

;;; ----------------------------------------------------------------
;;; A Dchunk contains the bits we look at to decode an
;;; instruction.
;;; I tried to keep this abstract so that if using integers > the machine
;;; word size conses too much, it can be changed to use bit-vectors or
;;; something.

(defconstant dchunk-bits 32)

(deftype dchunk ()
  `(unsigned-byte ,dchunk-bits))

(defconstant dchunk-zero 0)
(defconstant dchunk-one #xFFFFFFFF)

(defmacro dchunk-copy (x)
  x)

(defun dchunk-or (to from)
  (declare (type dchunk to from))
  (the dchunk (logior to from)))
(defun dchunk-and (to from)
  (declare (type dchunk to from))
  (the dchunk (logand to from)))
(defun dchunk-clear (to from)
  (declare (type dchunk to from))
  (the dchunk (logandc2 to from)))
(defun dchunk-not (from)
  (declare (type dchunk from))
  (the dchunk (lognot from)))

(defmacro dchunk-andf (to from)
  `(setf ,to (dchunk-and ,to ,from)))
(defmacro dchunk-orf (to from)
  `(setf ,to (dchunk-or ,to ,from)))
(defmacro dchunk-clearf (to from)
  `(setf ,to (dchunk-clear ,to ,from)))

(defun dchunk-make-mask (pos)
  (the dchunk (mask-field pos -1)))
(defun dchunk-make-field (pos value)
  (the dchunk (dpb value pos 0)))

(defmacro make-dchunk (value)
  value)

(defun sap-ref-dchunk (sap byte-offset byte-order)
  (declare (type system:system-area-pointer sap)
	   (type fixnum byte-offset))
  (if (eq byte-order :big-endian)
      (+ (ash (system:sap-ref-8 sap byte-offset) 24)
	 (ash (system:sap-ref-8 sap (+ 1 byte-offset)) 16)
	 (ash (system:sap-ref-8 sap (+ 2 byte-offset)) 8)
	 (system:sap-ref-8 sap (+ 3 byte-offset)))
      (+ (system:sap-ref-8 sap byte-offset)
	 (ash (system:sap-ref-8 sap (+ 1 byte-offset)) 8)
	 (ash (system:sap-ref-8 sap (+ 2 byte-offset)) 16)
	 (ash (system:sap-ref-8 sap (+ 3 byte-offset)) 24))))

(defun correct-dchunk-bytespec-for-endianness (bs unit-bits byte-order)
  (if (eq byte-order :big-endian)
      (byte (byte-size bs) (+ (byte-position bs) (- dchunk-bits unit-bits)))
      bs))

(defun dchunk-extract (from pos)
  (declare (type dchunk from))
  (ldb pos (the dchunk from)))

(defun dchunk-corrected-extract (from pos unit-bits byte-order)
  (declare (type dchunk from))
  (if (eq byte-order :big-endian)
      (ldb (byte (byte-size pos) (+ (byte-position pos) (- dchunk-bits unit-bits)))
	   (the dchunk from))
      (ldb pos (the dchunk from))))

(defmacro dchunk-insertf (place pos value)
  `(setf ,place (the dchunk (dpb ,value ,pos (the dchunk,place)))))

(defun dchunk= (x y)
  (declare (type dchunk x y))
  (= x y))
(defmacro dchunk-zerop (x)
  `(dchunk= ,x dchunk-zero))

(defun dchunk-strict-superset-p (sup sub)
  (and (zerop (logandc2 sub sup))
       (not (zerop (logandc2 sup sub)))))

(defun dchunk-count-bits (x)
  (declare (type dchunk x))
  (logcount x))

;;; ----------------------------------------------------------------

(defun sign-extend (int size)
  (declare (type integer int)
	   (fixnum size))
  (if (logbitp (1- size) int)
      (dpb int (byte size 0) -1)
      int))

(defun aligned-p (num size)
  "Returns non-NIL if NUM is aligned on a SIZE byte boundary."
  (declare (type integer num)
	   (type fixnum size))
  (zerop (logand (1- size) num)))

(defun align (num size)
  "Return NUM aligned *upward* to a SIZE byte boundary."
  (declare (type integer num)
	   (type fixnum size))
  (logandc1 (1- size) (+ (1- size) num)))

;;; ----------------------------------------------------------------

(defun integer-typespec-p (thing)
  (and (or (consp thing) (symbolp thing))
       (subtypep thing 'integer)))

(defun unsigned-typespec-p (thing)
  (and (or (consp thing) (symbolp thing))
       (subtypep thing '(integer 0))))

(defun signed-typespec-p (thing)
  (and (or (consp thing) (symbolp thing))
       (subtypep thing 'integer)
       (not (subtypep thing '(integer 0)))))

;;; ----------------------------------------------------------------

(defun self-evaluating-p (x)
  (typecase x
    (null t)
    (keyword t)
    (symbol (eq x t))
    (cons nil)
    (t t)))

;;; ----------------------------------------------------------------

(defstruct (field-type (:conc-name ftype-))
  (name nil :type symbol)
  ;; if printer is T or NIL, the number is printed, if a vector, the Nth
  ;; value is printed,  if a string, format is called with it and the value,
  ;; and if a function, it is called with the value, the stream, and a dstate
  (printer nil :type (or (member nil t) vector string function))
  (sign-extend nil :type (member nil t))
  (use-label nil :type (or (member t nil) function))
  )

(defstruct (field-instance (:conc-name finst-))
  (field (req) :type inst-format-field)
  (type (req) :type (or field-type cons symbol))
  (same-as nil :type (or null inst-format-field))
  (inverse-function nil :type (or null function))
  )

(defun finst-name (fi)
  (field-name (finst-field fi)))

(defun gen-field-type-decl-form (name options)
  (destructuring-bind (&key disassem-printer disassem-use-label
			    sign-extend &allow-other-keys)
      options
    `(setf (gethash ',name
		    (params-field-types
		     (c:backend-disassem-params c:*target-backend*)))
	   (make-field-type :name ',name
			    :printer ,disassem-printer
			    :sign-extend ,sign-extend
			    :use-label ,disassem-use-label))))

(defun parse-field-type (name field-types)
  (or (gethash name field-types)
      (if (integer-typespec-p name)
	  name
	  (error "Field-type ~s not defined, and not a subtype of integer"
		 name))))

;;; ----------------------------------------------------------------

(defstruct (inst-format-field (:conc-name field-))
  (name nil :type symbol)
  (pos (byte 0 0))			; :type bytespec
  (default nil :type (or null integer))
  (default-type nil :type (or null symbol cons field-type))
  (inverse-function nil :type (or null function)))

(defstruct (inst-format (:conc-name format-)
			(:print-function %print-inst-format))
  (name nil :type symbol)
  (length 0 :type fixnum)
  (fields nil :type list)	; of inst-format-field
  (printer nil)
  (control nil)
  (ungrokable nil :type (member nil t))	;
  )

(defun %print-inst-format (format stream level)
  (declare (ignore level) (stream stream))
  (format stream "#<Instruction-format ~s ~:a>"
	  (format-name format)
	  (mapcar #'field-name (format-fields format))))

(defun gen-format-fields-maker-form (field-specs bits)
  (let ((ungrokable nil))
    (values
     `(list
       ,@(mapcar
	  #'(lambda (fspec)
	      (destructuring-bind (name pos
					&key default
					default-type
					function
					inverse-function
					&allow-other-keys)
		  fspec
		(when (and (not (null function)) (null inverse-function))
		  (setf ungrokable t))
		`(make-inst-format-field
		  :name ',name
		  :pos (correct-dchunk-bytespec-for-endianness
			,pos
			,bits
			(c:backend-byte-order c:*target-backend*))
		  :default ,default
		  :default-type
		    (parse-field-type ',default-type
				      (params-field-types
				       (c:backend-disassem-params
					c:*target-backend*)))
		  :inverse-function
		    ,(and inverse-function
			  `#',inverse-function))))
	      field-specs))
     ungrokable)))

(defun gen-inst-format-decl-form (name bits field-specs options)
  "Return a form that declares an instruction format for the disassembler
  referenced by C:*TARGET-BACKEND*.  Fields are:

  NAME		Name of instruction format
  BITS		Length of format, in bits
  FIELD-SPECS	A list of descriptions of the fields in this format.  Each
		  entry is a list of:
		      (NAME POS &KEY DEFAULT DEFAULT-TYPE FUNCTION INVERSE-FUNCTION)
		  Where POS is a byte-spec specifying the fields position
		  within the instruction, DEFAULT is an integer specifying the
		  value of the field when not otherwise specified, DEFAULT-TYPE
		  is the type of the field in this case, FUNCTION is not used,
		  but when present, means the field is unusable by the
		  disassembler except when INVERSE-FUNCTION is specified, which
		  is a mapping from the bits in a field to the correct value.
  OPTIONS		Keyword options.  Currently includes:
		  :DISASSEM-PRINTER 	Default printer for this format.
		  :DISASSEM-CONTROL	Default control for this format."
  (multiple-value-bind (fields-form ungrokable)
      (gen-format-fields-maker-form field-specs bits)
    `(setf (gethash ',name
		    (params-inst-formats
		     (c:backend-disassem-params c:*target-backend*)))
	   (make-inst-format :name ',name
			     :length (truncate ,bits vm:byte-bits)
			     :fields ,fields-form
			     :printer ,(getf options :disassem-printer)
			     :control ,(getf options :disassem-control)
			     :ungrokable ,ungrokable))))

;;; ----------------------------------------------------------------

(defstruct (inst (:print-function %print-inst))
  (name nil :type (or symbol string))

  (mask dchunk-zero :type dchunk)	; bits in the inst that are constant
  (id dchunk-zero :type dchunk)		; value of those constant bits

  (format nil :type (or null inst-format))

  (printer nil :type (or list function))
  (printer-source nil :type (or list function))
  (control nil :type (or null function))
  (use-label nil :type (or null function))

  (specializers nil :type list)		; of inst
  (args nil :type list)			; of field-instance (these are the
					; non-constant fields)
  )

(defun %print-inst (inst stream level)
  (declare (ignore level) (stream stream))
  (format stream "#<Instruction ~s ~:a {~s}>"
	  (inst-name inst)
	  (mapcar #'(lambda (field)
		      (cond ((inst-field-same-as-p inst field)
			     (format nil "~a=~a"
				     (field-name field)
				     (field-name (inst-field-same-as inst field))))
			    ((inst-field-const-p inst field)
			     (format nil "~a=~d"
				     (field-name field)
				     (inst-field-const-value inst field)))
			    (t
			     (field-name field))))
		  (remove-if #'(lambda (field)
				 (and (inst-field-const-p inst field)
				      (null (inst-field-type inst field))))
			     (format-fields (inst-format inst))))
	  (format-name (inst-format inst))))
    
(defun format-field-or-lose (name format)
  (or (find name (format-fields format) :key #'field-name)
      (error "Unknown field ~s in instruction format ~a"
	     name
	     (format-name format))))

(defun parse-inst-field (field-spec inst-format field-types)
  (destructuring-bind (field-name
		       &key constant type same-as mask inverse-function
		       &allow-other-keys)
      field-spec
    (when (eq inverse-function #'identity)
      (setf inverse-function nil))
    (let* ((field (format-field-or-lose field-name inst-format))
	   (pos (field-pos field))
	   (same-as
	    (and same-as (format-field-or-lose same-as inst-format)))
	   (type
	    (if type
		(parse-field-type type field-types)
		(field-default-type field))))
      (values
       ;; constant mask
       (cond (constant
	      (dchunk-make-mask pos))
	     (mask
	      (dchunk-clear (dchunk-make-mask pos)
			    (dchunk-make-field pos mask)))
	     (t
	      dchunk-zero))
       ;; constant id bits
       (if constant
	   (dchunk-make-field pos constant)
	   dchunk-zero)
       ;; field-instance
       (if (and (eq type (field-default-type field)) ; using this instead of
						     ; (null type) saves
						     ; about 17kbytes
		(null same-as)
		(null inverse-function))
	   nil
	   (make-field-instance :field field
				:type type
				:same-as same-as
				:inverse-function inverse-function))))))

(defun propagate-same-as-types (inst)
  "Propagate the types of specified fields to fields that are constrained to
  be the same as them, but have no types of their own."
  (let ((args (inst-args inst)))
    (do ((change t))
	((not change))
      (setf change nil)
      (dolist (finst args)
	(when (and (null (finst-type finst)) (finst-same-as finst))
	  (let* ((same-as-inst
		  (find (field-name (finst-same-as finst)) args
			:key #'finst-name))
		 (type
		  (if same-as-inst
		      (finst-type same-as-inst)
		      (field-default-type (finst-same-as finst)))))
	    (unless (null type)
	      (setf change t)
	      (setf (finst-type finst) type))))))))

(defun parse-inst-fields (inst-field-specs inst-format field-types)
  (let* ((mask dchunk-zero)
	 (id dchunk-zero)
	 (args nil))

    (dolist (field-spec inst-field-specs)
      (multiple-value-bind (field-mask field-id field-inst)
	  (parse-inst-field field-spec inst-format field-types)
	(dchunk-orf mask field-mask)
	(dchunk-orf id field-id)
	(when field-inst
	  (push field-inst args))))

    (dolist (field (format-fields inst-format))
      (unless (find (field-name field) inst-field-specs :key #'car)
	(let ((default (field-default field))
	      (default-type (field-default-type field)))
	  (unless (or default default-type)
	    (error "Field ~s in format ~s not supplied"
		   (field-name field)
		   (format-name inst-format)))
	  (cond (default
		 (dchunk-insertf id (field-pos field) default)
		 (dchunk-insertf mask (field-pos field) -1))))))

    (values mask id args)))

;;; ----------------------------------------------------------------

(defun inst-field-type (inst field)
  (let ((finst (find field (inst-args inst) :key #'finst-field)))
    (if finst
	(finst-type finst)
	(field-default-type field))))

(defun inst-field-same-as (inst field)
  (let ((finst (find field (inst-args inst) :key #'finst-field)))
    (and finst
	 (finst-same-as finst))))

(defun inst-field-const-value (inst field)
  "Returns the bit-pattern of FIELD in the instruction object INST."
  (dchunk-extract (inst-id inst) (field-pos field)))

(defun inst-field-const-p (inst field &optional value)
  "Returns non-NIL if FIELD in the instruction object INST is constrained to
  be the constant bit-pattern VALUE.  If VALUE is NIL, then non-NIL is returned
  if it's constrained to be any constant at all."
  (and (let ((field-mask (dchunk-make-mask (field-pos field))))
	 ;; must be *all* ones
	 (dchunk= (dchunk-and (inst-mask inst) field-mask)
		  field-mask))
       (or (null value)			; any constant will do
	   (= value
	      (inst-field-const-value inst field)))))

(defun inst-nfield-const-p (inst name &optional value)
  "Returns non-NIL if the field called NAME within the instruction object
  INST is constrained to be the constant bit-pattern VALUE.  If VALUE is NIL,
  then non-NIL is returned if it's constrained to be any constant at all."
  (let ((field (find name (format-fields (inst-format inst)) :key #'field-name)))
    (when (null field)
      (error "Unknown field ~s in ~s" name inst))
    (inst-field-const-p inst field value)))

(defun inst-field-same-as-p (inst field &optional other-field)
  "Returns non-NIL if FIELD within the instruction object
  INST is constrained to be the :SAME-AS as the field OTHER-FIELD.  If
  OTHER-FIELD is NIL, then non-NIL is returned if it's constrained to be the
  same as any other field."
  (let ((finst (find field (inst-args inst) :key #'finst-field)))
    (cond ((null finst)
	   nil)
	  ((not (null (finst-same-as finst)))
	   (or (null other-field)	; just generically
	       (eq other-field (finst-same-as finst))))
	  (t
	   nil))))

(defun inst-nfield-same-as-p (inst name &optional other-field)
  "Returns non-NIL if the field called NAME within the instruction object
  INST is constrained to be the :SAME-AS as the field named OTHER-FIELD.  If
  OTHER-FIELD is NIL, then non-NIL is returned if it's constrained to be the
  same as any other field."
  (let ((finst (find name (inst-args inst) :key #'finst-name)))
    (cond ((null finst)
	   (unless (find name
			 (format-fields (inst-format inst))
			 :Key #'field-name)
	     (error "Unknown field ~s in ~s" name inst))
	   nil)
	  ((not (null (finst-same-as finst)))
	   (or (null other-field)	; just generically
	       (eq other-field (field-name (finst-same-as finst)))))
	  (t
	   nil))))

;;; ----------------------------------------------------------------
;;; All this stuff here filters a printer specification-list, removing
;;; anything that's irrelevant, and reducing it to a single flat list of
;;; strings (to be printed verbatim) and atoms (field-names).

(defun choose-in-printer-p (printer)
  "Returns non-NIL if :CHOOSE occurs somewhere in PRINTER."
  (if (atom printer)
      (eq printer :choose)
      (some #'choose-in-printer-p printer)))

(defun all-printer-fields-in-format-p (printer format)
  (cond ((or (null printer) (keywordp printer) (eq printer t))
	 t)
	((symbolp printer)
	 (find printer (format-fields format) :key #'field-name))
	((listp printer)
	 (every #'(lambda (x) (all-printer-fields-in-format-p x format))
		printer))
	(t
	 t)))

(defun find-choice-in-format (choices format)
  (dolist (choice choices
		  (error "No suitable choice for format ~s found in ~s"
			 format choices))
    (when (choose-in-printer-p choice)
      (setf choice (filter-printer-for-inst-format choice format)))
    (when (all-printer-fields-in-format-p choice format)
      (return choice))))

(defun filter-printer-for-inst-format (printer format)
  "Returns a version of the disassembly-template PRINTER with any :CHOOSE
  operators resolved properly for the instruction format FORMAT.  (:CHOOSE Sub*)
  simply returns the first Sub in which every field reference refers to a field
  within FORMAT."
  (if (and (choose-in-printer-p printer) (listp printer))
      (if (eq (car printer) :choose)
	  (find-choice-in-format (cdr printer) format)
	  (mapcar #'(lambda (sub)
		      (filter-printer-for-inst-format sub format))
		  printer))
      printer))

(defun cons-maybe-cat (s cdr)
  "Returns (CONS S CDR), but if both S and the car of CDR are strings, they
  are concatenated."
  (if (and (stringp s) (stringp (car cdr)))
      (cons (concatenate 'string s (car cdr)) (cdr cdr))
      (cons s cdr)))

(defun eval-test (subj test inst)
  "Returns the result of the conditional TEST, with a default field-name of
  SUBJ, in the instruction object INST."
  (when (and (consp test) (symbolp (car test)) (not (keywordp (car test))))
    (setf subj (car test)
	  test (cdr test)))
  (and test
       (let ((key (if (consp test) (car test) test))
	     (body (if (consp test) (cdr test) nil)))
	 (cond ((eq key :constant)
		(inst-nfield-const-p inst subj (and body (car body))))
	       ((eq key :same-as)
		(inst-nfield-same-as-p inst subj (and body (car body))))
	       ((eq key :or)
		(some #'(lambda (sub) (eval-test subj sub inst)) body))
	       ((eq key :and)
		(some #'(lambda (sub) (eval-test subj sub inst)) body))
	       ((eq key :not)
		(not (eval-test subj body inst)))
	       ((and (consp key) (null body))
		(eval-test subj key inst))
	       ((eq key t)
		t)
	       (t
		(error "Bogus test-form: ~s" test))))))

(defun find-first-symbol (tree)
  "Returns the first non-keyword symbol in a depth-first search of TREE."
  (cond ((null tree)
	 nil)
	((and (symbolp tree) (not (keywordp tree)))
	 tree)
	((atom tree)
	 nil)
	(t
	 (or (find-first-symbol (car tree))
	     (find-first-symbol (cdr tree))))))

(defun flatten-printer (printer inst)
  "Returns a flat version of the disassembly template PRINTER, resolving any
  conditionals, and substituting the instruction name for :NAME, etc."
  (labels ((flatten (printer accum)
	     (flet ((handle-test-clause (clause do-when-true-p)
		      (destructuring-bind (test &rest body)
			  clause
			(let ((first-field (find-first-symbol body)))
			  (if (eq (eval-test first-field test inst)
				  do-when-true-p)
			      (values (flatten body accum) t)
			      (values nil nil))))))
	       (cond ((null printer)
		      accum)
		     ((eq printer :name)
		      (cons `',(inst-name inst) accum))
		     ((atom printer)
		      (when (and (symbolp printer)
				 (not (keywordp printer)))
			(when (not
			       (find printer
				     (format-fields (inst-format inst))
				     :key #'field-name))
			  (error "Unknown field name ~s in printer for ~s"
				 printer inst)))
		      (cons-maybe-cat printer accum))
		     ((eq (car printer) 'quote)
		      (cons printer accum))
		     ((eq (car printer) :unless)
		      (multiple-value-bind (result ok)
			  (handle-test-clause (cdr printer) nil)
			(if ok
			    result
			    accum)))
		     ((eq (car printer) :cond)
		      (dolist (clause (cdr printer)
				      (error "No test clause succeeds: ~s"
					     printer))
			(multiple-value-bind (result ok)
			    (handle-test-clause clause t)
			  (when ok
			    (return result)))))
		     ((eq (car printer) :or)
		      (dolist (sub (cdr printer)
				   (error "No suitable result for ~s found in ~s"
					  inst printer))
			(cond ((null sub)
			       (return nil))
			      (t
			       (let ((result (flatten sub accum)))
				 (unless (null result)
				   (return result)))))))
		     (t
		      (flatten (car printer)
			       (flatten (cdr printer) accum)))))))
    (flatten printer nil)))
      
(defun filter-printer-for-inst (printer inst)
  "Takes a complicated conditionalized disassembly template PRINTER, and
  returns a simple version customized for the instruction object INST,
  containing only those things which PRINT-INST-USING can handle."
  (if (functionp printer)
      printer
      (flatten-printer (filter-printer-for-inst-format printer
						       (inst-format inst))
		       inst)))

;;; ----------------------------------------------------------------

(defun gen-field-spec-forms (field-specs)
  `(list ,@(mapcar #'(lambda (fspec)
		       (destructuring-bind (name op arg
						 &key mask function
						 inverse-function type
						 &allow-other-keys)
			   fspec
			 `(list ',name
				,@(ecase op
				   (:constant
				    `(:constant ,arg))
				   (:argument
				    `(:type ',arg))
				   (:same-as
				    `(:same-as ',arg)))
				,@(and mask `(:mask ,mask))
				,@(and function `(:function #',function))
				,@(and type
				       (if (eq op :argument)
					   (error "Can't specifiy both ~s and ~s"
						  :argument :type)
					   `(:type ',type)))
				,@(and inverse-function
				       `(:inverse-function #',inverse-function))
				)))
		   field-specs)))

(defun slow-reject-inst-p (inst)
  "Reject this instruction (slow becuase we've already gone to the trouble
  of making it)."
  (or (zerop (inst-mask inst))		; probably some sort of data
					; instruction, which we can't handle
      (format-ungrokable (inst-format inst))
      ;; we *ignore* an instruction if it has an unprintable type... this
      ;; is a *STUPID* way of making sure we only have the ones we want...
      (some #'(lambda (arg)
		(let ((type (finst-type arg)))
		  (and (field-type-p type)
		       (null (ftype-printer type))
		       (null (ftype-use-label type)))))
	    (inst-args inst))))

(defun create-inst (name format-name field-specs printer control use-label params)
  (declare (type symbol name format-name)
	   (type list field-specs)
	   (type (or null list function) printer)
	   (type (or null function) control)
	   (type (or (member nil t) function) use-label)
	   (type params params))
  (let ((inst-format (gethash format-name (params-inst-formats params)))
	(field-types (params-field-types params)))
    (when (null inst-format)
      (error "Unknown instruction format ~s (for instruction ~s)"
	     format-name name))
    (multiple-value-bind (mask id args)
	(parse-inst-fields field-specs inst-format field-types)
      (let* ((printer-source
	      (or printer (format-printer inst-format)))
	     (inst
	      (make-inst :name name
			 :mask mask
			 :id id
			 :format inst-format
			 :args args
			 :control (or control (format-control inst-format))
			 :use-label use-label
			 :printer-source printer-source
			 )))
	(unless (slow-reject-inst-p inst)
	  (propagate-same-as-types inst)
	  (setf (inst-printer inst)
		(filter-printer-for-inst printer-source inst))
	  inst)))))
		
;;; Notice any instruction flavor-specifications that we obviously can't
;;; handle, without going to the trouble of consing up an instruction object.
(defun fast-reject-inst-p (flavor)
  (some #'(lambda (field-spec)
	    ;; we can only handle function-ized things if they can be inverted
	    (and (getf (cdddr field-spec) :function)
		 (not (getf (cdddr field-spec) :inverse-function))))
	(cdr flavor)))

(defun gen-inst-decl-form (inst-name inst-flavors options &optional augment)
  (destructuring-bind (&key disassem-printer disassem-control
			    disassem-use-label (disassemble t)
		       &allow-other-keys)
      options
    (when disassemble
      (let ((flavor-forms
	     (delete nil
		     (mapcar #'(lambda (flavor)
				 (destructuring-bind (format-name
						      &rest inst-field-specs)
				     flavor
				   (unless (fast-reject-inst-p flavor)
				     `(let ((inst
					     (create-inst ',inst-name
							  ',format-name
							  ,(gen-field-spec-forms
							    inst-field-specs)
							  printer
							  control
							  use-label
							  params)))
					(when inst
					  (push inst insts))))))
			     inst-flavors))))
	(unless (null flavor-forms)
	  `(let* ((params (c:backend-disassem-params c:*target-backend*))
		  (printer ,disassem-printer)
		  (control ,disassem-control)
		  (use-label ,disassem-use-label)
		  (insts nil))
	     ,@flavor-forms
	     ,(if augment
		  `(dolist (flav insts)
		     (push flav
			   (gethash ',inst-name (params-instructions params))))
		  `(setf (gethash ',inst-name (params-instructions params))
			 insts))))))))

(defmacro augment-instruction ((name &rest options) &body forms)
  `(gen-inst-decl-form ,name ,forms ,options 'c:*target-backend* t))

;;; ----------------------------------------------------------------
;;; stuff for specializing instructions

(defun inst-matches-spec-p (inst spec)
  (or (atom spec)
      (destructuring-bind (field-name constraint operand)
	  spec
	(ecase constraint
	  (:constant
	   (inst-nfield-const-p inst field-name operand))
	  (:same-as
	   (inst-nfield-same-as-p inst field-name operand))))))

(defun inst-compatible-with-specs-p (inst specs)
  "Returns non-NIL if the instruction object INST does not violate any
  constraints in SPECS, and contains all fields therein."
  (every #'(lambda (spec)
	     (let ((field
		    (find (if (atom spec) spec (car spec))
			  (format-fields (inst-format inst))
			  :key #'field-name)))
	       (and field
		    (or (atom spec)
			(inst-matches-spec-p inst spec)
			(and (not (inst-field-const-p inst field))
			     (not (inst-field-same-as-p inst field)))))))
	specs))

(defun inst-matches-specs-p (inst specs)
  (every #'(lambda (spec) (inst-matches-spec-p inst spec)) specs))

(defun non-matching-specs (inst specs)
  (remove-if #'(lambda (spec) (inst-matches-spec-p inst spec)) specs))

(defun field-in-spec-p (field specs)
  (let ((name (field-name field)))
    (some #'(lambda (spec)
	      (if (atom spec)
		  (eq name spec)
		  (eq name (car spec))))
	  specs)))

(defun field-matches-in-insts-p (field inst other-inst)
  "Returns non-NIL if FIELD is the same in both INST and OTHER-INST."
  (cond ((inst-field-const-p inst field)
	 (inst-field-const-p other-inst
			     field
			     (inst-field-const-value inst field)))
	((inst-field-same-as-p inst field)
	 (inst-field-same-as-p other-inst
			       field
			       (finst-same-as
				(find field (inst-args inst)
				      :key #'finst-field))))))

(defun inst-matches-except-for (inst other-inst exception-specs)
  "Returns non-NIL if the instruction object INST matches OTHER-INST in all
  fields except for those named in EXCEPTION-SPECS."
  (and (eq (inst-format inst) (inst-format other-inst))
       (every #'(lambda (field)
		  (or (field-in-spec-p field exception-specs)
		      (field-matches-in-insts-p field inst other-inst)))
	      (format-fields (inst-format inst)))))

(defun transmogrify-inst (old-inst specs)
  "Return a new copy of the instruction object OLD-INST, but with the
  additional field constraints SPECS."
  (let ((new-inst (copy-inst old-inst))
	(format (inst-format old-inst)))

    (setf (inst-args new-inst) (copy-list (inst-args old-inst)))

    (dolist (spec specs)
      ;; these better be real specs...
      (destructuring-bind (field-name operator operand)
	  spec
	(let ((field (format-field-or-lose field-name format)))
	  (ecase operator
	    (:constant 
	     (let ((pos (field-pos field)))
	       (dchunk-orf (inst-mask new-inst) (dchunk-make-mask pos))
	       (dchunk-orf (inst-id new-inst) (dchunk-make-field pos operand))))
	    (:same-as
	     (let ((finst
		    (find field-name (inst-args new-inst) :key #'finst-name)))

	       (cond ((null finst)
		      (setf finst
			    (make-field-instance
			     :field field
			     :type (field-default-type field))))
		     (t
		      ;; we have to copy it, since it's currently shared with
		      ;; the old instruction
		      (setf (inst-args new-inst)
			    (delete finst (inst-args new-inst)))
		      (setf finst
			    (copy-field-instance finst))))

	       (push finst (inst-args new-inst))
	       (setf (finst-same-as finst)
		     (format-field-or-lose operand format))))))))

    new-inst))

(defun apply-specializations (inst specializations)
  (destructuring-bind (&key (disassem-printer (inst-printer inst))
			    (disassem-control (inst-control inst))
			    (name (inst-name inst)))
      specializations
    (setf (inst-printer inst) disassem-printer
	  (inst-control inst) disassem-control
	  (inst-name inst) name)

    ;; after any changes
    (propagate-same-as-types inst)
    (setf (inst-printer inst)
	  (filter-printer-for-inst (inst-printer-source inst) inst))
    inst))

(defun specialize-insts (inst-list specs specializations)
  (let* ((applicable-insts		; set that have the specified fields
	  (remove-if-not #'(lambda (inst) (inst-compatible-with-specs-p inst specs))
			 inst-list))
	 (exact-matches
	  (remove-if-not #'(lambda (inst) (inst-matches-specs-p inst specs))
			 applicable-insts))
	 (non-exact-matches
	  (set-difference applicable-insts exact-matches))
	 (results
	  (remove-if #'(lambda (inst) (inst-compatible-with-specs-p inst specs))
		     inst-list)))
    (dolist (inst exact-matches)
      (push (apply-specializations inst specializations)
	    results))
    (dolist (inst non-exact-matches)
      (let ((non-matching-specs (non-matching-specs inst specs)))
	(unless (or (some #'(lambda (prev-result)
			      (inst-matches-except-for inst prev-result nil))
			  results)
		    (some #'(lambda (other-inst)
			      (inst-matches-except-for inst
						       other-inst
						       non-matching-specs))
			  results))
	  (push (apply-specializations (transmogrify-inst inst
							  non-matching-specs)
				       specializations)
		results))
	(push inst results)))
    results))

(eval-when (compile load eval)
  (defun make-specs-form (specs)
    `(list ,@(mapcar #'(lambda (spec)
			 (if (atom spec)
			     `',spec
			     `(list ',(car spec)
				    ',(cadr spec)
				    ,(if (eq (cadr spec) :constant)
					 (caddr spec)
					 `',(cadr spec)))))
		     specs)))

  (defun make-specializations-form (specializations)
    `(list ,@specializations))
  )

(defmacro specialize ((inst-name &rest specializations) &rest specs)
  "SPECIALIZE (Name Specialization-keywords*) Spec*
  where Spec is either a Field-name, or
    (Field-name Constraint Value)
  Constraint is either :SAME-AS, in which case Value should be the name of
  another field, or :CONSTANT, in which case Value should be a constant
  integer.

  Modifies the all instruction flavors named INST-NAME (possibly creating
  new, more specific ones), according to SPECS, and applies SPECIALIZATIONS to
  the resulting instructions.

  Specialization-keywords, is one of :DISASSEM-PRINTER or :DISASSEM-CONTROL,
  which have the same meaning as for DEFINE-INSTRUCTION, or :NAME, which lets
  you change the name to the given symbol/string.

  Any instruction flavors that match all given field-constraints exactly will
  be simply modified.  Any that don't, and don't have any conflicting
  constraints, will have copies made with the constraints applied to the
  copies, and then the copies modified.  If the resulting copy would be the
  same as an existing instruction, then it is not made.

  Only instruction flavors that contain all the the specified fields are used."
  `(let ((params (c:backend-disassem-params c:*target-backend*)))
     (setf (gethash ',inst-name (params-instructions params))
	   (specialize-insts (gethash ',inst-name (params-instructions params))
			     ,(make-specs-form specs)
			     ,(make-specializations-form specializations)))
     ',inst-name))

;;; ----------------------------------------------------------------
;;; an instruction space holds all known machine instructions in a form that
;;; can be easily searched

(defstruct (inst-space (:conc-name ispace-) (:print-function %print-ispace))
  (valid-mask dchunk-zero :type dchunk)	; applies to *children*
  (choices nil :type (or list vector))
  )

(defun %print-ispace (ispace stream level)
  (declare (ignore level))
  (format stream "#<Instruction-space {~x}>" (kernel:get-lisp-obj-address ispace)))

(defstruct (inst-space-choice (:conc-name ischoice-))
  (common-id dchunk-zero :type dchunk)	; applies to *parent's* mask
  (subspace (req) :type (or inst-space inst))
  )

;;; ----------------------------------------------------------------
;;; searching for an instruction in instruction space

(declaim (inline inst-matches-p choose-inst-specialization))

(defun inst-matches-p (inst chunk)
  "Returns non-NIL if all constant-bits in INST match CHUNK."
  (declare (type inst inst)
	   (type dchunk chunk))
  (dchunk= (dchunk-and (inst-mask inst) chunk) (inst-id inst)))

(defun choose-inst-specialization (inst chunk)
  "Given an instruction object, INST, and a bit-pattern, CHUNK, picks the
  most specific instruction on INST's specializer list who's constraints are
  met by CHUNK.  If none do, then INST is returned."
  (declare (type inst inst)
	   (type dchunk chunk))
  (or (dolist (spec (inst-specializers inst) nil)
	(declare (type inst spec))
	(and (inst-matches-p spec chunk)
	     (every #'(lambda (arg)
			(declare (type field-instance arg))
			(let ((same-as (finst-same-as arg)))
			  (or (null same-as)
			      (= (dchunk-extract chunk
						 (field-pos (finst-field arg)))
				 (dchunk-extract chunk
						 (field-pos same-as))))))
		    (inst-args spec))
	     (return spec)))
      inst))

(defun find-inst (chunk inst-space)
  "Returns the instruction object within INST-SPACE corresponding to the
  bit-pattern CHUNK, or NIL if there isn't one."
  (declare (type dchunk chunk)
	   (type (or null inst-space inst) inst-space))
  (cond ((null inst-space)
	 nil)
	((inst-p inst-space)
	 (if (inst-matches-p inst-space chunk)
	     (choose-inst-specialization inst-space chunk)
	     nil))
	((inst-space-p inst-space)
	 (let* ((mask (ispace-valid-mask inst-space))
		(id (dchunk-and mask chunk))
		(choices (ispace-choices inst-space)))
	   (declare (type dchunk id mask))
	   (cond ((listp choices)
		  (dolist (choice choices)
		    (declare (type inst-space-choice choice))
		    (when (dchunk= id (ischoice-common-id choice))
		      (return (find-inst chunk (ischoice-subspace choice))))))
		 (t			; must be a vector
		  (error "WOT?  NOT IMPLEMENTED")))))))

;;; ----------------------------------------------------------------
;;; building the instruction space

;;; sort of a hack, assume int types are more general
(defun field-type-specializes-p (ft1 ft2)
  "Returns non-NIL if the field-type FT1 is more specific than FT2."
  (and (integer-typespec-p ft2)
       (not (integer-typespec-p ft1))))

(defun inst-specializes-p (special general)
  "Returns non-NIL if the instruction SPECIAL is a more specific version of
  GENERAL (i.e., the same instruction, but with more constraints)."
  (declare (type inst special general))
  (and (eq (inst-format special)
	   (inst-format general))
       (let ((smask (inst-mask special))
	     (gmask (inst-mask general)))
	 (and (dchunk= (inst-id general)
		       (dchunk-and (inst-id special) gmask))
	      (or (dchunk-strict-superset-p smask gmask)
		  (and (dchunk= smask gmask)
		       (some #'(lambda (spec-finst)
				 (let ((field (finst-field spec-finst)))
				   (and (not (inst-field-const-p general field))
					(not (inst-field-same-as-p general field))
					(or (finst-same-as spec-finst)
					    (field-type-specializes-p (finst-type spec-finst)
								      (inst-field-type general field))))))
			     (inst-args special))))))))

;;; a bit arbitrary, but should work ok...
(defun specializer-rank (inst)
  "Returns an integer corresponding to the specifivity of the instruction INST."
  (declare (type inst inst))
  (+ (* (dchunk-count-bits (inst-mask inst)) 4)
     (count-if-not #'null (inst-args inst) :key #'finst-same-as)))

(defun order-specializers (insts)
  "Order the list of instructions INSTS with more specific (more constant
  bits, or same-as argument constains) ones first.  Returns the ordered list."
  (declare (type list insts))
  (sort insts
	#'(lambda (i1 i2)
	    (> (specializer-rank i1) (specializer-rank i2)))))

(defun try-specializing (insts)
  "Given a list of instructions INSTS, Sees if one of these instructions is a
  more general form of all the others, in which case they are put into its
  specializers list, and it is returned.  Otherwise an error is signaled."
  (declare (type list insts))
  (let ((masters (copy-list insts)))
    (dolist (possible-master insts)
      (dolist (possible-specializer insts)
	(unless (or (eq possible-specializer possible-master)
		    (inst-specializes-p possible-specializer possible-master))
	  (setf masters (delete possible-master masters))
	  (return)			; exit the inner loop
	  )))
    (cond ((null masters)
	   (error "Instructions aren't related: ~s" insts))
	  ((cdr masters)
	   (error "Multiple specializing master: ~s" masters))
	  (t
	   (let ((master (car masters)))
	     (setf (inst-specializers master)
		   (order-specializers (remove master insts)))
	     master)))))

(defun build-inst-space (insts &optional (initial-mask dchunk-one))
  "Returns an instruction-space object corresponding to the list of
  instructions INSTS.  If the optional parameter INITIAL-MASK is supplied, only
  bits it has set are used."
  ;; This is done by finding any set of bits that's common to
  ;; all instructions, building an instruction-space node that selects on those
  ;; bits, and recursively handle sets of instructions with a common value for
  ;; these bits (which, since there should be fewer instructions than in INSTS,
  ;; should have some additional set of bits to select on, etc).  If there
  ;; are no common bits, or all instructions have the same value within those
  ;; bits, TRY-SPECIALIZING is called, which handles the cases of many
  ;; variations on a single instruction.
  (declare (type list insts)
	   (type dchunk initial-mask))
  (cond ((null insts)
	 nil)
	((null (cdr insts))
	 (car insts))
	(t
	 (let ((vmask (dchunk-copy initial-mask)))
	   (dolist (inst insts)
	     (dchunk-andf vmask (inst-mask inst)))
	   (if (dchunk-zerop vmask)
	       (try-specializing insts)
	       (let ((buckets nil))
		 (dolist (inst insts)
		   (let* ((common-id (dchunk-and (inst-id inst) vmask))
			  (bucket (assoc common-id buckets :test #'dchunk=)))
		     (cond ((null bucket)
			    (push (list common-id inst) buckets))
			   (t
			    (push inst (cdr bucket))))))
		 (let ((submask (dchunk-clear initial-mask vmask)))
		   (if (= (length buckets) 1)
		       (try-specializing insts)
		       (let ((choices
			      (mapcar #'(lambda (bucket)
					  (make-inst-space-choice
					   :subspace (build-inst-space
						      (cdr bucket)
						      submask)
					   :common-id (car bucket)))
				      buckets)))
			 ;; note that we could instead build a vector of
			 ;; choices (which could be indexed), but that's more
			 ;; complicated, so we leave it off for now.
			 (make-inst-space :valid-mask vmask
					  :choices choices))
		       ))))))))

;;; ----------------------------------------------------------------
;;; a space printer for debugging purposes

(defun ind-inst-args (inst ind)
  (format t "~v,10t~{ ~s~} ~8,'0x ~8,'0x"
	  ind
	  (mapcar #'(lambda (fi)
		      (let ((same-as (finst-same-as fi))
			    (type (finst-type fi))
			    (name (field-name (finst-field fi))))
			(cond (same-as
			       (list name '= (field-name same-as)))
			      ((inst-field-const-p inst (finst-field fi))
			       (list name '=
				     (inst-field-const-value inst
							     (finst-field fi))))
			      ((field-type-p type)
			       (list name (ftype-name type)))
			      (t
			       (list name type)))))
		  (reverse (inst-args inst)))
	  (inst-mask inst)
	  (inst-id inst)))

(defun print-inst-space (inst-space &optional (indent 1))
  "Prints a nicely formatted version of INST-SPACE."
  (cond ((null inst-space)
	 nil)
	((inst-p inst-space)
	 (format t "~vt[~s" indent (inst-name inst-space))
	 (ind-inst-args inst-space indent)
	 (dolist (inst (inst-specializers inst-space))
	   (format t "~%~vt:~s" indent (inst-name inst))
	   (ind-inst-args inst indent))
	 (write-char #\])
	 (terpri))
	(t
	 (format t "~vt---- ~8,'0x ----~%" indent (ispace-valid-mask inst-space))
	 (map nil
	      #'(lambda (choice)
		  (format t "~vt~8,'0x ==>~%"
			  (+ 2 indent)
			  (ischoice-common-id choice))
		  (print-inst-space (ischoice-subspace choice)
				    (+ 4 indent)))
	      (ispace-choices inst-space)))))

;;;; ----------------------------------------------------------------
;;;; the actual disassembly part
;;;; ----------------------------------------------------------------

;;; ----------------------------------------------------------------
;;; getting at the source code...

(defstruct (source-form-cache (:conc-name sfcache-))
  (debug-source nil :type (or null di:debug-source))
  (top-level-form-index -1 :type fixnum)
  (top-level-form nil :type list)
  (form-number-mapping-table nil :type (or null (vector list)))
  (last-location-retrieved nil :type (or null di:code-location))
  (last-form-retrieved -1 :type fixnum)
  )

(defun get-top-level-form (debug-source tlf-index)
  (let ((name (di:debug-source-name debug-source)))
    (ecase (di:debug-source-from debug-source)
      (:file
       (cond ((not (probe-file name))
	      (warn "The source file ~s no longer seems to exist" name)
	      nil)
	     (t
	      (let ((start-positions
		     (di:debug-source-start-positions debug-source)))
		(cond ((null start-positions)
		       (warn "No start positions map")
		       nil)
		      (t
		       (let* ((local-tlf-index
			       (- tlf-index
				  (di:debug-source-root-number debug-source)))
			      (char-offset
			       (aref start-positions local-tlf-index)))
			 (with-open-file (f name)
			   (cond ((= (di:debug-source-created debug-source)
				     (file-write-date name))
				  (file-position f char-offset))
				 (t
				  (warn "Source file ~s has been modified; ~@
					 Using form offset instead of file index"
					name)
				  (dotimes (i local-tlf-index) (read f))))
			   (read f)
			   ))))))))
      ((:lisp :stream)
       (aref name tlf-index)))))

(defun cache-valid (loc cache)
  (and cache
       (and (eq (di:code-location-debug-source loc)
		(sfcache-debug-source cache))
	    (eq (di:code-location-top-level-form-offset loc)
		(sfcache-top-level-form-index cache)))))

(defun get-source-form (loc context &optional cache)
  (let* ((cache-valid (cache-valid loc cache))
	 (tlf-index (di:code-location-top-level-form-offset loc))
	 (form-number (di:code-location-form-number loc))
	 (top-level-form
	  (if cache-valid
	      (sfcache-top-level-form cache)
	      (get-top-level-form (di:code-location-debug-source loc)
				  tlf-index)))
	 (mapping-table
	  (if cache-valid
	      (sfcache-form-number-mapping-table cache)
	      (di:form-number-translations top-level-form tlf-index))))
    (when (and (not cache-valid) cache)
      (setf (sfcache-debug-source cache) (di:code-location-debug-source loc)
	    (sfcache-top-level-form-index cache) tlf-index
	    (sfcache-top-level-form cache) top-level-form
	    (sfcache-form-number-mapping-table cache) mapping-table))
    (cond ((null top-level-form)
	   nil)
	  ((> form-number (length mapping-table))
	   (warn "Bogus form-number in form!  The source file has probably ~@
		  been changed too much to cope with")
	   (when cache
	     ;; disable future warnings
	     (setf (sfcache-top-level-form cache) nil))
	   nil)
	  (t
	   (when cache
	     (setf (sfcache-last-location-retrieved cache) loc)
	     (setf (sfcache-last-form-retrieved cache) form-number))
	   (di:source-path-context top-level-form
				   (aref mapping-table form-number)
				   context)))))

(defun get-different-source-form (loc context &optional cache)
  (if (and (cache-valid loc cache)
	   (or (= (di:code-location-form-number loc)
		  (sfcache-last-form-retrieved cache))
	       (and (sfcache-last-location-retrieved cache)
		    (di:code-location= loc (sfcache-last-location-retrieved cache)))))
      (values nil nil)
      (values (get-source-form loc context cache) t)))

;;; ----------------------------------------------------------------
;;;

;;; All state during disassembly.  We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate-)
			   (:print-function %print-dstate))
  (curpos 0 :type integer)		; address of current instruction
  (nextpos 0 :type integer)		; address of next instruction

  (code nil :type (or null kernel:code-component))
  (real-code-insts-addr 0 :type integer) ; address of instructions area (only
					; used for avoiding gc effects) in
					; the code object
  (code-insts-addr 0 :type integer)	; the instruction area fixed at
					; creation time (doesn't change with
					; a gc)
  (code-insts-offset 0 :type fixnum)	; offset of instruction area from the
					; start of the code object

  (segment-start 0 :type integer)	; start of our instruction segment
  (segment-sap (req) :type system:system-area-pointer)
					; a sap pointing to our segment--
					; NOTE: this *may* be different from
					; segment-start!
  (segment-length 0 :type fixnum)	; length thereof

  (alignment vm:word-bytes :type fixnum) ; what to align to in most cases
  (byte-order :little-endian
	      :type (member :big-endian :little-endian))

  (properties nil :type list)		; for user code to hang stuff off of

  (addr-print-len nil :type		; used for prettifying printing
		  (or null fixnum))
  (argument-column 0 :type fixnum)
  (output-state :beginning		; to make output look nicer
		  :type (member :beginning
				:block-boundary
				nil))

  (labels nil :type list)		; alist of (address . label-number)
  (fun-header-addresses nil :type list)	; list of byte-offsets from code
  (hooks nil :type list)		; alist of (address . function)

  (label-hash (make-hash-table)		; same info in a different form
	      :type hash-table)

  ;; versions of the above being run through
  (cur-labels nil :type list)		; alist of (address . label-number)
  (cur-fun-header-addresses nil :type list) ; list of byte-offsets from code
  (cur-hooks nil :type list)		; alist of (address . function)

  (notes nil :type list)		; for the current location

  (storage-info nil			; info about source variables
		:type (or null storage-info))
  (current-valid-locations nil		; currently active source variables
			   :type (or null (vector bit)))

  (params (req) :type params)		; a handy pointer ...
  )

(defun %print-dstate (dstate stream level)
  (declare (ignore level))
  (format stream "#<Disassembly state at #x~x in #x~x[~d]~@[ in ~s~]>"
	  (dstate-curpos dstate)
	  (dstate-segment-start dstate)
	  (dstate-segment-length dstate)
	  (dstate-code dstate)))

(defmacro dstate-get-prop (dstate name)
  "Get the value of the property called NAME in DSTATE.  Also setf'able."
  `(getf (dstate-properties ,dstate) ,name))

;;; ----------------------------------------------------------------

(defun arg-value (name chunk inst)
  "Given the NAME of a field in the instruction with bit-pattern CHUNK and
  corresponding to the instruction object INST, returns two values: the
  contents of the field, and the field's type (which is either the type-spec of
  a subtype of integer or a FIELD-TYPE).  Any sign-extension is done here.  An
  error is signaled if NAME doesn't correspond to any field in INST."
  (declare (type symbol name)
	   (type dchunk chunk)
	   (type inst inst))
  (let* ((finst (find name (inst-args inst) :key #'finst-name))
	 (field
	  (if finst
	      (finst-field finst)
	      (find name (format-fields (inst-format inst))
		    :key #'field-name)))) 

    (when (null field)
      ;; must be a constant?
      (error "Unknown field ~a in ~s" name inst))

    ;; ok we know about it
    (let ((type
	   (if finst
	       (finst-type finst)
	       (field-default-type field)))
	  (ifunc (and finst (finst-inverse-function finst)))
	  (value (dchunk-extract chunk (field-pos field))))

      (when (null type)
	(error "Field ~s in ~s is of unknown type" name inst))

      (when (if (field-type-p type)
		(ftype-sign-extend type)
		(signed-typespec-p type))
	(setf value (sign-extend value (byte-size (field-pos field)))))

      (when ifunc
	(setf value (funcall ifunc value)))

      (values value type))))

;; sort of ugly, but it tries to handle everything
(defun print-field (name chunk inst stream dstate)
  "Write the contents field called NAME of the instruction with bit-pattern
  CHUNK and corresponding to the instruction object INST, to STREAM.  The
  format in which the contents are written is either specified by the type of
  the field within the instruction, or as simple integer (signed or non-signed
  specified by the instruction).  If the instruction tags it as a label and
  it's value (modified by the field's COMPUTE-LABEL function) corresponds to a
  label known by DSTATE, then that is used instead."
  (declare (type symbol name)
	   (type dchunk chunk)
	   (type inst inst)
	   (type stream stream)
	   (type disassem-state dstate))
  (multiple-value-bind (value type)
      (arg-value name chunk inst)
    (cond ((field-type-p type)
	   (let ((printer (ftype-printer type))
		 (use-label (ftype-use-label type)))

	     (when use-label
	       (unless (eq use-label t)
		 (setf value (funcall use-label value dstate)))
	       (let ((lookup-label
		      (gethash value (dstate-label-hash dstate))))
		 (when lookup-label
		   (setf value lookup-label))))

	     (cond ((or (null printer) (eq printer t))
		    (if (and use-label (integerp value))
			(write value :base 16 :radix t :stream stream)
			(princ value stream)))
		   ((stringp printer)
		    (format stream printer value))
		   ((vectorp printer)
		    (princ (aref printer value) stream))
		   (t
		    (funcall printer value stream dstate)))))
	  (t
	   (princ value stream)))))

(defun print-inst-using (printer chunk inst stream dstate)
  "Print a disassembled version of the instruction with bit-pattern CHUNK,
  and corresponding to the instruction object INST, to STREAM, using PRINTER as
  a template.  PRINTER should be a list, where each element is either a string,
  whose contents are written verbatim, :NAME, which causes the instruction name
  to be written, :TAB, which causes the cursor to be moved to the
  argument-column of the output, (QUOTE symbol), which causes the symbol to be
  written, or a symbol, which causes the contents of the field of this name
  within the instruction to be written."
  (declare (type list printer)
	   (type dchunk chunk)
	   (type inst inst)
	   (type stream stream)
	   (type disassem-state dstate))
  (dolist (element printer)
    (etypecase element
      (string
       (write-string element stream))
      (symbol
       (if (eq element :tab)
	   (format stream "~v,1t" (dstate-argument-column dstate))
	   (print-field element chunk inst stream dstate)))
      (cons
       (if (eq (car element) 'quote)
	   (princ (cadr element) stream)
	   (error "Bogus element ~s to print-inst-using" element)))
      )))

(defun print-inst (chunk inst stream dstate)
  "Print a disassembled version of the instruction with bit-pattern CHUNK,
  and corresponding to the instruction object INST to STREAM.  DSTATE is a
  DISASSEM-STATE object."
  (declare (type dchunk chunk)
	   (type inst inst)
	   (type stream stream)
	   (type disassem-state dstate))
  (let ((printer (inst-printer inst)))
    (unless printer
      (error "I don't know how to print ~s" inst))
    (etypecase printer
      (function
       (funcall printer chunk inst stream dstate))
      (list
       (print-inst-using printer chunk inst stream dstate)))))

;;; ----------------------------------------------------------------

(defmacro to-bytes (num)
  "Converts a word-offset NUM to a byte-offset."
  `(ash ,num vm:word-shift))

(defmacro to-words (num)
  "Converts a byte-offset NUM to a word-offset."
  `(ash ,num (- vm:word-shift)))

;;; Code object layout:
;;;	header-word
;;;	code-size (starting from first inst, in words)
;;;	entry-points (points to first function header)
;;;	debug-info
;;;	trace-table-offset (starting from first inst, in bytes)
;;;	constant1
;;;	constant2
;;;	...
;;;	<padding to dual-word boundry>
;;;	start of instructions
;;;	...
;;;	function-headers and lra's buried in here randomly
;;;	...
;;;	start of trace-table
;;;	<padding to dual-word boundry>
;;;
;;; Function header layout (dual word aligned):
;;;	header-word
;;;	self pointer
;;;	next pointer (next function header)
;;;	name
;;;	arglist
;;;	type
;;;
;;; LRA layout (dual word aligned):
;;;	header-word

(defconstant lra-size (to-bytes 1))

(defmacro with-matching-addresses ((thing-var address alist-place) &body body)
  (let ((source-alist-var (gensym))
	(cell-var (gensym))
	(address-var (gensym)))
    `(let ((,source-alist-var ,alist-place)
	   (,address-var ,address))
       (loop
	 (unless (and ,source-alist-var (<= (caar ,source-alist-var) ,address-var))
	   (return))
	 (let ((,cell-var (pop ,source-alist-var)))
	   (when (= (car ,cell-var) ,address-var)
	     (let ((,thing-var (cdr ,cell-var)))
	       ,@body))
	   (setf ,alist-place ,source-alist-var))))))

;;; ----------------------------------------------------------------
;;; Routines to find things in the lisp environment.  Obviously highly
;;; implementation specific!

(defconstant groked-symbol-slots
  (sort `((,vm:symbol-value-slot . symbol-value)
	  (,vm:symbol-plist-slot . symbol-plist)
	  (,vm:symbol-name-slot . symbol-name)
	  (,vm:symbol-package-slot . symbol-package))
	#'<
	:key #'car)
  "An alist of (SYMBOL-SLOT-OFFSET . ACCESS-FUNCTION-NAME) for slots in a
symbol object that we know about.")

(defun grok-symbol-slot-ref (address)
  "Given ADDRESS, try and figure out if which slot of which symbol is being
  refered to.  Of course we can just give up, so it's not a big deal...
  Returns two values, the symbol and the name of the access function of the
  slot."
  (declare (type integer address))
  (if (not (aligned-p address vm:word-bytes))
      (values nil nil)
      (do ((slots-tail groked-symbol-slots (cdr slots-tail)))
	  ((null slots-tail)
	   (values nil nil))
	(let* ((field (car slots-tail))
	       (slot-offset (to-bytes (car field)))
	       (maybe-symbol-addr (- address slot-offset))
	       (maybe-symbol (kernel:make-lisp-obj (+ maybe-symbol-addr vm:other-pointer-type))))
	  (when (symbolp maybe-symbol)
	    (return (values maybe-symbol (cdr field))))))))

(defconstant nil-addr (kernel:get-lisp-obj-address nil))

(defun grok-nil-indexed-symbol-slot-ref (byte-offset)
  "Given a BYTE-OFFSET from NIL, try and figure out if which slot of which
  symbol is being refered to.  Of course we can just give up, so it's not a big
  deal...  Returns two values, the symbol and the access function."
  (declare (type fixnum byte-offset))
  (grok-symbol-slot-ref (+ nil-addr byte-offset)))

(defun get-nil-indexed-object (byte-offset)
  "Returns the lisp object located BYTE-OFFSET from NIL."
  (declare (type fixnum byte-offset))
  (kernel:make-lisp-obj (+ nil-addr byte-offset)))

(defun get-code-constant (byte-offset dstate)
  "Returns two values; the lisp-object located at BYTE-OFFSET in the constant
  area of the code-object in DSTATE and T, or NIL and NIL if there is no
  code-object in DSTATE."
  (declare (type fixnum byte-offset)
	   (type disassem-state dstate))
  (let ((code (dstate-code dstate)))
    (if code
	(values
	 (kernel:code-header-ref code
				 (ash (+ byte-offset vm:other-pointer-type)
				      (- vm:word-shift)))
	 t)
	(values
	 nil
	 nil))))

(defvar *assembler-routines-by-addr* nil)

(defun find-assembler-routine (address)
  "Returns the name of the primitive lisp assembler routine located at
  ADDRESS, or NIL if there isn't one."
  (declare (type integer address))
  (when (null *assembler-routines-by-addr*)
    (setf *assembler-routines-by-addr* (make-hash-table))
    (maphash #'(lambda (name address)
		 (setf (gethash address *assembler-routines-by-addr*) name))
	     lisp::*assembler-routines*))
  (gethash address *assembler-routines-by-addr*))

;;; ----------------------------------------------------------------
;;; function ops

(defun fun-self (fun)
  (declare (type compiled-function fun))
  (ext:truly-the compiled-function (system:%primitive function-self fun)))

(defun fun-code (fun)
  (declare (type compiled-function fun))
  (kernel:function-code-header (fun-self fun)))

(defun fun-next (fun)
  (declare (type compiled-function fun))
  (ext:truly-the compiled-function (system:%primitive function-next fun)))

(defun fun-address (function)
  (declare (type compiled-function function))
  (- (kernel:get-lisp-obj-address function)
     vm:function-pointer-type))

(defun fun-offset (function)
  (declare (type compiled-function function))
  (- (fun-address function)
     (system:sap-int (kernel:code-instructions (fun-code function)))))

;;; ----------------------------------------------------------------
;;; Operations on code-components (which hold the instructions for
;;; one or more functions).

(defun code-inst-area-length (code-component)
  "Returns the length of the instruction area in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (kernel:code-header-ref code-component vm:code-trace-table-offset-slot))

(defun code-inst-area-address (code-component)
  "Returns the address of the instruction area in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (system:sap-int (kernel:code-instructions code-component)))

(defun code-first-function (code-component)
  "Returns the first function in CODE-COMPONENT."
  (declare (type kernel:code-component code-component))
  (kernel:code-header-ref code-component vm:code-trace-table-offset-slot))

(defun code-addr-offset (code-component addr)
  "Returns the offset from the beginning of CODE-COMPONENT of ADDR."
  (declare (type kernel:code-component code-component)
	   (type integer addr))
  (- addr (logandc1 vm:lowtag-mask (kernel:get-lisp-obj-address code-component))))

(defun code-offs-address (code-component offs)
  "Returns the address of OFFS bytes from the beginning of CODE-COMPONENT."
  (declare (type kernel:code-component code-component)
	   (type fixnum offs))
  (+ offs (logandc1 vm:lowtag-mask (kernel:get-lisp-obj-address code-component))))

;;; ----------------------------------------------------------------

(defun dstate-code-insts-offs-address (dstate offs)
  (declare (type disassem-state dstate)
	   (type fixnum offs))
  (+ (dstate-code-insts-addr dstate)
     offs))

(defun dstate-true-code-address (dstate address)
  "Translates a canonical address in DSTATE's code-component (e.g.,
  at the position when DSTATE was first created) to the actual
  address, which could be different if the code-component was moved
  by a GC."
  (declare (type disassem-state dstate)
	   (type integer address))
  (+ (- address
	(dstate-code-insts-addr dstate))
     (dstate-real-code-insts-addr dstate)))

(defun dstate-code-addr-offset (dstate address)
  "Translates an address in DSTATE's code-component to the address
  it would be if no GCs had occurred since DSTATE was created."
  (declare (type disassem-state dstate)
	   (type integer address))
  (+ (- address
	(dstate-code-insts-addr dstate))
     (dstate-code-insts-offset dstate)))

(defun make-sorted-fun-header-addr-list (code)
  "Returns a sorted list of the ADDRESSES of function-headers in the
  code-object CODE."
  (do ((fun (kernel:code-header-ref code vm:code-entry-points-slot)
	    (fun-next fun))
       (fun-header-addrs nil))
      ((null fun)
       (sort fun-header-addrs #'<))
    (let ((fun-offset (kernel:get-closure-length fun)))
      ;; There is function header fun-offset words from the
      ;; code header.
      (push (code-offs-address code (to-bytes fun-offset))
	    fun-header-addrs))))

(defun lra-p (chunk dstate)
  "Returns non-NIL if CHUNK is a valid LRA header in DSTATE."
  (declare (type dchunk chunk)
	   (type disassem-state dstate))
  (and (aligned-p (dstate-curpos dstate) (* 2 vm:word-bytes))
       (let ((byte-offset
	      (dstate-code-addr-offset dstate (dstate-curpos dstate))))
	 (= (dchunk-corrected-extract chunk
				      (byte vm:word-bits 0)
				      vm:word-bits
				      (dstate-byte-order dstate))
	    (logior (ash (to-words byte-offset) vm:type-bits)
		    vm:return-pc-header-type)))))

(defun at-fun-header-p (dstate)
  "Returns non-NIL if DSTATE is currently pointing at a function header."
  (declare (type disassem-state dstate))
  (let ((header-addresses (dstate-cur-fun-header-addresses dstate)))
    (when header-addresses
      (let ((addr (dstate-curpos dstate)))
	(if (< (car header-addresses) addr)
	    (do ()
		((or (null header-addresses)
		     (>= (car header-addresses) addr))
		 (cond ((and header-addresses
			     (= addr (car header-addresses)))
			(setf (dstate-cur-fun-header-addresses dstate) (cdr header-addresses))
			t)
		       (t
			(setf (dstate-cur-fun-header-addresses dstate) header-addresses)
			nil)))
	      (pop header-addresses))
	    (if (= addr (car header-addresses))
		(progn
		  (setf (dstate-cur-fun-header-addresses dstate) (cdr header-addresses))
		  t)
		nil))))))

(defun print-fun-header (stream dstate)
  "Print the function-header (entry-point) pseudo-instruction at the current
  location in DSTATE to STREAM."
  (declare (type stream stream)
	   (type disassem-state dstate))
  (let* ((code (dstate-code dstate))
	 (woffs
	  (to-words
	   (dstate-code-addr-offset dstate (dstate-curpos dstate))))
	 (name
	  (kernel:code-header-ref code (+ woffs vm:function-header-name-slot)))
	 (args
	  (kernel:code-header-ref code (+ woffs vm:function-header-arglist-slot)))
	 (type
	  (kernel:code-header-ref code (+ woffs vm:function-header-type-slot))))
    (format stream ".~a ~s~:a" 'entry name args)
    (note #'(lambda (stream)
	      (format stream "~:s" type)) ; use format to print NIL as ()
	  dstate)))

(defun check-for-moved-code (dstate)
  "If the code object in DSTATE has moved since we last checked, make the sap
  pointing to the start of the segment point to its corresponding location at
  the new address.  Isn't worth much unless called with GCing turned off."
  (declare (type disassem-state dstate))
  (let ((code (dstate-code dstate)))
    (when code
      (let ((old-code-insts-addr (dstate-real-code-insts-addr dstate))
	    (new-code-insts-addr (code-inst-area-address code)))
	(when (/= old-code-insts-addr
		  new-code-insts-addr)
#+nil	  (format t "~&;;; CODE MOVED: #x~x --> #x~x~%"
		  old-code-insts-addr
		  new-code-insts-addr)
	  (setf (dstate-segment-sap dstate)
		(system:int-sap
		 (+ new-code-insts-addr
		    (- (dstate-segment-start dstate)
		       (dstate-code-insts-addr dstate)))))
#+nil	  (format t "~&;;; SAP now points at #x~x (#~x+~d)~%"
		  (system:sap-int (dstate-segment-sap dstate))
		  new-code-insts-addr
		  (- (dstate-segment-start dstate)
		       (dstate-code-insts-addr dstate)))
	  (setf (dstate-real-code-insts-addr dstate)
		new-code-insts-addr))))))

;;; ----------------------------------------------------------------

(defun rewind-current-segment (dstate)
  (declare (type disassem-state dstate))
  (setf (dstate-curpos dstate) (dstate-segment-start dstate)
	(dstate-cur-fun-header-addresses dstate) (dstate-fun-header-addresses dstate)
	(dstate-cur-labels dstate) (dstate-labels dstate)))

(defun compute-labels (dstate)
  "Make an initial non-printing disassembly pass through DSTATE, noting any
  addresses that are referenced by instructions."
  ;; add labels at the beginning with a label-number of nil; we'll notice
  ;; later and fill them in (and sort them)
  (declare (type disassem-state dstate))
  (let ((byte-order (dstate-byte-order dstate))
	(ispace (get-inst-space (dstate-params dstate)))
	(labels (dstate-labels dstate)))

    (rewind-current-segment dstate)

    (loop
      (unless (aligned-p (dstate-curpos dstate) (dstate-alignment dstate))
	(setf (dstate-curpos dstate)
	      (align (dstate-curpos dstate) (dstate-alignment dstate))))

      (let ((offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
	(when (>= offs (dstate-segment-length dstate))
	  ;; done!
	  (setf (dstate-labels dstate) labels)
	  (setf (dstate-notes dstate) nil) ; just in case any got
					; left there by labeling
					; (they shouldn't but...)
	  (return labels))

	(system:without-gcing
	 (check-for-moved-code dstate)

	 (cond ((at-fun-header-p dstate)
		(incf (dstate-curpos dstate)
		      (to-bytes vm:function-header-code-offset)))
	       (t
		(let* ((chunk
			(sap-ref-dchunk (dstate-segment-sap dstate)
					offs
					byte-order))
		       (inst (find-inst chunk ispace)))
		  (labels ((add-label (addr)
			     (unless (find addr labels :test #'= :key #'car)
			       (push (cons addr nil) labels)))
			   (maybe-label-field (field type)
			     (when (field-type-p type)
			       (let ((use-label (ftype-use-label type)))
				 (when use-label
				   (let* ((pos (field-pos field))
					  (value (dchunk-extract chunk pos)))
				     (when (ftype-sign-extend type)
				       (setf value
					     (sign-extend value
							  (byte-size pos))))

				     (let ((addr
					    (if (eq use-label t)
						value
						(funcall use-label
							 value dstate))))
				       (add-label addr))))))))
		    (cond ((lra-p chunk dstate)
			   (incf (dstate-curpos dstate) lra-size))
			  ((null inst)
			   ;; let alignment fix it up
			   (incf (dstate-curpos dstate)))
			  ((inst-use-label inst)
			   (add-label (funcall (inst-use-label inst)
					       chunk inst dstate)))
			  (t
			   (setf (dstate-nextpos dstate)
				 (+ (dstate-curpos dstate)
				    (format-length (inst-format inst))))

			   (let ((inst-args (inst-args inst)))
			     ;; Look at possible labels refered to in the
			     ;; various instruction fields.
			     ;; We do it in this order (explicit args, then fields
			     ;; NOT in the arg list) instead of the obvious
			     ;; one (just all fields, looking in arg list
			     ;; for type overrides) for efficiency.
			     (dolist (finst inst-args)
			       (maybe-label-field (finst-field finst)
						  (finst-type finst)))
			     (dolist (field (format-fields (inst-format inst)))
			       (let ((type (field-default-type field)))
				 (when type
				   (unless (find field inst-args :key #'finst-field)
				     (maybe-label-field field type)))))
			     )

			   (if (inst-control inst)
			       (funcall (inst-control inst)
					chunk inst nil dstate))

			   (setf (dstate-curpos dstate)
				 (dstate-nextpos dstate))
			   )))))))))))

(defun number-labels (dstate)
  "If any labels in DSTATE have been added since the last call to this
  function, give them label-numbers, enter them in the hash-table, and make
  sure the label list is in sorted order."
  (let ((labels (dstate-labels dstate)))
    (when (and labels (null (cdar labels)))
      ;; at least one label left un-numbered
      (setf labels (sort labels #'< :key #'car))
      (let ((max -1)
	    (label-hash (dstate-label-hash dstate)))
	(dolist (label labels)
	  (when (not (null (cdr label)))
	    (setf max (max max (cdr label)))))
	(dolist (label labels)
	  (when (null (cdr label))
	    (incf max)
	    (setf (cdr label) max)
	    (setf (gethash (car label) label-hash)
		  (format nil "L~d" max)))))
      (setf (dstate-labels dstate) labels))))

;;; ----------------------------------------------------------------

(defun get-inst-space (params)
  "Get the instruction-space from PARAMS, creating it if necessary."
  (declare (type params params))
  (let ((ispace (params-inst-space params)))
    (when (null ispace)
      (let ((insts nil))
	(maphash #'(lambda (name inst-flavs)
		     (declare (ignore name))
		     (dolist (flav inst-flavs)
		       (push flav insts)))
		 (params-instructions params))
	(setf ispace (build-inst-space insts)))
      (setf (params-inst-space params) ispace))
    ispace))  

;;; ----------------------------------------------------------------
;;; add global hooks

(defun add-hook (dstate addr hook)
  (let ((entry (cons addr hook)))
    (if (null (dstate-hooks dstate))
	(setf (dstate-hooks dstate) (list entry))
	(push entry (cdr (last (dstate-hooks dstate)))))))

(defun add-note-hook (dstate addr note)
  (add-hook dstate
	    addr
	    #'(lambda (stream)
		(when stream
		  (note note dstate)))))

(defun add-comment-hook (dstate addr comment)
  (add-hook dstate
	    addr
	    #'(lambda (stream)
		(when stream
		  (write-string ";;; " stream)
		  (etypecase comment
		    (string
		     (write-string comment stream))
		    (function
		     (funcall comment stream)))
		  (terpri stream)))))

;;; ----------------------------------------------------------------

(defun set-address-printing-range (dstate from length)
  (setf (dstate-addr-print-len dstate)
	;; 4 bits per hex digit
	(ceiling (integer-length (logxor from (+ from length))) 4)))

(defun print-current-address (stream dstate)
  "Print the current address in DSTATE to STREAM, plus any labels that
  correspond to it, and leave the cursor in the instruction column."
  (declare (type stream stream)
	   (type disassem-state dstate))
  (let ((address (dstate-curpos dstate))
	(address-column-width
	 (params-address-column-width (dstate-params dstate)))
	(plen (dstate-addr-print-len dstate)))

    (when (null plen)
      (setf plen address-column-width)
      (set-address-printing-range dstate
				  (dstate-segment-start dstate)
				  (dstate-segment-length dstate)))
    (when (eq (dstate-output-state dstate) :beginning)
      (setf plen address-column-width))

    (format stream "~&~v,0t~v,'0x:"
	    (- address-column-width plen)
	    plen
	    (ldb (byte (* 4 plen) 0) address))
    (with-matching-addresses (label-number address (dstate-cur-labels dstate))
      (format stream " L~d:" label-number))
    (format stream "~v,0t" (+ address-column-width 1 label-column-width))
    ))

;;; ----------------------------------------------------------------

(defmacro with-print-restrictions (&rest body)
  `(let ((*print-pretty* t)
	 (*print-lines* 2)
	 (*print-length* 4)
	 (*print-level* 3))
     ,@body))

(defun print-notes-and-newline (stream dstate)
  "Print a newline to STREAM, inserting any pending notes in DSTATE as
  end-of-line comments.  If there is more than one note, a separate line
  will be used for each one."
  (declare (type stream stream)
	   (type disassem-state dstate))
  (with-print-restrictions
    (dolist (note (dstate-notes dstate))
      (format stream "~vt; " *note-column*)
      (etypecase note
	(string
	 (write-string note stream))
	(function
	 (funcall note stream)))
      (terpri stream))
    (fresh-line stream)
    (setf (dstate-notes dstate) nil)))

(defun print-bytes (num stream dstate)
  "Disassemble NUM bytes to STREAM as simple `BYTE' instructions"
  (declare (type fixnum num)
	   (type stream stream)
	   (type disassem-state dstate))
  (format stream "~a~vt" 'BYTE (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
    (dotimes (offs num)
      (unless (zerop offs)
	(write-string ", " stream))
      (format stream "#x~2,'0x" (system:sap-ref-8 sap (+ offs start-offs))))))

(defun print-words (num stream dstate)
  "Disassemble NUM machine-words to STREAM as simple `WORD' instructions"
  (declare (type fixnum num)
	   (type stream stream)
	   (type disassem-state dstate))
  (format stream "~a~vt" 'WORD (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
    (dotimes (offs num)
      (unless (zerop offs)
	(write-string ", " stream))
      (format stream "#x~8,'0x"
	      (system:sap-ref-32 sap (+ offs start-offs))))))

;;; ----------------------------------------------------------------

(defun create-dstate (code params)
  "Make a disassembler-state object for the code-component CODE (which may
  also be NIL).  the call to this function should probably be done with GC
  disabled, but if CODE is non-NIL, then it's safe to turn GCing back on
  again after it returns."
  (declare (type (or null kernel:code-component) code)
	   (type params params))
  (let ((sap
	 ;; something safe; it will get changed later
	 (if code
	     (kernel:code-instructions code)
	     (system:int-sap
	      (- (kernel:get-lisp-obj-address nil) vm:list-pointer-type) )))
	(inst-area-addr (if code (code-inst-area-address code) 0)))
    (make-disassem-state :code code
			 :segment-sap sap
			 :segment-start 0
			 :segment-length 0
			 :params params

			 :fun-header-addresses
			   (and code
				(make-sorted-fun-header-addr-list code))
			 :real-code-insts-addr inst-area-addr
			 :code-insts-addr inst-area-addr
			 :code-insts-offset
			   (if code (code-addr-offset code inst-area-addr) 0)

			 :argument-column
			   (+ (or *opcode-column-width*
				  (params-opcode-column-width params)
				  0)
			      (params-address-column-width params)
			      1
			      label-column-width)
			 :alignment
			   (params-instruction-alignment params)
			 :byte-order
			   (c:backend-byte-order (params-backend params))
			 )))

(defun set-dstate-segment (dstate base length)
  "Make the current segment in DSTATE to start at BASE and be LENGTH long"
  (let ((sap
	 (if (integerp base)
	     (system:int-sap (dstate-true-code-address dstate base))
	     base))
	(addr (if (integerp base) base (system:sap-int base))))
    (setf (dstate-segment-sap dstate) sap
	  (dstate-segment-start dstate) addr
	  (dstate-segment-length dstate) length)
    nil))

(defun disassemble-current-segment (dstate stream)
  "Disassemble the current memory segment in DSTATE to STREAM."
  (declare (type stream stream)
	   (type disassem-state dstate)
	   (type (member t nil) t))
  (let ((ispace (get-inst-space (dstate-params dstate))))
    (fresh-line stream)			; otherwise, was tabbing funny on the
					; first line

    (number-labels dstate)
    (rewind-current-segment dstate)

    (loop
      (let (offs)
	(loop
	  (let ((curpos (dstate-curpos dstate))
		(align (dstate-alignment dstate)))
	    (setf offs (- curpos (dstate-segment-start dstate)))
	    (when (>= offs (dstate-segment-length dstate))
	      ;; done!
	      (return-from disassemble-current-segment))

	    (with-matching-addresses (hook curpos (dstate-cur-hooks dstate))
	      (funcall hook stream))

	    (print-current-address stream dstate)

	    (when (aligned-p curpos align)
	      ;; stop trying to align things
	      (return))

	    (format stream "~a~vt~d~%" '.align
		    (dstate-argument-column dstate)
		    align)
	    (setf (dstate-curpos dstate) (align curpos align))
	    ;; now go around and try again!
	    ))

	(system:without-gcing
	 (check-for-moved-code dstate)

	 (cond ((at-fun-header-p dstate)
		(print-fun-header stream dstate)
		(incf (dstate-curpos dstate)
		      (to-bytes vm:function-header-code-offset)))
	       (t
		(let* ((chunk
			(sap-ref-dchunk (dstate-segment-sap dstate)
					offs
					(dstate-byte-order dstate)))
		       (inst (find-inst chunk ispace)))

		  (cond ((lra-p chunk dstate)
			 (princ '.lra stream)
			 (incf (dstate-curpos dstate) lra-size))
			((null inst)
			 (let ((alignment (dstate-alignment dstate)))
			   (multiple-value-bind (words bytes)
			       (truncate alignment vm:word-bytes)
			     (when (> words 0)
			       (print-words words stream dstate))
			     (when (> bytes 0)
			       (print-bytes bytes stream dstate)))
			   (incf (dstate-curpos dstate) alignment)))
			(t
			 (setf (dstate-nextpos dstate)
			       (+ (dstate-curpos dstate)
				  (format-length (inst-format inst))))

			 (print-inst chunk inst stream dstate)
			 (when (inst-control inst)
			   (funcall (inst-control inst)
				    chunk inst stream dstate))

			 (setf (dstate-curpos dstate)
			       (dstate-nextpos dstate))))))))

	(print-notes-and-newline stream dstate)
	(setf (dstate-output-state dstate) nil)
	))))

;;; ----------------------------------------------------------------

;;; just for fun
(defun print-fun-headers (function)
  (declare (type compiled-function function))
  (let* ((self (fun-self function))
	 (code (kernel:function-code-header self)))
    (format t "Code-header ~s: size: ~s, trace-table-offset: ~s~%"
	    code
	    (kernel:code-header-ref code vm:code-code-size-slot)
	    (kernel:code-header-ref code vm:code-trace-table-offset-slot))
    (do ((fun (kernel:code-header-ref code vm:code-entry-points-slot)
	      (fun-next fun)))
	((null fun))
      (let ((fun-offset (kernel:get-closure-length fun)))
	;; There is function header fun-offset words from the
	;; code header.
	(format t "Fun-header ~s at offset ~d: ~s~a => ~s~%"
		fun
		fun-offset
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-header-name-slot))
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-header-arglist-slot))
		(kernel:code-header-ref
		 code (+ fun-offset vm:function-header-type-slot)))))))

;;; ----------------------------------------------------------------
;;; stuff to use debugging-info to augment the disassembly

(defun code-function-map (code)
  (declare (type kernel:code-component code))
  (di::get-debug-info-function-map
   (kernel:code-header-ref code vm:code-debug-info-slot)))

(defstruct storage-class
  (name nil :type symbol)
  (locations #() :type (vector (or list fixnum)))
  )

(defstruct storage-info
  (classes nil :type list)		; alist of (name . sc-info)
  (debug-variables #() :type vector)
  )

(defun dstate-debug-variables (dstate)
  "Return the vector of debug-variables currently associated with DSTATE."
  (declare (type disassem-state dstate))
  (storage-info-debug-variables (dstate-storage-info dstate)))

(defun find-valid-storage-location (offset sc-name dstate)
  "Given the OFFSET of a location within the storage class SC-NAME (which is
  *not* the same as the compiler's SC), see if there's a current mapping to a
  source variable in DSTATE, and if so, return the offset of that variable in
  the current debug-variable vector."
  (declare (type fixnum offset)
	   (type symbol sc-name)
	   (type disassem-state dstate))
  (let* ((storage-info
	  (dstate-storage-info dstate))
	 (storage-class
	  (and storage-info
	       (cdr (assoc sc-name (storage-info-classes storage-info)))))
	 (currently-valid
	  (dstate-current-valid-locations dstate)))
    (and storage-class
	 (not (null currently-valid))
	 (let ((locations (storage-class-locations storage-class)))
	   (and (< offset (length locations))
		(let ((used-by (aref locations offset)))
		  (and used-by
		       (let ((debug-var-num
			      (typecase used-by
				(fixnum
				 (and (not
				       (zerop (bit currently-valid used-by)))
				      used-by))
				(list
				 (some #'(lambda (num)
					   (and (not
						 (zerop
						  (bit currently-valid num)))
						num))
				       used-by)))))
			 (and debug-var-num
			      (progn
				;; Found a valid storage reference!
				;; can't use it again until it's revalidated...
				(setf (bit (dstate-current-valid-locations dstate)
					   debug-var-num)
				      0)
				debug-var-num))
			 ))))))))

(defun copy-bit-vector (bvec)
  "This is stupid."
  (declare (type simple-bit-vector bvec))
  (let ((copy (bit-not bvec)))
    (bit-not copy copy)
    copy))

(defun grow-vector (vec new-len &optional initial-element)
  "Return a new vector which has the same contents as the old one VEC, plus
  new cells (for a total size of NEW-LEN).  The additional elements are
  initailized to INITIAL-ELEMENT."
  (declare (type vector vec)
	   (type fixnum new-len)) 
  (let ((new
	 (make-sequence `(vector ,(array-element-type vec) ,new-len)
			new-len
			:initial-element initial-element)))
    (dotimes (i (length vec))
      (setf (aref new i) (aref vec i)))
    new))

(defun storage-info-for-debug-function (debug-function dstate)
  "Returns a STORAGE-INFO struction describing the object-to-source
  variable mappings from DEBUG-FUNCTION."
  (declare (type di:debug-function debug-function)
	   (type disassem-state dstate))
  (let ((compiler-sc-vec
	 (c::backend-sc-numbers
	  (params-backend (dstate-params dstate))))
	(sc-sets
	 (params-storage-class-sets
	  (dstate-params dstate)))
	(classes
	 nil)
	(debug-variables
	 (di::debug-function-debug-variables debug-function)))
    (dotimes (debug-var-offset (length debug-variables))
      (let ((debug-var (aref debug-variables debug-var-offset)))
#+nil	(format t ";;; At offset ~d: ~s~%" debug-var-offset debug-var)
	(let* ((compiler-sc-offset
		(di::compiled-debug-variable-sc-offset debug-var))
	       (compiler-sc
		(aref compiler-sc-vec
		      (c:sc-offset-scn compiler-sc-offset)))
	       (compiler-sc-name
		(c:sc-name compiler-sc))
	       (set-name
		(car (find compiler-sc-name sc-sets
			   :test #'(lambda (name set)
				     (member name (cdr set)))))))
#+nil	  (format t ";;; CSC: ~s, SET: ~s[~d]~%" compiler-sc-name set-name
		  (c:sc-offset-offset compiler-sc-offset))
	  (unless (null set-name)
	    (let ((class (cdr (assoc set-name classes))))
	      (when (null class)
		(setf class (make-storage-class :name set-name))
		(push `(,set-name . ,class) classes))
	      (let* ((locations (storage-class-locations class))
		     (length (length locations))
		     (offset (c:sc-offset-offset compiler-sc-offset)))
		(when (>= offset length)
		  (setf locations
			(grow-vector locations
				     (max (* 2 length)
					  (1+ offset))
				     nil)
			(storage-class-locations class)
			locations))
		(let ((already-there (aref locations offset)))
		  (cond ((null already-there)
			 (setf (aref locations offset) debug-var-offset))
			((eql already-there debug-var-offset))
			(t
			 (if (listp already-there)
			     (pushnew debug-var-offset (aref locations offset))
			     (setf (aref locations offset)
				   (list debug-var-offset already-there)))))
		  )))))))
    (make-storage-info :classes classes
		       :debug-variables debug-variables)))

(defun source-available-p (debug-function)
  (handler-case
      (di:do-debug-function-blocks (block debug-function)
	(declare (ignore block))
	(return t))
    (di:no-debug-blocks () nil)))

(defun print-block-boundary (stream dstate)
  (let ((os (dstate-output-state dstate)))
    (when (not (eq os :beginning))
      (when (not (eq os :block-boundary))
	(terpri stream))
      (setf (dstate-output-state dstate)
	    :block-boundary))))

(defun source-tracking-hooks-for-debug-function (debug-function dstate &optional sfcache)
  "Return a set of hooks to track to track the source code from DEBUG-FUNCTION during
  disassembly.  SFCACHE can be either NIL or it can be a SOURCE-FORM-CACHE
  structure, in which case it is used to cache forms from files."
  (declare (type di:debug-function debug-function)
	   (type (or null source-form-cache) sfcache)
	   (type disassem-state dstate))
  (let ((last-block-pc -1)
	(hooks nil))
    (flet ((add-hook (pc fun)
	     (push (cons (dstate-code-insts-offs-address dstate pc) fun)
		   hooks)))
      (handler-case
	  (di:do-debug-function-blocks (block debug-function)
	    (let ((first-location-in-block-p t))
	      (di:do-debug-block-locations (loc block)
		(let ((pc (di::compiled-code-location-pc loc)))

		  ;; Put blank lines in at block boundaries
		  (when (and first-location-in-block-p
			     (/= pc last-block-pc))
		    (setf first-location-in-block-p nil)
		    (add-hook pc #'(lambda (stream)
				     (print-block-boundary stream dstate)))
		    (setf last-block-pc pc))

		  ;; Print out corresponding source; this information is not all
		  ;; that accurate, but it's better than nothing
		  (unless (zerop (di:code-location-form-number loc))
		    (multiple-value-bind (form new)
			(get-different-source-form loc 0 sfcache)
		      (when new
			 (let ((at-block-begin (= pc last-block-pc)))
			   (add-hook pc
				     #'(lambda (stream)
					 (when stream
					   (unless at-block-begin
					     (terpri stream))
					   (format stream ";;; [~d] "
						   (di:code-location-form-number loc))
					   (prin1-short form stream)
					   (terpri stream)
					   (terpri stream))))))))

		  ;; Keep track of variable live-ness as best we can
		  (let ((live-set
			 (copy-bit-vector
			  (di::compiled-code-location-live-set loc))))
		    (add-hook pc
			      #'(lambda (stream)
				  (declare (ignore stream))
				  (setf (dstate-current-valid-locations dstate)
					live-set)
      #+nil			    (note #'(lambda (stream)
					    (let ((*print-length* nil))
					      (format stream "Live set: ~s" live-set)))
					dstate))))
		  ))))
	(di:no-debug-blocks () nil)))
    (nreverse hooks)))

;;; ----------------------------------------------------------------

(defstruct (segment (:conc-name seg-)
		    (:print-function %print-segment))
  (start 0 :type integer)
  (length 0 :type fixnum)
  (debug-function nil :type (or null di:debug-function))
  (storage-info nil :type (or null storage-info))
  (hooks nil :type list)
  )

(defun %print-segment (seg stream level)
  (declare (ignore level))
  (format stream "#<Memory segment #x~x[~d]~@[ in ~s~]>"
	  (seg-start seg)
	  (seg-length seg)
	  (and (seg-debug-function seg)
	       (di:debug-function-name (seg-debug-function seg)))))

(defun create-segment (addr length debug-function dstate &optional sfcache)
  "Return a memory segment starting at ADDR and LENGTH bytes long, and using
  debugging information from DEBUG-FUNCTION unless it's NIL.  SFCACHE may be
  optionally supplied to improve access to the source-code."
  (let ((storage-info
	 (and debug-function
	      (storage-info-for-debug-function debug-function
					       dstate)))
	(hooks
	 (and debug-function
	      (source-tracking-hooks-for-debug-function debug-function
							dstate
							sfcache))))
    (when debug-function
      (let ((kind (di:debug-function-kind debug-function)))
	(flet ((anh (n)
	         (push (cons addr
			     #'(lambda (stream)
				 (declare (ignore stream))
				 (note n dstate)))
		       hooks)))
	  (case kind
	    (:external)
	    ((nil)
	     (anh "No-arg-parsing entry point"))
	    (t
	     (anh #'(lambda (stream)
		      (format stream "~s entry point" kind))))))))
    (make-segment :start addr
		  :length length
		  :debug-function debug-function
		  :storage-info storage-info
		  :hooks hooks)))

(defun get-function-segments (function dstate)
  "Returns a list of the segments of memory containing machine code
  instructions for FUNCTION.  DSTATE is used to translate from instruction
  offsets to addresses."
  (declare (type compiled-function function))
  (let* ((code (fun-code function))
	 (function-map (code-function-map code))
	 (fname (kernel:%function-header-name function))
	 (sfcache (make-source-form-cache)))
    (let ((first-block-seen-p nil)
	  (nil-block-seen-p nil)
	  (last-offset 0)
	  (last-debug-function nil)
	  (segments nil))
      (flet ((add-seg (offs len df)
	       (when (> len 0)
		 (push (create-segment
			(dstate-code-insts-offs-address dstate offs)
			len
			df
			dstate
			sfcache)
		       segments))))
	(dotimes (fmap-index (length function-map))
	  (let ((fmap-entry (aref function-map fmap-index)))
	    (etypecase fmap-entry
	      (integer
	       (when first-block-seen-p
		 (add-seg last-offset
			  (- fmap-entry last-offset)
			  last-debug-function)
		 (setf last-debug-function nil))
	       (setf last-offset fmap-entry))
	      (c::compiled-debug-function
	       (let ((name (c::compiled-debug-function-name fmap-entry))
		     (kind (c::compiled-debug-function-kind fmap-entry)))
#+nil	         (format t ";;; SAW ~s ~s ~s,~s ~d,~d~%"
				       name kind first-block-seen-p nil-block-seen-p
				       last-offset (c::compiled-debug-function-start-pc fmap-entry))
		 (cond (#+nil (eq last-offset fun-offset)
			      (and (equal name fname) (not first-block-seen-p))
			      (setf first-block-seen-p t))
		       ((eq kind :external)
			(when first-block-seen-p
			  (return)))
		       ((eq kind nil)
			(when nil-block-seen-p
			  (return))
			(when first-block-seen-p
			  (setf nil-block-seen-p t))))
		 (setf last-debug-function
		       (di::make-compiled-debug-function fmap-entry code))
		 )))))
	(let ((max-offset (code-inst-area-length code)))
	  (when (and first-block-seen-p last-debug-function)
	    (add-seg last-offset
		     (- max-offset last-offset)
		     last-debug-function))
	  (if (null segments)
	      (let ((offs (fun-offset function)))
		(make-segment
		 :start (dstate-code-insts-offs-address dstate offs)
		 :length (- max-offset offs)))
	      (nreverse segments)))))))

(defun get-code-segments (dstate &optional
				 (start (dstate-code-insts-addr dstate))
				 (length (code-inst-area-length (dstate-code dstate))))
  "Returns a list of the segments of memory containing machine code
  instructions for the code-component in DSTATE.  If START and/or LENGTH is
  supplied, only that part of the code-segment is used (but these are
  constrained to lie within the code-segment)."
  (declare (type disassem-state dstate)
	   (type integer start)
	   (type fixnum length))
  (let ((code-component (dstate-code dstate))
	(segments nil))
    (when code-component
      (let ((function-map (code-function-map code-component))
	    (sfcache (make-source-form-cache)))
	(let ((last-offset 0)
	      (last-debug-function nil))
	  (flet ((add-seg (offs len df)
			  (let* ((raw-addr (dstate-code-insts-offs-address dstate offs))
				 (addr (min (max start raw-addr) (+ start length)))
				 (len
				  (- (min (max start (+ raw-addr len)) (+ start length))
				     addr)))
			    (when (> len 0)
			      (push (create-segment addr len df dstate sfcache)
				    segments)))))
	    (dotimes (fmap-index (length function-map))
	      (let ((fmap-entry (aref function-map fmap-index)))
		(etypecase fmap-entry
		  (integer
		   (add-seg last-offset (- fmap-entry last-offset) last-debug-function)
		   (setf last-debug-function nil)
		   (setf last-offset fmap-entry))
		  (c::compiled-debug-function
		   (setf last-debug-function
			 (di::make-compiled-debug-function fmap-entry code-component)))
		  )))
	    (when last-debug-function
	      (add-seg last-offset
		       (- (code-inst-area-length code-component) last-offset)
		       last-debug-function))))))
    (if (null segments)
	(make-segment :start start :length length)
	(nreverse segments))))

;;; ----------------------------------------------------------------

#+nil
(defun find-function-segment (fun)
  "Return the address of the instructions for function and its length.
  The length is computed using a heuristic, and so may not be accurate."
  (declare (type compiled-function fun))
  (let* ((code
	  (fun-code fun))
	 (fun-addr
	  (- (kernel:get-lisp-obj-address fun)
	     vm:function-pointer-type))
	 (max-length
	  (code-inst-area-length code))
	 (upper-bound
	  (+ (code-inst-area-address code) max-length)))
    (do ((some-fun (code-first-function code)
		   (fun-next some-fun)))
	((null some-fun)
	 (values fun-addr (- upper-bound fun-addr)))
      (let ((some-addr (fun-address some-fun)))
	(when (and (> some-addr fun-addr)
		   (< some-addr upper-bound))
	  (setf upper-bound some-addr))))))

;;; ----------------------------------------------------------------

(defun label-segments (seglist dstate)
  "Computes labels for all the memory segments in SEGLIST and adds them to
  DSTATE.  It's important to call this function with all the segments you're
  interested in, so it can find references from one to another."
  (declare (type list seglist)
	   (type disassem-state dstate))
  (dolist (seg seglist)
    (set-dstate-segment dstate (seg-start seg) (seg-length seg))
    (compute-labels dstate)))

(defun disassemble-segment (segment stream dstate)
  "Disassemble the machine code instructions in SEGMENT to STREAM."
  (set-dstate-segment dstate (seg-start segment) (seg-length segment))
  (setf (dstate-storage-info dstate) (seg-storage-info segment))
  (setf (dstate-cur-hooks dstate)
	(sort (append (seg-hooks segment) (dstate-hooks dstate)) #'< :key #'car))
  (disassemble-current-segment dstate stream))

(defun disassemble-segments (segments stream dstate)
  "Disassemble the machine code instructions in each memory segment in
  SEGMENTS in turn to STREAM."
  (unless (null segments)
    (let ((first (car segments))
	  (last (car (last segments))))
      (set-address-printing-range dstate
				  (seg-start first)
				  (- (+ (seg-start last) (seg-length last))
				     (seg-start first)))
      (setf (dstate-output-state dstate) :beginning)
      (unless (and (seg-debug-function first)
		   (source-available-p (seg-debug-function first)))
	(format stream "~&;;; Not enough debugging information to find source code~%"))
      (dolist (seg segments)
	(disassemble-segment seg stream dstate)))))

;;; ----------------------------------------------------------------
;;; top-level functions

(defun disassemble-function (function &key (stream *standard-output*)
				      (use-labels t)
				      (backend c:*backend*))
  "Disassemble the machine code instructions for FUNCTION."
  (declare (type compiled-function function)
	   (type stream stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let* ((dstate
	  (system:without-gcing
	   (create-dstate (fun-code function)
			  (c:backend-disassem-params backend))))
	 (segments (get-function-segments function dstate)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun compile-function-lambda-expr (function)
  (declare (type function function))
  (multiple-value-bind
      (lambda closurep name)
      (function-lambda-expression function)
    (declare (ignore name))
    (when closurep
      (error "Cannot compile a lexical closure"))
    (compile nil lambda)))

(defun compiled-function-or-lose (thing &optional (name thing))
  (cond ((or (symbolp thing)
	     (and (listp thing)
		  (eq (car thing) 'lisp:setf)))
	 (compiled-function-or-lose (fdefinition thing) thing))
	((eval:interpreted-function-p thing)
	 (compile-function-lambda-expr thing))
	((functionp thing)
	 thing)
	((and (listp thing)
	      (eq (car thing) 'lisp::lambda))
	 (compile nil thing))
	(t
	 (error "Can't make a compiled function from ~S" name))))

(defun disassemble (object &optional (stream *standard-output*)
			   &key (use-labels t)
			   (backend c:*backend*))
  "Disassemble the machine code associated with OBJECT, which can be a
  function, a lambda expression, or a symbol with a function definition.  If
  it is not already compiled, the compiler is called to produce something to
  disassemble.  If STREAM is T, *STANDARD-OUTPUT* is used (so you can 
  use the keywords without having to type it!)."
  (declare (type (or function symbol cons) object)
	   (type (or (member t) stream) stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (disassemble-function (fun-self	; we can't detect closures, so
					; be careful
			 (compiled-function-or-lose object))
			:stream (if (eq stream t)
				    *standard-output*
				    stream)
			:use-labels use-labels
			:backend backend))

(defun disassemble-memory (address length
				   &key (stream *standard-output*)
				   code-component
				   (use-labels t) (backend c:*backend*))
  "Disassembles the given area of memory starting at ADDRESS and LENGTH long.
  Note that if CODE-COMPONENT is NIL and this memory could move during a GC,
  you'd better disable it around the call to this function."
  (declare (type (or integer system:system-area-pointer) address)
	   (type fixnum length)
	   (type stream stream)
	   (type (or null kernel:code-component) code-component)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let*	((dstate
	  (system:without-gcing
	   (create-dstate code-component
			  (c:backend-disassem-params backend))))
	 (segments
	  (if code-component
	      (get-code-segments dstate address length)
	      (list (make-segment :start address :length length)))))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

(defun disassemble-code-component (code-component &key
						  (stream *standard-output*)
						  (use-labels t)
						  (backend c:*backend*))
  "Disassemble the machine code instructions associated with
  CODE-COMPONENT (this may include multiple entry points)."
  (declare (type (or null kernel:code-component compiled-function) code-component)
	   (type stream stream)
	   (type (member t nil) use-labels)
	   (type c::backend backend))
  (let*	((dstate
	  (system:without-gcing
	   (create-dstate (if (functionp code-component)
			      (fun-code code-component)
			      code-component)
			  (c:backend-disassem-params backend))))
	 (segments
	  (get-code-segments dstate)))
    (when use-labels
      (label-segments segments dstate))
    (disassemble-segments segments stream dstate)))

;;; ----------------------------------------------------------------
;;; some handy function for machine-dependent code to use...

(defun note (note dstate)
  "Store NOTE (which can be either a string or a function with a single
  stream argument) to be printed as an end-of-line comment after the current
  instruction is disassembled."
  (declare (type (or string function) note)
	   (type disassem-state dstate))
  (push note (dstate-notes dstate)))

(defun prin1-short (thing stream)
  (with-print-restrictions
    (prin1 thing stream)))

(defun prin1-quoted-short (thing stream)
  (if (self-evaluating-p thing)
      (prin1-short thing stream)
      (prin1-short `',thing stream)))

(defun note-code-constant (byte-offset dstate)
  "Store a note about the lisp constant located BYTE-OFFSET bytes from the
  current code-component, to be printed as an end-of-line comment after the
  current instruction is disassembled."
  (declare (type fixnum byte-offset)
	   (type disassem-state dstate))
  (multiple-value-bind (const valid)
      (get-code-constant byte-offset dstate)
    (when valid
      (note #'(lambda (stream)
		(disassem:prin1-quoted-short const stream))
	    dstate))
    const))

(defun maybe-note-nil-indexed-symbol-slot-ref (nil-byte-offset dstate)
  "If the memory address located NIL-BYTE-OFFSET bytes from the constant NIL
  is a valid slot in a symbol, store a note describing which symbol and slot,
  to be printed as an end-of-line comment after the current instruction is
  disassembled.  Returns non-NIL iff a note was recorded."
  (declare (type fixnum nil-byte-offset)
	   (type disassem-state dstate))
  (multiple-value-bind (symbol access-fun)
      (grok-nil-indexed-symbol-slot-ref nil-byte-offset)
    (when access-fun
      (note #'(lambda (stream)
		(prin1 (if (eq access-fun 'symbol-value)
			   symbol
			   `(,access-fun ',symbol))
		       stream))
	    dstate))
    access-fun))

(defun maybe-note-nil-indexed-object (nil-byte-offset dstate)
  "If the memory address located NIL-BYTE-OFFSET bytes from the constant NIL
  is a valid lisp object, store a note describing which symbol and slot, to
  be printed as an end-of-line comment after the current instruction is
  disassembled.  Returns non-NIL iff a note was recorded."
  (declare (type fixnum nil-byte-offset)
	   (type disassem-state dstate))
  (let ((obj (get-nil-indexed-object nil-byte-offset)))
    (note #'(lambda (stream)
	      (prin1-quoted-short obj stream))
	  dstate)
    t))

(defun maybe-note-assembler-routine (address dstate)
  "If ADDRESS is the address of a primitive assembler routine, store a note
  describing which one, to be printed as an end-of-line comment after the
  current instruction is disassembled.  Returns non-NIL iff a note was
  recorded."
  (declare (type integer address)
	   (type disassem-state dstate))
  (let ((name (find-assembler-routine address)))
    (unless (null name)
      (note #'(lambda (stream)
		(format stream "#x~8,'0x: Primitive ~s" address name))
	    dstate))
    name))

(defun maybe-note-single-storage-ref (offset sc-name dstate)
  "If there's a valid mapping from OFFSET in the storage class SC-NAME to a
  source variable, make a note of the source-variable name, to be printed as
  an end-of-line comment after the current instruction is disassembled.
  Returns non-NIL iff a note was recorded."
  (declare (type fixnum offset)
	   (type symbol sc-name)
	   (type disassem-state dstate))
  (let ((storage-location
	 (find-valid-storage-location offset sc-name dstate)))
    (when storage-location
      (note #'(lambda (stream)
		(princ (di:debug-variable-symbol
			(aref (storage-info-debug-variables
			       (dstate-storage-info dstate))
			      storage-location))
		       stream))
	    dstate)
      t)))

(defun maybe-note-associated-storage-ref (offset sc-name assoc-with dstate)
  "If there's a valid mapping from OFFSET in the storage class SC-NAME to a
  source variable, make a note equating ASSOC-WITH with the source-variable
  name, to be printed as an end-of-line comment after the current instruction
  is disassembled.  Returns non-NIL iff a note was recorded."
  (declare (type fixnum offset)
	   (type symbol sc-name)
	   (type (or symbol string) assoc-with)
	   (type disassem-state dstate))
  (let ((storage-location
	 (find-valid-storage-location offset sc-name dstate)))
    (when storage-location
      (note #'(lambda (stream)
		(format stream "~a = ~s"
			assoc-with
			(di:debug-variable-symbol
			 (aref (dstate-debug-variables dstate)
			       storage-location))
		       stream))
	    dstate)
      t)))

;;; ----------------------------------------------------------------
;;; these should be somewhere else...

(defun get-error-name (errnum)
  (car (svref (c:backend-internal-errors c:*backend*) errnum)))


(defun get-sc-name (sc-offs)
  (c::location-print-name
   (c::make-random-tn :kind :normal
		      :sc (svref (c::backend-sc-numbers c:*backend*)
				 (c:sc-offset-scn sc-offs))
		      :offset (c:sc-offset-offset sc-offs))))

;;; ----------------------------------------------------------------

(defun handle-break-args (error-parse-fun stream dstate)
  "When called from an error break instruction's :DISASSEM-CONTROL (or
  :DISASSEM-PRINTER) function, will correctly deal with printing the
  arguments to the break.

  ERROR-PARSE-FUN should be a function that accepts:
    1) a SYSTEM-AREA-POINTER
    2) a BYTE-OFFSET from the SAP to begin at
    3) optionally, LENGTH-ONLY, which if non-NIL, means to only return
       the byte length of the arguments (to avoid unnecessary consing)
  It should read information from the SAP starting at BYTE-OFFSET, and return
  four values:
    1) the error number
    2) the total length, in bytes, of the information
    3) a list of SC-OFFSETs of the locations of the error parameters
    4) a list of the length (as read from the SAP), in bytes, of each of the
       return-values." 
  (declare (type function error-parse-fun)
	   (type (or null stream) stream)
	   (type disassem-state dstate))
  (multiple-value-bind (errnum adjust sc-offsets lengths)
      (funcall error-parse-fun
	       (dstate-segment-sap dstate)
	       (- (dstate-nextpos dstate) (dstate-segment-start dstate))
	       (null stream))
    (when stream
      (setf (dstate-curpos dstate)
	    (dstate-nextpos dstate))
      (flet ((emit-err-arg (note)
	       (let ((num (pop lengths)))
		 (print-notes-and-newline stream dstate)
		 (print-current-address stream dstate)
		 (print-bytes num stream dstate)
		 (incf (dstate-curpos dstate) num)
		 (when note
		   (note note dstate)))))
	(emit-err-arg nil)
	(emit-err-arg (symbol-name (get-error-name errnum)))
	(dolist (sc-offs sc-offsets)
	  (emit-err-arg (get-sc-name sc-offs))))
      )
    (incf (dstate-nextpos dstate) adjust)
    ))
