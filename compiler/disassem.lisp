;;; -*- Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/disassem.lisp,v 1.1 1991/11/15 15:25:37 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Machine independent disassembler for CMU Common Lisp
;;;
;;; Written by Miles Bader <miles@cogsci.ed.ac.uk>
;;;

(in-package 'disassem)

(export '(set-disassem-params
	  gen-inst-format-decl-form gen-field-type-decl-form gen-inst-decl-form
	  specialize
	  disassemble-memory
	  print-field field-suppressed-p
	  disassem-state dstate-curpos dstate-nextpos dstate-code
	  dstate-segment-start dstate-segment-length dstate-segment-sap
	  dstate-get-prop
	  inst inst-printer inst-name inst-format inst-id inst-mask
	  print-inst-using
	  get-code-constant
	  arg-value
	  sign-extend
	  note
	  print-notes-and-newline
	  print-current-address
	  print-bytes print-words
	  prin1-short
	  prin1-quoted-short
	  note-code-constant
	  maybe-note-nil-indexed-symbol-slot-ref
	  maybe-note-nil-indexed-object
	  maybe-note-assembler-routine
	  handle-break-args
	  ))

(import 'xp:*print-lines*)

;;; ----------------------------------------------------------------

(defun req () (error "Required argument missing"))

;;; ----------------------------------------------------------------

(defvar *opcode-column-width* 10
  "The width of the column in which instruction-names are printed.")
(defvar *note-column* 45
  "The column in which end-of-line comments for notes are started.")

;;; ----------------------------------------------------------------

(defstruct params
  (field-types (make-hash-table :test #'eq) :type hash-table)
  (inst-formats (make-hash-table :test #'eq) :type hash-table)
  (instructions (make-hash-table :test #'eq) :type hash-table)
  (inst-space nil :type (or null inst-space))
  (instruction-alignment vm:word-bytes :type fixnum)
  (address-column-width 8 :type fixnum)
  (backend (req) :type c::backend)	; for convenience
  )

(defmacro set-disassem-params (&key instruction-alignment address-size)
  "Specify global disassembler params for C:*TARGET-BACKEND*.  Currently
includes:

:INSTRUCTION-ALIGNMENT		Minimum alignment of instructions, in bytes.
:ADDRESS-SIZE			Size of a machine address, in bytes."
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
     nil))

;;; ----------------------------------------------------------------
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

(defmacro dchunk-or (to from)
  `(logior ,to ,from))
(defmacro dchunk-and (to from)
  `(logand ,to ,from))
(defmacro dchunk-clear (x y)
  `(logandc2 ,x ,y))
(defmacro dchunk-not (x)
  `(lognot ,x))

(defmacro dchunk-andf (to from)
  `(setf ,to (logand ,to ,from)))
(defmacro dchunk-orf (to from)
  `(setf ,to (logior ,to ,from)))
(defmacro dchunk-clearf (to from)
  `(setf ,to (logandc2 ,to ,from)))

(defmacro dchunk-make-mask (pos)
  `(mask-field ,pos -1))
(defmacro dchunk-make-field (pos value)
  `(dpb ,value ,pos 0))

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

(defmacro dchunk-extract (from pos)
  `(ldb ,pos ,from))

(defmacro dchunk-insertf (place pos value)
  `(setf ,place (dpb ,value ,pos ,place)))

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
  (if (logbitp (1- size) int)
      (dpb int (byte size 0) -1)
      int))

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
  (destructuring-bind (&key disassem-printer disassem-use-label sign-extend &allow-other-keys)
      options
    `(setf (gethash ',name (params-field-types (c:backend-disassem-params c:*target-backend*)))
	   (make-field-type :name ',name
			    :printer ,disassem-printer
			    :sign-extend ,sign-extend
			    :use-label ,disassem-use-label))))

(defun parse-field-type (name field-types)
  (or (gethash name field-types)
      (if (integer-typespec-p name)
	  name
	  (error "Field-type ~s not defined, and not a subtype of integer" name))))

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
  (ungrokable nil :type (member nil t))	;
  )

(defun %print-inst-format (format stream level)
  (declare (ignore level))
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
		`(make-inst-format-field :name ',name
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
		:DISASSEM-PRINTER 	default printer for this format."
  (multiple-value-bind (fields-form ungrokable)
      (gen-format-fields-maker-form field-specs bits)
    `(setf (gethash ',name (params-inst-formats (c:backend-disassem-params c:*target-backend*)))
	   (make-inst-format :name ',name
			     :length (truncate ,bits vm:byte-bits)
			     :fields ,fields-form
			     :printer ,(getf options :disassem-printer)
			     :ungrokable ,ungrokable))))

;;; ----------------------------------------------------------------

(defstruct (inst (:print-function %print-inst))
  (name nil :type (or symbol string))

  (mask dchunk-zero :type dchunk)	; bits in the inst that are constant
  (id dchunk-zero :type dchunk)		; value of those constant bits

  (format nil :type (or null inst-format))

  (printer nil :type (or list function))
  (control nil :type (or null function))
  (use-label nil :type (or null function))

  (specializers nil :type list)		; of inst
  (args nil :type list)			; of field-instance (these are the
					; non-constant fields)
  )

(defun %print-inst (inst stream level)
  (declare (ignore level))
  (format stream "#<Instruction ~s ~:a {~s}>"
	  (inst-name inst)
	  (mapcar #'(lambda (fi)
		      (let ((name (symbol-name (field-name (finst-field fi))))
			    (type (finst-type fi)))
			(if (field-type-p type)
			    (list name (ftype-name type))
			    name)))
		  (remove-if #'finst-same-as (inst-args inst)))
	  (format-name (inst-format inst))))
    
(defun format-field-or-lose (name format)
  (or (find name (format-fields format) :key #'field-name)
      (error "Unknown field ~s in instruction format ~a"
	     name
	     (format-name format))))

(defun parse-inst-field (field-spec inst-format field-types)
  (destructuring-bind (field-name &key constant type same-as mask inverse-function &allow-other-keys)
      field-spec
    (when (eq inverse-function #'identity)
      (setf inverse-function nil))
    (let* ((field (format-field-or-lose field-name inst-format))
	   (pos (field-pos field))
	   (same-as
	    (and same-as (format-field-or-lose same-as inst-format)))
	   (type
	    (and type (parse-field-type type field-types))))
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
       (and (not (and constant
		      (null type)
		      (null same-as)))
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
	  (let ((same-as-inst
		 (find (field-name (finst-same-as finst)) args
		       :key #'finst-name)))
	    (unless (null same-as-inst)
	      (setf change t)
	      (setf (finst-type finst) (finst-type same-as-inst)))))))))

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
		 (dchunk-insertf mask (field-pos field) -1))
		(default-type
		  (push (make-field-instance :field field
					     :type default-type)
			args))))))

    (values mask id args)))

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
	 (every #'(lambda (x) (all-printer-fields-in-format-p x format)) printer))
	(t
	 t)))

(defun find-choice-in-format (choices format)
  (dolist (choice choices
		  (error "No suitable choice for format ~s found in ~s" format choices))
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

(defun inst-field-const-value (inst field)
  "Returns the bit-pattern of FIELD in the instruction object INST."
  (dchunk-extract (inst-id inst) (field-pos field)))

(defun inst-field-const-p (inst field value)
  "Returns non-NIL if FIELD in the instruction object INST is constrained to
be the constant bit-pattern VALUE.  If VALUE is NIL, then non-NIL is returned
if it's constrained to be any constant at all."
  (and (= (integer-length (dchunk-extract (inst-mask inst) (field-pos field)))
	  (byte-size (field-pos field)))
       (or (null value)			; any constant will do
	   (= value
	      (inst-field-const-value inst field)))))

(defun inst-nfield-const-p (inst name value)
  "Returns non-NIL if the field called NAME within the instruction object
INST is constrained to be the constant bit-pattern VALUE.  If VALUE is NIL,
then non-NIL is returned if it's constrained to be any constant at all."
  (let ((field (find name (format-fields (inst-format inst)) :key #'field-name)))
    (when (null field)
      (error "Unknown field ~s in ~s" name inst))
    (inst-field-const-p inst field value)))

(defun inst-nfield-same-as-p (inst name other-field)
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
		      (cons-maybe-cat (string (inst-name inst)) accum))
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
		     ((eq (car printer) :unless)
		      (multiple-value-bind (result ok)
			  (handle-test-clause (cdr printer) nil)
			(if ok
			    result
			    accum)))
		     ((eq (car printer) :cond)
		      (dolist (clause (cdr printer)
				      (error "No test clause succeeds: ~s" printer))
			(multiple-value-bind (result ok)
			    (handle-test-clause clause t)
			  (when ok
			    (return result)))))
		     ((eq (car printer) :or)
		      (dolist (sub (cdr printer)
				   (error "No suitable result for ~s found in ~s" inst printer))
			(cond ((null sub)
			       (return nil))
			      (t
			       (let ((result (flatten sub accum)))
				 (unless (null result)
				   (return result)))))))
		     (t
		      (flatten (car printer) (flatten (cdr printer) accum)))))))
    (flatten printer nil)))
      
(defun filter-printer-for-inst (printer inst)
  "Takes a complicated conditionalized disassembly template PRINTER, and
returns a simple version customized for the instruction object INST,
containing only those things which PRINT-INST-USING can handle."
  (if (functionp printer)
      printer
      (flatten-printer (filter-printer-for-inst-format printer (inst-format inst)) inst)))

;;; ----------------------------------------------------------------

(defun gen-field-spec-forms (field-specs)
  `(list ,@(mapcar #'(lambda (fspec)
		       (destructuring-bind (name op arg
						 &key mask function
						 inverse-function type)
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
  (let ((inst-format (gethash format-name (params-inst-formats params)))
	(field-types (params-field-types params)))
    (when (null inst-format)
      (error "Unknown instruction format ~s (for instruction ~s)" format-name name))
    (multiple-value-bind (mask id args)
	(parse-inst-fields field-specs inst-format field-types)
      (let ((inst
	     (make-inst :name name
			:mask mask
			:id id
			:format inst-format
			:args args
			:control control
			:use-label use-label)))
	(unless (slow-reject-inst-p inst)
	  (propagate-same-as-types inst)
	  (setf (inst-printer inst)
		(filter-printer-for-inst (or printer
					     (format-printer (inst-format inst)))
					 inst))
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
				 (destructuring-bind (format-name &rest inst-field-specs)
				     flavor
				   (unless (fast-reject-inst-p flavor)
				     `(let ((inst
					     (create-inst ',inst-name
							  ',format-name
							  ,(gen-field-spec-forms inst-field-specs)
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
		     (push flav (gethash ',inst-name (params-instructions params))))
		  `(setf (gethash ',inst-name (params-instructions params)) insts))))))))

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
			(and (not (inst-field-const-p inst field nil))
			     (not
			      (inst-nfield-same-as-p inst
						     (field-name field)
						     nil)))))))
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
  (cond ((inst-field-const-p inst field nil)
	 (inst-field-const-p other-inst
			     field
			     (inst-field-const-value inst field)))
	((inst-nfield-same-as-p inst (field-name field) nil)
	 (inst-nfield-same-as-p other-inst
				(field-name field)
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
			    (make-field-instance :field field
						 :type (field-default-type field))))
		     (t
		      ;; we have to copy it, since it's currently shared with
		      ;; the old instruction
		      (setf (inst-args new-inst) (delete finst (inst-args new-inst)))
		      (setf finst (copy-field-instance finst))))

	       (push finst (inst-args new-inst))
	       (setf (finst-same-as finst)
		     (format-field-or-lose operand format))))))))

    ;; after any changes
    (setf (inst-printer new-inst)
	  (filter-printer-for-inst (inst-printer old-inst)
				   new-inst))

    new-inst))

(defun apply-specializations (inst specializations)
  (destructuring-bind (&key (disassem-printer (inst-printer inst))
			    (disassem-control (inst-control inst))
			    (name (inst-name inst)))
      specializations
    (setf (inst-printer inst) disassem-printer
	  (inst-control inst) disassem-control
	  (inst-name inst) name)
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
	  (push (apply-specializations (transmogrify-inst inst non-matching-specs)
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

(defstruct (inst-space (:conc-name ispace-))
  (valid-mask dchunk-zero :type dchunk)	; applies to *children*
  (choices nil :type (or list vector))
  )

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
			      (= (dchunk-extract chunk (field-pos (finst-field arg)))
				 (dchunk-extract chunk (field-pos same-as))))))
		    (inst-args spec))
	     (return spec)))
      inst))

(defun find-inst (chunk inst-space)
  "Returns the instruction object within INST-SPACE corresponding to the
bit-pattern CHUNK."
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
  (and (eq (inst-format special)
	   (inst-format general))
       (let ((smask (inst-mask special))
	     (gmask (inst-mask general)))
	 (and (dchunk= (inst-id general)
		       (dchunk-and (inst-id special) gmask))
	      (or (dchunk-strict-superset-p smask gmask)
		  (and (dchunk= smask gmask)
		       (let ((general-field-insts (inst-args general)))
			 (some #'(lambda (spec-arg)
				   (let ((general-arg
					  (find (finst-field spec-arg)
						general-field-insts
						:key #'finst-field)))
				     (and general-arg ; if not around, must be a
					; constant
					  (null (finst-same-as general-arg))
					  (or (finst-same-as spec-arg) ; *we* have a same-as constraint, but
					; general does not!
					      (field-type-specializes-p (finst-type spec-arg)
									(finst-type general-arg))))))
			       (inst-args special)))))))))

;;; a bit arbitrary, but should work ok...
(defun specializer-rank (inst)
  "Returns an integer corresponding to the specifivity of the instruction INST."
  (+ (* (dchunk-count-bits (inst-mask inst)) 4)
     (count-if-not #'null (inst-args inst) :key #'finst-same-as)))

(defun order-specializers (insts)
  "Order the list of instructions INSTS with more specific (more constant
bits, or same-as argument constains) ones first.  Returns the ordered list."
  (sort insts
	#'(lambda (i1 i2)
	    (> (specializer-rank i1) (specializer-rank i2)))))

(defun try-specializing (insts)
  "Given a list of instructions INSTS, Sees if one of these instructions is a
more general form of all the others, in which case they are put into its
specializers list, and it is returned.  Otherwise an error is signaled."
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
					   :subspace (build-inst-space (cdr bucket) submask)
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
			      ((inst-field-const-p inst (finst-field fi) nil)
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
		  (format t "~vt~8,'0x ==>~%" (+ 2 indent) (ischoice-common-id choice))
		  (print-inst-space (ischoice-subspace choice) (+ 4 indent)))
	      (ispace-choices inst-space)))))

;;; ----------------------------------------------------------------
;;; stuff used for printing

;;; All state during disassembly.  We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate- ))
  (curpos 0 :type integer)		; address of current instruction
  (nextpos 0 :type integer)		; address of next instruction

  (code nil)				; code object or nil
  (code-insts-addr 0 :type integer)	; address of instructions area (only
					; used for avoiding gc effects) in
					; the code object
  (code-insts-offset 0 :type fixnum)	; offset of instruction area from the
					; start of the code object

  (segment-start 0 :type integer)	; start of our instruction segment
  (segment-sap (req) :type system:system-area-pointer)
					; a sap pointing to our segment--
					; NOTE: this *may* be different from
					; segment-start!
  (segment-length 0 :type fixnum)	; length thereof

  (alignment vm:word-bytes :type fixnum) ; what to align to in most cases
  (byte-order :little-endian :type (member :big-endian :little-endian))

  (properties nil :type list)		; for user code to hang stuff off of

  (addr-print-len nil :type (or null fixnum)) ; used for prettifying printing
  (argument-column 0 :type fixnum)

  (labels nil :type list)		; alist of (address . name)
  (label-hash (make-hash-table) :type hash-table) ; same info in a different format
  (fun-header-offsets nil :type list)	; list of byte-offsets from code
  (pending-notes nil :type list)	; list of strings or functions to print

  (params (req) :type params)		; a handy pointer ...
  )

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
  (let* ((finst
	  (find name
		(inst-args inst)
		:key #'(lambda (fi)
			 (field-name (finst-field fi)))))
	 (field
	  (if finst
	      (finst-field finst)
	      (find name (format-fields (inst-format inst)) :key #'field-name)))) 

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
		    (princ (aref printer value)))
		   (t
		    (funcall printer value stream)))))
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
  (dolist (element printer)
    (etypecase element
      (string
       (write-string element stream))
      (symbol
       (if (eq element :tab)
	   (format stream "~vt" (dstate-argument-column dstate))
	   (print-field element chunk inst stream dstate)))
      (cons
       (if (eq (car element) 'quote)
	   (princ (cadr element) stream)
	   (error "Bogus element ~s to print-inst-using" element)))
      )))

(defun print-inst (chunk inst stream dstate)
  "Print a disassembled version of the instruction with bit-pattern CHUNK,
and corresponding to the instruction object INST to STREAM."
  (declare (type dchunk chunk)
	   (type inst inst)
	   (type stream stream)
	   (type disassem-state dstate))
  (let ((printer (inst-printer inst)))
    (unless printer
      (error "I don't know how to print ~s~" inst))
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

(defun aligned-p (num size)
  "Returns non-NIL if NUM is aligned on a SIZE byte boundary."
  (zerop (logand (1- size) num)))

(defun align (num size)
  "Return NUM aligned *upward* to a SIZE byte boundary."
  (logandc1 (1- size) (+ (1- size) num)))

;;; ----------------------------------------------------------------
;;; Routines to find things in the lisp environment.  Obviously highly
;;; implementation specific!

(defconstant groked-symbol-slots
  (sort `((,vm:symbol-value-slot . lisp::symbol-value)
	  (,vm:symbol-function-slot . lisp::symbol-function)
	  (,vm:symbol-raw-function-addr-slot . lisp::symbol-raw-function-addr)
	  (,vm:symbol-setf-function-slot . lisp::symbol-setf-function)
	  (,vm:symbol-plist-slot . lisp::symbol-plist)
	  (,vm:symbol-name-slot . lisp::symbol-name)
	  (,vm:symbol-package-slot . lisp::symbol-package))
	#'<
	:key #'car)
  "An alist of (SYMBOL-SLOT-OFFSET . ACCESS-FUNCTION-NAME) for slots in a
symbol object that we know about.")

(defun grok-symbol-slot-ref (address)
  "Given ADDRESS, try and figure out if which slot of which symbol is being
refered to.  Of course we can just give up, so it's not a big deal...
Returns two values, the symbol and the name of the access function of the
slot."
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
  (grok-symbol-slot-ref (+ nil-addr byte-offset)))

(defun get-nil-indexed-object (byte-offset)
  "Returns the lisp object located BYTE-OFFSET from NIL."
  (kernel:make-lisp-obj (+ nil-addr byte-offset)))

(defun get-code-constant (byte-offset dstate)
  "Returns two values; the lisp-object located at BYTE-OFFSET in the constant
area of the code-object in DSTATE and T, or NIL and NIL if there is no
code-object in DSTATE."
  (let ((code (dstate-code dstate)))
    (if code
	(values (kernel:code-header-ref code
					(ash (+ byte-offset vm:other-pointer-type)
					     (- vm:word-shift)))
		t)
	(values nil
		nil))))

(defvar *assembler-routines-by-addr* nil)

(defun find-assembler-routine (address)
  "Returns the name of the primitive lisp assembler routine located at
ADDRESS, or NIL if there isn't one."
  (when (null *assembler-routines-by-addr*)
    (setf *assembler-routines-by-addr* (make-hash-table))
    (maphash #'(lambda (name address)
		 (setf (gethash address *assembler-routines-by-addr*) name))
	     lisp::*assembler-routines*))
  (gethash address *assembler-routines-by-addr*))

;;; ----------------------------------------------------------------

(defun code-sap-offset (code sap)
  "Returns the offset from the beginning of the code-object CODE of SAP."
  (- (system:sap-int sap)
     (logandc1 vm:lowtag-mask (kernel:get-lisp-obj-address code))))

(defun make-sorted-fun-header-list (code)
  "Returns a sorted list of the addresses of function-headers in the
code-object CODE."
  (do ((fun (kernel:code-header-ref code vm:code-entry-points-slot)
	    (system:%primitive function-next fun))
       (fun-header-offsets nil))
      ((null fun)
       (sort fun-header-offsets #'<))
    (let ((fun-offset (kernel:get-closure-length fun)))
      ;; There is function header fun-offset words from the
      ;; code header.
      (push (to-bytes fun-offset) fun-header-offsets))))

(defun code-offset (dstate)
  "Returns the offset of the CURPOS in DSTATE from the beginning of
its code object.  Insensitive to GC."
  (+ (- (dstate-curpos dstate) (dstate-segment-start dstate))
     (- (system:sap-int (dstate-segment-sap dstate))
	(dstate-code-insts-addr dstate))
     (dstate-code-insts-offset dstate)))

(defun lra-p (chunk dstate)
  "Returns non-NIL if CHUNK is a valid LRA header in DSTATE."
  (and (aligned-p (dstate-curpos dstate) (* 2 vm:word-bytes))
       (let ((byte-offset (code-offset dstate)))
	 (= (dchunk-extract chunk
			    (correct-dchunk-bytespec-for-endianness
			     (byte vm:word-bits 0)
			     vm:word-bits
			     (dstate-byte-order dstate)))
	    (logior (ash (to-words byte-offset) vm:type-bits)
		    vm:return-pc-header-type)))))

(defun at-fun-header-p (dstate)
  "Returns non-NIL if DSTATE is currently pointing at a function header."
  (let ((header-offsets (dstate-fun-header-offsets dstate)))
    (and header-offsets
	 (= (code-offset dstate) (car header-offsets)))))

(defun print-fun-header (stream dstate)
  "Print the function-header (entry-point) pseudo-instruction at the current
location in DSTATE to STREAM."
  (pop (dstate-fun-header-offsets dstate))
  (let* ((code (dstate-code dstate))
	 (woffs (to-words (code-offset dstate)))
	 (name
	  (kernel:code-header-ref code (+ woffs vm:function-header-name-slot)))
	 (args
	  (kernel:code-header-ref code (+ woffs vm:function-header-arglist-slot)))
	 (type
	  (kernel:code-header-ref code (+ woffs vm:function-header-type-slot))))
    (format stream ".ENTRY ~s~:a" name args)
    (note #'(lambda (stream)
	      (format stream "~:s" type)) ; use format to print NIL as ()
	  dstate)))

(defun check-for-moved-code (dstate)
  "If the code object in DSTATE has moved since we last checked, make the sap
pointing to the start of the segment point to its corresponding location at
the new address."
  (let ((code (dstate-code dstate)))
    (when code
      (let ((old-code-insts-addr
	     (dstate-code-insts-addr dstate))
	    (new-code-insts-addr
	     (system:sap-int (kernel:code-instructions code))))
	(when (/= old-code-insts-addr
		  new-code-insts-addr)
	  (setf (dstate-segment-sap dstate)
		(system:int-sap
		 (+ new-code-insts-addr
		    (- (system:sap-int (dstate-segment-sap dstate))
		       old-code-insts-addr))))
	  (setf (dstate-code-insts-addr dstate)
		new-code-insts-addr))))))

;;; ----------------------------------------------------------------

(defun compute-labels (dstate)
  "Make an initial non-printing disassembly pass through DSTATE, noting any
addresses that are referenced by instructions that point within its segment,
and recording them in the LABEL-HASH (a hashtable mapping addresses to
label-names) and LABELS (an alist of (address . label-name) sorted by
address) fields of DSTATE."
  (let ((base-address (dstate-segment-start dstate))
	(length (dstate-segment-length dstate))
	(byte-order (dstate-byte-order dstate))
	(ispace (params-inst-space (dstate-params dstate)))
	(addrs nil))

    (setf (dstate-curpos dstate) (dstate-segment-start dstate))

    (loop
      (unless (aligned-p (dstate-curpos dstate) (dstate-alignment dstate))
	(setf (dstate-curpos dstate) (align (dstate-curpos dstate) (dstate-alignment dstate))))

      (let ((offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
	(when (>= offs (dstate-segment-length dstate))
	  ;; done!
	  (let ((label-count 0)
		(label-hash (dstate-label-hash dstate)))
	    (setf (dstate-labels dstate)
		  (mapcar #'(lambda (addr)
			      (let ((name (format nil "L~d" label-count)))
				(incf label-count)
				(setf (gethash addr label-hash) name)
				(cons addr name)))
			  (sort addrs #'<))))
	  (setf (dstate-pending-notes dstate) nil) ; just in case any got
						   ; left there by labeling
						   ; (they shouldn't but...)
	  (return))

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
		  (flet ((add-label (addr)
			   (unless (or (< addr base-address)
				       (>= addr (+ base-address length))
				       (find addr addrs :test #'=))
			     (push addr addrs))))

		    (cond ((lra-p chunk dstate)
			   (incf (dstate-curpos dstate) lra-size))
			  ((null inst)
			   (incf (dstate-curpos dstate))) ; let alignment fix it up
			  ((inst-use-label inst)
			   (add-label (funcall (inst-use-label inst)
					       chunk inst dstate)))
			  (t
			   (setf (dstate-nextpos dstate)
				 (+ (dstate-curpos dstate) (format-length (inst-format inst))))

			   (dolist (finst (inst-args inst))
			     (let ((field (finst-field finst))
				   (type (finst-type finst)))
			       (when (field-type-p type)
				 (let ((use-label (ftype-use-label type)))
				   (when use-label
				     (let* ((pos (field-pos field))
					    (value (dchunk-extract chunk pos)))
				       (when (ftype-sign-extend type)
					 (setf value (sign-extend value (byte-size pos))))

				       (let ((addr
					      (if (eq use-label t)
						  value
						  (funcall use-label value dstate))))
					 (add-label addr))))))))

			   (if (inst-control inst)
			       (funcall (inst-control inst) chunk inst nil dstate))

			   (setf (dstate-curpos dstate)
				 (dstate-nextpos dstate))
			   )))))))))))

;;; ----------------------------------------------------------------

(defun ensure-inst-space (params)
  "Get the instruction-space from PARAMS, creating it if necessary."
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

(defun print-current-address (stream dstate)
  "Print the current address in DSTATE to STREAM, plus any labels that
correspond to it, and leave the cursor in the instruction column."
  (let ((address (dstate-curpos dstate))
	(address-column-width
	 (params-address-column-width (dstate-params dstate)))
	(plen (dstate-addr-print-len dstate))
	(labels (dstate-labels dstate)))
    (when (null plen)
      (setf plen address-column-width)
      (setf (dstate-addr-print-len dstate)
	    ;; 4 bits per hex digit
	    (ceiling (integer-length
			  (logxor (dstate-segment-start dstate)
				  (+ (dstate-segment-start dstate)
				     (dstate-segment-length dstate))))
		     4)))
    (cond ((and labels (= (caar labels) address))
	   (format stream "~v,0t~v,'0x: ~5@<~a:~> "
		   (- address-column-width plen)
		   plen
		   (ldb (byte (* 4 plen) 0) address)
		   (cdar labels))
	   (pop (dstate-labels dstate)))
	  (t
	   (format stream "~v,0t~v,'0x:       "
		   (- address-column-width plen)
		   plen
		   (ldb (byte (* 4 plen) 0) address))))))

;;; ----------------------------------------------------------------

;;; If DSTATE isn't aligned, align it while printing an appropiate `.ALIGN'
;;; pseudo-instruction to STREAM and starting a new instruction line.  If
;;; this would put us past the end of the segment, don't print anything.
(defun maybe-align (stream dstate)
  (unless (aligned-p (dstate-curpos dstate) (dstate-alignment dstate))
    (let ((aligned-pos
	   (align (dstate-curpos dstate) (dstate-alignment dstate))))

      (setf (dstate-curpos dstate) aligned-pos)

      (unless (>= aligned-pos		; we might be done anyway
		  (+ (dstate-segment-start dstate)
		     (dstate-segment-length dstate)))
	(format stream ".ALIGN~vt~d" (dstate-argument-column dstate) (dstate-alignment dstate))
	(print-notes-and-newline stream dstate)
	(print-current-address stream dstate))
    t)))

(defmacro with-print-restrictions (&rest body)
  `(let ((*print-pretty* t)
	 (*print-lines* 2)
	 (*print-length* 4)
	 (*print-level* 3))
     ,@body))

(defun print-notes-and-newline (stream dstate)
  "Print a newline to STREAM, inserting any pending notes in DSTATE as
end-of-line comments.  If there is more than one note, a separate line will be
used for each one."
  (with-print-restrictions
    (dolist (note (dstate-pending-notes dstate))
      (format stream "~vt; " *note-column*)
      (etypecase note
	(string
	 (write-string note stream))
	(function
	 (funcall note stream)))
      (terpri))
    (fresh-line)
    (setf (dstate-pending-notes dstate) nil)))

(defun print-bytes (num stream dstate)
  "Disassemble NUM bytes to STREAM as simple `BYTE' instructions"
  (format stream "BYTE~vt" (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
    (dotimes (offs num)
      (unless (zerop offs)
	(write-string ", " stream))
      (format stream "#x~2,'0x" (system:sap-ref-8 sap (+ offs start-offs))))))

(defun print-words (num stream dstate)
  "Disassemble NUM machine-words to STREAM as simple `WORD' instructions"
  (maybe-align stream dstate)
  (format stream "WORD~vt" (dstate-argument-column dstate))
  (let ((sap (dstate-segment-sap dstate))
	(start-offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
    (dotimes (offs num)
      (unless (zerop offs)
	(write-string ", " stream))
      (format stream "#x~8,'0x" (system:sap-ref-32 sap (+ offs start-offs))))))

;;; ----------------------------------------------------------------

(defun create-dstate (base length code params)
  "Make a disassembler-state object for the area from BASE, LENGTH long,
corresponding to the code object CODE.  BASE may be either a sap or an
integer address; the call to this function should probably be done with GC
disabled, but if CODE is non-NIL, then it's safe to turn GCing back on 
again after it returns."
  (declare (type (or integer system:system-area-pointer) base)
	   (type fixnum length)
	   ;?? (type code code)
	   (type params params))
  (let ((sap (if (integerp base) (system:int-sap base) base))
	(addr (if (integerp base) base (system:sap-int base))))
    (make-disassem-state :code code
			 :fun-header-offsets
			   (and code (make-sorted-fun-header-list code))
			 :code-insts-offset
			   (if code (code-sap-offset code sap) 0)
			 :code-insts-addr
			   (if code
			       (system:sap-int (kernel:code-instructions code))
			       0)
			 :segment-sap sap
			 :segment-start addr
			 :segment-length length
			 :argument-column (+ (or *opcode-column-width* 0) 13)
			 :alignment (params-instruction-alignment params)
			 :byte-order (c:backend-byte-order (params-backend params))
			 :params params)))

(defun disassemble-segment (dstate stream &optional (use-labels t))
  "Disassemble the memory segment in DSTATE to STREAM.  If USE-LABELS is
non-NIL, calculate and print labels for branches, etc, instead of using raw
addresses-- this requires that two passes be made."
  (let ((ispace (ensure-inst-space (dstate-params dstate))))

    (when use-labels
      (compute-labels dstate))

    (fresh-line)			; otherwise, was tabbing funny on the
					; first line

    (setf (dstate-curpos dstate) (dstate-segment-start dstate))

    (loop
      (let ((offs (- (dstate-curpos dstate) (dstate-segment-start dstate))))
	(when (>= offs (dstate-segment-length dstate))
	  ;; done!
	  (return))

	(print-current-address stream dstate)

	(when (maybe-align stream dstate)
	  (setf offs (- (dstate-curpos dstate) (dstate-segment-start dstate)))
	  (when (>= offs (dstate-segment-length dstate))
	    ;; done!
	    (return)))

	(system:without-gcing
	 (check-for-moved-code dstate)

	 (cond ((at-fun-header-p dstate)
		(print-fun-header stream dstate)
		(incf (dstate-curpos dstate) (to-bytes vm:function-header-code-offset)))
	       (t
		(let* ((chunk
			(sap-ref-dchunk (dstate-segment-sap dstate)
					offs
					(dstate-byte-order dstate)))
		       (inst (find-inst chunk ispace)))

		  (cond ((lra-p chunk dstate)
			 (format stream ".LRA")
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
			       (+ (dstate-curpos dstate) (format-length (inst-format inst))))

			 (print-inst chunk inst stream dstate)
			 (when (inst-control inst)
			   (funcall (inst-control inst) chunk inst stream dstate))

			 (setf (dstate-curpos dstate)
			       (dstate-nextpos dstate))))))))

	(print-notes-and-newline stream dstate)))))

;;; ----------------------------------------------------------------
;;; top level calls... stolen from pmax-disassem.lisp

(defun compile-function-lambda-expr (function)
  (multiple-value-bind
      (lambda closurep name)
      (function-lambda-expression function)
    (declare (ignore name))
    (when closurep
      (error "Cannot compile lexical closure."))
    (compile nil lambda)))

;;; just for fun
(defun print-fun-headers (function)
  (declare (type compiled-function function))
  (let* ((self (system:%primitive function-self function))
	 (code (kernel:function-code-header self)))
    (format t "Code-header ~s: size: ~s, trace-table-offset: ~s~%"
	    code
	    (kernel:code-header-ref code vm:code-code-size-slot)
	    (kernel:code-header-ref code vm:code-trace-table-offset-slot))
    (do ((fun (kernel:code-header-ref code vm:code-entry-points-slot)
	      (system:%primitive function-next fun)))
	((null fun))
      (let ((fun-offset (kernel:get-closure-length fun)))
	;; There is function header fun-offset words from the
	;; code header.
	(format t "Fun-header ~s at offset ~d: ~s~a => ~s~%"
		fun
		fun-offset
		(kernel:code-header-ref code (+ fun-offset vm:function-header-name-slot))
		(kernel:code-header-ref code (+ fun-offset vm:function-header-arglist-slot))
		(kernel:code-header-ref code (+ fun-offset vm:function-header-type-slot)))))))

(defun fun-code (fun)
  (declare (type compiled-function fun))
  (kernel:function-code-header (system:%primitive function-self fun)))

(defun disassemble-function (function &key (stream *standard-output*)
				           (use-labels t)
					   (backend c:*backend*))
  (declare (type compiled-function function))
  (let* ((code (fun-code function))
	 (trace-table-offset
	  (kernel:code-header-ref code vm:code-trace-table-offset-slot))
	 (dstate
	  (system:without-gcing
	   (create-dstate (kernel:code-instructions code)
			  trace-table-offset
			  code
			  (c:backend-disassem-params backend)))))
    (disassemble-segment dstate stream use-labels)))

(defun compiled-function-or-lose (thing)
  (cond ((or (symbolp thing)
	     (and (listp thing)
		  (eq (car thing) 'lisp:setf)))
	 (let ((temp (fdefinition thing)))
	   (when (not (functionp temp))
	     (error "~s has no function" thing))
	   (if (eval:interpreted-function-p temp)
	       (compile-function-lambda-expr temp)
	       temp)))
	((eval:interpreted-function-p thing)
	 (compile-function-lambda-expr thing))
	((functionp thing)
	 thing)
	((and (listp thing)
	      (eq (car thing) 'lisp::lambda))
	 (compile nil thing))
	(t
	 (error "Invalid argument to disassemble: ~S"
		thing))))

(defun disassemble (object &optional (stream *standard-output*)
			   &key (use-labels t)
			        (backend c:*backend*))
  (disassemble-function (compiled-function-or-lose object)
			:stream stream
			:use-labels use-labels
			:backend backend))

(defun disassemble-memory (address length
				   &key (stream *standard-output*)
				   (use-labels t) (backend c:*backend*))
  "Disassembles the given area of memory starting at ADDRESS and LENGTH long.
Note that if this memory could move during a GC, you'd better disable it
around the call to this function."
  (disassemble-segment (create-dstate address
				      length
				      nil
				      (c:backend-disassem-params
				       backend))
		       stream
		       use-labels))

;;; ----------------------------------------------------------------
;;; some handy function for machine-dependent code to use...

(defun note (note dstate)
  (push note (dstate-pending-notes dstate)))

(defun prin1-short (thing stream)
  (with-print-restrictions
    (prin1 thing stream)))

(defun prin1-quoted-short (thing stream)
  (if (self-evaluating-p thing)
      (prin1-short thing stream)
      (prin1-short `',thing stream)))

(defun note-code-constant (byte-offset dstate)
  (multiple-value-bind (const valid)
      (get-code-constant byte-offset dstate)
    (when valid
      (note #'(lambda (stream)
		(disassem:prin1-quoted-short const stream))
	    dstate))
    const))

(defun maybe-note-nil-indexed-symbol-slot-ref (nil-byte-offset dstate)
  (multiple-value-bind (symbol access-fun)
      (grok-nil-indexed-symbol-slot-ref nil-byte-offset)
    (when access-fun
      (note #'(lambda (stream)
		(prin1 `(,access-fun ',symbol) stream))
	    dstate))
    access-fun))

(defun maybe-note-nil-indexed-object (nil-byte-offset dstate)
  (let ((obj (get-nil-indexed-object nil-byte-offset)))
    (note #'(lambda (stream)
	      (prin1-quoted-short obj stream))
	  dstate)
    t))

(defun maybe-note-assembler-routine (address dstate)
  (let ((name (find-assembler-routine address)))
    (unless (null name)
      (note #'(lambda (stream)
		(format stream "#x~8,'0x: Primitive ~s" address name))
	    dstate))
    name))

;;; ----------------------------------------------------------------
;;; these should be somewhere else...

(defun get-error-name (errnum)
  (kernel::error-info-name (svref kernel::*internal-errors* errnum)))

(defun get-sc-name (sc-offs)
  (c::location-print-name
   (c::make-random-tn :kind :normal
		      :sc (svref (c::backend-sc-numbers c:*backend*)
				 (c:sc-offset-scn sc-offs))
		      :offset (c:sc-offset-offset sc-offs))))

;;; ----------------------------------------------------------------

(defun handle-break-args (error-parse-fun stream dstate)
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
