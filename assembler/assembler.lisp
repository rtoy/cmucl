;;; -*- Mode: Lisp; Package: Compiler; Log: clc.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; A user-level assembler for the ROMP.
;;; Written by Skef Wholey.
;;;
;;; This program processes a file of lispy assembly code and produces a Lisp
;;; FASL file.  It will be used primarily for coding the assembler support
;;; routines, but later might be used by applications programmers to speed up
;;; inner loops.
;;;
;;; The file to be assembled consists of Lisp forms.  Most forms are simply
;;; evaluated -- macro definitions, constant declarations, and things like that
;;; are made this way.  Three forms are treated specially:
;;;   Define-Miscop, which defines a miscop,
;;;   Define-Assembler-Routine, which defines a random piece of assembly code,
;;;   Defun-In-Assembly-Code, which defines a Lisp-callable function.
;;;
(in-package "COMPILER")

(export 'assemble-file)

(import '(lisp::define-fop lisp::fop-assembler-routine
			   lisp::fop-fixup-miscop-routine
			   lisp::fop-fixup-user-miscop-routine
			   lisp::fop-fixup-assembler-routine
			   lisp::fop-fun)
	(find-package 'compiler))

;;; %%% Unixfy external definitions, references
;;; %%% Possibly help enforce 8 character uniqueness lossage
;;; %%% Data blocks
;;; %%% Tied down things


(defvar *undefined-labels* ()
  "List of all undefined labels in the current file.")

(defvar *defined-labels* ()
  "List of all defined labels in the current file.")

(defparameter romp-assembler-version "1.0")

;;; The ROMP instruction set database for the user-level assembler.

;;; All the stuff we need to know about an instruction is held in a structure
;;; hanging off of its %Instruction-Info property.

(defstruct (romp-info (:print-function print-romp-info)
		      (:constructor make-romp-info (name format opcode)))
  name				; symbolic name
  format			; ROMP format
  opcode)			; numeric opcode

(defun print-romp-info (structure stream depth)
  (declare (ignore depth))
  (format stream "#<The ~S instruction>" (romp-info-name structure)))

(defmacro def-romp-instr (name format opcode)
  `(setf (get ',name '%instruction-info)
	 (make-romp-info ',name ',format ,opcode)))


;;; The half dozen "instruction formats" described in the ROMP architecture
;;; guide aren't really sufficient for our purposes -- we've added a couple.
;;; The formats are:
;;;	JI  4 bit opcode, 4 bit N, 8 bits jump offset
;;;     X   4 bit opcode, 3 register fields
;;;     DS  4 bit opcode, 4 bit N, 2 registers
;;;	R   8 bit opcode, 2 registers
;;;	R1  8 bit opcode, 4 bit N, 1 register
;;;	R2  8 bit opcode, 1 register, 4 bit N
;;;     BI  8 bit opcode, 1 register, 20 bits jump offset
;;;	BA  8 bit opcode, 24 bits absolute jump address
;;;	D   8 bit opcode, 2 registers, 16 bit N
;;;	SR  12 bit opcode, 1 register
;;;	SN  12 bit opcode, 4 bit N


;;; Storage Access instructions:

(def-romp-instr lcs  ds #x04)
(def-romp-instr lc   d  #xCE)
(def-romp-instr lhas ds #x05)
(def-romp-instr lha  d  #xCA)
(def-romp-instr lhs  r  #xEB)
(def-romp-instr lh   d  #xDA)
(def-romp-instr ls   ds #x07)
(def-romp-instr l    d  #xCD)
(def-romp-instr lm   d  #xC9)
(def-romp-instr tsh  d  #xCF)
(def-romp-instr stcs ds #x01)
(def-romp-instr stc  d  #xDE)
(def-romp-instr sths ds #x02)
(def-romp-instr sth  d  #xDC)
(def-romp-instr sts  ds #x03)
(def-romp-instr st   d  #xDD)
(def-romp-instr stm  d  #xD9)

;;; Address Computation instructions:

(def-romp-instr cal  d  #xC8)
(def-romp-instr cal16 d #xC2)
(def-romp-instr cau  d  #xD8)
(def-romp-instr cas  x  #x06)
(def-romp-instr ca16 r  #xF3)
(def-romp-instr inc  r2 #x91)
(def-romp-instr dec  r2 #x93)
(def-romp-instr lis  r2 #xA4)
 
;;; Branching instructions:

(def-romp-instr bala ba #x8A)
(def-romp-instr balax ba #x8B)
(def-romp-instr bali bi #x8C)
(def-romp-instr balix bi #x8D)
(def-romp-instr balr r  #xEC)
(def-romp-instr balrx r #xED)
(def-romp-instr jb   ji #x01)
(def-romp-instr bb   bi #x8E)
(def-romp-instr bbx  bi #x8F)
(def-romp-instr bbr  r1 #xEE)
(def-romp-instr bbrx r1 #xEF)
(def-romp-instr jnb  ji #x00)
(def-romp-instr bnb  bi #x88)
(def-romp-instr bnbx bi #x89)
(def-romp-instr bnbr r  #xE8)
(def-romp-instr bnbrx r  #xe9)

;;; Trap instructions:

(def-romp-instr ti   d  #xCC)
(def-romp-instr tgte r  #xBD)
(def-romp-instr tlt  r  #xBE)

;;; Move and Insert instructions.

(def-romp-instr mc03 r  #xF9)
(def-romp-instr mc13 r  #xFA)
(def-romp-instr mc23 r  #xFB)
(def-romp-instr mc33 r  #xFC)
(def-romp-instr mc30 r  #xFD)
(def-romp-instr mc31 r  #xFE)
(def-romp-instr mc32 r  #xFF)
(def-romp-instr mftb r  #xBC)
(def-romp-instr mftbil r2 #x9D)
(def-romp-instr mftbiu r2 #x9C)
(def-romp-instr mttb r  #xBF)
(def-romp-instr mttbil r2 #x9F)
(def-romp-instr mttbiu r2 #x9E)

;;; Arithmetic instructions.

(def-romp-instr a    r  #xE1)
(def-romp-instr ae   r  #xF1)
(def-romp-instr aei  d  #xD1)
(def-romp-instr ai   d  #xC1)
(def-romp-instr ais  r2 #x90)
(def-romp-instr abs  r  #xE0)
(def-romp-instr onec r  #xF4)
(def-romp-instr twoc r  #xE4)
(def-romp-instr c    r  #xB4)
(def-romp-instr cis  r2 #x94)
(def-romp-instr ci   d  #xD4)
(def-romp-instr cl   r  #xB3)
(def-romp-instr cli  d  #xD3)
(def-romp-instr exts r  #xB1)
(def-romp-instr s    r  #xE2)
(def-romp-instr sf   r  #xB2)
(def-romp-instr se   r  #xF2)
(def-romp-instr sfi  d  #xD2)
(def-romp-instr sis  r2 #x92)
(def-romp-instr d    r  #xB6)
(def-romp-instr m    r  #xE6)

;;; Logical instructions.

(def-romp-instr clrbl r2 #x99)
(def-romp-instr clrbu r2 #x98)
(def-romp-instr setbl r2 #x9B)
(def-romp-instr setbu r2 #x9A)
(def-romp-instr n     r  #xE5)
(def-romp-instr nilz  d  #xC5)
(def-romp-instr nilo  d  #xC6)
(def-romp-instr niuz  d  #xD5)
(def-romp-instr niuo  d  #xD6)
(def-romp-instr o     r  #xE3)
(def-romp-instr oil   d  #xC4)
(def-romp-instr oiu   d  #xC3)
(def-romp-instr x     r  #xE7)
(def-romp-instr xil   d  #xC7)
(def-romp-instr xiu   d  #xD7)
(def-romp-instr clz   r  #xF5)

;;; Shifting instructions.

(def-romp-instr sar   r  #xB0)
(def-romp-instr sari  r2 #xA0)
(def-romp-instr sari16 r2 #xA1)
(def-romp-instr sr    r  #xB8)
(def-romp-instr sri   r2 #xA8)
(def-romp-instr sri16 r2 #xA9)
(def-romp-instr srp   r  #xB9)
(def-romp-instr srpi  r2 #xAC)
(def-romp-instr srpi16 r2 #xAD)
(def-romp-instr sl    r  #xBA)
(def-romp-instr sli   r2 #xAA)
(def-romp-instr sli16 r2 #xAB)
(def-romp-instr slp   r  #xBB)
(def-romp-instr slpi  r2 #xAE)
(def-romp-instr slpi16 r2 #xAF)

;;; Special Purpose Register Manipulation instructions.

(def-romp-instr mtmq  sr #xB5A)
(def-romp-instr mfmq  sr #x96A)
(def-romp-instr mtcsr sr #xB5F)
(def-romp-instr mfcsr sr #x96F)
(def-romp-instr clrcb sn #x95F)
(def-romp-instr setcb sn #x97F)

;;; Execution Control instructions.

(def-romp-instr lps   d  #xD0)
(def-romp-instr svc   d  #xC0)

(defvar *produce-unixy-cruft* nil
  "If T, the LAP file will have stuff that should be legal food for a unixy
  assembler.")


;;; PRINT-FILE-HEADER prints assorted information at the start of an
;;; ascii output file.

(defun print-file-header (stream output-type input-namestring)
  (format stream
	  "~%;;; ~A output for file ~A."
	  output-type input-namestring)
  (format stream
	  "~%;;; Assembled by assembler version ~A.~%"
	  romp-assembler-version))

(defun assemble-file (input-pathname
		      &key (output-file t) (error-file t) (listing-file nil)
		           ((:unixy-lap-file *produce-unixy-cruft*) nil))
  "Assembles the file named by the Input-Pathname."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((*clc-input-stream*
	  (open (merge-pathnames input-pathname ".romp") :direction :input))
	 (*clc-fasl-stream*
	  (if output-file
	      (open (if (eq output-file t)
			(make-pathname :defaults input-pathname :type "fasl")
			output-file)
		    :if-exists :rename-and-delete
		    :direction :output
		    :element-type '(unsigned-byte 8))))
	 (error-file-stream
	  (if error-file
	      (open (if (eq error-file t)
			(make-pathname :defaults input-pathname :type "err")
			error-file)
		    :if-exists :rename-and-delete
		    :direction :output)))
	 (*clc-err-stream*
	  (if error-file
	      (make-broadcast-stream error-file-stream *standard-output*)
	      *standard-output*))
	 (*clc-lap-stream*
	  (if listing-file
	      (open (if (eq listing-file t)
			(make-pathname :defaults input-pathname
				       :type (if *produce-unixy-cruft*
						 "s"
						 "list"))
			listing-file)
		    :if-exists :rename-and-delete
		    :direction :output)))
	 (error-count 0)
	 (warning-count 0)
	 (assembly-won nil)
	 (input-namestring (namestring input-pathname))
	 (*defined-labels* ())
	 (*undefined-labels* ())
	 (*package* (find-package "COMPILER")))
    (unwind-protect
      (progn
	;; Initialize the files.
	(when output-file
	  (format *clc-fasl-stream*
		  "FASL FILE output from ~A~%" input-namestring)
	  (start-fasl-file)
	  (fasl-dump-cold-load-form '(in-package "COMPILER")))
	(if error-file
	    (print-file-header error-file-stream "Error" input-namestring))
	(if (and listing-file (not *produce-unixy-cruft*))
	    (print-file-header *clc-lap-stream* "Listing" input-namestring))
	;; All set up.  Let the festivities begin.	    
	(clc-mumble "~%Starting assembly of file ~S.~%" input-namestring)
	(assembler-loop)
	;; All done.  Let the post-mortems begin.
	(clc-mumble "~2%Finished assembly of file ~S." input-namestring)
	(clc-mumble "~%~S Errors, ~S Warnings." error-count warning-count)
	(dolist (label *defined-labels*)
	  (setq *undefined-labels*
		(delete label *undefined-labels* :test #'eq)))
	(if (> (length *undefined-labels*) 0)
	    (clc-mumble "~%~S Undefined labels in file ~S: ~{~%	~S~}."
			(length *undefined-labels*) input-namestring
			*undefined-labels*))
	(terpri)
	(setq assembly-won t))
      ;; Close files.  Unwind-Protect makes sure that these get closed even
      ;; if compilation is aborted.  If the assembly did not win, abort the fasl
      ;; file instead of writing out a whole lot of useless stuff.
      (close *clc-input-stream*)
      (when (streamp *clc-fasl-stream*)
	(terminate-fasl-file)
	(close *clc-fasl-stream* :abort (not assembly-won)))
      (when error-file-stream (close error-file-stream))
      (when (streamp *clc-lap-stream*) (close *clc-lap-stream*)))))

(defvar *unique-thing* '(*unique-thing*))

(defun assembler-loop ()
  (do ((form (read *clc-input-stream* nil *unique-thing*)
	     (read *clc-input-stream* nil *unique-thing*)))
      ((eq form *unique-thing*))
    (process-assembler-form form)))

(defun process-assembler-form (form)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((atom form))
	((eq (car form) 'define-miscop)
	 (process-define-miscop form))
	((eq (car form) 'define-user-miscop)
	 (process-define-user-miscop form))
	((eq (car form) 'define-assembler-routine)
	 (process-define-assembler-routine form))
	((eq (car form) 'defun-in-assembly-code)
	 (process-defun-in-assembly-code form))
	((macro-function (car form))
	 (process-assembler-form (macroexpand form)))
	((or (functionp (car form)) (special-form-p (car form)))
	 (eval form))
	(t
	 (eval form))))

;;; There are three kinds of labels:
;;;  Local labels, which are used within a miscop or routine.
;;;  External labels, which are used between routines.
;;;  Absolute labels, which might not be used at all.
;;;
;;; Labels appear as symbols in the code, and information about a label is
;;; stored on that symbol's property list.  The kind of label, either LOCAL,
;;; EXTERNAL, or ABSOLUTE, is store on the %LABEL-KIND property.  The location
;;; of the label, is stored on the %LABEL-LOCATION property.  For local labels,
;;; the location is the offset in halfwords from the beginning of the routine.
;;; The locations of external labels is left unresolved until load time.  The
;;; location of absolute labels the byte address of the code they address.
;;;
;;; Labels may be referenced in one of four ways:
;;;  JI, which means the reference is from a JI relative branch instruction.
;;;  BI, which means the reference is from a BI relative branch instruction.
;;;  BA, which means the reference is from a BA absolute branch instruction.
;;;  L, which means the reference is from a pair of CA instructions.
;;;
;;; When each routine is assembled and dumped, the locations of any external
;;; labels defined in it are also dumped.  The locations of any external labels
;;; it defines are dumped as well.  When all files making up the system are
;;; loaded, the routines are "linked" by resolving the external references.


(defvar *local-labels* ()
  "List of labels local to this routine.")

(defvar *external-labels* ()
  "List of external labels defined by this routine.  Each definition is of the
  form (Label . Location).")

(defvar *external-references* ()
  "List of external references made by this routine.  Each reference is of the
  form (Type Label Location).")

;;; Define-XXX-Label defines a label at the given location.

(defun define-local-label (label location)
  (declare (optimize (speed 3) (safety 0)))
  (when (get label '%label-kind)
    (clc-error "~S is already a defined label." label))
  (push label *local-labels*)
  (push label *defined-labels*)
  (setf (get label '%label-kind) 'local)
  (setf (get label '%label-location) location))

(defun define-external-label (label location)
  (declare (optimize (speed 3) (safety 0)))
  (when (get label '%label-kind)
    (clc-error "~S is already a defined label." label))
  (push (cons label location) *external-labels*)
  (push label *defined-labels*)
  (setf (get label '%label-kind) 'external)
  (setf (get label '%label-location) location))


;;; Reference-Label references a label in the given way.  If a location can
;;; sensibly be returned, it is.  Otherwise, 0 is returned and the reference
;;; is added to the list of references to be resolved at load time.  The
;;; Location parameter is the halfword offset in the current routine at
;;; which the reference is made.

(defun reference-label (label how location)
  (declare (optimize (speed 3) (safety 0)))
  (when (and (not (memq label *defined-labels*))
	     (not (memq label *undefined-labels*)))
    (push label *undefined-labels*))
  (case (get label '%label-kind)
    ((local external)			; treated the same at this point
     (case how
       ((bi ji)
	(get label '%label-location))
       ((ba l)
	(push `(,how ,label ,location)
	      *external-references*)
	0)))
    (absolute
     (case how
       ((bi ji)
	(push `(,how ,label ,location)
	      *external-references*)
	0)
       ((ba l)
	(get label '%label-location))))
    (T
     (push `(,how ,label ,location) *external-references*)
     0)))


;;; Short-Jump-P is used by the jump optimizer to determine if a branch can be
;;; turned into a Jump.  The Location of the branching instruction and the
;;; label that is the destination of the jump are given.  Note that if the thing
;;; is 128 words away, we'll be able to short jump to it after the halfword is
;;; optimizied out of the branching instruction.

(defun short-jump-p (location label)
  (declare (fixnum location))
  (case (get label '%label-kind)
    ((local external)
     (<= -128 (the fixnum (- (the fixnum (get label '%label-location))
			     location)) 128))
    (t
     nil)))


;;; Clean-Up-Labels nukes the properties of labels defined in the
;;; current routine.

(defun clean-up-labels ()
  (declare (optimize (speed 3) (safety 0)))
  (dolist (label *local-labels*)
    (remprop label '%label-kind)
    (remprop label '%label-location))
  (dolist (label *external-labels*)
    (remprop (car label) '%label-kind)
    (remprop (car label) '%label-location)))

;;; Process-XXX does a little special stuff and calls Assemble-Top-Level-List
;;; to spit out code.

(defun process-define-miscop (form)
  (let ((function-name (cadr form))
	(*local-labels* '())
	(*external-labels* '())
	(*external-references* '())
	(body (cddr form)))
    (define-external-label function-name 0)
    (unwind-protect
      (assemble-top-level-list function-name body)
      (clean-up-labels))
    (dump-fop 'lisp::fop-fixup-miscop-routine)
    (clc-mumble "~%~S assembled." function-name)))

(defun process-define-user-miscop (form)
  (let ((function-name (cadr form))
	(*local-labels* '())
	(*external-labels* '())
	(*external-references* '())
	(body (cddr form)))
    (define-external-label function-name 0)
    (unwind-protect
      (assemble-top-level-list function-name body)
      (clean-up-labels))
    (dump-fop 'lisp::fop-fixup-user-miscop-routine)
    (clc-mumble "~%~S assembled." function-name)))

(defun process-define-assembler-routine (form)
  (let ((function-name (cadr form))
	(*local-labels* '())
	(*external-labels* '())
	(*external-references* '())
	(body (cddr form)))
    (define-external-label function-name 0)
    (unwind-protect
      (assemble-top-level-list function-name body)
      (clean-up-labels))
    (dump-fop 'lisp::fop-fixup-assembler-routine)
    (clc-mumble "~%~S assembled." function-name)))

(defun process-defun-in-assembly-code (form)
  (declare (ignore form))
  (clc-error "Defun-In-Assembly-Code is not yet implemented."))

;;; Pass 1.  We just fly down the list, expanding macros and finding addresses
;;; of the labels.  The macroexpanded code is put into *pass1-list*.  Each
;;; element of that list is either a cons of the instruction's byte offset from
;;; the start of the routine and the instruction or a label.

(defvar *pass1-list* '())

;;; Assemble-Top-Level-List performs both passes, writing out the length in
;;; bytes of the function before anything else.  The Process-mumbles count on
;;; the length in bytes to be written out that way.

(defun assemble-top-level-list (function-name list)
  (let ((*pass1-list* '()))
    (do ((list list (cdr list))
	 (location 0))
	((null list)
	 (setq *pass1-list* (nreverse *pass1-list*))
	 (setq location (optimize-jumps location))
	 (dump-fop 'lisp::fop-assembler-routine)
	 (quick-dump-number (ash location 1) 4)
	 (pass2-top-level-list)
	 (let ((*hands-off-table* t))
	   (dump-fop 'lisp::fop-normal-load)
	   (dump-object function-name)
	   (dump-object *external-labels*)
	   (dump-object *external-references*)
	   (dump-fop 'lisp::fop-maybe-cold-load)))
      (setq location (assemble-one-instruction (car list) location)))))

;;; Symbols in the instruction list are labels -- keywords are external labels.
;;; Lists that don't begin with an instruction mnemonic are macroexpanded and
;;; expected to return a list of instructions.

(defun assemble-one-instruction (inst location)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((atom inst)
	 (if (keywordp inst)
	     (define-external-label inst location)
	     (define-local-label inst location))
	 (push inst *pass1-list*))
	(t
	 (let ((info (get (car inst) '%instruction-info)))
	   (cond (info
		  (push (cons location inst) *pass1-list*)
		  (case (romp-info-format info)
		    ((ji x ds r r1 r2 sr sn)
		     (setq location (+ location 1)))
		    ((bi ba d)
		     (setq location (+ location 2)))))
		 ((macro-function (car inst))
		  (dolist (inst (macroexpand inst))
		    (setq location (assemble-one-instruction inst location))))
		 (t
		  (clc-error "~S is a bad instruction list." inst))))))
  location)

;;; Jump optimizer.  We turn BI branches into JI branches if we can.  Currently
;;; we punt on the possibility that the halfword saved by optimizing a branch
;;; might make possible optimization of branches already processed, since such
;;; computation chews up a lot of time for relatively little gain.  We return
;;; the new number of halfwords in the *Pass1-List*.

(defun optimize-jumps (length)
  (declare (optimize (speed 3) (safety 0)))
  (do* ((list *pass1-list* (cdr list))
	(stuff (car list) (car list))
	(instp (consp stuff) (consp stuff))
	(location (if instp (car stuff)) (if instp (car stuff)))
	(inst (if instp (cdr stuff)) (if instp (cdr stuff))))
       ((null list) length)
    (when (and instp
	       (or (eq (car inst) 'bb) (eq (car inst) 'bnb))
	       (<= 8 (eval (cadr inst)) 15)
	       (short-jump-p location (caddr inst)))
      (setf (car inst) (cdr (assoc (car inst) '((bb . jb) (bnb . jnb)))))
      (setf (cadr inst) (- (cadr inst) 8))
      (setq length (1- length))
      (dolist (stiff (cdr list))
	(if (consp stiff)
	    (setf (car stiff) (1- (car stiff)))))
      (dolist (label *local-labels*)
	(let ((loc (get label '%label-location)))
	  (when (> loc location)
	    (setf (get label '%label-location) (1- loc)))))
      (dolist (label *external-labels*)
	(let ((loc (cdr label)))
	  (when (> loc location)
	    (setf (cdr label) (1- loc))
	    (setf (get (car label) '%label-location) (1- loc))))))))

;;; Pass 2.  We fly down the list created in pass 1, and interpret the
;;; instructions according to their format.  We spit the binary code out to the
;;; *Clc-Fasl-Stream*, and listing information out to *Clc-Lap-Stream* using
;;; Dump-Instruction.

(defun pass2-top-level-list ()
  (declare (optimize (speed 3) (safety 0)))
  (when *clc-lap-stream*
    (if *produce-unixy-cruft*
	(format *clc-lap-stream* "~2%~(~A~):" (unixfy function-name))
	(format *clc-lap-stream* "~2%                    ~S~%" function-name)))
  (dolist (stuff *pass1-list*)
    (if (atom stuff)
	(dump-instruction stuff)
	(let* ((location (car stuff))
	       (inst (cdr stuff))
	       (name (car inst))
	       (args (cdr inst))
	       (info (get name '%instruction-info))
	       (opcode (romp-info-opcode info)))
	  (case (romp-info-format info)
	    (ji
	     (let* ((n (car args))
		    (n-value (eval n))
		    (ji (cadr args))
		    (ji-value (reference-label ji 'ji location)))
	       (cond ((not (and n ji))
		      (clc-error "Bad JI format instruction: ~S." inst))
		     ((not (<= 0 n-value 7))
		      (clc-error "N field out of range in ~S." inst))
		     (t
		      (let ((distance (- ji-value location)))
			(unless (<= -128 distance 127)
			  (clc-error "Out of range JI branch in ~S." inst))
			(dump-instruction
			 inst (logior (ash opcode 3) n-value)
			      (logand distance 255)))))))
	    (x
	     (let* ((ra (car args))
		    (ra-value (eval-register ra))
		    (rb (cadr args))
		    (rb-value (eval-register rb))
		    (rc (caddr args))
		    (rc-value (eval-register rc)))
	       (cond ((not (and ra-value rb-value rc-value))
		      (clc-error "Too few or illegal registers specified in ~S."
				 inst))
		     (t
		      (dump-instruction
		       inst (logior (ash opcode 4) ra-value)
		            (logior (ash rb-value 4) rc-value))))))
	    (ds
	     (let* ((rb (car args))
		    (rb-value (eval-register rb))
		    (rc (cadr args))
		    (rc-value (eval-register rc))
		    (i (caddr args))
		    (i-value (eval i)))
	       (cond ((not (and i rb rc))
		      (clc-error "Bad DS format instruction: ~S." inst))
		     ((not (<= 0 i-value 15))
		      (clc-error "I field out of range in ~S." inst))
		     (t
		      (dump-instruction
		       inst (logior (ash opcode 4) i-value)
		            (logior (ash rb-value 4) rc-value))))))
	    (r
	     (let* ((rb (car args))
		    (rb-value (eval-register rb))
		    (rc (cadr args))
		    (rc-value (eval-register rc)))
	       (cond ((not (and rb-value rc-value))
		      (clc-error "Bad R format instruction: ~S" inst))
		     (t
		      (dump-instruction
		       inst opcode (logior (ash rb-value 4) rc-value))))))
	    (r1
	     (let* ((rb (car args))
		    (rb-value (eval rb))
		    (rc (cadr args))
		    (rc-value (eval-register rc)))
	       (cond ((not (and rb-value rc-value))
		      (clc-error "Bad R format instruction: ~S" inst))
		     (t
		      (dump-instruction
		       inst opcode (logior (ash rb-value 4) rc-value))))))
	    (r2
	     (let* ((rb (car args))
		    (rb-value (eval-register rb))
		    (rc (cadr args))
		    (rc-value (eval rc)))
	       (cond ((not (and rb-value rc-value))
		      (clc-error "Bad R format instruction: ~S" inst))
		     (t
		      (dump-instruction
		       inst opcode (logior (ash rb-value 4) rc-value))))))
	    (sr
	     (let* ((rb (car args))
		    (rb-value (eval-register rb)))
	       (cond ((not rb-value)
		      (clc-error "Bad S format instruction: ~S" inst))
		     (t
		      (dump-instruction
		       inst (ash opcode -4)
		            (logior (logand (ash opcode 4) 255) rb-value))))))
	    (sn
	     (let* ((rb (car args))
		    (rb-value (eval rb)))
	       (cond ((not rb-value)
		      (clc-error "Bad S format instruction: ~S" inst))
		     (t
		      (dump-instruction
		       inst (ash opcode -4)
		            (logior (logand (ash opcode 4) 255) rb-value))))))
	    (bi
	     (let* ((rb (car args))
		    (rb-value (eval-register rb))
		    (bi (cadr args))
		    (bi-value (reference-label bi 'bi location)))
	       (cond ((not (and rb-value bi))
		      (clc-error "Bad BI format instruction: ~S." inst))
		     (t
		      (let ((distance (- bi-value location)))
			(unless (<= -524288 distance 524287)
			  (clc-error "Out of range JI branch in ~S." inst))
			(dump-instruction
			 inst opcode
			      (logior (ash rb-value 4)
				      (logand (ash distance -16) 15))
		              (logand (ash distance -8) 255)
		              (logand distance 255)))))))
	    (ba
	     (let* ((ba (car args))
		    (ba-value (cond ((fixnump ba) ba)
				    ((and (listp ba)
					  (eq (car ba) 'symbol-value))
				     (symbol-value (cadr ba)))
				    (T (reference-label ba 'ba location)))))
	       (cond ((not ba)
		      (clc-error "Bad BA format instruction: ~S." inst))
		     (t
		      (dump-instruction inst opcode
					(logand (ash ba-value -16) 255)
					(logand (ash ba-value -8) 255)
					(logand ba-value 255))))))
	    (d
             (let* ((rb (car args))
		    (rb-value (eval-register rb))
		    (rc (cadr args))
		    (rc-value (eval-register rc))
		    (i (caddr args))
		    (i-value (eval i)))
	       (cond ((not (and i rb rc))
		      (clc-error "Bad D format instruction: ~S." inst))
		     ;; Sometimes I is sign extended, sometimes not.  Assume the
		     ;;  guy knows what he's doing.
		     ((not (<= -32768 i-value 65535))
		      (clc-error "I field out of range in ~S." inst))
		     (t
		      (dump-instruction
		       inst opcode
		            (logior (ash rb-value 4) rc-value)
		            (logand (ash i-value -8) 255)
		            (logand i-value 255))))))))))
  (when *clc-lap-stream*
    (terpri *clc-lap-stream*)))


;;; Dump-Instruction dumps out some bytes to the fasl file and a nice line to
;;; the listing file.

(defun dump-instruction (inst &rest bytes)
  (declare (optimize (speed 3) (safety 0)))
  (when *clc-lap-stream*
    (if *produce-unixy-cruft*
	(output-unixy-instruction inst)
	(if (atom inst)
	    (format *clc-lap-stream* "~%                    ~S" inst)
	    (if (cddr bytes)
		(format *clc-lap-stream*
			"~%~2,'0X ~2,'0X ~2,'0X ~2,'0X         ~S"
			(car bytes) (cadr bytes) (caddr bytes)
			(cadddr bytes) inst)
		(format *clc-lap-stream*
			"~%~2,'0X ~2,'0X               ~S"
			(car bytes) (cadr bytes) inst)))))
  (dolist (byte bytes)
    (dump-byte byte)))

;;; Compatability for silly Unixy assembler.

(defun output-unixy-instruction (inst)
  (declare (optimize (speed 3) (safety 0)))
  (if (atom inst)
      (format *clc-lap-stream* "~%~(~A~):" (unixfy inst))
      (let* ((name (car inst))
	     (args (cdr inst))
	     (info (get name '%instruction-info)))
	(case (romp-info-format info)
	  (ji
	   (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
		   (case name (jb 'bb) (jnb 'bnb))
		   (+ (eval (car args)) 8)
		   (unixfy (cadr args))))
	  (x
	   (format *clc-lap-stream* "~%~(	~A	~A,~A,~A~)"
		   name (unixfy (car args)) (unixfy (cadr args))
		   (unixfy (caddr args))))
	  (ds
	   (format *clc-lap-stream* "~%~(	~A	~A,~A(~A)~)"
		   (get-unixy-long-name name)
		   (unixfy (car args)) (eval (caddr args))
		   (unixfy (cadr args))))
	  (r
	   (if (eq name 'lhs)
	       (format *clc-lap-stream* "~%~(	~A	~A,0(~A)~)"
		       'lh (unixfy (car args)) (unixfy (cadr args)))
	       (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
		       name (unixfy (car args)) (unixfy (cadr args)))))
	  (r1
	   (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
		   name (eval (car args)) (unixfy (cadr args))))
	  (r2
	   (if (eq name 'cis) (setq name 'ci))
	   (cond ((memq name '(sari16 sri16 srpi16 sli16 slpi16))
		  (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			  (case name
			    (sari16 'sari)
			    (sri16 'sri)
			    (srpi16 'srpi)
			    (sli16 'sli)
			    (slpi16 'slpi))
			  (unixfy (car args)) (+ 16 (eval (cadr args)))))
		 ((memq name '(mftbil mttbil))
		  (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			  (case name
			    (mftbil 'mftbi)
			    (mttbil 'mttbi))
			  (unixfy (car args)) (- 32 (eval (cadr args)))))
		 ((memq name '(mftbiu mttbiu))
		  (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			  (case name
			    (mftbiu 'mftbi)
			    (mttbiu 'mttbi))
			  (unixfy (car args)) (- 16 (eval (cadr args)))))
		 (T (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			    name (unixfy (car args)) (eval (cadr args))))))
	  (bi
	   (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
		   name (unixfy (car args)) (unixfy (cadr args))))
	  (ba
	   (format *clc-lap-stream* "~%~(	~A	~A~)"
		   name (unixfy (car args))))
	  (d
	   (if (memq name '(lc lha lh l lm tsh stc sth st stm cal cal16 cau))
	       (format *clc-lap-stream* "~%~(	~A	~A,~A(~A)~)"
		       name (unixfy (car args)) (eval (caddr args))
		       (unixfy (cadr args)))
	       (if (memq name '(ci cli))
		   (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			   name (unixfy (cadr args)) (eval (caddr args)))
		   (format *clc-lap-stream* "~%~(	~A	~A,~A,~A~)"
			   name (unixfy (car args)) (unixfy (cadr args))
			   (eval (caddr args))))))
	  (sr
	   (cond ((memq name '(mtmq mfmq))
		  (format *clc-lap-stream* "~%~(	~A	~A,~A~)"
			  (if (eq name 'mtmq) 'mts 'mfs)
			  10 (unixfy (car args))))
		 (T (format *clc-lap-stream* "~%~(	~A	~A~)"
			    name (unixfy (car args))))))
	  (sn
	   (format *clc-lap-stream* "~%~(	~A	~A~)"
		   name (eval (car args))))))))

(defun unixfy (thing)
  (if (symbolp thing)
      (substitute #\_ #\-
		  (the string (copy-seq (the string (symbol-name thing)))))
      thing))

(defun get-unixy-long-name (name)
  (case name
    (ls 'l)
    (sts 'st)
    (lhas 'lha)
    (sths 'sth)
    (lcs 'lc)
    (stcs 'stc)))

;;; Nice branching instructions.

(defconstant condition-code-alist
  '((pz . 8) (lt . 9) (eq . 10) (gt . 11) (c0 . 12) (ov . 14) (tb . 15)
	     (<0 . 9) (=0 . 10) (>0 . 11)))

(defconstant not-condition-code-alist
  '((po . 8) (ge . 9)  (ne . 10)  (le . 11) (nc0 . 12) (nov . 14) (ntb . 15)
	     (>=0 . 9) (<>0 . 10) (<=0 . 11)))

(defmacro defbranch (name on on-not)
  `(defmacro ,name (destination &optional (condition 'po))
     (let ((cc (cdr (assq condition condition-code-alist))))
       (if cc
	   `((,',on ,cc ,destination))
	   (if (setq cc (cdr (assq condition not-condition-code-alist)))
	       `((,',on-not ,cc ,destination))
	       (error "Unknown condition code: ~S."))))))

(defbranch branch   bb   bnb)		; branch
(defbranch branchx  bbx  bnbx)		; branch with execute
(defbranch rbranch  bbr  bnbr)		; register branch
(defbranch rbranchx bbrx bnbrx)  	; register branch with execute

(defmacro b (destination) `((branch ,destination)))
(defmacro beq (destination) `((branch ,destination =0)))
(defmacro bne (destination) `((branch ,destination <>0)))
(defmacro blt (destination) `((branch ,destination <0)))
(defmacro bgt (destination) `((branch ,destination >0)))
(defmacro bge (destination) `((branch ,destination >=0)))
(defmacro ble (destination) `((branch ,destination <=0)))
(defmacro bx (destination) `((branchx ,destination)))
(defmacro beqx (destination) `((branchx ,destination =0)))
(defmacro bnex (destination) `((branchx ,destination <>0)))
(defmacro bltx (destination) `((branchx ,destination <0)))
(defmacro bgtx (destination) `((branchx ,destination >0)))
(defmacro bgex (destination) `((branchx ,destination >=0)))
(defmacro blex (destination) `((branchx ,destination <=0)))
(defmacro br (destination) `((rbranch ,destination)))
(defmacro breq (destination) `((rbranch ,destination =0)))
(defmacro brne (destination) `((rbranch ,destination <>0)))
(defmacro brlt (destination) `((rbranch ,destination <0)))
(defmacro brgt (destination) `((rbranch ,destination >0)))
(defmacro brge (destination) `((rbranch ,destination >=0)))
(defmacro brle (destination) `((rbranch ,destination <=0)))
(defmacro brx (destination) `((rbranchx ,destination)))
(defmacro breqx (destination) `((rbranchx ,destination =0)))
(defmacro brnex (destination) `((rbranchx ,destination <>0)))
(defmacro brltx (destination) `((rbranchx ,destination <0)))
(defmacro brgtx (destination) `((rbranchx ,destination >0)))
(defmacro brgex (destination) `((rbranchx ,destination >=0)))
(defmacro brlex (destination) `((rbranchx ,destination <=0)))

(defmacro bov (destination) `((branch ,destination ov)))
(defmacro bnov (destination) `((branch ,destination nov)))
(defmacro brov (destination) `((rbranch ,destination ov)))
(defmacro brnov (destination) `((rbranch ,destination nov)))
(defmacro bovx (destination) `((branchx ,destination ov)))
(defmacro bnovx (destination) `((branchx ,destination nov)))
(defmacro brovx (destination) `((rbranchx ,destination ov)))
(defmacro brnovx (destination) `((rbranchx ,destination nov)))

(defmacro btb (destination) `((branch ,destination tb)))
(defmacro bntb (destination) `((branch ,destination ntb)))
(defmacro brtb (destination) `((rbranch ,destination tb)))
(defmacro brntb (destination) `((rbranch ,destination ntb)))
(defmacro btbx (destination) `((branchx ,destination tb)))
(defmacro bntbx (destination) `((branchx ,destination ntb)))
(defmacro brtbx (destination) `((rbranchx ,destination tb)))
(defmacro brntbx (destination) `((rbranchx ,destination ntb)))

(defmacro bc0 (destination) `((branch ,destination c0)))
(defmacro bnc0 (destination) `((branch ,destination nc0)))
(defmacro brc0 (destination) `((rbranch ,destination c0)))
(defmacro brnc0 (destination) `((rbranch ,destination nc0)))
(defmacro bc0x (destination) `((branchx ,destination c0)))
(defmacro bnc0x (destination) `((branchx ,destination nc0)))
(defmacro brc0x (destination) `((rbranchx ,destination c0)))
(defmacro brnc0x (destination) `((rbranchx ,destination nc0)))

;;; Lr loads register1 with the contents of register2.  Nicer looking than
;;; cas r1,r2,0.

(defmacro lr (register1 register2)
  `((cas ,register1 ,register2 0)))

;;; Loadi loads the specified Constant into the given Register.

(defmacro loadi (register constant)
  (let ((value (eval constant)))
    (cond ((<= 0 value 15)
	   `((lis ,register ,value)))
	  ((<= -32768 value 32767)
	   `((cal ,register 0 ,value)))
	  ((= (logand value 65535) 0)
	   `((cau ,register 0 (logand (ash ,value -16) #xFFFF))))
	  (t
	   `((cau ,register 0 (logand (ash ,value -16) #xFFFF))
	     (oil ,register ,register (logand ,value 65535)))))))

(defmacro cmpi (register constant)
  (let ((value (eval constant)))
    (cond ((<= 0 value 15)
	   `((cis ,register ,value)))
	  ((<= -32768 value 32767)
	   `((ci 0 ,register ,value)))
	  (T (clc-error "~A is to big for a compare immediate instruction."
			value)))))

(defmacro defmemref (name ds d shift)
  `(defmacro ,name (register index-register &optional (offset 0))
     (let ((value (eval offset)))
       (cond ((and (eql (logand value (1- (ash 1 (abs ,shift)))) 0)
		   (<= 0 (ash value ,shift) ,(if (eq name 'loadh) 0 15)))
	      `((,',ds ,register ,index-register ,(ash value ,shift))))
	     (t
	      `((,',d ,register ,index-register ,value)))))))

(defmemref loadc lcs lc 0)		; loads a character or byte
(defmemref loadha lhas lha -1)		; loads a halfword, sign-extending
(defmemref loadh lhs lh -1)		; loads a halfword, no sign extend.
(defmemref loadw ls l -2)		; loads a fullword

(defmemref storec stcs stc 0)		; stores a character or byte
(defmemref storeha sths sth -1)		; stores a halfword
(defmemref storew sts st -2)		; stores a fullword

;;; (Mulitply Reg1 Reg2) multiplies the contents of Reg1 by Reg2 leaving the high order
;;; result in Reg1 and the low order result in Reg2.  This macro could try and play
;;; games, but to reduce the number of multiply steps, but it turns out that the extra
;;; logic becomes pretty hariy, and you end up gaining a small amount.

(defmacro multiply (Reg1 Reg2)
  `((mtmq ,Reg1)
    (s	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (m	,Reg1 ,Reg2)
    (mfmq ,Reg2)))

;;; More useful macros.

(defmacro noop ()
  `((cas NL0 NL0 NL0)))

(defmacro pushm (reg)
  `((inc cs 4)
    (sts ,reg cs 0)))

(defmacro popm (reg)
  `((ls ,reg cs 0)
    (dec cs 4)))

(defmacro verify-type (reg type error &optional ignore-nil)
  (let ((label (gensym "Label")))
    (if (or (eq type 'type-+-fixnum) (eq type 'type-negative-fixnum))
	`(,@(if (memq reg '(A0 A1))
		`((srpi16 ,reg ,type-shift-16)
		  (cmpi ,(if (eq reg 'A0) 'NL0 'NL1) ,type)
		  (,(if (eq type 'type-+-fixnum) 'bgt 'blt) ,error))
		`((lr NL0 ,reg)
		  (sri16 NL0 ,type-shift-16)
		  (cmpi NL0 ,type)
		  (,(if (eq type 'type-+-fixnum) 'bgt 'blt) ,error))))
	`(,@(if (and (eq type 'type-symbol) (null ignore-nil))
		`((xiu NL0 ,reg nil-16)
		  (beq ,label)))
	  ,@(if (memq reg '(A0 A1))
		`((srpi16 ,reg ,type-shift-16)
		  (cmpi ,(if (eq reg 'A0) 'NL0 'NL1) ,type)
		  (bne ,error))
		`((niuz NL0 ,reg type-mask-16)
		  (xiu NL0 NL0 (get-type-mask-16 ,type))
		  (bne ,error)))
	  ,@(if (and (eq type 'type-symbol) (null ignore-nil))
		(list label))))))

(defmacro verify-not-type (reg type error)
  (if (or (eq type 'type-+-fixnum) (eq type 'type-negative-fixnum))
      `(,@(if (memq reg '(A0 A1))
	      `((srpi16 ,reg ,type-shift-16)
		(cmpi ,(if (eq reg 'A0) 'NL0 'NL1) ,type)
		(,(if (eq type 'type-+-fixnum) 'bgt 'blt) ,error))
	      `((lr NL0 ,reg)
		(sri16 NL0 ,type-shift-16)
		(cmpi NL0 ,type)
		(,(if (eq type 'type-+-fixnum) 'bgt 'blt) ,error))))
      `(,@(if (memq reg '(A0 A1))
	      `((srpi16 ,reg ,type-shift-16)
		(cmpi ,(if (eq reg 'A0) 'NL0 'NL1) ,type)
		(beq ,error))
	      `((niuz NL0 ,reg type-mask-16)
		(xiu NL0 NL0 (get-type-mask-16 ,type))
		(beq ,error))))))

(defmacro test-nil (reg label)
  `((xiu NL0 ,reg nil-16)
    (beq ,label)))

(defmacro test-not-nil (reg label)
  `((xiu NL0 ,reg nil-16)
    (bne ,label)))

(defmacro test-t (reg label)
  `((xiu NL0 ,reg t-16)
    (beq ,label)))

(defmacro test-not-t (reg label)
  `((xiu ro ,reg t-16)
    (bne ,label)))

(defmacro test-trap (reg label)
  `((xiu NL0 ,reg trap-16)
    (beq ,label)))

(defmacro test-not-trap (reg label)
  `((xiu NL0 ,reg trap-16)
    (bne ,label)))

(defmacro get-type (reg type-reg)
  (cond ((and (eq reg 'A0) (eq type-reg 'NL0))
	 `((srpi16 A0 type-shift-16)))
	((and (eq reg 'A1) (eq type-reg 'NL1))
	 `((srpi16 A1 type-shift-16)))
	((and (eq reg 'A2) (eq type-reg 'A3))
	 `((srpi16 A2 type-shift-16)))
	(T `(,@(unless (eq reg type-reg)
		 `((cas ,type-reg ,reg 0)))
	     (sri16 ,type-reg type-shift-16)))))

(defmacro type-equal (reg type label)
  `((cmpi ,reg ,type)
    ,(cond ((eq type 'type-+-fixnum)
	    `(ble ,label))
	   ((eq type 'type-negative-fixnum)
	    `(bge ,label))
	   (T `(beq ,label)))))

(defmacro type-not-equal (reg type label)
  `((cmpi ,reg ,type)
    ,(cond ((eq type 'type-+-fixnum)
	    `(bgt ,label))
	   ((eq type 'type-negative-fixnum)
	    `(blt ,label))
	   (T `(bne ,label)))))

(defmacro error0 (error-code)
  `((bx error0)
    (loadi a0 ,error-code)))

(defmacro error1 (error-code reg)
  `(,@(unless (eq reg 'a1)
	`((cas a1 ,reg 0)))
    (bx error1)
    (loadi a0 ,error-code)))

(defmacro error2 (error-code reg1 reg2)
  `(,@(unless (eq reg2 'a2)
	`((cas a2 ,reg2 0)))
    ,@(unless (eq reg1 'a1)
	`((cas a1 ,reg1 0)))
    (bx error2)
    (loadi a0 ,error-code)))

;;; Floating Point support on the IBM RT PC.  The floating point support
;;; provided assumes that there is hardware support for floating point.
;;; This means the machine must have an FPA card, have an APC (which
;;; has an MC68881 chip on board), or an AFPA card.

;;; The macros defined below assume that there are only 7 floating point
;;; registers available to Lisp miscops.  On the FPA's all register numbers
;;; are shifted left one before being used, since the Mc68881 only has
;;; 8 registers.  On the FPA the register 14 and 15 are not useable which
;;; leaves 7 registers for Lisp.

(float-register FR0 0)
(float-register FR1 1)
(float-register FR2 2)
(float-register FR3 3)
(float-register FR4 4)
(float-register FR5 5)
(float-register FR6 6)
(float-register FR7 7)

;;; Floatop generates code that checks (at runtime) the type of
;;; floating point hardware available.  Shift-code is inserted between
;;; the load of the hardware-type and the comparison.  Floatop then
;;; branches to either the mc68881-code or the fpa-code.  The code for
;;; each type of hardware should normally return to Lisp code.  However,
;;; if the code should fall through, then the optional argument fall-through
;;; should be passed a non-NIL value.

(defmacro floatop (mc68881-code fpa-code &optional shift-code fall-through)
  (let ((tag (gensym "LABEL"))
	(tag2 (when fall-through (gensym "LABEL"))))
    `((cau A3 0 romp-data-base)
      (loadw A3 A3 floating-point-hardware-available)
      ,@shift-code
      (cmpi A3 float-mc68881)
      (bne ,tag)
      ,@mc68881-code
      ,@(when fall-through `((b ,tag2)))
      ,tag
      ,@fpa-code
      ,@(when fall-through `(,tag2)))))

;;; Macros to support Floating point operations on the IBM RT PC.
;;; The following code supports the FPA and AFPA.

(defconstant read-float-register #x0BC)
(defconstant read-status-register #x037)
(defconstant write-float-register #x094)
(defconstant write-status-register #x10F)
(defconstant convert-float-long-to-float-short #x016)
(defconstant convert-float-short-to-float-long #x01B)
(defconstant negate-float-short #x055)
(defconstant negate-float-long #x054)
(defconstant absolute-float-short #x075)
(defconstant absolute-float-long #x074)
(defconstant copy-float-short #x045)
(defconstant copy-float-long #x044)
(defconstant compare-float-short #x049)
(defconstant compare-float-long #x048)
(defconstant divide-float-short #x061)
(defconstant divide-float-long #x060)
(defconstant multiply-float-short #x071)
(defconstant multiply-float-long #x070)
(defconstant subtract-float-short #x051)
(defconstant subtract-float-long #x050)
(defconstant add-float-short #x041)
(defconstant add-float-long #x040)
(defconstant round-float-long-to-word #x023)
(defconstant truncate-float-long-to-word #x02B)
(defconstant floor-float-long-to-word #x03B)
(defconstant round-float-short-to-integer #x027)
(defconstant truncate-float-short-to-integer #x02F)
(defconstant floor-float-short-to-integer #x03F)
(defconstant convert-float-short-immediate-to-float-long #x21B)
(Defconstant convert-word-immediate-to-float-long #x203)
(defconstant convert-word-immediate-to-float-short #x207)
(defconstant compare-float-immediate-short #x249)
(defconstant divide-float-immediate-short #x261)
(defconstant divide-float-short-immediate #x161)
(defconstant multiply-float-immediate-short #x271)
(defconstant multiply-float-short-immediate #x171)
(defconstant subtract-float-immediate-short #x251)
(defconstant subtract-float-short-immediate #x151)
(defconstant add-float-immediate-short #x241)
(defconstant add-float-short-immediate #x141)
(defconstant convert-float-long-immediate-to-float-short #x216)
(defconstant compare-float-immediate-long #x248)
(defconstant divide-float-immediate-long #x260)
(defconstant divide-float-long-immediate #x160)
(defconstant multiply-float-immediate-long #x270)
(defconstant multiply-float-long-immediate #x170)
(defconstant subtract-float-immediate-long #x250)
(defconstant subtract-float-long-immediate #x150)
(defconstant add-float-immediate-long #x240)
(defconstant add-float-long-immediate #x140)
(defconstant afpa-atanl #x0D4)
(defconstant afpa-cosl #x0C2)
(defconstant afpa-expl #x0D8)
(defconstant afpa-log10l #x0DE)
(defconstant afpa-logl #x0DC)
(defconstant afpa-sinl #x0C0)
(defconstant afpa-sqrs #x065)
(defconstant afpa-sqrl #x064)
(defconstant afpa-tanl #x0C4)

(register float-status-register 14)

;;; Check-For-Float-Errors checks to make sure a floating point operation
;;; did not overflow or cause some other form of error.  The  argument
;;; Error-Routine is the label to branch to if an error occurred.  There
;;; is one routine for short, single, and long floating pointer errors
;;; respectively.  The argument Reg is a general register to use in
;;; the calculations.

(defmacro fpa-check-for-float-error (underflow overflow reg &optional divide)
  `((rdstr ,reg ,reg)			; get float status into register.
    (nilz ,reg ,reg #x7)		; Clear useless bits.
    (cmpi ,reg 1)			; Underflow ?
    (beq ,underflow)			; Yes, go generate error.
    (cmpi ,reg 2)			; Overflow ?
    (beq ,overflow)			; Yes, go generate error.
    ,@(if divide `((cmpi ,reg 3)	; Check for divide by 0.
		   (beq ,divide)))))

(defmacro invoke-fpa-float (opcode op1 op2 base
			       &optional (data-reg 'NL0) (data-op 'storew))
  (let* ((opcode (cond ((integerp opcode) opcode)
		       ((symbolp opcode) (symbol-value opcode))
		       (T (error "Illegal value: ~A." opcode))))
	 (high-op (logior #xFF00 (logand (ash opcode -6) #xF)))
	 (low-op (logior (ash (logand opcode #x3F) 10)
			 (ash (eval-register op1) 6)
			 (ash (eval-register op2) 2))))
    (declare (fixnum high-op low-op))
    (if (/= (logand low-op #x8000) 0)
	(setq high-op (1+ high-op)))
    `((cau ,base 0 ,high-op)
      (,data-op ,data-reg ,base ,low-op))))

(defmacro rdfr (gpr fpr &optional (base 'NL1))
  `((invoke-fpa-float ,read-float-register ,fpr 0 ,base ,gpr loadw)))

(defmacro rdstr (gpr &optional (base 'NL1))
  `((invoke-fpa-float ,read-status-register float-status-register
		  float-status-register ,base ,gpr loadw)))

(defmacro wtfr (gpr fpr &optional (base 'NL1))
  `((invoke-fpa-float ,write-float-register 0 ,fpr ,base ,gpr)))

(defmacro wtstr (gpr &optional (base 'NL1))
  `((invoke-fpa-float ,write-status-register float-status-register
		  float-status-register ,base ,gpr)))

(defmacro cisl (gr sfr lfr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-float-short-immediate-to-float-long
		  ,sfr ,lfr ,base ,gr)))

(defmacro csl (sfr lfr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-float-short-to-float-long ,sfr ,lfr ,base)))

(defmacro cls  (lfr sfr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-float-long-to-float-short ,lfr ,sfr ,base)))

(defmacro cils (gr lfr sfr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-float-long-immediate-to-float-short
		  ,lfr ,sfr ,base ,gr)))

(defmacro fixnum-to-short (ir sr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-word-immediate-to-float-short R0 ,sr ,base ,ir)))

(defmacro fixnum-to-long (ir lr &optional (base 'NL1))
  `((invoke-fpa-float ,convert-word-immediate-to-float-long R0 ,lr ,base ,ir)))

(defmacro coms (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,compare-float-short ,fr1 ,fr2 ,base)))

(defmacro comis (gr fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,compare-float-immediate-short ,fpr1 ,fpr2 ,base ,gr)))

(defmacro coml (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,compare-float-long ,fr1 ,fr2 ,base)))

(defmacro comil (gr fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,compare-float-immediate-long ,fpr1 ,fpr2 ,base ,gr)))

(defmacro cops (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,copy-float-short ,fr1 ,fr2 ,base)))

(defmacro copl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,copy-float-long ,fr1 ,fr2 ,base)))

(defmacro abss (fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,absolute-float-short ,fpr1 ,fpr2 ,base)))

(defmacro absl (fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,absolute-float-long ,fpr1 ,fpr2 ,base)))

(defmacro negs (fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,negate-float-short ,fpr1 ,fpr2 ,base)))

(defmacro negl (fpr1 fpr2 &optional (base 'NL1))
  `((invoke-fpa-float ,negate-float-long ,fpr1 ,fpr2 ,base)))

(defmacro adds (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-short ,fr1 ,fr2 ,base)))

(defmacro addis (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-immediate-short ,fr1 ,fr2 ,base ,gr)))

(defmacro addsi (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-short-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro addl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-long ,fr1 ,fr2 ,base)))

(defmacro addil (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-immediate-long ,fr1 ,fr2 ,base ,gr)))

(defmacro addli (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,add-float-long-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro subs (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-short ,fr1 ,fr2 ,base)))

(defmacro subis (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-immediate-short ,fr1 ,fr2 ,base ,gr)))

(defmacro subsi (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-short-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro subl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-long ,fr1 ,fr2 ,base)))

(defmacro subil (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-immediate-long ,fr1 ,fr2 ,base ,gr)))

(defmacro subli (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,subtract-float-long-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro muls (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-short ,fr1 ,fr2 ,base)))

(defmacro mulis (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-immediate-short ,fr1 ,fr2 ,base ,gr)))

(defmacro mulsi (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-short-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro mull (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-long ,fr1 ,fr2 ,base)))

(defmacro mulil (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-immediate-long ,fr1 ,fr2 ,base ,gr)))

(defmacro mulli (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,multiply-float-long-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro divs (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-short ,fr1 ,fr2 ,base)))

(defmacro divis (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-immediate-short ,fr1 ,fr2 ,base ,gr)))

(defmacro divsi (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-short-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro divl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-long ,fr1 ,fr2 ,base)))

(defmacro divil (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-immediate-long ,fr1 ,fr2 ,base ,gr)))

(defmacro divli (gr fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,divide-float-long-immediate ,fr1 ,fr2 ,base ,gr)))

(defmacro atanl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-atanl ,fr1 ,fr2 ,base)))

(defmacro cosl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-cosl ,fr1 ,fr2 ,base)))

(defmacro cosl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-cosl ,fr1 ,fr2 ,base)))

(defmacro expl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-expl ,fr1 ,fr2 ,base)))

(defmacro logl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-logl ,fr1 ,fr2 ,base)))

(defmacro sinl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-sinl ,fr1 ,fr2 ,base)))

(defmacro sqrs (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-sqrs ,fr1 ,fr2 ,base)))

(defmacro sqrl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-sqrl ,fr1 ,fr2 ,base)))

(defmacro tanl (fr1 fr2 &optional (base 'NL1))
  `((invoke-fpa-float ,afpa-tanl ,fr1 ,fr2 ,base)))

;;; The following code is to support the MC6881 floating point chip on
;;; the APC card.

;;; MC68881 opcodes.

(defconstant fop881-move #x00)
(defconstant fop881-int #x01)
(defconstant fop881-sinh #x02)
(defconstant fop881-intrz #x03)
(defconstant fop881-sqrt #x04)
(defconstant fop881-lognp1 #x06)
(defconstant fop881-etoxm1 #x08)
(defconstant fop881-tanh #x09)
(defconstant fop881-atan #x0A)
(defconstant fop881-asin #x0C)
(defconstant fop881-atanh #x0D)
(defconstant fop881-sin #x0E)
(defconstant fop881-tan #x0F)
(defconstant fop881-etox #x10)
(defconstant fop881-twotox #x11)
(defconstant fop881-tentox #x12)
(defconstant fop881-logn #x14)
(defconstant fop881-log10 #x15)
(defconstant fop881-log2 #x16)
(defconstant fop881-abs #x18)
(defconstant fop881-cosh #x19)
(defconstant fop881-neg #x1A)
(defconstant fop881-acos #x1C)
(defconstant fop881-cos #x1D)
(defconstant fop881-getexp #x1E)
(defconstant fop881-getman #x1F)
(defconstant fop881-div #x20)
(defconstant fop881-mod #x21)
(defconstant fop881-add #x22)
(defconstant fop881-mul #x23)
(defconstant fop881-sgldiv #x24)
(defconstant fop881-rem #x25)
(defconstant fop881-scale #x26)
(defconstant fop881-sglmul #x27)
(defconstant fop881-sub #x28)
(defconstant fop881-sincos #x30)
(defconstant fop881-cmp #x38)
(defconstant fop881-tst #x3A)

;;; Instruction classes.
(defconstant f881-freg-to-freg 0)
(defconstant f881-mem-to-freg 2)
(defconstant f881-freg-to-mem 3)
(defconstant f881-mem-to-scr 4)
(defconstant f881-scr-to-mem 5)

;;; When going to or from memory, have to specify type of the operand.
(defconstant f881-mem-integer 0)	;; 32 bit integer.
(defconstant f881-mem-single 1)		;; 32 bit float.
(defconstant f881-mem-double 5)		;; 64 bit float.
(defconstant f881-transfer-control-field-table
  '#(0 #x3c0000 0 #x3c0000 0 #x3c0000 0 #x3c0000))

;;; Control registers on the MC688881

(defconstant f881-fpsr 2)
(defconstant f881-fpcr 4)
(defconstant f881-fpiar 1)


(defmacro mc68881-check-for-error (under over opr baser &optional divide)
  `((f881op ,f881-scr-to-mem ,f881-fpsr 0 ,fop881-move 1 ,opr ,baser)
    (setcb 8)
    (loadw NL0 ,baser 0)
    (nilz ,opr NL0 #x800)
    (bne ,under)
    (nilz ,opr NL0 #x1000)
    (bne ,over)
    ,@(when divide
	`((nilz ,opr NL0 #x400)
	  (bne ,divide)))))

(defmacro f881op (class src dst operation length &optional (opr 'A2) (dtr 'NL1))
  (let* ((ecl (eval class))
	 (opcode (logior #xFC000000
			 (svref f881-transfer-control-field-table ecl)
			 (ash ecl 15)
			 (let ((r (eval-register src)))
			   (unless r
			     (setq r (eval src)))
			   (ash r 12))
			 (let ((r (eval-register dst)))
			   (unless r
			     (setq r (eval dst)))
			   (ash r 9))
			 (ash (eval operation) 2)
			 length))
	 (low (logand opcode #xFFFF))
	 (high (+ (logand (ash opcode -16) #xFFFF)
		  (if (eql (logand low #x8000) 0) 0 1))))
    `((cau ,opr 0 ,high)
      (storew ,dtr ,opr ,low))))

;;; Assumes NL0 contains the (now) single floating point number.
;;; Returns result in A0 as well as returning to caller.

(defmacro short-monadic-f881op (op &optional (opr 'A2) (base 'NL1))
  `((loadi ,base mc68881-float-temporary)
    (storew NL0 ,base 0)
    (f881op ,f881-mem-to-freg ,f881-mem-single FR0 ,(symbol-value op)
	    1 ,opr ,base)
    (f881op ,f881-freg-to-mem ,f881-mem-single FR0 ,fop881-move 1 ,opr ,base)
    (setcb 8)
    (loadw NL0 ,base 0)
    (sri NL0 short-float-shift-16)
    (brx PC)
    (oiu A0 NL0 short-float-4bit-mask-16)))

;;; Assumes A0 contains a pointer to a long floating point number.
;;; Allocates storage to hold the result of the computation.

(defmacro long-monadic-f881op (op &optional (opr 'A2) (base 'NL1))
  `((cal ,base A0 long-float-high-data)
    (f881op ,f881-mem-to-freg ,f881-mem-double FR0 ,(symbol-value op)
	    2 ,opr ,base)
    (allocate A0 type-long-float long-float-size ,base NL0)
    (cal ,base A0 long-float-high-data)
    (f881op ,f881-freg-to-mem ,f881-mem-double FR0 ,fop881-move 2 ,opr ,base)
    (setcb 8)
    (br PC)))

;;; Assumes NL0 and NL1 contain the first and second number respectively.

(defmacro short-dyadic-f881op (op type1 type2
				  &optional (opr 'A2) (base 'A3) divide)
  (let ((t1 (case type1
	      (integer f881-mem-integer)
	      (short-float f881-mem-single)
	      (T type1)))
	(t2 (case type2
	      (integer f881-mem-integer)
	      (short-float f881-mem-single)
	      (T type2)))
	(fr (if (float-register-p type1) type1 'FR6)))
    `((loadi ,base ,mc68881-float-temporary)
      (loadi ,opr 0)
      (storew ,opr ,base 0)
      (f881op ,f881-mem-to-scr ,f881-fpsr 0 ,fop881-move 1 ,opr ,base)
      ,@(when (null (float-register-p type1))
	  `((storew NL0 ,base 0)
	    (f881op ,f881-mem-to-freg ,t1 ,fr ,fop881-move 1 ,opr ,base)))
      ,@(if (null (float-register-p type2))
	    `((storew NL1 ,base 4)
	      (inc ,base 4)
	      (f881op ,f881-mem-to-freg ,t2 ,fr ,(symbol-value op)
		      1 ,opr ,base))
	    `((f881op ,f881-freg-to-freg ,t2 ,fr ,(symbol-value op)
			     0 ,opr ,base)))
      (f881op ,f881-freg-to-mem ,f881-mem-single ,fr ,fop881-move 1 ,opr ,base)
      (setcb 8)
      (inc ,base 4)
      (mc68881-check-for-error short-float-underflow short-float-overflow
			       ,opr ,base ,divide)
      (loadw NL0 ,base -4)
      (sri NL0 short-float-shift-16)
      (brx PC)
      (oiu A0 NL0 short-float-4bit-mask-16))))

;;; Assumes A0 contains the first number, and A1 the second.  Type1 and
;;; type2 specify the type of the first and second number respectively.
;;; Allocates storage to hold the result of the computation.

(defmacro long-dyadic-f881op (op type1 type2
				 &optional (opr 'A2) (base 'NL1) divide)
  (let ((l1 (if (eq type1 'long-float) 2 1))
	(l2 (if (eq type2 'long-float) 2 1))
	(t1 (case type1
	      (integer f881-mem-integer)
	      (short-float f881-mem-single)
	      (long-float f881-mem-double)
	      (T type1)))
	(t2 (case type2
	      (integer f881-mem-integer)
	      (short-float f881-mem-single)
	      (long-float f881-mem-double)
	      (T type2)))
	(fr (if (float-register-p type1) type1 'FR6)))
    `(,@(when (null (float-register-p type1))
	  `(,@(case type1
		(integer `((loadi ,base ,mc68881-float-temporary)
			   (storew A0 ,base 0)))
		(short-float `((loadi ,base ,mc68881-float-temporary)
			       (slpi A0 short-float-shift-16)
			       (storew NL0 ,base 0)))
		(long-float `((cal ,base A0 long-float-high-data))))
	      (f881op ,f881-mem-to-freg ,t1 ,fr ,fop881-move ,l1 ,opr ,base)))
	,@(case type2
	    (integer `((loadi ,base mc68881-float-temporary)
		       (storew A1 ,base 4)
		       (inc ,base 4)))
	    (short-float `((loadi ,base mc68881-float-temporary)
			   (lr NL0 A1)
			   (sli NL0 short-float-shift-16)
			   (storew NL0 ,base 4)
			   (inc ,base 4)))
	    (long-float `((cal ,base A1 long-float-high-data))))
	,@(if (null (float-register-p type2))
	      `((f881op ,f881-mem-to-freg ,t2 ,fr ,(symbol-value op)
			,l2 ,opr ,base))
	      `((f881op ,f881-freg-to-freg ,t2 ,fr ,(symbol-value op)
			       0 ,opr ,base)))
	(loadi ,base mc68881-float-temporary)
	(mc68881-check-for-error long-float-underflow long-float-overflow
				 ,opr ,base ,divide)
	(allocate A0 type-long-float long-float-size ,base NL0)
	(cal ,base A0 long-float-high-data)
	(f881op ,f881-freg-to-mem ,f881-mem-double ,fr ,fop881-move
		2 ,opr ,base)
	(setcb 8)
	(br PC))))
