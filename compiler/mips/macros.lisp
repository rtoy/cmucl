;;; -*- Package: MIPS; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/macros.lisp,v 1.46 1991/11/09 02:37:41 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains various useful macros for generating MIPS code.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 

(in-package "MIPS")

;;; Handy macro for defining top-level forms that depend on the compile
;;; environment.

(defmacro expand (expr)
  (let ((gensym (gensym)))
    `(macrolet
	 ((,gensym ()
	    ,expr))
       (,gensym))))


;;; Instruction-like macros.

(defmacro move (dst src &optional (always-emit-code-p nil))
  "Move SRC into DST (unless they are location= and ALWAYS-EMIT-CODE-P
  is nil)."
  (once-only ((n-dst dst)
	      (n-src src))
    (if always-emit-code-p
	`(inst move ,n-dst ,n-src)
	`(unless (location= ,n-dst ,n-src)
	   (inst move ,n-dst ,n-src)))))

(defmacro def-mem-op (op inst shift load)
  `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
     `(progn
	(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag))
	,,@(when load '('(inst nop))))))
;;; 
(def-mem-op loadw lw word-shift t)
(def-mem-op storew sw word-shift nil)


(defmacro load-symbol (reg symbol)
  `(inst add ,reg null-tn (vm:static-symbol-offset ,symbol)))

(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
					  "LOAD-SYMBOL-"
					  (string slot))))
	     (storer (intern (concatenate 'simple-string
					  "STORE-SYMBOL-"
					  (string slot))))
	     (offset (intern (concatenate 'simple-string
					  "SYMBOL-"
					  (string slot)
					  "-SLOT")
			     (find-package "VM"))))
	 `(progn
	    (defmacro ,loader (reg symbol)
	      `(progn
		 (inst lw ,reg null-tn
		       (+ (vm:static-symbol-offset ',symbol)
			  (ash ,',offset vm:word-shift)
			  (- vm:other-pointer-type)))
		 (inst nop)))
	    (defmacro ,storer (reg symbol)
	      `(inst sw ,reg null-tn
		     (+ (vm:static-symbol-offset ',symbol)
			(ash ,',offset vm:word-shift)
			(- vm:other-pointer-type))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *backend*)
      (:little-endian
       `(inst lbu ,n-target ,n-source ,n-offset ))
      (:big-endian
       `(inst lbu ,n-target ,n-source (+ ,n-offset 3))))))


;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,function (- (ash vm:function-header-code-offset
					vm:word-shift)
				   vm:function-pointer-type))
     (inst j ,lip)
     (move code-tn ,function)))

(defmacro lisp-return (return-pc lip &key (offset 0) (frob-code t))
  "Return to RETURN-PC.  LIP is an interior-reg temporary."
  `(progn
     (inst addu ,lip ,return-pc
	   (- (* (1+ ,offset) vm:word-bytes) vm:other-pointer-type))
     (inst j ,lip)
     ,(if frob-code
	  `(move code-tn ,return-pc)
	  '(inst nop))))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (align vm:lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (loadw reg fp-tn offset))))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
	 ((control-stack)
	  (storew reg fp-tn offset))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
	      (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
	(sc-case ,n-stack
	  ((any-reg descriptor-reg)
	   (move ,n-reg ,n-stack))
	  ((control-stack)
	   (loadw ,n-reg fp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  `(pseudo-atomic (,temp-tn)
     (inst addu ,result-tn alloc-tn vm:other-pointer-type)
     (inst addu alloc-tn alloc-tn (vm:pad-data-block ,size))
     (inst li ,temp-tn (logior (ash (1- ,size) vm:type-bits) ,type-code))
     (storew ,temp-tn ,result-tn 0 vm:other-pointer-type)
     ,@body))


;;;; Three Way Comparison

(defun three-way-comparison (x y condition flavor not-p target temp)
  (ecase condition
    (:eq
     (if not-p
	 (inst bne x y target)
	 (inst beq x y target)))
    (:lt
     (ecase flavor
       (:unsigned
	(inst sltu temp x y))
       (:signed
	(inst slt temp x y)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target)))
    (:gt
     (ecase flavor
       (:unsigned
	(inst sltu temp y x))
       (:signed
	(inst slt temp y x)))
     (if not-p
	 (inst beq temp zero-tn target)
	 (inst bne temp zero-tn target))))
  (inst nop))


;;;; Simple Type Checking Macros

(defmacro simple-test-tag (register temp target not-p tag-type tag-mask)
  `(progn
     (unless (zerop ,tag-mask)
       (inst and ,temp ,register ,tag-mask))
     (inst xor ,temp ,temp ,tag-type)
     (if ,not-p
	 (inst bne ,temp zero-tn ,target)
	 (inst beq ,temp zero-tn ,target))
     (inst nop)))

(defmacro simple-test-simple-type (register temp target not-p type-code)
  "Emit conditional code that test whether Register holds an object with
  the tag specificed if Tag-Type.  Temp should be an unboxed register."
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p)
	      (n-type-code type-code))
    `(cond ((< ,n-type-code vm:lowtag-limit)
	    (simple-test-tag ,n-register ,n-temp ,n-target ,n-not-p
			     ,n-type-code lowtag-mask))
	   (t
	    ;; Nothing clever in this version.  Assume other-immediate
	    ;; type is already in register.
	    ;; 
	    (simple-test-tag ,n-temp ,n-temp ,n-target ,n-not-p
			     ,n-type-code type-mask)))))

(defmacro test-simple-type (register temp target not-p type-code
				     &key (lowtag 'vm:other-pointer-type))
  "Emit conditional code that test whether Register holds an object with
  the tag specificed if Tag-Type.  If the Tag-Type is a type for a heap
  object than the register is dereferencd and the heap object is
  checked.  Temp should be an unboxed register."
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p)
	      (n-type-code type-code))
    `(cond ((< ,n-type-code vm:lowtag-limit)
	    (simple-test-tag ,n-register ,n-temp ,n-target ,n-not-p
			     ,n-type-code vm:lowtag-mask))
	   ((or (= ,n-type-code vm:base-char-type)
		(= ,n-type-code vm:unbound-marker-type))
	    (simple-test-tag ,n-register ,n-temp ,n-target ,n-not-p
			     ,n-type-code vm:type-mask))
	   (t
	    (let* ((out-label (gen-label))
		   (not-other-label (if ,n-not-p ,n-target out-label)))
	      (simple-test-tag ,n-register ,n-temp not-other-label t
			       ,lowtag vm:lowtag-mask)
	      (load-type ,n-temp ,n-register (- ,lowtag))
	      (inst nop)
	      (simple-test-tag ,n-temp ,n-temp ,n-target ,n-not-p
			       ,n-type-code 0)
	      (emit-label out-label))))))


;;;; Hairy Type Checking Macros

(defun canonicalize-type-codes (type-codes &optional (shift 0))
  (unless type-codes (return-from canonicalize-type-codes nil))
  (let* ((type-codes (sort (remove-duplicates type-codes) #'<))
	 (canonical-type-codes nil)
	 (first-type-code (pop type-codes))
	 (last-type-code (ash first-type-code shift))
	 (range-start first-type-code)
	 (range-end nil))
    (dolist (type-code type-codes)
      (let ((shifted-type-code (ash type-code shift)))
	(cond ((= last-type-code (1- shifted-type-code))
	       (setf range-end type-code))
	      (t
	       (push (if range-end (cons range-start range-end) range-start)
		     canonical-type-codes)
	       (setf range-start type-code)
	       (setf range-end nil)))
	(setf last-type-code shifted-type-code)))
    (push (if range-end (cons range-start range-end) range-start)
	  canonical-type-codes)
    (nreverse canonical-type-codes)))

(defmacro hairy-test-tag (register temp target not-p tag-types tag-mask)
  (let ((in-label (gensym))
	(out-label (gensym)))
    (collect ((emit))
      (macrolet ((frob (value)
		   `(let ((diff (+ (- ,value) last-type-code)))
		      (unless (zerop diff)
			(emit `(inst add ,temp ,temp ,diff))
			(setf last-type-code ,value)))))
	(do* ((types tag-types (cdr types))
	      (type (car types) (car types))
	      (last-type-check-p (null (cdr types)) (null (cdr types)))
	      (last-type-code 0))
	     ((null types))
	  (cond ((consp type)
		 (let ((low (car type))
		       (high (cdr type)))
		   (frob low)
		   (emit `(inst bltz ,temp ,out-label))
		   (frob high)
		   (cond (last-type-check-p
			  (emit `(if ,not-p
				     (inst bgtz ,temp ,target)
				     (inst blez ,temp ,target))))
			 (t
			  (emit `(inst blez ,temp ,in-label))))))
		(t
		 (frob type)
		 (cond (last-type-check-p
			(emit `(if ,not-p
				   (inst bne ,temp zero-tn ,target)
				   (inst beq ,temp zero-tn ,target))))
		       (t
			(emit `(inst beq ,temp zero-tn ,in-label))))))))
      `(let* ((drop-through (gen-label))
	      (,in-label (if ,not-p drop-through ,target))
	      (,out-label (if ,not-p ,target drop-through)))
	 ,in-label			; squelch possible warning
	 ,out-label
	 (unless (zerop ,tag-mask)
	   (inst and ,temp ,register ,tag-mask))
	 ,@(emit)
	 (inst nop)
	 (emit-label drop-through)))))

(defmacro test-hairy-type (register temp target not-p &rest types)
  "Test-Hairy-Type Register Temp Target Not-P {Type | (Low-Type High-Type)}+
  
  Test whether Register holds a value with one of a specified union of
  type codes.  All low tag type codes will be checked first.  Then the
  pointer will be checked to see if it is an other-pointer-type type
  pointer in which case it will be dereferenced and the remaining type
  codes (the header word type codes) will be checked.  All of the
  type-code expressions are evaluated at macroexpand time.  Temp should
  be an unboxed register." 
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p))
    (unless types (error "Must specify at least one type."))
    ;; 
    ;; Partition the type codes.
    (collect ((low-tag-types)
	      (header-word-types))
      (dolist (type types)
	(let ((type (eval type)))
	  (cond ((< type vm:lowtag-limit)
		 (low-tag-types type))
		(t
		 (header-word-types type)))))
      
      (let ((low-tag-types (canonicalize-type-codes (low-tag-types)))
	    (header-word-types (canonicalize-type-codes
				(header-word-types) (- (1- lowtag-bits)))))
	;; 
	;; Generate code
	`(let* ((out-label (gen-label))
		(in-low-tag-label (if ,n-not-p out-label ,n-target))
		(not-other-label (if ,n-not-p ,n-target out-label)))
	   in-low-tag-label            ; may not be used -- squelch warning
	   not-other-label
	   ,@(when low-tag-types
	       (if header-word-types
		   `((hairy-test-tag ,n-register ,n-temp in-low-tag-label nil
				     ,low-tag-types vm:lowtag-mask))
		   `((hairy-test-tag ,n-register ,n-temp ,n-target ,n-not-p
				     ,low-tag-types vm:lowtag-mask))))
	   ,@(when header-word-types
	       `((simple-test-tag ,n-register ,n-temp not-other-label t
				  vm:other-pointer-type vm:lowtag-mask)
		 (load-type ,n-temp ,n-register (- vm:other-pointer-type))
		 (inst nop)
		 (hairy-test-tag ,n-register ,n-temp ,n-target ,n-not-p
				 ,header-word-types 0)))
	   (emit-label out-label))))))

(defmacro simple-test-hairy-type (register temp target not-p &rest types)
  "Test-Hairy-Type Register Temp Target Not-P {Type | (Low-Type High-Type)}+

  Test whether Register holds a value with one of a specified union of
  type codes.  The type codes must either be all low tag codes or all
  header word tag codes.  Each separately specified Type is matched, and
  also all types between a Low-Type and High-Type pair (inclusive) are
  matched.  All of the type-code expressions are evaluated at
  macroexpand time.  Temp should be an unboxed register."
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p))
    (unless types (error "Must specify at least one type."))
    ;; 
    ;; Partition the type codes.
    (collect ((low-tag-types)
	      (header-word-types))
      (dolist (type types)
	(cond ((< type vm:lowtag-limit)
	       (low-tag-types type))
	      (t
	       (header-word-types type))))
      (let ((low-tag-types (low-tag-types))
	    (header-word-types (header-word-types)))
	(cond ((and low-tag-types header-word-types)
	       (error "SIMPLE-TEST-HAIRY-TYPE cannot check both low tag ~
	       types and other-pointer-type tag types."))
	      (low-tag-types
	       `((hairy-test-tag ,n-register ,n-temp ,n-target ,n-not-p
				 ,(canonicalize-type-codes low-tag-types)
				 vm:lowtag-mask)))
	      (header-word-types
	       `(progn
		  (inst srl ,n-temp ,n-register vm:lowtag-bits)
		  (hairy-test-tag ,n-temp ,n-temp ,n-target ,n-not-p
				  ,(canonicalize-type-codes header-word-types)
				  vm:type-mask)))
	      (t
	       (error "Lost big.  Should not be here.")))))))


;;;; Error Code


(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
		   (make-array 16
			       :element-type '(unsigned-byte 8)
			       :fill-pointer 0
			       :adjustable t))))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
	 (progn
	   ,@body)
       (push ,var *adjustable-vectors*))))

(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
	  (when vop
	    (note-this-location vop :internal-error)))
	(inst break ,kind)
	(with-adjustable-vector (,vector)
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer (make-sc-offset (sc-number
							       (tn-sc tn))
							      (tn-offset tn))
					      ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))
	(align vm:word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop vm:error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst b ,label)
     ,@(emit-error-break vop vm:cerror-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (let ((continue (gensym "CONTINUE-LABEL-"))
	(error (gensym "ERROR-LABEL-")))
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (let ((,error (gen-label)))
	   (emit-label ,error)
	   (cerror-call ,vop ,continue ,error-code ,@values)
	   ,error)))))


;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
(defmacro pseudo-atomic ((ndescr-temp) &rest forms)
  (let ((label (gensym "LABEL-")))
    `(let ((,label (gen-label)))
       (inst and flags-tn flags-tn (logxor (ash 1 interrupted-flag) #Xffff))
       (inst or flags-tn flags-tn (ash 1 atomic-flag))
       ,@forms
       (inst and flags-tn flags-tn (logxor (ash 1 atomic-flag) #Xffff))
       (inst and ,ndescr-temp flags-tn (ash 1 interrupted-flag))
       (inst beq ,ndescr-temp zero-tn ,label)
       (inst nop)
       (inst break vm:pending-interrupt-trap)
       (emit-label ,label))))


