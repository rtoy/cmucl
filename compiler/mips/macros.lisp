;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/macros.lisp,v 1.32 1990/05/31 00:17:33 wlott Exp $
;;;
;;;    This file contains various useful macros for generating MIPS code.
;;;
;;; Written by William Lott and Christopher Hoover.
;;; 

(in-package "C")

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
	`(inst addu ,n-dst ,n-src zero-tn)
	`(unless (location= ,n-dst ,n-src)
	   (inst addu ,n-dst ,n-src zero-tn)))))

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
    (ecase vm:target-byte-order
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


;;;; Three Way Comparison

(defmacro three-way-comparison (x y condition flavor not-p target temp)
  (once-only ((n-x x)
	      (n-y y)
	      (n-condition condition)
	      (n-flavor flavor)
	      (n-not-p not-p)
	      (n-target target)
	      (n-temp temp))
    `(progn
       (ecase ,n-condition
	 (:eq
	  (if ,n-not-p
	      (inst bne ,n-x ,n-y ,n-target)
	      (inst beq ,n-x ,n-y ,n-target)))
	 (:lt
	  (ecase ,n-flavor
	    (:unsigned
	     (inst sltu ,n-temp ,n-x ,n-y))
	    (:signed
	     (inst slt ,n-temp ,n-x ,n-y)))
	  (if ,n-not-p
	      (inst beq ,n-temp zero-tn ,n-target)
	      (inst bne ,n-temp zero-tn ,n-target)))
	 (:gt
	  (ecase ,n-flavor
	    (:unsigned
	     (inst sltu ,n-temp ,n-y ,n-x))
	    (:signed
	     (inst slt ,n-temp ,n-y ,n-x)))
	  (if ,n-not-p
	      (inst bne ,n-temp zero-tn ,n-target)
	      (inst beq ,n-temp zero-tn ,n-target))))
       (inst nop))))


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

(defmacro test-simple-type (register temp target not-p type-code)
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
	   ((or (= ,n-type-code vm:base-character-type)
		(= ,n-type-code vm:unbound-marker-type))
	    (simple-test-tag ,n-register ,n-temp ,n-target ,n-not-p
			     ,n-type-code vm:type-mask))
	   (t
	    (let* ((out-label (gen-label))
		   (not-other-label (if ,n-not-p ,n-target out-label)))
	      (simple-test-tag ,n-register ,n-temp not-other-label t
			       vm:other-pointer-type vm:lowtag-mask)
	      (load-type ,n-temp ,n-register (- vm:other-pointer-type))
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


;;;; Test-Special-Value

;;; ### We may want this.

#+nil
(defmacro test-special-value (reg temp value target not-p)
  "Test whether Reg holds the specified special Value (T, NIL, %Trap-Object).
  Temp is an unboxed register."
  (once-only ((n-reg reg)
	      (n-temp temp)
	      (n-value value)
	      (n-target target)
	      (n-not-p not-p))
    `(progn
       (inst xiu ,n-temp ,n-reg
	     (or (cdr (assoc ,n-value
			     `((t . ,',clc::t-16)
			       (nil . ,',clc::nil-16)
			       (%trap-object . ,',clc::trap-16))))
		 (error "Unknown special value: ~S." ,n-value)))
       (if ,n-not-p
	   (inst bnb :eq ,n-target)
	   (inst bb :eq ,n-target)))))


;;;; Error Code

(eval-when (compile load eval)
  (defun emit-error-break (kind code values)
    `((inst break ,kind)
      (inst byte ,code)
      ,@(mapcar #'(lambda (tn)
		    `(let ((tn ,tn))
		       (assert (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
		       (inst byte (tn-offset tn))))
		values)
      (inst byte 0)
      (align vm:word-shift))))

(defmacro error-call (error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vm:error-trap error-code values)))


(defmacro cerror-call (label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst b ,label)
     ,@(emit-error-break vm:cerror-trap error-code values)))

(defmacro generate-error-code (error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (error-code &rest values)
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
	   (cerror-call ,continue ,error-code ,@values)
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
