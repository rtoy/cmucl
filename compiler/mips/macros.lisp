;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/macros.lisp,v 1.20 1990/02/28 18:25:17 wlott Exp $
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


(defmacro nop ()
  "Emit a nop."
  '(inst sll zero-tn zero-tn 0))

(defmacro move (dst src &optional (always-emit-code-p nil))
  "Move SRC into DST (unless they are location= and ALWAYS-EMIT-CODE-P
  is nil)."
  (once-only ((n-dst dst)
	      (n-src src))
    (if always-emit-code-p
	`(inst addu ,n-dst ,n-src zero-tn)
	`(unless (location= ,n-dst ,n-src)
	   (inst addu ,n-dst ,n-src zero-tn)))))

(defmacro b (label)
  "Unconditional branch"
  `(inst beq zero-tn zero-tn ,label))


(defmacro def-mem-op (op inst shift load)
  `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
     `(progn
	(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag))
	,,@(when load '('(nop))))))
;;; 
(def-mem-op loadw lw word-shift t)
(def-mem-op storew sw word-shift nil)


(defmacro loadi (reg const)
  (once-only ((n-reg reg)
	      (n-const const))
    `(cond ((<= #x-8000 ,n-const #x7fff)
	    (inst addi ,n-reg zero-tn ,n-const))
	   ((<= #x8000 ,n-const #xffff)
	    (inst ori ,n-reg zero-tn ,n-const))
	   ((<= #x-80000000 ,n-const #xffffffff)
	    (inst lui ,n-reg (ldb (byte 16 16) ,n-const))
	    (let ((low (ldb (byte 16 0) ,n-const)))
	      (unless (zerop low)
		(inst ori ,n-reg ,n-reg low))))
	   (t
	    (error "Constant ~D cannot be loaded." ,n-const)))))

(defmacro load-symbol (reg symbol)
  `(inst addi ,reg null-tn (vm:static-symbol-offset ,symbol)))

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
		 (nop)))
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
       `(inst lb ,n-target ,n-source ,n-offset ))
      (:big-endian
       `(inst lb ,n-target ,n-source (+ ,n-offset 3))))))


;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst addiu ,lip ,function (- (ash vm:function-header-code-offset
					vm:word-shift)
				   vm:function-pointer-type))
     (inst jr ,lip)
     (nop)))

(defmacro lisp-return (return-pc lip)
  "Return to RETURN-PC.  LIP is an interior-reg temporary."
  `(progn
     (inst addiu ,lip ,return-pc (- vm:word-bytes vm:other-pointer-type))
     (inst jr ,lip)
     (nop)))

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
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg base-character-reg sap-reg)
	(sc-case ,n-stack
	  ((control-stack number-stack base-character-stack sap-stack)
	   (loadw ,n-reg cont-tn (tn-offset ,n-stack))))))))

(defmacro store-stack-tn (stack reg)
  (once-only ((n-stack stack)
	      (n-reg reg))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg base-character-reg sap-reg)
	(sc-case ,n-stack
	  ((control-stack number-stack base-character-stack sap-stack)
	   (storew ,n-reg cont-tn (tn-offset ,n-stack))))))))



;;;; Simple Type Checking Macros

(defmacro simple-test-tag (register temp target not-p tag-type tag-mask)
  `(progn
     (unless (zerop ,tag-mask)
       (inst andi ,temp ,register ,tag-mask))
     (inst xori ,temp ,temp ,tag-type)
     (if ,not-p
	 (inst bne ,temp zero-tn ,target)
	 (inst beq ,temp zero-tn ,target))
     (nop)))

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
			     ,n-type-code lowtag-mask))
	   (t
	    (let* ((out-label (gen-label))
		   (not-other-label (if ,n-not-p ,n-target out-label)))
	      (simple-test-tag ,n-register ,n-temp not-other-label t
			       vm:other-pointer-type vm:lowtag-mask)
	      (load-type ,n-temp ,n-register (- vm:other-pointer-type))
	      (nop)
	      (simple-test-tag ,n-temp ,n-temp ,n-target ,n-not-p
			       ,n-type-code 0)
	      (emit-label out-label))))))


;;;; Hairy Type Checking Macros

(defun enumerate-type-codes (types)
  (let ((type-codes nil))
    (dolist (type types)
      (cond ((listp type)
	     (let ((low (eval (first type)))
		   (high (eval (second type))))
	       (when (> low high) (rotatef low high))
	       (do ((n low (1+ n)))
		   ((> n high))
		 (push n type-codes))))
	    (t
	     (push (eval type) type-codes))))
    (sort (remove-duplicates type-codes) #'<)))

(defun canonicalize-type-codes (type-codes &optional (shift 0))
  (unless type-codes (return-from canonicalize-type-codes nil))
  (let* ((type-codes type-codes)
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
			(emit `(inst addi ,temp ,temp ,diff))
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
	   (inst andi ,temp ,register ,tag-mask))
	 ,@(emit)
	 (nop)
	 (emit-label drop-through)))))

(defmacro test-hairy-type (register temp target not-p &rest types)
  "Test-Hairy-Type Register Temp Target Not-P {Type | (Low-Type High-Type)}+
  
  Test whether Register holds a value with one of a specified union of
  type codes.  All low tag type codes will be checked first.  Then the
  pointer will be checked to see if it is an other-pointer-type type
  pointer in which case it will be dereferenced and the remaining type
  codes (the header word type codes) will be checked.  Each separately
  specified Type is matched, and also all types between a Low-Type and
  High-Type pair (inclusive) are matched.  All of the type-code
  expressions are evaluated at macroexpand time.  Temp should be an
  unboxed register." 
  (once-only ((n-register register)
	      (n-temp temp)
	      (n-target target)
	      (n-not-p not-p))
    (unless types (error "Must specify at least one type."))
    ;; 
    ;; Partition the type codes.
    (collect ((low-tag-types)
	      (header-word-types))
      (dolist (type (enumerate-type-codes types))
	(cond ((< type vm:lowtag-limit)
	       (low-tag-types type))
	      (t
	       (header-word-types type))))

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
		 (nop)
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
      (dolist (type (enumerate-type-codes types))
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

(defmacro error-call (error-code &rest values)
  (unless (< (length values) register-arg-count)
    (error "Can't use ERROR-CALL with ~D values"
	   (length values)))
  `(progn
     ,@(let ((index -1))
	 (mapcar #'(lambda (value)
		     `(move (nth ,(incf index) register-arg-tns) ,value))
		 values))
     (inst break ,error-code)))

(defmacro generate-error-code (node error-code &rest values)
  "Generate-Error-Code Node Error-code Value*
  Emit code for an error with the specified Error-Code and context Values.
  Node is used for source context."
  `(unassemble
     (assemble-elsewhere ,node
       (let ((start-lab (gen-label)))
	 (emit-label start-lab)
	 (error-call ,error-code ,@values)
	 start-lab))))


;;; PSEUDO-ATOMIC -- Handy macro for making sequences look atomic.
;;;
(defmacro pseudo-atomic ((ndescr-temp) &rest forms)
  (let ((label (gensym "LABEL-")))
    `(let ((,label (gen-label)))
       (inst andi flags-tn flags-tn (lognot (ash 1 interrupted-flag)))
       (inst ori flags-tn flags-tn (ash 1 atomic-flag))
       ,@forms
       (inst andi flags-tn flags-tn (lognot (ash 1 atomic-flag)))
       (inst andi ,ndescr-temp flags-tn (ash 1 interrupted-flag))
       (inst beq ,ndescr-temp zero-tn ,label)
       (nop)
       (inst break vm:pending-interrupt-trap)
       (emit-label ,label))))
