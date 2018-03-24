;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/macros.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains various useful macros for generating ARM code.
;;;

(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

;; (loadw object base &optional (offset 0) (lowtag 0) temp)
;; (storew object base &optional (offset 0) (lowtag 0) temp)
;;
;; Load a word at a given address into the register OBJECT, or store
;; OBJECT at the given address.. The address of the word is in
;; register BASE, plus an offset given by OFFSET, which is in words.
;; LOWTAG is an adjustment to OFFSET to account for any tag bits used
;; in the BASE descriptor register.
;;
;; In some situations, the offset may be so large that it cannot fit
;; into the offset field of the LDR(STR) instruction (a 13-bit signed
;; quantity).  In this situation, the TEMP register (any-reg), if
;; supplied, is used to compute the correct offset.  If TEMP is not
;; given, the offset is assumed to fit.
;;
;; Samething for storew, except we store OBJECT at the given address.
(macrolet
    ((def-load/store-word (op inst shift)
     `(defmacro ,op (object base &optional (offset 0) (lowtag 0) temp)
       (if temp
	   (let ((offs (gensym)))
	     `(let ((,offs (- (ash ,offset ,',shift) ,lowtag)))
	       (if (typep '(signed-byte 13),offs)
		   (inst ,',inst ,object ,base ,offs)
		   (progn
		     (inst li ,temp ,offs)
		     (inst ,',inst ,object ,base ,temp)))))
	   `(inst ,',inst ,object ,base (- (ash ,offset ,',shift) ,lowtag))))))
  (def-load/store-word loadw ldr word-shift)
  (def-load/store-word storew str word-shift))

(defmacro load-symbol (reg symbol)
  `(inst add ,reg null-tn (static-symbol-offset ,symbol)))

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
	      `(inst ldr ,reg null-tn
			      (+ (static-symbol-offset ',symbol)
				 (ash ,',offset word-shift)
				 (- other-pointer-type))))
	    (defmacro ,storer (reg symbol)
	      `(inst str ,reg null-tn
			      (+ (static-symbol-offset ',symbol)
				 (ash ,',offset word-shift)
				 (- other-pointer-type))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst ldrb ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst ldrb ,n-target ,n-source (+ ,n-offset (1- word-bytes)))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions. 

(defmacro lisp-jump (function)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (move code-tn ,function)
     (inst add lip-tn ,function
	   (- (ash function-code-offset word-shift) vm:function-pointer-type))
     (inst bx lip-tn)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     ,(when frob-code
	`(move code-tn ,return-pc))
     (inst add lip-tn ,return-pc (- (* (1+ ,offset) word-bytes) other-pointer-type))
     (inst bx lip-tn)))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (align lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))


;;;; Stack TN's

;;; Load-Stack-TN, Store-Stack-TN  --  Interface
;;;
;;;    Move a stack TN to a register and vice-versa.
;;;
;;; FIXME: On SPARC, sometimes the offset to the stack TN won't fit in
;;; the offset field of the LD instruction.  The same is true on
;;; ARM. For SPARC, there is a dedicated register to use as a temp.
;;; It would be useful on ARM if Load-Stack-TN and Store-Stack-TN took
;;; an optional arg to specify a temp non-descriptor reg that could be
;;; used to allow access to the entire stack.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
	 (stack ,stack))
     (sc-case stack
       ((control-stack)
	;; Stack grows down, so take the negative of the tn-offset!
	(loadw reg cfp-tn (- (tn-offset stack)) 0)))))

(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
	 (reg ,reg))
     (sc-case stack
       ((control-stack)
	;; Stack grows down, so take the negative of the tn-offset!
	(storew reg cfp-tn (- (tn-offset stack)) 0)))))


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
	   (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))


;;;; Storage allocation:

;; Allocation macro
;;
;; This macro does the appropriate stuff to allocate space.
;;
;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;; applied.  The amount of space to be allocated is SIZE bytes (which
;; must be a multiple of the lisp object size).
;;
;; If STACK-P is given, then allocation occurs on the control stack
;; (for dynamic-extent).  In this case, you MUST also specify NODE, so
;; that the appropriate compiler policy can be used, and TEMP-TN,
;; which is needed for work-space.  TEMP-TN MUST be a non-descriptor
;; reg.
;;
;; If generational GC is enabled, you MUST supply a value for TEMP-TN
;; because a temp register is needed to do inline allocation.
;; TEMP-TN, in this case, can be any register, since it holds a
;; double-word aligned address (essentially a fixnum).
(defmacro allocation (result-tn size lowtag temp-tn &key stack-p)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.
  `(cond (,stack-p
	  ;; Stack allocation, not supported
	  (error "Stack allocation not supported"))
	 #-gencgc
	 (t
	  ;; Normal allocation to the heap.
	  (if (logbitp (1- lowtag-bits) ,lowtag)
	      (progn
		(load-symbol-value ,temp-tn lisp::*allocation-pointer*)
		(inst orr ,result-tn ,temp-tn ,lowtag)
		(inst add ,temp-tn ,temp-tn ,size)
		(store-symbol-value ,temp-tn lisp::*allocation-pointer*))
	      (progn
		(load-symbol-value ,temp-tn lisp::*allocation-pointer*)
		(inst bic ,result-tn ,temp-tn lowtag-mask)
		(inst orr ,result-tn ,result-tn ,lowtag)
		(inst add ,temp-tn ,temp-tn ,size)
		(store-symbol-value ,temp-tn lisp::*allocation-pointer*))))
	 #+gencgc
	 (t
	  (error "Gencgc not supported"))))

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size
					    &key (lowtag other-pointer-type)
					    stack-p)
				 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
	      (type-code type-code) (size size)
	      (lowtag lowtag))
    `(pseudo-atomic (,temp-tn)
       (allocation ,result-tn (pad-data-block ,size) ,lowtag ,temp-tn
	           :stack-p ,stack-p)
      (when ,type-code
	(inst li ,temp-tn (logior (ash (1- ,size) type-bits) ,type-code))
	(storew ,temp-tn ,result-tn 0 ,lowtag))
       ,@body)))


;;;; Type testing noise.

;;; GEN-RANGE-TEST -- internal
;;;
;;; Generate code that branches to TARGET iff REG contains one of VALUES.
;;; If NOT-P is true, invert the test.  Jumping to NOT-TARGET is the same
;;; as falling out the bottom.
;;; 
(defun gen-range-test (reg target not-target not-p min seperation max values)
  (let ((tests nil)
	(start nil)
	(end nil)
	(insts nil))
    (multiple-value-bind (equal less-or-equal greater-or-equal label)
			 (if not-p
			     (values :ne :gt :lt not-target)
			     (values :eq :le :ge target))
      (flet ((emit-test ()
	       (if (= start end)
		   (push start tests)
		   (push (cons start end) tests))))
	(dolist (value values)
	  (cond ((< value min)
		 (error (intl:gettext "~S is less than the specified minimum of ~S")
			value min))
		((> value max)
		 (error (intl:gettext "~S is greater than the specified maximum of ~S")
			value max))
		((not (zerop (rem (- value min) seperation)))
		 (error (intl:gettext "~S isn't an even multiple of ~S from ~S")
			value seperation min))
		((null start)
		 (setf start value))
		((> value (+ end seperation))
		 (emit-test)
		 (setf start value)))
	  (setf end value))
	(emit-test))
      (macrolet ((inst (name &rest args)
		       `(push (list 'inst ',name ,@args) insts)))
	(do ((remaining (nreverse tests) (cdr remaining)))
	    ((null remaining))
	  (let ((test (car remaining))
		(last (null (cdr remaining))))
	    (if (atom test)
		(progn
		  (inst cmp reg test)
		  (if last
		      (inst b target equal)
		      (inst b label :eq)))
		(let ((start (car test))
		      (end (cdr test)))
		  (cond ((and (= start min) (= end max))
			 (warn (intl:gettext "The values ~S cover the entire range from ~
			 ~S to ~S [step ~S].")
			       values min max seperation)
			 (push `(unless ,not-p (inst b ,target)) insts))
			((= start min)
			 (inst cmp reg end)
			 (if last
			     (inst b target less-or-equal)
			     (inst b label :le)))
			((= end max)
			 (inst cmp reg start)
			 (if last
			     (inst b target greater-or-equal)
			     (inst b label :ge)))
			(t
			 (inst cmp reg start)
			 (inst b (if not-p target not-target) :lt)
			 (inst cmp reg end)
			 (if last
			     (inst b target less-or-equal)
			     (inst b label :le))))))))))
    (nreverse insts)))

(defun gen-other-immediate-test (reg target not-target not-p values)
  (gen-range-test reg target not-target not-p
		  (+ other-immediate-0-type lowtag-limit)
		  (- other-immediate-1-type other-immediate-0-type)
		  (ash 1 type-bits)
		  values))


(defun test-type-aux (reg temp target not-target not-p lowtags immed hdrs
			  function-p)
  (let* ((fixnump (and (member even-fixnum-type lowtags :test #'eql)
		       (member odd-fixnum-type lowtags :test #'eql)))
	 (lowtags (sort (if fixnump
			    (delete even-fixnum-type
				    (remove odd-fixnum-type lowtags
					    :test #'eql)
				    :test #'eql)
			    (copy-list lowtags))
			#'<))
	 (lowtag (if function-p
		     vm:function-pointer-type
		     vm:other-pointer-type))
	 (hdrs (sort (copy-list hdrs) #'<))
	 (immed (sort (copy-list immed) #'<)))
    (append
     (when immed
       `((inst and ,temp ,reg type-mask)
	 ,@(if (or fixnump lowtags hdrs)
	       (let ((fall-through (gensym)))
		 `((let (,fall-through (gen-label))
		     ,@(gen-other-immediate-test
			temp (if not-p not-target target)
			fall-through nil immed)
		     (emit-label ,fall-through))))
	       (gen-other-immediate-test temp target not-target not-p immed))))
     (when fixnump
       `((inst tst ,reg fixnum-tag-mask)
	 ,(if (or lowtags hdrs)
	      `(inst b ,(if not-p not-target target) :eq)
	      `(inst b ,target  ,(if not-p :ne :eq)))))
     (when (or lowtags hdrs)
       `((inst and ,temp ,reg lowtag-mask)))
     (when lowtags
       (if hdrs
	   (let ((fall-through (gensym)))
	     `((let ((,fall-through (gen-label)))
		 ,@(gen-range-test temp (if not-p not-target target)
				   fall-through nil
				   0 1 (1- lowtag-limit) lowtags)
		 (emit-label ,fall-through))))
	   (gen-range-test temp target not-target not-p 0 1
			   (1- lowtag-limit) lowtags)))
     (when hdrs
       `((inst cmp ,temp ,lowtag)
	 (inst b ,(if not-p target not-target) :ne)
	 (load-type ,temp ,reg (- ,lowtag))
	 ,@(gen-other-immediate-test temp target not-target not-p hdrs))))))

(defconstant immediate-types
  (list base-char-type unbound-marker-type))

(defconstant function-subtypes
  (list funcallable-instance-header-type
	#-double-double dylan-function-header-type
	function-header-type closure-function-header-type
	closure-header-type))

(defmacro test-type (register temp target not-p &rest type-codes)
  (let* ((type-codes (mapcar #'eval type-codes))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended immediate-types :test #'eql))
	 (headers (set-difference extended immediate-types :test #'eql))
	 (function-p nil))
    (unless type-codes
      (error (intl:gettext "Must supply at least on type for test-type.")))
    (when (and headers (member other-pointer-type lowtags))
      (warn (intl:gettext "OTHER-POINTER-TYPE supersedes the use of ~S") headers)
      (setf headers nil))
    (when (and immediates
	       (or (member other-immediate-0-type lowtags)
		   (member other-immediate-1-type lowtags)))
      (warn (intl:gettext "OTHER-IMMEDIATE-n-TYPE supersedes the use of ~S") immediates)
      (setf immediates nil))
    (when (intersection headers function-subtypes)
      (unless (subsetp headers function-subtypes)
	(error (intl:gettext "Can't test for mix of function subtypes and normal ~
		header types.")))
      (setq function-p t))
      
    (let ((n-reg (gensym))
	  (n-temp (gensym))
	  (n-target (gensym))
	  (not-target (gensym)))
      `(let ((,n-reg ,register)
	     (,n-temp ,temp)
	     (,n-target ,target)
	     (,not-target (gen-label)))
	 (declare (ignorable ,n-temp))
	 ,@(if (constantp not-p)
	       (test-type-aux n-reg n-temp n-target not-target
			      (eval not-p) lowtags immediates headers
			      function-p)
	       `((cond (,not-p
			,@(test-type-aux n-reg n-temp n-target not-target t
					 lowtags immediates headers
					 function-p))
		       (t
			,@(test-type-aux n-reg n-temp n-target not-target nil
					 lowtags immediates headers
					 function-p)))))
	 (emit-label ,not-target)))))


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
	(inst udf ,kind)
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
	(align word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     ,@(emit-error-break vop cerror-trap error-code values)
     (inst b ,label)))

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
(defmacro pseudo-atomic ((temp-tn &key (extra 0)) &rest forms)
  (declare (ignore extra))
  (once-only ((temp-tn temp-tn))
    (let ((label (gensym "LABEL-")))
      `(let ((,label (gen-label)))
	 ;; Set the pseudo-atomic flag, in *pseudo-atomic-atomic*
	 (without-scheduling ()
	   (load-symbol-value ,temp-tn lisp::*pseudo-atomic-atomic*)
	   (inst orr ,temp-tn ,temp-tn pseudo-atomic-value)
	   (store-symbol-value ,temp-tn lisp::*pseudo-atomic-atomic*))
	 ,@forms
	 ;; Reset the pseudo-atomic flag
	 (without-scheduling ()
	   ;; Remove the pseudo-atomic flag.
	   (load-symbol-value ,temp-tn lisp::*pseudo-atomic-atomic*)
	   (inst bic ,temp-tn ,temp-tn pseudo-atomic-value)
	   ;; Check to see if pseudo-atomic interrupted flag is set
	   (inst tst ,temp-tn pseudo-atomic-interrupted-value)
	   (inst b ,label :ne)
	   ;; The C code needs to process this correctly and fixup alloc-tn.
	   (inst bkpt pseudo-atomic-trap)
	   (emit-label ,label))))))
