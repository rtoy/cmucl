;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: src/compiler/x86/macros.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the a bunch of handy macros for the x86.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1998,1999.
;;;
(in-package :x86)
(intl:textdomain "cmucl-x86-vm")


;;; We can load/store into fp registers through the top of
;;; stack %st(0) (fr0 here). Loads imply a push to an empty register
;;; which then changes all the reg numbers. These macros help manage that.

;;; Use this when don't have to load anything. It preserves old tos value
;;; But probably destroys tn with operation.
(defmacro with-tn@fp-top((tn) &body body)
  `(progn
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))

;;; Use this to prepare for load of new value from memory. This
;;; changes the register numbering so the next instruction had better
;;; be a FP load from memory; a register load from another register
;;; will probably be loading the wrong register!
(defmacro with-empty-tn@fp-top((tn) &body body)
  `(progn
    (inst fstp ,tn)
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))		; save into new dest and restore st(0)


;;; Instruction-like macros.
(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
	      (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro make-ea-for-object-slot (ptr slot lowtag)
  `(make-ea :dword :base ,ptr :disp (- (* ,slot word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))


;;;; Macros to generate useful values.

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg
	 (make-ea :dword
		  :disp (+ nil-value
			   (static-symbol-offset ',symbol)
			   (ash symbol-value-slot word-shift)
			   (- other-pointer-type)))))

(defmacro store-symbol-value (reg symbol)
  `(inst mov
	 (make-ea :dword
		  :disp (+ nil-value
			   (static-symbol-offset ',symbol)
			   (ash symbol-value-slot word-shift)
			   (- other-pointer-type)))
	 ,reg))

(defun make-symbol-value-ea (symbol)
  (make-ea :dword
	   :disp (+ nil-value
		    (static-symbol-offset symbol)
		    (ash symbol-value-slot word-shift)
		    (- other-pointer-type))))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst movzx ,n-target
	      (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst movzx ,n-target
	      (make-ea :byte :base ,n-source :disp (+ ,n-offset 3)))))))

(defmacro load-foreign-data-symbol (reg name )
  #+linkage-table `(inst mov ,reg (make-fixup (extern-alien-name ,name)
					      :foreign-data))
  #-linkage-table `(inst lea ,reg (make-fixup (extern-alien-name ,name)
					      :foreign)))

;;;; Allocation helpers

;;; Two allocation approaches are implemented. A call into C can be
;;; used where special care can be taken to disable
;;; interrupts. Alternatively with gencgc inline allocation is possible
;;; although it isn't interrupt safe.

;;; For GENCGC it is possible to inline object allocation, to permit
;;; this set the following variable to True.
(defparameter *maybe-use-inline-allocation* t)

(defun load-size (alloc-tn dst-tn size)
  (unless (and (tn-p size) (location= alloc-tn size))
    (inst mov dst-tn size)))

(defun inline-allocation (alloc-tn size)
  (let ((ok (gen-label))
	(done (gen-label)))

    ;; Load the size first so that the size can be in the same
    ;; register as alloc-tn.
    (load-size alloc-tn alloc-tn size)

    ;; Try inline allocation, incrementing the
    ;; current-region-free-pointer by the size.  If we didn't pass the
    ;; end of the region, then inline allocation succeeded, and we're
    ;; done.
    (inst add alloc-tn
	  (make-symbol-value-ea '*current-region-free-pointer*))
    (inst cmp alloc-tn
	  (make-symbol-value-ea '*current-region-end-addr*))
    (inst jmp :be OK)

    ;; Inline allocation didn't work so we need to call alloc,
    ;; carefully.  Need to recompute the size because we can't just
    ;; reload size because it might have already been destroyed if
    ;; size = alloc-tn (which does happen).
    (inst sub alloc-tn (make-symbol-value-ea '*current-region-free-pointer*))
    (case (tn-offset alloc-tn)
      (#.eax-offset
       (inst call (make-fixup (extern-alien-name "alloc_overflow_sse2")
			      :foreign))
       (inst jmp done))
      (t
       (inst push eax-tn)		; Save any value in eax
       (inst mov eax-tn alloc-tn)
       (inst call (make-fixup (extern-alien-name "alloc_overflow_sse2")
			      :foreign))
       (inst mov alloc-tn eax-tn)  	; Put allocated address in alloc-tn
       (inst pop eax-tn)		; Restore old value of eax
       (inst jmp done)))
			       
    (emit-label ok)
    (inst xchg (make-symbol-value-ea '*current-region-free-pointer*)
	  alloc-tn)
    (emit-label done))
  
  (values))

(defun not-inline-allocation (alloc-tn size)
  ;; C call to allocate. The size may be a register or a constant.
  (load-size alloc-tn alloc-tn size)
  (case (tn-offset alloc-tn)
    (#.eax-offset
     (inst call (make-fixup (extern-alien-name "alloc_overflow_sse2")
			    :foreign)))
    (t
     (inst push eax-tn)			; Save any value in eax
     (inst mov eax-tn alloc-tn)
     (inst call (make-fixup (extern-alien-name "alloc_overflow_sse2")
			    :foreign))
     (inst mov alloc-tn eax-tn)	  ; Save allocated address in alloc-tn
     (inst pop eax-tn)))
  (values))

;;;
;;; Allocate SIZE bytes from the stack, storing a pointer to the
;;; allocated memory in ALLOC-TN.
;;;
(defun dynamic-extent-allocation (alloc-tn nbytes)
  (inst sub esp-tn nbytes)
  (inst and esp-tn #xfffffff8)
  (inst mov alloc-tn esp-tn)
  (values))

(defun allocation (alloc-tn size &optional inline dynamic-extent)
  "Allocate an object with a size in bytes given by Size.
   The size may be an integer or a TN.
   If Inline is a VOP node-var then it is used to make an appropriate
   speed vs size decision.  If Dynamic-Extent is true, and otherwise
   appropriate, allocate from the stack."
  (cond (dynamic-extent
	 (dynamic-extent-allocation alloc-tn size))
	((and *maybe-use-inline-allocation*
	      (or (null inline)
		  (policy inline (>= speed space)))
	      (backend-featurep :gencgc))
	 (inline-allocation alloc-tn size))
	(t
	 (not-inline-allocation alloc-tn size)))
  (values))

(defmacro with-fixed-allocation ((result-tn type-code size &optional inline)
				 &rest forms)
  "Allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN."
  `(pseudo-atomic
    (allocation ,result-tn (pad-data-block ,size) ,inline)
    (storew (logior (ash (1- ,size) vm::type-bits) ,type-code) ,result-tn)
    (inst lea ,result-tn
     (make-ea :byte :base ,result-tn :disp other-pointer-type))
    ,@forms))


;;;; Error Code

(eval-when (compile load eval)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym))
	  (length (gensym)))
      `((let ((vop ,vop))
  	  (when vop
	    (note-this-location vop :internal-error)))
	(inst ud1 ,kind)		; eg trap_Xyyy
	(let ((,vector (make-array 8 :element-type '(unsigned-byte 8)
				   :fill-pointer 0 :adjustable t)))
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer
			    (make-sc-offset (sc-number (tn-sc tn))
			     ;; tn-offset is zero for constant tns.
			     (or (tn-offset tn) 0))
			    ,vector)))
		    values)
	  (let ((,length (length ,vector)))
	    (inst byte ,length)
	    (dotimes (i ,length)
	      (inst byte (aref ,vector i)))))))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
	(emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     ,@(emit-error-break vop cerror-trap error-code values)
     (inst jmp ,label)))

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
    `(let ((,continue (gen-label))
	   (,error (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
	 (emit-label ,error)
	 (cerror-call ,vop ,continue ,error-code ,@values))
       ,error)))



;;;; PSEUDO-ATOMIC.

(defparameter *enable-pseudo-atomic* t)

;;; PSEUDO-ATOMIC -- Internal Interface.
;;;
(defmacro pseudo-atomic (&rest forms)
  (let ((label (gensym "LABEL-")))
    `(let ((,label (gen-label)))
      (when *enable-pseudo-atomic*
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-interrupted*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      0)
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-atomic*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      (fixnumize 1)))
      ,@forms
      (when *enable-pseudo-atomic*
	(inst mov (make-ea :byte :disp (+ nil-value
					  (static-symbol-offset
					   'lisp::*pseudo-atomic-atomic*)
					  (ash symbol-value-slot word-shift)
					  (- other-pointer-type)))
	      0)
	(inst cmp (make-ea :byte
			   :disp (+ nil-value
				    (static-symbol-offset
				     'lisp::*pseudo-atomic-interrupted*)
				    (ash symbol-value-slot word-shift)
				    (- other-pointer-type)))
	      0)
	(inst jmp :eq ,label)
	(inst ud1 pending-interrupt-trap)
	(emit-label ,label)))))


;;;; Indexed references:

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; pw was 5
	 (inst mov value (make-ea :dword :base object :index index
				  :disp (- (* ,offset word-bytes) ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2			; pw was 5
	 (inst mov value (make-ea :dword :base object
				  :disp (- (* (+ ,offset index) word-bytes)
					   ,lowtag)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg))
	      (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4			; was 5
	 (inst mov (make-ea :dword :base object :index index
			    :disp (- (* ,offset word-bytes) ,lowtag))
	       value)
	 (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
	      (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3			; was 5
	 (inst mov (make-ea :dword :base object
			    :disp (- (* (+ ,offset index) word-bytes) ,lowtag))
	       value)
	 (move result value)))))

(defmacro define-partial-reffer (name type size signed offset lowtag scs
				      el-type &optional translate)
  (let ((scale (ecase size (:byte 1) (:word 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg))
		(index :scs (unsigned-reg)))
	 (:arg-types ,type positive-fixnum)
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   (inst ,(if signed 'movsx 'movzx) value
		 (make-ea ,size :base object :index index :scale ,scale
			  :disp (- (* ,offset word-bytes) ,lowtag)))))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg)))
	 (:info index)
	 (:arg-types ,type (:constant (signed-byte 30)))
	 (:results (value :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 4
	   (inst ,(if signed 'movsx 'movzx) value
		 (make-ea ,size :base object
			  :disp (- (+ (* ,offset word-bytes) (* ,scale index))
				   ,lowtag))))))))

(defmacro define-partial-setter (name type size offset lowtag scs el-type
				      &optional translate)
  (let ((scale (ecase size (:byte 1) (:word 2))))
    `(progn
       (define-vop (,name)
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :to (:eval 0))
		(index :scs (unsigned-reg) :to (:eval 0))
		(value :scs ,scs :target ,(ecase size
					    (:byte 'temp)
					    (:word 'result))))
	 (:arg-types ,type positive-fixnum ,el-type)
	 ,@(when (eq size :byte)
	     '((:temporary (:sc unsigned-reg :offset eax-offset :target result
			    :from (:argument 2) :to (:result 0))
			   temp)))
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 5
	   ,@(when (eq size :byte)
	       '((move temp value)))
	   (inst mov (make-ea ,size :base object :index index :scale ,scale
			      :disp (- (* ,offset word-bytes) ,lowtag))
		 ,(ecase size (:byte 'temp) (:word 'value)))
	   ,(ecase size
	      (:byte '(move result temp))
	      (:word '(move result value)))))
       (define-vop (,(symbolicate name "-C"))
	 ,@(when translate
	     `((:translate ,translate)))
	 (:policy :fast-safe)
	 (:args (object :scs (descriptor-reg) :to (:eval 0))
		(value :scs ,scs :target ,(ecase size
                                            (:byte 'temp)
                                            (:word 'result))))
	 (:info index)
	 (:arg-types ,type (:constant (signed-byte 30)) ,el-type)
	 ,@(when (eq size :byte)
	     '((:temporary (:sc unsigned-reg :offset eax-offset :target result
			    :from (:argument 1) :to (:result 0))
			   temp)))
	 (:results (result :scs ,scs))
	 (:result-types ,el-type)
	 (:generator 4
	   ,@(when (eq size :byte)
	       '((move temp value)))
	   (inst mov (make-ea ,size :base object
			      :disp (- (+ (* ,offset word-bytes) (* ,scale index))
				       ,lowtag))
		 ,(ecase size (:byte 'temp) (:word 'value)))
	   ,(ecase size
	      (:byte '(move result temp))
	      (:word '(move result value))))))))

(defmacro define-full-conditional-setter (name type offset lowtag scs el-type
					  &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :result)
	      (index :scs (any-reg) :to :result)
	      (old-value :scs ,scs :target eax)
	      (new-value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type ,el-type)
       (:temporary (:sc ,(first scs) :offset eax-offset
		    :from (:argument 2) :to :result :target result) eax)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
	 (move eax old-value)
	 (inst cmpxchg (make-ea :dword :base object :index index :scale 1
				:disp (- (* ,offset word-bytes) ,lowtag))
	       new-value)
	 (move result eax)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
	   `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :result)
	      (old-value :scs ,scs :target eax)
	      (new-value :scs ,scs))
       (:info index)
       (:arg-types ,type (:constant (signed-byte 30)) ,el-type ,el-type)
       (:temporary (:sc ,(first scs) :offset eax-offset
		    :from (:argument 1) :to :result :target result)  eax)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4
	 (move eax old-value)
	 (inst cmpxchg (make-ea :dword :base object
				:disp (- (* (+ ,offset index) word-bytes)
					 ,lowtag))
	       new-value)
	 (move result eax)))))
