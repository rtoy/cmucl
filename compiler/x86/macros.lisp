;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/macros.lisp,v 1.5 1997/11/04 09:11:08 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the a bunch of handy macros for the x86.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997.
;;;
(in-package :x86)


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


(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
	      (n-source source)
	      (n-offset offset))
    (ecase (backend-byte-order *target-backend*)
      (:little-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst mov ,n-target
	      (make-ea :byte :base ,n-source :disp (+ ,n-offset 3)))))))


;;;; Allocation helpers

;;; Two allocation approaches are implemented. A call into C can be
;;; used where special care can be taken to disable
;;; interrupt. Alternatively with gencgc inline allocation is possible
;;; although it isn't interrupt safe.

;;;; Call into C.
(defmacro var-allocation (alloc-tn size-tn)
  "Allocate an object with a size in bytes given by Size-TN."
  `(let ((alloc-tn-offset (tn-offset ,alloc-tn)))
    ;; Dispatch to the appropriate allocation routine. Each
    ;; destination has a special entry point. The size may be a
    ;; register or a constant, although the constant case is better
    ;; handled by with-fixed-cgc-allocation.
    (ecase alloc-tn-offset
      (,eax-offset
       (move eax-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_eax") :foreign)))
      (,ecx-offset
       (move ecx-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_ecx") :foreign)))
      (,edx-offset
       (move edx-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_edx") :foreign)))
      (,ebx-offset
       (move ebx-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_ebx") :foreign)))
      (,esi-offset
       (move esi-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_esi") :foreign)))
      (,edi-offset
       (move edi-tn ,size-tn)
       (inst call (make-fixup (extern-alien-name "alloc_to_edi") :foreign))))))

(defmacro allocation (alloc-tn size)
  "Allocate an object of the given fixed size in bytes; the size must be
   an integer."
  `(let ((alloc-tn-offset (tn-offset ,alloc-tn)))
    (assert (integerp ,size))
    ;; Dispatch to the appropriate allocation routine. Each
    ;; destination has a special entry point and there are special
    ;; case for 2 and 4 word objects.
    (ecase alloc-tn-offset
      (,eax-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_eax")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_eax")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov eax-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_eax")
				 :foreign)))))
      (,ecx-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_ecx")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_ecx")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov ecx-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_ecx")
				 :foreign)))))
      (,edx-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_edx")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_edx")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov edx-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_edx")
				 :foreign)))))
      (,ebx-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_ebx")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_ebx")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov ebx-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_ebx") 
				 :foreign)))))
      (,esi-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_esi")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_esi")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov esi-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_esi")
				 :foreign)))))
      (,edi-offset
       (case ,size
	 (8 (inst call (make-fixup (extern-alien-name "alloc_8_to_edi")
				   :foreign)))
	 (16 (inst call (make-fixup (extern-alien-name "alloc_16_to_edi")
				    :foreign)))
	 (t
	  ;; Else: push the size onto the stack.
	  (inst mov edi-tn ,size)
	  (inst call (make-fixup (extern-alien-name "alloc_to_edi")
				 :foreign))))))))

(defmacro fixed-allocation (result-tn type-code size)
  "Allocate an other-pointer object of fixed Size with a single
   word header having the specified Type-Code.  The result is placed in
   Result-TN."
  `(progn
    (allocation ,result-tn (pad-data-block ,size))
    (storew (logior (ash (1- ,size) vm:type-bits) ,type-code) ,result-tn)
    (inst lea ,result-tn
     (make-ea :byte :base ,result-tn :disp other-pointer-type))))


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
      `((inst int 3)				; i386 breakpoint instruction
	;; The return PC points here; note the location for the debugger.
	(let ((vop ,vop))
  	  (when vop
		(note-this-location vop :internal-error)))
	(inst byte ,kind)			; eg trap_Xyyy
	(with-adjustable-vector (,vector)	; interr arguments
	  (write-var-integer (error-number-or-lose ',code) ,vector)
	  ,@(mapcar #'(lambda (tn)
			`(let ((tn ,tn))
			   (write-var-integer
			    (make-sc-offset (sc-number
					     (tn-sc tn))
			     ;; zzzzz jrd here.  tn-offset
			     ;; is zero for constant tns.
			     ;; (tn-offset tn)
			     (or (tn-offset tn) 0)
			     )
			    ,vector)))
		    values)
	  (inst byte (length ,vector))
	  (dotimes (i (length ,vector))
	    (inst byte (aref ,vector i))))))))

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

