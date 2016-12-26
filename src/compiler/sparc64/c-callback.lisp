;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/sparc/c-callback.lisp $")

(in-package "SPARC")
(intl:textdomain "cmucl-sparc64-vm")
(use-package "ALIEN")
(use-package "ALIEN-INTERNALS")

;;; Support for callbacks to Lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))
)

(defun callback-accessor-form (type sp offset)
  (let ((parsed-type (alien::parse-alien-type type)))
    (typecase parsed-type
      (alien::double$
       ;; Due to sparc calling conventions, a double arg doesn't have to
       ;; be aligned on a double word boundary.  We have to get the two
       ;; words separately and create the double from them.  Doubles are
       ;; stored in big-endian order, naturally.
       `(kernel:make-double-float
	 (alien:deref (sap-alien (sys:sap+ ,sp ,offset) (* c-call:int)))
	 (alien:deref (sap-alien (sys:sap+ ,sp (+ ,offset vm:word-bytes))
				 (* c-call:unsigned-int)))))
      (alien::integer-64$
       ;; Same as for double, above
       `(let ((hi (alien:deref (sap-alien (sys:sap+ ,sp ,offset)
					  ,(if (alien-integer-type-signed parsed-type)
					       '(* c-call:int)
					       '(* c-call:unsigned-int)))))
	      (lo (alien:deref (sap-alien (sys:sap+ ,sp
						    (+ ,offset vm:word-bytes))
					  (* c-call:unsigned-int)))))
	  (+ (ash hi vm:word-bits) lo)))
      (alien::integer$
       ;; All other objects can be accessed directly.  But we need to
       ;; get the offset right, since the offset we're given is the
       ;; start of the object, and we're a big-endian machine.
       (let ((byte-offset
	      (- vm:word-bytes
		 (ceiling (alien::alien-integer-type-bits parsed-type)
			  vm:byte-bits))))
	 `(deref (sap-alien (sys:sap+ ,sp ,(+ byte-offset offset))
			    (* ,type)))))
      (t
       `(deref (sap-alien (sys:sap+ ,sp ,offset)
			  (* ,type)))))))

(defun compatible-function-types-p (type1 type2)
  (flet ((machine-rep (type)
	   (etypecase type
	     (alien::integer-64$ :dword)
	     ((or alien::integer$ alien::pointer$ alien::sap$) :word)
	     (alien::single$ :single)
	     (alien::double$ :double)
	     (alien::void$ :void))))
    (let ((type1 (alien-function-type-result-type type1))
	  (type2 (alien-function-type-result-type type2)))
      (eq (machine-rep type1) (machine-rep type2)))))

(defun make-callback-trampoline (index fn-type)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (let ((return-type (alien-function-type-result-type fn-type)))
    (flet ((def-reg-tn (offset)
	     (c:make-random-tn :kind :normal
			       :sc (c:sc-or-lose 'vm::unsigned-reg)
			       :offset offset)))
      (let* ((segment (make-segment))
	     ;; Window save area (16 registers)
	     (window-save-size (* 16 vm:word-bytes))
	     ;; Structure return pointer area (1 register)
	     (struct-return-size vm:word-bytes)
	     ;; Register save area (6 registers)
	     (reg-save-area-size (* 6 vm:word-bytes))
	     ;; Local var.  Should be large enough to hold a double-float or long
	     (return-value-size (* 2 vm:word-bytes))
	     ;; Frame size: the register window, the arg save area, the
	     ;; structure return area, and return-value-area, all
	     ;; rounded to a multiple of eight.
	     (framesize (* 8 (ceiling (+ window-save-size struct-return-size
					 reg-save-area-size
					 return-value-size)
				      8)))
	     ;; Offset from FP where the first arg is located.
	     (arg0-save-offset (+ window-save-size struct-return-size))
	     ;; Establish the registers we need
	     (%g0 (def-reg-tn vm::zero-offset))
	     (%o0 (def-reg-tn vm::nl0-offset))
	     (%o1 (def-reg-tn vm::nl1-offset))
	     (%o2 (def-reg-tn vm::nl2-offset))
	     (%o3 (def-reg-tn vm::nl3-offset))
	     (%o7 (def-reg-tn vm::nargs-offset))
	     (%sp (def-reg-tn vm::nsp-offset)) ; aka %o6
	     (%l0 (def-reg-tn vm::a0-offset))
	     (%i0 (def-reg-tn vm::cname-offset))
	     (%i1 (def-reg-tn vm::lexenv-offset))
	     (%i2 (def-reg-tn 26))
	     (%i3 (def-reg-tn vm::nfp-offset))
	     (%i4 (def-reg-tn vm::cfunc-offset))
	     (%i5 (def-reg-tn vm::code-offset))
	     (%fp (def-reg-tn 30))
	     (%i7 (def-reg-tn vm::lip-offset))
	     (f0-s (c:make-random-tn :kind :normal
				     :sc (c:sc-or-lose 'vm::single-reg)
				     :offset 0))
	     (f0-d (c:make-random-tn :kind :normal
				     :sc (c:sc-or-lose 'vm::double-reg)
				     :offset 0))
	     )
	;; The generated assembly roughly corresponds to this C code:
	;;
	;;        tramp(int a0, int a1, int a2, int a3, int a4, int a5, ...)
	;;	{
	;;	  double result;
	;;	  funcall3(call-callback, <index>, &a0, &result);
	;;	  return <cast> result;
	;;	}
	;;
	;; Except, of course, the result is the appropriate result type
	;; for the trampoline.
	;;	       
	(assemble (segment)
	  ;; Save old %fp, etc. establish our call frame with local vars
	  ;; %i contains the input args
	  (inst save %sp %sp (- framesize))
	  ;; The stack frame now looks like
	  ;;
	  ;; TOP (high memory)
	  ;;
	  ;; argn
	  ;; ...
	  ;; arg6
	  ;; arg5
	  ;; arg4
	  ;; arg3
	  ;; arg2
	  ;; arg1
	  ;; arg0
	  ;; struct_return
	  ;; window-save-area	<- %fp + 64
	  ;;			<- %fp
	  ;; local-vars-extra-args	(8-bytes)
	  ;; arg5-save
	  ;; arg4-save
	  ;; arg3-save
	  ;; arg2-save
	  ;; arg1-save
	  ;; arg0-save
	  ;; struct_return		
	  ;; window-save-area
	  ;;			<- %sp

	  ;; Save all %i arg register values on the stack.  (We
	  ;; might not always need to save all, but this is safe
	  ;; and easy.)
	  (inst st %i0 %fp (+ arg0-save-offset (* 0 vm:word-bytes)))
	  (inst st %i1 %fp (+ arg0-save-offset (* 1 vm:word-bytes)))
	  (inst st %i2 %fp (+ arg0-save-offset (* 2 vm:word-bytes)))
	  (inst st %i3 %fp (+ arg0-save-offset (* 3 vm:word-bytes)))
	  (inst st %i4 %fp (+ arg0-save-offset (* 4 vm:word-bytes)))
	  (inst st %i5 %fp (+ arg0-save-offset (* 5 vm:word-bytes)))

	  ;; Set up our args to call funcall3
	  ;;
	  ;; %o0 = address of call-callback
	  ;; %o1 = index
	  ;; %o2 = pointer to the arguments of the caller (address
	  ;;       of arg0 above)
	  ;; %o3 = pointer to return area

	  (inst li %o0 (alien::address-of-call-callback))
	  (inst li %o1 (ash index vm:fixnum-tag-bits))
	  (inst add %o2 %fp arg0-save-offset)
	  (inst add %o3 %fp (- return-value-size))

	  ;; And away we go to funcall3!
	  (let ((addr (alien::address-of-funcall3)))
	    (inst sethi %l0 (ldb (byte 22 10) addr))
	    (inst jal %o7 %l0 (ldb (byte 10 0) addr))
	    (inst nop))

	  ;; Ok, we're back.  The value returned is actually
	  ;; stored in the return area.  Need to get that into
	  ;; the right registers for return.
	  (etypecase return-type
	    (alien::integer-64$
	     ;; A 64-bit bignum, stored big-endian
	     (inst ld %i0 %fp (- return-value-size))
	     (inst ld %i1 %fp (- (- return-value-size vm:word-bytes))))
	    ((or alien::integer$ alien::pointer$ alien::sap$)
	     (inst ld %i0 %fp (- return-value-size)))
	    (alien::single$
	     ;; Get the FP value into F0
	     (inst ldf f0-s %fp (- return-value-size))
	     )
	    (alien::double$
	     (inst lddf f0-d %fp (- return-value-size)))
	    (alien::void$
	     ))

	  (inst jal %g0 %i7 8)
	  (inst restore %g0 %g0 %g0)
	  )
	(let ((length (finalize-segment segment)))
	  (prog1 (alien::segment-to-trampoline segment length)
	    (release-segment segment)))))))


