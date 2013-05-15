;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/compiler/ppc/c-callback.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VOPs and other necessary machine specific support
;;; routines for call-out to C.
;;;
;;; Written by William Lott.
;;;
(in-package "PPC")


(eval-when (:compile-toplevel :load-toplevel :execute)
(export '(make-callback-trampoline callback-accessor-form
	  compatible-function-types-p))
)

(defun callback-accessor-form (type sp offset)
  (let ((parsed-type (alien::parse-alien-type type)))
    (typecase parsed-type
      (alien::integer-64$
       ;; Get both words of a 64-bit integer and combine together, in
       ;; a big-endian fashion.
       `(let ((hi (alien:deref (sap-alien (sys:sap+ ,sp ,offset)
					  ,(if (alien-integer-type-signed parsed-type)
					       '(* c-call:int)
					       '(* c-call:unsigned-int)))))
	      (lo (alien:deref (sap-alien (sys:sap+ ,sp
						    (+ ,offset vm:word-bytes))
					  (* c-call:unsigned-int)))))
	     (+ (ash hi vm:word-bits) lo)))
      (alien::integer$
       ;; We can access machine integers directly, but we need to get
       ;; the offset right, since the offset we're given is the start
       ;; of the object, and we're a big-endian machine.
       (let ((byte-offset
	      (- vm:word-bytes
		 (ceiling (alien::alien-integer-type-bits parsed-type)
			  vm:byte-bits))))
	 `(deref (sap-alien (sys:sap+ ,sp ,(+ byte-offset offset))
			    (* ,type)))))
      (t
       ;; This should work for everything else.
       `(deref (sap-alien (sys:sap+ ,sp ,offset)
			  (* ,type)))))))

(defun compatible-function-types-p (fun-type1 fun-type2)
  (labels ((type-words (type)
             (ceiling (alien-type-bits type) vm:word-bits))
           (compatible-type-p (type1 type2)
             (let ((float1 (alien-float-type-p type1))
                   (float2 (alien-float-type-p type2)))
               (and (if float1
			float2
			(not float2))
                    (= (type-words type1) (type-words type2))))))
    (let ((args1 (alien-function-type-arg-types fun-type1))
          (args2 (alien-function-type-arg-types fun-type2))
          (ret1 (alien-function-type-result-type fun-type1))
          (ret2 (alien-function-type-result-type fun-type2)))
      (and (= (length args1) (length args2))
           (every #'compatible-type-p args1 args2)
           (compatible-type-p ret1 ret2)))))

(defun make-callback-trampoline (index fn-type)
  (let ((return-type (alien-function-type-result-type fn-type))
	(arg-types (alien::alien-function-type-arg-types fn-type)))
    (make-callback-trampoline-segment index arg-types return-type)))

(defun make-callback-trampoline-segment (index argument-types return-type)
  "Return an sb-assem:segment which calls call-callback with INDEX and
a pointer to the arguments."
  (declare (type (unsigned-byte 16) index)
	   (optimize (debug 3)))
  (flet ((make-gpr (n)
	   (make-random-tn :kind :normal :sc (sc-or-lose 'any-reg) :offset n))
	 (make-fpr (n)
	   (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
			   :offset n))
	 (round-up-16 (n)
	   ;; Round up to a multiple of 16.  Darwin wants that for the
	   ;; stack pointer.
	   (* 16 (ceiling n 16))))

    ;; The "Mach-O Runtime Conventions" document for OS X almost specifies
    ;; the calling convention (it neglects to mention that the linkage area
    ;; is 24 bytes).
    (let* ((segment (make-segment))
	   (save-gprs (mapcar #'make-gpr '(13 24)))
	   
	   (argument-words
	    (mapcar (lambda (arg) (ceiling (alien-type-bits arg) vm:word-bits))
		    argument-types))
	   (linkage-area-size 24))
      (assemble (segment)

	(let ((sp (make-gpr 1)))
	  
	  ;; To save our arguments, we follow the algorithm sketched in the
	  ;; "PowerPC Calling Conventions" section of that document.
	  (let ((words-processed 0)
		(gprs (mapcar #'make-gpr '(3 4 5 6 7 8 9 10)))
		(fprs (mapcar #'make-fpr '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
	    (flet ((handle-arg (type words)
		     (let ((integerp (not (alien-float-type-p type)))
			   (offset (+ (* words-processed vm:word-bytes)
				      linkage-area-size)))
		       (cond
			 (integerp
			  (dotimes (k words)
			    (let ((gpr (pop gprs)))
			      (when gpr
				     (inst stw gpr sp offset)
				     (incf words-processed)
				     (incf offset vm:word-bytes)))))
			 ;; The handling of floats is a little ugly because we
			 ;; hard-code the number of words for single- and
			 ;; double-floats.
			 ((alien-single-float-type-p type)
			  (pop gprs)
			  (let ((fpr (pop fprs)))
			    (inst stfs fpr sp offset))
			  (incf words-processed))
			 ((alien-double-float-type-p type)
			  (setf gprs (cddr gprs))
			  (let ((fpr (pop fprs)))
			    (inst stfd fpr sp offset))
			  (incf words-processed 2))))))
	      (mapc #'handle-arg argument-types argument-words)))

	  ;; The args have been saved to memory.
	  ;;
	  ;; The stack frame is something like this:
	  ;;
	  ;; stack arg n
	  ;; ...
	  ;; stack arg 1
	  ;; stack arg 0
	  ;; save arg 7
	  ;; save arg 6
	  ;; save arg 5
	  ;; save arg 4
	  ;; save arg 3
	  ;; save arg 2
	  ;; save arg 1
	  ;; save arg 0
	  ;; 24 bytes for linkage area
	  ;; -> sp points to the bottom of the linkage area
	  ;;
	  ;; Set aside space for our stack frame.  We need enough room
	  ;; for the callback return area, some space to save the
	  ;; non-volatile (callee-saved) registers, space to save the
	  ;; args for the function we're calling, and the linkage
	  ;; area.  The space is rounded up to a multiple of 16 bytes
	  ;; because the stack should be aligned to a multiple of 16
	  ;; bytes.
	  ;;
	  ;; Our stack frame will look something like this now:
	  ;;
	  ;; Offset    Value
	  ;; 64        Caller's frame (see above)
	  ;; 56/60     return area (1 or 2 words) 
	  ;; 48        filler (unused)
	  ;; 44        save r24
	  ;; 40        save r13
	  ;; 36        save arg 3
	  ;; 32        save arg 2
	  ;; 28        save arg 1
	  ;; 24        save arg 0
	  ;; 0         linkage area (24 bytes)
	  ;;
	  ;;
	  ;; The return area is allocated at the top of the frame.
	  ;; When we call funcall3, the linkage table entry is used,
	  ;; which unconditionally uses r13 and r24.  (See
	  ;; lisp/ppc-arch.c.)  So these need to be saved.  funcall3,
	  ;; which calls call_into_lisp, will take care of saving all
	  ;; the remaining registers that could be used.

	  ;; INDEX is fixnumized, ARGS and RETURN-AREA don't need to
	  ;; be because they're word-aligned.  Kinda gross, but
	  ;; hey....
	  
	  (let* ((return-area-words (ceiling (or (alien-type-bits return-type)
						0)
					    vm:word-bits))
		 (save-words (length save-gprs))
		 (args-size (* 4 vm:word-bytes))
		 (frame-size
		  (round-up-16 (+ linkage-area-size
				  (* return-area-words vm:word-bytes)
				  (* save-words vm:word-bytes)
				  args-size))))
	    (destructuring-bind (r0 arg1 arg2 arg3 arg4)
		(mapcar #'make-gpr '(0 3 4 5 6))
	      ;; Setup the args for the call.  We call
	      ;; funcall3(call-callback, index, arg-pointer,
	      ;; return-area-address)
	      (inst lr arg1 (alien::address-of-call-callback))
	      (inst li arg2 (fixnumize index))
	      (inst addi arg3 sp linkage-area-size)
	      (inst addi arg4 sp (- (* return-area-words vm:word-bytes)))

	      ;; Save sp, setup the frame
	      (inst mflr r0)
	      (inst stw r0 sp (* 2 vm:word-bytes))
	      (inst stwu sp sp (- frame-size))

	      ;; Save the caller-saved registers that the linkage
	      ;; table trampoline clobbers.
	      (let ((save-offset (+ linkage-area-size args-size)))
		(dolist (r save-gprs)
		  (inst stw r sp save-offset)
		  (incf save-offset vm:word-bytes)))
	      
	      ;; Make the call
	      (inst lr r0 (alien::address-of-funcall3))
	      (inst mtlr r0)
	      (inst blrl)

	      (let ((return-offset (- frame-size
				      (* return-area-words vm:word-bytes))))
		(etypecase return-type
		  ((or alien::integer$ alien::pointer$ alien::sap$
		       alien::integer-64$)
		   (loop repeat return-area-words
		         with gprs = (mapcar #'make-gpr '(3 4))
		         for gpr = (pop gprs)
		         for offset from return-offset by vm:word-bytes
			 do (inst lwz gpr sp offset)))
		  (alien::single$
		   ;; Get the FP value into F1
		   (let ((f1 (make-fpr 1)))
		     (inst lfs f1 sp return-offset)))
		  (alien::double$
		   ;; Get the FP value into F1
		   (let ((f1 (make-fpr 1)))
		     (inst lfd f1 sp return-offset)))
		  (alien::void$
		   ;; Nothing to do
		   )))
	      
	      ;; Restore the GPRS we saved.
	      (let ((save-offset (+ linkage-area-size args-size)))
		(dolist (r save-gprs)
		  (inst lwz r sp save-offset)
		  (incf save-offset vm:word-bytes)))

	      ;; All done.  Restore sp and lr and return.
	      (inst lwz r0 sp (+ frame-size (* 2 vm:word-bytes)))
	      (inst mtlr r0)
	      (inst addic sp sp frame-size)
	      
	      ;; And back we go!
	      (inst blr)))))

      (let ((length (finalize-segment segment)))
	(prog1 (alien::segment-to-trampoline segment length)
	  (release-segment segment))))))
