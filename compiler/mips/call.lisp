;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/call.lisp,v 1.23 1990/06/27 06:43:46 wlott Exp $
;;;
;;;    This file contains the VM definition of function call for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for the MIPS by William Lott.
;;;
(in-package "C")


;;;; Interfaces to IR2 conversion:

;;; Standard-Argument-Location  --  Interface
;;;
;;;    Return a wired TN describing the N'th full call argument passing
;;; location.
;;;
(defun standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *any-primitive-type*
		     register-arg-scn
		     (elt register-arg-offsets n))
      (make-wired-tn *any-primitive-type*
		     control-stack-arg-scn n)))


;;; Make-Return-PC-Passing-Location  --  Interface
;;;
;;;    Make a passing location TN for a local call return PC.  If standard is
;;; true, then use the standard (full call) location, otherwise use any legal
;;; location.  Even in the non-standard case, this may be restricted by a
;;; desire to use a subroutine call instruction.
;;;
(defun make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *any-primitive-type* register-arg-scn)))


;;; Make-Old-FP-Passing-Location  --  Interface
;;;
;;;    Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; Old-FP in.  This is (obviously) wired in the standard convention, but is
;;; totally unrestricted in non-standard conventions, since we can always fetch
;;; it off of the stack using the arg pointer.
;;;
(defun make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* immediate-arg-scn old-fp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make-Old-FP-Save-Location, Make-Return-PC-Save-Location  --  Interface
;;;
;;;    Make the TNs used to hold Old-FP and Return-PC within the current
;;; function.  We treat these specially so that the debugger can find them at a
;;; known location.
;;;
(defun make-old-fp-save-location (env)
  (environment-live-tn
   (make-wired-tn *fixnum-primitive-type*
		  control-stack-arg-scn
		  old-fp-save-offset)
   env))
;;;
(defun make-return-pc-save-location (env)
  (environment-live-tn
   (make-wired-tn *any-primitive-type* control-stack-arg-scn lra-save-offset)
   env))

;;; Make-Argument-Count-Location  --  Interface
;;;
;;;    Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
;;;
(defun make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;; MAKE-NFP-TN  --  Interface
;;;
;;;    Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
;;;
(defun make-nfp-tn ()
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

;;; MAKE-STACK-POINTER-TN ()
;;; 
(defun make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; MAKE-NUMBER-STACK-POINTER-TN ()
;;; 
(defun make-number-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; Make-Unknown-Values-Locations  --  Interface
;;;
;;;    Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
;;;
(defun make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; Select-Component-Format  --  Interface
;;;
;;;    This function is called by the Entry-Analyze phase, allowing
;;; VM-dependent initialization of the IR2-Component structure.  We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
;;;
(defun select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (undefined-value))


;;;; Frame hackery:

;;; Used for setting up the Old-FP in local call.
;;;
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (move val fp-tn)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
;;;
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
	(inst addu val nfp
	      (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
		 vm:word-bytes))))))


(define-vop (xep-allocate-frame)
  (:info start-lab)
  (:vop-var vop)
  (:generator 1
    ;; Make sure the label is aligned.
    (align vm:lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst function-header-word)
    (dotimes (i (1- vm:function-header-code-offset))
      (inst word 0))
    (inst addu csp-tn fp-tn
	  (* vm:word-bytes (sb-allocated-size 'control-stack)))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
	(inst addu nsp-tn nsp-tn
	      (- (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
		    vm:word-bytes)))
	(move nfp-tn nsp-tn)))))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
	    (nfp :scs (any-reg)))
  (:info callee)
  (:generator 2
    (move res csp-tn)
    (inst addu csp-tn csp-tn
	  (* vm:word-bytes (sb-allocated-size 'control-stack)))
    (when (ir2-environment-number-stack-p callee)
      (inst addu nsp-tn nsp-tn
	    (- (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
		  vm:word-bytes)))
      (move nfp nsp-tn))))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst addu csp-tn csp-tn (* nargs vm:word-bytes)))))




;;; Default-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;;    Move-Temp and Nil-Temp are Descriptor-Reg TNs used as temporaries.
;;;
;;;    This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 4, whereas a return of other
;;; than one value returns directly at the return PC.
;;;
;;;    If 0 or 1 values are expected, then we just emit an instruction to reset
;;; the SP (which will only be executed when other than 1 value is returned.)
;;;
;;; In the general case, we have to do three things:
;;;  -- Default unsupplied register values.  This need only be done when a
;;;     single value is returned, since register values are defaulted by the
;;;     called in the non-single case.
;;;  -- Default unsupplied stack values.  This needs to be done whenever there
;;;     are stack values.
;;;  -- Reset SP.  This must be done whenever other than 1 value is returned,
;;;     regardless of the number of values desired.
;;;
;;; The general-case code looks like this:
#|
	b regs-defaulted		; Skip if MVs
	nop

	move a1 null-tn			; Default register values
	...
	loadi nargs 1			; Force defaulting of stack values
	move old-fp sp			; Set up args for SP resetting

regs-defaulted
	sub temp nargs register-arg-count

	bltz temp default-value-4	; jump to default code
        addu temp temp -1
	loadw move-temp old-fp-tn 3	; Move value to correct location.
	store-stack-tn val4-tn move-temp

	bltz temp default-value-5
        addu temp temp -1
	loadw move-temp old-fp-tn 4
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	move sp old-fp			; Reset SP.
<end of code>

<elsewhere>
default-value-4 
	store-stack-tn val4-tn null-tn	; Nil out 4'th value.

default-value-5
	store-stack-tn val5-tn null-tn	; Nil out 5'th value.

	...

	br defaulting-done
        nop
|#
;;;
(defun default-unknown-values (values nvals move-temp temp lra-label)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals) (type tn move-temp temp))
  (if (<= nvals 1)
      (progn
	(move csp-tn old-fp-tn)
	(inst nop)
	(inst compute-code-from-lra code-tn code-tn lra-label temp))
      (let ((regs-defaulted (gen-label))
	    (defaulting-done (gen-label)))
	;; Branch off to the MV case.
	(inst b regs-defaulted)
	(inst nop)
	
	;; Do the single value calse.
	(do ((i 1 (1+ i))
	     (val (tn-ref-across values) (tn-ref-across val)))
	    ((= i (min nvals register-arg-count)))
	  (move (tn-ref-tn val) null-tn))
	(when (> nvals register-arg-count)
	  (inst li nargs-tn (fixnum 1))
	  (move old-fp-tn csp-tn))
	
	(emit-label regs-defaulted)
	(inst compute-code-from-lra code-tn code-tn lra-label temp)
	
	(when (> nvals register-arg-count)
	  (inst addu temp nargs-tn (fixnum (- register-arg-count)))
	  (collect ((defaults))
	    (do ((i register-arg-count (1+ i))
		 (val (do ((i 0 (1+ i))
			   (val values (tn-ref-across val)))
			  ((= i register-arg-count) val))
		      (tn-ref-across val)))
		((null val))
	      
	      (let ((default-lab (gen-label))
		    (tn (tn-ref-tn val)))
		(defaults (cons default-lab tn))
		
		(inst bltz temp default-lab)
		(inst addu temp temp (fixnum -1))
		(loadw move-temp old-fp-tn i)
		(store-stack-tn tn move-temp)))
	    
	    (emit-label defaulting-done)
	    (move csp-tn old-fp-tn)
	    
	    (assemble (*elsewhere*)
	      (dolist (def (defaults))
		(emit-label (car def))
		(store-stack-tn (cdr def) null-tn))
	      (inst b defaulting-done)
	      (inst nop))))))
  (undefined-value))


;;;; Unknown values receiving:

;;; Receive-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return point for an unknown-values call for an
;;; arbitrary number of values.
;;;
;;;    We do the single and non-single cases with no shared code: there doesn't
;;; seem to be any potential overlap, and receiving a single value is more
;;; important efficiency-wise.
;;;
;;;    When there is a single value, we just push it on the stack, returning
;;; the old SP and 1.
;;;
;;;    When there is a variable number of values, we move all of the argument
;;; registers onto the stack, and return Args and Nargs.
;;;
;;;    Args and Nargs are TNs wired to the named locations.  We must
;;; explicitly allocate these TNs, since their lifetimes overlap with the
;;; results Start and Count (also, it's nice to be able to target them).
;;;
(defun receive-unknown-values (args nargs start count lra-label temp)
  (declare (type tn args nargs start count temp))
  (let ((variable-values (gen-label))
	(done (gen-label)))
    (inst b variable-values)
    (inst nop)
    
    (inst compute-code-from-lra code-tn code-tn lra-label temp)
    (inst addu csp-tn csp-tn 4)
    (storew (first register-arg-tns) csp-tn -1)
    (inst addu start csp-tn -4)
    (inst li count (fixnum 1))
    
    (emit-label done)
    
    (assemble (*elsewhere*)
      (emit-label variable-values)
      (inst compute-code-from-lra code-tn code-tn lra-label temp)
      (do ((arg register-arg-tns (rest arg))
	   (i 0 (1+ i)))
	  ((null arg))
	(storew (first arg) args i))
      (move start args)
      (move count nargs)
      (inst b done)
      (inst nop)))
  (undefined-value))


;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :offset old-fp-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset nargs-offset
	       :from :eval :to (:result 1))
	      nvals)
  (:temporary (:scs (non-descriptor-reg)) temp))



;;;; Local call with unknown values convention return:

;;; Non-TR local call for a fixed number of values passed according to the
;;; unknown values convention.
;;;
;;; Args are the argument passing locations, which are specified only to
;;; terminate their lifetimes in the caller.
;;;
;;; Values are the return value locations (wired to the standard passing
;;; locations).
;;;
;;; Save is the save info, which we can ignore since saving has been done.
;;; Return-PC is the TN that the return PC should be passed in.
;;; Target is a continuation pointing to the start of the called function.
;;; Nvals is the number of values received.
;;;
(define-vop (call-local)
  (:args (fp :scs (any-reg))
	 (nfp :scs (any-reg))
	 (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:ignore arg-locs args nfp)
  (:vop-var vop)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 5
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (move fp-tn fp)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (move callee-nfp nfp)))
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (inst b target)
      (inst nop)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (default-unknown-values values nvals move-temp temp label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp :scs (any-reg))
	 (nfp :scs (any-reg))
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:generator 20
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (move fp-tn fp)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (move callee-nfp nfp)))
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (inst b target)
      (inst nop)
      (emit-return-pc label)
      (note-this-location vop :unknown-return)
      (receive-unknown-values values-start nvals start count label temp)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
(define-vop (known-call-local)
  (:args (fp :scs (any-reg))
	 (nfp :scs (any-reg))
	 (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save)
  (:vop-var vop)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (let ((label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (move fp-tn fp)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (let ((callee-nfp (callee-nfp-tn callee)))
	(when callee-nfp
	  (move callee-nfp nfp)))
      (inst compute-lra-from-code
	    (callee-return-pc-tn callee) code-tn label temp)
      (inst b target)
      (inst nop)
      (note-this-location vop :known-return)
      (emit-return-pc label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
(define-vop (known-return)
  (:args (old-fp :scs (any-reg))
	 (return-pc :scs (descriptor-reg))
	 (vals :more t))
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (move csp-tn fp-tn)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(inst addu nsp-tn cur-nfp
	      (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
		 vm:word-bytes))))
    (inst addu lip return-pc (- vm:word-bytes vm:other-pointer-type))
    (inst j lip)
    (move fp-tn old-fp)))


;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.
;;;
;;; In full call, the arguments are passed creating a partial frame on the
;;; stack top and storing stack arguments into that frame.  On entry to the
;;; callee, this partial frame is pointed to by FP.  If there are no stack
;;; arguments, we don't bother allocating a partial frame, and instead set FP
;;; to SP just before the call.

;;; Define-Full-Call  --  Internal
;;;
;;;    This macro helps in the definition of full call VOPs by avoiding code
;;; replication in defining the cross-product VOPs.
;;;
;;; Name is the name of the VOP to define.
;;; 
;;; Named is true if the first argument is a symbol whose global function
;;; definition is to be called.
;;;
;;; Return is either :Fixed, :Unknown or :Tail:
;;; -- If :Fixed, then the call is for a fixed number of values, returned in
;;;    the standard passing locations (passed as result operands).
;;; -- If :Unknown, then the result values are pushed on the stack, and the
;;;    result values are specified by the Start and Count as in the
;;;    unknown-values continuation representation.
;;; -- If :Tail, then do a tail-recursive call.  No values are returned.
;;;    The Old-Fp and Return-PC are passed as the second and third arguments.
;;;
;;; In non-tail calls, the pointer to the stack arguments is passed as the last
;;; fixed argument.  If Variable is false, then the passing locations are
;;; passed as a more arg.  Variable is true if there are a variable number of
;;; arguments passed on the stack.  Variable cannot be specified with :Tail
;;; return.  TR variable argument call is implemented separately.
;;;
;;; In tail call with fixed arguments, the passing locations are passed as a
;;; more arg, but there is no new-FP, since the arguments have been set up in
;;; the current frame.
;;;
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
     (:args
      ,@(unless (eq return :tail)
	  '((new-fp :scs (any-reg) :to :eval)))

      ,(if named
	   '(name :scs (descriptor-reg) :target name-pass)
	   '(arg-fun :scs (descriptor-reg) :target lexenv :to :eval))
      
      ,@(when (eq return :tail)
	  '((old-fp :scs (any-reg) :target old-fp-pass)
	    (return-pc :scs (descriptor-reg) :target return-pc-pass)))
      
      ,@(unless variable '((args :more t))))

     ,@(when (eq return :fixed)
	 '((:results (values :more t))))
   
     ,@(unless (eq return :tail)
	 `((:save-p t)
	   ,@(unless variable
	       '((:move-args :full-call)))))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
	    ,@(unless variable '(nargs))
	    ,@(when (eq return :fixed) '(nvals)))

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
		  :offset old-fp-offset
		  :from (:argument 1)
		  :to :eval)
		 old-fp-pass)

     (:temporary (:sc descriptor-reg
		  :offset lra-offset
		  :from (:argument ,(if (eq return :tail) 2 1))
		  :to :eval)
		 return-pc-pass)

     ,@(when named 
	 `((:temporary (:sc descriptor-reg :offset cname-offset
			:from (:argument ,(if (eq return :tail) 0 1))
			:to :eval)
		       name-pass)))

     (:temporary (:sc descriptor-reg :offset lexenv-offset
		  :from (:argument ,(if (eq return :tail) 0 1)) :to :eval)
		 lexenv)

     (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
		 function)

     (:temporary (:sc descriptor-reg :offset nargs-offset :to :eval)
		 nargs-pass)

     ,@(when variable
	 (mapcar #'(lambda (name offset)
		     `(:temporary (:sc descriptor-reg
				   :offset ,offset
				   :to :eval)
			 ,name))
		 register-arg-names register-arg-offsets))
     ,@(when (eq return :fixed)
	 '((:temporary (:scs (descriptor-reg) :from :eval) move-temp)))

     ,@(unless (eq return :tail)
	 '((:temporary (:scs (non-descriptor-reg) :from :eval) temp)
	   (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)))

     (:temporary (:scs (interior-reg) :type interior) lip)

     (:generator ,(+ (if named 5 0)
		     (if variable 19 1)
		     (if (eq return :tail) 0 10)
		     15
		     (if (eq return :unknown) 25 0))
       
       ,@(if named
	     `((move name-pass name)
	       (loadw lexenv name-pass
		      vm:symbol-function-slot vm:other-pointer-type))
	     `((move lexenv arg-fun)))
       
       ,@(if variable
	     `((inst subu nargs-pass csp-tn new-fp)
	       ,@(let ((index -1))
		   (mapcar #'(lambda (name)
			       `(loadw ,name new-fp ,(incf index)))
			   register-arg-names)))
	     `((inst li nargs-pass (fixnum nargs))))
       
       (let ((cur-nfp (current-nfp-tn vop))
	     ,@(unless (eq return :tail)
		 '((lra-label (gen-label)))))
	 ,@(unless (eq return :tail)
	     `((inst compute-lra-from-code
		     return-pc-pass code-tn lra-label temp)))

	 (loadw function lexenv vm:closure-function-slot
		vm:function-pointer-type)
	 (inst addu lip function (- (ash vm:function-header-code-offset
					 vm:word-shift)
				    vm:function-pointer-type))

	 ,@(if (eq return :tail)
	       '((move old-fp-pass old-fp)
		 (move return-pc-pass return-pc)
		 (when cur-nfp
		   (inst addu nsp-tn cur-nfp
			 (* (logandc2 (1+ (sb-allocated-size
					   'non-descriptor-stack))
				      1)
			    vm:word-bytes)))
		 (inst j lip)
		 (move code-tn function))
	       `((move old-fp-pass fp-tn)
		 (when cur-nfp
		   (store-stack-tn nfp-save cur-nfp))
		 ,(if variable
		      '(move fp-tn new-fp)
		      '(if (> nargs register-arg-count)
			   (move fp-tn new-fp)
			   (move fp-tn csp-tn)))
		 (inst j lip)
		 (move code-tn function)
		 (emit-return-pc lra-label)))

	 ,@(ecase return
	     (:fixed
	      '((note-this-location vop :unknown-return)
		(default-unknown-values values nvals move-temp temp lra-label)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:unknown
	      '((note-this-location vop :unknown-return)
		(receive-unknown-values values-start nvals start count
					lra-label temp)
		(when cur-nfp
		  (load-stack-tn cur-nfp nfp-save))))
	     (:tail))))))


(define-full-call call nil :fixed nil)
(define-full-call call-named t :fixed nil)
(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.
;;;
(expand
 `(define-vop (tail-call-variable)
    (:args
     (args :scs (any-reg) :to (:result 0))
     (function-arg :scs (descriptor-reg) :target lexenv)
     (old-fp-arg :scs (any-reg) :target old-fp)
     (return-pc-arg :scs (descriptor-reg) :target return-pc))
    (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 0))
		lexenv)
    (:temporary (:sc any-reg :offset old-fp-offset :from (:argument 1))
		old-fp)
    (:temporary (:sc any-reg :offset lra-offset :from (:argument 2))
		return-pc)
    (:temporary (:sc any-reg :offset nargs-offset) nargs)
    (:temporary (:scs (descriptor-reg)) function)
    ,@(mapcar #'(lambda (offset name)
		  `(:temporary (:sc any-reg :offset ,offset) ,name))
	      register-arg-offsets register-arg-names)
    (:temporary (:scs (any-reg) :type fixnum) src dst count)
    (:temporary (:scs (descriptor-reg)) temp)
    (:temporary (:scs (interior-reg) :type interior) lip)
    (:vop-var vop)
    (:generator 75
      (let ((loop (gen-label))
	    (test (gen-label)))
	;; Move these into the passing locations if they are not already there.
	(move lexenv function-arg)
	(move old-fp old-fp-arg)
	(move return-pc return-pc-arg)

	;; Calculate NARGS (as a fixnum)
	(inst subu nargs csp-tn args)
	
	;; Load the argument regs (must do this now, 'cause the blt might
	;; trash these locations)
	,@(let ((index -1))
	    (mapcar #'(lambda (name)
			`(loadw ,name args ,(incf index)))
		    register-arg-names))
	
	;; Calc SRC, DST, and COUNT
	(inst addu src args (* vm:word-bytes register-arg-count))
	(inst addu dst fp-tn (* vm:word-bytes register-arg-count))
	(inst b test)
	(inst addu count nargs (fixnum (- register-arg-count)))
	
	(emit-label loop)
	;; Copy one arg.
	(loadw temp src)
	(inst addu src src vm:word-bytes)
	(storew temp dst)
	(inst addu dst dst vm:word-bytes)
	
	;; Are we done?
	(emit-label test)
	(inst bgtz count loop)
	(inst addu count count (fixnum -1))
	
	;; Clear the number stack if anything is there.
	(let ((cur-nfp (current-nfp-tn vop)))
	  (when cur-nfp
	    (inst addu nsp-tn cur-nfp
		  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack))
			       1)
		     vm:word-bytes))))

	;; We are done.  Do the jump.
	(loadw function lexenv vm:closure-function-slot
	       vm:function-pointer-type)
	(lisp-jump function lip)))))


;;;; Unknown values return:


;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; FP and jump to the single-value entry at Return-PC + 4.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore FP and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
;;;
(expand
 `(define-vop (return)
    (:args
     (old-fp :scs (any-reg))
     (return-pc :scs (descriptor-reg) :to (:eval 1))
     (values :more t))
    (:ignore values)
    (:info nvals)
    ,@(mapcar #'(lambda (name offset)
		  `(:temporary (:sc descriptor-reg :offset ,offset
				    :from (:eval 0) :to (:eval 1))
			       ,name))
	      register-arg-names register-arg-offsets)
    (:temporary (:sc any-reg :offset nargs-offset) nargs)
    (:temporary (:sc any-reg :offset old-fp-offset) val-ptr)
    (:temporary (:scs (interior-reg) :type interior) lip)
    (:vop-var vop)
    (:generator 6
      (cond ((= nvals 1)
	     ;; Clear the stacks.
	     (let ((cur-nfp (current-nfp-tn vop)))
	       (when cur-nfp
		 (inst addu nsp-tn cur-nfp
		       (* (logandc2 (1+ (sb-allocated-size
					 'non-descriptor-stack))
				    1)
			  vm:word-bytes))))
	     (move csp-tn fp-tn)
	     ;; Reset the frame pointer.
	     (move fp-tn old-fp)
	     ;; Out of here.
	     (inst addu lip return-pc (- (* 3 word-bytes) other-pointer-type))
	     (inst j lip)
	     (move code-tn return-pc))
	    (t
	     (inst li nargs (fixnum nvals))
	     ;; Clear the number stack.
	     (let ((cur-nfp (current-nfp-tn vop)))
	       (when cur-nfp
		 (inst addu nsp-tn cur-nfp
		       (* (logandc2 (1+ (sb-allocated-size
					 'non-descriptor-stack))
				    1)
			  vm:word-bytes))))
	     (move val-ptr fp-tn)
	     ;; Reset the frame pointer.
	     (move fp-tn old-fp)
	     
	     (inst addu csp-tn val-ptr (* nvals word-bytes))
	     
	     ,@(let ((index 0))
		 (mapcar #'(lambda (name)
			     `(when (< nvals ,(incf index))
				(move ,name null-tn)))
			 register-arg-names))
	     
	     (lisp-return return-pc lip))))))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls a miscop.
;;;
;;; The Return-Multiple miscop uses a non-standard calling convention.  For one
;;; thing, it doesn't return.  We only use BALA because there isn't a BA
;;; instruction.   Also, we don't use A0..A2 for argument passing, since the
;;; miscop will want to load these with values off of the stack.  Instead, we
;;; pass Old-Fp, Start and Count in the normal locations for these values.
;;; Return-PC is passed in A3 since PC is trashed by the BALA. 
;;;


(expand
 (let ((label-names (mapcar #'(lambda (name)
				(intern (concatenate 'simple-string
						     "DEFAULT-"
						     (string name)
						     "-AND-ON")))
			    register-arg-names)))
   `(define-vop (return-multiple)
      (:args
       (old-fp :scs (any-reg) :to (:eval 1))
       (return-pc :scs (descriptor-reg) :to (:eval 1))
       (start :scs (any-reg) :target src)
       (nvals :scs (any-reg) :target nargs))
      (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)) nargs)
      ,@(mapcar #'(lambda (name offset)
		    `(:temporary (:sc descriptor-reg :offset ,offset
				      :from (:eval 0) :to (:eval 1))
				 ,name))
		register-arg-names register-arg-offsets)
      (:temporary (:sc any-reg :offset old-fp-offset :type fixnum) vals)
      (:temporary (:scs (any-reg) :type fixnum) count dst)
      (:temporary (:scs (any-reg) :type fixnum :from (:argument 2)) src)
      (:temporary (:scs (descriptor-reg)) temp)
      (:temporary (:scs (interior-reg) :type interior) lip)
      (:vop-var vop)
      (:generator 13
	(let ((not-single (gen-label))
	      (loop (gen-label))
	      ,@(mapcar #'(lambda (name)
			    `(,name (gen-label)))
			label-names)
	      (done (gen-label)))
	  
	  ;; Clear the number stack.
	  (let ((cur-nfp (current-nfp-tn vop)))
	    (when cur-nfp
	      (inst addu nsp-tn cur-nfp
		    (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack))
				 1)
		       vm:word-bytes))))

	  ;; Single case?
	  (inst li count (fixnum 1))
	  (inst bne count nvals not-single)
	  (inst nop)
	  
	  ;; Return with one value.
	  (loadw ,(first register-arg-names) start)
	  (move csp-tn fp-tn)
	  (move fp-tn old-fp)
	  ;; Note: we can't use the lisp-return macro, 'cause we want to
	  ;; skip two instructions.
	  (inst addu lip return-pc (- (* 3 word-bytes) other-pointer-type))
	  (inst j lip)
	  (move code-tn return-pc)

	  ;; Nope, not the single case.
	  (emit-label not-single)
	  
	  ;; Load the register args, bailing out when we are done.
	  (move nargs nvals)
	  (move count nvals)
	  (move src start)
	  ,@(mapcar #'(lambda (name label)
			`(progn
			   (inst blez count ,label)
			   (inst addu count count (fixnum -1))
			   (loadw ,name src)
			   (inst addu src src vm:word-bytes)))
		    register-arg-names
		    label-names)
	  
	  ;; Copy the remaining args to the top of the stack.
	  (inst addu dst fp-tn (* vm:word-bytes register-arg-count))
	  
	  (emit-label loop)
	  (inst blez count done)
	  (inst addu count count (fixnum -1))
	  (loadw temp src)
	  (inst addu src src vm:word-bytes)
	  (storew temp dst)
	  (inst b loop)
	  (inst addu dst dst vm:word-bytes)
	  
	  ;; Default some number of registers.
	  ,@(mapcar #'(lambda (name label)
			`(progn
			   (emit-label ,label)
			   (move ,name null-tn)))
		    register-arg-names label-names)
	  
	  ;; Clear the stack.
	  (emit-label done)
	  (move vals fp-tn)
	  (inst addu csp-tn vals nargs)
	  (move fp-tn old-fp)
	  
	  ;; Return.
	  (lisp-return return-pc lip))))))




;;;; XEP hackery:


;;; Fetch the constant pool from the function entry structure.
;;;
(define-vop (setup-environment)
  (:info label)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    ;; Fix CODE, cause the function object was passed in.
    (inst compute-code-from-fn code-tn code-tn label temp)))

;;; Return the current Env as our result, then indirect throught the closure
;;; and the closure-entry to find the constant pool
;;;
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
	       :to (:result 0))
	      lexenv)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:generator 6
    ;; Fix CODE, cause the function object was passed in.
    (inst compute-code-from-fn code-tn code-tn label temp)
    ;; Get result.
    (move closure lexenv)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments. 
;;;
(define-vop (copy-more-arg)
  (:temporary (:sc any-reg :offset nl0-offset) result)
  (:temporary (:sc any-reg :offset nl1-offset) count)
  (:temporary (:sc any-reg :offset nl2-offset) src)
  (:temporary (:sc any-reg :offset nl3-offset) dst)
  (:temporary (:sc descriptor-reg :offset l0-offset) temp)
  (:info fixed)
  (:generator 20
    (let ((loop (gen-label))
	  (do-regs (gen-label))
	  (done (gen-label)))
      (when (< fixed register-arg-count)
	;; Save a pointer to the results so we can fill in register args.
	;; We don't need this if there are more fixed args than reg args.
	(move result csp-tn))
      ;; Allocate the space on the stack.
      (cond ((zerop fixed)
	     (inst addu csp-tn csp-tn nargs-tn)
	     (inst beq nargs-tn done)
	     (inst nop))
	    (t
	     (inst addu count nargs-tn (fixnum (- fixed)))
	     (inst blez count done)
	     (inst nop)
	     (inst addu csp-tn csp-tn count)))
      (when (< fixed register-arg-count)
	;; We must stop when we run out of stack args, not when we run out of
	;; more args.
	(inst addu count nargs-tn (fixnum (- register-arg-count))))
      ;; Everything of interest in registers.
      (inst blez count do-regs)
      ;; Initialize dst to be end of stack.
      (move dst csp-tn)
      ;; Initialize src to be end of args.
      (inst addu src fp-tn nargs-tn)

      (emit-label loop)
      ;; *--dst = *--src, --count
      (inst addu src src (- vm:word-bytes))
      (inst addu count count (fixnum -1))
      (loadw temp src)
      (inst addu dst dst (- vm:word-bytes))
      (inst bgtz count loop)
      (storew temp dst)

      (emit-label do-regs)
      (when (< fixed register-arg-count)
	;; Now we have to deposit any more args that showed up in registers.
	(inst addu count nargs-tn (fixnum (- fixed)))
	(do ((i fixed (1+ i)))
	    ((>= i register-arg-count))
	  ;; Don't deposit any more than there are.
	  (inst beq count zero-tn done)
	  (inst addu count count (fixnum -1))
	  ;; Store it relative to the pointer saved at the start.
	  (storew (nth i register-arg-tns) result (- i fixed))))
      (emit-label done))))


;;; More args are stored consequtively on the stack, starting immediately at
;;; the context pointer.  The context pointer is not typed, so the lowtag is 0.
;;;
(define-vop (more-arg word-index-ref)
  (:variant 0 0)
  (:translate %more-arg))


;;; Turn more arg (context, count) into a list.
;;;
(define-vop (listify-rest-args)
  (:args (context-arg :target context :scs (descriptor-reg))
	 (count-arg :target count :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:scs (any-reg) :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :from :eval) temp)
  (:temporary (:scs (non-descriptor-reg) :from :eval) ndescr dst)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:generator 20
    (let ((enter (gen-label))
	  (loop (gen-label))
	  (done (gen-label)))
      (move context context-arg)
      (move count count-arg)
      ;; Check to see if there are any arguments.
      (inst beq count zero-tn done)
      (move result null-tn)

      ;; We need to do this atomically.
      (pseudo-atomic (ndescr)
	;; Allocate a cons (2 words) for each item.
	(inst addu result alloc-tn vm:list-pointer-type)
	(move dst result)
	(inst addu alloc-tn alloc-tn count)
	(inst b enter)
	(inst addu alloc-tn alloc-tn count)

	;; Store the current cons in the cdr of the previous cons.
	(emit-label loop)
	(storew dst dst -1 vm:list-pointer-type)

	;; Grab one value and stash it in the car of this cons.
	(emit-label enter)
	(loadw temp context)
	(inst addu context context vm:word-bytes)
	(storew temp dst 0 vm:list-pointer-type)

	;; Dec count, and if != zero, go back for more.
	(inst addu count count (fixnum -1))
	(inst bne count zero-tn loop)
	(inst addu dst dst (* 2 vm:word-bytes))

	;; NIL out the last cons.
	(storew null-tn dst -1 vm:list-pointer-type))
      (emit-label done))))



;;; Return the location and size of the more arg glob created by Copy-More-Arg.
;;; Supplied is the total number of arguments supplied (originally passed in
;;; NARGS.)  Fixed is the number of non-rest arguments.
;;;
;;; We must duplicate some of the work done by Copy-More-Arg, since at that
;;; time the environment is in a pretty brain-damaged state, preventing this
;;; info from being returned as values.  What we do is compute
;;; supplied - fixed, and return a pointer that many words below the current
;;; stack top.
;;;
(define-vop (more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info fixed)
  (:results
   (context :scs (descriptor-reg))
   (count :scs (any-reg)))
  (:generator 5
    (inst addu count supplied (fixnum (- fixed)))
    (inst subu context csp-tn count)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:info count)
  (:generator 3
    (let ((err-lab (generate-error-code invalid-argument-count-error
					nargs)))
      (cond ((zerop count)
	     (inst bne nargs zero-tn err-lab)
	     (inst nop))
	    (t
	     (inst li temp (fixnum count))
	     (inst bne nargs temp err-lab)
	     (inst nop))))))

;;; Signal an argument count error.
;;;
(macrolet ((frob (name error &rest args)
	     `(define-vop (,name)
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:generator 1000
		  (error-call ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error nargs)
  (frob type-check-error object-not-type-error object type)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error key))
