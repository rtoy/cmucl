;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/call.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition of function call for the ARM.
;;;
;;;
(in-package "ARM")
(intl:textdomain "cmucl-arm-vm")


;;;; Interfaces to IR2 conversion:

;;; Standard-Argument-Location  --  Interface
;;;
;;;    Return a wired TN describing the N'th full call argument passing
;;; location.
;;;
(def-vm-support-routine standard-argument-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *any-primitive-type* register-arg-scn
		     (elt register-arg-offsets n))
      (make-wired-tn *any-primitive-type* control-stack-arg-scn n)))


;;; Make-Return-PC-Passing-Location  --  Interface
;;;
;;;    Make a passing location TN for a local call return PC.  If standard is
;;; true, then use the standard (full call) location, otherwise use any legal
;;; location.  Even in the non-standard case, this may be restricted by a
;;; desire to use a subroutine call instruction.
;;;
(def-vm-support-routine make-return-pc-passing-location (standard)
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
(def-vm-support-routine make-old-fp-passing-location (standard)
  (if standard
      (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset)
      (make-normal-tn *fixnum-primitive-type*)))

;;; Make-Old-FP-Save-Location, Make-Return-PC-Save-Location  --  Interface
;;;
;;;    Make the TNs used to hold Old-FP and Return-PC within the current
;;; function.  We treat these specially so that the debugger can find them at a
;;; known location.
;;;
(def-vm-support-routine make-old-fp-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *fixnum-primitive-type*) env)
   (make-wired-tn *fixnum-primitive-type*
		  control-stack-arg-scn
		  ocfp-save-offset)))
;;;
(def-vm-support-routine make-return-pc-save-location (env)
  (specify-save-tn
   (environment-debug-live-tn (make-normal-tn *any-primitive-type*) env)
   (make-wired-tn *any-primitive-type*
		  control-stack-arg-scn
		  lra-save-offset)))

;;; Make-Argument-Count-Location  --  Interface
;;;
;;;    Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
;;;
(def-vm-support-routine make-argument-count-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nargs-offset))


;;; MAKE-NFP-TN  --  Interface
;;;
;;;    Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
;;;


;; FIXME: What are we using for nfp? It's not defined.
(def-vm-support-routine make-nfp-tn ()
  ;; FIXME: Is this right?  We currently don't have an NFP register on
  ;; ARM.  Instead, a static symbol is used to hold the value of the
  ;; number frame pointer.  This is just a general TN that can be used
  ;; as a marker to note that the VOP might have a NFP value.  (Some
  ;; vops check the value of (CURRENT-NFP-TN).)
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset))
  #+nil
  (make-restricted-tn *fixnum-primitive-type* immediate-arg-scn)
  )

;;; MAKE-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; MAKE-NUMBER-STACK-POINTER-TN ()
;;; 
(def-vm-support-routine make-number-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

;;; Make-Unknown-Values-Locations  --  Interface
;;;
;;;    Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
;;;
(def-vm-support-routine make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
	(make-normal-tn *fixnum-primitive-type*)))


;;; Select-Component-Format  --  Interface
;;;
;;;    This function is called by the Entry-Analyze phase, allowing
;;; VM-dependent initialization of the IR2-Component structure.  We push
;;; placeholder entries in the Constants to leave room for additional
;;; noise in the code object header.
;;;
(def-vm-support-routine select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
			(ir2-component-constants (component-info component))))
  (undefined-value))


;;;; Frame hackery:

;;; BYTES-NEEDED-FOR-NON-DESCRIPTOR-STACK-FRAME -- internal
;;;
;;; Return the number of bytes needed for the current non-descriptor
;;; stack frame.  Non-descriptor stack frames must be multiples of 8
;;; bytes on the ARM (at a public interface). See section 5.2.1.1-2 in
;;; IHI0042D AAPCS manual.
;;; 
(defun bytes-needed-for-non-descriptor-stack-frame ()
  (* (logandc2 (1+ (sb-allocated-size 'non-descriptor-stack)) 1)
     vm:word-bytes))


;;; Used for setting up the Old-FP in local call.
;;;
(define-vop (current-fp)
  (:results (val :scs (any-reg)))
  (:generator 1
    (emit-not-implemented)
    (move val cfp-tn)))

;;; Used for computing the caller's NFP for use in known-values return.  Only
;;; works assuming there is no variable size stuff on the nstack.
;;;
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :offset lip-offset) temp)
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
	(emit-not-implemented)
	(load-symbol-value temp *number-frame-pointer*)
	(inst add val temp (bytes-needed-for-non-descriptor-stack-frame))))))


(define-vop (xep-allocate-frame)
  (:info start-lab copy-more-arg-follows)
  (:ignore copy-more-arg-follows)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 1
    ;; Make sure the function is aligned, and drop a label pointing to this
    ;; function header.
    (align vm:lowtag-bits)
    (emit-label start-lab)
    ;; Allocate function header.
    (inst function-header-word)
    (dotimes (i (1- vm:function-code-offset))
      (inst udf function-header-trap))

    (emit-not-implemented)

    ;; The start of the actual code.
    ;; Fix CODE, cause the function object was passed in.
    (inst compute-code-from-fn code-tn code-tn start-lab temp)

    ;; Build our stack frames.
    (let ((size (* vm:word-bytes (sb-allocated-size 'control-stack))))
      ;; Micro-optimize: If size is small enough, this can be
      ;; optimized to a single sub instruction.
      (inst li temp size)
      (inst sub csp-tn cfp-tn temp))
    (let ((nfp-tn (current-nfp-tn vop)))
      (when nfp-tn
	(not-implemented "XEP-ALLOCATE-FRAME-WITH-NFP")
	(load-symbol-value temp *number-frame-pointer*)
	(inst sub temp temp (bytes-needed-for-non-descriptor-stack-frame))
	(inst add temp temp number-stack-displacement)
	(store-symbol-value temp *number-frame-pointer*)))

    (trace-table-entry trace-table-normal)))

(define-vop (allocate-frame)
  (:results (res :scs (any-reg))
	    (nfp :scs (any-reg)))
  (:info callee #+nil clear-memory-p)
  (:generator 2
    (trace-table-entry trace-table-function-prologue)
    (emit-not-implemented)))

;;; Allocate a partial frame for passing stack arguments in a full call.  Nargs
;;; is the number of arguments passed.  If no stack arguments are passed, then
;;; we don't have to do anything.
;;;
(define-vop (allocate-full-call-frame)
  (:info nargs)
  (:results (res :scs (any-reg)))
  (:generator 2
    (emit-not-implemented)
    (when (> nargs register-arg-count)
      (move res csp-tn)
      (inst add csp-tn csp-tn (* nargs vm:word-bytes)))))




;;; Default-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).
;;;
;;;    Move-Temp is a Descriptor-Reg TN used as a temporary.
;;;
;;;    This code exploits the fact that in the unknown-values convention, a
;;; single value return returns at the return PC + 8, whereas a return of other
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
	move old-fp csp			; Set up args for SP resetting

regs-defaulted
	subcc temp nargs register-arg-count

	b :lt default-value-7	; jump to default code
	loadw move-temp ocfp-tn 6	; Move value to correct location.
        subcc temp 1
	store-stack-tn val4-tn move-temp

	b :lt default-value-8
	loadw move-temp ocfp-tn 7
        subcc temp 1
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	move csp ocfp			; Reset SP.
<end of code>

<elsewhere>
default-value-7
	store-stack-tn val4-tn null-tn	; Nil out 7'th value. (first on stack)

default-value-8
	store-stack-tn val5-tn null-tn	; Nil out 8'th value.

	...

	br defaulting-done
        nop
|#
;;;
(defun default-unknown-values (vop values nvals move-temp temp lra-label)
  (declare (type (or tn-ref null) values)
	   (type unsigned-byte nvals) (type tn move-temp temp))
  (if (<= nvals 1)
      (progn
	(new-assem:without-scheduling ()
	  (note-this-location vop :single-value-return)
	  (not-implemented default-unknown-values-1))
	(inst compute-code-from-lra code-tn code-tn lra-label temp))
      (let ((regs-defaulted (gen-label))
	    (defaulting-done (gen-label))
	    (default-stack-vals (gen-label)))
	;; Branch off to the MV case.
	(new-assem:without-scheduling ()
	  (note-this-location vop :unknown-return)
	  (not-implemented default-unknown-values-multi-branch))
	
	;; Do the single value calse.
	(do ((i 1 (1+ i))
	     (val (tn-ref-across values) (tn-ref-across val)))
	    ((= i (min nvals register-arg-count)))
	  (move (tn-ref-tn val) null-tn))
	(when (> nvals register-arg-count)
	  (not-implemented default-unknown-values-single-value))
	
	(emit-label regs-defaulted)
	(when (> nvals register-arg-count)
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

		(not-implemented default-unknown-values-multi)))
	    
	    (emit-label defaulting-done)
	    (move csp-tn ocfp-tn)
	    
	    (let ((defaults (defaults)))
	      (when defaults
		(assemble (*elsewhere*)
		  (emit-label default-stack-vals)
		  (trace-table-entry trace-table-function-prologue)
		  (do ((remaining defaults (cdr remaining)))
		      ((null remaining))
		    (let ((def (car remaining)))
		      (emit-label (car def))
		      (when (null (cdr remaining))
			(inst b defaulting-done))
		      (store-stack-tn (cdr def) null-tn)))
		  (trace-table-entry trace-table-normal))))))

	(inst compute-code-from-lra code-tn code-tn lra-label temp)))
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
    (new-assem:without-scheduling ()
      (not-implemented receive-unknown-values-pre))

    (not-implemented receive-unknown-values-body)
    
    (assemble (*elsewhere*)
      (trace-table-entry trace-table-function-prologue)
      (emit-label variable-values)
      (not-implemented receive-unknown-values-post)
      (trace-table-entry trace-table-normal)))
  (undefined-value))


;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (any-reg))
   (count :scs (any-reg))))



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
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info arg-locs callee target nvals)
  (:vop-var vop)
  (:ignore arg-locs args ocfp)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))


;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:save-p t)
  (:move-args :local-call)
  (:info save callee target)
  (:ignore args save)
  (:vop-var vop)
  (:generator 20
    (trace-table-entry trace-table-call-site)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-call-local)
  (:args (fp)
	 (nfp)
	 (args :more t))
  (:results (res :more t))
  (:move-args :local-call)
  (:save-p t)
  (:info save callee target)
  (:ignore args res save)
  (:vop-var vop)
  (:generator 5
    (trace-table-entry trace-table-call-site)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore FP and CSP and jump to the Return-PC.
;;;
;;; Note: we can't use normal load-tn allocation for the fixed args, since all
;;; registers may be tied up by the more operand.  Instead, we use
;;; MAYBE-LOAD-STACK-TN.
;;;
(define-vop (known-return)
  (:args (old-fp)
	 (return-pc)
	 (vals :more t))
  (:move-args :known-return)
  (:info val-locs)
  (:ignore val-locs vals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))


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
	   '(name)
	   '(arg-fun))
      
      ,@(when (eq return :tail)
	  '((old-fp)
	    (return-pc)))
      
      ,@(unless variable '((args :more t :scs (descriptor-reg)))))

     ,@(when (eq return :fixed)
	 '((:results (values :more t))))
   
     (:save-p ,(if (eq return :tail) :compute-only t))

     ,@(unless (or (eq return :tail) variable)
	 '((:move-args :full-call)))

     (:vop-var vop)
     (:info ,@(unless (or variable (eq return :tail)) '(arg-locs))
	    ,@(unless variable '(nargs))
	    ,@(when (eq return :fixed) '(nvals)))

     (:ignore
      ,@(unless (or variable (eq return :tail)) '(arg-locs))
      ,@(unless variable '(args)))
     (:generator ,(+ (if named 5 0)
		     (if variable 19 1)
		     (if (eq return :tail) 0 10)
		     15
		     (if (eq return :unknown) 25 0))
       (trace-table-entry trace-table-call-site)
       (emit-not-implemented)
       (trace-table-entry trace-table-normal))))


(define-full-call call nil :fixed nil)
;;(define-full-call call-named t :fixed nil)

;; Based on rt version.
#+nil
(define-vop (call-named)
  (:args (new-fp :scs (any-reg) :to :eval)
	 (name :scs (descriptor-reg) :target name-pass)
	 (args :more t :scs (descriptor-reg)))
  (:results (values :more t))
  (:save-p t)
  (:move-args :full-call)
  (:vop-var vop)
  (:info arg-locs nargs nvals) (:ignore arg-locs args)
  (:temporary
   (:sc descriptor-reg :offset ocfp-offset :from (:argument 1) :to :eval)
   old-fp-pass)
  (:temporary
   (:sc descriptor-reg :offset lra-offset :from (:argument 1) :to :eval)
   return-pc-pass)
  (:temporary
   (:sc descriptor-reg :offset cname-offset :from (:argument 1) :to :eval)
   name-pass)
  (:temporary (:sc descriptor-reg :offset nargs-offset :to :eval) nargs-pass)
  (:temporary (:scs (descriptor-reg) :from :eval) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 31
    (let ((cur-nfp (current-nfp-tn vop))
	  (lra-label (gen-label)))
      (inst li nargs-pass (fixnumize nargs))
      (sc-case name
	(descriptor-reg
	 (move name-pass name))
	(control-stack
	 (loadw name-pass cfp-tn (tn-offset name) 0 temp))
	(constant
	 (loadw name-pass code-tn (tn-offset name)
	     vm:other-pointer-type temp)))
      (loadw lip name-pass vm:fdefn-raw-addr-slot vm:other-pointer-type)
      (inst compute-lra-from-code return-pc-pass code-tn lra-label temp)
      (move old-fp-pass cfp-tn)
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (if (> nargs register-arg-count)
	  (move cfp-tn new-fp)
	  (move cfp-tn csp-tn))
      (inst bx lip)
      (emit-return-pc lra-label)
      (note-this-location vop :unknown-return)
      (default-unknown-values vop values nvals move-temp temp lra-label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

;; Sparc version
#+nil
(define-vop (call-named)
  (:args (new-fp :scs (any-reg) :to :eval)
	 (name :scs (descriptor-reg control-stack) :target name-pass)
	 (args :more t :scs (descriptor-reg)))
  (:results (values :more t))
  (:save-p t)
  (:move-args :full-call)
  (:vop-var vop)
  (:info arg-locs nargs nvals)
  (:ignore arg-locs args)
  (:temporary (:sc descriptor-reg :offset ocfp-offset :from (:argument 1))
	      old-fp-pass)
  (:temporary
   (:sc descriptor-reg :offset lra-offset :from (:argument 1) :to :eval)
   return-pc-pass)
  (:temporary
   (:sc descriptor-reg :offset cname-offset :from (:argument 1) :to :eval)
   name-pass)
  (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval) function)
  (:temporary (:sc any-reg :offset nargs-offset :to :eval) nargs-pass)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from :eval) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:generator 31
    (trace-table-entry trace-table-call-site)
    (let* ((cur-nfp (current-nfp-tn vop))
	   (lra-label (gen-label))
	   (filler
	     (remove nil
		     (list :load-nargs :comp-lra
			   (when cur-nfp
			     :frob-nfp)
			   :save-fp :load-fp))))

      (flet ((do-next-filler ()
	       (let* ((next (pop filler))
		      (what (if (consp next) (car next) next)))
		 (ecase what
		   (:load-nargs (inst li nargs-pass (fixnumize nargs)))
		   (:comp-lra
		    (inst compute-lra-from-code return-pc-pass code-tn lra-label
			  temp))
		   (:frob-nfp
		    (describe cur-nfp *debug-io*)
		    (store-stack-tn nfp-save cur-nfp))
		   (:save-fp (inst mov old-fp-pass cfp-tn))
		   (:load-fp
		    (if (> nargs register-arg-count)
			(move cfp-tn new-fp)
			(move cfp-tn csp-tn)))
		   ((nil))))))
	(sc-case name
	  (descriptor-reg
	   (move name-pass name))
	  (control-stack
	   (loadw name-pass cfp-tn (tn-offset name) 0 temp)
	   (do-next-filler))
	  (constant
	   (loadw name-pass code-tn (tn-offset name) vm:other-pointer-type temp)
	   (do-next-filler)))
	(loadw function name-pass fdefn-raw-addr-slot other-pointer-type)
	(do-next-filler)
	(loop
	  (if filler (do-next-filler) (return)))
	(note-this-location vop :call-site)
	(inst mov code-tn function)
	(inst add lip function
	      (- (ash vm:function-code-offset vm:word-shift)
		 vm:function-pointer-type))
	(inst bx lip))
      (emit-return-pc lra-label)
      (default-unknown-values vop values nvals move-temp temp lra-label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))
  (trace-table-entry trace-table-normal)))

;; Based on x86 version
#+nil
(define-vop (call-named)
  (:args (new-fp :scs (any-reg) :to (:argument 1))
	 (name :scs (descriptor-reg control-stack) :target name-pass :to
              (:argument 0))
	 (args :more t :scs (descriptor-reg)))
  (:results (values :more t))
  (:save-p t)
  (:move-args :full-call)
  (:vop-var vop)
  (:info arg-locs nargs nvals)
  (:ignore arg-locs args)
  (:temporary
   (:sc descriptor-reg :offset cname-offset :from (:argument 0) :to
	:eval)
   name-pass)
  (:temporary (:sc unsigned-reg :offset ecx-offset :to :eval) ecx)
  (:generator 31 (trace-table-entry trace-table-call-site)
    (move eax name)
    (if (zerop nargs)
	(inst xor ecx ecx)
	(inst mov ecx (fixnumize nargs)))
    nil (storew ebp-tn new-fp (- (1+ ocfp-save-offset)))
    (move ebp-tn new-fp) (note-this-location vop :call-site)
    (inst call
	  (make-ea :dword :base
		   eax
		   :disp (- (* fdefn-raw-addr-slot word-bytes)
			    other-pointer-type)))
    (default-unknown-values vop values nvals)
    (trace-table-entry trace-table-normal)))

;; RT
(define-vop (call-named)
  (:args (new-fp :scs (any-reg) :to :eval)
	 (name :scs (descriptor-reg) :target name-pass)
	 (args :more t :scs (descriptor-reg)))
  (:results (values :more t))
  (:save-p t)
  (:move-args :full-call)
  (:vop-var vop)
  (:info arg-locs nargs nvals)
  (:ignore arg-locs args)
  (:temporary
   (:sc descriptor-reg :offset ocfp-offset :from (:argument 1) :to
	:eval)
   old-fp-pass)
  (:temporary
   (:sc descriptor-reg :offset lra-offset :from (:argument 1) :to
	:eval)
   return-pc-pass)
  (:temporary
   (:sc descriptor-reg :offset cname-offset :from (:argument 1) :to
	:eval)
   name-pass)
  (:temporary (:sc descriptor-reg :offset nargs-offset :to :eval)
	      nargs-pass)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from :eval) move-temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:generator 31
    (let ((cur-nfp (current-nfp-tn vop)) (lra-label (gen-label)))
      (inst li nargs-pass (fixnumize nargs))
      (move name-pass name)
      (loadw lip
	  name-pass
	  fdefn-raw-addr-slot
	  other-pointer-type)
      (inst compute-lra-from-code return-pc-pass code-tn lra-label temp)
      (move old-fp-pass cfp-tn)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (if (> nargs register-arg-count)
	  (move cfp-tn new-fp)
	  (move cfp-tn csp-tn))
      (inst bx lip)
      (emit-return-pc lra-label)
      (note-this-location vop :unknown-return)
      (default-unknown-values vop values nvals move-temp temp lra-label)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save)))))

(define-full-call multiple-call nil :unknown nil)
(define-full-call multiple-call-named t :unknown nil)
(define-full-call tail-call nil :tail nil)
(define-full-call tail-call-named t :tail nil)

(define-full-call call-variable nil :fixed t)
(define-full-call multiple-call-variable nil :unknown t)


;;; Defined separately, since needs special code that BLT's the arguments
;;; down.
;;;
(define-vop (tail-call-variable)
  (:args
   (args-arg :scs (any-reg) #+nil :target #+nil args)
   (function-arg :scs (descriptor-reg) #+nil :target #+nil lexenv)
   (old-fp-arg :scs (any-reg) #+nil :target #+nil old-fp)
   (lra-arg :scs (descriptor-reg) #+nil :target #+nil lra))
  (:vop-var vop)

  (:generator 75

    (emit-not-implemented)))


;;;; Unknown values return:


;;; Return a single value using the unknown-values convention.
;;; 
(define-vop (return-single)
  (:args (old-fp :scs (any-reg))
	 (return-pc :scs (descriptor-reg))
	 (value))
  (:ignore value)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; FP and jump to the single-value entry at Return-PC + 8.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore FP and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
;;;
(define-vop (return)
  (:args
   (old-fp :scs (any-reg))
   (return-pc :scs (descriptor-reg) :to (:eval 1))
   (values :more t))
  (:ignore values)
  (:info nvals)
  (:vop-var vop)
  (:generator 6
    (trace-table-entry trace-table-function-epilogue)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls an assembly-routine.
;;;
(define-vop (return-multiple)
  (:args
   (old-fp-arg :scs (any-reg) :to (:eval 1))
   (lra-arg :scs (descriptor-reg) :to (:eval 1))
   (vals-arg :scs (any-reg) #+nil :target #+nil vals)
   (nvals-arg :scs (any-reg) #+nil :target #+nil nvals))
  (:vop-var vop)

  (:generator 13
    (trace-table-entry trace-table-function-epilogue)
    (emit-not-implemented)
    (trace-table-entry trace-table-normal)))



;;;; XEP hackery:


;;; We don't need to do anything special for regular functions.
;;;
(define-vop (setup-environment)
  (:info label)
  (:ignore label)
  (:generator 0
    ;; Don't bother doing anything.
    ))

;;; Get the lexical environment from it's passing location.
;;;
(define-vop (setup-closure-environment)
  (:results (closure :scs (descriptor-reg)))
  (:info label)
  (:ignore label)
  (:generator 6
    ;; Get result.
    (emit-not-implemented)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments. 
;;;
(define-vop (copy-more-arg)
  (:info fixed)
  (:generator 20
    (emit-not-implemented)))


;;; More args are stored consequtively on the stack, starting immediately at
;;; the context pointer.  The context pointer is not typed, so the lowtag is 0.
;;;
(define-vop (more-arg word-index-ref)
  (:variant 0 0)
  (:translate %more-arg))


;;; Turn more arg (context, count) into a list.
;;;
(define-vop (listify-rest-args)
  (:args (context-arg :scs (descriptor-reg))
	 (count-arg :scs (any-reg)))
  (:arg-types * tagged-num (:constant t))
  (:info dynamic-extent)
  (:results (result :scs (descriptor-reg)))
  (:translate %listify-rest-args)
  (:policy :safe)
  (:generator 20
    (emit-not-implemented)))


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
  (:policy :fast-safe)
  (:translate c::%more-arg-context)
  (:args (supplied :scs (any-reg)))
  (:arg-types tagged-num (:constant fixnum))
  (:info fixed)
  (:results (context :scs (descriptor-reg))
	    (count :scs (any-reg)))
  (:result-types t tagged-num)
  (:note _N"more-arg-context")
  (:generator 5
    (emit-not-implemented)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:policy :fast-safe)
  (:translate c::%verify-argument-count)
  (:args (nargs :scs (any-reg)))
  (:arg-types positive-fixnum (:constant t))
  (:info count)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (emit-not-implemented)
    (let ((err-lab
	    (generate-error-code vop invalid-argument-count-error nargs)))
      (inst cmp nargs (fixnumize count))
      ;; Assume we don't take the branch
      (inst b err-lab :ne))))

;;; Signal various errors.
;;;
(macrolet ((frob (name error translate &rest args)
	     `(define-vop (,name)
		,@(when translate
		    `((:policy :fast-safe)
		      (:translate ,translate)))
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:vop-var vop)
		(:save-p :compute-only)
		(:generator 1000
		  (error-call vop ,error ,@args)))))
  (frob argument-count-error invalid-argument-count-error
    c::%argument-count-error nargs)
  (frob type-check-error object-not-type-error c::%type-check-error
    object type)
  (frob layout-invalid-error layout-invalid-error c::%layout-invalid-error
    object layout)
  (frob odd-keyword-arguments-error odd-keyword-arguments-error
    c::%odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error unknown-keyword-argument-error
    c::%unknown-keyword-argument-error key)
  (frob nil-function-returned-error nil-function-returned-error nil fun))
