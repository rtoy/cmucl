;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/call.lisp,v 1.1 1990/04/13 13:27:45 wlott Exp $
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
(defun make-return-pc-passing-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn lra-offset)
      (make-restricted-tn *any-primitive-type* (list register-arg-scn))))


;;; Make-Old-Cont-Passing-Location  --  Interface
;;;
;;;    Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; Old-Cont in.  This is (obviously) wired in the standard convention, but is
;;; totally unrestricted in non-standard conventions, since we can always fetch
;;; it off of the stack using the arg pointer.
;;;
(defun make-old-cont-passing-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn oldcont-offset)
      (make-normal-tn *any-primitive-type*)))

;;; Make-Old-Cont-Save-Location, Make-Return-PC-Save-Location  --  Interface
;;;
;;;    Make the TNs used to hold Old-Cont and Return-PC within the current
;;; function.  We treat these specially so that the debugger can find them at a
;;; known location.
;;;
(defun make-old-cont-save-location (env)
  (make-wired-environment-tn *any-primitive-type* control-stack-arg-scn
			     oldcont-save-offset env))
;;;
(defun make-return-pc-save-location (env)
  (make-wired-environment-tn *any-primitive-type* control-stack-arg-scn
			     lra-save-offset env))


;;; Make-Argument-Pointer-Location  --  Interface
;;;
;;;    Similar to Make-Return-PC-Passing-Location, but makes a location to pass
;;; an argument pointer in.  Even when non-standard locations are allowed, this
;;; must be restricted to a register, since the argument pointer is used to
;;; fetch stack arguments.
;;;
(defun make-argument-pointer-location (standard)
  (if standard
      (make-wired-tn *any-primitive-type* register-arg-scn args-offset)
      (make-restricted-tn *any-primitive-type* (list register-arg-scn))))


;;; Make-Argument-Count-Location  --  Interface
;;;
;;;    Make a TN for the standard argument count passing location.  We only
;;; need to make the standard location, since a count is never passed when we
;;; are using non-standard conventions.
;;;
(defun make-argument-count-location ()
  (make-wired-tn *any-primitive-type* register-arg-scn nargs-offset))


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


;;;; Argument/value passing, frame hackery:

;;; Fetch the argument Arg using the argument pointer Argp, yeilding the value
;;; Val.  This operation is used at function entry to move the arguments from
;;; their passing locations to the appropriate variable.
;;;
(define-vop (move-argument)
  (:args (arg :scs (any-reg descriptor-reg)  :load nil  :target val)
	 (argp :scs (any-reg descriptor-reg)))
  (:results (val :scs (any-reg descriptor-reg)))
  (:generator 0
    (sc-case arg
      (control-stack
       (loadw val argp (tn-offset arg)))
      ((any-reg descriptor-reg)
       (move val arg)))))

;;; Similar to Move-Argument, but is used to store known values into the frame
;;; being returned into.  In this case, it is Loc that is potentially on the
;;; stack in a different frame.
;;;
(define-vop (move-value)
  (:args (value :scs (any-reg descriptor-reg)
		:target loc)
	 (old-cont :scs (any-reg descriptor-reg)))
  (:results (loc :scs (descriptor-reg)  :load nil))
  (:generator 0
    (sc-case loc
      (control-stack
       (storew value old-cont (tn-offset loc)))
      ((any-reg descriptor-reg)
       (move loc value)))))


;;; Used for setting up the Old-Cont in local call.
;;;
(define-vop (current-cont)
  (:results (val :scs (any-reg descriptor-reg)))
  (:generator 1
    (move val cont-tn)))


;;; Notes the place at which the environment is properly initialized, for
;;; debug-info purposes.
;;;
(define-vop (note-environment-start)
  (:info start-lab)
  (:generator 0
    (emit-label start-lab)))


;;; Default-Unknown-Values  --  Internal
;;;
;;;    Emit code needed at the return-point from an unknown-values call for a
;;; fixed number of values.  Values is the head of the TN-Ref list for the
;;; locations that the values are to be received into.  Nvals is the number of
;;; values that are to be received (should equal the length of Values).  Node
;;; is the node to use for source context in emitted code.
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
	lr args sp			; Set up args for SP resetting

regs-defaulted
	sub temp nargs register-arg-count

	bltz temp default-value-4	; jump to default code
        addiu temp temp -1
	loadw move-temp args-tn 3	; Move value to correct location.
	store-stack-tn val4-tn move-temp

	bltz temp default-value-5
        addiu temp temp -1
	loadw move-temp args-tn 4
	store-stack-tn val5-tn move-temp

	...

defaulting-done
	lr sp args			; Reset SP.
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
(defun default-unknown-values (node values nvals move-temp temp lra-label)
  (declare (type node node) (type (or tn-ref null) values)
	   (type unsigned-byte nvals) (type tn move-temp temp))
  (assemble node
    (if (<= nvals 1)
	(progn
	  (move csp-tn args-tn)
	  (nop)
	  (inst compute-code-from-lra code-tn code-tn lra-label))
	(let ((regs-defaulted (gen-label))
	      (defaulting-done (gen-label)))
	  ;; Branch off to the MV case.
	  (b regs-defaulted)
	  (inst compute-code-from-lra code-tn code-tn lra-label)

	  ;; Do the single value calse.
	  (inst compute-code-from-lra code-tn code-tn lra-label)
	  (do ((i 1 (1+ i))
	       (val (tn-ref-across values) (tn-ref-across val)))
	      ((= i (min nvals register-arg-count)))
	    (move (tn-ref-tn val) null-tn))
	  (when (> nvals register-arg-count)
	    (loadi nargs-tn (fixnum 1))
	    (move args-tn csp-tn))

	  (emit-label regs-defaulted)

	  (when (> nvals register-arg-count)
	    (inst addiu temp nargs-tn (fixnum (- register-arg-count)))
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
		  (inst addiu temp temp (fixnum -1))
		  (loadw move-temp args-tn i)
		  (store-stack-tn tn move-temp)))

	      (emit-label defaulting-done)
	      (move csp-tn args-tn)

	      (unassemble
	       (assemble-elsewhere node
		 (dolist (def (defaults))
		   (emit-label (car def))
		   (store-stack-tn (cdr def) null-tn))
		 (b defaulting-done)
		 (nop))))))))
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
(defun receive-unknown-values (node args nargs start count lra-label)
  (declare (type node node) (type tn args nargs start count))
  (assemble node
    (let ((variable-values (gen-label))
	  (done (gen-label)))
      (b variable-values)
      (inst compute-code-from-lra code-tn code-tn lra-label)

      (inst compute-code-from-lra code-tn code-tn lra-label)
      (inst addiu csp-tn csp-tn 4)
      (storew (first register-arg-tns) csp-tn -1)
      (inst addiu start csp-tn -4)
      (loadi count (fixnum 1))

      (emit-label done)

      (unassemble
	(assemble-elsewhere node
	  (emit-label variable-values)
	  (do ((arg register-arg-tns (rest arg))
	       (i 0 (1+ i)))
	      ((null arg))
	    (storew (first arg) args i))
	  (move start args)
	  (move count nargs)
	  (b done)
	  (nop)))))
  (undefined-value))


;;; VOP that can be inherited by unknown values receivers.  The main thing this
;;; handles is allocation of the result temporaries.
;;;
(define-vop (unknown-values-receiver)
  (:results
   (start :scs (descriptor-reg))
   (count :scs (descriptor-reg)))
  (:node-var node)
  (:temporary (:sc descriptor-reg :offset args-offset
		   :from :eval :to (:result 0))
	      values-start)
  (:temporary (:sc any-reg :offset nargs-offset
	       :from :eval  :to (:result 1))
	      nvals))


;;;; Tail-recursive local call:

;;; We just do the control transfer.  The other stuff is done with explicit
;;; move VOPs.
;;;
(define-vop (tail-call-local)
  (:args
   (args :more t))
  (:info start)
  (:ignore args)
  (:generator 5
    (when start
      (b start)
      (nop))))


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
  (:args (args :more t))
  (:results (values :more t))
  (:save-p t)
  (:info save return-pc target nvals)
  (:ignore args save)
  (:node-var node)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:generator 5
    (let ((lra-label (gen-label)))
      (inst compute-lra-from-code return-pc code-tn lra-label)
      (move cont-tn csp-tn)
      (b target)
      (multiple-value-bind (cs-size ns-size)
			   (current-frame-size)
	(inst addiu csp-tn csp-tn cs-size)
	(unless (zerop ns-size)
	  (inst addiu nsp-tn nsp-tn (- ns-size))))
      (emit-return-pc lra-label)
      (unassemble
	(default-unknown-values node values nvals move-temp temp lra-label)))))

;;; Non-TR local call for a variable number of return values passed according
;;; to the unknown values convention.  The results are the start of the values
;;; glob and the number of values received.
;;;
(define-vop (multiple-call-local unknown-values-receiver)
  (:args (args :more t))
  (:save-p t)
  (:info save return-pc target)
  (:ignore args save)
  (:generator 20
    (let ((lra-label (gen-label)))
      (inst compute-lra-from-code return-pc code-tn lra-label)
      (move cont-tn csp-tn)
      (b target)
      (multiple-value-bind (cs-size ns-size)
			   (current-frame-size)
	(inst addiu csp-tn csp-tn cs-size)
	(unless (zerop ns-size)
	  (inst addiu nsp-tn nsp-tn (- ns-size))))
      (emit-return-pc lra-label)
      (unassemble
	(receive-unknown-values node values-start nvals
				start count lra-label)))))


;;;; Local call with known values return:

;;; Non-TR local call with known return locations.  Known-value return works
;;; just like argument passing in local call.
;;;
(define-vop (known-call-local)
  (:args
   (args :more t))
  (:results
   (res :more t))
  (:save-p t)
  (:info save return-pc target)
  (:ignore args res save)
  (:generator 5
    (let ((lra-label (gen-label)))
      (inst compute-lra-from-code return-pc code-tn lra-label)
      (move cont-tn csp-tn)
      (b target)
      (multiple-value-bind (cs-size ns-size)
			   (current-frame-size)
	(inst addiu csp-tn csp-tn cs-size)
	(unless (zerop ns-size)
	  (inst addiu nsp-tn nsp-tn (- ns-size))))
      (emit-return-pc lra-label))))

;;; Return from known values call.  We receive the return locations as
;;; arguments to terminate their lifetimes in the returning function.  We
;;; restore CONT and SP and jump to the Return-PC.
;;;
(define-vop (known-return)
  (:args
   (old-cont :scs (descriptor-reg))
   (return-pc :scs (descriptor-reg))
   (locs :more t))
  (:temporary (:scs (interior-reg) :type interior) lip)
  (:ignore locs)
  (:generator 6
    (move csp-tn cont-tn)
    (move cont-tn old-cont)
    (multiple-value-bind (cs-size ns-size)
			 (current-frame-size)
      (declare (ignore cs-size))
      (unless (zerop ns-size)
	(inst addiu nsp-tn nsp-tn ns-size)))
    (lisp-return return-pc lip)))


;;;; Full call:
;;;
;;;    There is something of a cross-product effect with full calls.  Different
;;; versions are used depending on whether we know the number of arguments or
;;; the name of the called function, and whether we want fixed values, unknown
;;; values, or a tail call.

;;; In full call, the arguments are passed by placing them in TNs wired to the
;;; beginning of the current frame (as done by Standard-Argument-Location).  A
;;; pointer to these arguments is then passed as the argument pointer.


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
;;;    The Old-Cont and Return-PC are passed as the second and third arguments.
;;;
;;; Variable is true if there are a variable number of arguments passed on the
;;; stack, with the last argument pointing to the beginning of the arguments.
;;; If Variable is false, the arguments are set up in the standard passing
;;; locations and are passed as the remaining arguments.  Variable cannot be
;;; specified with :Tail return.  TR variable argument call is implemented
;;; separately.
;;;
(defmacro define-full-call (name named return variable)
  (assert (not (and variable (eq return :tail))))
  `(define-vop (,name
		,@(when (eq return :unknown)
		    '(unknown-values-receiver)))
     (:args
      ,(if named
	   '(name :scs (descriptor-reg)
		  :target name-pass)
	   '(arg-fun :scs (descriptor-reg)
		     :target lexenv))
      
      ,@(when (eq return :tail)
	  '((old-cont :scs (descriptor-reg)
		      :target old-cont-pass)
	    (return-pc :scs (descriptor-reg)
		       :target return-pc-pass)))
      
      ,(if variable
	   '(args :scs (descriptor-reg)
		  :target args-pass)
	   '(args :more t)))

     ,@(when (eq return :fixed)
	 '((:results (values :more t))))
   
     ,@(unless (eq return :tail)
	 '((:save-p t)
	   (:node-var node)))

     (:info ,@(unless (eq return :tail) '(save))
	    ,@(unless variable '(nargs))
	    ,@(when (eq return :fixed) '(nvals)))

     (:ignore
      ,@(unless (eq return :tail) '(save))
      ,@(unless variable '(args)))

     (:temporary (:sc descriptor-reg
		  :offset oldcont-offset
		  :from (:argument ,(if (eq return :tail) 1 0))
		  :to :eval)
		 old-cont-pass)

     (:temporary (:sc descriptor-reg
		  :offset lra-offset
		  :from (:argument ,(if (eq return :tail) 2 0))
		  :to :eval)
		 return-pc-pass)

     ,@(when named 
	 '((:temporary (:sc descriptor-reg :offset cname-offset
			:from (:argument 0) :to :eval)
		       name-pass)))

     (:temporary (:sc descriptor-reg :offset lexenv-offset
		  :from (:argument 0) :to :eval)
		 lexenv)

     (:temporary (:scs (descriptor-reg) :from (:argument 0) :to :eval)
		 function)

     (:temporary (:sc descriptor-reg :offset args-offset
		  :from (:argument ,(if variable 1 0)) :to :eval)
		 args-pass)

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
	 '((:temporary (:scs (descriptor-reg)
			:from :eval)
		       move-temp)
	   (:temporary (:scs (descriptor-reg any-reg)
			     :from :eval)
		       temp)))

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
	     `((move args-pass args)
	       (inst sub nargs-pass csp-tn args-pass)
	       ,@(let ((index -1))
		   (mapcar #'(lambda (name)
			       `(loadw ,name args-pass ,(incf index)))
			   register-arg-names)))
	     `((loadi nargs-pass (fixnum nargs))
	       (move args-pass cont-tn)))
       
       (let (,@(unless (eq return :tail)
		 '((lra-label (gen-label)))))
	 ,@(unless (eq return :tail)
	     `((inst compute-lra-from-code return-pc-pass code-tn lra-label)))

	 (loadw function lexenv vm:closure-function-slot
		vm:function-pointer-type)
	 (inst addiu lip function (- (ash vm:function-header-code-offset
					 vm:word-shift)
				    vm:function-pointer-type))

	 ,@(if (eq return :tail)
	       '((move old-cont-pass old-cont)
		 (move return-pc-pass return-pc)
		 (multiple-value-bind (cs-size ns-size)
				      (current-frame-size)
		   (declare (ignore cs-size))
		   (unless (zerop ns-size)
		     (inst addiu nsp-tn nsp-tn ns-size)))
		 (inst jr lip)
		 (move code-tn function))
	       '((move old-cont-pass cont-tn)
		 (move cont-tn csp-tn)
		 (inst jr lip)
		 (move code-tn function)
		 (emit-return-pc lra-label)))

	 ,@(ecase return
	     (:fixed
	      '((unassemble
		 (default-unknown-values node values nvals
					 move-temp temp lra-label))))
	     (:unknown
	      '((unassemble
		 (receive-unknown-values node values-start nvals
					 start count lra-label))))
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
;;; This miscop uses a non-standard calling convention so that the argument
;;; registers are free for loading of stack arguments.  Old-Cont, Args and the
;;; function are passed in the registers that they will ultimately go in:
;;; OLD-CONT, ARGS and ENV.  The Return-PC is passed in A3 rather than PC
;;; because the BALA trashes PC.  We use BALA even though the miscop never
;;; returns, since there isn't any BA.
;;;
(expand
 `(define-vop (tail-call-variable)
    (:args
     (function-arg :scs (descriptor-reg) :target lexenv)
     (old-cont-arg :scs (descriptor-reg) :target old-cont)
     (return-pc-arg :scs (descriptor-reg) :target return-pc)
     (args-arg :scs (descriptor-reg) :target args))
    (:temporary (:sc any-reg :offset lexenv-offset :from (:argument 0))
		lexenv)
    (:temporary (:sc any-reg :offset oldcont-offset :from (:argument 1))
		old-cont)
    (:temporary (:sc any-reg :offset lra-offset :from (:argument 2))
		return-pc)
    (:temporary (:sc any-reg :offset args-offset :from (:argument 3))
		args)
    (:temporary (:sc any-reg :offset nargs-offset) nargs)
    (:temporary (:scs (descriptor-reg)) function)
    ,@(mapcar #'(lambda (offset name)
		  `(:temporary (:sc any-reg :offset ,offset) ,name))
	      register-arg-offsets register-arg-names)
    (:temporary (:scs (any-reg) :type fixnum) src dst count)
    (:temporary (:scs (descriptor-reg)) temp)
    (:temporary (:scs (interior-reg) :type interior) lip)
    (:generator 75
      (let ((loop (gen-label))
	    (test (gen-label)))
	(move lexenv function-arg)
	(move old-cont old-cont-arg)
	(move return-pc return-pc-arg)
	(move args args-arg)
	
	;; Calculate NARGS (as a fixnum)
	(inst subu nargs csp-tn args)
	
	;; Load the argument regs (must do this now, 'cause the blt might
	;; trash these locations)
	,@(let ((index -1))
	    (mapcar #'(lambda (name)
			`(loadw ,name args ,(incf index)))
		    register-arg-names))
	
	;; Calc SRC, DST, and COUNT
	(inst addiu src args (* vm:word-bytes register-arg-count))
	(inst addiu dst cont-tn (* vm:word-bytes register-arg-count))
	(b test)
	(inst addiu count nargs (fixnum (- register-arg-count)))
	
	(emit-label loop)
	;; Copy one arg.
	(loadw temp src)
	(inst addiu src src vm:word-bytes)
	(storew temp dst)
	(inst addiu dst dst vm:word-bytes)
	
	;; Are we done?
	(emit-label test)
	(inst bgtz count loop)
	(inst addiu count count (fixnum -1))
	
	;; We are done.  Do the jump.
	(multiple-value-bind (cs-size ns-size)
			     (current-frame-size)
	  (declare (ignore cs-size))
	  (unless (zerop ns-size)
	    (inst addiu nsp-tn nsp-tn ns-size)))
	(loadw function lexenv vm:closure-function-slot
	       vm:function-pointer-type)
	(lisp-jump function lip)))))


;;;; Unknown values return:


;;; Do unknown-values return of a fixed number of values.  The Values are
;;; required to be set up in the standard passing locations.  Nvals is the
;;; number of values returned.
;;;
;;; If returning a single value, then deallocate the current frame, restore
;;; CONT and jump to the single-value entry at Return-PC + 4.
;;;
;;; If returning other than one value, then load the number of values returned,
;;; NIL out unsupplied values registers, restore CONT and return at Return-PC.
;;; When there are stack values, we must initialize the argument pointer to
;;; point to the beginning of the values block (which is the beginning of the
;;; current frame.)
;;;
(expand
 `(define-vop (return)
    (:args
     (old-cont :scs (descriptor-reg any-reg))
     (return-pc :scs (descriptor-reg) :target pc-pass)
     (values :more t))
    (:ignore values)
    (:info nvals)
    (:temporary (:scs (descriptor-reg) :from (:argument 2)) pc-pass)
    ,@(mapcar #'(lambda (name offset)
		  `(:temporary (:sc descriptor-reg :offset ,offset
				    :from (:eval 0) :to (:eval 1))
			       ,name))
	      register-arg-names register-arg-offsets)
    (:temporary (:sc descriptor-reg :offset nargs-offset) nvals-loc)
    (:temporary (:sc descriptor-reg :offset args-offset) vals-loc)
    (:temporary (:scs (interior-reg) :type interior) lip)
    (:generator 6
      (cond ((= nvals 1)
	     (move csp-tn cont-tn)
	     (move cont-tn old-cont)
	     (multiple-value-bind (cs-size ns-size)
				  (current-frame-size)
	       (declare (ignore cs-size))
	       (unless (zerop ns-size)
		 (inst addiu nsp-tn nsp-tn ns-size)))
	     (move pc-pass return-pc)
	     (inst addiu lip pc-pass (- (* 3 word-bytes)
					other-pointer-type))
	     (inst jr lip)
	     (move code-tn pc-pass))
	    (t
	     (loadi nvals-loc (fixnum nvals))
	     (move vals-loc cont-tn)
	     (inst addiu csp-tn vals-loc (* nvals word-bytes))
	     (move cont-tn old-cont)
	     (multiple-value-bind (cs-size ns-size)
				  (current-frame-size)
	       (declare (ignore cs-size))
	       (unless (zerop ns-size)
		 (inst addiu nsp-tn nsp-tn ns-size)))
	     (move pc-pass return-pc)
	     
	     ,@(let ((index 0))
		 (mapcar #'(lambda (name)
			     `(when (< nvals ,(incf index))
				(move ,name null-tn)))
			 register-arg-names))
	     
	     (lisp-return pc-pass lip))))))

;;; Do unknown-values return of an arbitrary number of values (passed on the
;;; stack.)  We check for the common case of a single return value, and do that
;;; inline using the normal single value return convention.  Otherwise, we
;;; branch off to code that calls a miscop.
;;;
;;; The Return-Multiple miscop uses a non-standard calling convention.  For one
;;; thing, it doesn't return.  We only use BALA because there isn't a BA
;;; instruction.   Also, we don't use A0..A2 for argument passing, since the
;;; miscop will want to load these with values off of the stack.  Instead, we
;;; pass Old-Cont, Start and Count in the normal locations for these values.
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
       (old-cont-arg :scs (descriptor-reg) :target old-cont)
       (return-pc-arg :scs (descriptor-reg) :target return-pc)
       (start-arg :scs (descriptor-reg) :target start)
       (nvals-arg :scs (descriptor-reg) :target nvals))
      (:temporary (:scs (descriptor-reg) :from (:argument 0)) old-cont)
      (:temporary (:scs (descriptor-reg) :from (:argument 1)) return-pc)
      (:temporary (:scs (any-reg) :from (:argument 2) :type fixnum) start)
      (:temporary (:sc any-reg :offset nargs-offset :from (:argument 3)
		       :type fixnum)
		  nvals)
      ,@(mapcar #'(lambda (name offset)
		    `(:temporary (:sc descriptor-reg :offset ,offset
				      :from (:eval 0) :to (:eval 1))
				 ,name))
		register-arg-names register-arg-offsets)
      (:temporary (:sc any-reg :offset args-offset :type fixnum) vals)
      (:temporary (:scs (any-reg) :type fixnum) count dst)
      (:temporary (:scs (descriptor-reg)) temp)
      (:temporary (:scs (interior-reg) :type interior) lip)
      (:generator 13
	(let ((not-single (gen-label))
	      (loop (gen-label))
	      ,@(mapcar #'(lambda (name)
			    `(,name (gen-label)))
			label-names)
	      (done (gen-label)))
	  (move old-cont old-cont-arg)
	  (move return-pc return-pc-arg)
	  (move start start-arg)
	  (move nvals nvals-arg)
	  
	  ;; Single case?
	  (loadi count (fixnum 1))
	  (inst bne count nvals not-single)
	  (nop)
	  
	  ;; Return with one value.
	  (loadw ,(first register-arg-names) start)
	  (move csp-tn cont-tn)
	  (move cont-tn old-cont)
	  (multiple-value-bind (cs-size ns-size)
			       (current-frame-size)
	    (declare (ignore cs-size))
	    (unless (zerop ns-size)
	      (inst addiu nsp-tn nsp-tn ns-size)))
	  (lisp-return return-pc lip)
	  
	  ;; Nope, not the single case.
	  (emit-label not-single)
	  
	  ;; Load the register args, bailing out when we are done.
	  (move count nvals)
	  ,@(mapcar #'(lambda (name label)
			`(progn
			   (inst blez count ,label)
			   (inst addiu count count (fixnum -1))
			   (loadw ,name start)
			   (inst addiu start start vm:word-bytes)))
		    register-arg-names
		    label-names)
	  
	  ;; Copy the remaining args to the top of the stack.
	  (inst addiu dst cont-tn (* vm:word-bytes register-arg-count))
	  
	  (emit-label loop)
	  (inst blez count done)
	  (inst addiu count count (fixnum -1))
	  (loadw temp start)
	  (inst addiu start start vm:word-bytes)
	  (storew temp dst)
	  (b loop)
	  (inst addiu dst dst vm:word-bytes)
	  
	  ;; Default some number of registers.
	  ,@(mapcar #'(lambda (name label)
			`(progn
			   (emit-label ,label)
			   (move ,name null-tn)))
		    register-arg-names label-names)
	  
	  ;; Clear the stack.
	  (emit-label done)
	  (move vals cont-tn)
	  (inst addu csp-tn vals nvals)
	  (move cont-tn old-cont)
	  (multiple-value-bind (cs-size ns-size)
			       (current-frame-size)
	    (declare (ignore cs-size))
	    (unless (zerop ns-size)
	      (inst addiu nsp-tn nsp-tn ns-size)))
	  
	  ;; Return.
	  (lisp-return return-pc lip))))))




;;;; XEP hackery:


(define-vop (allocate-frame)
  (:info label)
  (:generator 1
    ;; Make sure the label is aligned.
    (align vm:lowtag-bits)
    (emit-label label)
    ;; Allocate function header.
    (inst function-header-word)
    (dotimes (i (1- vm:function-header-code-offset))
      (inst word 0))
    ;; Allocate space needed on the stack.
    (multiple-value-bind (cs-size ns-size)
			 (current-frame-size)
      (inst addiu csp-tn csp-tn cs-size)
      (unless (zerop ns-size)
	(inst addiu nsp-tn nsp-tn (- ns-size))))))

;;; Fetch the constant pool from the function entry structure.
;;;
(define-vop (setup-environment)
  (:node-var node)
  (:generator 5
    ;; Fix CODE, cause the function object was passed in.
    (inst compute-code-from-fn
	  code-tn code-tn (block-label (node-block node)))))

;;; Return the current Env as our result, then indirect throught the closure
;;; and the closure-entry to find the constant pool
;;;
(define-vop (setup-closure-environment)
  (:temporary (:sc descriptor-reg :offset lexenv-offset :target closure
	       :to (:result 0))
	      lexenv)
  (:results (closure :scs (descriptor-reg)))
  (:node-var node)
  (:generator 6
    ;; Fix CODE, cause the function object was passed in.
    (inst compute-code-from-fn
	  code-tn code-tn (block-label (node-block node)))
    ;; Get result.
    (move closure lexenv)))

;;; Copy a more arg from the argument area to the end of the current frame.
;;; Fixed is the number of non-more arguments. 
;;;
(define-vop (copy-more-arg)
  (:temporary (:scs (any-reg) :type fixnum) result count src dst)
  (:temporary (:scs (descriptor-reg)) temp)
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
	     (inst addu csp-tn csp-tn nargs-tn))
	    (t
	     (inst addiu count nargs-tn (fixnum (- fixed)))
	     (inst addu csp-tn csp-tn count)))
      (when (< fixed register-arg-count)
	;; We must stop when we run out of stack args, not when we run out of
	;; more args.
	(inst addiu count nargs-tn (fixnum (- register-arg-count))))
      ;; Everything of interest in registers.
      (inst blez count do-regs)
      ;; Initialize dst to be end of stack.
      (move dst csp-tn)
      ;; Initialize src to be end of args.
      (inst addu src args-tn nargs-tn)

      (emit-label loop)
      ;; *--dst = *--src, --count
      (inst addiu src src (- vm:word-bytes))
      (inst addiu count count (fixnum -1))
      (loadw temp src)
      (inst addiu dst dst (- vm:word-bytes))
      (inst bgtz count loop)
      (storew temp dst)

      (emit-label do-regs)
      (when (< fixed register-arg-count)
	;; Now we have to deposit any more args that showed up in registers.
	(inst addiu count nargs-tn (fixnum (- fixed)))
	(do ((i fixed (1+ i)))
	    ((>= i register-arg-count))
	  ;; Don't deposit any more than there are.
	  (inst beq count zero-tn done)
	  (inst addiu count count (fixnum -1))
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
  (:args (context-arg :target context :scs (any-reg descriptor-reg))
	 (count-arg :target count :scs (any-reg descriptor-reg)))
  (:temporary (:scs (any-reg) :type fixnum :from (:argument 0)) context)
  (:temporary (:scs (any-reg) :type fixnum :from (:argument 1)) count)
  (:temporary (:scs (descriptor-reg) :type fixnum :from :eval) temp dst)
  (:temporary (:scs (non-descriptor-reg) :type random :from :eval) ndescr)
  (:results (result :scs (any-reg descriptor-reg)))
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
	(inst addiu result alloc-tn vm:list-pointer-type)
	(move dst result)
	(inst addu alloc-tn alloc-tn count)
	(b enter)
	(inst addu alloc-tn alloc-tn count)

	;; Store the current cons in the cdr of the previous cons.
	(emit-label loop)
	(storew dst dst -1 vm:list-pointer-type)

	;; Grab one value and stash it in the car of this cons.
	(emit-label enter)
	(loadw temp context)
	(inst addiu context context vm:word-bytes)
	(storew temp dst 0 vm:list-pointer-type)

	;; Dec count, and if != zero, go back for more.
	(inst addiu count count (fixnum -1))
	(inst bne count zero-tn loop)
	(inst addiu dst dst (* 2 vm:word-bytes))

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
  (:args
   (supplied :scs (any-reg descriptor-reg)))
  (:info fixed)
  (:results
   (context :scs (descriptor-reg))
   (count :scs (any-reg descriptor-reg)))
  (:generator 5
    (inst addiu count supplied (fixnum (- fixed)))
    (inst subu context csp-tn count)))


;;; Signal wrong argument count error if Nargs isn't = to Count.
;;;
(define-vop (verify-argument-count)
  (:args
   (nargs :scs (any-reg descriptor-reg)))
  (:temporary (:scs (any-reg) :type fixnum) temp)
  (:info count)
  (:node-var node)
  (:generator 3
    (let ((err-lab (generate-error-code node
					di:invalid-argument-count-error
					nargs)))
      (cond ((zerop count)
	     (inst bne nargs zero-tn err-lab)
	     (nop))
	    (t
	     (loadi temp (fixnum count))
	     (inst bne nargs temp err-lab)
	     (nop))))))

;;; Signal an argument count error.
;;;
(macrolet ((frob (name error &rest args)
	     `(define-vop (,name)
		(:args ,@(mapcar #'(lambda (arg)
				     `(,arg :scs (any-reg descriptor-reg)))
				 args))
		(:generator 1000
		  (error-call ,error ,@args)))))
  (frob argument-count-error di:invalid-argument-count-error nargs)
  (frob type-check-error di:object-not-type-error object type)
  (frob odd-keyword-arguments-error di:odd-keyword-arguments-error)
  (frob unknown-keyword-argument-error di:unknown-keyword-argument-error key))
