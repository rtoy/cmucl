;;; -*- Mode: Lisp; Package: Compiler; Log: clc.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Constants for the Romp.
;;; Written by: David B. McDonald and Skef Wholey.
;;;

(in-package "COMPILER")


;;; Utilities for hacking header objects.

(defmacro load-slot (value object index)
  `(loadw ,value ,object (ash (+ ,index g-vector-header-size-in-words) 2)))

(defmacro store-slot (value object index)
  `(storew ,value ,object (ash (+ ,index g-vector-header-size-in-words) 2)))


;;; Pointer manipulation macros.

(defmacro get-type-mask-16 (type)
  `(ash ,type type-shift-16))

(defmacro get-address-16 (type space)
  `(logior (ash ,type type-shift-16)
	   (ash ,space space-shift-16)))


(defmacro romp-immed-fixnum-p (object)
  `(let ((thing ,object))
     (and (fixnump thing)
	  (>= (the fixnum thing) (the fixnum romp-min-immed-number))
	  (<= (the fixnum thing) (the fixnum romp-max-immed-number)))))

(defmacro romp-short-fixnum-p (object)
  `(let ((thing ,object))
     (and (fixnump thing)
	(>= (the fixnum thing) (the fixnum romp-min-short-immed-number))
	(<= (the fixnum thing) (the fixnum romp-max-short-immed-number)))))

(defmacro romp-fixnum-p (object)
  `(let ((thing ,object))
     (and (fixnump thing)
	(>= (the fixnum thing) (the fixnum romp-min-fixnum))
	(<= (the fixnum thing) (the fixnum romp-max-fixnum)))))


;;; Registers are given a separate namespace from the constants.  Registers are
;;; defined using the Register macro.  Constants can be defined using the Lisp
;;; Defconstant.  Numbers may be used in place of registers.

(defmacro register (name value)
  `(setf (get ',name '%assembler-register) ,value))

(defun registerp (name)
  (and (symbolp name) (get name '%assembler-register)))

(defmacro float-register (name value)
  `(setf (get ',name '%assembler-float-register) ,value))

(defun float-register-p (name)
  (and (symbolp name) (get name '%assembler-float-register)))

(defun eval-register (form)
  (cond ((numberp form) form)
	((symbolp form) (or (get form '%assembler-register)
			    (get form '%assembler-float-register)))
	(t (clc-error "~S can't be used as a register." form))))

(defun translate-romp-type (register type label not-flag)
  (case type
    (+fixnum `((cmpi ,register type-+-fixnum)
	       (,(get-branch-to-use label not-flag '+fixnum) ,label)))
    (-fixnum `((cmpi ,register type-negative-fixnum)
	       (,(get-branch-to-use label not-flag '-fixnum) ,label)))
    (fixnum `((cmpi ,register type-+-fixnum)
	      (,(get-branch-to-use label not-flag '+fixnum) ,label)
	      (cmpi ,register type-negative-fixnum)
	      (,(get-branch-to-use label not-flag '-fixnum) ,label)))
    (bignum `((cmpi ,register type-bignum)
	      (,(get-branch-to-use label not-flag) ,label)))
    (ratio `((cmpi ,register type-ratio)
	     (,(get-branch-to-use label not-flag) ,label)))
    (+short-float `((cmpi ,register type-short-float-low)
		    (,(get-branch-to-use label not-flag) ,label)))
    (-short-float `((cmpi ,register type-short-float-high)
		    (,(get-branch-to-use label not-flag) ,label)))
    (short-float
     (let ((tag (gensym "L")))
       `((cmpi ,register type-short-float-low)
	 (blt ,(if not-flag label tag))
	 (cmpi ,register type-short-float-high)
	 (,(if not-flag 'bgt 'ble) ,label)
	 ,tag)))
#|
    (single-float `((cmpi ,register type-single-float)
		    (,(get-branch-to-use label not-flag) ,label)))
|#
    (long-float `((cmpi ,register type-long-float)
		  (,(get-branch-to-use label not-flag) ,label)))
    (complex `((cmpi ,register type-complex)
	       (,(get-branch-to-use label not-flag) ,label)))
    (string `((cmpi ,register type-string)
	      (,(get-branch-to-use label not-flag) ,label)))
    (bit-vector `((cmpi ,register type-bit-vector)
		  (,(get-branch-to-use label not-flag) ,label)))
    (i-vector `((cmpi ,register type-i-vector)
		      (,(get-branch-to-use label not-flag) ,label)))
    (g-vector `((cmpi ,register type-g-vector)
		      (,(get-branch-to-use label not-flag) ,label)))
    (array `((cmpi ,register type-array)
	     (,(get-branch-to-use label not-flag) ,label)))
    (function `((cmpi ,register type-function)
		(,(get-branch-to-use label not-flag) ,label)))
    (symbol `((cmpi ,register type-symbol)
	      (,(get-branch-to-use label not-flag) ,label)))
    (list `((cmpi ,register type-list)
	    (,(get-branch-to-use label not-flag) ,label)))
    (control-stack-pointer `((cmpi ,register type-control-stack-pointer)
			     (,(get-branch-to-use label not-flag) ,label)))
    (binding-stack-pointer `((cmpi ,register type-binding-stack-pointer)
			     (,(get-branch-to-use label not-flag) ,label)))
    (gc-forward `((cmpi ,register type-gc-forward)
		  (,(get-branch-to-use label not-flag) ,label)))
    (string-char `((cmpi ,register type-string-char)
		   (,(get-branch-to-use label not-flag) ,label)))
    (trap `((cmpi ,register type-trap)
	    (,(get-branch-to-use label not-flag) ,label)))
    ((T))
    (T (clc-error "Unknow type ~A to type-dispatch macro." type))))

(defun get-branch-to-use (label not-flag &optional fixnum)
  (cond ((eq label 'PC)
	 (if (eq fixnum '+fixnum)
	     (if not-flag 'brgt 'brle)
	     (if (eq fixnum '-fixnum)
		 (if not-flag 'brlt 'brge)
		 (if not-flag 'brne 'breq))))
	(T (if (eq fixnum '+fixnum)
	       (if not-flag 'bgt 'ble)
	       (if (eq fixnum '-fixnum)
		   (if not-flag 'blt 'bge)
		   (if not-flag 'bne 'beq))))))

(defmacro type-dispatch (register &rest forms)
    (do* ((form-list forms (cdr form-list))
	  (form (car form-list) (car form-list))
	  (label (gensym "L") (gensym))
	  (type-code NIL)
	  (clause-code NIL))
	 ((null form-list)
	  (append type-code clause-code))
      (let ((types (cond ((listp (car form)) (car form))
			 (t (list (car form)))))
	    (label (if (and (cadr form) (atom (cadr form)))
		       (cadr form) label))
	    (not-flag (let ((next-form (cadr form-list)))
			(cond ((and next-form
				    (eq (car next-form) 'T)
				    (cadr next-form)
				    (atom (cadr next-form))
				    (not (and (cadr form) (atom (cadr form)))))
			       (setq form-list nil)
			       (cadr next-form))))))
	(if not-flag (setq label not-flag))
	(dolist (x types)
	  (setq type-code
		(append type-code
			(translate-romp-type register x label not-flag))))
	(cond ((and (memq T types) (not (null (cdr form))) (atom (cadr form)))
	       (setq type-code (append type-code `((b ,(cadr form))))))
	      ((memq T types)
	       (setq type-code
		     (append type-code (cdr form)))
	       (setq form-list nil))
	      ((and (cdr form) (atom (cadr form))))
	      ((null not-flag)
	       (setq clause-code
		     (append clause-code (list label) (cdr form))))
	      (T (setq type-code (append type-code (cdr form))))))))

(defmacro access-i-vector (vector index access-code)
  (case access-code
    ((0 1 2)
     `((lr	NL1 ,index)
       (sri	NL1 ,(case access-code (0 3) (1 2) (2 1)))
       (cas	NL1 NL1 ,vector)
       (loadc	NL0 NL1 i-vector-header-size)
       (nilz	NL1 ,index ,(case access-code (0 #x7) (1 #x3) (2 #x1)))
       (xil	NL1 NL1 ,(case access-code (0 #x7) (1 #x3) (2 #x1)))
       ,@(case access-code (0 nil) (1 `((sli NL1 1))) (2 `((sli NL1 2))))
       (sr	NL0 NL1)
       (brx	PC)
       (nilz	A0 NL0 ,(case access-code (0 #x1) (1 #x3) (2 #xF)))))
    (3
     `((cas	NL1 ,vector ,index)
       (brx	PC)
       (loadc	A0 NL1 i-vector-header-size)))
    (4
     `((sli	,index 1)
       (cas	NL1 ,vector ,index)
       (brx	PC)
       (loadh	A0 NL1 i-vector-header-size)))
    (5 (let* ((tag1 (gensym))
	      (tag2 (gensym)))
	 `((sli ,index 2)		; Get index to word.
	   (cas NL1 ,vector ,index)
	   (loadw NL0 NL1 i-vector-header-size) ; Pickup 32 bit quantity
	   (srpi16 NL0 fixnum?-shift-16) ; Shift bits to a more useful place.
	   (bne ,tag1)			; Not a fixnum, go create a bignum
	   (brx PC)		 	; Return it as a fixnum.
	   (cas A0 NL0 0)		; Put into return register.
,tag1
	   (xiu NL1 NL0 #x8000)		; 1 or 2 word bignum?

	   (bnex ,tag2)
	   (loadi A1 (+ bignum-header-size 8)) ; Assume two word.
	   (noop)			; For execute.

	   (loadi A1 (+ bignum-header-size 4)) ; A one word bignum.
,tag2
	   (allocate A0 type-bignum A1 A2 A3) ; Allocate a bignum.
	   (storew NL0 A0 bignum-header-size) ; Store result in bignum.
	   (sri A1 2)
	   (brx PC)			; Return to caller.
	   (storew A1 A0 0))))		; Store word count in bignum header.
    (T (clc-error "Illegal access code (~A) in access-i-vector."
		  access-code))))

(defmacro store-i-vector (vector index access-code value)
  (case access-code
    ((0 1 2)
     `((cas	NL1 ,index 0)
       (sri	NL1 ,(case access-code (0 3) (1 2) (2 1)))
       (cas	NL1 NL1 ,vector)
       (loadc	NL0 NL1 i-vector-header-size)
       (nilz	,index ,index ,(case access-code (0 #x7) (1 #x3) (2 #x1)))
       (xil	,index ,index ,(case access-code (0 #x7) (1 #x3) (2 #x1)))
       ,@(case access-code (0 nil) (1 `((sli ,index 1))) (2 `((sli ,index 2))))
       (nilz	,value ,value ,(case access-code (0 #x1) (1 #x3) (2 #xF)))
       (lr	A0 ,value)
       (sl	,value ,index)
       (lis	A3 ,(case access-code (0 #x1) (1 #x3) (2 #xF)))
       (sl	A3 ,index)
       (onec	A3 A3)
       (n	NL0 A3)
       (o	NL0 ,value)
       (brx	PC)
       (storec	NL0 NL1 i-vector-header-size)))
    (3
     `((cas	NL1 ,vector ,index)
       (lr	A0 ,value)
       (brx	PC)
       (storec	,value NL1 i-vector-header-size)))
    (4
     `((sli	,index 1)
       (cas	NL1 ,vector ,index)
       (lr	A0 ,value)
       (brx	PC)
       (storeha ,value NL1 i-vector-header-size)))
    (5 (let ((tag1 (gensym)))
	 `((sli ,index 2)		; Get index to word.
	   (cas NL1 ,index ,vector)
	   (get-type ,value NL0)		; Get type of object.
	   (cmpi NL0 type-bignum)	; Bignum?
	   (beq ,tag1)	; Yes, go process it.
	   (storew ,value NL1 i-vector-header-size) ; Assume fixnum and store it.
	   (brx PC)			; Return to caller.
	   (lr A0 ,value)		; Put into return register.
,tag1
	   (loadw NL0 ,value bignum-header-size) ; Pull out low order 32 bits.
	   (storew NL0 NL1 i-vector-header-size) ; Store it into vector.
	   (brx PC)
	   (lr A0 ,value))))
    (T (clc-error "Illegal access code (~A) in access-i-vector."
		  access-code))))

;;; The bit-bash-loop macro is used to generate code for the various operations
;;; in the bit-bash misc-op.  It accepts a list of instructions, that should
;;; only modify NL0 and NL1 leaving the result in NL0.

(defmacro bit-bash-loop (operation-code)
  (let ((loop-label (gensym "LABEL-"))
	(done-label (gensym "LABEL-")))
    `((lr	A3 NL0)
      ,loop-label
      (dec	A3 4)
      (cmpi	A3 i-vector-header-size)
      (blt	,done-label)
      (cas	NL1 A0 A3)
      (loadw	NL0 NL1 0)
      (cas	NL1 A1 A3)
      (loadw	NL1 NL1 0)
      ,@operation-code
      (cas	NL1 A2 A3)
      (bx	,loop-label)
      (storew	NL0 NL1 0)
      ,done-label
      (brx	PC)
      (lr	A0 A2))))

;;; Macro to call a conversion routine inside an arithmetic miscop.

(defmacro call-conversion-routine (conversion-routine register)
  `((inc CS 12)			; Make room on for A0, A1, PC.
    (storew PC CS 0)		; Store PC
    (storew A0 CS -4)		; Store A0.
    ,@(unless (eq register 'A0) `((cas A0 ,register 0)))
    
    (mo-callx ,conversion-routine) ; Convert A0 to whatever.
    (storew A1 CS -8)		; while saving A1.

    (cas A2 A0 0)		; Get returned 
    (loadw PC CS 0)		; Restore PC
    (loadw A1 CS -8)		; Get A1 back.
    (loadw A0 CS -4)		; Restore A0.
    (dec CS 12)))		; Restore Stack pointer.

(defmacro save-registers (&rest registers)
  (let ((amount (ash (length registers) 2)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,(if (< amount 16) `(inc CS ,amount) `(cal CS CS ,amount))
	    ,@(nreverse inst-list)))
      (push `(storew ,register CS ,(- (ash (1+ i) 2) amount)) inst-list))))

(defmacro restore-registers (&rest registers)
  (let ((amount (ash (length registers) 2)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,@(nreverse inst-list)
	    ,(if (< amount 16) `(dec CS ,amount) `(cal CS CS ,(- amount)))))
      (push `(loadw ,register CS ,(- (ash (1+ i) 2) amount)) inst-list))))

(defmacro save-registers-PC (&rest registers)
  (let ((amount (+ (ash (length registers) 2) 8)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,(if (< amount 16) `(inc CS ,amount) `(cal CS CS ,amount))
	    ,@(nreverse inst-list)
	    (stm AF CS -4)))
      (push `(storew ,register CS ,(- (ash (1+ i) 2) amount)) inst-list))))

(defmacro restore-registers-PC (&rest registers)
  (let ((amount (+ (ash (length registers) 2) 8)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,@(nreverse inst-list)
	    (lm AF CS -4)
	    (storew BS CS 0)			; Clobber return PC.
	    ,(if (< amount 16) `(dec CS ,amount) `(cal CS CS ,(- amount)))))
      (if register
	  (push `(loadw ,register CS ,(- (ash (1+ i) 2) amount)) inst-list)))))

(defmacro save-registers-internal-PC (&rest registers)
  (let ((amount (+ (ash (length registers) 2) 4)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,(if (< amount 16) `(inc CS ,amount) `(cal CS CS ,amount))
	    ,@(nreverse inst-list)
	    (storew PC CS 0)))
      (push `(storew ,register CS ,(- (ash (1+ i) 2) amount)) inst-list))))

(defmacro restore-registers-internal-PC (&rest registers)
  (let ((amount (+ (ash (length registers) 2) 4)))
    (do* ((i 0 (1+ i))
	  (reg-list registers (cdr reg-list))
	  (register (car reg-list) (car reg-list))
	  (inst-list NIL))
	 ((null reg-list)
	  `(,@(nreverse inst-list)
	    (loadw PC CS 0)
	    ,(if (< amount 16) `(dec CS ,amount) `(cal CS CS ,(- amount)))))
      (if register
	  (push `(loadw ,register CS ,(- (ash (1+ i) 2) amount)) inst-list)))))

;;; Macros to call misc-ops and internal assembler routines

(defmacro mo-call (routine)
  `((bali PC ,routine)))

(defmacro mo-callx (routine)
  `((balix PC ,routine)))

(defmacro load-global-addr (register offset)
  `((cau ,register 0 romp-data-base)
    (oil ,register ,register ,offset)))

(defmacro load-global (register offset &optional (base NIL base-defined))
  (if (not base-defined) (setq base register))
  `((cau ,base 0 romp-data-base)
    (loadw ,register ,base ,offset)))

(defmacro load-multiple-global (register offset
					 &optional (base NIL base-defined))
  (if (not base-defined) (setq base register))
  `((cau ,base 0 romp-data-base)
    (lm ,register ,base ,offset)))

(defmacro store-global (register offset &optional (base 'NL1))
  `((cau ,base 0 romp-data-base)
    (storew ,register ,base ,offset)))

(defmacro store-multiple-global (register offset &optional (base 'NL1))
  `((cau ,base 0 romp-data-base)
    (stm ,register ,base ,offset)))

(defmacro load-symbol-addr (register offset)
  `((cau ,register 0 (get-address-16 type-symbol static-space))
    (oil ,register ,register ,offset)))

(defmacro load-symbol-offset (register symbol-offset offset)
  `((cau ,register 0 (get-address-16 type-symbol static-space))
    (loadw ,register ,register (+ ,symbol-offset ,offset))))

(defmacro store-symbol-offset (register symbol-offset offset &optional (base 'NL1))
  `((cau ,base 0 (get-address-16 type-symbol static-space))
    (storew ,register ,base (+ ,symbol-offset ,offset))))

;;; Escape-Routine  --  Interface
;;;
;;;    Call the function that is the definition of the symbol at the specifed
;;; offset, passing Nargs arguments.  The arguments should already be set up in
;;; A0..A2.  The function must take no more than 3 arguments and return no more
;;; than 3 values.  We make an "escape frame" and save the entire register set
;;; in it.  If the called function returns, we restore the saved registers
;;; *except* for A<N> and NL<N>.  This is so that we return the values returned
;;; by the escape routine, rather than restoring whatever garbage was in the
;;; argument registers before the call.  We only saved the arg registers for
;;; the benefit of the debugger.
;;;
(defmacro escape-routine (symbol-offset nargs)
  `((cal SP SP (* 4 %escape-frame-size)) ; Allocate frame
    ;; Clear type bits in unboxed registers so that GC doesn't gag.  If these
    ;; hold user fixnum or string-char variables, then this won't destroy the
    ;; info.
    (niuo NL0 NL0 clc::type-not-mask-16)
    (niuo NL1 NL1 clc::type-not-mask-16)
    ;; Save all registers...
    (stm NL0 CS (* 4 (- (- %escape-frame-size
			   %escape-frame-general-register-start-slot))))
    ;; Save current CONT as OLD-CONT.
    (storew CONT SP (* 4 (+ (- %escape-frame-size) c::old-cont-save-offset)))
    ;; Compute escape frame start from SP.
    (cal CONT SP (* 4 (- %escape-frame-size)))
    ;; Store escape frame start in to register save area as old SP, since we
    ;; trashed SP before saving registers.
    (storew CONT CONT (* 4 (+ %escape-frame-general-register-start-slot
			      c::sp-offset)))
    ;; Zero ENV save area to indicate an escape frame.
    (loadi NL1 0)
    (storew NL1 CONT (* 4 c::env-save-offset))
    ;; Save miscop return PC as PC escape frame is returning to.
    (storew PC CONT (* 4 c::return-pc-save-offset))

    ;; Get definition
    (load-symbol-offset ENV ,symbol-offset symbol-definition)
    ;; Get entry offset (in bytes, including header size).
    (loadw PC ENV (+ g-vector-header-size (* 4 %function-offset-slot)))
    ;; Get code vector.
    (loadw NL1 ENV (+ g-vector-header-size (* 4 %function-code-slot)))
    ;; Compute entry PC.
    (cas PC PC NL1)
    (lr OLD-CONT CONT) ; OLD-CONT gets escape frame.
    (lr CONT SP) ; So escape frame doesn't get overwritten.
    ;; Call, giving this miscop as return PC for escape frame.
    (balrx PC PC)
    (lis NARGS ,nargs) ; Load argument count
    (noop)
    (cal 0 0 0) ; 32bit noop for single-value return.
    ;; Now restore all registers except for A<N> and NL<N>.
    ;; CONT should be restored to the escape frame by returning function.
    (lm SP CONT (* 4 (+ %escape-frame-general-register-start-slot
			c::sp-offset)))
    ;; Return to caller.
    (br PC)))


;;; The Allocate macro allocates a chunk of storage, frobing the free pointers
;;; of the correct space and possibly invoking the garbage collector.  A newly
;;; allocated object of the given Type and Length is left in the specified
;;; Register.  The Length may be a constant or a register.

;;; We take advantage of the fact that a GC will never happen during execution
;;; of a miscop.  Things get significantly hairier if that is not true.

(defmacro allocate (reg type length temp1 temp2)
  `((load-symbol-offset ,temp1 current-allocation-space-offset symbol-value)
    (oil ,temp1 ,temp1 (ash ,type 5))		; or in type
    (cau ,temp1 ,temp1 alloc-table-address-16)	; add alloc-type address
    (loadw ,reg ,temp1)				; fetch free pointer
    ,@(if (registerp length)			; update free pointer
	  `((cas ,temp2 ,length ,reg))
	  `((cal ,temp2 ,reg ,length)))
    (storew ,temp2 ,temp1)))			; write free pointer back to memory

(defmacro static-allocate (reg type length temp1 temp2)
  `((cau ,temp1 0 alloc-table-address-16)
    (oil ,temp1 ,temp1 (+ (ash ,type 5) 16))
    (loadw ,reg ,temp1)
    ,@(if (registerp length)
	  `((cas ,temp2 ,reg ,length))
	  `((cal ,temp2 ,reg ,length)))
    (storew ,temp2 ,temp1)))

;;; Check-pc-for-interrupt checks to see if a miscops got interrupted
;;; before it entered an interruptable state.

(defmacro check-pc-for-interrupt ()
  (let ((l (gensym)))
    `((srpi16 PC type-shift-16)
      (bne ,l)
      (load-global PC interrupt-pc)
      (cau L0 0 interrupted-16)
      ,l)))

(defmacro service-interrupt ()
  `((xiu L0 L0 interrupted-16)
    (brnex PC)
    (cau L0 0 nil-16)
    (store-global PC interrupt-pc NL1)
    (load-global PC interrupt-routine)))

;;; Various ranges for fixnums on the Romp.

(eval-when (compile load eval)
  (defconstant romp-code-base #x0020)
  (defconstant romp-data-base #x0010)
)

(defconstant romp-min-immed-number (1- (- #x7fff)))
(defconstant romp-max-immed-number #x7fff)

(defconstant romp-min-short-immed-number 0)
(defconstant romp-max-short-immed-number 15)

(defconstant romp-max-fixnum #x7FFFFFF)
(defconstant romp-min-fixnum (1- (- romp-max-fixnum)))

(defconstant Page-Size 8192)
(defconstant Page-Mask-16 #x1FFF)
(defconstant Page-Not-Mask-16 #xE000)
(defconstant Page-Shift-16 13)

;;; Type codes:

(eval-when (compile load eval)
  (defconstant type-+-fixnum 0)
  (defconstant type-gc-forward 1)
  (defconstant type-trap 4)
  (defconstant type-bignum 5)
  (defconstant type-ratio 6)
  (defconstant type-complex 7)
  (defconstant type-+-short-float 8)
  (defconstant type---short-float 9)
  (defconstant type-double-float 10)
  (defconstant type-long-float 10)
  (defconstant type-string 11)
  (defconstant type-bit-vector 12)
  (defconstant type-i-vector 13)
  (defconstant type-code-vector 14)
  (defconstant type-g-vector 15)
  (defconstant type-array 16)
  (defconstant type-function 17)
  (defconstant type-symbol 18)
  (defconstant type-list 19)
  (defconstant type-control-stack-pointer 20)
  (defconstant type-binding-stack-pointer 21)
  (defconstant type-assembler-code 0)


  (defconstant type-short-float 8)
  (defconstant type-short-float-low 8)
  (defconstant type-short-float-high 9)

  (defconstant type-string-char 26)
  (defconstant type-bitsy-char 27)
  (defconstant type-interruptable 28)

  (defconstant type-negative-fixnum 31)

  (defconstant first-pointer-type 4)
  (defconstant last-pointer-type 19)
  (defconstant first-lisp-pointer-type 4)
  (defconstant last-lisp-pointer-type 21)
)

;;; Header sizes and offsets to access words in Lisp objects.

(defconstant bignum-header-size 4)
(defconstant bignum-header-size-in-words 1)
(defconstant bignum-header-words 0)

(defconstant ratio-numerator 0)
(defconstant ratio-denominator 4)

(defconstant float-header-size 4)
(defconstant float-header-size-in-words 1)
(defconstant long-float-size 12)
#|
(defconstant single-float-size 8)
(defconstant single-float-data 4)
|#
(defconstant long-float-high-data 4)
(defconstant long-float-low-data 8)
(defconstant short-float-shift-16 4)
(defconstant short-float-4bit-type #x4)
(defconstant short-float-4bit-mask-16 #x4000)
(defconstant float-compare-shift-16 9)
(defconstant mc68881-compare-shift-16 10)
(defconstant float-compare-mask-16 #x3)
(defconstant mc68881-compare-mask-16 #x3)
(defconstant float-compare-equal 1)

(defconstant short-float-zero-16 #x4000)
(defconstant single-float-one #x3F80)
(defconstant long-float-one #x3FF0)
(defconstant short-float-one
  (logior (ash short-float-4bit-type (- 16 short-float-shift-16))
	  (ash single-float-one (- short-float-shift-16))))
(defconstant single-float-minus-one #xBF80)
(defconstant long-float-minus-one #xBFF0)
(defconstant short-float-minus-one
  (logior (ash short-float-4bit-type (- 16 short-float-shift-16))
	  (ash single-float-minus-one (- short-float-shift-16))))

(defconstant complex-realpart 0)
(defconstant complex-imagpart 4)
(defconstant complex-size 8)

(defconstant vector-subtype-byte 0)

(defconstant string-header-size 8)
(defconstant string-header-words 0)
(defconstant string-header-entries 4)

(defconstant bit-vector-header-size 8)
(defconstant bit-vector-header-words 0)
(defconstant bit-vector-header-entries 4)

(defconstant i-vector-header-size 8)
(defconstant i-vector-header-size-in-words 2)
(defconstant i-vector-header-words 0)
(defconstant i-vector-header-entries 4)
(defconstant i-vector-access-byte 4)

(defconstant iv-access-code-1 0)
(defconstant iv-access-code-2 1)
(defconstant iv-access-code-4 2)
(defconstant iv-access-code-8 3)
(defconstant iv-access-code-16 4)
(defconstant iv-access-code-32 5)

(defconstant g-vector-header-size 4)
(defconstant g-vector-header-size-in-words 1)
(defconstant g-vector-header-words 0)

(defconstant array-header-size 20)
(defconstant array-header-size-in-words 5)
(defconstant array-header-words 0)
(defconstant array-data-vector 4)
(defconstant array-nelements 8)
(defconstant array-fill-pointer 12)
(defconstant array-displacement 16)

(defconstant function-header-size 24)
(defconstant function-header-size-in-words 6)
(defconstant function-header-words 0)
(defconstant function-nconstants 4)
(defconstant function-code 8)
(defconstant function-arginfo 12)
(defconstant min-arg-count-mask-16 #xFF)
(defconstant max-arg-count-mask-16 #xFF00)
(defconstant max-arg-shift-16 8)
(defconstant function-symbol 16)
(defconstant function-arguments 20)

(defconstant symbol-size 20)
(defconstant symbol-value 0)
(defconstant symbol-definition 4)
(defconstant symbol-property-list 8)
(defconstant symbol-print-name 12)
(defconstant symbol-package 16)

(defconstant cons-size 8)
(defconstant list-size 8)
(defconstant list-car 0)
(defconstant list-cdr 4)

(defconstant frame-size 36)
(defconstant frame-size-in-words 9)
(defconstant frame-saved-l0 0)
(defconstant frame-saved-l1 4)
(defconstant frame-saved-l2 8)
(defconstant frame-saved-l3 12)
(defconstant frame-saved-l4 16)
(defconstant frame-binding-stack 20)
(defconstant frame-active-frame 24)
(defconstant frame-active-function 28)
(defconstant frame-pc 32)

(defconstant catch-frame-size 24)
(defconstant catch-frame-size-in-words 6)
(defconstant catch-binding-stack 0)
(defconstant catch-active-frame 4)
(defconstant catch-active-function 8)
(defconstant catch-pc 12)
(defconstant catch-tag-caught 16)
(defconstant catch-prev-catch 20)

(defconstant binding-symbol 4)
(defconstant binding-value 0)

;;; Structure for the link table.

(defconstant lt-vector-size 5)
(defconstant lt-access-code 5)
(defconstant lt-nargs-ac 4)

(defconstant lt-link-table-size 8192)
(defconstant lt-log-table-size 13)
(defconstant link-table-end-in-bytes
  (+ i-vector-header-size (ash lt-link-table-size 3)))
(defconstant lt-table-count 4)
(defconstant lt-symbol-table 8)
(defconstant lt-nargs-table 12)
(defconstant lt-link-table 16)
(defconstant lt-next-table 20)

;;; Masks and shifts for various operations on the ROMP.

(eval-when (compile load eval)
  (defconstant type-mask-16 #xF800)
  (defconstant type-not-mask-16 #x7FF)
  (defconstant type-shift-16 11)

  (defconstant space-mask-16 #x0600)
  (defconstant space-shift-16 9)
  (defconstant dynamic-space-mask-16 #x0200)
)

(defconstant space-mask-result-16 #x3)

(defconstant dynamic-0-space 0)
(defconstant dynamic-1-space 1)
(defconstant static-space 2)
(defconstant read-only-space 3)

(defconstant nil-16 (get-address-16 type-list static-space))
(defconstant t-16 (get-address-16 type-symbol static-space))
(defconstant trap-16 (ash type-trap type-shift-16))

(defconstant interruptable-16 (get-type-mask-16 type-interruptable))
(defconstant interrupted-16 (+ (get-type-mask-16 type-interruptable) 1))

(defconstant g-vector-words-mask-16 #x00FF)
(defconstant right-shifted-subtype-mask-16 #x0007)
(defconstant subtype-shift-16 8)
(defconstant subtype-mask-16 #x0700)
(defconstant g-vector-must-rehash #x0400)
(defconstant i-vector-entries-mask-16 #x0FFF)
(defconstant i-vector-words-mask-16 #x00FF)
(defconstant access-code-shift-byte-16 4)
(defconstant access-code-shift-word-16 12)

(defconstant access-code-1-mask-16 #x0000)
(defconstant access-code-2-mask-16 #x1000)
(defconstant access-code-4-mask-16 #x2000)
(defconstant access-code-8-mask-16 #x3000)
(defconstant access-code-16-mask-16 #x4000)
(defconstant access-code-32-mask-16 #x5000)

(defconstant fixnum-mask-16 #xF800)
(defconstant fixnum-not-mask-16 #x07FF)
(defconstant fixnum-bits 28)
(defconstant fixnum-sign-bit-16 4)
(defconstant fixnum-shift-16 4)
(defconstant fixnum?-shift-16 11)
(defconstant most-negative-fixnum-16 #xF800)
(defconstant smallest-+-bignum-16 #x0800)
(defconstant smallest-positive-bignum-address-16
  (get-address-16 type-bignum static-space))
(defconstant least-negative-bignum-offset 8)

;;; Define values for the boole operations.

(defconstant boole-clr 0)
(defconstant boole-set 1)
(defconstant boole-1 2)
(defconstant boole-2 3)
(defconstant boole-c1 4)
(defconstant boole-c2 5)
(defconstant boole-and 6)
(defconstant boole-ior 7)
(defconstant boole-xor 8)
(defconstant boole-eqv 9)
(defconstant boole-nand 10)
(defconstant boole-nor 11)
(defconstant boole-andc1 12)
(defconstant boole-andc2 13)
(defconstant boole-orc1 14)
(defconstant boole-orc2 15)

;;; Register definitions.

(register r0 0)
(register r1 1)
(register r2 2)
(register r3 3)
(register r4 4)
(register r5 5)
(register r6 6)
(register r7 7)
(register r8 8)
(register r9 9)
(register r10 10)
(register r11 11)
(register r12 12)
(register r13 13)
(register r14 14)
(register r15 15)

(register NArgs 0)		; Number of arguments to a function.
(register nl0 0)		; Unboxed scratch
(register a0 1)			; First argument and return value
(register nl1 2)		; Unboxed scratch
(register a1 3)			; Second argument
(register t0 4)			; Boxed scratch
(register a3 4)			; Fourth arg to some misc-ops.
(register a2 5)			; Third argument
(register cs 6)			; Control Stack Pointer (old name)
(register sp 6)			; Stack pointer
(register l0 7)			; Boxed Temporary
(register l1 8)			; Boxed Temporary
(register l2 9)			; Boxed Temporary
(register name 9)		; Name of function we are trying to call
(register l3 10)		; Boxed Temporary
(register old-cont 10)		; Cont to return to
(register l4 11)		; Boxed Temporary
(register args 11)		; Pointer to stack arguments
(register bs 12)		; Binding Stack Pointer
(register fp 13)		; Active Frame Pointer (old name)
(register cont 13)		; Current Cont
(register af 14)		; Active Function Pointer (old name)
(register env 14)		; Current constant pool, called function.
(register pc 15)		; PC, Return PC for misc-ops, and
(register st 15)		; Super Temporary (old name)
(register t1 15)		; Boxed scratch

;;; Floating point hardware types.

(defconstant float-none 0)
(defconstant float-fpa 2)
(defconstant float-afpa 4)
(defconstant float-mc68881 1)

;;; Error codes.

(defconstant error-not-list 1)
(defconstant error-not-symbol 2)
(defconstant error-object-not-number 3)
(defconstant error-object-not-integer 4)
(defconstant error-object-not-ratio 5)
(defconstant error-object-not-complex 6)
(defconstant error-object-not-vector 7)
(defconstant error-object-not-simple-vector 8)
(defconstant error-illegal-function 9)
(defconstant error-object-not-header 10)
(defconstant error-object-not-i-vector 11)
(defconstant error-object-not-simple-bit-vector 12)
(defconstant error-object-not-simple-string 13)
(defconstant error-object-not-character 14)
(defconstant error-not-control-stack-pointer 15)
(defconstant error-not-binding-stack-pointer 16)
(defconstant error-object-not-array 17)
(defconstant error-object-not-non-negative-fixnum 18)
(defconstant error-object-not-sap-pointer 19)
(defconstant error-object-not-system-pointer 20)
(defconstant error-object-not-float 21)
(defconstant error-object-not-rational 22)
(defconstant error-object-not-non-complex-number 23)

(defconstant error-symbol-unbound 25)
(defconstant error-symbol-undefined 26)
(defconstant error-modify-nil 27)
(defconstant error-modify-t 28)

(defconstant error-bad-access-code 30)
(defconstant error-bad-vector-length 31)
(defconstant error-index-out-of-range 32)
(defconstant error-illegal-index 33)
(defconstant error-illegal-shrink-value 34)
(defconstant error-not-a-shrink 35)
(defconstant error-illegal-data-vector 36)
(defconstant error-too-few-indices 37)
(defconstant error-too-many-indices 38)

(defconstant error-illegal-byte-specifier 40)
(defconstant error-illegal-position-in-byte-spec 41)
(defconstant error-illegal-size-in-byte-spec 42)
(defconstant error-illegal-shift-count 43)
(defconstant error-illegal-boole-operation 44)

(defconstant error-wrong-number-args 50)

(defconstant error-not-<= 55)

(defconstant error-divide-by-zero 60)
(defconstant error-unseen-throw-tag 61)
(defconstant error-short-float-underflow 62)
(defconstant error-short-float-overflow 63)
#|
(defconstant error-single-float-underflow 64)
(defconstant error-single-float-overflow 65)
|#
(defconstant error-long-float-underflow 66)
(defconstant error-long-float-overflow 67)
(defconstant error-monadic-short-underflow 68)
(defconstant error-monadic-short-overflow 69)
(defconstant error-monadic-long-underflow 70)
(defconstant error-monadic-long-overflow 71)

(defconstant error-log-of-zero 72)

(defconstant error-object-not-string-char 73)
(defconstant error-object-not-short-float 74)
(defconstant error-object-not-long-float 75)
(defconstant error-object-not-fixnum 76)
(defconstant error-object-not-cons 77)

(defconstant error-invalid-exit 78)

(defconstant error-odd-keyword-arguments 79)
(defconstant error-unknown-keyword-argument 80)
(defconstant error-object-not-type 81)
(defconstant error-object-not-function-or-symbol 82)
(defconstant error-not-= 83)


;;; Addresses of memory blocks used by the assembler routines.

(defconstant alloc-table-address-16 romp-data-base)
(defconstant alloc-table-address (ash alloc-table-address-16 16))
(defconstant mc68881-float-temporary (+ (ash romp-data-base 16) #x10000))
(defconstant get-real-time (+ (ash romp-data-base 16) #x20000))
(defconstant get-run-time (+ (ash romp-data-base 16) #x20008))
(defconstant alloc-table-size 2048)

(defconstant prime-table-offset	(+ alloc-table-size 0))
(defconstant mo-nargs-nil-routine-addr (+ alloc-table-size 64))
(defconstant mo-nargs-t-routine-addr (+ alloc-table-size 68))
(defconstant check-nargs-t-addr (+ alloc-table-size 72))
(defconstant current-catch-block (+ alloc-table-size 76))
(defconstant space-address (+ alloc-table-size 80))
(defconstant newspace-address (+ alloc-table-size 84))
(defconstant gc-save-NL0 (+ alloc-table-size 88))
(defconstant gc-save-NL1 (+ alloc-table-size 92))
(defconstant gc-save-CS (+ alloc-table-size 96))
(defconstant gc-save-BS (+ alloc-table-size 100))
(defconstant mo-no-entry-routine-addr (+ alloc-table-size 104))
(defconstant save-c-stack-pointer (+ alloc-table-size 108))
(defconstant current-unwind-protect-block (+ alloc-table-size 112))
(defconstant interrupt-signal (+ alloc-table-size 116))
(defconstant interrupt-code (+ alloc-table-size 120))
(defconstant interrupt-scp (+ alloc-table-size 124))
(defconstant interrupt-pc (+ alloc-table-size 128))
(defconstant interrupt-routine (+ alloc-table-size 132))
(defconstant interrupt-reset-pc (+ alloc-table-size 136))
(defconstant interrupt-old-r5 (+ alloc-table-size 140))
(defconstant in-call-foreign (+ alloc-table-size 144))
(defconstant in-cf-save-regs-ptr (+ alloc-table-size 148))
(defconstant software-interrupt-offset (+ alloc-table-size 152))
(defconstant floating-point-hardware-available (+ alloc-table-size 156))
(defconstant bignum-cache-timestamp (+ alloc-table-size 160))
(defconstant bignum-cache-hits (+ alloc-table-size 164))
(defconstant bignum-cache-table (+ alloc-table-size 168))
(defconstant bignum-cache-bignums (+ alloc-table-size 172))
(defconstant get-time-buffer (+ alloc-table-size 300))

;;; Define offsets from the begining of static symbol space for all the symbols
;;; that the assembler code needs to know about.

(defconstant %sp-t-offset 0)
(defconstant %sp-internal-apply-offset 20)
(defconstant %sp-internal-error-offset 40)
(defconstant %sp-software-interrupt-handler-offset 60)
(defconstant %sp-internal-throw-tag-offset 80)
(defconstant %sp-initial-function-offset 100)
(defconstant %link-table-header-offset 120)
(defconstant current-allocation-space-offset 140)
(defconstant %sp-bignum/fixnum 160)
(defconstant %sp-bignum/bignum 180)
(defconstant %sp-fixnum/bignum 200)
(defconstant %sp-abs-ratio 220)
(defconstant %sp-abs-complex 240)
(defconstant %sp-negate-ratio 260)
(defconstant %sp-negate-complex 280)
(defconstant %sp-integer+ratio 300)
(defconstant %sp-ratio+ratio 320)
(defconstant %sp-complex+number 340)
(defconstant %sp-number+complex 360)
(defconstant %sp-complex+complex 380)
(defconstant %sp-1+ratio 400)
(defconstant %sp-1+complex 420)
(defconstant %sp-integer-ratio 440)
(defconstant %sp-ratio-integer 460)
(defconstant %sp-ratio-ratio 480)
(defconstant %sp-complex-number 500)
(defconstant %sp-number-complex 520)
(defconstant %sp-complex-complex 540)
(defconstant %sp-1-ratio 560)
(defconstant %sp-1-complex 580)
(defconstant %sp-integer*ratio 600)
(defconstant %sp-ratio*ratio 620)
(defconstant %sp-number*complex 640)
(defconstant %sp-complex*number 660)
(defconstant %sp-complex*complex 680)
(defconstant %sp-integer/ratio 700)
(defconstant %sp-ratio/integer 720)
(defconstant %sp-ratio/ratio 740)
(defconstant %sp-number/complex 760)
(defconstant %sp-complex/number 780)
(defconstant %sp-complex/complex 800)
(defconstant %sp-integer-truncate-ratio 820)
(defconstant %sp-ratio-truncate-integer 840)
(defconstant %sp-ratio-truncate-ratio 860)
(defconstant %sp-number-truncate-complex 880)
(defconstant %sp-complex-truncate-number 900)
(defconstant %sp-complex-truncate-complex 920)
(defconstant maybe-gc 940)
(defconstant lisp-environment-list 960)
(defconstant call-lisp-from-c 980)
(defconstant lisp-command-line-list 1000)
(defconstant *nameserverport*-offset 1020)
(defconstant *ignore-floating-point-underflow*-offset 1040)
(defconstant lisp::%sp-sin-rational 1060)
(defconstant lisp::%sp-sin-short 1080)
(defconstant lisp::%sp-sin-long 1100)
(defconstant lisp::%sp-sin-complex 1120)
(defconstant lisp::%sp-cos-rational 1140)
(defconstant lisp::%sp-cos-short 1160)
(defconstant lisp::%sp-cos-long 1180)
(defconstant lisp::%sp-cos-complex 1200)
(defconstant lisp::%sp-tan-rational 1220)
(defconstant lisp::%sp-tan-short 1240)
(defconstant lisp::%sp-tan-long 1260)
(defconstant lisp::%sp-tan-complex 1280)
(defconstant lisp::%sp-atan-rational 1300)
(defconstant lisp::%sp-atan-short 1320)
(defconstant lisp::%sp-atan-long 1340)
(defconstant lisp::%sp-atan-complex 1360)
(defconstant lisp::%sp-exp-rational 1380)
(defconstant lisp::%sp-exp-short 1400)
(defconstant lisp::%sp-exp-long 1420)
(defconstant lisp::%sp-exp-complex 1440)
(defconstant lisp::%sp-log-rational 1460)
(defconstant lisp::%sp-log-short 1480)
(defconstant lisp::%sp-log-long 1500)
(defconstant lisp::%sp-log-complex 1520)
(defconstant lisp::%sp-sqrt-rational 1540)
(defconstant lisp::%sp-sqrt-short 1560)
(defconstant lisp::%sp-sqrt-long 1580)
(defconstant lisp::%sp-sqrt-complex 1600)
(defconstant *eval-stack-top*-offset 1620)
