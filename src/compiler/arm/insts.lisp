;;; -*- Package: ARM -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm/insts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the ARM architecture,
;;;
(in-package "ARM")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p t
  :max-locations 101)			; TODO: How many locations?


;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error (intl:gettext "~S isn't a register.") tn)))))

#+nil
(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error (intl:gettext "~S isn't a floating-point register.") tn))
  (let ((offset (tn-offset tn)))
    (cond ((> offset 31)
	   ;; Use the sparc v9 double float register encoding.
	   (assert (backend-featurep :sparc-v9))
	   ;; No single register encoding greater than reg 31.
	   (assert (zerop (mod offset 2)))
	   ;; Upper bit of the register number is encoded in the low bit.
	   (1+ (- offset 32)))
	  (t
	   (tn-offset tn)))))

(disassem:set-disassem-params :instruction-alignment 32
			      :opcode-column-width 11)


(def-vm-support-routine location-number (loc)
  (etypecase loc
    (null)
    (number)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (registers
	(unless (zerop (tn-offset loc))
	  (tn-offset loc)))
       (float-registers
	(sc-case loc
	  (single-reg
	   (+ (tn-offset loc) 32))
	  (double-reg
	   (let ((offset (tn-offset loc)))
	     (assert (zerop (mod offset 2)))
	     (values (+ offset 32) 2)))))
       (control-registers
	96)
       (immediate-constant
	nil)))
    (symbol
     (ecase loc
       (:memory 0)
       (:psr 97)
       (:fsr 98)
       (:y 99)
       (:tick 100)))))

;;; symbols used for disassembly printing
;;;
(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol name))))
       *register-names*)
  "The Lisp names for the ARM integer registers")

(defun get-reg-name (index)
  (aref reg-symbols index))

(eval-when (compile load eval)
(defun reg-arg-printer (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (get-reg-name value)))
    (princ regname stream)
    (disassem:maybe-note-associated-storage-ref value
						'registers
						regname
						dstate)
    (maybe-add-notes value dstate)))
) ; eval-when
      
(disassem:define-argument-type reg
  :printer #'reg-arg-printer)

;; Do we need a separate set for single-float and double-float regs? 
;; The ARM names are s<n> and d<n>.
(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 to 63 collect (make-symbol (format nil "%F~d" n)))
   'vector))

(disassem:define-argument-type fp-reg
  :printer #'(lambda (value stream dstate)
	       (declare (stream stream) (fixnum value))
	       (let ((regname (aref float-reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

;; Reference: ARM Architecture Reference Manual, ARMv7-A and ARMv7-R,
;; http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0406c/index.html
;;
;; Table A8-1
(defconstant condition-codes
  '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al))

;; Hmm.  The only way to distinguish :ror from :rrx is the value of
;; the shift.  If the shift is 0, then it's :rrx.  Otherwise it's
;; :ror.  What should we do here?
(defconstant shift-types
  '(:lsl :lsr :asr :ror :rrx))

(deftype condition-code ()
  `(member ,@condition-codes))

(defconstant condition-code-name-vec
  (coerce condition-codes 'vector))

(disassem:define-argument-type condition-code
  :printer condition-code-name-vec)

(deftype shift-type ()
  `(member ,@shift-types))

(defun inst-condition-code (options-list)
  (let ((c (remove :s options-list)))
    (unless (cdr c)
      (error "invalid condition code: ~S" c))
    (let ((position (position (car c) condition-codes)))
      (or position
	  #b1111))))

(defun inst-set-flags (options-list)
  (if (member :s options-list)
      1
      0))
      
(defconstant condition-true
  #b1111)

(defun rotate-right2 (value amount)
  "Rotate VALUE right by 2*AMOUNT bits in a register of length 32 bits."
  (declare (type (unsigned-byte 32) value)
	   (type (integer 0 15) amount))
  (let* ((shift (* 2 amount))
	 (out (ldb (byte shift 0) value)))
    (logior (ash out (- 32 shift))
	    (ash value (- shift)))))

(defun rotate-left (value amount)
  "Rotate VALUE left by AMOUNT bits in a register of length 32 bits."
  (declare (type (unsigned-byte 32) value)
	   (type (integer 0 15) amount))
  (let* ((out (ldb (byte amount (- 32 amount)) value)))
    (logior out
	    (ash (ldb (byte (- 32 amount) 0) value) amount))))

(defun find-encoding (value)
  "Find a V and N such that VALUE = rotate_right(V, 2*N)
  The values N and V are returned as multiple values, in that order.
  If no such values of N and V exist, then NIL is returned."
  (cond ((< value 256)
	 (values 0 value))
	((zerop (ldb (byte 8 24) value))
	 ;; Easy case where the rotation didn't rotate anything into
	 ;; the top byte.
	 (let* ((trailing-zeros (floor (logcount (logand (lognot value)
							      (1- value)))
					    2))
		(new (ash value (- (* 2 trailing-zeros)))))
	   (when (< new 256)
	     (values (- 16 trailing-zeros) new))))
	(t
	 ;; Rotate the value left 8 bits.  This guarantees that only
	 ;; shifts and not rotates are needed to figure out the
	 ;; answer, if the top bits 8 bits are zero.
	 (let ((new (rotate-left value 8)))
	   (when (zerop (ldb (byte 8 24) new))
	     (multiple-value-bind (rot val)
		 (find-encoding new)
	       (when rot
		 (values (mod (+ 4 rot) 16) val))))))))


;;; Define instruction formats. See DDI0406C_b, section A5 for details.
(defconstant format-1-immed-printer
  `(:name (:unless (s :constant 0) 's)
	  (:unless (:constant ,condition-true) cond)
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  immed))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun modified-immed-printer (value stream dstate)
  (declare (ignore dstate))
  (let ((rot (ldb (byte 4 8) value))
	(v (ldb (byte 8 0) value)))
    (format stream "#~X" (rotate-right2 v rot))))
)

(define-emitter emit-format-1-immed 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 12 0))

(disassem:define-instruction-format
    (format-1-immed 32 :default-printer format-1-immed-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op0   :field (byte 3 25) :value #b001)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (immed :field (byte 12 0) :printer #'modified-immed-printer))

(defconstant format-1-reg-printer
  `(:name (:unless (s :constant 0) 's)
	  (:unless (:constant ,condition-true) cond)
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  src2
	  (:unless (shift :constant 0)
	    (:cond ((type :constant #b11) ; ror or rrx
		    (:cond ((shift :constant 0) 'rrx)
			   (t
			    'ror
			    " "
			    shift)))
		   (t
		    type
		    " "
		    shift)))))

(define-emitter emit-format-1-reg 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 5 7)
  (byte 2 5) (byte 1 4) (byte 4 0))
  
(disassem:define-instruction-format
    (format-1-reg 32 :default-printer format-1-reg-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op0   :field (byte 3 25) :value #b000)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (shift :field (byte 5 7))
  (type  :field (byte 2 5))
  (rs    :field (byte 1 4) :value 0)
  (src2  :field (byte 4 0) :type 'reg))

(defconstant format-1-reg-shifted-printer
  `(:name (:unless (s :constant 0) 's)
	  (:unless (:constant ,condition-true) cond)
	  :tab
	  dst
	  ", "
	  src1
	  ", "
	  src2
	  ", "
	  type
	  ", "
	  sreg))

(define-emitter emit-format-1-reg-shifted 32
  (byte 4 28) (byte 3 25) (byte 4 21) (byte 1 20) (byte 4 16) (byte 4 12) (byte 4 8)
  (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))
  
(disassem:define-instruction-format
    (format-1-reg-shifted 32 :default-printer format-1-reg-shifted-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op0   :field (byte 3 25) :value #b000)
  (op    :field (byte 4 21))
  (s     :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (sreg  :field (byte 4 8) :type 'reg)
  (z     :field (byte 1 7) :value 0)
  (type  :field (byte 2 5))
  (rs    :field (byte 1 4) :value 1)
  (src2  :field (byte 4 0) :type 'reg))


;; Structure for hold the flexible operand used in data processing
;; instructions.
(defstruct flex-operand
  ;; Indicates if the shift is an immediate or register value
  (type (required-argument)
   :type (member :reg-shift-imm :reg-shift-reg))

  ;; Register to be shifted
  reg

  ;; Shift type
  shift-type

  ;; Amount of the shift, either an immediate or a register
  shift-reg-or-imm)

;; See Table A5-6 for encoding of modified immediates in ARM
;; processing instructions.

;; src2 = reg SFT amount, where SFT is LSL, LSR, ASR, ROR, RRX and
;; amount is either a number or a register.
(defun make-shift (reg shift-type &optional amount)
  (declare (type shift-type shift-type))
  (typecase amount
    ((unsigned-byte 5)
     (when (and (eq shift-type :rrx)
		amount
		(not (zerop amount)))
       (error "Cannot specify non-zero shift amount with :rrx"))
     (make-flex-operand :type :reg-shift-imm
			:reg reg
			:shift-reg-or-imm amount
			:shift-type shift-type))
    (otherwise
     (when (eq shift-type :rrx)
       (error "Cannot specify :rrx type with register shift"))
     (make-flex-operand :type :reg-shift-reg
			:reg reg
			:shift-reg-or-imm amount
			:shift-type shift-type))))


(defmacro define-data-proc (name opcode &optional force-set-p)
  `(define-instruction ,name (segment dst src1 src2 &rest opts)
     (:declare (type tn dst)
	       (type tn src1)
	       (type (or (signed-byte 32)
			 (unsigned-byte 32)
			 reg
			 flex-operand)
		     src2))
     (:printer format-1-immed
	       ((op0 #b001) (op ,opcode)))
     (:printer format-1-reg
	       ((op0 #b000) (op ,opcode) (rs 0)))
     (:printer format-1-reg-shifted
	       ((op0 #b000) (op ,opcode) (rs 1)))
     (:dependencies
      (reads src1)
      (writes dst))
     (:delay 0)
     (:emitter
      (etypecase src2
	(integer
	 (multiple-value-bind (rot val)
	     (find-encoding src2)
	   (unless rot
	     (error "Cannot encode the immediate value ~S~%" src2))
	   (emit-format-1-immed segment
				  (inst-condition-code opts)
				  ,opcode
				  ,(if force-set-p
				       1
				       `(inst-set-flags opts))
				  (reg-tn-encoding src1)
				  (reg-tn-encoding dst)
				  rot
				  val)))
	(reg
	 (emit-format-1-reg segment
			    (inst-condition-code opts)
			    ,opcode
			    ,(if force-set-p
				 1
				 `(inst-set-flags opts))
			    (reg-tn-encoding src1)
			    (reg-tn-encoding dst)
			    0
			    :lsl
			    (reg-tn-encoding src2)))
	(flex-operand
	 (ecase (flex-operand-type src2)
	   (:reg-shift-imm
	    (emit-format-1-reg segment
			       (inst-condition-code opts)
			       ,opcode
			       ,(if force-set-p
				    1
				    `(inst-set-flags opts))
			       (reg-tn-encoding src1)
			       (reg-tn-encoding dst)
			       (flex-operand-shift-reg-or-imm src2)
			       (flex-operand-shift-type src2)
			       (flex-operand-reg src2)))
	   (:reg-shift-reg
	    (emit-format-1-reg-shift segment
				     (inst-condition-code opts)
				     ,opcode
				     ,(if force-set-p
					  1
					  `(inst-set-flags opts))
				     (reg-tn-encoding src1)
				     (reg-tn-encoding dst)
				     (flex-operand-shift-reg-or-imm src2)
				     (flex-operand-shift-type src2)
				     (reg-tn-encoding (flex-operand-reg src2))))))))))


(define-data-proc and #b0000)
(define-data-proc eor #b0001)
;; #b0010 is sub and adr (immediate operand)
(define-data-proc sub #b0010)
(define-data-proc rsb #b0011)
;; #b0100 is add and adr (immediate operand)
(define-data-proc add #b0100)
(define-data-proc adc #b0101)
(define-data-proc sbc #b0110)
(define-data-proc rsc #b0111)
;; #b10xx is data processing and miscellaneous instructions
(define-data-proc tst #b1000 t)
(define-data-proc teq #b1001 t)
(define-data-proc cmp #b1010 t)
(define-data-proc cmn #b1011 t)
(define-data-proc orr #b1100)
(define-data-proc mov #b1101)
(define-data-proc bic #b1110)
(define-data-proc mvn #b1111)		; aka bitwise not


;; See A8.8.63
;; LDR/STR (immediate)
;;
;; ldr<c> dst, [src1, #+/-<imm>]
;; ldr<c> dst, [src1, #+/-<imm>]!
;; ldr<c> dst, [src1], #+/-<imm>
(defconstant format-2-immed-printer
  `(:name (:unless (:constant ,condition-true) cond)
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 1)
		  ", #"
		  (:unless (u :constant 0) "-")
		  immed
		  "]"
		  (:unless (w :constant 1) "!"))
		 (t
		  "], #"
		  (:unless (u :constant 0) "-")
		  immed))))
		  

(define-emitter emit-format-2-immed 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 0 12))

  
(disassem:define-instruction-format
    (format-2-immed 32 :default-printer format-2-immed-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (opc   :field (byte 3 25) :value #b010)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (byte  :field (byte 1 22))		; byte (1) or word (0)
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))		; ldr (1) or str (0)
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (immed :field (byte 0 12)))

;; See A8.8.66
;; LDR/STR (register)
;;
;; ldr<c> dst, [src1, +/-src2, shift]<!>
;; ldr<c> dst, [src1], +/-src2, shift
(defconstant format-2-reg-printer
  `(:name (:unless (:constant ,condition-true) cond)
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 1)
		  ", "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  rs
		  ", "
		  type
		  " "
		  imm
		  "]"
		  (:unless (w :constant 1) "!"))
		 (t
		  "], "
		  rs
		  ", "
		  type
		  " "
		  imm))))
  
(define-emitter emit-format-2-immed 32
  (byte 4 28) (byte 3 25) (byte 1 24) (byte 1 23) (byte 1 22) (byte 1 21)
  (byte 1 20) (byte 4 16) (byte 4 12) (byte 5 7) (byte 2 5) (byte 1 4) (byte 4 0))

(disassem:define-instruction-format
    (format-2-reg 32 :default-printer format-2-reg-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (opc   :field (byte 3 25) :value #b011)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (byte  :field (byte 1 22))
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))		; ldr (1) or str (0)
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (imm5  :field (byte 5 7))
  (type  :field (byte 2 5))
  (z     :field (byte 1 4) :value 0)
  (rs    :field (byte 4 0) :type 'reg))

;; LDRH/STRH (register)
(defconstant format-2-halfword-reg-printer
  `(:name (:unless (:constant ,condition-true) cond)
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 0)
		  "], "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  src2)
		 (t
		  ", "
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  src2
		  "]"
		  (:unless (w :constant 0) "!")))))

(disassem:define-instruction-format
    (format-2-halfword-reg 32 :default-printer format-2-halfword-reg-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op0   :field (byte 3 25) :value #b000)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (imm   :field (byte 1 22) :value 0)
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (z     :field (byte 4 8) :value 0)
  (halfp :field (byte 2 5))
  (src2  :field (byte 4 0) :type 'reg))

;; LDRH/STRH (register)

(defconstant format-2-halfword-imm-printer
  `(:name (:unless (:constant ,condition-true) cond)
          :tab
	  dst
	  ", ["
	  src1
	  (:cond ((p :constant 0)
		  "], #"
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  imm8)
		 (t
		  ", #"
		  (:cond ((u :constant 0) "-")
			 (t "+"))
		  imm8
		  "]"
		  (:unless (w :constant 0) "!")))))
	  
(disassem:define-instruction-format
    (format-2-halfword-imm 32 :default-printer format-2-halfword-imm-printer)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op0   :field (byte 3 25) :value #b000)
  (p     :field (byte 1 24))
  (u     :field (byte 1 23))
  (imm   :field (byte 1 22) :value 1)
  (w     :field (byte 1 21))
  (ld    :field (byte 1 20))
  (src1  :field (byte 4 16) :type 'reg)
  (dst   :field (byte 4 12) :type 'reg)
  (imm8  :fields (list (byte 4 8) (byte 0 4)))
  (halfp :field (byte 2 5) :value #b1011))

(defstruct load-store-index
  (type (required-argument) :type '(member :reg :immediate))
  base-reg
  offset
  shift-type
  shift-amount
  add
  update
  post-indexed)

(defun make-ea-imm (base-reg &key (imm 0) update)
  (let ((mag (abs imm))
	(add (not (minusp imm))))
    (assert (typep mag '(unsigned-byte 12)))
    (make-load-store-index :type :immediate
			   :base-reg base-reg
			   :offset mag
			   :add add
			   :update update)))

(defun make-ea (base-reg &key (offset 0) update
			   (add t addp) 
			   (shift-type :lsl shift-type-p)
			   (amount 0 amountp)
			   post-indexed)
  (assert (typep amount '(unsigned-byte 5)))
  (assert (typep shift-type shift-types))
  (etypecase offset
    (reg
     (make-load-store-index :type :reg
			    :base-reg base-reg
			    :offset offset
			    :add add
			    :shift-type shift-type
			    :shift-amount amount
			    :update update
			    :post-indexed post-indexed))
    ((or (unsigned-byte 12) (signed-byte 12))
     (let ((mag (abs imm))
	   (add (not (minusp imm))))
       ;; The :add :shift-type and :amount keywords aren't allowed in
       ;; this case.
       (assert (not addp))
       (assert (not shift-type-p))
       (assert (not amountp))
       (make-load-store-index :type :immediate
			      :base-reg base-reg
			      :offset mag
			      :add add
			      :update update
			      :post-indexed post-indexed)))))

(defun decode-load-store-index (index)
  "Determine the P, U, and W bits from the load-store-index"
  (values (if (load-store-index-post-indexed index) 0 1)
	  (if (load-store-index-add index) 1 0)
	  (if (load-store-index-update index) 1 0)))

(defmacro define-load/store (name loadp &optional bytep)
  `(define-instruction ,name (segment dst src1 &rest opts)
     (:declare (type tn dst src1)
	       (type load-store-index src2))
     (:dependencies
      (reads src1)
      (writes dst))
     (:printer format-2-immed
	       ((opc #b010)))
     (:printer format-2-reg
	       ((opc #b011)))
     (:emitter
      (etype (load-store-index-type src2)
	(:reg
	 (multiple-value-bind (p u w)
	     (decode-load-store-index src2)
	   (emit-format-2-reg segment
			      (inst-condition-code opts)
			      p
			      u
			      ,bytep
			      w
			      ,loadp
			      (load-store-index-base-reg src1)
			      (reg-tn-encoding dst)
			      (load-store-index-shift-amount src1)
			      (load-store-index-shift-type src1)
			      (load-store-index-offset src1))))
	(:immediate
	 (multiple-value-bind (p u w)
	     (decode-load-store-index src2)
	   (emit-format-2-immed segment
				(inst-condition-code opts)
				p
				u
				,bytep
				w
				,loadp
				(reg-tn-encoding (load-store-index-base-reg src1))
				(reg-tn-encoding dst)
				(load-store-index-offset src1))))))))

(define-load/store ldr t)
(define-load/store ldrb t t)
(define-load/store str nil)
(define-load/store strb nil t)

(defmacro define-load/store-extra (name &optional loadp signedp bytep)
  `(disassem:define-instruction ,name (segment dst src1 &rest opts)
     (:declare (type tn dst src1)
	       (type load-store-index src2))
     (:dependencies
      (reads src1)
      (writes dst))
     (:printer format-2-halfword-imm
	       ((opc #b010)))
     (:printer format-2-halfword-reg
	       ((opc #b011)))
     (:emitter
      (let ((op (cond (,bytep
		       (when ,signedp
			 #b1101))
		      (t
		       (if ,signedp
			   #b1111
			   #b1011)))))
			      
	(ecase (load-store-index-type src2)
	  (:reg
	   (multiple-value-bind (p u w)
	       (decode-load-store-index src2)
	     (emit-format-2-halfword-reg segment
					 (inst-condition-code opts)
					 p
					 u
					 w
					 ,(if loadp 1 0)
					 (load-store-index-base-reg src1)
					 (reg-tn-encoding dst)
					 (load-store-index-shift-amount src1)
					 op
					 (load-store-index-offset src1))))
	  (:immediate
	   (multiple-value-bind (p u w)
	       (decode-load-store-index src2)
	     (emit-format-2-halfword-imm segment
					 (inst-condition-code opts)
					 p
					 u
					 w
					 ,(if loadp 1 0)
					 (reg-tn-encoding (load-store-index-base-reg src1))
					 (reg-tn-encoding dst)
					 (load-store-index-offset src1)
					 op))))))))

(define-load/store-extra ldrh t)
(define-load/store-extra strh nil)
(define-load/store-extra ldrsh t t)
(define-load/store-extra strsh nil t)
(define-load/store-extra ldrsb t t t)
(define-load/store-extra strsb nil t t)


(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 24) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

(define-emitter emit-branch-imm 32
  (byte 4 28) (byte 4 24) (byte 0 24))

(disassem:define-instruction-format
    (branch-imm 32)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op    :field (byte 4 24))
  (imm24 :field (byte 0 24) :type 'relative-label))

(define-emitter emit-branch-reg 32
  (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 0 4))
  
(disassem:define-instruction-format
    (branch-reg 32)
  (cond  :field (byte 4 28) :type 'condition-code)
  (op    :field (byte 8 20) :value #b00010010)
  (op0   :field (byte 12 8) :value #b111111111111)
  (op1   :field (byte 4 4) :value #b0011)
  (src1  :field (byte 0 4) :type 'reg))

(defun emit-relative-branch (segment op cond target)
  (emit-back-patch segment 4
     #'(lambda (segment posn)
	 (emit-branch-imm segment
			  (inst-condition-code code)
			  op
			  (offset (ash (- (label-position target) posn) -2))))))

;; For these branch instructions, should we still keep the condition
;; at the end, like for other instructions?  Or can we have it
;; (optionally) first, like on sparc and x86?  This latter option
;; appeals to me (rtoy).

(define-instruction b (segment target &optional (cond :al))
  (:declare (type label) target
	    (type condition-code cond))
  (:attributes branch)
  (:emitter
   (emit-relative-branch segment #b1010 cond target)))

(define-instruction bl (segment target &optional (cond :al))
  (:declare (type label target)
	    (type condition-code cond))
  (:attributes branch)
  (:emitter
   (emit-relative-branch segment #b1011 cond target)))

(define-instruction blx (segment target)
  (:declare (type (or label reg) target))
  (:attributes branch)
  (:emitter
   (etypecase target
     (label
      (emit-relative-branch segment #b1010 :al target))
     (reg
      (emit-branch-reg setment
		       (inst-condition-code cond)
		       (reg-tn-encooding target))))))
  
