;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.42 1992/07/12 20:41:20 hallgren Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the MIPS architecture.
;;;
;;; Written by William Lott
;;;


(in-package "MIPS")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p nil)


;;;; Functions to convert TN's and random symbolic things into values.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error "~S isn't a register." tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (tn-offset tn))

(deftype float-format ()
  '(member :s :single :d :double :w :word))

(defun float-format-value (format)
  (ecase format
    ((:s :single) 0)
    ((:d :double) 1)
    ((:w :word) 4)))

(defconstant compare-kinds
  '(:f :un :eq :ueq :olt :ult :ole :ule :sf :ngle :seq :ngl :lt :nge :le :ngt))

(deftype compare-kind ()
  `(member ,@compare-kinds))

(defun compare-kind (kind)
  (or (position kind compare-kinds)
      (error "Unknown floating point compare kind: ~S~%Must be one of: ~S"
	     kind
	     compare-kinds)))


(defconstant float-operations '(+ - * /))

(deftype float-operation ()
  `(member ,@float-operations))

(defun float-operation (op)
  (or (position op float-operations)
      (error "Unknown floating point operation: ~S~%Must be one of: ~S"
	     op
	     float-operations)))


;;;; Primitive emitters.

(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))

(define-emitter emit-immediate-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

(define-emitter emit-jump-inst 32
  (byte 6 26) (byte 26 0))

(define-emitter emit-register-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 5 6) (byte 6 0))


(define-emitter emit-break-inst 32
  (byte 6 26) (byte 10 16) (byte 10 6) (byte 6 0))

(define-emitter emit-float-inst 32
  (byte 6 26) (byte 1 25) (byte 4 21) (byte 5 16)
  (byte 5 11) (byte 5 6) (byte 6 0))



;;;; Constants used by instruction emitters.

(defconstant special-op #b000000)
(defconstant bcond-op #b000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)



;;;; Math instructions.

(defun emit-math-inst (segment dst src1 src2 reg-opcode immed-opcode
			       &optional allow-fixups)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src1)
			 (reg-tn-encoding src2) (reg-tn-encoding dst)
			 0 reg-opcode))
    (integer
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
			  (reg-tn-encoding dst) src2))
    (fixup
     (unless allow-fixups
       (error "Fixups aren't allowed."))
     (note-fixup segment :addi src2)
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
			  (reg-tn-encoding dst) 0))))

(define-instruction add (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100000 #b001000)))

(define-instruction addu (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) fixup null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100001 #b001001 t)))

(define-instruction sub (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
		   (if (integerp src2) (- src2) src2)
		   #b100010 #b001000)))

(define-instruction subu (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type
    (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) fixup null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
		   (if (integerp src2) (- src2) src2)
		   #b100011 #b001001 t)))

(define-instruction and (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100100 #b001100)))

(define-instruction or (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100101 #b001101)))

(define-instruction xor (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100110 #b001110)))

(define-instruction nor (segment dst src1 &optional src2)
  (:declare (type tn dst src1) (type (or tn null) src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100111 #b000000)))

(define-instruction slt (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101010 #b001010)))

(define-instruction sltu (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101011 #b001011)))

(define-instruction div (segment src1 src2)
  (:declare (type tn src1 src2))
  (:reads (list src1 src2))
  (:writes :hi-and-lo-regs)
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011010)))

(define-instruction divu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:reads (list src1 src2))
  (:writes :hi-and-low-regs)
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011011)))

(define-instruction mult (segment src1 src2)
  (:declare (type tn src1 src2))
  (:reads (list src1 src2))
  (:writes :hi-and-low-regs)
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011000)))

(define-instruction multu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:reads (list src1 src2))
  (:writes :hi-and-low-regs)
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011001)))

(defun emit-shift-inst (segment opcode dst src1 src2)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src2)
			 (reg-tn-encoding src1) (reg-tn-encoding dst)
			 0 (logior #b000100 opcode)))
    ((unsigned-byte 5)
     (emit-register-inst segment special-op 0 (reg-tn-encoding src1)
			 (reg-tn-encoding dst) src2 opcode))))

(define-instruction sll (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-shift-inst segment #b00 dst src1 src2)))

(define-instruction sra (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-shift-inst segment #b11 dst src1 src2)))

(define-instruction srl (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:reads (if src2 (list src1 src2) (list dst src1)))
  (:writes dst)
  (:emitter
   (emit-shift-inst segment #b10 dst src1 src2)))


;;;; Floating point math.

(define-instruction float-op (segment operation format dst src1 src2)
  (:declare (type float-operation operation)
	    (type float-format format)
	    (type tn dst src1 src2))
  (:reads (list src1 src2))
  (:writes dst)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    (fp-reg-tn-encoding src2) (fp-reg-tn-encoding src1)
		    (fp-reg-tn-encoding dst) (float-operation operation))))

(define-instruction fabs (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:reads src)
  (:writes dst)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000101)))

(define-instruction fneg (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:reads src)
  (:writes dst)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000111)))
  
(define-instruction fcvt (segment format1 format2 dst src)
  (:declare (type float-format format1 format2) (type tn dst src))
  (:reads src)
  (:writes dst)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format2) 0
		    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    (logior #b100000 (float-format-value format1)))))

(define-instruction fcmp (segment operation format fs ft)
  (:declare (type compare-kind operation)
	    (type float-format format)
	    (type tn fs ft))
  (:reads (list fs ft))
  (:writes :float-status)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format) 
		    (fp-reg-tn-encoding ft) (fp-reg-tn-encoding fs) 0
		    (logior #b110000 (compare-kind operation)))))


;;;; Branch/Jump instructions.

(defun emit-relative-branch (segment opcode r1 r2 target)
  (emit-back-patch segment 4
		   #'(lambda (segment posn)
		       (emit-immediate-inst segment
					    opcode
					    (if (fixnump r1)
						r1
						(reg-tn-encoding r1))
					    (if (fixnump r2)
						r2
						(reg-tn-encoding r2))
					    (ash (- (label-position target)
						    (+ posn 4))
						 -2)))))

(define-instruction b (segment target)
  (:declare (type label target))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000100 0 0 target)))


(define-instruction beq (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
	    (type (or tn fixnum label) r2-or-target)
	    (type (or label null) target))
  (:attributes branch)
  (:delay 1)
  (:reads (list r1 r2-or-target))
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000100 r1 r2-or-target target)))

(define-instruction bne (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
	    (type (or tn fixnum label) r2-or-target)
	    (type (or label null) target))
  (:attributes branch)
  (:delay 1)
  (:reads (list r1 r2-or-target))
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000101 r1 r2-or-target target)))

(define-instruction blez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:emitter
   (emit-relative-branch segment #b000110 reg 0 target)))

(define-instruction bgtz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:emitter
   (emit-relative-branch segment #b000111 reg 0 target)))

(define-instruction bltz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00000 target)))

(define-instruction bgez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00001 target)))

(define-instruction bltzal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:writes :r31)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b01000 target)))

(define-instruction bgezal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:attributes branch)
  (:delay 1)
  (:reads reg)
  (:writes :r31)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b01001 target)))

(define-instruction j (segment target)
  (:declare (type (or tn fixup) target))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (etypecase target
     (tn
      (emit-register-inst segment special-op (reg-tn-encoding target)
			  0 0 0 #b001000))
     (fixup
      (note-fixup segment :jump target)
      (emit-jump-inst segment #b000010 0)))))

(define-instruction jal (segment reg-or-target &optional target)
  (:declare (type (or null tn fixup) target)
	    (type (or tn fixup (integer -16 31)) reg-or-target))
  (:attributes branch)
  (:delay 1)
  (:writes (if target (reg-tn-location reg-or-target) :r31))
  (:emitter
   (unless target
     (setf target reg-or-target)
     (setf reg-or-target 31))
   (etypecase target
     (tn
      (emit-register-inst segment special-op (reg-tn-encoding target) 0
			  reg-or-target 0 #b001001))
     (fixup
      (note-fixup segment :jump target)
      (emit-jump-inst segment #b000011 0)))))

(define-instruction bc1f (segment target)
  (:declare (type label target))
  (:reads :float-status)
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00000 target)))

(define-instruction bc1t (segment target)
  (:declare (type label target))
  (:reads :float-status)
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00001 target)))



;;;; Random movement instructions.

(define-instruction lui (segment reg value)
  (:declare (type tn reg)
	    (type (or fixup (signed-byte 16) (unsigned-byte 16)) value))
  (:reads)
  (:writes reg)
  (:emitter
   (when (fixup-p value)
     (note-fixup segment :lui value)
     (setf value 0))
   (emit-immediate-inst segment #b001111 0 (reg-tn-encoding reg) value)))

(define-instruction mfhi (segment reg)
  (:declare (type tn reg))
  (:reads :hi-and-low-regs)
  (:writes reg)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010000)))

(define-instruction mthi (segment reg)
  (:declare (type tn reg))
  (:reads reg)
  (:writes :hi-and-low-regs)
  (:delay 1) ;; ### Is there a delay here?
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010001)))

(define-instruction mflo (segment reg)
  (:declare (type tn reg))
  (:reads :hi-and-low-regs)
  (:writes reg)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010010)))

(define-instruction mtlo (segment reg)
  (:declare (type tn reg))
  (:writes :hi-and-low-regs)
  (:delay 1) ;; ### Is there a delay here?
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010011)))

(define-instruction move (segment dst src)
  (:declare (type tn dst src))
  (:reads src)
  (:writes dst)
  (:attributes flushable)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src) 0
		       (reg-tn-encoding dst) 0 #b100001)))

(define-instruction fmove (segment format dst src)
  (:declare (type float-format format) (type tn dst src))
  (:reads src)
  (:writes dst)
  (:attributes flushable)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format) 0
		    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000110)))

(defun %li (reg value)
  (etypecase value
    ((unsigned-byte 16)
     (inst or reg zero-tn value))
    ((signed-byte 16)
     (inst addu reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst lui reg (ldb (byte 16 16) value))
     (inst or reg (ldb (byte 16 0) value)))
    (fixup
     (inst lui reg value)
     (inst addu reg value))))
  
(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

(define-instruction mtc1 (segment to from)
  (:declare (type tn to from))
  (:reads from)
  (:writes to)
  (:delay 1) ;; ### Is this delay correct?
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
		       (fp-reg-tn-encoding to) 0 0)))

(define-instruction mtc1-odd (segment to from)
  (:declare (type tn to from))
  (:reads from)
  (:writes to)
  (:delay 1) ;; ### Is this delay correct?
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
		       (1+ (fp-reg-tn-encoding to)) 0 0)))

(define-instruction mfc1 (segment to from)
  (:declare (type tn to from))
  (:reads from)
  (:writes to)
  (:delay 1) ;; ### Is this delay correct?
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
		       (fp-reg-tn-encoding from) 0 0)))

(define-instruction mfc1-odd (segment to from)
  (:declare (type tn to from))
  (:reads from)
  (:writes to)
  (:delay 1) ;; ### Is this delay correct?
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
		       (1+ (fp-reg-tn-encoding from)) 0 0)))

(define-instruction cfc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  ;; ### reads/writes for this?
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding reg)
		       cr 0 0)))

(define-instruction ctc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  ;; ### reads/writes for this?
  (:emitter
   (emit-register-inst segment cop1-op #b00110 (reg-tn-encoding reg)
		       cr 0 0)))



;;;; Random system hackery and other noise

(define-instruction-macro entry-point ()
  nil)

(define-emitter emit-break-inst 32
  (byte 6 26) (byte 10 16) (byte 10 6) (byte 6 0))

(define-instruction break (segment code &optional (subcode 0))
  (:declare (type (unsigned-byte 10) code subcode))
  :pinned
  (:emitter
   (emit-break-inst segment special-op code subcode #b001101)))

(define-instruction syscall (segment)
  :pinned
  (:emitter
   (emit-register-inst segment special-op 0 0 0 0 #b001100)))

(define-instruction nop (segment)
  (:attributes flushable)
  (:emitter
   (emit-word segment 0)))

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:emitter
   (emit-byte segment byte)))


(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
		  (logior type
			  (ash (+ posn (component-header-length))
			       (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  :pinned
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  :pinned
  (:emitter
   (emit-header-data segment return-pc-header-type)))


(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (let ((delta (funcall calc label posn delta-if-after)))
	  (when (<= (- (ash 1 15)) delta (1- (ash 1 15)))
	    (emit-back-patch segment 4
			     #'(lambda (segment posn)
				 (assemble (segment vop)
					   (inst addu dst src
						 (funcall calc label posn 0)))))
	    t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
	 (assemble (segment vop)
		   (inst lui temp (ldb (byte 16 16) delta))
		   (inst or temp (ldb (byte 16 0) delta))
		   (inst addu dst src temp))))))

;; code = fn - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:reads src)
  (:writes (list dst temp))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     (label-position label posn delta-if-after)
			     (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:reads src)
  (:writes (list dst temp))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:reads src)
  (:writes (list dst temp))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))


;;;; Loads and Stores

(defun emit-load/store-inst (segment opcode reg base index)
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding reg)
		       (reg-tn-encoding base) index))

(define-instruction lb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100000 base reg index)))

(define-instruction lh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100001 base reg index)))

(define-instruction lwl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100010 base reg index)))

(define-instruction lw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100011 base reg index)))

(define-instruction lbu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100100 base reg index)))

(define-instruction lhu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100101 base reg index)))

(define-instruction lwr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads (list base :memory))
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100110 base reg index)))

(define-instruction sb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-load/store-inst segment #b101000 base reg index)))

(define-instruction sh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-load/store-inst segment #b101001 base reg index)))

(define-instruction swl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-load/store-inst segment #b101010 base reg index)))

(define-instruction sw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-load/store-inst segment #b101011 base reg index)))

(define-instruction swr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-load/store-inst segment #b101110 base reg index)))


(defun emit-fp-load/store-inst (segment opcode reg odd base index)
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding base)
		       (+ (fp-reg-tn-encoding reg) odd) index))

(define-instruction lwc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base :memory)
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 0 base index)))

(define-instruction lwc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base :memory)
  (:writes reg)
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 1 base index)))

(define-instruction swc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 0 base index)))

(define-instruction swc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:reads base reg)
  (:writes :memory)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 1 base index)))

