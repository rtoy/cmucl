;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.13 1990/04/24 19:10:22 wlott Exp $
;;;
;;; Description of the MIPS architecture.
;;;
;;; Written by William Lott
;;;

(in-package "MIPS")
(use-package "ASSEM")

(import '(c::tn-p c::tn-sc c::tn-offset c::sc-sb c::sb-name c::zero-tn
		  c::registers c::float-registers
		  c::component-header-length))


;;;; Resources.

(define-random-resources high low)
(define-register-file ireg 32)
(define-register-file fpreg 32)


;;;; Formats.

(defconstant special-op #b000000)
(defconstant bcond-op #b0000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)


(define-format (immediate 32)
  (op (byte 6 26))
  (rs (byte 5 21) :use ireg)
  (rt (byte 5 16) :clobber ireg)
  (immediate (byte 16 0)))

(define-format (jump 32)
  (op (byte 6 26))
  (target (byte 26 0)))

(define-format (register 32)
  (op (byte 6 26))
  (rs (byte 5 21) :use ireg)
  (rt (byte 5 16) :use ireg)
  (rd (byte 5 11) :clobber ireg)
  (shamt (byte 5 6) :default 0)
  (funct (byte 6 0)))



(define-format (break 32)
  (op (byte 6 26) :default special-op)
  (code (byte 10 16))
  (subcode (byte 10 6) :default 0)
  (funct (byte 6 0) :default #b001101))



;;;; Special argument types and fixups.

(defun register-p (object)
  (and (tn-p object)
       (eq (sb-name (sc-sb (tn-sc object))) 'registers)))

(define-argument-type register
  :type (satisfies register-p)
  :function tn-offset)

(defun fp-reg-p (object)
  (and (tn-p object)
       (eq (sb-name (sc-sb (tn-sc object)))
	   'float-registers)))

(define-argument-type fp-reg
  :type (satisfies fp-reg-p)
  :function tn-offset)



(defun label-offset (label)
  (1- (ash (- (label-position label) *current-position*) -2)))

(define-argument-type relative-label
  :type label
  :function label-offset)


(define-fixup-type :jump)
(define-fixup-type :lui)
(define-fixup-type :addi)



;;;; Instructions.


(defmacro define-math-inst (name r3 imm &optional imm-type function fixup)
  `(define-instruction (,name)
     ,@(when imm
	 `((immediate (op :constant ,imm)
		      (rt :argument register)
		      (rs :same-as rt)
		      (immediate :argument (,(case imm-type
					       (:signed 'signed-byte)
					       (:unsigned 'unsigned-byte))
					    16)
				 ,@(when function
				     `(:function ,function))))
	   (immediate (op :constant ,imm)
		      (rt :argument register)
		      (rs :argument register)
		      (immediate :argument (,(case imm-type
					       (:signed 'signed-byte)
					       (:unsigned 'unsigned-byte))
					    16)
				 ,@(when function
				     `(:function ,function))))))
     ,@(when (and imm fixup)
	 `((immediate (op :constant ,imm)
		      (rt :argument register)
		      (rs :same-as rt)
		      (immediate :argument addi-fixup))
	   (immediate (op :constant ,imm)
		      (rt :argument register)
		      (rs :argument register)
		      (immediate :argument addi-fixup))))
     ,@(when r3
	 `((register (op :constant special-op)
		     (rd :argument register)
		     (rs :argument register)
		     (rt :argument register)
		     (funct :constant ,r3))
	   (register (op :constant special-op)
		     (rd :argument register)
		     (rs :same-as rd)
		     (rt :argument register)
		     (funct :constant ,r3))))))

(define-math-inst add #b100000 #b001000 :signed)
(define-math-inst addu #b100001 #b001001 :signed nil t)
(define-math-inst sub #b100010 #b001000 :signed -)
(define-math-inst subu #b100011 #b001001 :signed -)
(define-math-inst and #b100100 #b001100 :unsigned)
(define-math-inst or #b100101 #b001101 :unsigned)
(define-math-inst xor #b100110 #b001110 :unsigned)
(define-math-inst nor #b100111 #b001111 :unsigned)

(define-math-inst slt #b101010 #b001010 :signed)
(define-math-inst sltu #b101011 #b001011 :signed)

(define-instruction (beq)
  (immediate (op :constant #b000100)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label))
  (immediate (op :constant #b000100)
	     (rs :argument register)
	     (rt :argument register)
	     (immediate :argument relative-label)))

(define-instruction (bne)
  (immediate (op :constant #b000101)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label))
  (immediate (op :constant #b000101)
	     (rs :argument register)
	     (rt :argument register)
	     (immediate :argument relative-label)))

(define-instruction (blez)
  (immediate (op :constant #b000110)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (bgtz)
  (immediate (op :constant #b000111)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (bltz)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b00000)
	     (immediate :argument relative-label)))

(define-instruction (bgez)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b00001)
	     (immediate :argument relative-label)))

(define-instruction (bltzal)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b01000)
	     (immediate :argument relative-label)))

(define-instruction (bgezal)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b01001)
	     (immediate :argument relative-label)))

(define-instruction (break)
  (break (code :argument (unsigned-byte 10)))
  (break (code :argument (unsigned-byte 10))
	 (subcode :argument (unsigned-byte 10))))

(define-instruction (div :clobber (low high))
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011010)))

(define-instruction (divu :clobber (low high))
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011011)))

(define-instruction (j)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :constant 0)
	    (rd :constant 0)
	    (funct :constant #b001000))
  (jump (op :constant #b000010)
	(target :argument jump-fixup)))

(define-instruction (jal)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :constant 0)
	    (rd :constant 31)
	    (funct :constant #b001001))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :argument register)
	    (rt :constant 0)
	    (funct :constant #b001001))
  (jump (op :constant #b000011)
	(target :argument jump-fixup)))


(defmacro define-load/store-instruction (name op)
  `(define-instruction (,name)
     (immediate (op :constant ,op)
		(rs :argument register)
		(rt :argument register)
		(immediate :argument (signed-byte 16)))
     (immediate (op :constant ,op)
		(rs :argument register)
		(rt :argument register)
		(immediate :argument addi-fixup))
     (immediate (op :constant ,op)
		(rs :argument register)
		(rt :argument register)
		(immediate :constant 0))))

(define-load/store-instruction lb #b100000)
(define-load/store-instruction lh #b100001)
(define-load/store-instruction lwl #b100010)
(define-load/store-instruction lw #b100011)
(define-load/store-instruction lbu #b100100)
(define-load/store-instruction lhu #b100101)
(define-load/store-instruction lwr #b100110)
(define-load/store-instruction sb #b101000)
(define-load/store-instruction sh #b101001)
(define-load/store-instruction swl #b101010)
(define-load/store-instruction sw #b101011)
(define-load/store-instruction swr #b101110)

(define-instruction (lui)
  (immediate (op :constant #b001111)
	     (rs :constant 0)
	     (rt :argument register)
	     (immediate :argument (or (unsigned-byte 16) (signed-byte 16))))
  (immediate (op :constant #b001111)
	     (rs :constant 0)
	     (rt :argument register)
	     (immediate :argument lui-fixup)))


(define-instruction (mfhi :use high)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010000)))

(define-instruction (mthi :clobber high)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010001)))

(define-instruction (mflo :use low)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010010)))

(define-instruction (mtlo :clobber low)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010011)))


(define-instruction (mult :clobber (low high))
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011000)))

(define-instruction (multu :clobber (low high))
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011001)))

(define-instruction (sll)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000000))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000000))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :argument register)
	    (funct :constant #b000100))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :argument register)
	    (funct :constant #b000100)))

(define-instruction (sra)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000011))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000011))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :argument register)
	    (funct :constant #b000111))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :argument register)
	    (funct :constant #b000111)))

(define-instruction (srl)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000010))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :constant 0)
	    (shamt :argument (unsigned-byte 5))
	    (funct :constant #b000010))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :argument register)
	    (rs :argument register)
	    (funct :constant #b000110))
  (register (op :constant special-op)
	    (rd :argument register)
	    (rt :same-as rd)
	    (rs :argument register)
	    (funct :constant #b000110)))

(define-instruction (syscall)
  (register (op :constant special-op)
	    (rd :constant 0)
	    (rt :constant 0)
	    (rs :constant 0)
	    (funct :constant #b001100)))



;;;; Pseudo-instructions

(define-pseudo-instruction li 64 (reg value)
  (etypecase value
    ((unsigned-byte 16)
     (inst or reg zero-tn value))
    ((signed-byte 16)
     (inst addu reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst lui reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst or reg low))))
    (fixup
     (inst lui reg value)
     (inst addu reg value))))

(define-instruction (b)
  (immediate (op :constant #b000100)
	     (rs :constant 0)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (nop)
  (register (op :constant 0)
	    (rd :constant 0)
	    (rt :constant 0)
	    (rs :constant 0)
	    (funct :constant 0)))

(define-format (word-format 32)
  (data (byte 32 0)))
(define-instruction (word)
  (word-format (data :argument (or (unsigned-byte 32) (signed-byte 32)))))

(define-format (short-format 16)
  (data (byte 16 0)))
(define-instruction (short)
  (word-format (data :argument (or (unsigned-byte 16) (signed-byte 16)))))

(define-format (byte-format 8)
  (data (byte 8 0)))
(define-instruction (byte)
  (word-format (data :argument (or (unsigned-byte 8) (signed-byte 8)))))



;;;; Function and LRA Headers emitters and calculation stuff.

(defun header-data (ignore)
  (declare (ignore ignore))
  (ash (+ *current-position* (component-header-length)) (- vm:word-shift)))

(define-format (header-object 32)
  (type (byte 8 0))
  (data (byte 24 8) :default 0 :function header-data))

(define-instruction (function-header-word)
  (header-object (type :constant #x5e)))

(define-instruction (lra-header-word)
  (header-object (type :constant #x66)))


(define-pseudo-instruction compute-code-from-fn 64 (code fn label)
  ;; code = fn - fn-tag - header - label-offset + other-pointer-tag
  (let ((offset (- vm:other-pointer-type
		   vm:function-pointer-type
		   (label-position label)
		   (component-header-length))))
    (inst addu code fn offset)))

(define-pseudo-instruction compute-code-from-lra 64 (code lra label)
  ;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
  (let ((offset (- (+ (label-position label) (component-header-length)))))
    (inst addu code lra offset)))

(define-pseudo-instruction compute-lra-from-code 64 (lra code label)
  ;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
  (let ((offset (+ (label-position label) (component-header-length))))
    (inst addu lra code offset)))
