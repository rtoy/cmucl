;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.21 1990/06/25 23:44:45 wlott Exp $
;;;
;;; Description of the MIPS architecture.
;;;
;;; Written by William Lott
;;;

(in-package "MIPS")
(use-package "ASSEM")
(use-package "EXT")

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


(define-format (coproc-branch 32)
  (op (byte 6 26))
  (funct (byte 10 16))
  (offset (byte 16 0)))

(define-format (float 32)
  (op (byte 6 26) :default #b010001)
  (filler (byte 1 25) :default #b1)
  (format (byte 4 21))
  (ft (byte 5 16))
  (fs (byte 5 11))
  (fd (byte 5 6))
  (funct (byte 6 0)))

(define-format (float-aux 32)
  (op (byte 6 26) :default #b010001)
  (filler-1 (byte 1 25) :default #b1)
  (format (byte 4 21))
  (ft (byte 5 16) :default 0)
  (fs (byte 5 11))
  (fd (byte 5 6))
  (funct (byte 2 4))
  (sub-funct (byte 4 0)))



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

(define-argument-type odd-fp-reg
  :type (satisfies fp-reg-p)
  :function (lambda (tn)
	      (1+ (tn-offset tn))))

(defun label-offset (label)
  (1- (ash (- (label-position label) *current-position*) -2)))

(define-argument-type relative-label
  :type label
  :function label-offset)



(defun float-format-value (format)
  (ecase format
    ((:s :single) 0)
    ((:d :double) 1)
    ((:w :word) 4)))

(define-argument-type float-format
  :type (member :s :single :d :double :w :word)
  :function float-format-value)


(eval-when (compile load eval)

(defconstant compare-kinds
  '(:f :un :eq :ueq :olt :ult :ole :ule :sf :ngle :seq :ngl :lt :nge :le :ngt))

(defconstant float-operations '(+ - * /))

); eval-when

(defun compare-kind (kind)
  (or (position kind compare-kinds)
      (error "Unknown floating point compare kind: ~S~%Must be one of: ~S"
	     kind
	     compare-kinds)))

(define-argument-type compare-kind
  :type (member #.compare-kinds)
  :function compare-kind)

(defun float-operation (op)
  (or (position op float-operations)
      (error "Unknown floating point operation: ~S~%Must be one of: ~S"
	     op
	     float-operations)))

(define-argument-type float-operation
  :type (member #.float-operations)
  :function float-operation)


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

(define-instruction (bc1f)
  (coproc-branch (op :constant cop1-op)
		 (funct :constant #x100)
		 (offset :argument relative-label)))

(define-instruction (bc1t)
  (coproc-branch (op :constant cop1-op)
		 (funct :constant #x101)
		 (offset :argument relative-label)))

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


(defmacro define-load/store-instruction (name op &optional (rt-kind 'register))
  `(define-instruction (,name)
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind)
		(rs :argument register)
		(immediate :argument (signed-byte 16)))
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind)
		(rs :argument register)
		(immediate :argument addi-fixup))
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind)
		(rs :argument register)
		(immediate :constant 0))))

(define-load/store-instruction lb #b100000)
(define-load/store-instruction lh #b100001)
(define-load/store-instruction lwl #b100010)
(define-load/store-instruction lw #b100011)
(define-load/store-instruction lbu #b100100)
(define-load/store-instruction lhu #b100101)
(define-load/store-instruction lwr #b100110)
(define-load/store-instruction lwc1 #o61 fp-reg)
(define-load/store-instruction lwc1-odd #o61 odd-fp-reg)
(define-load/store-instruction sb #b101000)
(define-load/store-instruction sh #b101001)
(define-load/store-instruction swl #b101010)
(define-load/store-instruction sw #b101011)
(define-load/store-instruction swr #b101110)
(define-load/store-instruction swc1 #o71 fp-reg)
(define-load/store-instruction swc1-odd #o71 odd-fp-reg)

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



;;;; Floating point instructions.

(macrolet ((frob (name kind)
	     `(define-instruction (,name)
		(register (op :constant #b010001)
			  (rs :constant #b00100)
			  (rd :argument ,kind)
			  (rt :argument register)
			  (funct :constant 0)))))
  (frob mtc1 fp-reg)
  (frob mtc1-odd odd-fp-reg))

(macrolet ((frob (name kind)
	     `(define-instruction (,name)
		(register (op :constant #b010001)
			  (rs :constant #b00000)
			  (rt :argument register)
			  (rd :argument ,kind)
			  (funct :constant 0)))))
  (frob mfc1 fp-reg)
  (frob mfc1-odd odd-fp-reg))


(define-instruction (float-op)
  (float (funct :argument float-operation)
	 (format :argument float-format)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (ft :argument fp-reg)))


(define-instruction (fabs)
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (funct :constant #b000101))
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :same-as fd)
	 (funct :constant #b000101)))

(define-instruction (fneg)
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (funct :constant #b000101))
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :same-as fd)
	 (funct :constant #b000111)))


(define-instruction (fcvt)
  (float-aux (sub-funct :argument float-format)
	     (format :argument float-format)
	     (fd :argument fp-reg)
	     (fs :argument fp-reg)
	     (funct :constant #b10)))

  
(define-instruction (fcmp)
  (float-aux (sub-funct :argument compare-kind)
	     (format :argument float-format)
	     (fd :constant 0)
	     (fs :argument fp-reg)
	     (ft :argument fp-reg)
	     (funct :constant #b11)))


;;;; Pseudo-instructions

(define-instruction (move)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :argument register)
	    (rt :constant 0)
	    (funct :constant #b100001))
  (float (format :argument float-format)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (ft :constant 0)
	 (funct :constant #b000110)))

(define-pseudo-instruction li 64 (reg value)
  (etypecase value
    ((unsigned-byte 16)
     (inst or reg zero-tn value))
    ((signed-byte 16)
     (inst addu reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst lui reg
	   #+new-compiler (ldb (byte 16 16) value)
	   #-new-compiler (logand #xffff (ash value -16)))
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
  (short-format (data :argument (or (unsigned-byte 16) (signed-byte 16)))))

(define-format (byte-format 8)
  (data (byte 8 0)))
(define-instruction (byte)
  (byte-format (data :argument (or (unsigned-byte 8) (signed-byte 8)))))



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


(defmacro define-compute-instruction (name calculation)
  (let ((addui (symbolicate name "-ADDUI"))
	(lui (symbolicate name "-LUI"))
	(ori (symbolicate name "-ORI")))
    `(progn
       (defun ,name (label)
	 (let ((result ,calculation))
	   (assert (typep result '(signed-byte 16)))
	   result))
       (define-instruction (,addui)
	 (immediate (op :constant #b001001)
		    (rt :argument register)
		    (rs :argument register)
		    (immediate :argument label
			       :function ,name)))
       (define-instruction (,lui)
	 (immediate (op :constant #b001111)
		    (rs :constant 0)
		    (rt :argument register)
		    (immediate :argument label
			       :function (lambda (label)
					   (ash ,calculation -16)))))
       (define-instruction (,ori)
	 (immediate (op :constant #b001101)
		    (rt :argument register)
		    (rs :same-as rt)
		    (immediate :argument label
			       :function (lambda (label)
					   (logand ,calculation #xffff)))))
       (define-pseudo-instruction ,name 96 (dst src label temp)
	 (cond ((typep ,calculation '(signed-byte 16))
		(inst ,addui dst src label))
	       (t
		(inst ,lui temp label)
		(inst ,ori temp label)
		(inst addu dst src temp)))))))


;; code = fn - fn-tag - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-fn
			    (- vm:other-pointer-type
			       vm:function-pointer-type
			       (label-position label)
			       (component-header-length)))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-lra
			    (- (+ (label-position label)
				  (component-header-length))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-compute-instruction compute-lra-from-code
			    (+ (label-position label)
			       (component-header-length)))

