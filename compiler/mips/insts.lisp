;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please
;;; contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU).
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.11 1990/03/05 21:06:44 wlott Exp $
;;; 
;;; Assembler instruction definitions for the MIPS R2000.
;;;
;;; Written by Christopher Hoover
;;;

(in-package "C")

;;; Clear out any old definitions.
;;; 
(clrhash *instructions*)
(clrhash *instruction-formats*)


;;;; Formats

(eval-when (compile load eval)

(defconstant special-op #b000000)
(defconstant bcond-op #b0000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)

) ; eval-when

;;;
;;; Load Store Format
;;; 
(def-instruction-format (load-store 4) (rt base &optional (offset 0))
  (op :unsigned 6 :instruction-constant)
  (base :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (offset :signed 16 :immediate))

;;;
;;; Signed Immediate Format
;;; 
(def-instruction-format (signed-immed 4) (rt rs immed)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (immed :signed 16 :immediate))

;;;
;;; Unsigned Immediate Format
;;; 
(def-instruction-format (unsigned-immed 4) (rt rs immed)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (immed :unsigned 16 :immediate))

;;;
;;; LUI Special Format
;;;
(def-instruction-format (lui-special 4) (rt immed)
  (op :unsigned 6 :instruction-constant)
  (rt :unsigned 5 :constant 0)
  (rt :unsigned 5 :register)
  (immed :unsigned 16 :immediate))

;;;
;;; Jump (J) Format
;;;
(def-instruction-format (jump 4) (target)
  (op :unsigned 6 :instruction-constant)
  (target :unsigned 26 :branch #'(lambda (x) (ash x -2)))) ; This isn't right

;;;
;;; JR Special Format
;;; 
(def-instruction-format (jr-special 4) (rs)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (zero :unsigned 15 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; JALR Special Format
;;;
(def-instruction-format (jalr-special 4) (rd rs)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (zero-1 :unsigned 5 :constant 0)
  (rd :unsigned 5 :register)
  (zero-2 :unsigned 5 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;; 
;;; Branch Format
;;;
(def-instruction-format (branch 4) (rs offset)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (zero :unsigned 5 :constant 0)
  (offset :signed 16 :branch #'(lambda (x) (ash (- x 4) -2))))

;;;
;;; Branch-2 Format
;;;
(def-instruction-format (branch-2 4) (rs rt offset)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (offset :signed 16 :branch #'(lambda (x) (ash (- x 4) -2))))

;;;
;;; Branch-Z Format
;;;
(def-instruction-format (branch-z 4) (rs offset)
  (bcond :unsigned 6 :constant bcond-op)
  (rs :unsigned 5 :register)
  (op :unsigned 5 :instruction-constant)
  (offset :signed 16 :branch #'(lambda (x) (ash (- x 4) -2))))

;;;
;;; R3 Format
;;; 
(def-instruction-format (r3 4) (rd rs rt)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (rd :unsigned 5 :register)
  (zero :unsgined 5 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; MF Format
;;;
(def-instruction-format (mf 4) (rd)
  (special :unsigned 6 :constant special-op)
  (zero-1 :unsigned 10 :constant 0)
  (rd :unsigned 5 :register)
  (zero-2 :unsigned 5 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;; 
;;; MT Format
;;;
(def-instruction-format (mt 4) (rs)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (zero :unsigned 15 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; Mult Format
;;;
(def-instruction-format (mult 4) (rs rt)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (zero :unsigned 10 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; Shift Format
;;;
(def-instruction-format (shift 4) (rd rt shamt)
  (special :unsigned 6 :constant special-op)
  (zero :unsgined 5 :constant 0)
  (rt :unsigned 5 :register)
  (rd :unsigned 5 :register)
  (shamt :unsigned 5 :immediate)
  (op :unsigned 6 :instruction-constant))

;;;
;;; Shift-Var Format
;;;
(def-instruction-format (shift-var 4) (rd rt rs)
  (special :unsigned 6 :constant special-op)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (rd :unsigned 5 :register)
  (zero :unsigned 5 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; BREAK-Special Format
;;;
(def-instruction-format (break-special 4) (code)
  (special :unsigned 6 :constant special-op)
  (code :unsigned 10 :immediate) ; The MIPS assembler only uses half the bits.
  (unused :unsigned 10 :constant 0)
  (op :unsigned 6 :instruction-constant))

;;;
;;; SYSCALL-Special Format
;;;
(def-instruction-format (syscall-special 4) ()
  (special :unsigned 6 :constant special-op)
  (zero :unsigned 20 :constant 0)
  (op :unsigned 6 :instruction-constant))


;;;; Instructions

(def-instruction j-inst jump :op #b00010)
(def-instruction jal-inst jump :op #b00011)
(def-instruction jr jr-special :op #b001000)
(def-instruction jalr jalr-special :op #b001001)
(def-instruction beq-inst branch-2 :op #b000100)
(def-instruction bne-inst branch-2 :op #b000101)
(def-instruction blez-inst branch :op #b000110)
(def-instruction bgtz-inst branch :op #b000111)

(def-instruction addi signed-immed :op #b001000)
(def-instruction addiu signed-immed :op #b001001)
(def-instruction slti signed-immed :op #b001010)
(def-instruction sltiu signed-immed :op #b001011)
(def-instruction andi unsigned-immed :op #b001100)
(def-instruction ori unsigned-immed :op #b001101)
(def-instruction xori unsigned-immed :op #b001110)
(def-instruction lui lui-special :op #b001111)

(def-instruction lb load-store :op #b100000)
(def-instruction lh load-store :op #b100001)
(def-instruction lwl load-store :op #b100010)
(def-instruction lw load-store :op #b100011)
(def-instruction lbu load-store :op #b100100)
(def-instruction lhu load-store :op #b100101)
(def-instruction lwr load-store :op #b100110)

(def-instruction sb load-store :op #b101000)
(def-instruction sh load-store :op #b101001)
(def-instruction swl load-store :op #b101010)
(def-instruction sw load-store :op #b101011)
(def-instruction swr load-store :op #b101110)

(def-instruction sll shift :op #b000000)
(def-instruction srl shift :op #b000010)
(def-instruction sra shift :op #b000011)
(def-instruction sllv shift-var :op #b000100)
(def-instruction srlv shift-var :op #b000110)
(def-instruction srav shift-var :op #b000111)

(def-instruction syscall syscall-special :op #b001100)
(def-instruction break break-special :op #b001101)

(def-instruction mfhi mf :op #b010000)
(def-instruction mthi mt :op #b010001)
(def-instruction mflo mf :op #b010010)
(def-instruction mtlo mt :op #b010011)

(def-instruction mult mult :op #b011000)
(def-instruction multu mult :op #b011001)
(def-instruction div mult :op #b011010)
(def-instruction divu mult :op #b011011)

(def-instruction add r3 :op #b100000)
(def-instruction addu r3 :op #b100001)
(def-instruction sub r3 :op #b100010)
(def-instruction subu r3 :op #b100011)
(def-instruction and r3 :op #b100100)
(def-instruction or r3 :op #b100101)
(def-instruction xor r3 :op #b100110)
(def-instruction nor r3 :op #b100111)

(def-instruction slt r3 :op #b101010)
(def-instruction sltu r3 :op #b101011)

(def-instruction bltz-inst branch-z :op #b00000)
(def-instruction bltzal-inst branch-z :op #b10000)

(def-instruction bgez-inst branch-z :op #b00001)
(def-instruction bgezal-inst branch-z :op #b10001)


;;;; Branches

(defmacro most-positive-twos-complement-number (n-bits)
  `(1- (ash 1 (1- ,n-bits))))

(defmacro most-negative-twos-complement-number (n-bits)
  `(- (ash 1 (1- ,n-bits))))


;;;
;;; ### These two aren't right
;;; 
(def-branch j (label) label
  (0 (ash 1 28) (j-inst label)))
;;; 
(def-branch jal (label) label
  (0 (ash 1 28) (jal-inst label)))


(def-branch beq (rs rt label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (beq-inst rs rt label)))

(def-branch bne (rs rt label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bne-inst rs rt label)))

(def-branch blez (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (blez-inst rs label)))

(def-branch bgtz (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bgtz-inst rs label)))

(def-branch bltz (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bltz-inst rs label)))

(def-branch bltzal (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bltzal-inst rs label)))

(def-branch bgez (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bgez-inst rs label)))

(def-branch bgezal (rs label) label
  ((most-negative-twos-complement-number 18)
   (most-positive-twos-complement-number 18)
   (bgezal-inst rs label)))


;;;; Pseduo Instructions to Support Component Dumping

(defun component-header-length (&optional (component *compile-component*))
  (let* ((2comp (component-info component))
	 (constants (ir2-component-constants 2comp))
	 (num-consts (length constants)))
    (ash (logandc2 (1+ num-consts) 1) word-shift)))


;;; COMPUTE-CODE-FROM-FN
;;;
;;; code = fn - fn-tag - header - label-offset + other-pointer-tag
;;; 
(def-instruction-format (compute-code-from-fn-format 4) (rt rs label)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (label :signed 16
	 :label #'(lambda (label-offset instr-offset)
		    (declare (ignore instr-offset))
		    (- other-pointer-type function-pointer-type label-offset
		       (component-header-length)))))

(def-instruction compute-code-from-fn compute-code-from-fn-format
  :op #b001001)


;;; COMPUTE-CODE-FROM-LRA
;;;
;;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
;;; 
(def-instruction-format (compute-code-from-lra-format 4) (rt rs label)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (label :signed 16
	 :label #'(lambda (label-offset instr-offset)
		    (declare (ignore instr-offset))
		    (- (+ label-offset (component-header-length))))))

(def-instruction compute-code-from-lra compute-code-from-lra-format
  :op #b001001)


;;; COMPUTE-LRA-FROM-CODE
;;;
;;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
;;; 
(def-instruction-format (compute-lra-from-code-format 4) (rt rs label)
  (op :unsigned 6 :instruction-constant)
  (rs :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (label :signed 16
	 :label #'(lambda (label-offset instr-offset)
		    (declare (ignore instr-offset))
		    (+ label-offset (component-header-length)))))

(def-instruction compute-lra-from-code compute-lra-from-code-format
  :op #b001001)


;;;
;;; LRA-HEADER-WORD, FUNCTION-HEADER-WORD
;;; 

(def-instruction-format (header-word-format 4) ()
  (data :unsigned 24 :calculation #'(lambda (posn)
				      (ash (+ posn (component-header-length))
					   (- vm:word-shift))))
  (type :unsigned 8 :instruction-constant))

(def-instruction lra-header-word header-word-format
  :type vm:return-pc-header-type)

(def-instruction function-header-word header-word-format
  :type vm:function-header-type)



;;; LOAD-FOREIGN-ADDRESS and LOAD-FOREIGN-VALUE
;;; 
;;; This ``instruction'' emits a LUI followed by either an ADDIU, LW, or SW
;;;
(def-instruction-format (load-foreign-format 8) (rt symbol)
  ;; We need to switch the order of the two instructions because the MIPS
  ;; is little-endian.
  (op2 :unsigned 6 :instruction-constant)
  (rt :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (filler :unsigned 16 :constant 0)
  (op1 :unsigned 6 :constant #b001111)
  (rt :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (symbol :unsigned 16 :fixup :foreign))

(def-instruction load-foreign-address load-foreign-format
  :op2 #b001001)

(def-instruction load-foreign-value load-foreign-format
  :op2 #b100011)

(def-instruction store-foreign-value load-foreign-format
  :op2 #b101011)



;;; LOAD-ASSEMBLY-ADDRESS and LOAD-ASSEMBLY-VALUE
;;; 
;;; This ``instruction'' emits a LUI followed by either an ADDIU, LW, or SW.
;;; The difference is that we use :assembly fixups.
;;;
(def-instruction-format (load-assembly-format 8) (rt symbol)
  ;; We need to switch the order of the two instructions because the MIPS
  ;; is little-endian.
  (op2 :unsigned 6 :instruction-constant)
  (rt :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (filler :unsigned 16 :constant 0)
  (op1 :unsigned 6 :constant #b001111)
  (rt :unsigned 5 :register)
  (rt :unsigned 5 :register)
  (symbol :unsigned 16 :fixup :assembly))

(def-instruction load-assembly-address load-assembly-format
  :op2 #b001001)

(def-instruction load-assembly-value load-assembly-format
  :op2 #b100011)

(def-instruction store-assembly-value load-assembly-format
  :op2 #b101011)



;;; BYTE, SHORT, and WORD instructions.
;;;
;;; These instructions emit a byte, short, or word in the instruction
;;; stream.  If you use them, be sure to use (align 2) afterwords to
;;; assure that additional code gets properly aligned.

(def-instruction-format (byte-format 1) (value)
  (value :unsigned 8 :immediate))
(def-instruction byte byte-format)

(def-instruction-format (short-format 2) (value)
  (value :unsigned 16 :immediate))
(def-instruction short short-format)

(def-instruction-format (word-format 4) (value)
  (value :unsigned 32 :immediate))
(def-instruction word word-format)

