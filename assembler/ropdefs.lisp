;;; -*- Mode: Lisp; Package: Compiler -*-

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************

;;; This file defines the instruction set for the Common Lisp Romp Assembler
;;; used by the compiler.

;;; Written by David B. McDonald.

(in-package 'Compiler)

(defvar romp-4bit-opcode-symbol (make-array 16))
(defvar romp-8bit-opcode-symbol (make-array 256))

;;; Define-Romp-Instruction defines a Romp instruction to the assembler.
;;; It accepts three arguments:
;;;	romp-instruction is a symbol whose pname is the name of a Romp instruction
;;;	romp-inst-type specifies the underlying type of the Romp instruction.
;;;		it must have one of the following values: JI, X, DS, R, BI, BA,
;;;		or D.
;;;	romp-code specifies the numeric op code for the instruction.


(defmacro Define-Romp-Instruction (romp-instruction romp-inst-type romp-code)
  `(progn (setf (get ',romp-instruction 'romp-instruction-type)
		',romp-inst-type)
	  (setf (get ',romp-instruction 'romp-operation-code)
		,romp-code)
	  ,(case romp-inst-type
	     (ji
	      `(setf (svref romp-4bit-opcode-symbol 0) ',romp-instruction))
	     ((x ds)
	      `(setf (svref romp-4bit-opcode-symbol ,romp-code) ',romp-instruction))
	     ((r bi ba d)
	      `(setf (svref romp-8bit-opcode-symbol ,romp-code) ',romp-instruction))
	     (T
	      (error "Illegal instruction type: ~A, for instruction ~A (~A).~%"
		     romp-inst-type romp-instruction romp-code)))))

;;; Define-Romp-Branch defines a Romp branch instruction to the assembler.  It
;;; accepts three arguments:
;;;     romp-branch is a symbol whose pname is the name of a branch instruction.
;;;     romp-instruction is the underlying romp-instruction that should be used
;;;             used to implement the branch instruction.
;;;     condition-code specifies the bit of the condition code that should be
;;;             be tested to implement the branch.

(defmacro Define-Romp-Branch (romp-branch romp-instruction condition-code)
  `(progn (setf (get ',romp-branch 'romp-branch-instruction)
		',romp-instruction)
	  (setf (get ',romp-branch 'romp-condition-code)
		,condition-code)))

;;; Define-Condition-Code associates the integer code for a particular condition
;;; code bit with a symbol.  It accepts a symbol and a value.

(defmacro Define-Condition-Code (condition-code value)
  `(setf (get ',condition-code 'condition-code) ,value))

;;; Storage Access Instructions.

(define-romp-instruction lcs ds #x4)
(define-romp-instruction lc d #xCE)
(define-romp-instruction lhas ds #x5)
(define-romp-instruction lha d #xCA)
(define-romp-instruction lhs r #xEB)
(define-romp-instruction lh d #xDA)
(define-romp-instruction ls ds #x7)
(define-romp-instruction l d #xCD)
(define-romp-instruction lm d #xC9)
(define-romp-instruction tsh d #xCF)
(define-romp-instruction stcs ds #x1)
(define-romp-instruction stc d #xDE)
(define-romp-instruction sths ds #x2)
(define-romp-instruction sth d #xDC)
(define-romp-instruction sts ds #x3)
(define-romp-instruction st d #xDD)
(define-romp-instruction stm d #xD9)

;;; Address Computation Instructions.

(define-romp-instruction cal d #xC8)
(define-romp-instruction cal16 d #xC2)
(define-romp-instruction cau d #xD8)
(define-romp-instruction cas x #x6)
(define-romp-instruction ca16 r #xF3)
(define-romp-instruction inc r #x91)
(define-romp-instruction dec r #x93)
(define-romp-instruction lis r #xA4)

;;; Basic Romp Branch Instructions.

(define-romp-instruction bala ba #x8A)
(define-romp-instruction balax ba #x8B)
(define-romp-instruction bali bi #x8C)
(define-romp-instruction balix bi #x8D)
(define-romp-instruction balr r #xEC)
(define-romp-instruction balrx r #xED)
(define-romp-instruction jb ji #x1)
(define-romp-instruction bb bi #x8E)
(define-romp-instruction bbx bi #x8F)
(define-romp-instruction bbr r #xEE)
(define-romp-instruction bbrx r #xEF)
(define-romp-instruction jnb ji #x0)
(define-romp-instruction bnb bi #x88)
(define-romp-instruction bnbx bi #x89)
(define-romp-instruction bnbr r #xE8)
(define-romp-instruction bnbrx r #xE9)

;;; Romp Trap Instrunctions.

(define-romp-instruction ti d #xCC)
(define-romp-instruction tgte r #xBD)
(define-romp-instruction tlt r #xBE)

;;; Romp Move and Insert Instructions.

(define-romp-instruction mc03 r #xF9)
(define-romp-instruction mc13 r #xFA)
(define-romp-instruction mc23 r #xFB)
(define-romp-instruction mc33 r #xFC)
(define-romp-instruction mc30 r #xFD)
(define-romp-instruction mc31 r #xFE)
(define-romp-instruction mc32 r #xFF)
(define-romp-instruction mftb r #xBC)
(define-romp-instruction mftbil r #x9D)
(define-romp-instruction mftbiu r #x9C)
(define-romp-instruction mttb r #xBF)
(define-romp-instruction mttbil r #x9F)
(define-romp-instruction mttbiu r #x9E)

;;; Romp Arithmetic Instructions.

(define-romp-instruction a r #xE1)
(define-romp-instruction ae r #xF1)
(define-romp-instruction aei d #xD1)
(define-romp-instruction ai d #xC1)
(define-romp-instruction ais r #x90)
(define-romp-instruction abs r #xE0)
(define-romp-instruction onec r #xF4)
(define-romp-instruction twoc r #xE4)
(define-romp-instruction c r #xB4)
(define-romp-instruction cis r #x94)
(define-romp-instruction ci d #xD4)
(define-romp-instruction cl r #xB3)
(define-romp-instruction cli d #xD3)
(define-romp-instruction exts r #xB1)
(define-romp-instruction s r #xE2)
(define-romp-instruction sf r #xB2)
(define-romp-instruction se r #xF2)
(define-romp-instruction sfi d #xD2)
(define-romp-instruction sis r #x92)
(define-romp-instruction d r #xB6)
(define-romp-instruction m r #xE6)

;;; Romp Logical Operations

(define-romp-instruction clrbl r #x99)
(define-romp-instruction clrbu r #x98)
(define-romp-instruction setbl r #x9B)
(define-romp-instruction setbu r #x9A)
(define-romp-instruction n r #xE5)
(define-romp-instruction nilz d #xC5)
(define-romp-instruction nilo d #xC6)
(define-romp-instruction niuz d #xD5)
(define-romp-instruction niuo d #xD6)
(define-romp-instruction o r #xE3)
(define-romp-instruction oil d #xC4)
(define-romp-instruction oiu d #xC3)
(define-romp-instruction x r #xE7)
(define-romp-instruction xil d #xC7)
(define-romp-instruction xiu d #xD7)
(define-romp-instruction clz r #xF5)

;;; Romp Shift Instructions

(define-romp-instruction sar r #xB0)
(define-romp-instruction sari r #xA0)
(define-romp-instruction sari16 r #xA1)
(define-romp-instruction sr r #xB8)
(define-romp-instruction sri r #xA8)
(define-romp-instruction sri16 r #xA9)
(define-romp-instruction srp r #xB9)
(define-romp-instruction srpi r #xAC)
(define-romp-instruction srpi16 r #xAD)
(define-romp-instruction sl r #xBA)
(define-romp-instruction sli r #xAA)
(define-romp-instruction sli16 r #xAB)
(define-romp-instruction slp r #xBB)
(define-romp-instruction slpi r #xAE)
(define-romp-instruction slpi16 r #xAF)

;;; Romp System Control Instructions.

(define-romp-instruction mts r #xB5)
(define-romp-instruction mfs r #x96)
(define-romp-instruction clrsb r #x95)
(define-romp-instruction setsb r #x97)
(define-romp-instruction lps d #xD0)
(define-romp-instruction wait r #xF0)
(define-romp-instruction svc d #xC0)

;;; Romp Input/Output Instructions.

(define-romp-instruction ior d #xCB)
(define-romp-instruction iow d #xDB)

;;; Define bit setting for examining condition codes.

(define-condition-code pz 8)
(define-condition-code lt 9)
(define-condition-code eq 10)
(define-condition-code gt 11)
(define-condition-code cz 12)
(define-condition-code ov 14)
(define-condition-code tb 15)
