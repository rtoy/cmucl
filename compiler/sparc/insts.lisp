;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/insts.lisp,v 1.3 1990/12/17 19:36:52 wlott Exp $
;;;
;;; Description of the SPARC architecture.
;;;
;;; Written by William Lott
;;;

(in-package "SPARC")
(use-package "ASSEM")
(use-package "EXT")
(use-package "C")



;;;; Formats:

(define-format (format-1 32)
  (op (byte 2 30) :default 1)
  (disp (byte 30 0)))


(define-format (format-2-immed 32)
  (op (byte 2 30) :default 0)
  (rd (byte 5 25))
  (op2 (byte 3 22))
  (immed (byte 22 0)))

(define-format (format-2-branch 32)
  (op (byte 2 30) :default 0)
  (a (byte 1 29))
  (cond (byte 4 25))
  (op2 (byte 3 22))
  (disp (byte 22 0)))

(define-format (format-2-unimp 32)
  (op (byte 2 30) :default 0)
  (ignore (byte 5 25) :default 0)
  (op2 (byte 3 22) :default 0)
  (data (byte 22 0)))


(define-format (format-3-reg 32)
  (op (byte 2 30))
  (rd (byte 5 25))
  (op3 (byte 6 19))
  (rs1 (byte 5 14))
  (i (byte 1 13) :default 0)
  (asi (byte 8 5) :default 0)
  (rs2 (byte 5 0)))

(define-format (format-3-immed 32)
  (op (byte 2 30))
  (rd (byte 5 25))
  (op3 (byte 6 19))
  (rs1 (byte 5 14))
  (i (byte 1 13) :default 1)
  (immed (byte 13 0)))

(define-format (format-3-fpop 32)
  (op (byte 2 30))
  (rd (byte 5 25))
  (op3 (byte 6 19))
  (rs1 (byte 5 14))
  (opf (byte 9 5))
  (rs2 (byte 5 0)))



;;;; Special argument types and fixups.

(define-argument-type reg
  :type '(satisfies (lambda (object)
		      (and (tn-p object)
			   (or (eq (sc-name (tn-sc object)) 'null)
			       (eq (sc-name (tn-sc object)) 'zero)
			       (eq (sb-name (sc-sb (tn-sc object)))
				   'registers)))))
  :function (lambda (tn)
	      (case (sc-name (tn-sc tn))
		(null null-offset)
		(zero 0)
		(t (tn-offset tn)))))


(define-argument-type fp-reg
  :type '(satisfies (lambda (object)
		      (and (tn-p object)
			   (eq (sb-name (sc-sb (tn-sc object)))
			       'float-registers))))
  :function tn-offset)

(define-argument-type odd-fp-reg
  :type '(satisfies (lambda (object)
		      (and (tn-p object)
			   (eq (sb-name (sc-sb (tn-sc object)))
			       'float-registers))))
  :function (lambda (tn) (1+ (tn-offset tn))))


(define-argument-type relative-label
  :type 'label
  :function (lambda (label)
	      (ash (- (label-position label) *current-position*) -2)))


(eval-when (compile eval load)
  (defconstant branch-conditions
    '(:f :eq :le :lt :leu :ltu :n :vs :t :ne :gt :ge :gtu :geu :p :vc)))

(define-argument-type branch-condition
  :type '(member . #.branch-conditions)
  :function (lambda (cond) (position cond branch-conditions)))


(eval-when (compile eval load)
  (defconstant branch-fp-conditions
    '(:f :ne :lg :ul :l :ug :g :u :t :eq :ue :ge :uge :le :ule :o)))


(define-argument-type branch-fp-condition
  :type '(member . #.branch-fp-conditions)
  :function (lambda (fp-cond) (position fp-cond branch-fp-conditions)))


(define-fixup-type :call)
(define-fixup-type :sethi)
(define-fixup-type :add)



;;;; Instructions.



(eval-when (compile eval)

(defmacro define-f3-inst (name op op3 &key (dest-kind 'reg) fixup load-store)
  `(define-instruction (,name)
     (format-3-reg (op :constant ,op)
		   (rd :argument ,dest-kind)
		   (op3 :constant ,op3)
		   (rs1 :argument reg)
		   (rs2 :argument reg))
     ,(if (not load-store)
	  `(format-3-reg (op :constant ,op)
			 (rd :argument ,dest-kind)
			 (op3 :constant ,op3)
			 (rs1 :same-as rd)
			 (rs2 :argument reg))
	  `(format-3-immed (op :constant ,op)
			   (rd :argument ,dest-kind)
			   (op3 :constant ,op3)
			   (rs1 :argument reg)
			   (immed :constant 0)))
     (format-3-immed (op :constant ,op)
		     (rd :argument ,dest-kind)
		     (op3 :constant ,op3)
		     (rs1 :argument reg)
		     (immed :argument (signed-byte 13)))
     (format-3-immed (op :constant ,op)
		     (rd :argument ,dest-kind)
		     (op3 :constant ,op3)
		     (rs1 :same-as rd)
		     (immed :argument (signed-byte 13)))
     ,@(when (or load-store fixup)
	 `((format-3-immed (op :constant ,op)
			   (rd :argument ,dest-kind)
			   (op3 :constant ,op3)
			   (rs1 :argument reg)
			   (immed :argument add-fixup))
	   (format-3-immed (op :constant ,op)
			   (rd :argument ,dest-kind)
			   (op3 :constant ,op3)
			   (rs1 :same-as rd)
			   (immed :argument add-fixup))))))

(setf (macro-function 'define-f3-inst)
      (compile nil (function-lambda-expression
		    (macro-function 'define-f3-inst))))

) ; eval-when

(define-f3-inst ldsb #b11 #b001001 :load-store t)
(define-f3-inst ldsh #b11 #b001010 :load-store t)
(define-f3-inst ldub #b11 #b000001 :load-store t)
(define-f3-inst lduh #b11 #b000010 :load-store t)
(define-f3-inst ld #b11 #b000000 :load-store t)
(define-f3-inst ldd #b11 #b000011 :load-store t)

(define-f3-inst ldf #b11 #b100000 :dest-kind fp-reg :load-store t)
(define-f3-inst ldf-odd #b11 #b100000 :dest-kind odd-fp-reg :load-store t)
(define-f3-inst lddf #b11 #b100011 :dest-kind fp-reg :load-store t)

(define-f3-inst stb #b11 #b000101 :load-store t)
(define-f3-inst sth #b11 #b000110 :load-store t)
(define-f3-inst st #b11 #b000100 :load-store t)
(define-f3-inst std #b11 #b000111 :load-store t)

(define-f3-inst stf #b11 #b100100 :dest-kind fp-reg :load-store t)
(define-f3-inst stf-odd #b11 #b100100 :dest-kind odd-fp-reg :load-store t)
(define-f3-inst stdf #b11 #b100111 :dest-kind fp-reg :load-store t)

(define-f3-inst ldstub #b11 #b001101 :load-store t)
(define-f3-inst swap #b11 #b001111 :load-store t)

(define-instruction (ldfsr)
  (format-3-immed (op :constant #b11)
		  (rd :constant 0)
		  (op3 :constant #b100001)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-instruction (stfsr)
  (format-3-immed (op :constant #b11)
		  (rd :constant 0)
		  (op3 :constant #b100101)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-f3-inst add #b10 #b000000 :fixup t)
(define-f3-inst addcc #b10 #b010000)
(define-f3-inst addx #b10 #b001000)
(define-f3-inst addxcc #b10 #b011000)

(define-f3-inst taddcc #b10 #b100000)
(define-f3-inst taddcctv #b10 #b100010)

(define-f3-inst sub #b10 #b000100)
(define-f3-inst subcc #b10 #b010100)
(define-f3-inst subx #b10 #b001100)
(define-f3-inst subxcc #b10 #b011100)

(define-f3-inst tsubcc #b10 #b100001)
(define-f3-inst tsubcctv #b10 #b100011)

(define-f3-inst mulscc #b10 #b100100)

(define-f3-inst and #b10 #b000001)
(define-f3-inst andcc #b10 #b010001)
(define-f3-inst andn #b10 #b000101)
(define-f3-inst andncc #b10 #b010101)
(define-f3-inst or #b10 #b000010)
(define-f3-inst orcc #b10 #b010010)
(define-f3-inst orn #b10 #b000110)
(define-f3-inst orncc #b10 #b010110)
(define-f3-inst xor #b10 #b000011)
(define-f3-inst xorcc #b10 #b010011)
(define-f3-inst xnor #b10 #b000111)
(define-f3-inst xnorcc #b10 #b010111)

(define-f3-inst sll #b10 #b100101)
(define-f3-inst srl #b10 #b100110)
(define-f3-inst sra #b10 #b100111)

(define-instruction (sethi)
  (format-2-immed (rd :argument reg)
		  (op2 :constant #b100)
		  (immed :argument (or (unsigned-byte 22) (signed-byte 22))))
  (format-2-immed (rd :argument reg)
		  (op2 :constant #b100)
		  (immed :argument sethi-fixup)))

(define-f3-inst save #b10 #b111100)
(define-f3-inst restore #b10 #b111101)

(define-instruction (b)
  (format-2-branch (op :constant #b00)
		   (a :constant 0)
		   (cond :argument branch-condition)
		   (op2 :constant #b010)
		   (disp :argument relative-label))
  (format-2-branch (op :constant #b00)
		   (a :constant 0)
		   (cond :constant #b1000)
		   (op2 :constant #b010)
		   (disp :argument relative-label)))

(define-instruction (ba)
  (format-2-branch (op :constant #b00)
		   (a :constant 1)
		   (cond :argument branch-condition)
		   (op2 :constant #b010)
		   (disp :argument relative-label))
  (format-2-branch (op :constant #b00)
		   (a :constant 1)
		   (cond :constant #b1000)
		   (op2 :constant #b010)
		   (disp :argument relative-label)))

(define-instruction (t)
  (format-3-immed (op :constant #b10)
		  (rd :argument branch-condition)
		  (op3 :constant #b111010)
		  (rs1 :constant 0)
		  (immed :argument (or (signed-byte 13) (unsigned-byte 13)))))

(define-instruction (fb)
  (format-2-branch (op :constant #b00)
		   (a :constant 0)
		   (cond :argument branch-fp-condition)
		   (op2 :constant #b110)
		   (disp :argument relative-label)))


(define-instruction (jal)
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b111000)
		(rs1 :argument reg)
		(rs2 :argument reg))
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b111000)
		(rs1 :constant 0)
		(rs2 :argument reg))
  (format-3-immed (op :constant #b10)
		  (rd :argument reg)
		  (op3 :constant #b111000)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-instruction (j)
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b111000)
		(rs1 :argument reg)
		(rs2 :argument reg))
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b111000)
		(rs1 :argument reg)
		(rs2 :constant 0))
  (format-3-immed (op :constant #b10)
		  (rd :constant 0)
		  (op3 :constant #b111000)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-instruction (rdy)
  (format-3-immed (op :constant #b10)
		  (rd :argument reg)
		  (op3 :constant #b101000)
		  (rs1 :constant 0)
		  (immed :constant 0)))

(define-instruction (wry)
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b110000)
		(rs1 :argument reg)
		(rs2 :argument reg))
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b110000)
		(rs1 :argument reg)
		(rs2 :constant 0))
  (format-3-immed (op :constant #b10)
		  (rd :constant 0)
		  (op3 :constant #b110000)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-instruction (unimp)
  (format-2-unimp (data :argument (unsigned-byte 22))))


(eval-when (compile eval)

(defmacro define-unary-fp-inst (name opf &optional odd)
  (let ((kind (if odd 'odd-fp-reg 'fp-reg)))
    `(define-instruction (,name)
       (format-3-fpop (op :constant #b10)
		      (rd :argument ,kind)
		      (op3 :constant #b110100)
		      (rs1 :argument ,kind)
		      (opf :constant ,opf)
		      (rs2 :argument ,kind))
       (format-3-fpop (op :constant #b10)
		      (rd :argument ,kind)
		      (op3 :constant #b110100)
		      (rs1 :same-as rd)
		      (opf :constant ,opf)
		      (rs2 :argument ,kind)))))

(defmacro define-binary-fp-inst (name opf &optional (op3 #b110100))
  `(define-instruction (,name)
     (format-3-fpop (op :constant #b10)
		    (rd :argument fp-reg)
		    (op3 :constant ,op3)
		    (rs1 :argument fp-reg)
		    (opf :constant ,opf)
		    (rs2 :argument fp-reg))
     (format-3-fpop (op :constant #b10)
		    (rd :argument fp-reg)
		    (op3 :constant ,op3)
		    (rs1 :same-as rd)
		    (opf :constant ,opf)
		    (rs2 :argument fp-reg))))

); eval-when (compile eval)

(define-unary-fp-inst fitos #b011000100)
(define-unary-fp-inst fitod #b011001000)
(define-unary-fp-inst fitox #b011001100)

(define-unary-fp-inst fstoir #b011000001)
(define-unary-fp-inst fdtoir #b011000010)
(define-unary-fp-inst fxtoir #b011000011)

(define-unary-fp-inst fstoi #b011010001)
(define-unary-fp-inst fdtoi #b011010010)
(define-unary-fp-inst fxtoi #b011010011)

(define-unary-fp-inst fstod #b011001001)
(define-unary-fp-inst fstox #b011001101)
(define-unary-fp-inst fdtos #b011000110)
(define-unary-fp-inst fdtox #b011001110)
(define-unary-fp-inst fxtos #b011000111)
(define-unary-fp-inst fxtod #b011001011)

(define-unary-fp-inst fmovs #b000000001)
(define-unary-fp-inst fmovs-odd #b000000001 t)
(define-unary-fp-inst fnegs #b000000101)
(define-unary-fp-inst fabss #b000001001)

(define-unary-fp-inst fsqrts #b000101001)
(define-unary-fp-inst fsqrtd #b000101010)
(define-unary-fp-inst fsqrtx #b000101011)


(define-binary-fp-inst fadds #b001000001)
(define-binary-fp-inst faddd #b001000010)
(define-binary-fp-inst faddx #b001000011)
(define-binary-fp-inst fsubs #b001000101)
(define-binary-fp-inst fsubd #b001000110)
(define-binary-fp-inst fsubx #b001000111)

(define-binary-fp-inst fmuls #b001001001)
(define-binary-fp-inst fmuld #b001001010)
(define-binary-fp-inst fmulx #b001001011)
(define-binary-fp-inst fdivs #b001001101)
(define-binary-fp-inst fdivd #b001001110)
(define-binary-fp-inst fdivx #b001001111)

(define-binary-fp-inst fcmps #b001010001 #b110101)
(define-binary-fp-inst fcmpd #b001010010 #b110101)
(define-binary-fp-inst fcmpx #b001010011 #b110101)
(define-binary-fp-inst fcmpes #b001010101 #b110101)
(define-binary-fp-inst fcmped #b001010110 #b110101)
(define-binary-fp-inst fcmpex #b001010111 #b110101)




;;;; Pseudo-instructions, etc.

(define-pseudo-instruction li 64 (reg value)
  (etypecase value
    ((signed-byte 13)
     (inst add reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (let ((hi (ldb (byte 22 10) value))
	   (lo (ldb (byte 10 0) value)))
       (inst sethi reg hi)
       (unless (zerop lo)
	 (inst add reg lo))))
    (fixup
     (inst sethi reg value)
     (inst add reg value))))

(define-instruction (nop)
  (format-2-immed (rd :constant 0)
		  (op2 :constant #b100)
		  (immed :constant 0)))

(define-instruction (cmp)
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b010100)
		(rs1 :argument reg)
		(rs2 :argument reg))
  (format-3-reg (op :constant #b10)
		(rd :constant 0)
		(op3 :constant #b010100)
		(rs1 :argument reg)
		(rs2 :constant 0))
  (format-3-immed (op :constant #b10)
		  (rd :constant 0)
		  (op3 :constant #b010100)
		  (rs1 :argument reg)
		  (immed :argument (signed-byte 13))))

(define-instruction (not)
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b000111)
		(rs1 :argument reg)
		(rs2 :constant 0))
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b000111)
		(rs1 :same-as rd)
		(rs2 :constant 0)))

(define-instruction (neg)
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b000100)
		(rs1 :constant 0)
		(rs2 :argument reg))
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b000100)
		(rs1 :constant 0)
		(rs2 :same-as rd)))

(define-instruction (move)
  (format-3-reg (op :constant #b10)
		(rd :argument reg)
		(op3 :constant #b000010)
		(rs1 :constant 0)
		(rs2 :argument reg)))


;;; Instructions for dumping data and header objects.

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


(define-format (header-object 32)
  (type (byte 8 0))
  (data (byte 24 8)
	:default 0
	:function (lambda (ignore)
		    (declare (ignore ignore))
		    (ash (+ *current-position* (component-header-length))
			 (- vm:word-shift)))))

(define-instruction (function-header-word)
  (header-object (type :constant vm:function-header-type)))

(define-instruction (lra-header-word)
  (header-object (type :constant vm:return-pc-header-type)))



;;;; Instructions for converting between code objects, functions, and lras

(eval-when (compile eval)

(defmacro define-compute-instruction (name calculation)
  (let ((add (symbolicate name "-ADD"))
	(sethi (symbolicate name "-SETHI"))
	(or (symbolicate name "-OR")))
    `(progn
       (define-instruction (,add)
	 (format-3-immed
	  (op :constant #b10)
	  (rd :argument reg)
	  (op3 :constant #b000000)
	  (rs1 :argument reg)
	  (immed :argument label
		 :function (lambda (label)
			     (let ((result ,calculation))
			       (assert (typep result '(signed-byte 13)))
			       result)))))
       (define-instruction (,sethi)
	 (format-2-immed (rd :argument reg)
			 (op2 :constant #b100)
			 (immed :argument label
				:function (lambda (label)
					    (ash ,calculation -10)))))
       (define-instruction (,or)
	 (format-3-immed (op :constant #b10)
			 (rd :argument reg)
			 (op3 :constant #b000010)
			 (rs1 :same-as rd)
			 (immed :argument label
				:function (lambda (label)
					    (logand ,calculation
						    (1- (ash 1 10)))))))
       (define-pseudo-instruction ,name 96 (dst src label temp)
	 (cond ((typep ,calculation '(signed-byte 13))
		(inst ,add dst src label))
	       (t
		(inst ,sethi temp label)
		(inst ,or temp label)
		(inst add dst src temp)))))))

); eval-when (compile eval)


;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
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

