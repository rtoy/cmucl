;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/insts.lisp,v 1.9 1992/07/09 19:46:32 hallgren Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the SPARC architecture.
;;;
;;; Written by William Lott.
;;;
;;; Converted to the SPARC by Sean Hallgren.
;;;

(in-package "SPARC")

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

(defun fp-reg-tn-encoding (tn &optional odd)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (if odd
      (1+ (tn-offset tn))
      (tn-offset tn)))

(defconstant branch-conditions
  '(:f :eq :le :lt :leu :ltu :n :vs :t :ne :gt :ge :gtu :geu :p :vc))

(deftype branch-condition ()
  `(member ,@branch-conditions))

(defun branch-condition (condition)
  (or (position condition branch-conditions)
      (error "Unknown branch condition: ~S~%Must be one of: ~S"
	     condition branch-conditions)))

(defconstant fp-branch-conditions
  '(:f :ne :lg :ul :l :ug :g :u :t :eq :ue :ge :uge :le :ule :o))

(deftype fp-branch-condition ()
  `(member ,@fp-branch-conditions))

(defun fp-branch-condition (condition)
  (or (position condition fp-branch-conditions)
      (error "Unknown fp-branch condition: ~S~%Must be one of: ~S"
	     condition fp-branch-conditions)))


;;;; Primitive emitters.

(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))

(define-emitter emit-format-1 32
  (byte 2 30) (byte 30 0))

(define-emitter emit-format-2-immed 32
  (byte 2 30) (byte 5 25) (byte 3 22) (byte 22 0))

(define-emitter emit-format-2-branch 32
  (byte 2 30) (byte 1 29) (byte 4 25) (byte 3 22) (byte 22 0))

(define-emitter emit-format-2-unimp 32
  (byte 2 30) (byte 5 25) (byte 3 22) (byte 22 0))

(define-emitter emit-format-3-reg 32
  (byte 2 30) (byte 5 25) (byte 6 19) (byte 5 14) (byte 1 13) (byte 8 5)
  (byte 5 0))

(define-emitter emit-format-3-immed 32
  (byte 2 30) (byte 5 25) (byte 6 19) (byte 5 14) (byte 1 13) (byte 13 0))


(define-emitter emit-format-3-fpop 32
  (byte 2 30) (byte 5 25) (byte 6 19) (byte 5 14) (byte 9 5) (byte 5 0))



;;;; Buncha format-3-instructions.

(defun emit-format-3-inst (segment op op3 dst src1 src2 &key load-store fixup)
  (unless src2
    (cond ((and (typep src1 'tn) load-store)
	   (setf src2 0))
	  (t
	   (setf src2 src1)
	   (setf src1 dst))))
  (etypecase src2
    (tn
     (emit-format-3-reg segment op (reg-tn-encoding dst) op3
			(reg-tn-encoding src1) 0 0 (reg-tn-encoding src2)))
    (integer
     (emit-format-3-immed segment op (reg-tn-encoding dst) op3
			  (reg-tn-encoding src1) 1 src2))
    (fixup
     (unless (or load-store fixup)
       (error "Fixups aren't allowed."))
     (note-fixup segment :add src2)
     (emit-format-3-immed segment op (reg-tn-encoding dst) op3
			  (reg-tn-encoding src1) 1 0))))

(defun emit-fp-format-3-inst (segment op op3 dst src1 src2
				      &key load-store fixup odd)
  (unless src2
    (cond ((and (typep src1 'tn) load-store)
	   (setf src2 0))
	  (t
	   (setf src2 src1)
	   (setf src1 dst))))
  (etypecase src2
    (tn
     (emit-format-3-reg segment op (fp-reg-tn-encoding dst odd) op3
			(reg-tn-encoding src1) 0 0
			(reg-tn-encoding src2)))
    (integer
     (emit-format-3-immed segment op (fp-reg-tn-encoding dst odd) op3
			  (reg-tn-encoding src1) 1 src2))
    (fixup
     (unless (or load-store fixup)
       (error "Fixups aren't allowed."))
     (note-fixup segment :add src2)
     (emit-format-3-immed segment op (fp-reg-tn-encoding dst odd) op3
			  (reg-tn-encoding src1) 1 0))))


(eval-when (compile eval)

(defmacro define-f3-inst (name op op3 &key fixup load-store dest-kind)
  `(define-instruction ,name (segment dst src1 &optional src2)
     (:declare (type tn dst)
	       ,(if (or fixup load-store)
		    '(type (or tn (signed-byte 13) null fixup) src1 src2)
		    '(type (or tn (signed-byte 13) null) src1 src2)))
     ,(if dest-kind
	  `(:emitter (emit-fp-format-3-inst segment ,op ,op3 dst src1 src2
					    :load-store ,load-store
					    :fixup ,fixup
					    :odd (eq ',dest-kind 'odd-fp-reg)))
	  `(:emitter (emit-format-3-inst segment ,op ,op3 dst src1 src2
					 :load-store ,load-store
					 :fixup ,fixup)))))

) ; eval-when (compile eval)

(define-f3-inst ldsb #b11 #b001001 :load-store :load)
(define-f3-inst ldsh #b11 #b001010 :load-store :load)
(define-f3-inst ldub #b11 #b000001 :load-store :load)
(define-f3-inst lduh #b11 #b000010 :load-store :load)
(define-f3-inst ld #b11 #b000000 :load-store :load)
(define-f3-inst ldd #b11 #b000011 :load-store :load)
(define-f3-inst ldf #b11 #b100000 :dest-kind fp-reg :load-store :load)
(define-f3-inst ldf-odd #b11 #b100000 :dest-kind odd-fp-reg :load-store :load)
(define-f3-inst lddf #b11 #b100011 :dest-kind fp-reg :load-store :load)
(define-f3-inst stb #b11 #b000101 :load-store :store)
(define-f3-inst sth #b11 #b000110 :load-store :store)
(define-f3-inst st #b11 #b000100 :load-store :store)
(define-f3-inst std #b11 #b000111 :load-store :store)
(define-f3-inst stf #b11 #b100100 :dest-kind fp-reg :load-store :store)
(define-f3-inst stf-odd #b11 #b100100 :dest-kind odd-fp-reg :load-store :store)(define-f3-inst stdf #b11 #b100111 :dest-kind fp-reg :load-store :store)
(define-f3-inst ldstub #b11 #b001101 :load-store t)
(define-f3-inst swap #b11 #b001111 :load-store t)
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
(define-f3-inst save #b10 #b111100)
(define-f3-inst restore #b10 #b111101)


;;;; Random instructions.

(define-instruction ldfsr (segment src1 src2)
  (:declare (type tn src1) (type (signed-byte 13) src2))
  (:emitter (emit-format-3-immed segment #b11 0 #b100001
				 (reg-tn-encoding src1) 1 src2)))

(define-instruction stfsr (segment src1 src2)
  (:declare (type tn src1) (type (signed-byte 13) src2))
  (:emitter (emit-format-3-immed segment #b11 0 #b100101 
				 (reg-tn-encoding src1) 1 src2)))

(define-instruction sethi (segment dst src1)
  (:declare (type tn dst)
	    (type (or (signed-byte 22) (unsigned-byte 22) fixup) src1))
  (:emitter
   (etypecase src1
     (integer
      (emit-format-2-immed segment #b00 (reg-tn-encoding dst) #b100
				 src1))
     (fixup
      (note-fixup segment :sethi src1)
      (emit-format-2-immed segment #b00 (reg-tn-encoding dst) #b100 0)))))
			   

(define-instruction rdy (segment dst)
  (:declare (type tn dst))
  (:emitter (emit-format-3-immed segment #b10 (reg-tn-encoding dst) #b101000
				 0 1 0)))

(define-instruction wry (segment src1 &optional src2)
  (:declare (type tn src1) (type (or (signed-byte 13) tn null) src2))
  (:emitter
   (etypecase src2
     (null 
      (emit-format-3-reg segment #b10 0 #b110000 (reg-tn-encoding src1) 0 0 0))
     (tn
      (emit-format-3-reg segment #b10 0 #b110000 (reg-tn-encoding src1) 0 0
			 (reg-tn-encoding src2)))
     (integer
      (emit-format-3-immed segment #b10 0 #b110000 (reg-tn-encoding src1) 1
			   src2)))))

(define-instruction unimp (segment data)
  (:declare (type (unsigned-byte 22) data))
  (:emitter (emit-format-2-unimp segment 0 0 0 data)))



;;;; Branch instructions.

(defun emit-relative-branch (segment a op2 cond-or-target target &optional fp)
  (emit-back-patch segment 4
		  #'(lambda (segment posn)
		      (unless target
			(setf target cond-or-target)
			(setf cond-or-target :t))
		      (emit-format-2-branch
		       segment #b00 a
		       (if fp
			   (fp-branch-condition cond-or-target)
			   (branch-condition cond-or-target))
		       op2
		       (ash (- (label-position target) posn) -2)))))

(define-instruction b (segment cond-or-target &optional target)
  (:declare (type (or label branch-condition) cond-or-target)
	    (type (or label null) target))
  (:emitter
   (emit-relative-branch segment 0 #b010 cond-or-target target)))

(define-instruction ba (segment cond-or-target &optional target)
  (:declare (type (or label branch-condition) cond-or-target)
	    (type (or label null) target))
  (:emitter
   (emit-relative-branch segment 1 #b010 cond-or-target target)))

(define-instruction t (segment condition target)
  (:declare (type branch-condition condition)
	    (type (or (signed-byte 13) (unsigned-byte 13)) target))
  (:emitter (emit-format-3-immed segment #b10 (branch-condition condition)
				 #b111010 0 1 target)))

(define-instruction fb (segment condition target)
  (:declare (type fp-branch-condition condition) (type label target))
  (:emitter
   (emit-relative-branch segment 0 #b110 condition target t)))

(define-instruction jal (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn integer) src1)
	    (type (or null fixup tn (signed-byte 13)) src2))
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 0))
   (etypecase src2
     (tn
      (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b111000
			 (if (integerp src1)
			     src1
			     (reg-tn-encoding src1))
			 0 0 (reg-tn-encoding src2)))
     (integer
      (emit-format-3-immed segment #b10 (reg-tn-encoding dst) #b111000
			   (reg-tn-encoding src1) 1 src2))
     (fixup
      (note-fixup segment :add src2)
      (emit-format-3-immed segment #b10 (reg-tn-encoding dst)
			   #b111000 (reg-tn-encoding src1) 1 0)))))


(define-instruction j (segment src1 &optional src2)
  (:declare (type tn src1) (type (or tn (signed-byte 13) fixup null) src2))
  (:emitter
   (etypecase src2
     (null
      (emit-format-3-reg segment #b10 0 #b111000 (reg-tn-encoding src1) 0 0 0))
     (tn
      (emit-format-3-reg segment #b10 0 #b111000 (reg-tn-encoding src1) 0 0
			 (reg-tn-encoding src2)))
     (integer
      (emit-format-3-immed segment #b10 0 #b111000 (reg-tn-encoding src1) 1
			   src2))
     (fixup
      (note-fixup segment :add src2)
      (emit-format-3-immed segment #b10 0 #b111000 (reg-tn-encoding src1) 1
			   0)))))



;;;; Unary and binary fp insts.

(defun emit-fp-inst (segment opf op3 dst src1 src2)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (emit-format-3-fpop segment #b10 (fp-reg-tn-encoding dst) op3
		      (fp-reg-tn-encoding src1) opf (fp-reg-tn-encoding src2)))

(eval-when (compile eval)

(defmacro define-unary-fp-inst (name opf)
  `(define-instruction ,name (segment dst src1 &optional src2)
     (:declare (type tn dst src1) (type (or null tn) src2))
     (:emitter (emit-fp-inst segment ,opf #b110100 dst src1 src2))))

(defmacro define-binary-fp-inst (name opf &optional (op3 #b110100))
  `(define-instruction ,name (segment dst src1 &optional src2)
     (:declare (type tn dst src1) (type (or null tn) src2))
     (:emitter (emit-fp-inst segment ,opf ,op3 dst src1 src2))))
  
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
(define-unary-fp-inst fmovs-odd #b000000001)
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



;;;; li, jali, ji, nop, cmp, not, neg, move, and more

(define-instruction li (segment reg value)
  (:declare (type tn reg)
	    (type (or (signed-byte 13) (signed-byte 32) (unsigned-byte 32)
		      fixup) value))
  (:vop-var vop)
  (:emitter
   (assemble (segment vop)
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
		(inst add reg value))))))

;;; Jal to a full 32-bit address.  Tmpreg is trashed.
(define-instruction jali (segment link tmpreg value)
  (:declare (type tn link tmpreg)
	    (type (or (signed-byte 13) (signed-byte 32) (unsigned-byte 32)
		      fixup) value))
  (:vop-var vop)
  (:emitter
   (assemble (segment vop)
     (etypecase value
       ((signed-byte 13)
	(inst jal link zero-tn value))
       ((or (signed-byte 32) (unsigned-byte 32))
	(let ((hi (ldb (byte 22 10) value))
	      (lo (ldb (byte 10 0) value)))
	  (inst sethi tmpreg hi)
	  (inst jal link tmpreg lo)))
       (fixup
	(inst sethi tmpreg value)
	(inst jal link tmpreg value))))))

;;; Jump to a full 32-bit address.  Tmpreg is trashed.
(define-instruction ji (segment tmpreg value)
  (:declare (type tn tmpreg)
	    (type (or (signed-byte 13) (signed-byte 32) (unsigned-byte 32)
		      fixup) value))
  (:vop-var vop)
  (:emitter
   (assemble (segment vop)
	     (inst jali zero-tn tmpreg value))))

(define-instruction nop (segment)
  (:emitter (emit-format-2-immed segment 0 0 #b100 0)))

(define-instruction cmp (segment src1 &optional src2)
  (:declare (type tn src1) (type (or null tn (signed-byte 13)) src2))
  (:emitter
   (etypecase src2
     (null
      (emit-format-3-reg segment #b10 0 #b010100 (reg-tn-encoding src1) 0 0 0))
     (tn
      (emit-format-3-reg segment #b10 0 #b010100 (reg-tn-encoding src1) 0 0
			 (reg-tn-encoding src2)))
     (integer
      (emit-format-3-immed segment #b10 0 #b010100 (reg-tn-encoding src1) 1
			   src2)))))

(define-instruction not (segment dst &optional src1)
  (:declare (type tn dst) (type (or tn null) src1))
  (:emitter
   (unless src1
     (setf src1 dst))
   (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000111
		      (reg-tn-encoding src1) 0 0 0)))

(define-instruction neg (segment dst &optional src1)
  (:declare (type tn dst) (type (or tn null) src1))
  (:emitter
   (unless src1
     (setf src1 dst))
   (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000100
		      0 0 0 (reg-tn-encoding src1))))

(define-instruction move (segment dst src1)
  (:declare (type tn dst src1))
  (:emitter (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000010
			       0 0 0 (reg-tn-encoding src1))))


;;;; Instructions for dumping data and header objects.

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

(define-emitter emit-header-object 32
  (byte 24 8) (byte 8 0))


  
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


;;;; Instructions for converting between code objects, functions, and lras.


(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (let ((delta (funcall calc label posn delta-if-after)))
	 (when (<= (- (ash 1 12)) delta (1- (ash 1 12)))
	   (emit-back-patch segment 4
			    #'(lambda (segment posn)
				(assemble (segment vop)
					  (inst add dst src
						(funcall calc label posn 0)))))
	   t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
	 (assemble (segment vop)
		   (inst sethi temp (ldb (byte 22 10) delta))
		   (inst or temp (ldb (byte 10 0) delta))
		   (inst add dst src temp))))))

;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:reads src)
  (:writes (list dst temp))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     function-pointer-type
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
