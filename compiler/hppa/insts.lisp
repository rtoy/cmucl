;;; -*- Package: hppa -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/hppa/insts.lisp,v 1.1 1992/07/13 03:48:23 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the instruction set definition for the HP-PA.
;;;
;;; Written by William Lott.
;;;

(in-package :hppa)

(use-package :new-assem)

(def-assembler-params
  :scheduler-p nil)


;;;; Utility functions.

(defun reg-tn-encoding (tn)
  (declare (type tn tn)
	   (values (unsigned-byte 5)))
  (sc-case tn
    (null null-offset)
    (zero zero-offset)
    (t
     (assert (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
     (tn-offset tn))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn)
	   (values (unsigned-byte 5) (member t nil)))
  (sc-case tn
    (fp-single-zero (values 0 nil))
    (single-reg (values (tn-offset tn) nil))
    (fp-double-zero (values 0 t))
    (double-reg (values (tn-offset tn) t))))

(defconstant compare-conditions
  '(:never := :< :<= :<< :<<= :sv :od :tr :<> :>= :> :>>= :>> :nsv :ev))

(deftype compare-condition ()
  `(member nil ,@compare-conditions))

(defun compare-condition (cond)
  (declare (type compare-condition cond)
	   (values (unsigned-byte 3) (member t nil)))
  (if cond
      (let ((result (or (position cond compare-conditions :test #'eq)
			(error "Bogus Compare/Subtract condition: ~S" cond))))
	(values (ldb (byte 3 0) result)
		(logbitp 3 result)))
      (values 0 nil)))

(defconstant add-conditions
  '(:never := :< :<= :nuv :znv :sv :od :tr :<> :>= :> :uv :vnz :nsv :ev))

(deftype add-condition ()
  `(member nil ,@add-conditions))

(defun add-condition (cond)
  (declare (type add-condition cond)
	   (values (unsigned-byte 3) (member t nil)))
  (if cond
      (let ((result (or (position cond add-conditions :test #'eq)
			(error "Bogus Add condition: ~S" cond))))
	(values (ldb (byte 3 0) result)
		(logbitp 3 result)))
      (values 0 nil)))

(defconstant logical-conditions
  '(:never := :< :<= nil nil nil :od :tr :<> :>= :> nil nil nil :ev))

(deftype logical-condition ()
  `(member nil ,@(remove nil logical-conditions)))

(defun logical-condition (cond)
  (declare (type logical-condition cond)
	   (values (unsigned-byte 3) (member t nil)))
  (if cond
      (let ((result (or (position cond logical-conditions :test #'eq)
			(error "Bogus Logical condition: ~S" cond))))
	(values (ldb (byte 3 0) result)
		(logbitp 3 result)))
      (values 0 nil)))

(defconstant unit-conditions
  '(:never nil :sbz :shz :sdc :sbc :shc :tr nil :nbz :nhz :ndc :nbc :nhc))

(deftype unit-condition ()
  `(member nil ,@(remove nil unit-conditions)))

(defun unit-condition (cond)
  (declare (type unit-condition cond)
	   (values (unsigned-byte 3) (member t nil)))
  (if cond
      (let ((result (or (position cond unit-conditions :test #'eq)
			(error "Bogus Unit condition: ~S" cond))))
	(values (ldb (byte 3 0) result)
		(logbitp 3 result)))
      (values 0 nil)))

(defconstant extract/deposit-conditions
  '(:never := :< :od :tr :<> :>= :ev))

(deftype extract/deposit-condition ()
  `(member nil ,@extract/deposit-conditions))

(defun extract/deposit-condition (cond)
  (declare (type extract/deposit-condition cond)
	   (values (unsigned-byte 3) (member t nil)))
  (if cond
      (or (position cond extract/deposit-conditions :test #'eq)
	  (error "Bogus Extract/Deposit condition: ~S" cond))
      0))


(defun space-encoding (space)
  (declare (type (unsigned-byte 3) space)
	   (values (unsigned-byte 3)))
  (dpb (ldb (byte 2 0) space)
       (byte 2 1)
       (ldb (byte 1 2) space)))



;;;; Load and Store stuff.

(define-emitter emit-load/store 32
  (byte 6 26)
  (byte 5 21)
  (byte 5 16)
  (byte 2 14)
  (byte 14 0))


(defun im14-encoding (segment disp)
  (declare (type (or fixup (signed-byte 14))))
  (cond ((fixup-p disp)
	 (note-fixup segment :load disp)
	 (assert (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
	 0)
	(t
	 (dpb (ldb (byte 13 0) disp)
	      (byte 13 1)
	      (ldb (byte 1 13) disp)))))

(eval-when (compile eval)
  (defmacro define-load-inst (name opcode)
    `(define-instruction ,name (segment disp base reg)
       (:declare (type tn reg base)
		 (type (or fixup (signed-byte 14)) disp))
       (:emitter
	(emit-load/store segment ,opcode
			 (reg-tn-encoding base) (reg-tn-encoding reg) 0
			 (im14-encoding segment disp)))))  
  (defmacro define-store-inst (name opcode)
    `(define-instruction ,name (segment reg disp base)
       (:declare (type tn reg base)
		 (type (or fixup (signed-byte 14)) disp))
       (:emitter
	(emit-load/store segment ,opcode
			 (reg-tn-encoding base) (reg-tn-encoding reg) 0
			 (im14-encoding segment disp))))))

(define-load-inst ldw #x12)
(define-load-inst ldh #x11)
(define-load-inst ldb #x10)
(define-load-inst ldwm #x13)

(define-store-inst stw #x1A)
(define-store-inst sth #x19)
(define-store-inst stb #x18)
(define-store-inst stwm #x1B)

(define-emitter emit-extended-load/store 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 2 14) (byte 1 13)
  (byte 3 10) (byte 4 6) (byte 1 5) (byte 5 0))

(eval-when (compile eval)
  (defmacro define-load-indexed-inst (name opcode)
    `(define-instruction ,name (segment index base reg &key modify scale)
       (:declare (type tn reg base index)
		 (type (member t nil) modify scale))
       (:emitter
	(emit-extended-load/store
	 segment #x03 (reg-tn-encoding base) (reg-tn-encoding index)
	 0 (if scale 1 0) 0 ,opcode (if modify 1 0)
	 (reg-tn-encoding reg))))))

(define-load-indexed-inst ldwx 2)
(define-load-indexed-inst ldhx 1)
(define-load-indexed-inst ldbx 0)
(define-load-indexed-inst ldcwx 7)

(defun short-disp-encoding (segment disp)
  (declare (type (or fixup (signed-byte 5)) disp)
	   (values (unsigned-byte 5)))
  (cond ((fixup-p disp)
	 (note-fixup segment :load-short disp)
	 (assert (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
	 0)
	(t
	 (dpb (ldb (byte 4 0) disp)
	      (byte 4 1)
	      (ldb (byte 1 4) disp)))))

(eval-when (compile eval)
  (defmacro define-load-short-inst (name opcode)
    `(define-instruction ,name (segment base disp reg &key modify)
       (:declare (type tn base reg)
		 (type (or fixup (signed-byte 5)) disp)
		 (type (member :before :after nil) modify))
       (:emitter
	(multiple-value-bind
	    (m a)
	    (ecase modify
	      ((nil) (values 0 0))
	      (:after (values 1 0))
	      (:before (values 1 1)))
	  (emit-extended-load/store segment #x03 (reg-tn-encoding base)
				    (short-disp-encoding segment disp)
				    0 a 4 ,opcode m
				    (reg-tn-encoding reg))))))
  (defmacro define-store-short-inst (name opcode)
    `(define-instruction ,name (segment reg base disp &key modify)
       (:declare (type tn reg base)
		 (type (or fixup (signed-byte 5)) disp)
		 (type (member :before :after nil) modify))
       (:emitter
	(multiple-value-bind
	    (m a)
	    (ecase modify
	      ((nil) (values 0 0))
	      (:after (values 1 0))
	      (:before (values 1 1)))
	  (emit-extended-load/store segment #x03 (reg-tn-encoding base)
				    (short-disp-encoding segment disp)
				    0 a 4 ,opcode m
				    (reg-tn-encoding reg)))))))

(define-load-short-inst ldws 2)
(define-load-short-inst ldhs 1)
(define-load-short-inst ldbs 0)
(define-load-short-inst ldcws 7)

(define-store-short-inst stws 10)
(define-store-short-inst sths 9)
(define-store-short-inst stbs 8)

(define-instruction stbys (segment reg base disp where &key modify)
  (:declare (type tn reg base)
	    (type (signed-byte 5) disp)
	    (type (member :begin :end) where)
	    (type (member t nil) modify))
  (:emitter
   (emit-extended-load/store segment #x03 (reg-tn-encoding base)
			     (reg-tn-encoding reg) 0
			     (ecase where (:begin 0) (:end 1))
			     4 #xC (if modify 1 0)
			     (short-disp-encoding segment disp))))


;;;; Immediate Instructions.

(define-load-inst ldo #x0D)

(define-emitter emit-ldil 32
  (byte 6 26)
  (byte 5 21)
  (byte 21 0))

(defun immed-21-encoding (segment value)
  (declare (type (or fixup (signed-byte 21) (unsigned-byte 21)) value)
	   (values (unsigned-byte 21)))
  (cond ((fixup-p value)
	 (note-fixup segment :hi value)
	 (assert (or (null (fixup-offset value)) (zerop (fixup-offset value))))
	 0)
	(t
	 (logior (ash (ldb (byte 5 2) value) 16)
		 (ash (ldb (byte 2 7) value) 14)
		 (ash (ldb (byte 2 0) value) 12)
		 (ash (ldb (byte 11 9) value) 1)
		 (ldb (byte 1 20) value)))))

(define-instruction ldil (segment value reg)
  (:declare (type tn reg)
	    (type (or (signed-byte 21) (unsigned-byte 21) fixup) value))
  (:emitter
   (emit-ldil segment #x08 (reg-tn-encoding reg)
	      (immed-21-encoding segment value))))

(define-instruction addil (segment value reg)
  (:declare (type tn reg)
	    (type (or (signed-byte 21) (unsigned-byte 21) fixup) value))
  (:emitter
   (emit-ldil segment #x0A (reg-tn-encoding reg)
	      (immed-21-encoding segment value))))


;;;; Branch instructions.

(define-emitter emit-branch 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 11 2) (byte 1 1) (byte 1 0))

(defun label-relative-displacement (label posn &optional delta-if-after)
  (declare (type label label) (type index posn)
	   (values integer))
  (ash (- (if delta-if-after
	      (label-position label posn delta-if-after)
	      (label-position label))
	  (+ posn 8)) -2))

(defun decompose-branch-disp (segment disp)
  (declare (type (or fixup (signed-byte 17)) disp))
  (cond ((fixup-p disp)
	 (note-fixup segment :branch disp)
	 (assert (or (null (fixup-offset disp)) (zerop (fixup-offset disp))))
	 (values 0 0 0))
	(t
	 (values (ldb (byte 5 11) disp)
		 (dpb (ldb (byte 10 0) disp)
		      (byte 10 1)
		      (ldb (byte 1 10) disp))
		 (ldb (byte 1 16) disp)))))

(defun emit-relative-branch (segment opcode link sub-opcode target nullify)
  (declare (type (unsigned-byte 6) opcode)
	   (type (unsigned-byte 5) link)
	   (type (unsigned-byte 1) sub-opcode)
	   (type label target)
	   (type (member t nil) nullify))
  (emit-back-patch segment 4
    #'(lambda (segment posn)
	(let ((disp (label-relative-displacement target posn)))
	  (assert (<= (- (ash 1 16)) disp (1- (ash 1 16))))
	  (multiple-value-bind
	      (w1 w2 w)
	      (decompose-branch-disp segment disp)
	    (emit-branch segment opcode link w1 sub-opcode w2
			 (if nullify 1 0) w))))))

(define-instruction b (segment target &key nullify)
  (:declare (type label target) (type (member t nil) nullify))
  (:emitter
   (emit-relative-branch segment #x3A 0 0 target nullify)))

(define-instruction bl (segment target reg &key nullify)
  (:declare (type tn reg) (type label target) (type (member t nil) nullify))
  (:emitter
   (emit-relative-branch segment #x3A (reg-tn-encoding reg) 0 target nullify)))

(define-instruction gateway (segment target reg &key nullify)
  (:declare (type tn reg) (type label target) (type (member t nil) nullify))
  (:emitter
   (emit-relative-branch segment #x3A (reg-tn-encoding reg) 1 target nullify)))

;;; BLR is useless because we have no way to generate the offset.

(define-instruction bv (segment base &key nullify offset)
  (:declare (type tn base)
	    (type (member t nil) nullify)
	    (type (or tn null) offset))
  (:emitter
   (emit-branch segment #x3A (reg-tn-encoding base)
		(if offset (reg-tn-encoding offset) 0)
		6 0 (if nullify 1 0) 0)))

(define-instruction be (segment disp space base &key nullify)
  (:declare (type (or fixup (signed-byte 17)) disp)
	    (type tn base)
	    (type (unsigned-byte 3) space)
	    (type (member t nil) nullify))
  (:emitter
   (multiple-value-bind
       (w1 w2 w)
       (decompose-branch-disp segment disp)
     (emit-branch segment #x38 (reg-tn-encoding base) w1
		  (space-encoding space) w2 (if nullify 1 0) w))))

(define-instruction ble (segment disp space base &key nullify)
  (:declare (type (or fixup (signed-byte 17)) disp)
	    (type tn base)
	    (type (unsigned-byte 3) space)
	    (type (member t nil) nullify))
  (:emitter
   (multiple-value-bind
       (w1 w2 w)
       (decompose-branch-disp segment disp)
     (emit-branch segment #x39 (reg-tn-encoding base) w1
		  (space-encoding space) w2 (if nullify 1 0) w))))

(defun emit-conditional-branch (segment opcode r2 r1 cond target nullify)
  (emit-back-patch segment 4
    #'(lambda (segment posn)
	(let ((disp (label-relative-displacement target posn)))
	  (assert (<= (- (ash 1 11)) disp (1- (ash 1 11))))
	  (let ((w1 (logior (ash (ldb (byte 10 0) disp) 1)
			    (ldb (byte 1 10) disp)))
		(w (ldb (byte 1 11) disp)))
	    (emit-branch segment opcode r2 r1 cond w1 (if nullify 1 0) w))))))

(defun im5-encoding (value)
  (declare (type (signed-byte 5) value)
	   (values (unsigned-byte 5)))
  (dpb (ldb (byte 4 0) value)
       (byte 4 1)
       (ldb (byte 1 4) value)))

(eval-when (compile eval)
  (defmacro define-branch-inst (r-name r-opcode i-name i-opcode cond-kind)
    (let ((conditional (symbolicate cond-kind "-CONDITION")))
      `(progn
	 (define-instruction ,r-name (segment cond r1 r2 target &key nullify)
	   (:declare (type ,conditional cond)
		     (type tn r1 r2)
		     (type label target)
		     (type (member t nil) nullify))
	   (:emitter
	    (multiple-value-bind
		(cond-encoding false)
		(,conditional cond)
	      (emit-conditional-branch
	       segment (if false ,(+ r-opcode 2) ,r-opcode)
	       (reg-tn-encoding r2) (reg-tn-encoding r1)
	       cond-encoding target nullify))))
	 (define-instruction ,i-name (segment cond imm reg target &key nullify)
	   (:declare (type ,conditional cond)
		     (type (signed-byte 5) imm)
		     (type tn reg)
		     (type (member t nil) nullify))
	   (:emitter
	    (multiple-value-bind
		(cond-encoding false)
		(,conditional cond)
	      (emit-conditional-branch
	       segment (if false (+ ,i-opcode 2) ,i-opcode)
	       (reg-tn-encoding reg) (im5-encoding imm)
	       cond-encoding target nullify))))))))

(define-branch-inst movb #x32 movib #x33 extract/deposit)
(define-branch-inst comb #x20 comib #x21 compare)
(define-branch-inst addb #x28 addib #x29 add)

(define-instruction bb (segment cond reg posn target &key nullify)
  (:declare (type (member t nil) cond nullify)
	    (type tn reg)
	    (type (or (member :variable) (unsigned-byte 5)) posn))
  (:emitter
   (multiple-value-bind
       (opcode posn-encoding)
       (if (eq posn :variable)
	   (values  0)
	   (values #x31 posn))
     (emit-conditional-branch segment opcode posn-encoding
			      (reg-tn-encoding reg)
			      (if cond 2 6) target nullify))))


;;;; Computation Instructions

(define-emitter emit-r3-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 1 12) (byte 7 5) (byte 5 0))

(eval-when (compile eval)
  (defmacro define-r3-inst (name cond-kind opcode)
    `(define-instruction ,name (segment r1 r2 res &optional cond)
       (:declare (type tn res r1 r2))
       (:emitter
	(multiple-value-bind
	    (cond false)
	    (,(symbolicate cond-kind "-CONDITION") cond)
	  (emit-r3-inst segment #x02 (reg-tn-encoding r2) (reg-tn-encoding r1)
			cond (if false 1 0) ,opcode
			(reg-tn-encoding res)))))))
  
(define-r3-inst add add #x30)
(define-r3-inst addl add #x50)
(define-r3-inst addo add #x70)
(define-r3-inst addc add #x38)
(define-r3-inst addco add #x78)
(define-r3-inst sh1add add #x32)
(define-r3-inst sh1addl add #x52)
(define-r3-inst sh1addo add #x72)
(define-r3-inst sh2add add #x34)
(define-r3-inst sh2addl add #x54)
(define-r3-inst sh2addo add #x74)
(define-r3-inst sh3add add #x36)
(define-r3-inst sh3addl add #x56)
(define-r3-inst sh3addo add #x76)
(define-r3-inst sub compare #x20)
(define-r3-inst subo compare #x60)
(define-r3-inst subb compare #x28)
(define-r3-inst subbo compare #x68)
(define-r3-inst subt compare #x26)
(define-r3-inst subto compare #x66)
(define-r3-inst ds compare #x22)
(define-r3-inst comclr compare #x44)
(define-r3-inst or logical #x12)
(define-r3-inst xor logical #x14)
(define-r3-inst and logical #x10)
(define-r3-inst andcm logical #x00)
(define-r3-inst uxor unit #x1C)
(define-r3-inst uaddcm unit #x4C)
(define-r3-inst uaddcmt unit #x4E)
(define-r3-inst dcor unit #x5C)
(define-r3-inst idcor unit #x5E)

(define-emitter emit-imm-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 1 12) (byte 1 11) (byte 11 0))

(defun im11-encoding (value)
  (declare (type (signed-byte 11) value)
	   (values (unsigned-byte 11)))
  (dpb (ldb (byte 10 0) value)
       (byte 10 1)
       (ldb (byte 1 10) value)))

(eval-when (compile eval)
  (defmacro define-imm-inst (name cond-kind opcode subcode)
    `(define-instruction ,name (segment imm src dst &optional cond)
       (:declare (type tn dst src)
		 (type (signed-byte 11) imm))
       (:emitter
	(multiple-value-bind
	    (cond false)
	    (,(symbolicate cond-kind "-CONDITION") cond)
	  (emit-imm-inst segment ,opcode (reg-tn-encoding src)
			 (reg-tn-encoding dst) cond
			 (if false 1 0) ,subcode
			 (im11-encoding imm)))))))

(define-imm-inst addi add #x2D 0)
(define-imm-inst addio add #x2D 1)
(define-imm-inst addit add #x2C 0)
(define-imm-inst addito add #x2C 1)
(define-imm-inst subi compare #x25 0)
(define-imm-inst subio compare #x25 1)
(define-imm-inst comiclr compare #x24 0)

(define-emitter emit-extract/deposit-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13)
  (byte 3 10) (byte 5 5) (byte 5 0))

(define-instruction shd (segment r1 r2 count res &optional cond)
  (:declare (type tn res r1 r2)
	    (type (or (member :variable) (integer 0 31)) count))
  (:emitter
   (etypecase count
     ((member :variable)
      (emit-extract/deposit-inst segment #x34
				 (reg-tn-encoding r2) (reg-tn-encoding r1)
				 (extract/deposit-condition cond)
				 0 0 (reg-tn-encoding res)))
     ((integer 0 31)
      (emit-extract/deposit-inst segment #x34
				 (reg-tn-encoding r2) (reg-tn-encoding r1)
				 (extract/deposit-condition cond)
				 2 (- 31 count)
				 (reg-tn-encoding res))))))

(eval-when (compile eval)
  (defmacro define-extract-inst (name opcode)
    `(define-instruction ,name (segment src posn len res &optional cond)
       (:declare (type tn res src)
		 (type (or (member :variable) (integer 0 31)) posn)
		 (type (integer 1 32) len))
       (:emitter
	(etypecase posn
	  ((member :variable)
	   (emit-extract/deposit-inst segment #x34 (reg-tn-encoding src)
				      (reg-tn-encoding res)
				      (extract/deposit-condition cond)
				      ,(- opcode 2) 0 (- 32 len)))
	  ((integer 0 31)
	   (emit-extract/deposit-inst segment #x34 (reg-tn-encoding src)
				      (reg-tn-encoding res)
				      (extract/deposit-condition cond)
				      ,opcode posn (- 32 len))))))))

(define-extract-inst extru 6)
(define-extract-inst extrs 7)

(eval-when (compile eval)
  (defmacro define-deposit-inst (name opcode)
    `(define-instruction ,name (segment src posn len res &optional cond)
       (:declare (type tn res)
		 (type (or tn (signed-byte 5)) src)
		 (type (or (member :variable) (integer 0 31)) posn)
		 (type (integer 1 32) len))
       (:emitter
	(multiple-value-bind
	    (opcode src-encoding)
	    (etypecase src
	      (tn
	       (values ,opcode (reg-tn-encoding src)))
	      ((signed-byte 5)
	       (values ,(+ opcode 4) (im5-encoding src))))
	  (multiple-value-bind
	      (opcode posn-encoding)
	      (etypecase posn
		((member :variable)
		 (values opcode 0))
		((integer 0 31)
		 (values (+ opcode 2) (- 31 posn))))
	    (emit-extract/deposit-inst segment #x35 (reg-tn-encoding res)
				       src-encoding
				       (extract/deposit-condition cond)
				       opcode posn-encoding (- 32 len))))))))

(define-deposit-inst dep 1)
(define-deposit-inst zdep 0)



;;;; System Control Instructions.

(define-emitter emit-break 32
  (byte 6 26) (byte 13 13) (byte 8 5) (byte 5 0))

(define-instruction break (segment &optional (im5 0) (im13 0))
  (:declare (type (unsigned-byte 13) im13)
	    (type (unsigned-byte 5) im5))
  (:emitter
   (emit-break segment 0 im13 0 im5)))

(define-emitter emit-system-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13) (byte 8 5) (byte 5 0))

(define-instruction ldsid (segment res base &optional (space 0))
  (:declare (type tn res base)
	    (type (integer 0 3) space))
  (:emitter
   (emit-system-inst segment 0 (reg-tn-encoding base) 0 (ash space 1) #x85
		     (reg-tn-encoding res))))

(define-instruction mtsp (segment reg space)
  (:declare (type tn reg) (type (integer 0 7) space))
  (:emitter
   (emit-system-inst segment 0 0 (reg-tn-encoding reg) (space-encoding space)
		     #xC1 0)))

(define-instruction mfsp (segment space reg)
  (:declare (type tn reg) (type (integer 0 7) space))
  (:emitter
   (emit-system-inst segment 0 0 0 (space-encoding space) #x25
		     (reg-tn-encoding reg))))

(deftype control-reg ()
  '(or (unsigned-byte 5) (member :sar)))

(defun control-reg (reg)
  (declare (type control-reg reg)
	   (values (unsigned-byte 32)))
  (if (typep reg '(unsigned-byte 5))
      reg
      (ecase reg
	(:sar 11))))

(define-instruction mtctl (segment reg ctrl-reg)
  (:declare (type tn reg) (type control-reg ctrl-reg))
  (:emitter
   (emit-system-inst segment 0 (control-reg ctrl-reg) (reg-tn-encoding reg)
		     0 #xC2 0)))

(define-instruction mfctl (segment ctrl-reg reg)
  (:declare (type tn reg) (type control-reg ctrl-reg))
  (:emitter
   (emit-system-inst segment 0 (control-reg ctrl-reg) 0 0 #x45
		     (reg-tn-encoding reg))))



;;;; Floating point instructions.

(define-emitter emit-fp-load/store 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 2 14) (byte 1 13) (byte 1 12)
  (byte 2 10) (byte 1 9) (byte 3 6) (byte 1 5) (byte 5 0))

(define-instruction fldx (segment index base result &key modify scale side)
  (:declare (type tn index base result)
	    (type (member t nil) modify scale)
	    (type (member nil 0 1) side))
  (:emitter
   (multiple-value-bind
       (result-encoding double-p)
       (fp-reg-tn-encoding result)
     (when side
       (assert double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
			 (reg-tn-encoding index) 0 (if scale 1 0) 0 0 0
			 (or side 0) (if modify 1 0) result-encoding))))

(define-instruction fstx (segment value index base &key modify scale side)
  (:declare (type tn index base value)
	    (type (member t nil) modify scale)
	    (type (member nil 0 1) side))
  (:emitter
   (multiple-value-bind
       (value-encoding double-p)
       (fp-reg-tn-encoding value)
     (when side
       (assert double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
			 (reg-tn-encoding index) 0 (if scale 1 0) 0 0 1
			 (or side 0) (if modify 1 0) value-encoding))))
  
(define-instruction flds (segment disp base result &key modify side)
  (:declare (type tn base result)
	    (type (signed-byte 5) disp)
	    (type (member :before :after nil) modify)
	    (type (member nil 0 1) side))
  (:emitter
   (multiple-value-bind
       (result-encoding double-p)
       (fp-reg-tn-encoding result)
     (when side
       (assert double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
			 (short-disp-encoding segment disp) 0
			 (if (eq modify :before) 1 0) 1 0 0
			 (or side 0) (if modify 1 0) result-encoding))))

(define-instruction fsts (segment value disp base &key modify side)
  (:declare (type tn base value)
	    (type (signed-byte 5) disp)
	    (type (member :before :after nil) modify)
	    (type (member nil 0 1) side))
  (:emitter
   (multiple-value-bind
       (value-encoding double-p)
       (fp-reg-tn-encoding value)
     (when side
       (assert double-p)
       (setf double-p nil))
     (emit-fp-load/store segment (if double-p #x0B #x09) (reg-tn-encoding base)
			 (short-disp-encoding segment disp) 0
			 (if (eq modify :before) 1 0) 1 0 1
			 (or side 0) (if modify 1 0) value-encoding))))


(define-emitter emit-fp-class-0-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 3 13) (byte 2 11) (byte 2 9)
  (byte 3 6) (byte 1 5) (byte 5 0))

(define-emitter emit-fp-class-1-inst 32
  (byte 6 26) (byte 5 21) (byte 4 17) (byte 2 15) (byte 2 13) (byte 2 11)
  (byte 2 9) (byte 3 6) (byte 1 5) (byte 5 0))

;;; Note: classes 2 and 3 are similar enough to class 0 that we don't need
;;; seperate emitters.

(defconstant funops '(:copy :abs :sqrt :rnd))

(deftype funop ()
  `(member ,@funops))

(define-instruction funop (segment op from to)
  (:declare (type funop op)
	    (type tn from to))
  (:emitter
   (multiple-value-bind
       (from-encoding from-double-p)
       (fp-reg-tn-encoding from)
     (multiple-value-bind
	 (to-encoding to-double-p)
	 (fp-reg-tn-encoding to)
       (assert (eq from-double-p to-double-p))
       (emit-fp-class-0-inst segment #x0C from-encoding 0
			     (+ 2 (position op funops))
			     (if to-double-p 1 0) 0 0 0 to-encoding)))))

(eval-when (compile eval)
  (defmacro define-class-1-fp-inst (name subcode)
    `(define-instruction ,name (segment from to)
       (:declare (type tn from to))
       (:emitter
	(multiple-value-bind
	    (from-encoding from-double-p)
	    (fp-reg-tn-encoding from)
	  (multiple-value-bind
	      (to-encoding to-double-p)
	      (fp-reg-tn-encoding to)
	    (emit-fp-class-1-inst segment #x0C from-encoding 0 ,subcode
				  (if to-double-p 1 0) (if from-double-p 1 0)
				  1 0 0 to-encoding)))))))

(define-class-1-fp-inst fcnvff 0)
(define-class-1-fp-inst fcnvxf 1)
(define-class-1-fp-inst fcnvfx 2)
(define-class-1-fp-inst fcnvfxt 3)

(define-instruction fcmp (segment cond r1 r2)
  (:declare (type (unsigned-byte 5) cond)
	    (type tn r1 r2))
  (:emitter
   (multiple-value-bind
       (r1-encoding r1-double-p)
       (fp-reg-tn-encoding r1)
     (multiple-value-bind
	 (r2-encoding r2-double-p)
	 (fp-reg-tn-encoding r2)
       (assert (eq r1-double-p r2-double-p))
       (emit-fp-class-0-inst segment #x0C r1-encoding r2-encoding 0
			     (if r1-double-p 1 0) 2 0 0 cond)))))

(define-instruction ftest (segment)
  (:emitter
   (emit-fp-class-0-inst segment #x0C 0 0 1 0 2 0 1 0)))

(defconstant fbinops '(:add :sub :mpy :div))

(deftype fbinop ()
  `(member ,@fbinops))

(define-instruction fbinop (segment op r1 r2 result)
  (:declare (type fbinop op)
	    (type tn r1 r2 result))
  (:emitter
   (multiple-value-bind
       (r1-encoding r1-double-p)
       (fp-reg-tn-encoding r1)
     (multiple-value-bind
	 (r2-encoding r2-double-p)
	 (fp-reg-tn-encoding r2)
       (assert (eq r1-double-p r2-double-p))
       (multiple-value-bind
	   (result-encoding result-double-p)
	   (fp-reg-tn-encoding result)
	 (assert (eq r1-double-p result-double-p))
	 (emit-fp-class-0-inst segment #x0C r1-encoding r2-encoding
			       (position op fbinops)
			       (if r1-double-p 1 0) 3 0 0
			       result-encoding))))))



;;;; Instructions built out of other insts.

(define-instruction-macro move (src dst &optional cond)
  `(inst or ,src zero-tn ,dst ,cond))

(define-instruction-macro nop (&optional cond)
  `(inst or zero-tn zero-tn zero-tn ,cond))

(define-instruction li (segment value reg)
  (:declare (type tn reg)
	    (type (or fixup (signed-byte 32) (unsigned-byte 32)) value))
  (:vop-var vop)
  (:emitter
   (assemble (segment vop)
     (etypecase value
       (fixup
	(inst ldil value reg)
	(inst ldo value reg reg))
       ((signed-byte 14)
	(inst ldo value zero-tn reg))
       ((or (signed-byte 32) (unsigned-byte 32))
	(let ((hi (ldb (byte 21 11) value))
	      (lo (ldb (byte 11 0) value)))
	  (inst ldil hi reg)
	  (unless (zerop lo)
	    (inst ldo lo reg reg))))))))

(define-instruction-macro sll (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst zdep ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(define-instruction-macro sra (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst extrs ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(define-instruction-macro srl (src count result &optional cond)
  (once-only ((result result) (src src) (count count) (cond cond))
    `(inst extru ,src (- 31 ,count) (- 32 ,count) ,result ,cond)))

(defun maybe-negate-cond (cond negate)
  (if negate
      (multiple-value-bind
	  (value negate)
	  (compare-condition cond)
	(if negate
	    (nth value compare-conditions)
	    (nth (+ value 8) compare-conditions)))
      cond))

(define-instruction bc (segment cond not-p r1 r2 target)
  (:declare (type compare-condition cond)
	    (type (member t nil) not-p)
	    (type tn r1 r2)
	    (type label target))
  (:vop-var vop)
  (:emitter
   (emit-chooser segment 8 2
     #'(lambda (segment posn delta)
	 (let ((disp (label-relative-displacement target posn delta)))
	   (when (<= 0 disp (1- (ash 1 11)))
	     (assemble (segment vop)
	       (inst comb (maybe-negate-cond cond not-p) r1 r2 target
		     :nullify t))
	     t)))
     #'(lambda (segment posn)
	 (let ((disp (label-relative-displacement target posn)))
	   (assemble (segment vop)
	     (cond ((<= (- (ash 1 11)) disp (1- (ash 1 11)))
		    (inst comb (maybe-negate-cond cond not-p) r1 r2 target)
		    (inst nop))
		   (t
		    (inst comclr r1 r2 zero-tn
			  (maybe-negate-cond cond (not not-p)))
		    (inst b target :nullify t)))))))))

(define-instruction bci (segment cond not-p imm reg target)
  (:declare (type compare-condition cond)
	    (type (member t nil) not-p)
	    (type (signed-byte 11) imm)
	    (type tn reg)
	    (type label target))
  (:vop-var vop)
  (:emitter
   (emit-chooser segment 8 2
     #'(lambda (segment posn delta-if-after)
	 (let ((disp (label-relative-displacement target posn delta-if-after)))
	   (when (and (<= 0 disp (1- (ash 1 11)))
		      (<= (- (ash 1 4)) imm (1- (ash 1 4))))
	     (assemble (segment vop)
	       (inst comib (maybe-negate-cond cond not-p) imm reg target
		     :nullify t))
	     t)))
     #'(lambda (segment posn)
	 (let ((disp (label-relative-displacement target posn)))
	   (assemble (segment vop)
	     (cond ((and (<= (- (ash 1 11)) disp (1- (ash 1 11)))
			 (<= (- (ash 1 4)) imm (1- (ash 1 4))))
		    (inst comib (maybe-negate-cond cond not-p) imm reg target)
		    (inst nop))
		   (t
		    (inst comiclr imm reg zero-tn
			  (maybe-negate-cond cond (not not-p)))
		    (inst b target :nullify t)))))))))


;;;; Instructions to convert between code ptrs, functions, and lras.

(defun emit-compute-inst (segment vop src label temp dst calc)
  (emit-chooser
      ;; We emit either 12 or 4 bytes, so we maintain 3 byte alignments.
      segment 12 3
    #'(lambda (segment posn delta-if-after)
	(let ((delta (funcall calc label posn delta-if-after)))
	  (when (<= (- (ash 1 10)) delta (1- (ash 1 10)))
	    (emit-back-patch segment 4
			     #'(lambda (segment posn)
				 (assemble (segment vop)
				   (inst addi (funcall calc label posn 0) src
					 dst))))
	    t)))
    #'(lambda (segment posn)
	(let ((delta (funcall calc label posn 0)))
	  ;; Note: if we used addil/ldo to do this in 2 instructions then the
	  ;; intermediate value would be tagged but pointing into space.
	  (assemble (segment vop)
	    (inst ldil (ldb (byte 21 11) delta) temp)
	    (inst ldo (ldb (byte 11 0) delta) temp temp)
	    (inst add src temp dst))))))

;; code = fn - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment src label temp dst)
  (:declare (type tn src dst temp)
	    (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop src label temp dst
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     (label-position label posn delta-if-after)
			     (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lra (segment src label temp dst)
  (:declare (type tn src dst temp)
	    (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop src label temp dst
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment src label temp dst)
  (:declare (type tn src dst temp)
	    (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop src label temp dst
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))


;;;; Data instructions.

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-emitter emit-halfword 16
  (byte 16 0))

(define-instruction halfword (segment halfword)
  (:emitter
   (emit-halfword segment halfword)))

(define-emitter emit-word 32
  (byte 32 0))

(define-instruction word (segment word)
  (:emitter
   (emit-word segment word)))

(define-instruction function-header-word (segment)
  (:emitter
   (emit-back-patch
    segment 4
    #'(lambda (segment posn)
	(emit-word segment
		   (logior function-header-type
			   (ash (+ posn (component-header-length))
				(- type-bits word-shift))))))))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-back-patch
    segment 4
    #'(lambda (segment posn)
	(emit-word segment
		   (logior return-pc-header-type
			   (ash (+ posn (component-header-length))
				(- type-bits word-shift))))))))
