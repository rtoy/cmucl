;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.37 1992/02/24 00:43:55 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/insts.lisp,v 1.37 1992/02/24 00:43:55 wlott Exp $
;;;
;;; Description of the MIPS architecture.
;;;
;;; Written by William Lott
;;;

#|
(eval-when (compile load eval)
  (unless (find-package "OLD-MIPS")
    (rename-package (find-package "MIPS") "OLD-MIPS" '("VM"))))
|#

(in-package "MIPS")
(use-package "ASSEM")
(use-package "EXT")

(disassem:set-disassem-params
 :instruction-alignment 32
 :storage-class-sets '((register any-reg descriptor-reg base-char-reg
				 sap-reg signed-reg unsigned-reg
				 non-descriptor-reg interior-reg)
		       (float-reg single-reg double-reg)
		       (control-stack control-stack)
		       (number-stack signed-stack unsigned-stack
				     base-char-stack sap-stack
				     single-stack double-stack))
 )


;;;; Resources.

(define-resources high low memory float-status)


;;;; Special argument types and fixups.

(defun register-p (object)
  (and (tn-p object)
       (let* ((sc (tn-sc object))
	      (sc-name (sc-name sc))
	      (sb (sc-sb sc))
	      (sb-name (sb-name sb)))
	 (or (eq sc-name 'zero)
	     (eq sc-name 'null)
	     (eq sb-name 'registers)))))

(defun tn-register-number (tn)
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t (tn-offset tn))))

(defconstant reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol (concatenate 'string "$" name)))))
       *register-names*))

(define-argument-type register
  :type '(satisfies register-p)
  :function tn-register-number
  :disassem-printer #'(lambda (value stream dstate)
			(declare (stream stream) (fixnum value))
			(let ((regname (aref reg-symbols value)))
			  (princ regname stream)
			  (disassem:maybe-note-associated-storage-ref
			   value
			   'register
			   regname
			   dstate)))
  )

(defun fp-reg-p (object)
  (and (tn-p object)
       (eq (sb-name (sc-sb (tn-sc object)))
	   'float-registers)))

(defconstant float-reg-symbols
  (coerce 
   (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
   'vector))

(define-argument-type fp-reg
  :type '(satisfies fp-reg-p)
  :function tn-offset
  :disassem-printer #'(lambda (value stream dstate)
			(declare (stream stream) (fixnum value))
			(let ((regname (aref float-reg-symbols value)))
			  (princ regname stream)
			  (disassem:maybe-note-associated-storage-ref
			   value
			   'float-reg
			   regname
			   dstate)))
  )

(define-argument-type odd-fp-reg
  :type '(satisfies fp-reg-p)
  :function (lambda (tn)
	      (1+ (tn-offset tn))))

(define-argument-type control-register
  :type '(unsigned-byte 5)
  :function identity
  :disassem-printer "{CR:#x~X}")

(defun label-offset (label)
  (1- (ash (- (label-position label) *current-position*) -2)))

(define-argument-type relative-label
  :type 'label
  :function label-offset
  :sign-extend t
  :disassem-use-label #'(lambda (value dstate)
			  (declare (type disassem:disassem-state dstate))
			  (+ (ash (1+ value) 2)
			     (disassem:dstate-curpos dstate))))

(defun float-format-value (format)
  (ecase format
    ((:s :single) 0)
    ((:d :double) 1)
    ((:w :word) 4)))

(define-argument-type float-format
  :type '(member :s :single :d :double :w :word)
  :function float-format-value
  :disassem-printer #'(lambda (value stream dstate)
			(declare (ignore dstate)
				 (stream stream)
				 (fixnum value))
			(princ (case value
				 (0 's)
				 (1 'd)
				 (4 'w)
				 (t '?))
			       stream)))
			

(defconstant compare-kinds
  '(:f :un :eq :ueq :olt :ult :ole :ule :sf :ngle :seq :ngl :lt :nge :le :ngt))
(defconstant compare-kinds-vec
  (apply #'vector compare-kinds))

(defun compare-kind (kind)
  (or (position kind compare-kinds)
      (error "Unknown floating point compare kind: ~S~%Must be one of: ~S"
	     kind
	     compare-kinds)))

(define-argument-type compare-kind
  :type `(member ,@compare-kinds)
  :function compare-kind
  :disassem-printer compare-kinds-vec)


(defconstant float-operations '(+ - * /))
(defconstant float-operation-names
  ;; this gets used for output only
  #(add sub mul div))

(defun float-operation (op)
  (or (position op float-operations)
      (error "Unknown floating point operation: ~S~%Must be one of: ~S"
	     op
	     float-operations)))

(define-argument-type float-operation
  :type `(member ,@float-operations)
  :function float-operation
  :disassem-printer float-operation-names)

(define-fixup-type :jump
  :disassem-printer #'(lambda (value stream dstate)
			(let ((addr (ash value 2)))
			  (disassem:maybe-note-assembler-routine addr dstate)
			  (write addr :base 16 :radix t :stream stream))))
(define-fixup-type :lui :disassem-printer "#x~4,'0X")
(define-fixup-type :addi)



;;;; Formats.

(defconstant special-op #b000000)
(defconstant bcond-op #b0000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)

(defconstant immed-printer
  '(:name :tab rt (:unless (:same-as rt) ", " rs) ", " immediate))

;;; for things that use rt=0 as a nop
(defconstant immed-zero-printer
  '(:name :tab rt (:unless (:constant 0) ", " rs) ", " immediate))


(define-format (immediate 32
		:disassem-printer immed-printer)
  (op (byte 6 26))
  (rs (byte 5 21) :read t :default-type register)
  (rt (byte 5 16) :write t :default-type register)
  (immediate (byte 16 0) :default-type (signed-byte 16)))

(define-format (jump 32
		:disassem-printer '(:name :tab target))
  (op (byte 6 26))
  (target (byte 26 0)))

(defconstant reg-printer
  '(:name :tab rd (:unless (:same-as rd) ", " rs) ", " rt))

(define-format (register 32 :disassem-printer reg-printer)
  (op (byte 6 26))
  (rs (byte 5 21) :read t)
  (rt (byte 5 16) :read t)
  (rd (byte 5 11) :write t)
  (shamt (byte 5 6) :default 0)
  (funct (byte 6 0)))


(define-format (break 32
		:disassem-printer
		  '(:name :tab code (:unless (:constant 0) subcode)))
  (op (byte 6 26) :default special-op)
  (code (byte 10 16))
  (subcode (byte 10 6) :default 0)
  (funct (byte 6 0) :default #b001101))


(define-format (coproc-branch 32
		:use (float-status)
		:disassem-printer '(:name :tab offset))
  (op (byte 6 26))
  (funct (byte 10 16))
  (offset (byte 16 0)))

(defconstant float-fmt-printer
  '((:unless :constant funct)
    (:choose (:unless :constant sub-funct) nil)
    "." format))

(defconstant float-printer
  `(:name ,@float-fmt-printer
	  :tab
	  fd
	  (:unless (:same-as fd) ", " fs)
	  ", " ft))

(define-format (float 32 :use (float-status) :clobber (float-status)
		:disassem-printer float-printer)
  (op (byte 6 26) :default #b010001)
  (filler (byte 1 25) :default #b1)
  (format (byte 4 21))
  (ft (byte 5 16) :read t)
  (fs (byte 5 11) :read t)
  (fd (byte 5 6) :write t)
  (funct (byte 6 0)))

(define-format (float-aux 32 :use (float-status) :clobber (float-status)
			  :disassem-printer float-printer)
  (op (byte 6 26) :default #b010001)
  (filler-1 (byte 1 25) :default #b1)
  (format (byte 4 21))
  (ft (byte 5 16) :read t :default 0)
  (fs (byte 5 11) :read t)
  (fd (byte 5 6) :write t)
  (funct (byte 2 4))
  (sub-funct (byte 4 0)))



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

(defstruct lui-note
  target-reg
  high-bits
  following-addr)

(defun look-at-lui-note (chunk inst stream dstate)
  (when stream
    (let ((lui-note (disassem:dstate-get-prop dstate 'lui-note)))
      (when (and lui-note
		 (= (disassem:dstate-curpos dstate)
		    (lui-note-following-addr lui-note))
		 (= (disassem:arg-value 'rt chunk inst)
		    (lui-note-target-reg lui-note)))
	(let ((value
	       (+ (lui-note-high-bits lui-note)
		  (disassem:arg-value 'immediate
				      chunk inst))))
	(or (disassem:maybe-note-assembler-routine value dstate)
	    (disassem:note #'(lambda (stream)
			       (format stream "#x~x (~d)"
				       value
				       (disassem:sign-extend value 32)))
			   dstate))))))) 

(disassem:specialize (or :disassem-control #'look-at-lui-note)
  immediate)
(disassem:specialize (add :disassem-control #'look-at-lui-note)
  immediate)

;;; note: this must be after the above, because the disassem-controls
;;; are exclusive
(disassem:specialize (add
		      :disassem-control
		        #'(lambda (chunk inst stream dstate)
			    (when stream
			      (disassem:maybe-note-nil-indexed-object
			       (disassem:arg-value 'immediate chunk inst)
			       dstate))))
  immediate
  (rs :constant null-offset))


(define-instruction (beq :pinned t
			 :attributes (relative-branch delayed-branch))
  (immediate (op :constant #b000100)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label))
  (immediate (op :constant #b000100)
	     (rs :argument register)
	     (rt :argument register :read t :write nil)
	     (immediate :argument relative-label)))

(define-instruction (bne :pinned t
			 :attributes (relative-branch delayed-branch))
  (immediate (op :constant #b000101)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label))
  (immediate (op :constant #b000101)
	     (rs :argument register)
	     (rt :argument register :read t :write nil)
	     (immediate :argument relative-label)))

(defconstant cond-branch-printer
  '(:name :tab rs ", " immediate))

(define-instruction (blez :pinned t
			  :attributes (relative-branch delayed-branch)
			  :disassem-printer cond-branch-printer)
  (immediate (op :constant #b000110)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (bgtz :pinned t
			  :attributes (relative-branch delayed-branch)
			  :disassem-printer cond-branch-printer)
  (immediate (op :constant #b000111)
	     (rs :argument register)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (bltz :pinned t
			  :attributes (relative-branch delayed-branch)
			  :disassem-printer cond-branch-printer)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b00000)
	     (immediate :argument relative-label)))

(define-instruction (bgez :pinned t
			  :attributes (relative-branch delayed-branch)
			  :disassem-printer cond-branch-printer)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b00001)
	     (immediate :argument relative-label)))

(define-instruction (bltzal :pinned t
			    :attributes (relative-branch delayed-branch)
			    :disassem-printer cond-branch-printer)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b01000)
	     (immediate :argument relative-label)))

(define-instruction (bgezal :pinned t
			    :attributes (relative-branch delayed-branch)
			    :disassem-printer cond-branch-printer)
  (immediate (op :constant bcond-op)
	     (rs :argument register)
	     (rt :constant #b01001)
	     (immediate :argument relative-label)))

(define-instruction (bc1f :pinned t
			  :attributes (relative-branch delayed-branch))
  (coproc-branch (op :constant cop1-op)
		 (funct :constant #x100)
		 (offset :argument relative-label)))

(define-instruction (bc1t :pinned t
			  :attributes (relative-branch delayed-branch))
  (coproc-branch (op :constant cop1-op)
		 (funct :constant #x101)
		 (offset :argument relative-label)))

;;; ----------------------------------------------------------------

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (system:sap-ref-8 sap offset))
	 (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system:system-area-pointer sap)
	     (type (unsigned-byte 8) length)
	     (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
	   (values 0 (1+ length) nil nil))
	  (t
	   (kernel:copy-from-system-area sap (* mips:byte-bits (1+ offset))
					 vector (* mips:word-bits
						   mips:vector-data-offset)
					 (* length mips:byte-bits))
	   (collect ((sc-offsets)
		     (lengths))
	     (lengths 1)		; the length byte
	     (let* ((index 0)
		    (error-number (c::read-var-integer vector index)))
	       (lengths index)
	       (loop
		 (when (>= index length)
		   (return))
		 (let ((old-index index))
		   (sc-offsets (c::read-var-integer vector index))
		   (lengths (- index old-index))))
	       (values error-number
		       (1+ length)
		       (sc-offsets)
		       (lengths))))))))

(defmacro break-cases (breaknum &body cases)
  (let ((bn-temp (gensym)))
    (collect ((clauses))
      (dolist (case cases)
	(clauses `((= ,bn-temp ,(car case)) ,@(cdr case))))
      `(let ((,bn-temp ,breaknum))
	 (cond ,@(clauses))))))

(defun break-control (chunk inst stream dstate)
  (flet ((nt (x) (if stream (disassem:note x dstate))))
    (break-cases (disassem:arg-value 'code chunk inst)
      (vm:error-trap
       (nt "Error trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (vm:cerror-trap
       (nt "Cerror trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (vm:breakpoint-trap
       (nt "Breakpoint trap"))
      (vm:pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (vm:halt-trap
       (nt "Halt trap"))
      (vm:function-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
    )))

(define-instruction (break :pinned t :disassem-control #'break-control)
  (break (code :argument (unsigned-byte 10)))
  (break (code :argument (unsigned-byte 10))
	 (subcode :argument (unsigned-byte 10))))

;;; ----------------------------------------------------------------

(defconstant divmul-printer '(:name :tab rs ", " rt))

(define-instruction (div :clobber (low high) :disassem-printer divmul-printer)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011010)))

(define-instruction (divu :clobber (low high) :disassem-printer divmul-printer)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011011)))

(define-instruction (j :pinned t
		       :attributes (unconditional-branch delayed-branch)
		       :disassem-printer '(:name :tab (:choose rs target)))
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :constant 0)
	    (rd :constant 0)
	    (funct :constant #b001000))
  (jump (op :constant #b000010)
	(target :argument jump-fixup)))

(define-instruction (jal :pinned t
			 :attributes (delayed-branch assembly-call)
			 :disassem-printer
			 '(:name :tab
			   ;(:unless (:constant 31) rd ", ")
			   (:choose rs target)))
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

(defconstant load-store-printer 
  '(:name :tab
	  rt ", "
	  rs
	  (:unless (:constant 0) "[" immediate "]")))

(defmacro define-load/store-instruction (name read-p op
					      &optional (rt-kind 'register))
  `(define-instruction (,name ,@(if read-p
				    '(:use (memory) :attributes (delayed-load))
				    '(:clobber (memory)))
			      :disassem-printer load-store-printer)
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind ,@(unless read-p
					   '(:read t :write nil)))
		(rs :argument register)
		(immediate :argument (signed-byte 16)))
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind ,@(unless read-p
					   '(:read t :write nil)))
		(rs :argument register)
		(immediate :argument addi-fixup))
     (immediate (op :constant ,op)
		(rt :argument ,rt-kind ,@(unless read-p
					   '(:read t :write nil)))
		(rs :argument register)
		(immediate :constant 0))))

(define-load/store-instruction lb t #b100000)
(define-load/store-instruction lh t #b100001)
(define-load/store-instruction lwl t #b100010)
(define-load/store-instruction lw t #b100011)
(define-load/store-instruction lbu t #b100100)
(define-load/store-instruction lhu t #b100101)
(define-load/store-instruction lwr t #b100110)
(define-load/store-instruction lwc1 t #o61 fp-reg)
(define-load/store-instruction lwc1-odd t #o61 odd-fp-reg)
(define-load/store-instruction sb nil #b101000)
(define-load/store-instruction sh nil #b101001)
(define-load/store-instruction swl nil #b101010)
(define-load/store-instruction sw nil #b101011)
(define-load/store-instruction swr nil #b101110)
(define-load/store-instruction swc1 nil #o71 fp-reg)
(define-load/store-instruction swc1-odd nil #o71 odd-fp-reg)

;;; ----------------------------------------------------------------
;;; Disassembler annotation

(defun note-niss-ref (chunk inst stream dstate)
  (when stream
    (disassem:maybe-note-nil-indexed-symbol-slot-ref
     (disassem:arg-value 'immediate chunk inst)
     dstate)))

(defun note-control-stack-var-ref (chunk inst stream dstate)
  (when stream
    (disassem:maybe-note-single-storage-ref
     (disassem:arg-value 'immediate chunk inst)
     'control-stack
     dstate))
  )

(defun note-number-stack-var-ref (chunk inst stream dstate)
  (when stream
    (disassem:maybe-note-single-storage-ref
     (disassem:arg-value 'immediate chunk inst)
     'number-stack
     dstate))
  )

(disassem:specialize (lw
		      :disassem-control
		        #'(lambda (chunk inst stream dstate)
			    (when stream
			      (disassem:note-code-constant
			       (disassem:arg-value 'immediate chunk inst)
			       dstate))))
  immediate
  (rs :constant code-offset))

(disassem:specialize (lw :disassem-control #'note-niss-ref)
  immediate
  (rs :constant null-offset))
(disassem:specialize (lw :disassem-control #'note-control-stack-var-ref)
  immediate
  (rs :constant cfp-offset))
(disassem:specialize (lw :disassem-control #'note-number-stack-var-ref)
  immediate
  (rs :constant nfp-offset))

(disassem:specialize (sw :disassem-control #'note-niss-ref)
  immediate
  (rs :constant null-offset))
(disassem:specialize (sw :disassem-control #'note-control-stack-var-ref)
  immediate
  (rs :constant cfp-offset))
(disassem:specialize (sw :disassem-control #'note-number-stack-var-ref)
  immediate
  (rs :constant nfp-offset))

;;; floating point
(disassem:specialize (lwc1 :disassem-control #'note-number-stack-var-ref)
  immediate
  (rs :constant nfp-offset))
(disassem:specialize (swc1 :disassem-control #'note-number-stack-var-ref)
  immediate
  (rs :constant nfp-offset))


(defun lui-note (chunk inst stream dstate)
  (when stream
    (let ((lui-note (disassem:dstate-get-prop dstate 'lui-note)))
      (when (null lui-note)
	(setf lui-note (make-lui-note)
	      (disassem:dstate-get-prop dstate 'lui-note) lui-note))
      (setf (lui-note-target-reg lui-note)
	    (disassem:arg-value 'rt chunk inst))
      (setf (lui-note-high-bits lui-note)
	    (ash (disassem:arg-value 'immediate chunk inst) 10))
      (setf (lui-note-following-addr lui-note)
	    (disassem:dstate-nextpos dstate)))))

;;; ----------------------------------------------------------------

(define-instruction (lui :disassem-control #'lui-note)
  (immediate (op :constant #b001111)
	     (rs :constant 0)
	     (rt :argument register)
	     (immediate :argument (or (unsigned-byte 16) (signed-byte 16))))
  (immediate (op :constant #b001111)
	     (rs :constant 0)
	     (rt :argument register)
	     (immediate :argument lui-fixup)))


(defconstant mvsreg-printer '(:name :tab rd))

(define-instruction (mfhi :use (high) :disassem-printer mvsreg-printer)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010000)))

(define-instruction (mthi :clobber (high) :disassem-printer mvsreg-printer)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010001)))

(define-instruction (mflo :use (low) :disassem-printer mvsreg-printer)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010010)))

(define-instruction (mtlo :clobber (low) :disassem-printer mvsreg-printer)
  (register (op :constant special-op)
	    (rd :argument register)
	    (rs :constant 0)
	    (rt :constant 0)
	    (funct :constant #b010011)))


(define-instruction (mult :clobber (low high) :disassem-printer divmul-printer)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011000)))

(define-instruction (multu :clobber (low high)
			   :disassem-printer divmul-printer)
  (register (op :constant special-op)
	    (rs :argument register)
	    (rt :argument register)
	    (rd :constant 0)
	    (funct :constant #b011001)))

(defconstant shift-printer
  '(:name :tab
	  rd
	  (:unless (:same-as rd) ", " rt)
	  ", " (:cond ((rs :constant 0) shamt)
		      (t rs))))

(define-instruction (sll :disassem-printer shift-printer)
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

(define-instruction (sra :disassem-printer shift-printer)
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

(define-instruction (srl :disassem-printer shift-printer)
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

(define-instruction (syscall :pinned t :disassem-printer '(:name))
  (register (op :constant special-op)
	    (rd :constant 0)
	    (rt :constant 0)
	    (rs :constant 0)
	    (funct :constant #b001100)))



;;;; Floating point instructions.

(macrolet ((frob (name kind)
	     `(define-instruction (,name :attributes (delayed-load))
		(register (op :constant #b010001)
			  (rs :constant #b00100)
			  (rd :argument ,kind)
			  (rt :argument register)
			  (funct :constant 0)))))
  (frob mtc1 fp-reg)
  (frob mtc1-odd odd-fp-reg))

(macrolet ((frob (name kind)
	     `(define-instruction (,name :attributes (delayed-load))
		(register (op :constant #b010001)
			  (rs :constant #b00000)
			  (rt :argument register :read nil :write t)
			  (rd :argument ,kind :write nil :read t)
			  (funct :constant 0)))))
  (frob mfc1 fp-reg)
  (frob mfc1-odd odd-fp-reg))

(define-instruction (cfc1 :use (float-status) :attributes (delayed-load))
  (register (op :constant #b010001)
	    (rs :constant #b00010)
	    (rt :argument register :read nil :write t)
	    (rd :argument control-register :write nil)
	    (funct :constant 0)))

(define-instruction (ctc1 :use (float-status) :clobber (float-status)
			  :attributes (delayed-load))
  (register (op :constant #b010001)
	    (rs :constant #b00110)
	    (rt :argument register)
	    (rd :argument control-register :write nil)
	    (funct :constant 0)))

(define-instruction (float-op
		     :disassem-printer
		       '('f funct "." format
			    :tab
			    fd
			    (:unless (:same-as fd) ", " fs)
			    ", " ft))
  (float (funct :argument float-operation :mask #b11)
	 (format :argument float-format)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (ft :argument fp-reg)))


(defconstant float-unop-printer
  `(:name ,@float-fmt-printer :tab fd (:unless (:same-as fd) ", " fs)))

(define-instruction (fabs :disassem-printer float-unop-printer)
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

(define-instruction (fneg :disassem-printer float-unop-printer)
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :argument fp-reg)
	 (funct :constant #b000111))
  (float (format :argument float-format)
	 (ft :constant 0)
	 (fd :argument fp-reg)
	 (fs :same-as fd)
	 (funct :constant #b000111)))

(define-instruction (fcvt
		     :disassem-printer
		       `(:name "." sub-funct "." format
			       :tab
			       fd ", " fs))
  (float-aux (sub-funct :argument float-format)
	     (format :argument float-format)
	     (fd :argument fp-reg)
	     (fs :argument fp-reg)
	     (funct :constant #b10)))

  
(define-instruction (fcmp
		     :disassem-printer
		       `(:name "-" sub-funct "." format
			       :tab
			       fs ", " ft))
  (float-aux (sub-funct :argument compare-kind)
	     (format :argument float-format)
	     (fd :constant 0)
	     (fs :argument fp-reg)
	     (ft :argument fp-reg)
	     (funct :constant #b11)))


;;;; Pseudo-instructions

(define-instruction (move
		     :disassem-printer
		     '(:name
		       :tab
		       (:choose rd fd) ", "
		       (:choose rs fs)))
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
     (inst lui reg (ldb (byte 16 16) value))
     (let ((low (ldb (byte 16 0) value)))
       (unless (zerop low)
	 (inst or reg low))))
    (fixup
     (inst lui reg value)
     (inst addu reg value))))

(define-instruction (b :pinned t
		       :attributes (relative-branch unconditional-branch
						    delayed-branch)
		       :disassem-printer '(:name :tab immediate))
  (immediate (op :constant #b000100)
	     (rs :constant 0)
	     (rt :constant 0)
	     (immediate :argument relative-label)))

(define-instruction (nop :attributes (nop)
			 :disassem-printer '(:name))
  (register (op :constant 0)
	    (rd :constant 0)
	    (rt :constant 0)
	    (rs :constant 0)
	    (funct :constant 0)))

(define-format (word-format 32 :pinned t)
  (data (byte 32 0)))
(define-instruction (word :cost 0)
  (word-format (data :argument (or (unsigned-byte 32) (signed-byte 32)))))

(define-format (short-format 16 :pinned t)
  (data (byte 16 0)))
(define-instruction (short :cost 0)
  (short-format (data :argument (or (unsigned-byte 16) (signed-byte 16)))))

(define-format (byte-format 8 :pinned t)
  (data (byte 8 0)))
(define-instruction (byte :cost 0)
  (byte-format (data :argument (or (unsigned-byte 8) (signed-byte 8)))))



;;;; Function and LRA Headers emitters and calculation stuff.

(define-format (entry-point 0 :pinned t))
(define-instruction (entry-point)
  (entry-point))

(defun header-data (ignore)
  (declare (ignore ignore))
  (ash (+ *current-position* (component-header-length)) (- vm:word-shift)))

(define-format (header-object 32 :pinned t)
  (type (byte 8 0))
  (data (byte 24 8) :default 0 :function header-data))

(define-instruction (function-header-word)
  (header-object (type :constant vm:function-header-type)))

(define-instruction (lra-header-word)
  (header-object (type :constant vm:return-pc-header-type)))


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
		    (rt :argument register :read t)
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


;; code = fn - header - label-offset + other-pointer-tag
(define-compute-instruction compute-code-from-fn
			    (- vm:other-pointer-type
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
