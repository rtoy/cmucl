;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/sparc/insts.lisp,v 1.13 1994/10/31 04:46:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the SPARC architecture.
;;;
;;; Written by William Lott.
;;;
(in-package "SPARC")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p t
  :max-locations 68)


;;;; Constants, types, conversion functions, some disassembler stuff.

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

(disassem:set-disassem-params :instruction-alignment 32)

(defvar *disassem-use-lisp-reg-names* t)

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
	(+ (tn-offset loc) 32))
       (control-registers
	64)
       (immediate-constant
	nil)))
    (symbol
     (ecase loc
       (:memory 0)
       (:psr 65)
       (:fsr 66)
       (:y 67)))))

;;; symbols used for disassembly printing
;;;
(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol (concatenate 'string "%" name)))))
       sparc::*register-names*))

(disassem:define-argument-type reg
  :printer #'(lambda (value stream dstate)
	       (declare (stream stream) (fixnum value))
	       (let ((regname (aref reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'registers
		  regname
		  dstate))))

(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 to 31 collect (make-symbol (format nil "%F~d" n)))
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

(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 13) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash value 2) (disassem:dstate-cur-addr dstate))))

(defconstant branch-conditions
  '(:f :eq :le :lt :leu :ltu :n :vs :t :ne :gt :ge :gtu :geu :p :vc))

;;; Note that these aren't the standard names for branch-conditions, I think
;;; they're a bit more readable (e.g., "eq" instead of "e").  You could just
;;; put a vector of the normal ones here too.
(defconstant branch-cond-name-vec
  (coerce branch-conditions 'vector))

(disassem:define-argument-type branch-condition
  :printer branch-cond-name-vec)

(deftype branch-condition ()
  `(member ,@branch-conditions))

(defun branch-condition (condition)
  (or (position condition branch-conditions)
      (error "Unknown branch condition: ~S~%Must be one of: ~S"
	     condition branch-conditions)))

(defconstant branch-cond-true
  #b1000)

(defconstant branch-fp-conditions
  '(:f :ne :lg :ul :l :ug :g :u :t :eq :ue :ge :uge :le :ule :o))

(defconstant branch-fp-cond-name-vec
  (coerce branch-fp-conditions 'vector))

(disassem:define-argument-type branch-fp-condition
  :printer branch-fp-cond-name-vec)

(disassem:define-argument-type call-fixup :use-label t)

(deftype fp-branch-condition ()
  `(member ,@branch-fp-conditions))

(defun fp-branch-condition (condition)
  (or (position condition branch-fp-conditions)
      (error "Unknown fp-branch condition: ~S~%Must be one of: ~S"
	     condition branch-fp-conditions)))


;;;; dissassem:define-instruction-formats

(disassem:define-instruction-format
    (format-1 32 :default-printer '(:name :tab disp))
  (op   :field (byte 2 30) :value 1)
  (disp :field (byte 30 0)))

(disassem:define-instruction-format
    (format-2-immed 32 :default-printer '(:name :tab immed ", " rd))
  (op    :field (byte 2 30) :value 0)
  (rd    :field (byte 5 25) :type 'reg)
  (op2   :field (byte 3 22))
  (immed :field (byte 22 0)))

(defconstant branch-printer
  `(:name (:unless (:constant ,branch-cond-true) cond)
	  (:unless (a :constant 0) "," 'A)
	  :tab
	  disp))

(disassem:define-instruction-format
    (format-2-branch 32 :default-printer branch-printer)
  (op   :field (byte 2 30) :value 0)
  (a    :field (byte 1 29) :value 0)
  (cond :field (byte 4 25) :type 'branch-condition)
  (op2  :field (byte 3 22))
  (disp :field (byte 22 0) :type 'relative-label))

(disassem:define-instruction-format
    (format-2-unimp 32 :default-printer '(:name :tab data))
  (op     :field (byte 2 30) :value 0)
  (ignore :field (byte 5 25) :value 0)
  (op2    :field (byte 3 22) :value 0)
  (data   :field (byte 22 0)))

(defconstant f3-printer
  '(:name :tab
	  (:unless (:same-as rd) rs1 ", ")
	  (:choose rs2 immed) ", "
	  rd))

(disassem:define-instruction-format
    (format-3-reg 32 :default-printer f3-printer)
  (op  :field (byte 2 30))
  (rd  :field (byte 5 25) :type 'reg)
  (op3 :field (byte 6 19))
  (rs1 :field (byte 5 14) :type 'reg)
  (i   :field (byte 1 13) :value 0)
  (asi :field (byte 8 5)  :value 0)
  (rs2 :field (byte 5 0)  :type 'reg))

(disassem:define-instruction-format
    (format-3-immed 32 :default-printer f3-printer)
  (op    :field (byte 2 30))
  (rd    :field (byte 5 25) :type 'reg)
  (op3   :field (byte 6 19))
  (rs1   :field (byte 5 14) :type 'reg)
  (i     :field (byte 1 13) :value 1)
  (immed :field (byte 13 0) :sign-extend t))	; usually sign extended

(disassem:define-instruction-format
    (format-3-fpop 32
     :default-printer
        '(:name :tab (:unless (:same-as rd) rs1 ", ") rs2 ", " rd))
  (op	:field (byte 2 30))
  (rd 	:field (byte 5 25) :type 'fp-reg)
  (op3  :field (byte 6 19))
  (rs1  :field (byte 5 14) :type 'fp-reg)
  (opf  :field (byte 9 5))
  (rs2  :field (byte 5 0) :type 'fp-reg))



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



;;;; Most of the format-3-instructions.

(defun emit-format-3-inst (segment op op3 dst src1 src2
				   &key load-store fixup dest-kind odd)
  (unless src2
    (cond ((and (typep src1 'tn) load-store)
	   (setf src2 0))
	  (t
	   (setf src2 src1)
	   (setf src1 dst))))
  (etypecase src2
    (tn
     (emit-format-3-reg segment op
			(if dest-kind
			    (fp-reg-tn-encoding dst odd)
			    (reg-tn-encoding dst))
			op3 (reg-tn-encoding src1) 0 0 (reg-tn-encoding src2)))
    (integer
     (emit-format-3-immed segment op
			  (if dest-kind
			      (fp-reg-tn-encoding dst odd)
			      (reg-tn-encoding dst))
			  op3 (reg-tn-encoding src1) 1 src2))
    (fixup
     (unless (or load-store fixup)
       (error "Fixups aren't allowed."))
     (note-fixup segment :add src2)
     (emit-format-3-immed segment op
			  (if dest-kind
			      (fp-reg-tn-encoding dst odd)
			      (reg-tn-encoding dst))
			  op3 (reg-tn-encoding src1) 1 0))))

(eval-when (compile eval)

;;; have to do this because defconstant is evalutated in the null lex env.
(defmacro with-ref-format (printer)
  `(let* ((addend
	   '(:choose (:plus-integer immed) ("+" rs2)))
	  (ref-format
	   `("[" rs1 (:unless (:constant 0) ,addend) "]"
	     (:choose (:unless (:constant 0) asi) nil))))
     ,printer))

(defconstant load-printer
  (with-ref-format `(:NAME :TAB ,ref-format ", " rd)))

(defconstant store-printer
  (with-ref-format `(:NAME :TAB rd ", " ,ref-format)))

(defmacro define-f3-inst (name op op3 &key fixup load-store (dest-kind 'reg)
			       (printer :default) reads writes flushable)
  (let ((printer
	 (if (eq printer :default)
	     (case load-store
	       ((nil) :default)
	       ((:load t) 'load-printer)
	       (:store 'store-printer))
	     printer)))
    (when (and (atom reads) (not (null reads)))
      (setf reads (list reads)))
    (when (and (atom writes) (not (null writes)))
       (setf writes (list writes)))
    `(define-instruction ,name (segment dst src1 &optional src2)
       (:declare (type tn dst)
		 ,(if (or fixup load-store)
		      '(type (or tn (signed-byte 13) null fixup) src1 src2)
		      '(type (or tn (signed-byte 13) null) src1 src2)))
       ,@(unless (eq dest-kind 'odd-fp-reg)
	   `((:printer format-3-reg
		       ((op ,op) (op3 ,op3) (rd nil :type ',dest-kind))
		       ,printer)
	     (:printer format-3-immed
		       ((op ,op) (op3 ,op3) (rd nil :type ',dest-kind))
		       ,printer)))
       ,@(when flushable
	   '((:attributes flushable)))
       (:dependencies
	(reads src1)
	,@(let ((reads-list nil))
	    (dolist (read reads)
	      (push (list 'reads read) reads-list))
	    reads-list)
	,@(cond ((eq load-store :store)
		 '((reads dst)
		   (if src2 (reads src2))))
		 ((eq load-store t)
		  '((reads :memory)
		    (reads dst)
		    (if src2 (reads src2))))
		((eq load-store :load)
		 '((reads :memory)
		   (if src2 (reads src2) (reads dst))))
		(t
		 '((if src2 (reads src2) (reads dst)))))
	,@(let ((writes-list nil))
	    (dolist (write writes)
	      (push (list 'writes write) writes-list))
	    writes-list)
	,@(cond ((eq load-store :store)
		 '((writes :memory :partially t)))
		((eq load-store t)
		 '((writes :memory :partially t)
		   (writes dst)))
		((eq load-store :load)
		 '((writes dst)))
		(t
		 '((writes dst)))))
       (:delay 0)
       (:emitter (emit-format-3-inst segment ,op ,op3 dst src1 src2
				     :load-store ,load-store
				     :fixup ,fixup
				     :dest-kind (not (eq ',dest-kind 'reg))
				     :odd (eq ',dest-kind 'odd-fp-reg))))))

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
(define-f3-inst stf-odd #b11 #b100100 :dest-kind odd-fp-reg :load-store :store)
(define-f3-inst stdf #b11 #b100111 :dest-kind fp-reg :load-store :store)
(define-f3-inst ldstub #b11 #b001101 :load-store t)
(define-f3-inst swap #b11 #b001111 :load-store t)
(define-f3-inst add #b10 #b000000 :fixup t)
(define-f3-inst addcc #b10 #b010000 :writes :psr)
(define-f3-inst addx #b10 #b001000 :reads :psr)
(define-f3-inst addxcc #b10 #b011000 :reads :psr :writes :psr)
(define-f3-inst taddcc #b10 #b100000 :writes :psr)
(define-f3-inst taddcctv #b10 #b100010 :writes :psr)
(define-f3-inst sub #b10 #b000100)
(define-f3-inst subcc #b10 #b010100 :writes :psr)
(define-f3-inst subx #b10 #b001100 :reads :psr)
(define-f3-inst subxcc #b10 #b011100 :reads :psr :writes :psr)
(define-f3-inst tsubcc #b10 #b100001 :writes :psr)
(define-f3-inst tsubcctv #b10 #b100011 :writes :psr)
(define-f3-inst mulscc #b10 #b100100 :reads :y :writes (:psr :y))
(define-f3-inst and #b10 #b000001)
(define-f3-inst andcc #b10 #b010001 :writes :psr)
(define-f3-inst andn #b10 #b000101)
(define-f3-inst andncc #b10 #b010101 :writes :psr)
(define-f3-inst or #b10 #b000010)
(define-f3-inst orcc #b10 #b010010 :writes :psr)
(define-f3-inst orn #b10 #b000110)
(define-f3-inst orncc #b10 #b010110 :writes :psr)
(define-f3-inst xor #b10 #b000011)
(define-f3-inst xorcc #b10 #b010011 :writes :psr)
(define-f3-inst xnor #b10 #b000111)
(define-f3-inst xnorcc #b10 #b010111 :writes :psr)
(define-f3-inst sll #b10 #b100101)
(define-f3-inst srl #b10 #b100110)
(define-f3-inst sra #b10 #b100111)
(define-f3-inst save #b10 #b111100 :reads :psr :writes :psr)
(define-f3-inst restore #b10 #b111101 :reads :psr :writes :psr)



;;;; Random instructions.

(define-instruction ldfsr (segment src1 src2)
  (:declare (type tn src1) (type (signed-byte 13) src2))
  (:printer format-3-immed ((op #b11) (op3 #b100001)))
  :pinned
  (:delay 0)
  (:emitter (emit-format-3-immed segment #b11 0 #b100001
				 (reg-tn-encoding src1) 1 src2)))

(define-instruction stfsr (segment src1 src2)
  (:declare (type tn src1) (type (signed-byte 13) src2))
  (:printer format-3-immed ((op #b11) (op3 #b100101)))
  :pinned
  (:delay 0)
  (:emitter (emit-format-3-immed segment #b11 0 #b100101 
				 (reg-tn-encoding src1) 1 src2)))

(eval-when (compile load eval)
  (defun sethi-arg-printer (value stream dstate)
    (declare (ignore dstate))
    (format stream "%hi(#x~8,'0x)" (ash value 10)))
) ; eval-when (compile load eval)

(define-instruction sethi (segment dst src1)
  (:declare (type tn dst)
	    (type (or (signed-byte 22) (unsigned-byte 22) fixup) src1))
  (:printer format-2-immed
            ((op2 #b100) (immed nil :printer #'sethi-arg-printer)))
  (:dependencies (writes dst))
  (:delay 0)
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
  (:printer format-3-immed ((op #b10) (op3 #b101000) (rs1 0) (immed 0))
            '('RD :tab '%Y ", " rd))
  (:dependencies (reads :y) (writes dst))
  (:delay 0)
  (:emitter (emit-format-3-immed segment #b10 (reg-tn-encoding dst) #b101000
				 0 1 0)))

(defconstant wry-printer
  '('WR :tab rs1 (:unless (:constant 0) ", " (:choose immed rs2)) ", " '%Y))

(define-instruction wry (segment src1 &optional src2)
  (:declare (type tn src1) (type (or (signed-byte 13) tn null) src2))
  (:printer format-3-reg ((op #b10) (op3 #b110000) (rd 0)) wry-printer)
  (:printer format-3-immed ((op #b10) (op3 #b110000) (rd 0)) wry-printer)
  (:dependencies (reads src1) (if src2 (reads src2)) (writes :y))
  (:delay 3)
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

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (system:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (kernel:copy-from-system-area sap (* sparc:byte-bits (1+ offset))
                                         vector (* sparc:word-bits
                                                   sparc:vector-data-offset)
                                         (* length sparc:byte-bits))
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
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

(defun unimp-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (disassem:note x dstate))))
    (case (format-2-unimp-data chunk dstate)
      (#.vm:error-trap
       (nt "Error trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:cerror-trap
       (nt "Cerror trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:object-not-list-trap
       (nt "Object not list trap"))
      (#.vm:breakpoint-trap
       (nt "Breakpoint trap"))
      (#.vm:pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.vm:halt-trap
       (nt "Halt trap"))
      (#.vm:function-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
      (#.vm:object-not-instance-trap
       (nt "Object not instance trap"))
    )))

(define-instruction unimp (segment data)
  (:declare (type (unsigned-byte 22) data))
  (:printer format-2-unimp () :default :control #'unimp-control)
  (:delay 0)
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
	  (let ((offset (ash (- (label-position target) posn) -2)))
	    (when (and (= a 1) (> 0 offset))
	      (error "Offset of BA must be positive"))
	    offset)))))

(define-instruction b (segment cond-or-target &optional target)
  (:declare (type (or label branch-condition) cond-or-target)
	    (type (or label null) target))
  (:printer format-2-branch ((op #b00) (op2 #b010)))
  (:attributes branch)
  (:dependencies (reads :psr))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment 0 #b010 cond-or-target target)))

(define-instruction ba (segment cond-or-target &optional target)
  (:declare (type (or label branch-condition) cond-or-target)
	    (type (or label null) target))
  (:printer format-2-branch ((op #b00) (op2 #b010) (a 1))
            nil
            :print-name 'b)
  (:attributes branch)
  (:dependencies (reads :psr))
  (:delay 0)
  (:emitter
   (emit-relative-branch segment 1 #b010 cond-or-target target)))

(define-instruction t (segment condition target)
  (:declare (type branch-condition condition)
	    (type (or (signed-byte 13) (unsigned-byte 13)) target))
  (:printer format-3-immed ((op #b10)
                            (rd nil :type 'branch-condition)
                            (op3 #b111010)
                            (rs1 0))
            '(:name rd :tab immed))
  (:attributes branch)
  (:dependencies (reads :psr))
  (:delay 0)
  (:emitter (emit-format-3-immed segment #b10 (branch-condition condition)
				 #b111010 0 1 target)))

(define-instruction fb (segment condition target)
  (:declare (type fp-branch-condition condition) (type label target))
  (:printer format-2-branch ((op #B00)
                             (cond nil :type 'branch-fp-condition)
                             (op2 #b110)))
  (:attributes branch)
  (:dependencies (reads :fsr))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment 0 #b110 condition target t)))

(defconstant jal-printer
  '(:name :tab
          (:choose (rs1 (:unless (:constant 0) (:plus-integer immed)))
                   (:cond ((rs2 :constant 0) rs1)
                          ((rs1 :constant 0) rs2)
                          (t rs1 "+" rs2)))
          (:unless (:constant 0) ", " rd)))

(define-instruction jal (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn integer) src1)
	    (type (or null fixup tn (signed-byte 13)) src2))
  (:printer format-3-reg ((op #b10) (op3 #b111000)) jal-printer)
  (:printer format-3-immed ((op #b10) (op3 #b111000)) jal-printer)
  (:attributes branch)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 1)
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
  (:printer format-3-reg ((op #b10) (op3 #b111000) (rd 0)) jal-printer)
  (:printer format-3-immed ((op #b10) (op3 #b111000) (rd 0)) jal-printer)
  (:attributes branch)
  (:dependencies (reads src1) (if src2 (reads src2)))
  (:delay 1)
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

(defun emit-fp-inst (segment opf op3 dst src1 src2 &optional odd)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (emit-format-3-fpop segment #b10 (fp-reg-tn-encoding dst odd) op3
		      (fp-reg-tn-encoding src1 odd) opf
		      (fp-reg-tn-encoding src2 odd)))

(eval-when (compile eval)

(defmacro define-unary-fp-inst (name opf &key reads odd)
  `(define-instruction ,name (segment dst src1)
     (:declare (type tn dst src1))
     (:printer format-3-fpop ((op #b10) (op3 #b110100) (opf ,opf)))
     (:dependencies
      ,@(when reads
	  `((reads ,reads)))
      (reads dst)
      (reads src1)
      (writes dst))
     (:delay 0)
     (:emitter (emit-fp-inst segment ,opf #b110100 dst src1 nil ,odd))))

(defmacro define-binary-fp-inst (name opf
				      &key (op3 #b110100) reads writes delay)
  `(define-instruction ,name (segment dst src1 &optional src2)
     (:declare (type tn dst src1) (type (or null tn) src2))
     (:printer format-3-fpop ((op #b10) (op3 ,op3) (opf ,opf)))
     (:dependencies
      ,@(when reads
	  `((reads ,reads)))
      (reads src1)
      (if src2 (reads src2) (reads dst))
      ,@(when writes
	  `((writes ,writes)))
      (writes dst))
     ,@(if delay
	   `((:delay ,delay))
	   '((:delay 0)))
     (:emitter (emit-fp-inst segment ,opf ,op3 dst src1 src2))))
  
); eval-when (compile eval)


(define-unary-fp-inst fitos #b011000100 :reads :fsr)
(define-unary-fp-inst fitod #b011001000 :reads :fsr)
(define-unary-fp-inst fitox #b011001100 :reads :fsr)

(define-unary-fp-inst fstoir #b011000001 :reads :fsr)
(define-unary-fp-inst fdtoir #b011000010 :reads :fsr)
(define-unary-fp-inst fxtoir #b011000011 :reads :fsr)

(define-unary-fp-inst fstoi #b011010001)
(define-unary-fp-inst fdtoi #b011010010)
(define-unary-fp-inst fxtoi #b011010011)

(define-unary-fp-inst fstod #b011001001 :reads :fsr)
(define-unary-fp-inst fstox #b011001101 :reads :fsr)
(define-unary-fp-inst fdtos #b011000110 :reads :fsr)
(define-unary-fp-inst fdtox #b011001110 :reads :fsr)
(define-unary-fp-inst fxtos #b011000111 :reads :fsr)
(define-unary-fp-inst fxtod #b011001011 :reads :fsr)

(define-unary-fp-inst fmovs #b000000001)
(define-instruction fmovs-odd (segment dst src1)
  (:declare (type tn dst src1))
  (:dependencies (reads dst) (reads src1) (writes dst))
  (:delay 0)
  (:emitter (emit-fp-inst segment #b000000001 #b110100 dst src1 nil t)))

(define-unary-fp-inst fnegs #b000000101)
(define-unary-fp-inst fabss #b000001001)

(define-unary-fp-inst fsqrts #b000101001 :reads :fsr)
(define-unary-fp-inst fsqrtd #b000101010 :reads :fsr)
(define-unary-fp-inst fsqrtx #b000101011 :reads :fsr)



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

(define-binary-fp-inst fcmps #b001010001 :op3 #b110101 :writes :fsr :delay 1)
(define-binary-fp-inst fcmpd #b001010010 :op3 #b110101 :writes :fsr :delay 1)
(define-binary-fp-inst fcmpx #b001010011 :op3 #b110101 :writes :fsr :delay 1)
(define-binary-fp-inst fcmpes #b001010101 :op3 #b110101 :writes :fsr :delay 1)
(define-binary-fp-inst fcmped #b001010110 :op3 #b110101 :writes :fsr :delay 1)
(define-binary-fp-inst fcmpex #b001010111 :op3 #b110101 :writes :fsr :delay 1)



;;;; li, jali, ji, nop, cmp, not, neg, move, and more

(defun %li (reg value)
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
  
(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

;;; Jal to a full 32-bit address.  Tmpreg is trashed.
(define-instruction jali (segment link tmpreg value)
  (:declare (type tn link tmpreg)
	    (type (or (signed-byte 13) (signed-byte 32) (unsigned-byte 32)
		      fixup) value))
  (:attributes variable-length)
  (:vop-var vop)
  (:attributes branch)
  (:dependencies (writes link) (writes tmpreg))
  (:delay 1)
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
  (:attributes variable-length)
  (:vop-var vop)
  (:attributes branch)
  (:dependencies (writes tmpreg))
  (:delay 1)
  (:emitter
   (assemble (segment vop)
	     (inst jali zero-tn tmpreg value))))

(define-instruction nop (segment)
  (:printer format-2-immed ((rd 0) (op2 #b100) (immed 0)) '(:name))
  (:attributes flushable)
  (:delay 0)
  (:emitter (emit-format-2-immed segment 0 0 #b100 0)))

(def-vm-support-routine emit-nop (segment)
  (emit-format-2-immed segment 0 0 #b100 0))

(define-instruction cmp (segment src1 &optional src2)
  (:declare (type tn src1) (type (or null tn (signed-byte 13)) src2))
  (:printer format-3-reg ((op #b10) (op3 #b010100) (rd 0))
            '(:name :tab rs1 ", " rs2))
  (:printer format-3-immed ((op #b10) (op3 #b010100) (rd 0))
            '(:name :tab rs1 ", " immed))
  (:dependencies (reads src1) (if src2 (reads src2)) (writes :psr))
  (:delay 0)
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
  (:printer format-3-reg ((op #b10) (op3 #b000111) (rs2 0))
            '(:name :tab (:unless (:same-as rd) rs1 ", " ) rd))
  (:dependencies (if src1 (reads src1) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src1
     (setf src1 dst))
   (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000111
		      (reg-tn-encoding src1) 0 0 0)))

(define-instruction neg (segment dst &optional src1)
  (:declare (type tn dst) (type (or tn null) src1))
  (:printer format-3-reg ((op #b10) (op3 #b000100) (rs1 0))
            '(:name :tab (:unless (:same-as rd) rs2 ", " ) rd))
  (:dependencies (if src1 (reads src1) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src1
     (setf src1 dst))
   (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000100
		      0 0 0 (reg-tn-encoding src1))))

(define-instruction move (segment dst src1)
  (:declare (type tn dst src1))
  (:printer format-3-reg ((op #b10) (op3 #b000010) (rs1 0))
            '(:name :tab rs2 ", " rd))
  (:attributes flushable)
  (:dependencies (reads src1) (writes dst))
  (:delay 0)
  (:emitter (emit-format-3-reg segment #b10 (reg-tn-encoding dst) #b000010
			       0 0 0 (reg-tn-encoding src1))))



;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:delay 0)
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
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
  (:delay 0)
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
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
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
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
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))
