;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: src/compiler/x86/insts.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the x86 instruction set, for 80386 and above.
;;;
;;; Written by William Lott
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Debugging and enhancements by Douglas Crosher 1996, 1997, 1998.
;;;

(in-package :x86)
(intl:textdomain "cmucl-x86-vm")

(use-package :new-assem)

(def-assembler-params
  :scheduler-p nil)

(disassem:set-disassem-params :instruction-alignment 8
			      :opcode-column-width 10)



;;;; Primitive emitters.

(define-emitter emit-word 16
  (byte 16 0))

(define-emitter emit-dword 32
  (byte 32 0))

(define-emitter emit-byte-with-reg 8
  (byte 5 3) (byte 3 0))

(define-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))



;;;; Fixup emitters.

(defun emit-absolute-fixup (segment fixup)
  (note-fixup segment :absolute fixup)
  (let ((offset (fixup-offset fixup)))
    (if (label-p offset)
	(emit-back-patch segment 4
			 #'(lambda (segment posn)
			     (declare (ignore posn))
			     (emit-dword segment
					 (- (+ (component-header-length)
					       (or (label-position offset) 0) )
					    other-pointer-type))))
	(emit-dword segment (or offset 0)))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-dword segment (or (fixup-offset fixup) 0)))



;;;; The effective-address (ea) structure.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  ;; ea only has space for three bits of register number: regs r8
  ;; and up are selected by a REX prefix byte which caller is responsible
  ;; for having emitted where necessary already
  (ecase (sb-name (sc-sb (tn-sc tn)))
    (registers
     (let ((offset (mod (tn-offset tn) 16)))
       (logior (ash (logand offset 1) 2)
               (ash offset -1))))
    (float-registers
     (mod (tn-offset tn) 8))))


(defstruct (ea
	    (:constructor make-ea (size &key base index scale disp))
	    (:print-function %print-ea))
  (size nil :type (member :byte :word :dword))
  (base nil :type (or tn null))
  (index nil :type (or tn null))
  (scale 1 :type (member 1 2 4 8))
  (disp 0 :type (or (signed-byte 32) fixup)))

(defun valid-displacement-p (x)
  (typep x '(or (signed-byte 32) fixup)))

(defun %print-ea (ea stream depth)
  (declare (ignore depth))
  (cond ((or *print-escape* *print-readably*)
	 (print-unreadable-object (ea stream :type t)
	   (format stream
		   "~S~@[ base=~S~]~@[ index=~S~]~@[ scale=~S~]~@[ disp=~S~]"
		   (ea-size ea)
		   (ea-base ea)
		   (ea-index ea)
		   (let ((scale (ea-scale ea)))
		     (if (= scale 1) nil scale))
		   (ea-disp ea))))
	(t
	 (format stream "~A PTR [" (symbol-name (ea-size ea)))
	 (when (ea-base ea)
	   (write-string (x86-location-print-name (ea-base ea)) stream)
	   (when (ea-index ea)
	     (write-string "+" stream)))
	 (when (ea-index ea)
	   (write-string (x86-location-print-name (ea-index ea)) stream))
	 (unless (= (ea-scale ea) 1)
	   (format stream "*~A" (ea-scale ea)))
	 (typecase (ea-disp ea)
	   (null)
	   (integer
	    (format stream "~@D" (ea-disp ea)))
	   (t
	    (format stream "+~A" (ea-disp ea))))
	 (write-char #\] stream))))

(defun emit-ea (segment thing reg &optional allow-constants)
  (etypecase thing
    (tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       ((registers #+sse2 float-registers)
	(emit-mod-reg-r/m-byte segment #b11 reg (reg-tn-encoding thing)))
       (stack
	;; Convert stack tns into an index off of EBP.
	(let ((disp (- (* (1+ (tn-offset thing)) word-bytes))))
	  (cond ((< -128 disp 127)
		 (emit-mod-reg-r/m-byte segment #b01 reg #b101)
		 (emit-byte segment disp))
		(t
		 (emit-mod-reg-r/m-byte segment #b10 reg #b101)
		 (emit-dword segment disp)))))
       (constant
	(unless allow-constants
	  (error
	   "Constant TNs can only be directly used in MOV, PUSH, and CMP."))
	(emit-mod-reg-r/m-byte segment #b00 reg #b101)
	(emit-absolute-fixup segment
			     (make-fixup nil
					 :code-object
					 (- (* (tn-offset thing) word-bytes)
					    other-pointer-type))))))
    (ea
     (let* ((base (ea-base thing))
	    (index (ea-index thing))
	    (scale (ea-scale thing))
	    (disp (ea-disp thing))
	    (mod (cond ((or (null base)
			    (and (eql disp 0)
				 (not (= (reg-tn-encoding base) #b101))))
			#b00)
		       ((and (fixnump disp) (<= -128 disp 127))
			#b01)
		       (t
			#b10)))
	    (r/m (cond (index #b100)
		       ((null base) #b101)
		       (t (reg-tn-encoding base)))))
       (emit-mod-reg-r/m-byte segment mod reg r/m)
       (when (= r/m #b100)
	 (let ((ss (1- (integer-length scale)))
	       (index (if (null index)
			  #b100
			  (let ((index (reg-tn-encoding index)))
			    (if (= index #b100)
				(error "Can't index off of ESP")
				index))))
	       (base (if (null base)
			 #b101
			 (reg-tn-encoding base))))
	   (emit-sib-byte segment ss index base)))
       (cond ((= mod #b01)
	      (emit-byte segment disp))
	     ((or (= mod #b10) (null base))
	      (if (fixup-p disp)
		  (emit-absolute-fixup segment disp)
		  (emit-dword segment disp))))))
    (fixup
     (emit-mod-reg-r/m-byte segment #b00 reg #b101)
     (emit-absolute-fixup segment thing))))

(defun fp-reg-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))

;;;
;;; like the above, but for fp-instructions--jrd
;;;
(defun emit-fp-op (segment thing op)
  (if (fp-reg-tn-p thing)
      (emit-byte segment (dpb op (byte 3 3) (dpb (tn-offset thing)
						 (byte 3 0)
						 #b11000000)))
    (emit-ea segment thing op)))

(defun byte-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) byte-sc-names)
       t))

(defun byte-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :byte))
    (tn
     (and (member (sc-name (tn-sc thing)) byte-sc-names) t))
    (t nil)))

(defun word-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) word-sc-names)
       t))

(defun word-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :word))
    (tn (and (member (sc-name (tn-sc thing)) word-sc-names) t))
    (t nil)))

(defun dword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) dword-sc-names)
       t))

(defun dword-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :dword))
    (tn
     (and (member (sc-name (tn-sc thing)) dword-sc-names) t))
    (t nil)))

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun accumulator-p (thing)
  (and (register-p thing)
       (= (tn-offset thing) 0)))

(eval-when (compile load eval)
;; If a line has more than one value, then these are all synonyms, but
;; the first one is the one that is preferred when printing the
;; condition code out.
(defconstant conditions
  '(
    ;; OF = 1
    (:o . 0)
    ;; OF = 0
    (:no . 1)
    ;; Unsigned <; CF = 1
    (:b . 2) (:nae . 2) (:c . 2)
    ;; Unsigned >=; CF = 0
    (:ae . 3) (:nb . 3) (:nc . 3)
    ;; Equal; ZF = 1
    (:e . 4) (:eq . 4) (:z . 4)
    ;; Not equal; ZF = 0
    (:ne . 5) (:nz . 5)
    ;; Unsigned <=; CF = 1 or ZF = 1
    (:be . 6) (:na . 6)
    ;; Unsigned >; CF = 1 and ZF = 0
    (:a . 7) (:nbe . 7)
    ;; SF = 1
    (:s . 8)
    ;; SF = 0
    (:ns . 9)
    ;; Parity even
    (:p . 10) (:pe . 10)
    ;; Parity odd
    (:np . 11) (:po . 11)
    ;; Signed <; SF /= OF
    (:l . 12) (:nge . 12)
    ;; Signed >=; SF = OF
    (:ge . 13) (:nl . 13)
    ;; Signed <=; ZF = 1 or SF /= OF
    (:le . 14) (:ng . 14)
    ;; Signed >; ZF =0 and SF = OF
    (:g . 15) (:nle . 15)))

(defun conditional-opcode (condition)
  (cdr (assoc condition conditions :test #'eq))))



;;;; Utilities.

#-lispworks3
(defconstant operand-size-prefix-byte #b01100110)

#+lispworks3
(eval-when (compile load eval)
  (defconstant operand-size-prefix-byte #b01100110))

(defparameter *default-operand-size* :dword)

(defun maybe-emit-operand-size-prefix (segment size)
  (unless (or (eq size :byte) (eq size *default-operand-size*))
    (emit-byte segment operand-size-prefix-byte)))

(defun operand-size (thing)
  (typecase thing
    (tn
     (case (sc-name (tn-sc thing))
       (#.dword-sc-names
	:dword)
       (#.word-sc-names
	:word)
       (#.byte-sc-names
	:byte)
       ;; added by jrd.  float-registers is a separate size (?)
       (#.float-sc-names
	:float)
       (#.double-sc-names
	:double)
       (t
	(error "Can't tell the size of ~S ~S" thing (sc-name (tn-sc thing))))))
    (ea
     (ea-size thing))
    (t
     nil)))

(defun matching-operand-size (dst src)
  (let ((dst-size (operand-size dst))
	(src-size (operand-size src)))
    (if dst-size
	(if src-size
	    (if (eq dst-size src-size)
		dst-size
		(error "Size mismatch: ~S is a ~S and ~S is a ~S"
		       dst dst-size src src-size))
	    dst-size)
	(if src-size
	    src-size
	    (error "Can't tell the size of either ~S or ~S."
		   dst src)))))

(defun compatible-operand-size (dst src)
  (let ((dst-size (operand-size dst))
	(src-size (operand-size src)))
    (if dst-size
	dst-size
	(if src-size
	    src-size
	    (error "Can't tell the size of either ~S or ~S."
		   dst src)))))

(defun emit-sized-immediate (segment size value)
  (ecase size
    (:byte
     (emit-byte segment value))
    (:word
     (emit-word segment value))
    (:dword
     (emit-dword segment value))))



;;;; Disassembler support stuff.

(deftype reg () '(unsigned-byte 3))  
#+cross-compiler
(lisp:deftype reg () '(unsigned-byte 3))

(eval-when (compile eval load)

(defparameter *default-address-size*
  ;; Actually, :dword is the only one really supported.
  :dword)

(defparameter byte-reg-names
  #(al cl dl bl ah ch dh bh))
(defparameter word-reg-names
  #(ax cx dx bx sp bp si di))
(defparameter dword-reg-names
  #(eax ecx edx ebx esp ebp esi edi))

(defun print-reg-with-width (value width stream dstate)
  (declare (ignore dstate))
  (princ (aref (ecase width
		 (:byte byte-reg-names)
		 (:word word-reg-names)
		 (:dword dword-reg-names))
	       value)
	 stream)
  ;; plus should do some source-var notes
  )

(defun print-reg (value stream dstate)
  (declare (type reg value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (print-reg-with-width value
			(disassem:dstate-get-prop dstate 'width)
			stream
			dstate))

(defun print-word-reg (value stream dstate)
  (declare (type reg value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (print-reg-with-width value
			(or (disassem:dstate-get-prop dstate 'word-width)
			    *default-operand-size*)
			stream
			dstate))

(defun print-byte-reg (value stream dstate)
  (declare (type reg value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type reg value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (print-reg-with-width value *default-address-size* stream dstate))

;;; Value is a list of (BASE-REG OFFSET INDEX-REG INDEX-SCALE)
(defun print-mem-access (value stream print-size-p dstate)
  (declare (type list value)
	   (type stream stream)
	   (type (member t nil) print-size-p)
	   (type disassem:disassem-state dstate))
  (when print-size-p
    (princ (disassem:dstate-get-prop dstate 'width) stream)
    (princ '| PTR | stream))
  (write-char #\[ stream)
  (destructuring-bind (&optional base disp index scale) value
    (when base
      (print-addr-reg base stream dstate)
      (when index
	(write-char #\+ stream)))
    (when index
      (print-addr-reg index stream dstate))
    (when (and scale (> scale 1))
      (write-char #\* stream)
      (princ scale stream))
    (when (and disp (not (zerop disp)))
      (let ((unsigned-offset (ldb (byte vm:word-bits 0) disp)))
	(or (nth-value 1
		       (disassem::note-code-constant-absolute unsigned-offset
							      dstate))
	    (disassem:maybe-note-assembler-routine unsigned-offset
						   stream
						   dstate)
	    (let ((offs (- disp disassem::nil-addr)))
	      (when (typep offs 'disassem::offset)
		(or (disassem::maybe-note-nil-indexed-symbol-slot-ref offs
								      dstate)
		    (disassem::maybe-note-static-function offs dstate)))))
	(cond ((or base index)
	       (write-char (if (minusp disp) #\- #\+) stream)
	       (princ (abs disp) stream))
	      (t
	       (princ unsigned-offset stream))))))
  (write-char #\] stream))

(defun print-imm-data (value stream dstate)
  (let ((offset (- value disassem::nil-addr)))
    (princ value stream)
    (when (typep offset 'disassem::offset)
      (or (disassem::maybe-note-nil-indexed-object offset dstate)
	  (let ((unsigned-offset (ldb (byte vm:word-bits 0) value)))
	    (disassem::maybe-note-assembler-routine unsigned-offset stream dstate))
	  (nth-value 1
		     (disassem::note-code-constant-absolute offset
							    dstate))))))

(defun print-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (if (typep value 'reg)
      (print-reg value stream dstate)
      (print-mem-access value stream nil dstate)))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (if (typep value 'reg)
      (print-reg value stream dstate)
      (print-mem-access value stream t dstate)))

(defun print-byte-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
	   (type stream stream)
	   (type disassem:disassem-state dstate))
  (if (typep value 'reg)
      (print-byte-reg value stream dstate)
      (print-mem-access value stream t dstate)))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (princ (if (and (numberp value) (minusp value))
	     (ldb (byte vm:word-bits 0) value)
	     value)
	 stream))

;;; Returns either an integer, meaning a register, or a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component
;;; may be missing or nil to indicate that it's not used or has the
;;; obvious default value (e.g., 1 for the index-scale).
(defun prefilter-reg/mem (value dstate)
  (declare (type list value)
	   (type disassem:disassem-state dstate))
  (let ((mod (car value))
	(r/m (cadr value)))
    (declare (type (unsigned-byte 2) mod)
	     (type (unsigned-byte 3) r/m))
    (cond ((= mod #b11)
	   ;; registers
	   r/m)
	  ((= r/m #b100)
	   ;; sib byte
	   (let ((sib (disassem:read-suffix 8 dstate)))
	     (declare (type (unsigned-byte 8) sib))
	     (let ((base-reg (ldb (byte 3 0) sib))
		   (index-reg (ldb (byte 3 3) sib))
		   (index-scale (ldb (byte 2 6) sib)))
	       (declare (type (unsigned-byte 3) base-reg index-reg)
			(type (unsigned-byte 2) index-scale))
	       (let* ((offset
		       (case mod
			 (#b00
			  (if (= base-reg #b101)
			      (disassem:read-signed-suffix 32 dstate)
			      nil))
			 (#b01
			  (disassem:read-signed-suffix 8 dstate))
			 (#b10
			  (disassem:read-signed-suffix 32 dstate)))))
		 (list (if (and (= mod #b00) (= base-reg #b101)) nil base-reg)
		       offset
		       (if (= index-reg #b100) nil index-reg)
		       (ash 1 index-scale))))))
	  ((and (= mod #b00) (= r/m #b101))
	   (list nil (disassem:read-signed-suffix 32 dstate)) )
	  ((= mod #b00)
	   (list r/m))
	  ((= mod #b01)
	   (list r/m (disassem:read-signed-suffix 8 dstate)))
	  (t				; (= mod #b10)
	   (list r/m (disassem:read-signed-suffix 32 dstate))))))

(defun prefilter-reg-r (value dstate)
  (declare (type reg value)
           (type disassem:disassem-state dstate)
	   (ignore dstate))
  value)

;;; This is a sort of bogus prefilter that just
;;; stores the info globally for other people to use; it
;;; probably never gets printed.
(defun prefilter-width (value dstate)
  (setf (disassem:dstate-get-prop dstate 'width)
	(if (zerop value)
	    :byte
	    (let ((word-width
		   ;; set by a prefix instruction
		   (or (disassem:dstate-get-prop dstate 'word-width)
		       *default-operand-size*)))
	      (when (not (eql word-width *default-operand-size*))
		;; reset it
		(setf (disassem:dstate-get-prop dstate 'word-width)
		      *default-operand-size*))
	      word-width))))

(defun offset-next (value dstate)
  (declare (type integer value)
	   (type disassem:disassem-state dstate))
  (+ (disassem:dstate-next-addr dstate) value))

(defun read-address (value dstate)
  (declare (ignore value))		; always nil anyway
  (disassem:read-suffix (width-bits *default-address-size*) dstate))

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:float 32)
    (:double 64)))

;;; Return true if THING is an XMM register TN.
(defun xmm-register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value)
           (type stream stream)
           (ignore dstate))
  (format stream "~A~D" 'xmm value))

(defun print-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type disassem:disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
      (print-mem-access value stream nil dstate)))

;; Same as print-xmmreg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type disassem:disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
    (print-mem-access value stream nil dstate)))

); eval-when


;;;; Disassembler argument types.

(disassem:define-argument-type accum
  :printer #'(lambda (value stream dstate)
	       (declare (ignore value)
			(type stream stream)
			(type disassem:disassem-state dstate))
	       (print-reg 0 stream dstate))
  )

(disassem:define-argument-type word-accum
  :printer #'(lambda (value stream dstate)
	       (declare (ignore value)
			(type stream stream)
			(type disassem:disassem-state dstate))
	       (print-word-reg 0 stream dstate))
  )

(disassem:define-argument-type reg
  :printer #'print-reg)

(disassem:define-argument-type addr-reg
  :printer #'print-addr-reg)

(disassem:define-argument-type word-reg
  :printer #'print-word-reg)

(disassem:define-argument-type imm-addr
  :prefilter #'read-address
  :printer #'print-label)

(disassem:define-argument-type imm-data
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (disassem:read-suffix
		  (width-bits (disassem:dstate-get-prop dstate 'width))
		  dstate))
  :printer #'print-imm-data
  )

(disassem:define-argument-type signed-imm-data
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (let ((width (disassem:dstate-get-prop dstate 'width)))
		   (disassem:read-signed-suffix (width-bits width) dstate)))
  )

(disassem:define-argument-type signed-imm-byte
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (disassem:read-signed-suffix 8 dstate)))

(disassem:define-argument-type sign-extended-imm-byte
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (disassem:read-signed-suffix 8 dstate))
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate))
	       (princ (sign-extend value 8) stream)))

(disassem:define-argument-type signed-imm-dword
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value))		; always nil anyway
		 (disassem:read-signed-suffix 32 dstate)))

(disassem:define-argument-type imm-word
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (let ((width
			(or (disassem:dstate-get-prop dstate 'word-width)
			    *default-operand-size*)))
		   (disassem:read-suffix (width-bits width) dstate))))

;;; Needed for the ret imm16 instruction
(disassem:define-argument-type imm-word-16
  :prefilter #'(lambda (value dstate)
		 (declare (ignore value)) ; always nil anyway
		 (disassem:read-suffix 16 dstate)))

(disassem:define-argument-type reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-reg/mem)
(disassem:define-argument-type sized-reg/mem
  ;; Same as reg/mem, but prints an explicit size indicator for
  ;; memory references.
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem)
(disassem:define-argument-type byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-byte-reg/mem)

;;;
;;; added by jrd
;;;
(eval-when (compile load eval)
(defun print-fp-reg (value stream dstate)
  (declare (ignore dstate))
  (format stream "~A(~D)" 'st value))

(defun prefilter-fp-reg (value dstate)
  ;; just return it
  (declare (ignore dstate))
  value)
)
(disassem:define-argument-type fp-reg
			       :prefilter #'prefilter-fp-reg
			       :printer #'print-fp-reg)

(disassem:define-argument-type width
  :prefilter #'prefilter-width
  :printer #'(lambda (value stream dstate)
	       (if ;; (zerop value)
		   (or (null value) (and (numberp value) (zerop value)))	; zzz jrd
		   (princ 'b stream)
		   (let ((word-width
			  ;; set by a prefix instruction
			  (or (disassem:dstate-get-prop dstate 'word-width)
			      *default-operand-size*)))
		     (princ (schar (symbol-name word-width) 0) stream)))))


;;;; Disassembler instruction formats.

(eval-when (compile eval)
  (defun swap-if (direction field1 separator field2)
    `(:if (,direction :constant 0)
	  (,field1 ,separator ,field2)
	  (,field2 ,separator ,field1))))

(disassem:define-instruction-format (byte 8 :default-printer '(:name))
  (op    :field (byte 8 0))
  ;; optional fields
  (accum :type 'accum)
  (imm))

(disassem:define-instruction-format (simple 8)
  (op    :field (byte 7 1))
  (width :field (byte 1 0) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Same as simple, but with direction bit
(disassem:define-instruction-format (simple-dir 8 :include 'simple)
  (op :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as simple, but with the immediate value occuring by default,
;;; and with an appropiate printer.
(disassem:define-instruction-format (accum-imm 8
				     :include 'simple
				     :default-printer '(:name
							:tab accum ", " imm))
  (imm :type 'imm-data))

(disassem:define-instruction-format (reg-no-width 8
				     :default-printer '(:name :tab reg))
  (op	 :field (byte 5 3))
  (reg   :field (byte 3 0) :type 'word-reg)
  ;; optional fields
  (accum :type 'word-accum)
  (imm))

;;; adds a width field to reg-no-width
(disassem:define-instruction-format (reg 8 :default-printer '(:name :tab reg))
  (op    :field (byte 4 4))
  (width :field (byte 1 3) :type 'width)
  (reg   :field (byte 3 0) :type 'reg)
  ;; optional fields
  (accum :type 'accum)
  (imm)
  )

;;; Same as reg, but with direction bit
(disassem:define-instruction-format (reg-dir 8 :include 'reg)
  (op  :field (byte 3 5))
  (dir :field (byte 1 4)))

(disassem:define-instruction-format (two-bytes 16
				     :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(disassem:define-instruction-format (reg-reg/mem 16
				     :default-printer
				        `(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)	:type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
	   			:type 'sized-reg/mem)
  (reg     :field (byte 3 11)	:type 'reg)
  ;; optional fields
  (imm))

;;; same as reg-reg/mem, but with direction bit
(disassem:define-instruction-format (reg-reg/mem-dir 16
				     :include 'reg-reg/mem
				     :default-printer
				        `(:name
					  :tab
					  ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (op  :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as reg-rem/mem, but uses the reg field as a second op code.
(disassem:define-instruction-format (reg/mem 16
				     :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 7 1) (byte 3 11)))
  (width   :field (byte 1 0)	:type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
	   			:type 'sized-reg/mem)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but with the immediate value occuring by default,
;;; and with an appropiate printer.
(disassem:define-instruction-format (reg/mem-imm 16
				     :include 'reg/mem
				     :default-printer
				        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'imm-data))

;;; Same as reg/mem, but with using the accumulator in the default printer
(disassem:define-instruction-format
    (accum-reg/mem 16
     :include 'reg/mem :default-printer '(:name :tab accum ", " reg/mem))
  ;; This format uses the accumulator, so the size is known; therefore
  ;; we don't really need to print out the memory size, but let's do
  ;; it for consistency.
  (reg/mem :type 'sized-reg/mem)
  (accum :type 'accum))

;;; Same as reg-reg/mem, but with a prefix of #b00001111
(disassem:define-instruction-format (ext-reg-reg/mem 24
				     :default-printer
				        `(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :field (byte 7 9))
  (width   :field (byte 1 8)	:type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'sized-reg/mem)
  (reg     :field (byte 3 19)	:type 'reg)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but with a prefix of #b00001111
(disassem:define-instruction-format (ext-reg/mem 24
				     :default-printer '(:name :tab reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)	:type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'sized-reg/mem)
  ;; optional fields
  (imm))

;; Double shift instructions. Like ext-reg-reg/mem but there's no
;; width field.
(disassem:define-instruction-format (ext-reg-reg/mem-shift 24)
  (prefix  :field (byte 8 0)  :value #b00001111)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	                      :type 'sized-reg/mem)
  (reg     :field (byte 3 19) :type 'reg)
  ;; optional fields
  (imm))
							   

;;; ----------------------------------------------------------------
;;; this section added by jrd, for fp instructions.  

;;; 
;;; regular fp inst to/from registers/memory
;;;
(disassem:define-instruction-format (floating-point 16 
				      :default-printer `(:name :tab reg/mem))
  (prefix :field (byte 5 3) :value #b11011)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8)) :type 'reg/mem))

;;;
;;; fp insn to/from fp reg
;;;
(disassem:define-instruction-format (floating-point-fp 16 
				      :default-printer `(:name :tab fp-reg))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (fp-reg :field (byte 3 8) :type 'fp-reg))

;;;
;;; fp insn to/from fp reg, with the reversed source/destination flag.
;;;
(disassem:define-instruction-format
 (floating-point-fp-d 16 
   :default-printer `(:name :tab ,(swap-if 'd "ST0" ", " 'fp-reg)))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 2 0) (byte 3 11)))
  (d      :field (byte 1 2))
  (fp-reg :field (byte 3 8) :type 'fp-reg))


;;; pfw
;;; fp no operand isns
;;;
(disassem:define-instruction-format (floating-point-no 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011001)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(disassem:define-instruction-format (floating-point-3 16
				      :default-printer '(:name))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 6 8))))

(disassem:define-instruction-format (floating-point-5 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011011)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(disassem:define-instruction-format (floating-point-st 16
				      :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011111)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))


;;; ----------------------------------------------------------------


;;;; General Data Transfer

(eval-when (eval compile load)
  (defun toggle-word-width (chunk inst stream dstate)
    (declare (ignore chunk inst stream))
    (let ((ww (or (disassem:dstate-get-prop dstate 'word-width)
		  *default-operand-size*)))
      (setf (disassem:dstate-get-prop dstate 'word-width)
	    (ecase ww
	      (:word :dword)
	      (:dword :word))))))

;;; This isn't a really an instruction, but it's easier to deal with it this
;;; way.  We assume that it's actually used.
(define-instruction toggle-data-size (segment)
  (:printer byte ((op operand-size-prefix-byte))
	    nil				; don't actually print it
	    :control #'toggle-word-width))


(define-instruction mov (segment dst src)
  ;; immediate to register
  (:printer reg ((op #b1011) (imm nil :type 'imm-data))
	    '(:name :tab reg ", " imm))
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
	    `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm ((op '(#b1100011 #b000))))

  (:emitter
   (let ((size (compatible-operand-size dst src)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
	    (cond ((integerp src)
		   (emit-byte-with-reg segment
				       (if (eq size :byte)
					   #b10110
					   #b10111)
				       (reg-tn-encoding dst))
		   (emit-sized-immediate segment size src))
		  ((and (fixup-p src) (accumulator-p dst))
		   (emit-byte segment
			      (if (eq size :byte)
				  #b10100000
				  #b10100001))
		   (emit-absolute-fixup segment src))
		  (t
		   (emit-byte segment
			      (if (eq size :byte)
				  #b10001010
				  #b10001011))
		   (emit-ea segment src (reg-tn-encoding dst) t))))
	   ((and (fixup-p dst) (accumulator-p src))
	    (emit-byte segment (if (eq size :byte) #b10100010 #b10100011))
	    (emit-absolute-fixup segment dst))
	   ((integerp src)
	    (emit-byte segment (if (eq size :byte) #b11000110 #b11000111))
	    (emit-ea segment dst #b000)
	    (emit-sized-immediate segment size src))
	   ((register-p src)
	    (emit-byte segment (if (eq size :byte) #b10001000 #b10001001))
	    (emit-ea segment dst (reg-tn-encoding src)))
	   ((fixup-p src)
	    (assert (eq size :dword))
	    (emit-byte segment #b11000111)
	    (emit-ea segment dst #b000)
	    (emit-absolute-fixup segment src))
	   (t
	    (error "Bogus arguments to MOV: ~S ~S" dst src))))))

(defun emit-move-with-extension (segment dst src opcode)
  (assert (register-p dst))
  (let ((dst-size (operand-size dst))
	(src-size (operand-size src)))
    (ecase dst-size
      (:word
       (assert (eq src-size :byte))
       (maybe-emit-operand-size-prefix segment :word)
       (emit-byte segment #b00001111)
       (emit-byte segment opcode)
       (emit-ea segment src (reg-tn-encoding dst)))
      (:dword
       (ecase src-size
	 (:byte
	  (maybe-emit-operand-size-prefix segment :dword)
	  (emit-byte segment #b00001111)
	  (emit-byte segment opcode)
	  (emit-ea segment src (reg-tn-encoding dst)))
	 (:word
	  (emit-byte segment #b00001111)
	  (emit-byte segment (logior opcode 1))
	  (emit-ea segment src (reg-tn-encoding dst))))))))
	  
(define-instruction movsx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011111) (reg nil :type 'word-reg)))
  (:emitter
   (emit-move-with-extension segment dst src #b10111110)))

(define-instruction movzx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011011) (reg nil :type 'word-reg)))
  (:emitter
   (emit-move-with-extension segment dst src #b10110110)))

(define-instruction push (segment src)
  ;; Register.
  (:printer reg-no-width ((op #b01010)))
  ;; Register/Memory.
  (:printer reg/mem ((op '(#b1111111 #b110)) (width 1)))
  ;; Immediate.
  (:printer byte ((op #b01101010) (imm nil :type 'signed-imm-byte))
	    '(:name :tab imm))
  (:printer byte ((op #b01101000) (imm nil :type 'imm-word))
	    '(:name :tab imm))
  ;; ### Segment registers?

  (:emitter
   (cond ((integerp src)
	  (cond ((<= -128 src 127)
		 (emit-byte segment #b01101010)
		 (emit-byte segment src))
		(t
		 (emit-byte segment #b01101000)
		 (emit-dword segment src))))
	 ((fixup-p src)
	  ;; Interpret the fixup as an immediate dword to push
	  (emit-byte segment #b01101000)
	  (emit-absolute-fixup segment src))
	 (t
	  (let ((size (operand-size src)))
	    (assert (not (eq size :byte)))
	    (maybe-emit-operand-size-prefix segment size)
	    (cond ((register-p src)
		   (emit-byte-with-reg segment #b01010 (reg-tn-encoding src)))
		  (t
		   (emit-byte segment #b11111111)
		   (emit-ea segment src #b110 t))))))))

(define-instruction pusha (segment)
  (:printer byte ((op #b01100000)))
  (:emitter
   (emit-byte segment #b01100000)))

(define-instruction pop (segment dst)
  (:printer reg-no-width ((op #b01011)))
  (:printer reg/mem ((op '(#b1000111 #b000)) (width 1)))
  (:emitter
   (let ((size (operand-size dst)))
     (assert (not (eq size :byte)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
	    (emit-byte-with-reg segment #b01011 (reg-tn-encoding dst)))
	   (t
	    (emit-byte segment #b10001111)
	    (emit-ea segment dst #b000))))))

(define-instruction popa (segment)
  (:printer byte ((op #b01100001)))
  (:emitter
   (emit-byte segment #b01100001)))

(define-instruction xchg (segment operand1 operand2)
  ;; Register with accumulator.
  (:printer reg-no-width ((op #b10010)) '(:name :tab accum ", " reg))
  ;; Register/Memory with Register.
  (:printer reg-reg/mem ((op #b1000011)))
  (:emitter
   (let ((size (matching-operand-size operand1 operand2)))
     (maybe-emit-operand-size-prefix segment size)
     (labels ((xchg-acc-with-something (acc something)
		(if (and (not (eq size :byte)) (register-p something))
		    (emit-byte-with-reg segment
					#b10010
					(reg-tn-encoding something))
		    (xchg-reg-with-something acc something)))
	      (xchg-reg-with-something (reg something)
		(emit-byte segment (if (eq size :byte) #b10000110 #b10000111))
		(emit-ea segment something (reg-tn-encoding reg))))
       (cond ((accumulator-p operand1)
	      (xchg-acc-with-something operand1 operand2))
	     ((accumulator-p operand2)
	      (xchg-acc-with-something operand2 operand1))
	     ((register-p operand1)
	      (xchg-reg-with-something operand1 operand2))
	     ((register-p operand2)
	      (xchg-reg-with-something operand2 operand1))
	     (t
	      (error "Bogus args to XCHG: ~S ~S" operand1 operand2)))))))

(define-instruction lea (segment dst src)
  ;; Don't need to print out the width for the LEA instruction
  (:printer reg-reg/mem ((op #b1000110) (width 1) (reg/mem nil :type 'reg/mem)))
  (:emitter
   (assert (dword-reg-p dst))
   (emit-byte segment #b10001101)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cmpxchg (segment dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1011000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (assert (register-p src))
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b10110000 #b10110001))
     (emit-ea segment dst (reg-tn-encoding src)))))


;;;; Flag control instructions.

;;; CLC -- Clear Carry Flag.
;;;
(define-instruction clc (segment)
  (:printer byte ((op #b11111000)))
  (:emitter
   (emit-byte segment #b11111000)))

;;; CLD -- Clear Direction Flag.
;;;
(define-instruction cld (segment)
  (:printer byte ((op #b11111100)))
  (:emitter
   (emit-byte segment #b11111100)))

;;; CLI -- Clear Iterrupt Enable Flag.
;;;
(define-instruction cli (segment)
  (:printer byte ((op #b11111010)))
  (:emitter
   (emit-byte segment #b11111010)))

;;; CMC -- Complement Carry Flag.
;;;
(define-instruction cmc (segment)
  (:printer byte ((op #b11110101)))
  (:emitter
   (emit-byte segment #b11110101)))

;;; LAHF -- Load AH into flags.
;;;
(define-instruction lahf (segment)
  (:printer byte ((op #b10011111)))
  (:emitter
   (emit-byte segment #b10011111)))

;;; POPF -- Pop flags.
;;;
(define-instruction popf (segment)
  (:printer byte ((op #b10011101)))
  (:emitter
   (emit-byte segment #b10011101)))

;;; PUSHF -- push flags.
;;;
(define-instruction pushf (segment)
  (:printer byte ((op #b10011100)))
  (:emitter
   (emit-byte segment #b10011100)))

;;; SAHF -- Store AH into flags.
;;;
(define-instruction sahf (segment)
  (:printer byte ((op #b10011110)))
  (:emitter
   (emit-byte segment #b10011110)))

;;; STC -- Set Carry Flag.
;;;
(define-instruction stc (segment)
  (:printer byte ((op #b11111001)))
  (:emitter
   (emit-byte segment #b11111001)))

;;; STD -- Set Direction Flag.
;;;
(define-instruction std (segment)
  (:printer byte ((op #b11111101)))
  (:emitter
   (emit-byte segment #b11111101)))

;;; STI -- Set Interrupt Enable Flag.
;;;
(define-instruction sti (segment)
  (:printer byte ((op #b11111011)))
  (:emitter
   (emit-byte segment #b11111011)))



;;;; Arithmetic

(defun sign-extend (x n)
  "Sign extend the N-bit number X"
  (if (logbitp (1- n) x)
      (logior (ash -1 (1- n)) x)
      x))

(defun emit-random-arith-inst (name segment dst src opcode
				    &optional allow-constants)
  (let ((size (matching-operand-size dst src)))
    (maybe-emit-operand-size-prefix segment size)
    (cond
     ((integerp src)
      (cond ((accumulator-p dst)
	     (emit-byte segment
			(dpb opcode
			     (byte 3 3)
			     (if (eq size :byte)
				 #b00000100
				 #b00000101)))
	     (emit-sized-immediate segment size src))
	    ((and (not (eq size :byte)) (<= -128 (sign-extend src 32) 127))
	     (emit-byte segment #b10000011)
	     (emit-ea segment dst opcode allow-constants)
	     (emit-byte segment (ldb (byte 8 0) src)))
	    (t
	     (emit-byte segment (if (eq size :byte) #b10000000 #b10000001))
	     (emit-ea segment dst opcode allow-constants)
	     (emit-sized-immediate segment size src))))
     ((register-p src)
      (emit-byte segment
		 (dpb opcode
		      (byte 3 3)
		      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src) allow-constants))
     ((register-p dst)
      (emit-byte segment
		 (dpb opcode
		      (byte 3 3)
		      (if (eq size :byte) #b00000010 #b00000011)))
      (emit-ea segment src (reg-tn-encoding dst) allow-constants))
     (t
      (error "Bogus operands to ~A" name)))))

(defun arith-logical-constant-control (chunk inst stream dstate)
    (declare (ignore inst stream))
    (when (= (ldb (byte 8 0) chunk) #b10000011)
      (let ((imm (sign-extend (ldb (byte 8 16) chunk) 8)))
	(when (minusp imm)
	  (disassem:note #'(lambda (stream)
			     (princ (ldb (byte 32 0) imm) stream))
			 dstate)))))

(eval-when (compile eval)
  (defun arith-inst-printer-list (subop &key logical-op-p)
    `((accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (reg/mem-imm ((op (#b1000000 ,subop))))
      (reg/mem-imm ((op (#b1000001 ,subop))
		    (width 0)))
      (reg/mem-imm ((op (#b1000001 ,subop))
		    (width 1)
		    ,@(when logical-op-p
			`((imm nil :type sign-extended-imm-byte)))))
      (reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))))
  )

(define-instruction add (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b000))
  (:emitter
   (emit-random-arith-inst "ADD" segment dst src #b000)))

(define-instruction adc (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b010))
  (:emitter
   (emit-random-arith-inst "ADC" segment dst src #b010)))

(define-instruction sub (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b101))
  (:emitter
   (emit-random-arith-inst "SUB" segment dst src #b101)))

(define-instruction sbb (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b011))
  (:emitter
   (emit-random-arith-inst "SBB" segment dst src #b011)))

(define-instruction cmp (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b111))
  (:emitter
   (emit-random-arith-inst "CMP" segment dst src #b111 t)))

(define-instruction inc (segment dst)
  ;; Register.
  (:printer reg-no-width ((op #b01000)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((and (not (eq size :byte)) (register-p dst))
	    (emit-byte-with-reg segment #b01000 (reg-tn-encoding dst)))
	   (t
	    (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
	    (emit-ea segment dst #b000))))))

(define-instruction dec (segment dst)
  ;; Register.
  (:printer reg-no-width ((op #b01001)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b001))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((and (not (eq size :byte)) (register-p dst))
	    (emit-byte-with-reg segment #b01001 (reg-tn-encoding dst)))
	   (t
	    (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
	    (emit-ea segment dst #b001))))))

(define-instruction neg (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b011))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b011))))

(define-instruction aaa (segment)
  (:printer byte ((op #b00110111)))
  (:emitter
   (emit-byte segment #b00110111)))

(define-instruction aas (segment)
  (:printer byte ((op #b00111111)))
  (:emitter
   (emit-byte segment #b00111111)))

(define-instruction daa (segment)
  (:printer byte ((op #b00100111)))
  (:emitter
   (emit-byte segment #b00100111)))

(define-instruction das (segment)
  (:printer byte ((op #b00101111)))
  (:emitter
   (emit-byte segment #b00101111)))

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (assert (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

(define-instruction imul (segment dst &optional src1 src2)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer ext-reg-reg/mem ((op #b1010111) (width 1)))
  (:printer reg-reg/mem ((op #b0110100) (width 1) (imm nil :type 'imm-word))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:printer reg-reg/mem ((op #b0110101) (width 1)
			 (imm nil :type 'signed-imm-byte))
	    '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (flet ((r/m-with-immed-to-reg (reg r/m immed)
	    (let* ((size (matching-operand-size reg r/m))
		   (sx (and (not (eq size :byte)) (<= -128 immed 127))))
	      (maybe-emit-operand-size-prefix segment size)
	      (emit-byte segment (if sx #b01101011 #b01101001))
	      (emit-ea segment r/m (reg-tn-encoding reg))
	      (if sx
		  (emit-byte segment immed)
		  (emit-sized-immediate segment size immed)))))
     (cond (src2
	    (r/m-with-immed-to-reg dst src1 src2))
	   (src1
	    (if (integerp src1)
		(r/m-with-immed-to-reg dst dst src1)
		(let ((size (matching-operand-size dst src1)))
		  (maybe-emit-operand-size-prefix segment size)
		  (emit-byte segment #b00001111)
		  (emit-byte segment #b10101111)
		  (emit-ea segment src1 (reg-tn-encoding dst)))))
	   (t
	    (let ((size (operand-size dst)))
	      (maybe-emit-operand-size-prefix segment size)
	      (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
	      (emit-ea segment dst #b101)))))))

(define-instruction div (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b110))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (assert (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b110))))

(define-instruction idiv (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b111))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (assert (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b111))))

(define-instruction aad (segment)
  (:printer two-bytes ((op '(#b11010101 #b00001010))))
  (:emitter
   (emit-byte segment #b11010101)
   (emit-byte segment #b00001010)))

(define-instruction aam (segment)
  (:printer two-bytes ((op '(#b11010100 #b00001010))))
  (:emitter
   (emit-byte segment #b11010100)
   (emit-byte segment #b00001010)))

;;; CBW -- Convert Byte to Word.  AX <- sign_xtnd(AL)
;;;
(define-instruction cbw (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011000)))

;;; CWDE -- Convert Word To Double Word Extened.  EAX <- sign_xtnd(AX)
;;;
(define-instruction cwde (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011000)))

;;; CWD -- Convert Word to Double Word.  DX:AX <- sign_xtnd(AX)
;;;
(define-instruction cwd (segment)
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011001)))

;;; CDQ -- Convert Double Word to Quad Word. EDX:EAX <- sign_xtnd(EAX)
;;; 
(define-instruction cdq (segment)
  (:printer byte ((op #b10011001)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011001)))

(define-instruction xadd (segment dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1100000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (assert (register-p src))
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b11000000 #b11000001))
     (emit-ea segment dst (reg-tn-encoding src)))))


;;;; Logic.

(defun emit-shift-inst (segment dst amount opcode)
  (let ((size (operand-size dst)))
    (maybe-emit-operand-size-prefix segment size)
    (multiple-value-bind
	(major-opcode immed)
	(case amount
	  (:cl (values #b11010010 nil))
	  (1 (values #b11010000 nil))
	  (t (values #b11000000 t)))
      (emit-byte segment
		 (if (eq size :byte) major-opcode (logior major-opcode 1)))
      (emit-ea segment dst opcode)
      (when immed
	(emit-byte segment amount)))))

(eval-when (compile eval)
  (defun shift-inst-printer-list (subop)
    `((reg/mem ((op (#b1101000 ,subop)))
	       (:name :tab reg/mem ", 1"))
      (reg/mem ((op (#b1101001 ,subop)))
	       (:name :tab reg/mem ", " 'cl))
      (reg/mem-imm ((op (#b1100000 ,subop))
		    (imm nil :type signed-imm-byte))))))

(define-instruction rol (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b000))
  (:emitter
   (emit-shift-inst segment dst amount #b000)))

(define-instruction ror (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b001))
  (:emitter
   (emit-shift-inst segment dst amount #b001)))

(define-instruction rcl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b010))
  (:emitter
   (emit-shift-inst segment dst amount #b010)))

(define-instruction rcr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b011))
  (:emitter
   (emit-shift-inst segment dst amount #b011)))

(define-instruction shl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b100))
  (:emitter
   (emit-shift-inst segment dst amount #b100)))

(define-instruction shr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b101))
  (:emitter
   (emit-shift-inst segment dst amount #b101)))

(define-instruction sar (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b111))
  (:emitter
   (emit-shift-inst segment dst amount #b111)))

(defun emit-double-shift (segment opcode dst src amt)
  (let ((size (matching-operand-size dst src)))
    (when (eq size :byte)
      (error "Double shifts cannot be used with byte registers."))
    (maybe-emit-operand-size-prefix segment size)
    (emit-byte segment #b00001111)
    (emit-byte segment (dpb opcode (byte 1 3)
			    (if (eq amt :cl) #b10100101 #b10100100)))
    (emit-ea segment dst (reg-tn-encoding src))
    (unless (eq amt :cl)
      (emit-byte segment amt))))

(eval-when (compile eval)
  (defun double-shift-inst-printer-list (op)
    `((ext-reg-reg/mem-shift ((op ,(logior op #b100))
			      (imm nil :type signed-imm-byte))
         (:name :tab reg/mem ", " reg ", " imm))
      (ext-reg-reg/mem-shift ((op ,(logior op #b101)))
	 (:name :tab reg/mem ", " reg ", " 'cl)))))

(define-instruction shld (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b10100000))
  (:emitter
   (emit-double-shift segment #b0 dst src amt)))

(define-instruction shrd (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b10101000))
  (:emitter
   (emit-double-shift segment #b1 dst src amt)))

(define-instruction and (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b100 :logical-op-p t))
  (:emitter
   (emit-random-arith-inst "AND" segment dst src #b100)))

(define-instruction test (segment this that)
  (:printer accum-imm ((op #b1010100)))
  (:printer reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer reg-reg/mem ((op #b1000010)))
  (:emitter
   (let ((size (matching-operand-size this that)))
     (maybe-emit-operand-size-prefix segment size)
     (flet ((test-immed-and-something (immed something)
	      (cond ((accumulator-p something)
		     (emit-byte segment
				(if (eq size :byte) #b10101000 #b10101001))
		     (emit-sized-immediate segment size immed))
		    (t
		     (emit-byte segment
				(if (eq size :byte) #b11110110 #b11110111))
		     (emit-ea segment something #b000)
		     (emit-sized-immediate segment size immed))))
	    (test-reg-and-something (reg something)
	      (emit-byte segment (if (eq size :byte) #b10000100 #b10000101))
	      (emit-ea segment something (reg-tn-encoding reg))))
       (cond ((integerp that)
	      (test-immed-and-something that this))
	     ((integerp this)
	      (test-immed-and-something this that))
	     ((register-p this)
	      (test-reg-and-something this that))
	     ((register-p that)
	      (test-reg-and-something that this))
	     (t
	      (error "Bogus operands for TEST: ~S and ~S" this that)))))))

(define-instruction or (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b001 :logical-op-p t))
  (:emitter
   (emit-random-arith-inst "OR" segment dst src #b001)))

(define-instruction xor (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b110 :logical-op-p t))
  (:emitter
   (emit-random-arith-inst "XOR" segment dst src #b110)))

(define-instruction not (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b010))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b010))))


;;;; String manipulation.

(disassem:define-instruction-format (string-op 8
				     :include 'simple
				     :default-printer '(:name width)))

(define-instruction cmps (segment size)
  (:printer string-op ((op #b1010011)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (emit-byte segment (if (eq size :byte) #b10100110 #b10100111))))

(define-instruction ins (segment acc)
  (:printer string-op ((op #b0110110)))
  (:emitter
   (let ((size (operand-size acc)))
     (assert (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b01101100 #b01101101)))))

(define-instruction lods (segment acc)
  (:printer string-op ((op #b1010110)))
  (:emitter
   (let ((size (operand-size acc)))
     (assert (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101100 #b10101101)))))

(define-instruction movs (segment size)
  (:printer string-op ((op #b1010010)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (emit-byte segment (if (eq size :byte) #b10100100 #b10100101))))

(define-instruction outs (segment acc)
  (:printer string-op ((op #b0110111)))
  (:emitter
   (let ((size (operand-size acc)))
     (assert (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b01101110 #b01101111)))))

(define-instruction scas (segment acc)
  (:printer string-op ((op #b1010111)))
  (:emitter
   (let ((size (operand-size acc)))
     (assert (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101110 #b10101111)))))

(define-instruction stos (segment acc)
  (:printer string-op ((op #b1010101)))
  (:emitter
   (let ((size (operand-size acc)))
     (assert (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101010 #b10101011)))))

(define-instruction xlat (segment)
  (:printer byte ((op #b11010111)))
  (:emitter
   (emit-byte segment #b11010111)))

(define-instruction rep (segment)
  (:emitter
   (emit-byte segment #b11110010)))

(define-instruction repe (segment)
  (:printer byte ((op #b11110011)))
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repne (segment)
  (:printer byte ((op #b11110010)))
  (:emitter
   (emit-byte segment #b11110010)))


;;;; Bit Manipulation

(define-instruction bsf (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 0)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "Can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111100)
     (emit-ea segment src (reg-tn-encoding dst)))))

(define-instruction bsr (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 1)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "Can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111101)
     (emit-ea segment src (reg-tn-encoding dst)))))

(defun emit-bit-test-and-mumble (segment src index opcode)
  (let ((size (operand-size src)))
    (when (eq size :byte)
      (error "Can't scan bytes: ~S" src))
    (maybe-emit-operand-size-prefix segment size)
    (emit-byte segment #b00001111)
    (cond ((integerp index)
	   (emit-byte segment #b10111010)
	   (emit-ea segment src opcode)
	   (emit-byte segment index))
	  (t
	   (emit-byte segment (dpb opcode (byte 3 3) #b10000011))
	   (emit-ea segment src (reg-tn-encoding index))))))

;; Bit Test (BT) instructions
;; Ignoring the case with a src as a register, we have
;;
;;   0F A3 reg/mem  BT
;;   0F BB reg/mem  BTC
;;   0F B3 reg/mem
;;   0F AB reg/mem
;;
;; A3 = 10100011
;; BB = 10111011
;; B3 = 10110011
;; AB = 10101011
;;
;; So the pattern is 10xxx011
;;
;; The instruction format is then (little-endian)
;;
;; |reg/mem|10ooo011|00001111
(disassem:define-instruction-format
    (bit-test-reg/mem 24
		      :default-printer '(:name :tab reg/mem ", " reg))
  (prefix	:field (byte 8 0)	:value #b0001111)
  (op		:field (byte 8 8))
  ;;(test		:fields (list (byte 2 14) (byte 3 8)))
  (reg/mem	:fields (list (byte 2 22) (byte 3 16))
		:type 'reg/mem)
  (reg		:field (byte 3 19) :type 'reg)
  ;; optional fields
  (imm))

(define-instruction bt (segment src index)
  (:printer bit-test-reg/mem ((op #b10100011)))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b100)))

(define-instruction btc (segment src index)
  (:printer bit-test-reg/mem ((op #b10111011)))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b111)))

(define-instruction btr (segment src index)
  (:printer bit-test-reg/mem ((op #b10110011)))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b110)))

(define-instruction bts (segment src index)
  (:printer bit-test-reg/mem ((op #b10101011)))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b101)))


;;;; Control transfer.

(eval-when (compile load eval)
(defparameter condition-name-vec
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond conditions)
      (when (null (aref vec (cdr cond)))
	(setf (aref vec (cdr cond)) (car cond))))
    vec)))

(disassem:define-argument-type condition-code
  :printer condition-name-vec)

(disassem:define-argument-type displacement
  :sign-extend t
  :use-label #'offset-next
  :printer #'(lambda (value stream dstate)
	       (let ((unsigned-val (if (and (numberp value) (minusp value))
				       (+ value #x100000000)
				       value)))
		 (disassem:maybe-note-assembler-routine unsigned-val stream dstate))
	       (print-label value stream dstate)))

(disassem:define-instruction-format (short-cond-jump 16)
  (op    :field (byte 4 4))
  (cc	 :field (byte 4 0) :type 'condition-code)
  (label :field (byte 8 8) :type 'displacement))

(disassem:define-instruction-format (short-jump 16
				     :default-printer '(:name :tab label))
  (const :field (byte 4 4) :value #b1110)
  (op	 :field (byte 4 0))
  (label :field (byte 8 8) :type 'displacement))

(disassem:define-instruction-format (near-cond-jump 16)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#b00001111 #b1000))
  (cc	 :field (byte 4 8) :type 'condition-code)
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
	 :prefilter #'(lambda (value dstate)
			(declare (ignore value))   ; always nil anyway
			(disassem:read-signed-suffix 32 dstate))))

(disassem:define-instruction-format (near-jump 8
				     :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the address.
  (label :type 'displacement
	 :prefilter #'(lambda (value dstate)
			(declare (ignore value))   ; always nil anyway
			(disassem:read-signed-suffix 32 dstate))))


(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem ((op '(#b1111111 #b010)) (width 1)))
  (:emitter
   (typecase where
     (label
      (emit-byte segment #b11101000)
      (emit-back-patch segment 4
		       #'(lambda (segment posn)
			   (emit-dword segment
				       (- (label-position where)
					  (+ posn 4))))))
     (fixup
      (emit-byte segment #b11101000)
      (emit-relative-fixup segment where))
     (t
      (emit-byte segment #b11111111)
      (emit-ea segment where #b010)))))

(defun emit-byte-displacement-backpatch (segment target)
  (emit-back-patch segment 1
		   #'(lambda (segment posn)
		       (let ((disp (- (label-position target) (1+ posn))))
			 (assert (<= -128 disp 127))
			 (emit-byte segment disp)))))

(define-instruction jmp (segment cond &optional where)
  ;; conditional jumps
  (:printer short-cond-jump ((op #b0111)) '('j cc :tab label))
  (:printer near-cond-jump () '('j cc :tab label))
  ;; unconditional jumps
  (:printer short-jump ((op #b1011)))
  (:printer near-jump ((op #b11101001)) )
  (:printer reg/mem ((op '(#b1111111 #b100)) (width 1)))
  (:emitter
   (cond (where
	  (emit-chooser
	   segment 6 2
	   #'(lambda (segment posn delta-if-after)
	       (let ((disp (- (label-position where posn delta-if-after)
			      (+ posn 2))))
		 (when (<= -128 disp 127)
		       (emit-byte segment
				  (dpb (conditional-opcode cond)
				       (byte 4 0)
				       #b01110000))
		       (emit-byte-displacement-backpatch segment where)
		       t)))
	   #'(lambda (segment posn)
	       (let ((disp (- (label-position where) (+ posn 6))))
		 (emit-byte segment #b00001111)
		 (emit-byte segment
			    (dpb (conditional-opcode cond)
				 (byte 4 0)
				 #b10000000))
		 (emit-dword segment disp)))))
	 ((label-p (setq where cond))
	  (emit-chooser
	   segment 5 0
	   #'(lambda (segment posn delta-if-after)
	       (let ((disp (- (label-position where posn delta-if-after)
			      (+ posn 2))))
		 (when (<= -128 disp 127)
		       (emit-byte segment #b11101011)
		       (emit-byte-displacement-backpatch segment where)
		       t)))
	   #'(lambda (segment posn)
	       (let ((disp (- (label-position where) (+ posn 5))))
		 (emit-byte segment #b11101001)
		 (emit-dword segment disp))
	       )))
	 ((fixup-p where)
	  (emit-byte segment #b11101001)
	  (emit-relative-fixup segment where))
	 (t
	  (unless (or (ea-p where) (tn-p where))
		  (error "Don't know what to do with ~A" where))
	  (emit-byte segment #b11111111)
	  (emit-ea segment where #b100)))))

(define-instruction jmp-short (segment label)
  (:emitter
   (emit-byte segment #b11101011)
   (emit-byte-displacement-backpatch segment label)))

(define-instruction ret (segment &optional stack-delta)
  (:printer byte ((op #b11000011)))
  (:printer byte ((op #b11000010) (imm nil :type 'imm-word-16))
	    '(:name :tab imm))
  (:emitter
   (cond (stack-delta
	  (emit-byte segment #b11000010)
	  (emit-word segment stack-delta))
	 (t
	  (emit-byte segment #b11000011)))))

(define-instruction jecxz (segment target)
  (:printer short-jump ((op #b0011)))
  (:emitter
   (emit-byte segment #b11100011)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loop (segment target)
  (:printer short-jump ((op #b0010)))
  (:emitter
   (emit-byte segment #b11100010)	; pfw this was 11100011, or jecxz!!!!
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopz (segment target)
  (:printer short-jump ((op #b0001)))
  (:emitter
   (emit-byte segment #b11100001)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopnz (segment target)
  (:printer short-jump ((op #b0000)))
  (:emitter
   (emit-byte segment #b11100000)
   (emit-byte-displacement-backpatch segment target)))


;;; Conditional move.

(disassem:define-instruction-format (cond-move 24
				     :default-printer
				        '('cmov cond :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)	:value #b00001111)
  (op      :field (byte 4 12)	:value #b0100)
  (cond    :field (byte 4 8)	:type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   			:type 'reg/mem)
  (reg     :field (byte 3 19)	:type 'reg))

(define-instruction cmov (segment cond dst src)
  (:printer cond-move ())
  (:emitter
   (assert (register-p dst))
   (let ((size (matching-operand-size dst src)))
     (assert (or (eq size :word) (eq size :dword)))
     (maybe-emit-operand-size-prefix segment size))
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b01000000))
   (emit-ea segment src (reg-tn-encoding dst))))


;;;; Conditional byte set.

(disassem:define-instruction-format (cond-set 24
				     :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op    :field (byte 4 12) :value #b1001)
  (cc	 :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   :type 'byte-reg/mem)
  (reg     :field (byte 3 19)	:value #b000))

(define-instruction set (segment dst cond)
  (:printer cond-set ())
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b10010000))
   (emit-ea segment dst #b000)))


;;;; Enter/Leave

(disassem:define-instruction-format (enter-format 32
				     :default-printer '(:name
							:tab disp
							(:unless (:constant 0)
							  ", " level)))
  (op :field (byte 8 0))
  (disp :field (byte 16 8))
  (level :field (byte 8 24)))

(define-instruction enter (segment disp &optional (level 0))
  (:declare (type (unsigned-byte 16) disp)
	    (type (unsigned-byte 8) level))
  (:printer enter-format ((op #b11001000)))
  (:emitter
   (emit-byte segment #b11001000)
   (emit-word segment disp)
   (emit-byte segment level)))

(define-instruction leave (segment)
  (:printer byte ((op #b11001001)))
  (:emitter
   (emit-byte segment #b11001001)))


;;;; Interrupt instructions.

;;; Single byte instruction with an immediate byte argument.
(disassem:define-instruction-format (byte-imm 16
				     :default-printer '(:name :tab code))
 (op :field (byte 8 0))
 (code :field (byte 8 8)))


;; The UD1 instruction.  The mod bits of the mod r/m byte MUST be #b11
;; so that the reg/mem field is actually a register.  This is a hack
;; to allow us to print out the reg/mem reg as a 32-bit reg.
;;
;; While the instruction looks like an ext-reg-reg/mem format with
;; fixed width value of 1, it isn't because we need to disassemble the
;; reg/mem field as a 32-bit reg. ext-reg-reg/mem needs a width prefix
;; byte to specify that, and we definitely don't want that.  Hence,
;; use a special instruction format for the UD1 instruction.
(disassem:define-instruction-format
    (ud1 24 :default-printer '(:name :tab reg ", " reg/mem))
  (prefix    :field (byte 8 0) :value #b00001111)
  (op        :field (byte 8 8) :value #b10111001)
  ;; The mod bits ensure that the reg/mem field is interpreted as a
  ;; register, not memory.
  (reg/mem   :field (byte 3 16) :type 'word-reg)
  (reg	     :field (byte 3 19) :type 'word-reg)
  (mod       :field (byte 2 22) :value #b11))

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (system:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (kernel:copy-from-system-area sap (* byte-bits (1+ offset))
                                         vector (* word-bits
                                                   vector-data-offset)
                                         (* length byte-bits))
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

(defun ud1-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (disassem:note x dstate))))
    (let ((code (ldb (byte 6 16) chunk)))
      (ecase code
	(#.vm:error-trap
	 (nt #.(format nil "Trap ~D: Error trap" vm:error-trap))
	 (disassem:handle-break-args #'snarf-error-junk stream dstate))
	(#.vm:cerror-trap
	 (nt #.(format nil "Trap ~D: Cerror trap" vm:cerror-trap))
	 (disassem:handle-break-args #'snarf-error-junk stream dstate))
	(#.vm:pending-interrupt-trap
	 (nt #.(format nil "Trap ~D: Pending interrupt trap" vm:pending-interrupt-trap)))
	(#.vm:halt-trap
	 (nt #.(format nil "Trap ~D: Halt trap" vm:halt-trap)))
	(#.vm:function-end-breakpoint-trap
	 (nt #.(format nil "Trap ~D: Function end breakpoint trap"
		       vm:function-end-breakpoint-trap)))))))

;; The ud1 instruction where we smash the code (trap type) into the
;; low 6 bits of the mod r/m byte.  The mod bits are set to #b11 to
;; make sure the reg/mem part is interpreted to be a register and not
;; memory.
(define-instruction ud1 (segment code)
  (:declare (type (unsigned-byte 8) code))
  (:printer ud1 ((op #b10111001) (reg nil :type 'word-reg))
	    :default
	    :control #'ud1-control)
  (:emitter
   ;; We should not be using the breakpoint trap with UD1 anymore.
   ;; Breakpoint traps are handled in C now, using plain int3.
   (assert (/= code vm:breakpoint-trap))

   ;; Emit the bytes of the instruction.
   (emit-byte segment #x0f)
   (emit-byte segment #xb9)
   (emit-mod-reg-r/m-byte segment
			  #b11
			  (ldb (byte 3 3) code)
			  (ldb (byte 3 0) code))))

;; Handles both int and int3.  To get int3 you have to say (inst int
;; 3).  But int3 should not be used in Lisp code.  This is mainly so
;; that int3 gets disassembled correctly if a breakpoint has been set
;; in Lisp code.  (But in general the disassembly will be messed up
;; because the following byte will in general be the second byte of
;; some instruction, and not the first byte of an instruction.)
(define-instruction int (segment number)
  (:declare (type (unsigned-byte 8) number))
  (:printer byte-imm ((op #b11001101)))
  (:emitter
   (emit-byte segment #b11001101)
   (emit-byte segment number)))

(define-instruction int3 (segment)
  (:printer byte ((op #b11001100)))
  (:emitter
   (emit-byte segment #b11001100)))

(define-instruction into (segment)
  (:printer byte ((op #b11001110)))
  (:emitter
   (emit-byte segment #b11001110)))

(define-instruction bound (segment reg bounds)
  (:emitter
   (let ((size (matching-operand-size reg bounds)))
     (when (eq size :byte)
       (error "Can't bounds-test bytes: ~S" reg))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b01100010)
     (emit-ea segment bounds (reg-tn-encoding reg)))))

(define-instruction iret (segment)
  (:printer byte ((op #b11001111)))
  (:emitter
   (emit-byte segment #b11001111)))

;; read-time-stamp instruction, that counts executed cycles, present
;; from Pentium onwards
(define-instruction rdtsc (segment)
  (:printer two-bytes ((op '(#x0f #x31))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #x31)))

(define-instruction cpuid (segment)
  (:printer two-bytes ((op '(#x0f #xa2))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #xa2)))


;;;; Processor control

(define-instruction hlt (segment)
  (:printer byte ((op #b11110100)))
  (:emitter
   (emit-byte segment #b11110100)))

(define-instruction nop (segment)
  (:printer byte ((op #b10010000)))
  (:emitter
   (emit-byte segment #b10010000)))

(define-instruction wait (segment)
  (:printer byte ((op #b10011011)))
  (:emitter
   (emit-byte segment #b10011011)))

(define-instruction lock (segment)
  (:printer byte ((op #b11110000)))
  (:emitter
   (emit-byte segment #b11110000)))



;;;; Random hackery

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction word (segment word)
  (:emitter
   (emit-word segment word)))

(define-instruction dword (segment dword)
  (:emitter
   (emit-dword segment dword)))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-dword segment
		  (logior type
			  (ash (+ posn (component-header-length))
			       (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  (:emitter
   (emit-header-data segment function-header-type)))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-header-data segment return-pc-header-type)))

;;; ----------------------------------------------------------------
;;; added by jrd.  fp instructions
;;;

;;;
;;; we treat the single-precision and double-precision variants
;;; as separate instructions
;;;

;;;
;;; load single to st(0)
;;;
(define-instruction fld (segment source)
  (:printer floating-point ((op '(#b001 #b000))))
  (:emitter
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b000)))

;;;
;;; load double to st(0)
;;;
(define-instruction fldd (segment source)
  (:printer floating-point ((op '(#b101 #b000))))
  (:printer floating-point-fp ((op '(#b001 #b000))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011001)
     (emit-byte segment #b11011101))
    (emit-fp-op segment source #b000)))

;;;
;;; load long to st(0)
;;;
(define-instruction fldl (segment source)
  (:printer floating-point ((op '(#b011 #b101))))
  (:emitter
    (emit-byte segment #b11011011)
    (emit-fp-op segment source #b101)))

;;;
;;; store single from st(0)
;;;
(define-instruction fst (segment dest)
  (:printer floating-point ((op '(#b001 #b010))))
  (:emitter 
    (cond ((fp-reg-tn-p dest)
	   (emit-byte segment #b11011101)
	   (emit-fp-op segment dest #b010))
	  (t
	   (emit-byte segment #b11011001)
	   (emit-fp-op segment dest #b010)))))

;;;
;;; store double from st(0)
;;;
(define-instruction fstd (segment dest)
  (:printer floating-point ((op '(#b101 #b010))))
  (:printer floating-point-fp ((op '(#b101 #b010))))
  (:emitter 
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b010))
	 (t
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b010)))))

;;; Arithmetic ops are all done with at least one operand at top of
;;; stack. The other operand is is another register or a 32/64 bit
;;; memory loc.

;;; dtc: I've tried to follow the Intel ASM386 conventions, but note
;;; that these conflict with the Gdb conventions for binops. To reduce
;;; the confusion I've added comments showing the mathamatical
;;; operation and the two syntaxes. By the ASM386 convention the
;;; instruction syntax is:
;;;
;;;      Fop Source
;;; or   Fop Destination, Source
;;;
;;; If only one operand is given then it is the source and the
;;; destination is ST(0). There are reversed forms of the fsub and
;;; fdiv instructions inducated by an 'R' suffix.
;;;
;;; The mathematical operation for the non-reverse form is always:
;;;     destination = destination op source
;;;
;;; For the reversed form it is:
;;;     destination = source op destination
;;;
;;; The instructions below only accept one operand at present which is
;;; usually the source. I've hack in extra instructions to implement
;;; the fops with a ST(i) destination, these have a -sti suffix and
;;; the operand is the destination with the source being ST(0).

;;;
;;; Add single
;;; st(0) = st(0) + memory or st(i)
;;;
(define-instruction fadd (segment source)
  (:printer floating-point ((op '(#b000 #b000))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b000)))

;;;
;;; Add double
;;; st(0) = st(0) + memory or st(i)
;;;
(define-instruction faddd (segment source)
  (:printer floating-point ((op '(#b100 #b000))))
  (:printer floating-point-fp ((op '(#b000 #b000))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b000)))

;;;
;;; Add double destination st(i)
;;; st(i) = st(0) + st(i)
;;;
(define-instruction fadd-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b000)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fadd)
  (:emitter
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b000)))
;;; With pop
(define-instruction faddp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b000)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'faddp)
  (:emitter
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b000)))

;;;
;;; Subtract single
;;; st(0) = st(0) - memory or st(i)
;;;
(define-instruction fsub (segment source)
  (:printer floating-point ((op '(#b000 #b100))))
  (:emitter 
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b100)))

;;;
;;; Subtract single, reverse
;;; st(0) = memory or st(i) - st(0)
;;;
(define-instruction fsubr (segment source)
  (:printer floating-point ((op '(#b000 #b101))))
  (:emitter 
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b101)))

;;;
;;; Subtract double
;;; st(0) = st(0) - memory or st(i)
;;;
(define-instruction fsubd (segment source)
  (:printer floating-point ((op '(#b100 #b100))))
  (:printer floating-point-fp ((op '(#b000 #b100))))
  (:emitter 
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b100)))

;;;
;;; Subtract double, reverse
;;; st(0) = memory or st(i) - st(0)
;;;
(define-instruction fsubrd (segment source)
  (:printer floating-point ((op '(#b100 #b101))))
  (:printer floating-point-fp ((op '(#b000 #b101))))
  (:emitter 
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b101)))

;;;
;;; Subtract double, destination st(i)
;;; st(i) = st(i) - st(0)
;;;
;;; ASM386 syntax: FSUB ST(i), ST
;;; Gdb    syntax: fsubr %st,%st(i)
;;;
(define-instruction fsub-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b101)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fsub)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b101)))
;;; With a pop
(define-instruction fsubp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b101)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fsubp)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b101)))

;;;
;;; Subtract double, reverse, destination st(i)
;;; st(i) = st(0) - st(i)
;;;
;;; ASM386 syntax: FSUBR ST(i), ST
;;; Gdb    syntax: fsub %st,%st(i)
;;;
(define-instruction fsubr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b100)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fsubr)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b100)))
;;; With a pop
(define-instruction fsubrp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b100)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fsubrp)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b100)))


;;;
;;; Multiply single
;;; st(0) = st(0) * memory or st(i)
;;;
(define-instruction fmul (segment source)
  (:printer floating-point ((op '(#b000 #b001))))
  (:emitter 
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b001)))

;;;
;;; Multiply double
;;; st(0) = st(0) * memory or st(i)
;;;
(define-instruction fmuld (segment source)
  (:printer floating-point ((op '(#b100 #b001))))
  (:printer floating-point-fp ((op '(#b000 #b001))))
  (:emitter 
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b001)))

;;;
;;; Multiply double, destination st(i)
;;; st(i) = st(i) * st(0)
;;;
(define-instruction fmul-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b001)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fmul)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b001)))



;;;
;;; Divide single
;;; st(0) = st(0) / memory or st(i)
;;;
(define-instruction fdiv (segment source)
  (:printer floating-point ((op '(#b000 #b110))))
  (:emitter 
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b110)))

;;;
;;; Divide single, reverse
;;; st(0) = memory or st(i) / st(0)
;;;
(define-instruction fdivr (segment source)
  (:printer floating-point ((op '(#b000 #b111))))
  (:emitter 
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b111)))

;;;
;;; Divide double
;;; st(0) = st(0) / memory or st(i)
;;;
(define-instruction fdivd (segment source)
  (:printer floating-point ((op '(#b100 #b110))))
  (:printer floating-point-fp ((op '(#b000 #b110))))
  (:emitter 
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b110)))

;;;
;;; Divide double, reverse
;;; st(0) = memory or st(i) / st(0)
;;;
(define-instruction fdivrd (segment source)
  (:printer floating-point ((op '(#b100 #b111))))
  (:printer floating-point-fp ((op '(#b000 #b111))))
  (:emitter 
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b111)))

;;;
;;; Divide double, destination st(i)
;;; st(i) = st(i) / st(0)
;;;
;;; ASM386 syntax: FDIV ST(i), ST
;;; Gdb    syntax: fdivr %st,%st(i)
;;;
(define-instruction fdiv-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b111)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fdiv)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b111)))

;;;
;;; Divide double, reverse, destination st(i)
;;; st(i) = st(0) / st(i)
;;;
;;; ASM386 syntax: FDIVR ST(i), ST
;;; Gdb    syntax: fdiv %st,%st(i)
;;;
(define-instruction fdivr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b110)))
	    '(:name :tab fp-reg ", " '|ST(0)|)
	    :print-name 'fdivr)
  (:emitter 
   (assert (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b110)))

;;;
;;; exchange fr0 with fr(n).  no double variant
;;;
(define-instruction fxch (segment source)
  (:printer floating-point-fp ((op '(#b001 #b001))))
  (:emitter 
    (unless (and (tn-p source)
		 (eq (sb-name (sc-sb (tn-sc source))) 'float-registers))
      (lisp:break))
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b001)))
;;;
;;;
;;; push 32-bit integer to st0
;;;
(define-instruction fild (segment source)
  (:printer floating-point ((op '(#b011 #b000))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment source #b000)))
;;;
;;; push 64-bit integer to st0
;;;
(define-instruction fildl (segment source)
  (:printer floating-point ((op '(#b111 #b101))))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-fp-op segment source #b101)))
;;;
;;; store 32-bit integer
;;;
(define-instruction fist (segment dest)
  (:printer floating-point ((op '(#b011 #b010))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b010)))
;;;
;;; Store and pop 32-bit integer
;;;
(define-instruction fistp (segment dest)
  (:printer floating-point ((op '(#b011 #b011))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b011)))
;;;
;;; Store and pop 64-bit integer
;;;
(define-instruction fistpl (segment dest)
  (:printer floating-point ((op '(#b111 #b111))))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-fp-op segment dest #b111)))
;;;
;;; store single from st(0) and pop
;;;
(define-instruction fstp (segment dest)
  (:printer floating-point ((op '(#b001 #b011)))
	    '('fstp :tab 'dword " " 'ptr " " reg/mem))
  (:emitter 
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011))
	 (t
	  (emit-byte segment #b11011001)
	  (emit-fp-op segment dest #b011)))))
;;;
;;; store double from st(0) and pop
;;;
(define-instruction fstpd (segment dest)
  (:printer floating-point ((op '(#b101 #b011)))
	    '('fstp :tab 'qword " " 'ptr " " reg/mem))
  (:printer floating-point-fp ((op '(#b101 #b011))))
  (:emitter 
   (cond ((fp-reg-tn-p dest)
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011))
	 (t
	  (emit-byte segment #b11011101)
	  (emit-fp-op segment dest #b011)))))
;;;
;;; store long from st(0) and pop
;;;
(define-instruction fstpl (segment dest)
  (:printer floating-point ((op '(#b011 #b111))))
  (:emitter
    (emit-byte segment #b11011011)
    (emit-fp-op segment dest #b111)))

;;;
;;; decrement stack-top pointer
;;;
(define-instruction fdecstp (segment)
  (:printer floating-point-no ((op #b10110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110110)))
;;;
;;; increment stack-top pointer
;;;
(define-instruction fincstp (segment)
  (:printer floating-point-no ((op #b10111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110111)))
;;;
;;; free fp register
;;;
(define-instruction ffree (segment dest)
  (:printer floating-point-fp ((op '(#b101 #b000))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment dest #b000)))
;;;
;;; Free fp register and pop the stack.
;;;
(define-instruction ffreep (segment dest)
  (:printer floating-point-fp ((op '(#b111 #b000))))
  (:emitter 
   (emit-byte segment #b11011111)
   (emit-fp-op segment dest #b000)))

(define-instruction fabs (segment)
  (:printer floating-point-no ((op #b00001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100001)))

(define-instruction fchs (segment)
  (:printer floating-point-no ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100000)))
  
(define-instruction frndint(segment)
  (:printer floating-point-no ((op #b11100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111100)))

;;;
;;; Initialize NPX
;;;
(define-instruction fninit(segment)
  (:printer floating-point-5 ((op #b00011)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100011)))

;;;
;;; Store Status Word to AX
;;;
(define-instruction fnstsw(segment)
  (:printer floating-point-st ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-byte segment #b11100000)))

;;;
;;; Load Control Word
;;;
;;; src must be a memory location
(define-instruction fldcw(segment src)
  (:printer floating-point ((op '(#b001 #b101))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b101)))

;;;
;;; Store Control Word
;;;
(define-instruction fnstcw(segment dst)
  (:printer floating-point ((op '(#b001 #b111))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b111)))
;;;
;;; Store FP Environment
;;;
(define-instruction fstenv(segment dst)
  (:printer floating-point ((op '(#b001 #b110))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b110)))
;;;
;;; Retore FP Environment
;;;
(define-instruction fldenv(segment src)
  (:printer floating-point ((op '(#b001 #b100))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b100)))
;;;
;;; Save FP State
;;;
(define-instruction fsave(segment dst)
  (:printer floating-point ((op '(#b101 #b110))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment dst #b110)))
;;;
;;; Restore FP State
;;;
(define-instruction frstor (segment src)
  (:printer floating-point ((op '(#b101 #b100))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))
;;;
;;; Clear exceptions
;;;
(define-instruction fnclex (segment)
  (:printer floating-point-5 ((op #b00010)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100010)))

;;;
;;; Comparison
;;;
(define-instruction fcom (segment src)
  (:printer floating-point ((op '(#b000 #b010))))
  (:emitter
   (emit-byte segment #b11011000)
   (emit-fp-op segment src #b010)))

(define-instruction fcomd (segment src)
  (:printer floating-point ((op '(#b100 #b010))))
  (:printer floating-point-fp ((op '(#b000 #b010))))
  (:emitter
   (if (fp-reg-tn-p src)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment src #b010)))

;;; Compare ST1 to ST0, popping the stack twice.
(define-instruction fcompp (segment)
  (:printer floating-point-3 ((op '(#b110 #b011001))))
  (:emitter
   (emit-byte segment #b11011110)
   (emit-byte segment #b11011001)))

;;;
;;; Compare ST(i) to ST0 and update the flags.
;;;
;;; Intel syntax: FCOMI ST, ST(i)
;;;
(define-instruction fcomi (segment src)
  (:printer floating-point-fp ((op '(#b011 #b110))))
  (:emitter
   (assert (fp-reg-tn-p src))
   (emit-byte segment #b11011011)
   (emit-fp-op segment src #b110)))

;;;
;;; Unordered comparison
;;;
(define-instruction fucom (segment src)
  (:printer floating-point-fp ((op '(#b101 #b100))))
  (:emitter
   (assert (fp-reg-tn-p src))
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))
;;;
;;; Unordered compare ST(i) to ST0 and update the flags.
;;;
;;; Intel syntax: FUCOMI ST, ST(i)
;;;
(define-instruction fucomi (segment src)
  (:printer floating-point-fp ((op '(#b011 #b101))))
  (:emitter
   (assert (fp-reg-tn-p src))
   (emit-byte segment #b11011011)
   (emit-fp-op segment src #b101)))

(define-instruction ftst (segment)
  (:printer floating-point-no ((op #b00100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100100)))

;;; Compare and move ST(i) to ST0.
;;;
;;; Intel syntal: FCMOVcc ST, ST(i)
;;;
(define-instruction fcmov (segment cond src)
  #+nil (:printer floating-point ((op '(#b01? #b???))))
  (:emitter
   (assert (fp-reg-tn-p src))
   (emit-byte segment (ecase cond
			((:b :e :be :u) #b11011010)
			((:nb :ne :nbe :nu) #b11011011)))
   (emit-fp-op segment src (ecase cond
			     ((:b :nb) #b000)
			     ((:e :ne) #b000)
			     ((:be :nbe) #b000)
			     ((:u nu) #b000)))))

;;;
;;; 80387 Specials
;;;
;;;
(define-instruction fsqrt (segment)
  (:printer floating-point-no ((op #b11010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111010)))

(define-instruction fscale (segment)
  (:printer floating-point-no ((op #b11101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111101)))

(define-instruction fxtract (segment)
  (:printer floating-point-no ((op #b10100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110100)))

(define-instruction fsin (segment)
  (:printer floating-point-no ((op #b11110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111110)))

(define-instruction fcos (segment)
  (:printer floating-point-no ((op #b11111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111111)))

(define-instruction fprem1 (segment)
  (:printer floating-point-no ((op #b10101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110101)))

(define-instruction fprem (segment)
  (:printer floating-point-no ((op #b11000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111000)))

(define-instruction fxam (segment)
  (:printer floating-point-no ((op #b00101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100101)))

;;; These do push/pop to stack and need special handling
;;; in any VOPs that use them. See the book.

;; st0 <- st1*log2(st0)
(define-instruction fyl2x (segment)	; POPS STACK
  (:printer floating-point-no ((op #b10001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110001)))

(define-instruction fyl2xp1 (segment)
  (:printer floating-point-no ((op #b11001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111001)))

(define-instruction f2xm1 (segment)
  (:printer floating-point-no ((op #b10000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110000)))

(define-instruction fptan (segment)	; st(0) <- 1; st(1) <- tan
  (:printer floating-point-no ((op #b10010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110010)))

(define-instruction fpatan (segment)	; POPS STACK
  (:printer floating-point-no ((op #b10011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110011)))

;;; load constant

(define-instruction fldz (segment)
  (:printer floating-point-no ((op #b01110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101110)))

(define-instruction fld1 (segment)
  (:printer floating-point-no ((op #b01000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101000)))

(define-instruction fldpi (segment)
  (:printer floating-point-no ((op #b01011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101011)))

(define-instruction fldl2t (segment)
  (:printer floating-point-no ((op #b01001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101001)))

(define-instruction fldl2e (segment)
  (:printer floating-point-no ((op #b01010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101010)))

(define-instruction fldlg2 (segment)
  (:printer floating-point-no ((op #b01100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101100)))

(define-instruction fldln2 (segment)
  (:printer floating-point-no ((op #b01101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101101)))


;;; The XMM registers XMM0 - XMM7.
(deftype xmmreg () '(unsigned-byte 3))


;;; XMM registers
(disassem:define-argument-type xmmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-xmmreg)

(disassem:define-argument-type xmmreg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-xmmreg/mem)

(disassem:define-argument-type sized-xmmreg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-xmmreg/mem)



;;; All XMM instructions use an extended opcode (#x0F as the first
;;; opcode byte). Therefore in the following "EXT" in the name of the
;;; instruction formats refers to the formats that have an additional
;;; prefix (#x66, #xF2 or #xF3).

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The size of the operands is implicitly given by the instruction.
(disassem:define-instruction-format (xmm-xmm/mem 24
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg))

(disassem:define-instruction-format (ext-xmm-xmm/mem 32
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

;;; Same as xmm-xmm/mem etc., but with direction bit.

(disassem:define-instruction-format (xmm-xmm/mem-dir 24
				     :include 'xmm-xmm/mem
                                     :default-printer
                                     `(:name
                                        :tab
                                        ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 9))
  (dir     :field (byte 1 16)))

(disassem:define-instruction-format (ext-xmm-xmm/mem-dir 32
                                     :include 'ext-xmm-xmm/mem
                                     :default-printer
                                      `(:name
                                        :tab
                                        ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 17))
  (dir     :field (byte 1 16)))

;;; Instructions having an XMM register as one operand and a general-
;;; -purpose register or a memory location as the other operand.

(disassem:define-instruction-format (ext-xmm-reg/mem 32
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

;;; Instructions having a general-purpose register as one operand and an
;;; XMM register or a memory location as the other operand.

(disassem:define-instruction-format (ext-reg-xmm/mem 32
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

;;; Like ext-reg-xmm/mem, but both are registers
(disassem:define-instruction-format (ext-reg-reg/mem 32
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(disassem:define-instruction-format
    (ext-xmm-xmm/mem-imm 32
			 :include 'ext-xmm-xmm/mem
			 :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (imm :type 'imm-data))

(disassem:define-instruction-format
    (xmm-xmm/mem-imm 24
		     :include 'xmm-xmm/mem
		     :default-printer '(:name :tab reg ", " reg/mem ", " imm))
  (imm :type 'imm-data))

(defun emit-sse-inst (segment dst src prefix opcode &key operand-size)
  (declare (ignore operand-size))
  (when prefix
    (emit-byte segment prefix))
  (emit-byte segment #x0f)
  (emit-byte segment opcode)
  (emit-ea segment src (reg-tn-encoding dst)))

;;; Emit an SSE instruction that has an XMM register as the destination
;;; operand and for which the size of the operands is implicitly given
;;; by the instruction.
(defun emit-regular-sse-inst (segment dst src prefix opcode)
  (assert (xmm-register-p dst))
  (emit-sse-inst segment dst src prefix opcode
                 :operand-size :do-not-set))

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The operand size is implicitly given by the instruction.

(macrolet ((define-regular-sse-inst (name prefix opcode &optional register-only)
             `(define-instruction ,name (segment dst src)
                ,@(if prefix
                      `((:printer ext-xmm-xmm/mem
                                  ((prefix ,prefix) (op ,opcode))))
                      `((:printer xmm-xmm/mem ((op ,opcode)))))
                (:emitter
		 ,(when register-only
		    `(assert (xmm-register-p src)))	
                 (emit-regular-sse-inst segment dst src ,prefix ,opcode)))))
  ;; logical
  (define-regular-sse-inst andpd    #x66 #x54 t)
  (define-regular-sse-inst andps    nil  #x54)
  (define-regular-sse-inst orpd     #x66 #x56 t)
  (define-regular-sse-inst orps     nil  #x56)
  (define-regular-sse-inst xorpd    #x66 #x57 t)
  (define-regular-sse-inst xorps    nil  #x57)
  ;; comparison
  (define-regular-sse-inst comisd   #x66 #x2f)
  (define-regular-sse-inst comiss   nil  #x2f)
  (define-regular-sse-inst ucomisd   #x66 #x2e)
  (define-regular-sse-inst ucomiss   nil  #x2e)
  ;; arithmetic
  (define-regular-sse-inst addsd    #xf2 #x58)
  (define-regular-sse-inst addpd    #x66 #x58 t)
  (define-regular-sse-inst addps    nil  #x58 t)
  (define-regular-sse-inst addss    #xf3 #x58)
  (define-regular-sse-inst divsd    #xf2 #x5e)
  (define-regular-sse-inst divpd    #x66 #x5e t)
  (define-regular-sse-inst divps    nil  #x5e t)
  (define-regular-sse-inst divss    #xf3 #x5e)
  (define-regular-sse-inst mulsd    #xf2 #x59)
  (define-regular-sse-inst mulpd    #x66 #x59 t)
  (define-regular-sse-inst mulps    nil  #x59 t)
  (define-regular-sse-inst mulss    #xf3 #x59)
  (define-regular-sse-inst subsd    #xf2 #x5c)
  (define-regular-sse-inst subpd    #x66 #x5c t)
  (define-regular-sse-inst subps    nil  #x5c t)
  (define-regular-sse-inst subss    #xf3 #x5c)
  (define-regular-sse-inst sqrtsd   #xf2 #x51)
  (define-regular-sse-inst sqrtss   #xf3 #x51)
  ;; SSE3
  (define-regular-sse-inst addsubpd #x66 #xd0)
  
  ;; conversion
  (define-regular-sse-inst cvtsd2ss #xf2 #x5a)
  (define-regular-sse-inst cvtpd2ps #x66 #x5a)
  (define-regular-sse-inst cvtss2sd #xf3 #x5a)
  (define-regular-sse-inst cvtps2pd nil  #x5a)
  (define-regular-sse-inst cvtdq2pd #xf3 #xe6)
  (define-regular-sse-inst cvtdq2ps nil  #x5b)

  ;; Misc

  ;; UNPCKHPD:
  ;; dst[63:0] = dst[127:64];
  ;; dst[127:64] = src[127:64];
  (define-regular-sse-inst unpckhpd #x66 #x15 t)
  ;; UNPCKLPD
  ;; dst[63:0] = dst[63:0]
  ;; dst[127:64] = src[63:0]
  (define-regular-sse-inst unpcklpd #x66 #x14 t)
  (define-regular-sse-inst unpcklps nil  #x14 t)

  ;; PADDQ 64-bit integer add
  (define-regular-sse-inst paddq #x66 #xd4)
  )

(define-instruction popcnt (segment dst src)
  (:printer ext-reg-reg/mem
	    ((prefix #xf3) (op #xb8)))
  (:emitter (emit-sse-inst segment dst src #xf3 #xb8)))

;;; MOVSD, MOVSS
(macrolet ((define-movsd/ss-sse-inst (name prefix op)
             `(define-instruction ,name (segment dst src)
	        ,@(if prefix
		      `((:printer ext-xmm-xmm/mem-dir ((prefix ,prefix)
						       (op ,op))))
		      `((:printer xmm-xmm/mem-dir ((op ,op)))))
                (:emitter
                 (cond ((xmm-register-p dst)
                        (emit-sse-inst segment dst src ,prefix ,(ash op 1)
                                       :operand-size :do-not-set))
                       (t
                        (assert (xmm-register-p src))
                        (emit-sse-inst segment src dst ,prefix ,(1+ (ash op 1))
                                       :operand-size :do-not-set)))))))
  (define-movsd/ss-sse-inst movsd #xf2 #b0001000)
  (define-movsd/ss-sse-inst movss #xf3 #b0001000))


;; MOVHLPS and MOVLHPS are incorrectly disassembled as MOVLPS and
;; MOVHPS (respectively).  I (rtoy) don't know how to fix that;
;; instead. just print a note with the correct instruction name.
(defun movlps-control (chunk inst stream dstate)
  (declare (ignore inst))
  (when stream
    (when (>= (ldb (byte 8 16) chunk) #xc0)
      (disassem:note "MOVHLPS" dstate))))

(defun movhps-control (chunk inst stream dstate)
  (declare (ignore inst))
  (when stream
    (when (>= (ldb (byte 8 16) chunk) #xc0)
      (disassem:note "MOVLHPS" dstate))))


(macrolet ((define-mov-sse-inst (name prefix opcode-from opcode-to
                                      &key force-to-mem reg-reg-name control)
               `(progn
                  ,(when reg-reg-name
                     `(define-instruction ,reg-reg-name (segment dst src)
                        (:emitter
                         (assert (xmm-register-p dst))
                         (assert (xmm-register-p src))
                         (emit-regular-sse-inst segment dst src ,prefix ,opcode-from))))
                  (define-instruction ,name (segment dst src)
                    ,@(if prefix
                          `((:printer ext-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-from))
				      :default
				      :control ,control)
			    #+nil
                            (:printer ext-rex-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-from)))
                            (:printer ext-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg)
				      :control ,control)
			    #+nil
			    (:printer ext-rex-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg)))
                          `((:printer xmm-xmm/mem
                                      ((op ,opcode-from))
				      :default
				      :control ,control)
			    #+nil
                            (:printer rex-xmm-xmm/mem
                                      ((op ,opcode-from)))
                            (:printer xmm-xmm/mem
                                      ((op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg)
				      :control ,control)
			    #+nil
                            (:printer rex-xmm-xmm/mem
                                      ((op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg))))
                    (:emitter
                     (cond ((xmm-register-p dst)
                            ,(when force-to-mem
                               `(assert (not (or (register-p src)
                                               (xmm-register-p src)))))
                            (emit-regular-sse-inst segment dst src ,prefix ,opcode-from))
                           (t
                            (assert (xmm-register-p src))
                            ,(when force-to-mem
                               `(assert (not (or (register-p dst)
                                               (xmm-register-p dst)))))
                            (emit-regular-sse-inst segment src dst ,prefix ,opcode-to))))))))
  ;; direction bit?

  ;; This is useful for moving between xmm registers.  We don't have
  ;; aligned 128-bit objects.
  (define-mov-sse-inst movapd #x66 #x28 #x29)
  (define-mov-sse-inst movaps nil  #x28 #x29)
  (define-mov-sse-inst movdqa #x66 #x6f #x7f)
  (define-mov-sse-inst movdqu #xf3 #x6f #x7f)

  ;; Load/store high part of packed single/double.  Low part untouched.
  (define-mov-sse-inst movhpd #x66 #x16 #x17 :force-to-mem t)
  (define-mov-sse-inst movlpd #x66 #x12 #x13 :force-to-mem t)
  ;; Note: movhps and movlhps have exactly the same encoding.  The
  ;; only difference is that movhps moves between registers and memory
  ;; and movlhps moves between registers.  Same for movlps and movhlps.
  (define-mov-sse-inst movhps nil  #x16 #x17 :reg-reg-name movlhps :control #'movhps-control)
  (define-mov-sse-inst movlps nil  #x12 #x13 :reg-reg-name movhlps :control #'movlps-control)

  ;; We don't enforce it, but movupd should be used for moving to/from
  ;; memory because we 128-bit objects aren't aligned on 128-bit
  ;; boundaries.
  (define-mov-sse-inst movupd #x66 #x10 #x11)
  (define-mov-sse-inst movups nil  #x10 #x11))

;;; MOVQ
(define-instruction movq (segment dst src)
  (:printer ext-xmm-xmm/mem ((prefix #xf3) (op #x7e)))
  (:printer ext-xmm-xmm/mem ((prefix #x66) (op #xd6))
            '(:name :tab reg/mem ", " reg))
  (:emitter
   (emit-sse-inst segment dst src #xf3 #x7e
                         :operand-size :do-not-set)))

;;; MOVDDUP
;;;
;;; Like movsd, but the 64-bit low part is also duplicated to the high
;;; part of the xmm register.

;; SSE3
(define-instruction movddup (segment dst src)
  (:printer ext-xmm-xmm/mem ((prefix #xf2) (op #x12)))
  (:emitter
   (emit-sse-inst segment dst src #xf2 #x12
		  :operand-size :do-not-set)))

;;; SHUFPD
;;;
;;; Shuffle packed double floats.  Basically, the low part of dst is
;;; from either the high or low part of dst.  The high part of dst is
;;; from either the low or high part of src.
;;;
;;; if imm[0] = 0
;;;   then dst[63:0] = dst[63:0]
;;;   else dst[63:0] = dst[127:64]
;;;
;;; if imm[1] = 0
;;;   then dst[127:64] = src[63:0];
;;;   else dst[127:64] = src[127:64];
;;;
;;; To swap high and low parts, use shufpd r r 1.
(define-instruction shufpd (segment dst src imm)
  (:printer ext-xmm-xmm/mem-imm ((prefix #x66) (op #xc6)
				 (imm nil :type 'signed-imm-byte)))
  (:emitter
   ;; Don't support 128-bit memory access
   (assert (xmm-register-p src))
   ;; The immediate value must be zero every except for the least two
   ;; bits.
   (assert (zerop (logandc2 imm #x3)))
   (emit-sse-inst segment dst src #x66 #xc6
		  :operand-size :do-not-set)
   (emit-byte segment imm)))

;; SHUFPS
;;
;; dst[31:0] = imm[1:0] selects one 32-bit word of dst
;; dst[63:32] = imm[3:2] selects one 32-bit word of dst
;; dst[95:64] = imm[5:4] selects one 32-bit word of src
;; dst[127:96] = imm[7:6] selects one 32-bit word of src

(define-instruction shufps (segment dst src imm)
  (:printer xmm-xmm/mem-imm ((op #xc6)
			     (imm nil :type 'signed-imm-byte)))
  (:emitter
   ;; Don't support 128-bit memory access
   (assert (xmm-register-p src))
   ;; The immediate value must be zero every except for the least two
   ;; bits.
   (assert (typep imm '(unsigned-byte 8)))
   (emit-sse-inst segment dst src nil #xc6
		  :operand-size :do-not-set)
   (emit-byte segment imm)))

;;; Instructions having an XMM register as the destination operand
;;; and a general-purpose register or a memory location as the source
;;; operand. The operand size is calculated from the source operand.

;;; MOVD - Move a 32- or 64-bit value from a general-purpose register or
;;; a memory location to the low order 32 or 64 bits of an XMM register
;;; with zero extension or vice versa.
;;; We do not support the MMX version of this instruction.
(define-instruction movd (segment dst src)
  (:printer ext-xmm-reg/mem ((prefix #x66) (op #x6e)))
  (:printer ext-xmm-reg/mem ((prefix #x66) (op #x7e))
            '(:name :tab reg/mem ", " reg))
  (:emitter
   (cond ((xmm-register-p dst)
          (emit-sse-inst segment dst src #x66 #x6e))
         (t
          (assert (xmm-register-p src))
          (emit-sse-inst segment src dst #x66 #x7e)))))


(macrolet ((define-integer-source-sse-inst (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                (:printer ext-xmm-reg/mem ((prefix ,prefix) (op ,opcode)))
                (:emitter
                 (assert (xmm-register-p dst))
                 (let ((src-size (operand-size src)))
                   (assert (or (eq src-size :qword) (eq src-size :dword))))
                 (emit-sse-inst segment dst src ,prefix ,opcode)))))
  (define-integer-source-sse-inst cvtsi2sd #xf2 #x2a)
  (define-integer-source-sse-inst cvtsi2ss #xf3 #x2a))

;;; Instructions having a general-purpose register as the destination
;;; operand and an XMM register or a memory location as the source
;;; operand. The operand size is calculated from the destination
;;; operand.

(macrolet ((define-gpr-destination-sse-inst (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                (:printer ext-reg-xmm/mem ((prefix ,prefix) (op ,opcode)))
                (:emitter
                 (assert (register-p dst))
                 (let ((dst-size (operand-size dst)))
                   (assert (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-sse-inst segment dst src ,prefix ,opcode
                                  :operand-size dst-size))))))
  (define-gpr-destination-sse-inst cvtsd2si  #xf2 #x2d)
  (define-gpr-destination-sse-inst cvtss2si  #xf3 #x2d)
  (define-gpr-destination-sse-inst cvttsd2si #xf2 #x2c)
  (define-gpr-destination-sse-inst cvttss2si #xf3 #x2c))

;;; Other SSE instructions

;; Like ext-reg/mem, but we don't need a size printed out.
(disassem:define-instruction-format (ext-reg/mem-no-size 24
				     :include 'ext-reg/mem
				     :default-printer `(:name :tab reg/mem))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
	   :type 'reg/mem))

(define-instruction ldmxcsr (segment src)
  (:printer ext-reg/mem-no-size ((width 0) (op '(#b1010111 2))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment src 2)))

(define-instruction stmxcsr (segment dst)
  (:printer ext-reg/mem-no-size ((width 0) (op '(#b1010111 3))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment dst 3)))

(macrolet
    ((packed-cmp (name opcode)
       `(define-instruction ,name (segment dst src)
	  (:printer ext-xmm-xmm/mem
		    ((prefix #x66) (op ,opcode)))
	  (:printer xmm-xmm/mem ((op ,opcode)))
	  (:emitter
	   ;; We don't support the case where the src is a 128-bit
	   ;; memory operand.
	   (let ((prefix (if (xmm-register-p src)
			     #x66
			     nil)))
	     (emit-regular-sse-inst segment dst src prefix ,opcode))))))
  (packed-cmp pcmpeqb #x74)
  (packed-cmp pcmpeqw #x75)
  (packed-cmp pcmpeqd #x76))

(disassem:define-instruction-format (ext-ext-xmm-xmm/mem 40
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)  :value #x66)
  (x0f     :field (byte 8 8)  :value #x0f)
  (x38     :field (byte 8 16) :value #x38)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg))


;; This might be an sse3 instruction?  In any case, an Opteron doesn't
;; seem to have it.
#+nil
(define-instruction pcmpeqq (segment dst src)
  (:printer ext-ext-xmm-xmm/mem ((prefix #x66)
				 (x0f #x0f)
				 (x38 #x38)
				 (op #x29)))
  (:emitter
   (emit-byte segment #x66)
   (emit-byte segment #x0f)
   (emit-byte segment #x38)
   (emit-byte segment #x29)
   (emit-ea segment src (reg-tn-encoding dst))))

(disassem:define-instruction-format (ext-xmm-mem 32
                                     :default-printer
                                     '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)  :value #x66)
  (x0f     :field (byte 8 8)  :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
	   :type 'xmmreg/mem)
  (reg     :field (byte 3 27))
  (imm))

(macrolet
    ((packed-shift (name imm-op reg-op reg)
       ;; We don't support the MMX version.
       `(define-instruction ,name (segment dst src)
	  (:declare (type (satisfies xmm-register-p) dst)
		    (type (or fixnum (satisfies xmm-register-p)) src))
	  (:printer ext-xmm-mem ((prefix #x66) (op ,reg-op)))
	  (:printer ext-xmm-mem ((prefix #x66) (op ,imm-op)
				 (reg ,reg)
				 (imm nil :type 'signed-imm-byte))
		    '(:name :tab reg/mem ", " imm))
	  (:emitter
	   (cond ((fixnump src)
		  (emit-byte segment #x66)
		  (emit-byte segment #x0f)
		  (emit-byte segment ,imm-op)
		  (emit-mod-reg-r/m-byte segment #b11 ,reg (reg-tn-encoding dst))
		  (emit-byte segment src))
		 (t
		  (emit-regular-sse-inst segment dst src #x66 ,reg-op)))))))
  (packed-shift psrlq #x73 #xd3 2)
  (packed-shift psrld #x72 #xd2 2)
  (packed-shift psrlw #x71 #xd1 2)
  (packed-shift psllq #x73 #xf3 6)
  (packed-shift pslld #x72 #xf2 6)
  (packed-shift psllw #x71 #xf1 6)
  (packed-shift psrad #x72 #xe2 4)
  (packed-shift psraw #x71 #xe1 4))
