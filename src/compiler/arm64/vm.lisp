;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/vm.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the VM definition for the ARM64 (AArch64) architecture.
;;;
;;; Written by [ARM64 port contributors].
;;; Derived from the SPARC and PPC ports.
;;;
;;; AArch64 has 31 general-purpose 64-bit integer registers (X0-X30),
;;; plus X31 which is context-dependent: in data-processing and most
;;; other instruction positions it reads as zero (XZR); in load/store
;;; base-register and add/sub SP-arithmetic positions it is the hardware
;;; stack pointer (SP).  There are 32 floating-point/SIMD registers
;;; (V0-V31), accessed as Bn/Hn/Sn/Dn/Qn for 8/16/32/64/128-bit widths.
;;;
;;;
;;; Register usage:
;;;   X0-X7    nl0-nl7   Non-descriptor (C argument/return) registers.
;;;                      Caller-saved in both the C ABI and Lisp.
;;;   X8       nargs     Number of (Lisp) arguments passed.
;;;   X9       cfunc     C function address (for alien calls).
;;;   X10      nfp       Number-stack frame pointer.
;;;   X11      bsp       Binding stack pointer.
;;;   X12      cfp       Control frame pointer.
;;;   X13      csp       Control stack pointer.
;;;   X14      alloc     Allocation pointer (heap frontier).
;;;   X15      null      NIL / null register.
;;;   X16      code      Current code object (C ABI: ip0).
;;;   X17      fdefn     Function definition (C ABI: ip1; scratch, suits call-target use).
;;;   X18      cname     Called name / function name.
;;;   X19      lexenv    Lexical environment.
;;;   X20      ocfp      Old control frame pointer.
;;;   X21      lra       Lisp return address.
;;;   X22      a0        Argument 0.
;;;   X23      a1        Argument 1.
;;;   X24      a2        Argument 2.
;;;   X25      a3        Argument 3.
;;;   X26      l0        Local 0.
;;;   X27      l1        Local 1.
;;;   X28      gtemp     Global temporary (reserved, not available to Lisp).
;;;   X29      lip       Lisp interior pointer (C ABI FP; reclaimed for Lisp use).
;;;   X30      lr        Hardware link register.
;;;   X31/XZR  zero      Zero register (data-processing context).
;;;   X31/SP   nsp       Native (C) stack pointer (load/store / add-SP context).

(in-package "ARM64")


;;;; Define the registers

(eval-when (:compile-toplevel :execute)

(defmacro defreg (name offset)
  (let ((offset-sym (symbolicate name "-OFFSET")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,offset-sym ,offset)
       (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))



(defmacro defregset (name &rest regs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,name
       (list ,@(mapcar #'(lambda (name) (symbolicate name "-OFFSET")) regs)))))

) ; eval-when (:compile-toplevel :execute)


(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *register-names* (make-array 32 :initial-element nil))

) ; eval-when (:compile-toplevel :load-toplevel :execute)


;;; Non-descriptor (C argument/scratch) registers: X0-X7.
;;; Caller-saved in both the C ABI and Lisp; hold raw untagged values.
(defreg nl0 0)                          ; X0  - C arg 0 / return value
(defreg nl1 1)                          ; X1  - C arg 1
(defreg nl2 2)                          ; X2  - C arg 2
(defreg nl3 3)                          ; X3  - C arg 3
(defreg nl4 4)                          ; X4  - C arg 4
(defreg nl5 5)                          ; X5  - C arg 5
(defreg nl6 6)                          ; X6  - C arg 6
(defreg nl7 7)                          ; X7  - C arg 7

;;; Runtime state registers (non-descriptor range).
(defreg nargs 8)                        ; X8  - number of arguments
(defreg cfunc 9)                        ; X9  - C function address
(defreg nfp 10)                         ; X10 - number-stack frame pointer
(defreg bsp 11)                         ; X11 - binding stack pointer
(defreg cfp 12)                         ; X12 - control frame pointer
(defreg csp 13)                         ; X13 - control stack pointer
(defreg alloc 14)                       ; X14 - allocation pointer
(defreg null 15)                        ; X15 - NIL / null register

;;; Code and call-target registers.
(defreg code 16)                        ; X16 - current code object (C: ip0)
(defreg fdefn 17)                       ; X17 - function definition (C: ip1)

;;; Descriptor (Lisp object) registers.
(defreg cname 18)                       ; X18 - called name
(defreg lexenv 19)                      ; X19 - lexical environment
(defreg ocfp 20)                        ; X20 - old control frame pointer
(defreg lra 21)                         ; X21 - lisp return address

;;; Argument registers (descriptor).
(defreg a0 22)                          ; X22 - argument 0
(defreg a1 23)                          ; X23 - argument 1
(defreg a2 24)                          ; X24 - argument 2
(defreg a3 25)                          ; X25 - argument 3

;;; Local (descriptor) registers.
(defreg l0 26)                          ; X26 - local 0
(defreg l1 27)                          ; X27 - local 1

;;; NOTE: We need an otherwise unused register for the define-move-function
;;; functions so we can access things where the offset of the object is too
;;; large to fit in the offset part of an instruction.  It needs to be fixed
;;; because these move functions don't have any other args to use.  And since
;;; the register allocator doesn't know about such uses, we can't use it for
;;; anything else.
(defreg gtemp 28)                       ; X28 - global temp (reserved, not for Lisp use)

;;; Interior pointer and ABI registers.
(defreg lip 29)                         ; X29 - lisp interior pointer (C ABI FP, reclaimed)

;;; ABI link register; named for disassembly output.
(defreg lr 30)                          ; X30 - hardware link register

;;; X31 is context-dependent in AArch64:
;;;   - In data-processing encodings:       XZR (zero register), reads as 0.
;;;   - In load/store base and SP-arith:    SP  (stack pointer).
;;; Both zero-tn and nsp-tn are wired to offset 31.  The instruction
;;; emitter selects XZR vs SP encoding based on which TN it sees.
(defconstant zero-offset 31)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (svref *register-names* 31) "ZR/SP"))
(defreg nsp 31)                         ; X31/SP - native stack pointer



(defregset non-descriptor-regs
  nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 cfunc nargs nfp)

(defregset descriptor-regs
  fdefn a0 a1 a2 a3 ocfp lra cname lexenv l0 l1)

(defregset register-arg-offsets
  a0 a1 a2 a3)


;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;;
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
              (let* ((class (car classes))
                     (sc-name (car class))
                     (constant-name (intern (concatenate 'simple-string
                                                         (string sc-name)
                                                         "-SC-NUMBER"))))
                (list* `(define-storage-class ,sc-name ,index
                           ,@(cdr class))
                       `(defconstant ,constant-name ,index)
                       `(export ',constant-name)
                       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

  ;; Non-immediate constants in the constant pool.
  (constant constant)

  ;; ZERO and NULL are in registers.
  (zero immediate-constant)
  (null immediate-constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)


  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC.)
  (control-stack control-stack)

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack)           ; (signed-byte 64)
  (unsigned-stack non-descriptor-stack)         ; (unsigned-byte 64)
  (base-char-stack non-descriptor-stack)        ; non-descriptor characters
  (sap-stack non-descriptor-stack)              ; system area pointers
  (single-stack non-descriptor-stack)           ; single-floats
  (double-stack non-descriptor-stack
                :element-size 2 :alignment 2)   ; double-floats
  #+double-double
  (double-double-stack non-descriptor-stack :element-size 4 :alignment 2)
  (complex-single-stack non-descriptor-stack :element-size 2)
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)
  #+double-double
  (complex-double-double-stack non-descriptor-stack :element-size 8 :alignment 4)


  ;; **** Things that can go in the integer registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc.)
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters.
  (base-char-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (base-char-stack))

  ;; Non-Descriptor SAPs (arbitrary pointers into address space).
  (sap-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor 64-bit (signed or unsigned) numbers.
  (signed-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (signed-stack))

  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.
  ;;
  ;; AArch64 has 32 fully orthogonal FP/SIMD registers V0-V31.
  ;; Single and double registers occupy the same physical file with no
  ;; pairing constraint between Sn and Dn (unlike SPARC).

  ;; Non-Descriptor single-floats (Sn, 32-bit view of Vn).
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats (Dn, 64-bit view of Vn).
  (double-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  ;; Non-descriptor double-double floats (two consecutive Dn registers).
  #+double-double
  (double-double-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-double-stack))

  ;; Complex single-float: pair of consecutive Sn registers.
  (complex-single-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  ;; Complex double-float: pair of consecutive Dn registers.
  (complex-double-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  #+double-double
  (complex-double-double-reg float-registers
   :locations #.(loop for i from 0 to 28 by 4 collect i)
   :element-size 4 :alignment 4
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-double-stack))

  ;; A catch or unwind block.
  (catch-block control-stack :element-size vm:catch-block-size))


;;;; Make some random TNs for important registers.

(eval-when (:compile-toplevel :execute)

(defmacro defregtn (name sc)
  (let ((offset-sym (symbolicate name "-OFFSET"))
        (tn-sym (symbolicate name "-TN")))
    `(defparameter ,tn-sym
       (make-random-tn :kind :normal
                       :sc (sc-or-lose ',sc)
                       :offset ,offset-sym))))

) ; eval-when (:compile-toplevel :execute)

;;; zero-tn and nsp-tn are both wired to offset 31.  The instruction
;;; emitter uses context to select XZR vs SP encodings: nsp-tn appears
;;; as a load/store base or SP-arithmetic operand; zero-tn appears
;;; everywhere else.
(defregtn zero any-reg)
(defregtn nsp any-reg)
(defregtn lip interior-reg)
(defregtn null descriptor-reg)
(defregtn code descriptor-reg)
(defregtn alloc any-reg)
(defregtn gtemp any-reg)

(defregtn nargs any-reg)
(defregtn bsp any-reg)
(defregtn csp any-reg)
(defregtn cfp any-reg)
(defregtn ocfp any-reg)
(defregtn nfp any-reg)
(defregtn cfunc any-reg)

(defregtn lexenv descriptor-reg)
(defregtn cname descriptor-reg)
(defregtn lra descriptor-reg)



;;; Immediate-Constant-SC  --  Interface
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero *backend*))
    (null
     (sc-number-or-lose 'null *backend*))
    ((or fixnum system-area-pointer character)
     (sc-number-or-lose 'immediate *backend*))
    (symbol
     (if (static-symbol-p value)
         (sc-number-or-lose 'immediate *backend*)
         nil))))


;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
(defconstant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(defconstant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(defconstant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Offsets of special stack frame locations.
(defconstant ocfp-save-offset 0)
(defconstant lra-save-offset 1)
(defconstant nfp-save-offset 2)

;;; The number of arguments/return values passed in registers.
;;; AArch64 uses four Lisp argument registers: a0-a3 (X22-X25).
;;;
(defconstant register-arg-count 4)

;;; Names to use for the argument registers.
;;;
(defconstant register-arg-names '(a0 a1 a2 a3))

) ; eval-when (:compile-toplevel :load-toplevel :execute)


;;; A list of TNs describing the register arguments.
;;;
(defparameter register-arg-tns
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          register-arg-offsets))

(export 'single-value-return-byte-offset)

;;; SINGLE-VALUE-RETURN-BYTE-OFFSET
;;;
;;; This is used by the debugger.  On AArch64, as on PPC and SPARC,
;;; a single-value return jumps to LRA+8 (skipping the LRA header word
;;; and the two-instruction return sequence in the multiple-value case).
;;;
(defconstant single-value-return-byte-offset 8)


;;; LOCATION-PRINT-NAME  --  Interface
;;;
;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
                     (format nil "X~D" offset)))
      (float-registers (format nil "D~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
