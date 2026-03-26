;;; -*- Package: ARM64 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/compiler/arm64/float.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains floating point support for the AArch64 (ARM64)
;;; backend.
;;;
;;; Ported from the SPARC float.lisp by [your name here].
;;; Complex-float and double-double support follow the SPARC structure.
;;;
;;; Key differences from SPARC:
;;;   - AArch64 has 32 fully orthogonal FP/SIMD registers V0-V31.
;;;     Single-float uses the Sn view; double-float uses the Dn view.
;;;     There are no paired-register constraints (unlike SPARC FP pairs).
;;;   - Stack loads/stores use LDUR/STUR with an unscaled signed-byte-9
;;;     byte offset, matching the loadw/storew macro convention used
;;;     throughout the ARM64 backend.  The SPARC LDF/STDF pair is replaced
;;;     by a single LDUR/STUR accessing the Dn (64-bit) view of Vn.
;;;   - Double-register copies use FMOV Dd, Dn (always available).
;;;     No V9 feature guard is needed.
;;;   - Arithmetic: FADD/FSUB/FMUL/FDIV/FSQRT/FABS/FNEG dispatch on the
;;;     register SC (single-reg vs double-reg) to select S or D suffix.
;;;   - Comparison: FCMP sets the FPSR condition flags; the subsequent
;;;     conditional branch uses the ordinary Bcc instruction.  No
;;;     branch-delay slot, no NOP required.
;;;   - Integer<->float conversion:
;;;       integer -> float : SCVTF / UCVTF (no memory round-trip needed).
;;;       float -> integer : FCVTZS / FCVTZU (round toward zero).
;;;   - Float rounding modes: FRINTZ (toward zero), FRINTN (nearest),
;;;     FRINTP (toward +inf), FRINTM (toward -inf).
;;;   - FP control/status: read via MRS Xt, FPCR/FPSR;
;;;                        write via MSR FPCR/FPSR, Xt.
;;;   - No long-float (quad-precision) support is included here; it would
;;;     require software emulation or a future FEAT_FP16 extension path.
;;;   - No #+double-double support differs from SPARC: the SPARC backend
;;;     exploited paired double registers; on ARM64 we use two independent
;;;     Dn registers accessed by offset helpers identical to the SPARC
;;;     pattern.

(in-package "ARM64")
(intl:textdomain "cmucl-arm64-vm")


;;;; -----------------------------------------------------------------------
;;;; Helper: fp-reg-tn-encoding
;;;;
;;;; Both single-reg and double-reg TNs share the same physical register
;;;; file (V0-V31).  tn-offset gives the register number directly.
;;;; -----------------------------------------------------------------------

(declaim (inline fp-reg-tn-encoding))
(defun fp-reg-tn-encoding (tn)
  (sc-case tn
    ((single-reg double-reg
      complex-single-reg complex-double-reg
      #+double-double double-double-reg
      #+double-double complex-double-double-reg)
     (tn-offset tn))))


;;;; -----------------------------------------------------------------------
;;;; Complex-float component TN accessors
;;;;
;;;; On ARM64, complex-single-reg uses two consecutive Sn registers
;;;; (real=offset, imag=offset+1).  Complex-double-reg uses two consecutive
;;;; Dn registers (real=offset, imag=offset+1 in the double-reg SC, which
;;;; means the physical V register number differs by 1).
;;;; -----------------------------------------------------------------------

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg *backend*)
                  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (1+ (tn-offset x))))

#+double-double
(progn
(defun double-double-reg-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (tn-offset x)))
(defun double-double-reg-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (1+ (tn-offset x))))

(defun complex-double-double-reg-real-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (tn-offset x)))
(defun complex-double-double-reg-real-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (+ 1 (tn-offset x))))
(defun complex-double-double-reg-imag-hi-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (+ 2 (tn-offset x))))
(defun complex-double-double-reg-imag-lo-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg *backend*)
                  :offset (+ 3 (tn-offset x))))
) ; #+double-double progn


;;;; -----------------------------------------------------------------------
;;;; Move functions: stack <-> register
;;;;
;;;; SPARC used LDF/STF (32-bit) and LDDF/STDF (64-bit).
;;;; ARM64: LDUR Sr, [Xn, #off] / STUR Sr, [Xn, #off]  (32-bit, single)
;;;;        LDUR Dr, [Xn, #off] / STUR Dr, [Xn, #off]  (64-bit, double)
;;;; Offsets are unscaled byte offsets; ldur/stur accept a signed-byte-9
;;;; immediate directly (no memory-ref wrapper needed).
;;;; -----------------------------------------------------------------------

(define-move-function (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst ldur y (current-nfp-tn vop) (* (tn-offset x) vm:word-bytes)))

(define-move-function (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst stur x (current-nfp-tn vop) (* (tn-offset y) vm:word-bytes)))


(define-move-function (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset x) vm:word-bytes)))
    (inst ldur y nfp offset)))           ; 64-bit LDUR into Dn

(define-move-function (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset y) vm:word-bytes)))
    (inst stur x nfp offset)))           ; 64-bit STUR from Dn


;;;; -----------------------------------------------------------------------
;;;; Complex float stack <-> register move functions
;;;; -----------------------------------------------------------------------

(define-move-function (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldur real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldur imag-tn nfp (+ offset vm:word-bytes)))))

(define-move-function (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stur real-tn nfp offset))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stur imag-tn nfp (+ offset vm:word-bytes)))))


(define-move-function (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset x) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst ldur real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst ldur imag-tn nfp (+ offset (* 2 vm:word-bytes))))))

(define-move-function (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset y) vm:word-bytes)))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stur real-tn nfp offset))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stur imag-tn nfp (+ offset (* 2 vm:word-bytes))))))


#+double-double
(progn
(define-move-function (load-complex-double-double 4) (vop x y)
  ((complex-double-double-stack) (complex-double-double-reg))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset x) vm:word-bytes)))
    (inst ldur (complex-double-double-reg-real-hi-tn y) nfp offset)
    (inst ldur (complex-double-double-reg-real-lo-tn y) nfp (+ offset (* 1 vm:word-bytes)))
    (inst ldur (complex-double-double-reg-imag-hi-tn y) nfp (+ offset (* 2 vm:word-bytes)))
    (inst ldur (complex-double-double-reg-imag-lo-tn y) nfp (+ offset (* 3 vm:word-bytes)))))

(define-move-function (store-complex-double-double 4) (vop x y)
  ((complex-double-double-reg) (complex-double-double-stack))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset y) vm:word-bytes)))
    (inst stur (complex-double-double-reg-real-hi-tn x) nfp offset)
    (inst stur (complex-double-double-reg-real-lo-tn x) nfp (+ offset (* 1 vm:word-bytes)))
    (inst stur (complex-double-double-reg-imag-hi-tn x) nfp (+ offset (* 2 vm:word-bytes)))
    (inst stur (complex-double-double-reg-imag-lo-tn x) nfp (+ offset (* 3 vm:word-bytes)))))
) ; #+double-double progn


;;;; -----------------------------------------------------------------------
;;;; Register-to-register move helpers
;;;;
;;;; On SPARC, double-register copy required a V9 feature check because
;;;; FMOVD was not available on earlier cores.  On ARM64 FMOV Dd,Dn is
;;;; always available; no feature guard is needed.
;;;; -----------------------------------------------------------------------

;;; Copy a single-precision FP register.
(defun move-single-reg (dst src)
  (unless (location= dst src)
    (inst fmov dst src)))

;;; Copy a double-precision FP register.
(defun move-double-reg (dst src)
  (unless (location= dst src)
    (inst fmov dst src)))


;;;; -----------------------------------------------------------------------
;;;; Float Move VOPs (register <-> register)
;;;; -----------------------------------------------------------------------

(macrolet ((frob (vop sc move-fn)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note _N"float move")
                  (:generator 0
                    (emit-not-implemented)
                    (,move-fn y x)))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg move-single-reg)
  (frob double-move double-reg move-double-reg))


;;;; -----------------------------------------------------------------------
;;;; Move float value to/from heap-allocated descriptor
;;;;
;;;; SPARC used STF/STDF with an other-pointer-type adjusted offset.
;;;; ARM64 does likewise, using STR with the same offset arithmetic.
;;;; -----------------------------------------------------------------------

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note _N"float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:variant-vars format size type data)
  (:generator 13
    (emit-not-implemented)
    (with-fixed-allocation (y ndescr type size))
    (ecase format
      (:single
       (inst stur x y (- (* data vm:word-bytes) vm:other-pointer-type)))
      (:double
       (inst stur x y (- (* data vm:word-bytes) vm:other-pointer-type))))))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg :single
    vm:single-float-size vm:single-float-type vm:single-float-value-slot)
  (frob move-from-double double-reg :double
    vm:double-float-size vm:double-float-type vm:double-float-value-slot))


(macrolet ((frob (name sc format value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note _N"pointer to float coercion")
                  (:generator 2
                    (emit-not-implemented)
                    (inst ldur y x
                          (- (* ,value vm:word-bytes) vm:other-pointer-type))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg :single vm:single-float-value-slot)
  (frob move-to-double double-reg :double vm:double-float-value-slot))


;;;; -----------------------------------------------------------------------
;;;; Float move-argument VOPs
;;;;
;;;; When the destination is on the stack, use STR.
;;;; When the destination is a register, use FMOV (or identity).
;;;; -----------------------------------------------------------------------

(macrolet ((frob (name sc stack-sc)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note _N"float argument move")
                  (:generator 1
                    (emit-not-implemented)
                    (sc-case y
                      (,sc
                       (unless (location= x y)
                         (inst fmov y x)))
                      (,stack-sc
                       (inst stur x nfp (* (tn-offset y) vm:word-bytes))))))
                (define-move-vop ,name :move-argument
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-argument single-reg single-stack)
  (frob move-double-float-argument double-reg double-stack))


;;;; -----------------------------------------------------------------------
;;;; Complex float register-to-register move VOPs
;;;; -----------------------------------------------------------------------

(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg)
               :load-if (not (location= x y))))
  (:note _N"complex single float move")
  (:generator 0
    (emit-not-implemented)
    (unless (location= x y)
      (let ((x-real (complex-single-reg-real-tn x))
            (y-real (complex-single-reg-real-tn y)))
        (inst fmov y-real x-real))
      (let ((x-imag (complex-single-reg-imag-tn x))
            (y-imag (complex-single-reg-imag-tn y)))
        (inst fmov y-imag x-imag)))))
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg)
               :load-if (not (location= x y))))
  (:note _N"complex double float move")
  (:generator 0
    (emit-not-implemented)
    (unless (location= x y)
      (move-double-reg (complex-double-reg-real-tn y)
                       (complex-double-reg-real-tn x))
      (move-double-reg (complex-double-reg-imag-tn y)
                       (complex-double-reg-imag-tn x)))))
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))

#+double-double
(define-vop (complex-double-double-move)
  (:args (x :scs (complex-double-double-reg)
            :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-double-double-reg)
               :load-if (not (location= x y))))
  (:note _N"complex double-double float move")
  (:generator 0
    (emit-not-implemented)
    (unless (location= x y)
      (move-double-reg (complex-double-double-reg-real-hi-tn y)
                       (complex-double-double-reg-real-hi-tn x))
      (move-double-reg (complex-double-double-reg-real-lo-tn y)
                       (complex-double-double-reg-real-lo-tn x))
      (move-double-reg (complex-double-double-reg-imag-hi-tn y)
                       (complex-double-double-reg-imag-hi-tn x))
      (move-double-reg (complex-double-double-reg-imag-lo-tn y)
                       (complex-double-double-reg-imag-lo-tn x)))))
#+double-double
(define-move-vop complex-double-double-move :move
  (complex-double-double-reg) (complex-double-double-reg))


;;;; -----------------------------------------------------------------------
;;;; Complex float heap coercions
;;;; -----------------------------------------------------------------------

(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex single float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 13
    (emit-not-implemented)
    (with-fixed-allocation (y ndescr vm:complex-single-float-type
                              vm:complex-single-float-size))
    (let ((real-tn (complex-single-reg-real-tn x)))
      (inst stur real-tn y
            (- (* vm:complex-single-float-real-slot vm:word-bytes)
               vm:other-pointer-type)))
    (let ((imag-tn (complex-single-reg-imag-tn x)))
      (inst stur imag-tn y
            (- (* vm:complex-single-float-imag-slot vm:word-bytes)
               vm:other-pointer-type)))))
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex double float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 13
    (emit-not-implemented)
    (with-fixed-allocation (y ndescr vm:complex-double-float-type
                              vm:complex-double-float-size))
    (let ((real-tn (complex-double-reg-real-tn x)))
      (inst stur real-tn y
            (- (* vm:complex-double-float-real-slot vm:word-bytes)
               vm:other-pointer-type)))
    (let ((imag-tn (complex-double-reg-imag-tn x)))
      (inst stur imag-tn y
            (- (* vm:complex-double-float-imag-slot vm:word-bytes)
               vm:other-pointer-type)))))
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))


(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (emit-not-implemented)
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst ldur real-tn x
            (- (* complex-single-float-real-slot word-bytes)
               other-pointer-type)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst ldur imag-tn x
            (- (* complex-single-float-imag-slot word-bytes)
               other-pointer-type)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note _N"pointer to complex float coercion")
  (:generator 2
    (emit-not-implemented)
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst ldur real-tn x
            (- (* complex-double-float-real-slot word-bytes)
               other-pointer-type)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst ldur imag-tn x
            (- (* complex-double-float-imag-slot word-bytes)
               other-pointer-type)))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))


;;;; -----------------------------------------------------------------------
;;;; Complex float move-argument VOPs
;;;; -----------------------------------------------------------------------

(define-vop (move-complex-single-float-argument)
  (:args (x :scs (complex-single-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note _N"complex single-float argument move")
  (:generator 1
    (emit-not-implemented)
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
         (inst fmov (complex-single-reg-real-tn y)
                    (complex-single-reg-real-tn x))
         (inst fmov (complex-single-reg-imag-tn y)
                    (complex-single-reg-imag-tn x))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) word-bytes)))
         (inst stur (complex-single-reg-real-tn x) nfp offset)
         (inst stur (complex-single-reg-imag-tn x) nfp
                   (+ offset word-bytes)))))))
(define-move-vop move-complex-single-float-argument :move-argument
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-argument)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note _N"complex double-float argument move")
  (:generator 2
    (emit-not-implemented)
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
         (move-double-reg (complex-double-reg-real-tn y)
                          (complex-double-reg-real-tn x))
         (move-double-reg (complex-double-reg-imag-tn y)
                          (complex-double-reg-imag-tn x))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
         (inst stur (complex-double-reg-real-tn x) nfp offset)
         (inst stur (complex-double-reg-imag-tn x) nfp
                   (+ offset (* 2 word-bytes))))))))
(define-move-vop move-complex-double-float-argument :move-argument
  (complex-double-reg descriptor-reg) (complex-double-reg))


#+double-double
(define-vop (move-from-complex-double-double)
  (:args (x :scs (complex-double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"complex double-double float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 13
    (emit-not-implemented)
    (with-fixed-allocation (y ndescr vm::complex-double-double-float-type
                              vm::complex-double-double-float-size))
    (inst stur (complex-double-double-reg-real-hi-tn x) y
          (- (* vm::complex-double-double-float-real-hi-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst stur (complex-double-double-reg-real-lo-tn x) y
          (- (* vm::complex-double-double-float-real-lo-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst stur (complex-double-double-reg-imag-hi-tn x) y
          (- (* vm::complex-double-double-float-imag-hi-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst stur (complex-double-double-reg-imag-lo-tn x) y
          (- (* vm::complex-double-double-float-imag-lo-slot vm:word-bytes)
             vm:other-pointer-type))))
#+double-double
(define-move-vop move-from-complex-double-double :move
  (complex-double-double-reg) (descriptor-reg))

#+double-double
(define-vop (move-to-complex-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-double-reg)))
  (:note _N"pointer to complex double-double float coercion")
  (:generator 2
    (emit-not-implemented)
    (inst ldur (complex-double-double-reg-real-hi-tn y) x
          (- (* vm::complex-double-double-float-real-hi-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst ldur (complex-double-double-reg-real-lo-tn y) x
          (- (* vm::complex-double-double-float-real-lo-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst ldur (complex-double-double-reg-imag-hi-tn y) x
          (- (* vm::complex-double-double-float-imag-hi-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst ldur (complex-double-double-reg-imag-lo-tn y) x
          (- (* vm::complex-double-double-float-imag-lo-slot vm:word-bytes)
             vm:other-pointer-type))))
#+double-double
(define-move-vop move-to-complex-double-double :move
  (descriptor-reg) (complex-double-double-reg))

#+double-double
(define-vop (move-complex-double-double-float-argument)
  (:args (x :scs (complex-double-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-double-reg))))
  (:results (y))
  (:note _N"complex double-double float argument move")
  (:generator 2
    (emit-not-implemented)
    (sc-case y
      (complex-double-double-reg
       (unless (location= x y)
         (move-double-reg (complex-double-double-reg-real-hi-tn y)
                          (complex-double-double-reg-real-hi-tn x))
         (move-double-reg (complex-double-double-reg-real-lo-tn y)
                          (complex-double-double-reg-real-lo-tn x))
         (move-double-reg (complex-double-double-reg-imag-hi-tn y)
                          (complex-double-double-reg-imag-hi-tn x))
         (move-double-reg (complex-double-double-reg-imag-lo-tn y)
                          (complex-double-double-reg-imag-lo-tn x))))
      (complex-double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
         (inst stur (complex-double-double-reg-real-hi-tn x) nfp offset)
         (inst stur (complex-double-double-reg-real-lo-tn x) nfp
               (+ offset word-bytes))
         (inst stur (complex-double-double-reg-imag-hi-tn x) nfp
               (+ offset (* 2 word-bytes)))
         (inst stur (complex-double-double-reg-imag-lo-tn x) nfp
               (+ offset (* 3 word-bytes))))))))
#+double-double
(define-move-vop move-complex-double-double-float-argument :move-argument
  (complex-double-double-reg descriptor-reg) (complex-double-double-reg))


(define-move-vop move-argument :move-argument
  (single-reg double-reg
   #+double-double double-double-reg
   complex-single-reg complex-double-reg
   #+double-double complex-double-double-reg)
  (descriptor-reg))


;;;; -----------------------------------------------------------------------
;;;; Arithmetic VOPs
;;;;
;;;; SPARC used fadds/faddd etc.  ARM64 uses a single mnemonic (fadd, etc.)
;;;; that dispatches on the register SC (single vs double).
;;;; Generator costs are kept identical to the SPARC values.
;;;; -----------------------------------------------------------------------

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

;;; Binary ops: each SPARC paired (fadds,faddd) becomes a single ARM64 mnemonic.
;;; The SC-dispatch in the emitter is handled automatically by the VOP
;;; inheriting from single-float-op or double-float-op.
(macrolet ((frob (op inst sname scost dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (emit-not-implemented)
                    (note-this-location vop :internal-error)
                    (inst ,inst r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (emit-not-implemented)
                    (note-this-location vop :internal-error)
                    (inst ,inst r x y))))))
  ;;                 SPARC cost  ARM64 single/double instruction
  (frob + fadd +/single-float  2  +/double-float  2)
  (frob - fsub -/single-float  2  -/double-float  2)
  (frob * fmul */single-float  4  */double-float  5)
  (frob / fdiv //single-float 12  //double-float 19))


;;;; -----------------------------------------------------------------------
;;;; Unary float arithmetic: FABS, FNEG, FSQRT
;;;;
;;;; On SPARC, double FNEG/FABS required V9 guards and manual register
;;;; manipulation on V8.  ARM64 has FABS/FNEG for both S and D with no
;;;; feature guard.
;;;; -----------------------------------------------------------------------

(macrolet ((frob (name inst translate sc type)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note _N"inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst ,inst y x)))))
  (frob abs/single-float    fabs  abs      single-reg single-float)
  (frob abs/double-float    fabs  abs      double-reg double-float)
  (frob %negate/single-float fneg %negate  single-reg single-float)
  (frob %negate/double-float fneg %negate  double-reg double-float))


;;;; -----------------------------------------------------------------------
;;;; FSQRT
;;;; -----------------------------------------------------------------------

(define-vop (fsqrt/double-float)
  (:args (x :scs (double-reg)))
  (:results (y :scs (double-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types double-float)
  (:result-types double-float)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (emit-not-implemented)
    (note-this-location vop :internal-error)
    (inst fsqrt y x)))

(define-vop (fsqrt/single-float)
  (:args (x :scs (single-reg)))
  (:results (y :scs (single-reg)))
  (:translate %sqrt)
  (:policy :fast-safe)
  (:arg-types single-float)
  (:result-types single-float)
  (:note _N"inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
    (emit-not-implemented)
    (note-this-location vop :internal-error)
    (inst fsqrt y x)))


;;;; -----------------------------------------------------------------------
;;;; Comparison VOPs
;;;;
;;;; SPARC: FCMPS/FCMPD followed by optional NOP then FB<cc>.
;;;; ARM64:  FCMP  followed by B<cc> (no delay slot, no NOP).
;;;; The FCMP instruction sets the integer PSTATE condition flags (N,Z,C,V)
;;;; via the FP comparison mapping, so ordinary Bcc branches work.
;;;; -----------------------------------------------------------------------

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note _N"inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (emit-not-implemented)
    (note-this-location vop :internal-error)
    ;; FCMP sets PSTATE flags; SC dispatch is automatic from arg types.
    (ecase format
      (:single (inst fcmp x y))
      (:double (inst fcmp x y)))
    ;; ARM64 has no branch-delay slot; branch immediately.
    (inst b (if not-p nope yep) target)))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,yep ,nope))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,yep ,nope)))))
  ;; ARM64 condition codes after FCMP:
  ;;   <  -> :mi (minus / LT)   -- note: for FP, use :lt which maps to MI for ordered
  ;;   >  -> :gt
  ;;   =  -> :eq
  (frob <  :lt :ge  </single-float  </double-float)
  (frob >  :gt :le  >/single-float  >/double-float)
  (frob =  :eq :ne  eql/single-float eql/double-float))


#+double-double
(deftransform eql ((x y) (double-double-float double-double-float))
  '(and (eql (double-double-hi x) (double-double-hi y))
        (eql (double-double-lo x) (double-double-lo y))))


;;;; -----------------------------------------------------------------------
;;;; Conversion VOPs
;;;;
;;;; SPARC round-tripped through the stack (FITOS store/load) because the
;;;; integer and FP register files were separate.
;;;; ARM64: SCVTF/UCVTF convert directly from an integer register to an FP
;;;; register in one instruction; FCVTZS/FCVTZU go the other way.
;;;; No memory round-trip is needed.
;;;; -----------------------------------------------------------------------

(macrolet
    ((frob (name type)
       `(progn
          (defknown ,name ((signed-byte 32))
            ,type)
          (defun ,name (n)
            (declare (type (signed-byte 32) n))
            (,name n)))))
  (frob %%single-float single-float)
  (frob %%double-float double-float))

;;; Signed 32-bit integer -> float.
;;; ARM64: SCVTF.W Sd/Dd, Wn  (scvtf.w uses the W/32-bit source form).
(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note _N"inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 3
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst scvtf.w y x)))))
  (frob %single-float/signed %%single-float single-reg single-float)
  (frob %double-float/signed %%double-float double-reg double-float))

;;; Unsigned 32-bit integer -> float.
(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (unsigned-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types unsigned-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note _N"inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 3
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst ucvtf.w y x)))))
  (frob %single-float/unsigned %single-float single-reg single-float)
  (frob %double-float/unsigned %double-float double-reg double-float))

;;; Signed 64-bit integer -> float (for bignums / full-word conversions).
(macrolet ((frob (name translate to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (signed-reg)))
                (:results (y :scs (,to-sc)))
                (:arg-types signed-num)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note _N"inline float coercion (64-bit)")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 3
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst scvtf y x)))))
  (frob %single-float/signed64 %single-float single-reg single-float)
  (frob %double-float/signed64 %double-float double-reg double-float))

;;; Float precision conversion: single <-> double.
;;; ARM64: FCVT Dd, Sn  (single->double) / FCVT Sd, Dn  (double->single).
(macrolet ((frob (name translate from-sc from-type to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (,to-sc)))
                (:arg-types ,from-type)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note _N"inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 2
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst fcvt y x)))))
  (frob %single-float/double-float %single-float
    double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float
    single-reg single-float double-reg double-float))

;;; Float -> signed integer (round toward zero).
;;; ARM64: FCVTZS.W Wd, Sn/Dn  (32-bit result).
(macrolet ((frob (trans from-sc from-type)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)
                             :load-if (not (sc-is y signed-stack))))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note _N"inline float truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst fcvtzs.w y x)))))
  (frob %unary-truncate single-reg single-float)
  (frob %unary-truncate double-reg double-float))

;;; Float -> signed 64-bit integer (round toward zero).
(macrolet ((frob (trans from-sc from-type)
             `(define-vop (,(symbolicate trans "/64/" from-type))
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note _N"inline float truncate (64-bit result)")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (emit-not-implemented)
                  (note-this-location vop :internal-error)
                  (inst fcvtzs y x)))))
  (frob %unary-truncate single-reg single-float)
  (frob %unary-truncate double-reg double-float))

;;; Fast in-register float truncation: FRINTZ returns a float rounded
;;; toward zero without converting to integer.
;;; Replaces the SPARC fast-unary-ftruncate pattern (fstoi+fitos etc.).
(define-vop (fast-unary-ftruncate/single-float)
  (:args (x :scs (single-reg)))
  (:arg-types single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:note _N"inline ftruncate")
  (:generator 1
    (emit-not-implemented)
    (inst frintz r x)))

(define-vop (fast-unary-ftruncate/double-float)
  (:args (x :scs (double-reg)))
  (:arg-types double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:policy :fast-safe)
  (:translate c::fast-unary-ftruncate)
  (:note _N"inline ftruncate")
  (:generator 1
    (emit-not-implemented)
    (inst frintz r x)))


;;;; -----------------------------------------------------------------------
;;;; Bit-pattern VOPs: make-single-float, make-double-float,
;;;;                    single-float-bits, double-float-{high,low}-bits
;;;;
;;;; SPARC required a memory round-trip (ST to NFP stack, LDF back).
;;;; ARM64 has FMOV instructions that move bits directly between the integer
;;;; and FP register files:
;;;;   FMOV Sd, Wn   -- W-register bits -> single-float register
;;;;   FMOV Wn, Sd   -- single-float bits -> W register (signed view: read as X)
;;;;   FMOV Dd, Xn   -- X-register bits -> double-float register
;;;;   FMOV Xn, Dd   -- double-float bits -> X register
;;;; -----------------------------------------------------------------------

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg)))
  (:results (res :scs (single-reg)))
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:generator 2
    (emit-not-implemented)
    ;; FMOV Sd, Wn -- move 32-bit integer bits into Sn view of Vd.
    (inst fmov res bits)))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (emit-not-implemented)
    (sc-case float
      (single-reg
       ;; FMOV Wn, Sd -- move Sn bits into W register.
       (inst fmov bits float))
      (single-stack
       ;; ldur.w: 32-bit integer load with unscaled byte offset.
       (inst ldur.w bits (current-nfp-tn vop)
             (* (tn-offset float) vm:word-bytes)))
      (descriptor-reg
       (loadw bits float vm:single-float-value-slot
              vm:other-pointer-type)))))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (unsigned-reg)) tmp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (emit-not-implemented)
    ;; Combine hi-bits (upper 32) and lo-bits (lower 32) into a 64-bit Xn,
    ;; then FMOV Dd, Xn.
    (inst lsl tmp hi-bits 32)           ; shift hi into upper word
    (inst orr tmp tmp lo-bits)          ; merge lo into lower word (zero-extended)
    (inst fmov res tmp)))               ; FMOV Dd, Xn

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (unsigned-reg)) tmp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case float
      (double-reg
       (inst fmov tmp float)            ; FMOV Xn, Dd
       (inst lsr hi-bits tmp 32))       ; hi word
      (double-stack
       ;; Load the full 64-bit double from the stack slot, then extract
       ;; the high 32 bits.  ARM64 is little-endian: the full value is at
       ;; tn-offset*word-bytes; we shift right 32 to get the high word.
       (inst ldur tmp (current-nfp-tn vop)
             (* (tn-offset float) vm:word-bytes))
       (inst lsr hi-bits tmp 32))
      (descriptor-reg
       (loadw hi-bits float vm:double-float-value-slot
              vm:other-pointer-type)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg)) tmp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case float
      (double-reg
       (inst fmov tmp float)            ; FMOV Xn, Dd
       ;; Low 32 bits via UXTW (zero-extend lower word).
       (inst and lo-bits tmp #xFFFFFFFF))
      (double-stack
       (inst ldur lo-bits (current-nfp-tn vop)
             (* (tn-offset float) vm:word-bytes)))
      (descriptor-reg
       (loadw lo-bits float (1+ vm:double-float-value-slot)
              vm:other-pointer-type)))))

(define-vop (double-float-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg))
            (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (unsigned-reg)) tmp)
  (:arg-types double-float)
  (:result-types signed-num unsigned-num)
  (:translate kernel::double-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case float
      (double-reg
       (inst fmov tmp float)
       (inst lsr hi-bits tmp 32)
       (inst and lo-bits tmp #xFFFFFFFF))
      (double-stack
       (inst ldur tmp (current-nfp-tn vop)
             (* (tn-offset float) vm:word-bytes))
       (inst lsr hi-bits tmp 32)
       (inst and lo-bits tmp #xFFFFFFFF))
      (descriptor-reg
       (loadw hi-bits float vm:double-float-value-slot
              vm:other-pointer-type)
       (loadw lo-bits float (1+ vm:double-float-value-slot)
              vm:other-pointer-type)))))


;;;; -----------------------------------------------------------------------
;;;; Float mode hackery
;;;;
;;;; SPARC used STFSR/LDFSR to access the FSR register.
;;;; ARM64: FPCR (control) and FPSR (status) are system registers accessed
;;;; via MRS/MSR.  The combined "floating-point-modes" value packs both.
;;;;
;;;; Convention (mirrors SPARC FSR layout for CMUCL compatibility):
;;;;   bits [31:0] = FPSR (status: exception flags, QC, IDC)
;;;;   For (setf floating-point-modes) we write the rounding-mode bits
;;;;   into FPCR[23:22].  The full treatment would pack them; for now
;;;;   we follow the minimal pattern used by the SPARC port.
;;;; -----------------------------------------------------------------------

(deftype float-modes () '(unsigned-byte 32))
(defknown floating-point-modes () float-modes (flushable))
(defknown ((setf floating-point-modes)) (float-modes) float-modes)

(define-vop (floating-point-modes)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate floating-point-modes)
  (:policy :fast-safe)
  (:generator 3
    (emit-not-implemented)
    ;; Read FPSR into res.
    (inst mrs res :fpsr)))

(define-vop (set-floating-point-modes)
  (:args (new :scs (unsigned-reg) :target res))
  (:results (res :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:result-types unsigned-num)
  (:translate (setf floating-point-modes))
  (:policy :fast-safe)
  (:generator 3
    (emit-not-implemented)
    (inst msr :fpsr new)
    (move res new)))


;;;; -----------------------------------------------------------------------
;;;; Complex float arithmetic VOPs
;;;;
;;;; These follow the SPARC structure exactly, with the instruction
;;;; substitutions: fadds->fadd, faddd->fadd (SC dispatch), etc.
;;;; -----------------------------------------------------------------------

(define-vop (make-complex-single-float)
  (:translate complex)
  (:args (real :scs (single-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (single-reg) :to :save))
  (:arg-types single-float single-float)
  (:results (r :scs (complex-single-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-single-stack))))
  (:result-types complex-single-float)
  (:note _N"inline complex single-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case r
      (complex-single-reg
       (let ((r-real (complex-single-reg-real-tn r)))
         (unless (location= real r-real)
           (inst fmov r-real real)))
       (let ((r-imag (complex-single-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (inst fmov r-imag imag))))
      (complex-single-stack
       (let ((nfp    (current-nfp-tn vop))
             (offset (* (tn-offset r) vm:word-bytes)))
         (unless (location= real r)
           (inst stur real nfp offset))
         (inst stur imag nfp (+ offset vm:word-bytes)))))))

(define-vop (make-complex-double-float)
  (:translate complex)
  (:args (real :scs (double-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (double-reg) :to :save))
  (:arg-types double-float double-float)
  (:results (r :scs (complex-double-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-double-stack))))
  (:result-types complex-double-float)
  (:note _N"inline complex double-float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case r
      (complex-double-reg
       (let ((r-real (complex-double-reg-real-tn r)))
         (unless (location= real r-real)
           (move-double-reg r-real real)))
       (let ((r-imag (complex-double-reg-imag-tn r)))
         (unless (location= imag r-imag)
           (move-double-reg r-imag imag))))
      (complex-double-stack
       (let ((nfp    (current-nfp-tn vop))
             (offset (* (tn-offset r) vm:word-bytes)))
         (unless (location= real r)
           (inst stur real nfp offset))
         (inst stur imag nfp (+ offset (* 2 vm:word-bytes))))))))


(define-vop (complex-single-float-value)
  (:args (x :scs (complex-single-reg descriptor-reg)
            :load-if (not (sc-is x complex-single-stack))))
  (:arg-types complex-single-float)
  (:results (r :scs (single-reg)))
  (:result-types single-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (complex-single-reg
       (let ((value-tn (ecase slot
                         (:real (complex-single-reg-real-tn x))
                         (:imag (complex-single-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (inst fmov r value-tn))))
      (complex-single-stack
       (inst ldur r (current-nfp-tn vop)
             (* (+ (ecase slot (:real 0) (:imag 1)) (tn-offset x))
                vm:word-bytes)))
      (descriptor-reg
       (inst ldur r x
             (- (* (ecase slot
                     (:real vm:complex-single-float-real-slot)
                     (:imag vm:complex-single-float-imag-slot))
                   vm:word-bytes)
                vm:other-pointer-type))))))

(define-vop (realpart/complex-single-float complex-single-float-value)
  (:translate realpart)
  (:note _N"complex single float realpart")
  (:variant :real))

(define-vop (imagpart/complex-single-float complex-single-float-value)
  (:translate imagpart)
  (:note _N"complex single float imagpart")
  (:variant :imag))


(define-vop (complex-double-float-value)
  (:args (x :scs (complex-double-reg descriptor-reg)
            :load-if (not (sc-is x complex-double-stack))))
  (:arg-types complex-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (complex-double-reg
       (let ((value-tn (ecase slot
                         (:real (complex-double-reg-real-tn x))
                         (:imag (complex-double-reg-imag-tn x)))))
         (unless (location= value-tn r)
           (move-double-reg r value-tn))))
      (complex-double-stack
       (inst ldur r (current-nfp-tn vop)
             (* (+ (ecase slot (:real 0) (:imag 1)) (tn-offset x))
                vm:word-bytes)))
      (descriptor-reg
       (inst ldur r x
             (- (* (ecase slot
                     (:real vm:complex-double-float-real-slot)
                     (:imag vm:complex-double-float-imag-slot))
                   vm:word-bytes)
                vm:other-pointer-type))))))

(define-vop (realpart/complex-double-float complex-double-float-value)
  (:translate realpart)
  (:note _N"complex double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-float complex-double-float-value)
  (:translate imagpart)
  (:note _N"complex double float imagpart")
  (:variant :imag))


;;;; -----------------------------------------------------------------------
;;;; #+double-double support
;;;;
;;;; ARM64 uses two consecutive Dn registers per double-double value,
;;;; exactly as SPARC used two consecutive double-regs.  The move helpers
;;;; are already defined above; the VOPs follow the SPARC structure directly.
;;;; -----------------------------------------------------------------------

#+double-double
(progn

(define-move-function (load-double-double 2) (vop x y)
  ((double-double-stack) (double-double-reg))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset x) vm:word-bytes)))
    (inst ldur (double-double-reg-hi-tn y) nfp offset)
    (inst ldur (double-double-reg-lo-tn y) nfp (+ offset vm:word-bytes))))

(define-move-function (store-double-double 2) (vop x y)
  ((double-double-reg) (double-double-stack))
  (let ((nfp    (current-nfp-tn vop))
        (offset (* (tn-offset y) vm:word-bytes)))
    (inst stur (double-double-reg-hi-tn x) nfp offset)
    (inst stur (double-double-reg-lo-tn x) nfp (+ offset vm:word-bytes))))

(define-vop (double-double-move)
  (:args (x :scs (double-double-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (double-double-reg) :load-if (not (location= x y))))
  (:note _N"double-double float move")
  (:generator 0
    (emit-not-implemented)
    (unless (location= x y)
      (move-double-reg (double-double-reg-hi-tn y) (double-double-reg-hi-tn x))
      (move-double-reg (double-double-reg-lo-tn y) (double-double-reg-lo-tn x)))))
(define-move-vop double-double-move :move
  (double-double-reg) (double-double-reg))

(define-vop (move-from-double-double)
  (:args (x :scs (double-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:note _N"double-double float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 13
    (emit-not-implemented)
    (with-fixed-allocation (y ndescr vm::double-double-float-type
                              vm::double-double-float-size))
    (inst stur (double-double-reg-hi-tn x) y
          (- (* vm::double-double-float-hi-slot vm:word-bytes)
             vm:other-pointer-type))
    (inst stur (double-double-reg-lo-tn x) y
          (- (* vm::double-double-float-lo-slot vm:word-bytes)
             vm:other-pointer-type))))
(define-move-vop move-from-double-double :move
  (double-double-reg) (descriptor-reg))

(define-vop (move-to-double-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (double-double-reg)))
  (:note _N"pointer to double-double float coercion")
  (:generator 2
    (emit-not-implemented)
    (inst ldur (double-double-reg-hi-tn y) x
          (- (* double-double-float-hi-slot word-bytes)
             other-pointer-type))
    (inst ldur (double-double-reg-lo-tn y) x
          (- (* double-double-float-lo-slot word-bytes)
             other-pointer-type))))
(define-move-vop move-to-double-double :move
  (descriptor-reg) (double-double-reg))

(define-vop (move-double-double-float-argument)
  (:args (x :scs (double-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y double-double-reg))))
  (:results (y))
  (:note _N"double-double float argument move")
  (:generator 2
    (emit-not-implemented)
    (sc-case y
      (double-double-reg
       (unless (location= x y)
         (move-double-reg (double-double-reg-hi-tn y) (double-double-reg-hi-tn x))
         (move-double-reg (double-double-reg-lo-tn y) (double-double-reg-lo-tn x))))
      (double-double-stack
       (let ((offset (* (tn-offset y) word-bytes)))
         (inst stur (double-double-reg-hi-tn x) nfp offset)
         (inst stur (double-double-reg-lo-tn x) nfp (+ offset word-bytes)))))))
(define-move-vop move-double-double-float-argument :move-argument
  (double-double-reg descriptor-reg) (double-double-reg))

(define-vop (make/double-double-float)
  (:args (hi :scs (double-reg) :target res
             :load-if (not (location= hi res)))
         (lo :scs (double-reg) :to :save))
  (:results (res :scs (double-double-reg) :from (:argument 0)
                 :load-if (not (sc-is res double-double-stack))))
  (:arg-types double-float double-float)
  (:result-types double-double-float)
  (:translate kernel::%make-double-double-float)
  (:note _N"inline double-double float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case res
      (double-double-reg
       (let ((res-hi (double-double-reg-hi-tn res)))
         (unless (location= res-hi hi)
           (move-double-reg res-hi hi)))
       (let ((res-lo (double-double-reg-lo-tn res)))
         (unless (location= res-lo lo)
           (move-double-reg res-lo lo))))
      (double-double-stack
       (let ((nfp    (current-nfp-tn vop))
             (offset (* (tn-offset res) vm:word-bytes)))
         (unless (location= hi res)
           (inst stur hi nfp offset))
         (inst stur lo nfp (+ offset vm:word-bytes)))))))

(define-vop (double-double-float-value)
  (:args (x :scs (double-double-reg descriptor-reg) :target r
            :load-if (not (sc-is x double-double-stack))))
  (:arg-types double-double-float)
  (:results (r :scs (double-reg)))
  (:result-types double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (double-double-reg
       (let ((value-tn (ecase slot
                         (:hi (double-double-reg-hi-tn x))
                         (:lo (double-double-reg-lo-tn x)))))
         (unless (location= value-tn r)
           (move-double-reg r value-tn))))
      (double-double-stack
       (inst ldur r (current-nfp-tn vop)
             (* (+ (ecase slot (:hi 0) (:lo 1)) (tn-offset x))
                vm:word-bytes)))
      (descriptor-reg
       (inst ldur r x
             (- (* vm:word-bytes
                   (ecase slot
                     (:hi vm:double-double-float-hi-slot)
                     (:lo vm:double-double-float-lo-slot)))
                vm:other-pointer-type))))))

(define-vop (hi/double-double-value double-double-float-value)
  (:translate kernel::double-double-hi)
  (:note _N"double-double high part")
  (:variant :hi))

(define-vop (lo/double-double-value double-double-float-value)
  (:translate kernel::double-double-lo)
  (:note _N"double-double low part")
  (:variant :lo))


(define-vop (make-complex-double-double-float)
  (:translate complex)
  (:args (real :scs (double-double-reg) :target r
               :load-if (not (location= real r)))
         (imag :scs (double-double-reg) :to :save))
  (:arg-types double-double-float double-double-float)
  (:results (r :scs (complex-double-double-reg) :from (:argument 0)
               :load-if (not (sc-is r complex-double-double-stack))))
  (:result-types complex-double-double-float)
  (:note _N"inline complex double-double float creation")
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (emit-not-implemented)
    (sc-case r
      (complex-double-double-reg
       (move-double-reg (complex-double-double-reg-real-hi-tn r)
                        (double-double-reg-hi-tn real))
       (move-double-reg (complex-double-double-reg-real-lo-tn r)
                        (double-double-reg-lo-tn real))
       (move-double-reg (complex-double-double-reg-imag-hi-tn r)
                        (double-double-reg-hi-tn imag))
       (move-double-reg (complex-double-double-reg-imag-lo-tn r)
                        (double-double-reg-lo-tn imag)))
      (complex-double-double-stack
       (let ((nfp    (current-nfp-tn vop))
             (offset (* (tn-offset r) vm:word-bytes)))
         (inst stur (double-double-reg-hi-tn real) nfp offset)
         (inst stur (double-double-reg-lo-tn real) nfp (+ offset vm:word-bytes))
         (inst stur (double-double-reg-hi-tn imag) nfp (+ offset (* 2 vm:word-bytes)))
         (inst stur (double-double-reg-lo-tn imag) nfp (+ offset (* 3 vm:word-bytes))))))))

(define-vop (complex-double-double-float-value)
  (:args (x :scs (complex-double-double-reg descriptor-reg)
            :load-if (not (or (sc-is x complex-double-double-stack)))))
  (:arg-types complex-double-double-float)
  (:results (r :scs (double-double-reg)))
  (:result-types double-double-float)
  (:variant-vars slot)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 3
    (emit-not-implemented)
    (sc-case x
      (complex-double-double-reg
       (move-double-reg
        (double-double-reg-hi-tn r)
        (ecase slot
          (:real (complex-double-double-reg-real-hi-tn x))
          (:imag (complex-double-double-reg-imag-hi-tn x))))
       (move-double-reg
        (double-double-reg-lo-tn r)
        (ecase slot
          (:real (complex-double-double-reg-real-lo-tn x))
          (:imag (complex-double-double-reg-imag-lo-tn x)))))
      (complex-double-double-stack
       (inst ldur (double-double-reg-hi-tn r) (current-nfp-tn vop)
             (* (+ (ecase slot (:real 0) (:imag 2)) (tn-offset x))
                vm:word-bytes))
       (inst ldur (double-double-reg-lo-tn r) (current-nfp-tn vop)
             (* (+ (ecase slot (:real 1) (:imag 3)) (tn-offset x))
                vm:word-bytes)))
      (descriptor-reg
       (inst ldur (double-double-reg-hi-tn r) x
             (- (* (ecase slot
                     (:real vm::complex-double-double-float-real-hi-slot)
                     (:imag vm::complex-double-double-float-imag-hi-slot))
                   vm:word-bytes)
                vm:other-pointer-type))
       (inst ldur (double-double-reg-lo-tn r) x
             (- (* (ecase slot
                     (:real vm::complex-double-double-float-real-lo-slot)
                     (:imag vm::complex-double-double-float-imag-lo-slot))
                   vm:word-bytes)
                vm:other-pointer-type))))))

(define-vop (realpart/complex-double-double-float complex-double-double-float-value)
  (:translate realpart)
  (:note _N"complex double-double float realpart")
  (:variant :real))

(define-vop (imagpart/complex-double-double-float complex-double-double-float-value)
  (:translate imagpart)
  (:note _N"complex double-double float imagpart")
  (:variant :imag))

) ; #+double-double progn
