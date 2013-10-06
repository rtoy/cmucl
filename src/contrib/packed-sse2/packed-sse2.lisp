;;;; -*- Mode: LISP; -*-

(in-package "CL-USER")
;; Fail if we don't have sse2!

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (featurep :sse2)
    (error "Cannot use packed SSE2 instructions without SSE2 support.")))

;; SSE2 Packed operations.
;;
;; We use (complex double-float) variables to hold the packed values,
;; including packed doubles and packed singles.  For convenience, use
;; sse2-setpd or sse2-setps to initialize the packed variables with
;; the given scalar values.  To extract the individual components of
;; the packed values, use sse2-getpd or sse2-getps.

(in-package #:vm)

(export '(sse2-mulpd sse2-mulps
	  sse2-divpd sse2-divps
	  sse2-shufpd sse2-shufps
	  sse2-setpd sse2-setps
	  sse2-getpd sse2-getps))

(defknown (sse2-mulpd sse2-mulps) ((complex double-float) (complex double-float))
  (complex double-float)
  (movable foldable flushable))

(defknown (sse2-divpd sse2-divps) ((complex double-float) (complex double-float))
  (complex double-float)
  (movable foldable flushable))

(macrolet
    ((generate (movinst opinst commutative)
       `(cond
	 ((location= x r)
	  (inst ,opinst x y))
	 ((and ,commutative (location= y r))
	  (inst ,opinst y x))
	 ((not (location= r y))
	  (inst ,movinst r x)
	  (inst ,opinst r y))
	 (t
	  (inst ,movinst tmp x)
	  (inst ,opinst tmp y)
	  (inst ,movinst r tmp))))
     (packed-op (op inst float-type cost &optional commutative)
       (let* ((vop-name (symbolicate (symbol-name op) "/COMPLEX-" float-type "-FLOAT"))
	      (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
	      (complex-reg (symbolicate "COMPLEX-" float-type "-REG")))
	 ;; Note: It would probably improve things if we could use
	 ;; memory operands, but we can't because the instructions
	 ;; assumed 128-bit alignment, which we can't guarantee.
	 `(define-vop (,vop-name)
	   (:args (x :scs (,complex-reg) :target r)
	          (y :scs (,complex-reg)))
	   (:results (r :scs (,complex-reg)))
	   (:arg-types ,c-type ,c-type)
	   (:result-types ,c-type)
	   (:policy :fast-safe)
	   (:note "inline packed arithmetic")
	   (:translate ,op)
	   (:temporary (:sc ,complex-reg) tmp)
	   (:generator ,cost
	     (generate movaps ,inst ,commutative))))))
  (packed-op sse2-mulpd mulpd double 1 t)
  (packed-op sse2-mulps mulps double 1 t)
  (packed-op sse2-divpd divpd double 1)
  (packed-op sse2-divps divps double 1))

(defun sse2-mulpd (x y)
  "Packed multiply of packed doubles in X and Y"
  (declare (type (complex double-float) x y))
  (sse2-mulpd x y))

(defun sse2-mulps (x y)
  "Packed multiply of packed singles in X and Y"
  (declare (type (complex double-float) x y))
  (sse2-mulps x y))

(defun sse2-divpd (x y)
  "Packed divide of packed doubles in X and Y"
  (declare (type (complex double-float) x y))
  (sse2-divpd x y))

(defun sse2-divps (x y)
  "Packed divide of packed singles in X and Y"
  (declare (type (complex double-float) x y))
  (sse2-divps x y))

(defknown (%sse2-shufpd %sse2-shufps) ((complex double-float) (complex double-float) fixnum)
  (complex double-float)
  (movable foldable flushable))

(macrolet
    ((generate (movinst opinst commutative)
       `(cond
	 ((location= x r)
	  (inst ,opinst x y i))
	 ((and ,commutative (location= y r))
	  (inst ,opinst y x i))
	 ((not (location= r y))
	  (inst ,movinst r x)
	  (inst ,opinst r y i))
	 (t
	  (inst ,movinst tmp x)
	  (inst ,opinst tmp y i)
	  (inst ,movinst r tmp))))
     (packed-op (op inst float-type cost &optional commutative)
       (let* ((vop-name (symbolicate (symbol-name op) "/COMPLEX-" float-type "-FLOAT"))
	      (c-type (symbolicate "COMPLEX-" float-type "-FLOAT"))
	      (complex-reg (symbolicate "COMPLEX-" float-type "-REG")))
	 ;; Note: It would probably improve things if we could use
	 ;; memory operands, but we can't because the instructions
	 ;; assumed 128-bit alignment, which we can't guarantee.
	 `(define-vop (,vop-name)
	   (:args (x :scs (,complex-reg) :target r)
	          (y :scs (,complex-reg)))
	   (:info i)
	   (:results (r :scs (,complex-reg)))
	   (:arg-types ,c-type ,c-type (:constant integer))
	   (:result-types ,c-type)
	   (:policy :fast-safe)
	   (:note "inline packed arithmetic")
	   (:translate ,op)
	   (:temporary (:sc ,complex-reg) tmp)
	   (:generator ,cost
	     (generate movaps ,inst ,commutative))))))
  (packed-op %sse2-shufpd shufpd double 1)
  (packed-op %sse2-shufps shufps double 1))

(declaim (inline sse2-shufpd sse2-shufps))

;; This needs better documentation.  I (rtoy) can never remember what
;; the magic I values actually do.
(defun sse2-shufpd (dst src i)
  "Shuffle packed doubles in X and Y according to I."
  (declare (type (complex double-float) dst src)
	   (type (unsigned-byte 2) i))
  (ecase i
    (0
     (%sse2-shufpd dst src 0))
    (1
     (%sse2-shufpd dst src 1))
    (2
     (%sse2-shufpd dst src 2))
    (3
     (%sse2-shufpd dst src 3))))

(defun sse2-shufps (dst src i)
  "Shuffle packed singles in X and Y according to I."
  (declare (type (complex double-float) dst src)
	   (type (unsigned-byte 4) i))
  (ecase i
    (0
     (%sse2-shufps dst src 0))
    (1
     (%sse2-shufps dst src 1))
    (2
     (%sse2-shufps dst src 2))
    (3
     (%sse2-shufps dst src 3))
    (4
     (%sse2-shufps dst src 4))
    (5
     (%sse2-shufps dst src 5))
    (6
     (%sse2-shufps dst src 6))
    (7
     (%sse2-shufps dst src 7))))

;; x is the high part and y is the low part.
(declaim (inline sse2-setpd sse2-getpd sse2-setps sse2-getps))
(defun sse2-setpd (x y)
  "Create a packed double with X in the high part and Y in the low part"
  (declare (type double-float x y))
  ;; Complex double-floats store the real part in the low half of the
  ;; sse2 register.
  (complex y x))

(defun sse2-getpd (pd)
  "Extract the components of a packed double PD.  Two values are
  returned; the high part is the first value and the low part in the
  second."
  (declare (type (complex double-float) pd)) (values
  (imagpart pd) (realpart pd)))

;; x3 is the high part and x0 is the low.
(defun sse2-setps (x3 x2 x1 x0)
  "Create a packed single with X3 in the highest part and X0 in the lowest"
  (declare (single-float x3 x2 x1 x0))
  (flet ((pack-singles-to-double (hi lo)
	   (let ((hi-bits (single-float-bits hi))
		 (lo-bits (single-float-bits lo)))
	     ;; Create a double-float where the most significant 32
	     ;; bits contain the hi float and the low 32-bits contain
	     ;; the low float.
	     (make-double-float hi-bits (logand #xffffffff lo-bits)))))
    ;; Pack the singles into a double and the pack the two doubles
    ;; into a (complex double-float).
    (sse2-setpd (pack-singles-to-double x3 x2)
		(pack-singles-to-double x1 x0))))

(defun sse2-getps (ps)
  "Extract the components of a packed single PS.  Four values are
  returned.  The first value is the highest part of the packed single
  and the last value is the lowest part of the packed single."
  (declare (type (complex double-float) ps))
  (flet ((unpack-double-to-singles (d)
	   (multiple-value-bind (hi lo)
	       (double-float-bits d)
	     (values (make-single-float hi)
		     (if (logbitp 31 lo)
			 (- (make-single-float (ldb (byte 31 0) lo)))
			 (make-single-float lo))))))
    (multiple-value-bind (x3 x2)
	(unpack-double-to-singles (imagpart ps))
      (multiple-value-bind (x1 x0)
	  (unpack-double-to-singles (realpart ps))
	(values x3 x2 x1 x0)))))

(provide "contrib-packed-sse2")
