;;; Bootstrap file for fused multiply vops for ppc.

(in-package "VM")

#+ppc
(progn
(defknown (vm::fused-multiply-subtract vm::fused-multiply-add)
    (double-float double-float double-float)
  double-float
  (movable flushable))

(export '(fused-multiply-subtract fused-multiply-add))

(define-vop (fused-multiply-subtract/double)
  (:args (x :scs (double-reg))
         (y :scs (double-reg))
	 (z :scs (double-reg)))
  (:results (r :scs (double-reg)))
  (:arg-types double-float double-float double-float)
  (:result-types double-float)
  (:translate vm::fused-multiply-subtract)
  (:policy :fast-safe)
  (:generator 1
    ;; r = x*y - z	       
    (inst fmsub r x y z)))

(define-vop (fused-multiply-add/double)
  (:args (x :scs (double-reg))
         (y :scs (double-reg))
	 (z :scs (double-reg)))
  (:results (r :scs (double-reg)))
  (:arg-types double-float double-float double-float)
  (:result-types double-float)
  (:translate vm::fused-multiply-add)
  (:policy :fast-safe)
  (:generator 1
    ;; r = x*y + z
    (inst fmadd r x y z)))
)