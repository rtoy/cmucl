;;; Bootstrap for modular arithmetic.  (Taken from 18e/boot22.lisp)
;;; (Primarily for ppc.)

#-modular-arith
(pushnew :modular-arith *features*)

(in-package "KERNEL")
#-modular-arith
(defvar *modular-funs*
  (make-hash-table :test 'eq))
