;; Bootstrap the pseudo-atomic trap genesis change to put
;; trap_PseudoAtomic into internals.h.
#+sparc
(progn
  (load "target:code/exports.lisp")
  (in-package :sparc)
  (defconstant pseudo-atomic-trap 16))

