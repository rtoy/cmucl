;; Cross-compile script needed for updating the SSE2 call-out VOP and
;; x86 call_into_c.  In both places, we remove the need to do the fldz
;; and fstp dance to set up the FPU for C and Lisp.  SSE2 doesn't use
;; the FPU.

#+x86
(load "target:tools/cross-scripts/cross-x86-x86.lisp")

