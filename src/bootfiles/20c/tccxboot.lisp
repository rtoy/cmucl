
;; boot file for cross-compiler to add typed calling convention.

(c::define-info-type function c::calling-convention symbol nil)
(c::define-info-type function lisp::linkage lisp::linkage nil)
(delete-file (compile-file "target:compiler/knownfun" :load t))
(delete-file (compile-file "target:code/load" :load t))


