
;; boot file for cross-compiler to add typed calling convention.

(c::define-info-type function c::calling-convention symbol nil)
(c::define-info-type function lisp::linkage lisp::linkage nil)
(comf "target:code/fdefinition" :load t)
(comf "target:code/load" :load t)
