;; This simple file is used to bootstrap the changes that remove the
;; constraint on the read-only space being in low memory.  This also
;; means we don't need those linker hacks and adjuster hacks.


;; Standard cross-compile script is good enough.
#+ppc
(load "target:tools/cross-scripts/cross-ppc-ppc-darwin.lisp")
