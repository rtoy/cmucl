;; Simple cross-compile script to remove `*scavenge-read-only-space*`
;; which is no longer needed
;;
;; Nothing special needs to be done for the cross-compile.  Just use
;; this file for the -B option (not really necessary), and use the
;; standard cross-compile scripts in src/tools/cross-scripts.
;;
;; cross-build-world.sh -crl -B boot-2020-04-1 xtarget xcross src/tools/cross-scripts/cross-foo.lisp old-lisp
;;
;; x86: cross-x86-x86
;; sparc: cross-sparc-sparc

