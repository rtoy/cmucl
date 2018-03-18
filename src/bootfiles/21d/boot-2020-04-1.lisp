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

;; This is also used to easily change the order of x86::conditions
;; constant so that we prefer je instead of jeq.  Without a
;; cross-compile we'd need to handle the refefintion of the
;; defconstant in a different way.  See issue #95.

