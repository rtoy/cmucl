;;; Cross-compile script to add new slots to lisp-stream so we can
;;; have faster external formats.

;; Nothing special needed; use standard scripts

(load #+x86 "target:tools/cross-scripts/cross-x86-x86"
      #+sparc "target:tools/cross-scripts/cross-sparc-sparc"
      #+ppc "target:tools/cross-scripts/cross-ppc-ppc-darwin")
