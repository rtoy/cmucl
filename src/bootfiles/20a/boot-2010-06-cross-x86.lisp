;; Cross-compile script for x86 to add new SC numbers the xmm
;; (floating-point) registers.  The default cross-compile script is
;; good enough for this.
#+x86
(load "target:tools/cross-scripts/cross-x86-x86")
