;;; Cross-compile script to update extern-alien-name for
;;; darwin/x86. This makes extern-alien-name on darwin/x86 match
;;; darwin/ppc, where an underscore is prepended.
;;;
;;; A cross-compile might not truly be necessary, but this is known to
;;; work.

#+(and darwin x86)
(load "target:tools/cross-scripts/cross-x86-x86.lisp")
