;; Bootstrap for x86 to change the location of the floating-point
;; fields.  The fields must now match the definitions for the SSE2
;; MXCSR register, even for x87.
;;
;; (Really only needed if we're not compiling for sse2.)
(in-package "VM")

#+(and x86 (not sse2))
(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'continue))))
  (defconstant float-rounding-mode     (byte 2 13))
  (defconstant float-sticky-bits       (byte 6  0))
  (defconstant float-traps-byte        (byte 6  7))
  (defconstant float-exceptions-byte   (byte 6  0))
  )
