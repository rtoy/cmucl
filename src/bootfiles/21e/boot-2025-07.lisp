;; Bootstrap file to define some constants we use early in the build.

(in-package :x86)

(defconstant x87-float-infinity-control-byte
  (byte 1 (+ 12 16))
  "The bit in the x87 FPU control word that controls the infinity mode.")
(defconstant x87-float-rounding-mode
  (byte 2 (+ 10 16))
  "The bits in the x87 FPU control word for the rounding mode.")
(defconstant x87-float-precision-control-byte
  (byte 2 (+ 8 16))
  "The bits in the x87 FPU contol word for the FP operation precision.")
(defconstant x87-float-traps-byte
  (byte 6 16)
  "The bits in the x87 FPU control word indicating the exceptions that
 are enabled.")
(defconstant x87-float-precision-control-alist
  `((:24-bits . 0)
    (:reserved . 1)
    (:53-bits . 2)
    (:64-bits . 3))
  "Alist for the x87 precison control.  The car is the symbolic
 precision and the cdr is the value for the precision control field.")

