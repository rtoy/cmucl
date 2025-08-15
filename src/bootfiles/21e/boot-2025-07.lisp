;; Bootstrap file to define some constants we use early in the build.

(in-package :x86)

(defconstant x87-exceptions-byte
  (byte 6 0))
(defconstant x87-infinity-control-byte
  (byte 1 (+ 12 16)))
(defconstant x87-rounding-control-byte
  (byte 2 (+ 10 16)))
(defconstant x87-precision-control-byte
  (byte 2 (+ 8 16)))
(defconstant x87-exceptions-mask-byte
  (byte 6 16))
(defconstant x87-precision-control-alist
  `((:24-bits . 0)
    (:reserved . 1)
    (:53-bits . 2)
    (:64-bits . 3)))
