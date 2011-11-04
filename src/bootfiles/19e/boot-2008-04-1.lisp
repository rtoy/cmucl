;; Bootstrap to change the vops to use 32-bit registers for characters.
;; A basic cross-compile works too.

#+x86
(in-package :x86)

#+x86
(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'continue))))
  (defconstant byte-sc-names '(byte-reg))
  (defconstant dword-sc-names
    '(any-reg descriptor-reg sap-reg signed-reg unsigned-reg control-stack
      signed-stack unsigned-stack sap-stack single-stack constant
      base-char-reg base-char-stack)))