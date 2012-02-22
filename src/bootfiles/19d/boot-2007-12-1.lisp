(in-package "VM")
(handler-bind ((error (lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'continue))))
  (defconstant max-bits (1- (ash 1 vm:word-bits))
    "The maximum number of bits that can be delt with during a single call."))

