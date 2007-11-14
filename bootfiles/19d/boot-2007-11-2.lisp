(in-package :c)
(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'kernel::continue))))
  (defconstant policy-parameter-slots
    '((speed . cookie-speed) (space . cookie-space) (safety . cookie-safety)
      (cspeed . cookie-cspeed) (brevity . cookie-brevity)
      (debug . cookie-debug))))
