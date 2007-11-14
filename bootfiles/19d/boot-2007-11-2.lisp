;;; Bootstrap file for removing the FLOAT-ACCURACY stuff.
;;;
;;; For x86, I think you need to do a cross-compile.  Use this file as
;;; the cross-compile script.  For other architectures, I don't think
;;; you need to do a cross-compile.  Just a normal build with this as
;;; the bootstrap file will work.

#+x86
(load "target:bootfiles/19d/boot-2007-11-1")

(in-package :c)
(handler-bind ((error (lambda (c)
			(declare (ignore c))
			(invoke-restart 'kernel::continue))))
  (defconstant policy-parameter-slots
    '((speed . cookie-speed) (space . cookie-space) (safety . cookie-safety)
      (cspeed . cookie-cspeed) (brevity . cookie-brevity)
      (debug . cookie-debug))))

#+x86
(load "target:tools/cross-scripts/cross-x86-x86") 
