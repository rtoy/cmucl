;;;
;;; Boot file for adding fwrappers, and making TRACE and PROFILE
;;; use fwrappers.  To bootstrap, copy this file to target:bootstrap.lisp
;;; using Pierre Mai's build scripts, and do a full build.
;;;

(in-package :user)

;;;
;;; Move FDEFN-OR-LOSE to KERNEL and export it from there.
;;;
(setf (fdefinition 'kernel::fdefn-or-lose)
      (fdefinition 'lisp::fdefn-or-lose))
(unintern 'cl::fdefn-or-lose :cl)
(export 'kernel::fdefn-or-lose :kernel)


(in-package :profile)

(defmacro defstruct! (name &rest stuff)
  `(handler-bind ((error (lambda (c)
			   (declare (ignore c))
			   (invoke-restart 'kernel::clobber-it))))
     (defstruct ,name ,@stuff)))

(defstruct! (profile-info
	     (:conc-name pi-)
	     (:constructor make-profile-info (function-name callers-p)))
  function-name
  (callers-p nil :type boolean)
  (count 0 :type fixnum)
  (time 0 :type time-type)
  (consed-h 0 :type dfixnum:dfparttype)
  (consed-l 0 :type dfixnum:dfparttype)
  (consed-w/c-h 0 :type dfixnum:dfparttype)
  (consed-w/c-l 0 :type dfixnum:dfparttype)
  (profile 0 :type integer)
  (callers () :type list))

;;; end of file
