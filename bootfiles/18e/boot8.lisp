;;;
;;; Bootfile for adding package locks.
;;; Use this file as bootstrap.lisp using Pierre Mai's build scripts,
;;;

(in-package :lisp)

(defvar *enable-package-locked-errors* nil)

;;;
;;; Like DEFSTRUCT, but silently clobber old definitions.
;;;
(defmacro defstruct! (name &rest stuff)
  `(handler-bind ((error (lambda (c)
			   (declare (ignore c))
			   (invoke-restart 'kernel::clobber-it))))
     (defstruct ,name ,@stuff)))


(defstruct! (package
	    (:constructor internal-make-package)
	    (:predicate packagep)
	    (:print-function %print-package)
	    (:make-load-form-fun
	     (lambda (package)
	       (values `(package-or-lose ',(package-name package))
		       nil))))
  (tables (list nil))
  (%name nil :type (or simple-string null))
  (%nicknames () :type list)
  (%use-list () :type list)
  (%used-by-list () :type list)
  (internal-symbols (required-argument) :type package-hashtable)
  (external-symbols (required-argument) :type package-hashtable)
  (%shadowing-symbols () :type list)
  (lock nil :type boolean)
  (definition-lock nil :type boolean)
  (doc-string nil :type (or simple-string null)))


;;; end of file
