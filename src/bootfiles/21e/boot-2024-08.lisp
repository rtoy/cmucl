;; Bootstrap file for the ef-octet-count changes.  Just need to change
;; the value of +ef-max+

(in-package "STREAM")

(handler-bind
    ((error (lambda (c)
	      (declare (ignore c))
	      (invoke-restart 'continue))))
  (defconstant +ef-max+ 14))

;;; Bootstrap for adding %local-nicknames to package structure.
(in-package :lisp)

(intern "PACKAGE-LOCAL-NICKNAMES" "LISP")
(intern	"ADD-PACKAGE-LOCAL-NICKNAME" "LISP")
(intern	"REMOVE-PACKAGE-LOCAL-NICKNAME" "LISP")
(intern	"PACKAGE-LOCALLY-NICKNAMED-BY-LIST" "LISP")

;; Make sure we don't accidentally load fasls from somewhere.
(setf (ext:search-list "target:")
      '("src/"))

;; Ensure all packages have been set up, since package definition is broken
;; once this file has been loaded:
(load "target:code/exports-errno")
(load "target:code/exports")

(setf *enable-package-locked-errors* nil)

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
  (tables (list nil))	; A list of all the hashtables for inherited symbols.
  (%name nil :type (or simple-string null))
  (%nicknames () :type list)
  (%use-list () :type list)
  (%used-by-list () :type list)
  (internal-symbols (required-argument) :type package-hashtable)
  (external-symbols (required-argument) :type package-hashtable)
  (%shadowing-symbols () :type list)
  (lock nil :type boolean)
  (definition-lock nil :type boolean)
  (%local-nicknames () :type list)
  (doc-string nil :type (or simple-string null)))

;; Need to define this with the extra arg because compiling pcl uses
;; defpackage and we need this defined.  This isn't the actual
;; implementation; we just added the extra arg.
(defun %defpackage (name nicknames size shadows shadowing-imports
			 use imports interns exports doc-string &optional local-nicknames)
  (declare (type simple-base-string name)
	   (type list nicknames local-nicknames shadows shadowing-imports
		 imports interns exports)
	   (type (or list (member :default)) use)
	   (type (or simple-base-string null) doc-string))
  (let ((package (or (find-package name)
		     (progn
		       (when (eq use :default)
			 (setf use *default-package-use-list*))
		       (make-package name
				     :use nil
				     :internal-symbols (or size 10)
				     :external-symbols (length exports))))))
    (unless (string= (the string (package-name package)) name)
      (error 'simple-package-error
	     :package name
	     :format-control (intl:gettext "~A is a nick-name for the package ~A")
	     :format-arguments (list name (package-name name))))
    (enter-new-nicknames package nicknames)
    ;; Shadows and Shadowing-imports.
    (let ((old-shadows (package-%shadowing-symbols package)))
      (shadow shadows package)
      (dolist (sym-name shadows)
	(setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
      (dolist (simports-from shadowing-imports)
	(let ((other-package (package-or-lose (car simports-from))))
	  (dolist (sym-name (cdr simports-from))
	    (let ((sym (find-or-make-symbol sym-name other-package)))
	      (shadowing-import sym package)
	      (setf old-shadows (remove sym old-shadows))))))
      (when old-shadows
	(warn (intl:gettext "~A also shadows the following symbols:~%  ~S")
	      name old-shadows)))
    ;; Use
    (unless (eq use :default)
      (let ((old-use-list (package-use-list package))
	    (new-use-list (mapcar #'package-or-lose use)))
	(use-package (set-difference new-use-list old-use-list) package)
	(let ((laterize (set-difference old-use-list new-use-list)))
	  (when laterize
	    (unuse-package laterize package)
	    (warn (intl:gettext "~A previously used the following packages:~%  ~S")
		  name
		  laterize)))))
    ;; Import and Intern.
    (dolist (sym-name interns)
      (intern sym-name package))
    (dolist (imports-from imports)
      (let ((other-package (package-or-lose (car imports-from))))
	(dolist (sym-name (cdr imports-from))
	  (import (list (find-or-make-symbol sym-name other-package))
		  package))))
    ;; Exports.
    (let ((old-exports nil)
	  (exports (mapcar #'(lambda (sym-name) (intern sym-name package))
			   exports)))
      (do-external-symbols (sym package)
	(push sym old-exports))
      (export exports package)
      (let ((diff (set-difference old-exports exports)))
	(when diff
	  (warn (intl:gettext "~A also exports the following symbols:~%  ~S")
		name diff))))
    ;; Documentation
    (setf (package-doc-string package) doc-string)
    package))
