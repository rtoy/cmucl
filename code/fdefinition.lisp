;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.4 1991/02/08 13:32:37 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.4 1991/02/08 13:32:37 ram Exp $
;;;
;;;    Functions that hack on the global function namespace (primarily
;;; concerned with SETF functions here.)
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "LISP")
(export '(fdefinition fboundp fmakunbound))

(defvar *setf-functions* (make-hash-table :test #'equal))

(eval-when (compile eval)

;;; With-Function-Name  --  Internal
;;;
(defmacro with-function-name (name symbol-form setf-form)
  `(typecase ,name
     (symbol ,symbol-form)
     (cons
      (unless (and (eq (car ,name) 'setf)
		   (consp (cdr ,name))
		   (symbolp (cadr ,name)))
	(error "Malformed function name: ~S." ,name))
      ,setf-form)
     (t
      (error "Malformed function name: ~S." ,name))))

); Eval-When (Compile Eval)

#+new-compiler
(defun careful-symbol-function (name)
  (symbol-function name))

#+new-compiler
(defun set-symbol-function-carefully (name value)
  (setf (symbol-function name) value))

(defun fdefinition (name)
  "Return Name's global function definition."
  (with-function-name name
    (careful-symbol-function name)
    (or (gethash (cadr name) *setf-functions*)
	(error "Undefined function: ~S." name))))

(defsetf fdefinition %set-fdefinition)

(defun %set-fdefinition (name new-value)
  "Set Name's global function definition."
  (declare (type function new-value))
  (with-function-name name
    (set-symbol-function-carefully name new-value)
    (setf (gethash (cadr name) *setf-functions*) new-value)))

#+new-compiler
(defun fboundp (name)
  "Return true if Name has a global function definition."
  (with-function-name name
    (fboundp (the symbol name))
    (functionp (gethash (cadr name) *setf-functions*))))

#+new-compiler
(defun fmakunbound (name)
  "Make Name have no global function definition."
  (with-function-name name
    (fmakunbound (the symbol name))
    (remhash (cadr name) *setf-functions*))
  t)
