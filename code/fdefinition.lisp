;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.1.1.2 1990/04/20 00:36:19 wlott Exp $
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
    (functionp (%primitive fast-symbol-function name))
    (functionp (gethash (cadr name) *setf-functions*))))

#+new-compiler
(defvar *the-undefined-function*)

#+new-compiler
(defun fmakunbound (name)
  "Make Name have no global function definition."
  (with-function-name name
    (%primitive set-symbol-function name *the-undefined-function*)
    (remhash (cadr name) *setf-functions*))
  t)
