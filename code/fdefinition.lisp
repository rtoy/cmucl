;;; -*- Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/fdefinition.lisp,v 1.9 1991/11/05 16:51:20 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions that hack on the global function namespace (primarily
;;; concerned with SETF functions here).  Also, function encapsulation
;;; and routines that set and return definitions disregarding whether
;;; they might be encapsulated.
;;;
;;; Written by Rob MacLachlan
;;; Modified by Bill Chiles (wrote encapsulation stuff) 
;;;

(in-package "EXTENSIONS")

(export '(encapsulate unencapsulate encapsulated-p encapsulated-definition
	  argument-list basic-definition *setf-fdefinition-hook*))


(in-package "LISP")

(export '(fdefinition fboundp fmakunbound))



;;; Introduction:
;;;
;;; Definitions for symbol names are stored and accessed with SYMBOL-FUNCTION.
;;; Definitions for (SETF FOO) style names are stored and accessed in
;;; *setf-functions*.  ENCAPSULATED-DEFINITION gets the stored definition of
;;; any name, and it is SETF'able.
;;;
;;; FDEFINITION retrieves the original definition of a name before any
;;; encapsulations were installed.  When you SETF this form, you change the
;;; original definition leaving all encapsulations untouched.
;;;

(defvar *setf-functions* (make-hash-table :test #'equal))

(eval-when (compile eval)
(defmacro function-name-dispatch (name symbol-form setf-form)
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

) ;EVAL-WHEN

;;; CHECKING-SYMBOL-FUNCTION  --  Internal
;;;
;;;    Do a safe SYMBOL-FUNCTION.  The guts of functions in this file are
;;; normally compiled unsafe.
;;;
(declaim (inline checking-symbol-function))
(defun checking-symbol-function (x)
  (declare (optimize (safety 1)))
  (symbol-function x))


;;;; Definition Encapsulation.

;;; ENCAPSULATED-DEFINITION -- Public.
;;;
(defun encapsulated-definition (name)
  "Returns whatever definition is stored for name, regardless of whether it is
   encapsulated.  This is SETF'able."
  (function-name-dispatch name
    (checking-symbol-function name)
    (gethash (cadr name) *setf-functions*)))
;;;
(defun %set-encapsulated-definition (name value)
  (check-type value function)
  (function-name-dispatch name
    (setf (symbol-function name) value)
    (setf (gethash (cadr name) *setf-functions*) value)))
;;;
(defsetf encapsulated-definition %set-encapsulated-definition)


(defstruct (encapsulation-info (:print-function print-encapsulation-info)
			       (:constructor make-encapsulation-info
					     (type definition next)))
  ;; This is definition's encapsulation type.  The encapsulated definition is
  ;; in the previous encapsulation-info element or installed as the global
  ;; definition of some function name.
  type
  ;; Previous definition.  This used to be installed as a global definition
  ;; for some function name, but it was replaced by an encapsulation of type
  ;; type.
  (definition nil :type function)
  ;; If definition is an encapsulation, then this points to the information
  ;; about it (what's its type and what definition was encapsulated).
  (next nil :type (or null encapsulation-info)))
;;;
(defun print-encapsulation-info (obj str n)
  (declare (ignore n))
  (format str "#<Encapsulation-Info  Definition: ~S  Type: ~S>"
	  (%function-header-name (encapsulation-info-definition obj))
	  (encapsulation-info-type obj)))

;;; This maps function names to encapsulation-infos.
;;;
(defvar *encapsulation-info* (make-hash-table :test #'equal))

;;; ENCAPSULATE -- Public.
;;;
;;; We must bind and close over info.  Consider the case where we encapsulate
;;; (the second) an encapsulated (the first) definition, and later someone
;;; unencapsulates the encapsulated (first) definition.  We don't want our
;;; encapsulation (second) to bind basic-definition to the encapsulated (first)
;;; definition when it no longer exists.  When unencapsulating, we make sure to
;;; clobber the appropriate info structure to allow basic-definition to be
;;; bound to the next definition instead of an encapsulation that no longer
;;; exists.
;;;
(defun encapsulate (name type body)
  "Replaces the definition of name with a function that binds name's arguments
   a variable named argument-list, binds name's definition to a variable named
   basic-definition, and EVAL's body in that context.  Type is whatever you
   would like to associate with this encapsulation for identification in case
   you need multiple encapsuations of the same name."
  (unless (fboundp name)
    (error "~S has no function definition." name))
  (let ((info (make-encapsulation-info type (encapsulated-definition name)
				       (gethash name *encapsulation-info*))))
    (setf (gethash name *encapsulation-info*) info)
    (setf (encapsulated-definition name)
	  #'(lambda (&rest argument-list)
	      (declare (special argument-list))
	      (let ((basic-definition (encapsulation-info-definition info)))
		(declare (special basic-definition))
		(eval body)))))
  name)

;;; UNENCAPSULATE -- Public.
;;;
;;; When removing an encapsulation, we must remember that encapsulating
;;; definitions close over a reference to the encapsulation-info that describes
;;; the encapsulating definition.  When you find an info with the target type,
;;; the previous info in the chain has the ensulating definition of that type.
;;; We take the encapsulated definition from the info with the target type, and
;;; we store it in the previous info structure whose encapsulating definition
;;; it describes looks to this previous info structure for a definition to
;;; bind (see ENCAPSULATE).  Then we store the next pointer from the info with
;;; the target type into the next slot of the previous info structure.  When
;;; removing the first info structure, we do something conceptually equal, but
;;; mechanically it is different.
;;;
(defun unencapsulate (name type)
  "Removes name's most recent encapsulation of the specified type."
  (let ((encap-info (gethash name *encapsulation-info*)))
    (declare (type (or encapsulation-info null) encap-info))
    (cond ((not encap-info))
	  ;; Is it the first one?
	  ((eq (encapsulation-info-type encap-info) type)
	   (setf (encapsulated-definition name)
		 (encapsulation-info-definition encap-info))
	   (setf (gethash name *encapsulation-info*)
		 (encapsulation-info-next encap-info)))
	  (t
	   (let ((prev encap-info))
	     (setf encap-info (encapsulation-info-next encap-info))  
	     (loop
	       (unless encap-info (return))
	       (when (eq (encapsulation-info-type encap-info) type)
		 (setf (encapsulation-info-definition prev)
		       (encapsulation-info-definition encap-info))
		 (setf (encapsulation-info-next prev)
		       (encapsulation-info-next encap-info))
		 (return))
	       (setf prev encap-info)
	       (setf encap-info (encapsulation-info-next encap-info)))))))
  t)

;;; ENCAPSULATED-P -- Public.
;;;
(defun encapsulated-p (name type)
  "Returns t if name has an encapsulation of the given type, otherwise nil."
  (let ((encap-info (gethash name *encapsulation-info*)))
    (if encap-info
	(loop
	  (when (not (encapsulation-info-next encap-info))
	    (return nil))
	  (when (eq (encapsulation-info-type encap-info) type)
	    (return t))
	  (setf encap-info (encapsulation-info-next encap-info))))))



;;;; FDEFINITION.

(defun fdefinition (name)
  "Return name's global function definition taking care to regard any
   encapsulations and to return the innermost encapsulated definition.
   This is SETF'able."
  (macrolet ((basic-def (name fetch)
	       `(let ((encap-info (gethash ,name *encapsulation-info*)))
		  (if encap-info
		      (loop
			(when (not (encapsulation-info-next encap-info))
			  (return (encapsulation-info-definition encap-info)))
			(setf encap-info (encapsulation-info-next encap-info)))
		      ,fetch))))
    (function-name-dispatch name
      (basic-def name (checking-symbol-function name))
      (basic-def name (or (gethash (cadr name) *setf-functions*)
			  (error "Undefined function: ~S." name))))))

(defvar *setf-fdefinition-hook* nil
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value.  These functions take the function name and the new value.")

(defun %set-fdefinition (name new-value)
  "Set name's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (macrolet ((set-basic-def (name new-value form)
	       `(let ((encap-info (gethash ,name *encapsulation-info*)))
		  (cond (encap-info
			 (loop
			   (when (not (encapsulation-info-next encap-info))
			     (dolist (f *setf-fdefinition-hook*)
			       (funcall f ,name ,new-value))
			     (return
			      (setf (encapsulation-info-definition encap-info)
				    ,new-value)))
			   (setf encap-info
				 (encapsulation-info-next encap-info))))
			(t
			 (dolist (f *setf-fdefinition-hook*)
			   (funcall f ,name ,new-value))
			 (setf ,form ,new-value))))))
    (function-name-dispatch name
      (set-basic-def name new-value (symbol-function name))
      (set-basic-def name new-value (gethash (cadr name) *setf-functions*)))))
;;;
(defsetf fdefinition %set-fdefinition)



;;;; FBOUNDP and FMAKUNBOUND.

(defun fboundp (name)
  "Return true if name has a global function definition."
  (function-name-dispatch name
    (fboundp (the symbol name))
    (functionp (gethash (cadr name) *setf-functions*))))

(defun fmakunbound (name)
  "Make Name have no global function definition."
  (function-name-dispatch name
    (fmakunbound (the symbol name))
    (remhash (cadr name) *setf-functions*))
  t)
