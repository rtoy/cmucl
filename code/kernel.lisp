;;; -*- Log: code.log; Package: KERNEL -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/kernel.lisp,v 1.10 1993/08/21 08:16:06 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/kernel.lisp,v 1.10 1993/08/21 08:16:06 ram Exp $
;;;    
(in-package "KERNEL")

(export '(allocate-vector make-array-header function-subtype))


(defun get-header-data (x)
  "Return the 24 bits of data in the header of object X, which must be an
  other-pointer object."
  (get-header-data x))

(defun set-header-data (x val)
  "Sets the 24 bits of data in the header of object X (which must be an
  other-pointer object) to VAL."
  (set-header-data x val))

(defun get-closure-length (x)
  "Returns the length of the closure X.  This is one more than the number
  of variables closed over."
  (get-closure-length x))

(defun get-lowtag (x)
  "Returns the three-bit lowtag for the object X."
  (get-lowtag x))

(defun get-type (x)
  "Returns the 8-bit header type for the object X."
  (get-type x))

(defun vector-sap (x)
  "Return a System-Area-Pointer pointing to the data for the vector X, which
  must be simple."
  (declare (type (simple-unboxed-array (*)) x))
  (vector-sap x))


(defun c::binding-stack-pointer-sap ()
  "Return a System-Area-Pointer pointing to the end of the binding stack."
  (c::binding-stack-pointer-sap))

(defun c::dynamic-space-free-pointer ()
  "Returns a System-Area-Pointer pointing to the next free work of the current
  dynamic space."
  (c::dynamic-space-free-pointer))

(defun c::control-stack-pointer-sap ()
  "Return a System-Area-Pointer pointing to the end of the control stack."
  (c::control-stack-pointer-sap))

(defun function-subtype (function)
  "Return the header typecode for FUNCTION.  Can be set with SETF."
  (function-subtype function))

(defun (setf function-subtype) (type function)
  (setf (function-subtype function) type))

(defun %function-arglist (func)
  "Extracts the arglist from the function header FUNC."
  (%function-arglist func))

(defun %function-name (func)
  "Extracts the name from the function header FUNC."
  (%function-name func))

(defun %function-type (func)
  "Extracts the type from the function header FUNC."
  (%function-type func))

(defun %closure-function (closure)
  "Extracts the function from CLOSURE."
  (%closure-function closure))

(defun c::vector-length (vector)
  "Return the length of VECTOR.  There is no reason to use this, 'cause
  (length (the vector foo)) is the same."
  (c::vector-length vector))

(defun %sxhash-simple-string (string)
  "Return the SXHASH for the simple-string STRING."
  (%sxhash-simple-string string))

(defun %sxhash-simple-substring (string length)
  "Return the SXHASH for the first LENGTH characters of the simple-string
  STRING."
  (%sxhash-simple-substring string length))

(defun %closure-index-ref (closure index)
  "Extract the INDEXth slot from CLOSURE."
  (%closure-index-ref closure index))


(defun allocate-vector (type length words)
  "Allocate a unboxed, simple vector with type code TYPE, length LENGTH, and
  WORDS words long.  Note: it is your responsibility to assure that the
  relation between LENGTH and WORDS is correct."
  (allocate-vector type length words))

(defun make-array-header (type rank)
  "Allocate an array header with type code TYPE and rank RANK."
  (make-array-header type rank))


(defun code-instructions (code-obj)
  "Return a SAP pointing to the instructions part of CODE-OBJ."
  (code-instructions code-obj))

(defun code-header-ref (code-obj index)
  "Extract the INDEXth element from the header of CODE-OBJ.  Can be set with
  setf."
  (code-header-ref code-obj index))

(defun code-header-set (code-obj index new)
  (code-header-set code-obj index new))

(defsetf code-header-ref code-header-set)


(defun %raw-bits (object offset)
  (declare (type index offset))
  (kernel:%raw-bits object offset))

(defun %set-raw-bits (object offset value)
  (declare (type index offset) (type (unsigned-byte #.vm:word-bits) value))
  (setf (kernel:%raw-bits object offset) value))

(defsetf %raw-bits %set-raw-bits)

(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
(defun single-float-bits (x) (single-float-bits x))
(defun double-float-high-bits (x) (double-float-high-bits x))
(defun double-float-low-bits (x) (double-float-low-bits x))
