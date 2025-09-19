;;; -*- Package: ERRNO -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/errno.lisp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the UNIX low-level support, just enough to run
;;; CMUCL.
;;;
(in-package "ERRNO")
(intl:textdomain "cmucl-unix")

;;;; Errno stuff.

(eval-when (compile eval)

(defparameter *compiler-unix-errors* nil)

(defmacro def-unix-error (name number)
  `(progn
     (defconstant ,name ,number)
     (export ',name)))

) ;eval-when

;;; 
;;; From <errno.h>
;;; 
(def-unix-error ESUCCESS 0)

