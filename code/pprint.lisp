;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; CMU Common Lisp pretty printer.
;;; Written by Skef Wholey.
;;; Modified by Todd Kaufmann, Bill Chiles, Rob Maclachlan, William Lott.
;;;

(in-package "LISP")
(export '(pprint))


(in-package "EXTENSIONS")
(export '(grindef))
(in-package "LISP")

(defvar *pprint-initialized* nil)


(defun pprint (object &optional stream)
  "Prettily outputs the Object preceded by a newline and followed by a space."
  (let ((*print-pretty* t))
    (output-pretty-object object stream))
  (values))

;;; OUTPUT-PRETTY-OBJECT is called by WRITE, PRIN1, PRINC, and their associated
;;; ...-TO-STRING forms when *print-pretty* is non-nil.  Calling this when
;;; *print-pretty* is nil could cause XP and our system to recursively call
;;; each other for a very long time.  Stream has already been set correctly
;;; according the semantics in the manual with respect to t and nil.
;;;
(defun output-pretty-object (object stream)
  (assert *print-pretty*)
  (if (and *pprint-initialized*
	   (typep object '(or list structure vector array)))
      (xp::basic-write object stream)
      (let ((*print-pretty* nil))
	(output-object object stream))))


#|

(defun pretty-lambda-to-defun (name lambda &optional arglist)
  `(defun ,name ,(or arglist (cadr lambda))
     ,@(if (and (null (cdddr lambda)) (listp (caddr lambda))
		(eq (caaddr lambda) 'block))
	   (cddr (caddr lambda))
	   (cddr lambda))))

(defmacro grindef (function-name)
 "Prettily prints the definition of the function whose name is Function-Name."
 (if (and (symbolp function-name) (fboundp function-name))
     (let ((stuff (symbol-function function-name)))
       (if (and (listp stuff) (listp (cdr stuff)))
	   (case (car stuff)
	     (lambda `(pprint ',(pretty-lambda-to-defun function-name stuff)))
	     (macro `(pprint ',(pretty-lambda-to-defun function-name (cdr stuff)
						       '(&rest **macroarg**))))
	     (t `(pprint '(setf (symbol-function ,function-name) ',stuff))))
	   `(pprint '(setf (symbol-function ,function-name) ',stuff))))
     nil))

|#

;;; Tab-Over prints the specified number of spaces on *Standard-Output*.
;;; Taken from the old pretty printer.  Needed by some function in filesys.
(defconstant maximum-pp-indentation 70)
(defconstant pp-indentation-string (make-string 70 :initial-element #\space))

(defun tab-over (indent-pos)
  (write-string pp-indentation-string *standard-output*
		:start 0
		:end (min indent-pos maximum-pp-indentation)))

;;; Initialize Water's pretty printer.
(defun pprint-init ()
  (xp::install :package "XP" :shadow nil)
  (setf *pprint-initialized* t)
  (setf *print-pretty* t))

