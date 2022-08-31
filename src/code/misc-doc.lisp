;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/misc.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Documentation functions.  Needed by pclcom.lisp
(in-package "LISP")
(intl:textdomain "cmucl")

(export '(documentation))

;;; cobbled from stuff in describe.lisp.
(defun function-doc (x)
  (let ((name
	 (case (kernel:get-type x)
	   (#.vm:closure-header-type
	    (kernel:%function-name (%closure-function x)))
	   ((#.vm:function-header-type #.vm:closure-function-header-type)
	    (kernel:%function-name x))
	   (#.vm:funcallable-instance-header-type
	    (typecase x
	      (kernel:byte-function
	       (c::byte-function-name x))
	      (kernel:byte-closure
	       (c::byte-function-name (byte-closure-function x)))
	      (eval:interpreted-function
	       (multiple-value-bind 
		     (exp closure-p dname)
		   (eval:interpreted-function-lambda-expression x)
		 (declare (ignore exp closure-p))
		 dname))
	      (t ;; funcallable-instance
	       (kernel:%function-name
		(kernel:funcallable-instance-function x))))))))
    (when (and name (typep name '(or symbol cons)))
      (values (info function documentation name)))))

(defun documentation (x doc-type)
  "Returns the documentation string of Doc-Type for X, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T."
  (flet (;; CMUCL random-documentation.
	 (try-cmucl-random-doc (x doc-type)
	   (declare (symbol doc-type))
	   (cdr (assoc doc-type
		       (values (info random-documentation stuff x))))))
    (case doc-type
      (variable 
       (typecase x
	 (symbol (values (info variable documentation x)))))
      (function
       (typecase x
	 (symbol (values (info function documentation x)))
	 (function (function-doc x))
	 (list ;; Must be '(setf symbol)
	  (values (info function documentation (cadr x))))))
      (structure
       (typecase x
	 (symbol (when (eq (info type kind x) :instance)
		   (values (info type documentation x))))))
      (type
       (typecase x
	 (kernel::structure-class (values (info type documentation (%class-name x))))
	 (t (and (typep x 'symbol) (values (info type documentation x))))))
      (setf (info setf documentation x))
      ((t)
       (typecase x
	 (function (function-doc x))
	 (package (package-doc-string x))
	 (kernel::structure-class (values (info type documentation (%class-name x))))
	 (symbol (try-cmucl-random-doc x doc-type))))
      (t
       (typecase x
	 (symbol (try-cmucl-random-doc x doc-type)))))))

(defun (setf documentation) (string name doc-type)
  #-no-docstrings
  (case doc-type
    (variable
     #+nil
     (when string
       (%primitive print "Set variable text domain")
       (%primitive print (symbol-name name))
       (%primitive print intl::*default-domain*))
     (setf (info variable textdomain name) intl::*default-domain*)
     (setf (info variable documentation name) string))
    (function
     #+nil
     (when intl::*default-domain*
       (%primitive print "Set function text domain")
       (%primitive print (symbol-name name))
       (%primitive print intl::*default-domain*))
     (setf (info function textdomain name) intl::*default-domain*)
     (setf (info function documentation name) string))
    (structure
     (unless (eq (info type kind name) :instance)
       (error (intl:gettext "~S is not the name of a structure type.") name))
     (setf (info type textdomain name) intl::*default-domain*)
     (setf (info type documentation name) string))
    (type
     (setf (info type textdomain name) intl::*default-domain*)
     (setf (info type documentation name) string))
    (setf
     (setf (info setf textdomain name) intl::*default-domain*)
     (setf (info setf documentation name) string))
    (t
     (let ((pair (assoc doc-type (info random-documentation stuff name))))
       (if pair
	   (setf (cdr pair) string)
	   (push (cons doc-type string)
		 (info random-documentation stuff name))))))
  string)

