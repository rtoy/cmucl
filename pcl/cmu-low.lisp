;;;-*-Mode:LISP; Package:PCL; Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;; 
;;; This is the CMU Lisp version of the file low.
;;; 

(in-package 'pcl)


;;; Just use our without-interrupts.  We don't have the INTERRUPTS-ON/OFF local
;;; macros spec'ed in low.lisp, but they aren't used.
;;;
(defmacro without-interrupts (&rest stuff)
  `(sys:without-interrupts ,@stuff))


;;; Print the object addr in default printers.
;;;
(defun printing-random-thing-internal (thing stream)
  (format stream "{~X}" (sys:%primitive c:make-fixnum thing)))


(eval-when (compile load eval)
  (c:def-source-transform std-instance-p (x)
    (ext:once-only ((n-x x))
      `(and (ext:structurep ,n-x)
	    (eq (kernel:structure-ref ,n-x 0) 'std-instance)))))

  ;;   
;;;;;; Cache No's
  ;;  

(proclaim '(inline object-cache-no))

(defun object-cache-no (symbol mask)
  (logand (ext:truly-the fixnum (system:%primitive make-fixnum symbol))
	  (the fixnum mask)))


(defun function-arglist (fcn)
  "Returns the argument list of a compiled function, if possible."
  (cond ((symbolp fcn)
	 (when (fboundp fcn)
	   (function-arglist (symbol-function fcn))))
	((eval:interpreted-function-p fcn)
	 (eval:interpreted-function-arglist fcn))
	((functionp fcn)
	 (let ((lambda-expr (function-lambda-expression fcn)))
	   (if lambda-expr
	       (cadr lambda-expr)
	       (let ((function (kernel:%closure-function fcn)))
		 (values (read-from-string
			  (kernel:%function-header-arglist function)))))))))


;;; We have this here and in fin.lisp, 'cause PCL wants to compile this
;;; file first.
;;; 
(defsetf funcallable-instance-name set-funcallable-instance-name)

;;; And returns the function, not the *name*.
(defun set-function-name (fcn new-name)
  "Set the name of a compiled function object."
  (cond ((symbolp fcn)
	 (set-function-name (symbol-function fcn) new-name))
	((funcallable-instance-p fcn)
	 (setf (funcallable-instance-name fcn) new-name)
	 fcn)
	((eval:interpreted-function-p fcn)
	 (setf (eval:interpreted-function-name fcn) new-name)
	 fcn)
	(t
	 (let ((header (kernel:%closure-function fcn)))
	   (system:%primitive c::set-function-name header new-name))
	 fcn)))

(in-package "C")


(defun keyword-spec-name (x)
  (if (atom x)
      (intern (symbol-name x) (find-package "KEYWORD"))
      (let ((cx (car x)))
	(if (atom cx)
	    (intern (symbol-name cx) (find-package "KEYWORD"))
	    (car cx)))))
		  

(defun generic-function-type-from-lambda-list (name ll)
  (multiple-value-bind (req opt restp ignore keyp keys allowp)
		       (parse-lambda-list ll)
    (declare (ignore ignore))
    (let* ((old (info function type name))
	   (old-ftype (if (function-type-p old) old nil))
	   (old-keys (and old-ftype
			  (mapcar #'key-info-name
				  (function-type-keywords old-ftype)))))
      `(function (,@(make-list (length req) :initial-element t)
		  ,@(when opt
		      `(&optional ,@(make-list (length opt)
					       :initial-element t)))
		  ,@(when (or (and old-ftype (function-type-rest old-ftype))
			      restp)
		      '(&rest t))
		  ,@(when (or (and old-ftype (function-type-keyp old-ftype))
			      keyp)
		      '(&key))
		  ,@(mapcar #'(lambda (name)
				`(,name t))
			    (union old-keys
				   (mapcar #'keyword-spec-name keys)))
		  ,@(when (or (and old-ftype (function-type-allowp old-ftype))
			      allowp)
		      '(&allow-other-keys)))
		 *))))
