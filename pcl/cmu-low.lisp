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
	 (eval:interpreted-function-name fcn))
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

(defun set-function-name (fcn new-name)
  "Set the name of a compiled function object."
  (cond ((symbolp fcn)
	 (set-function-name (symbol-function fcn) new-name))
	((funcallable-instance-p fcn)
	 (setf (funcallable-instance-name fcn) new-name)
	 fcn)
	(t
	 (let ((header (kernel:%closure-function fcn)))
	   (system:%primitive c::set-function-name header
			      (if (symbolp new-name)
				  new-name
				  (let ((*package* *the-pcl-package*)
					(*print-case* :upcase)
					(*print-gensym* 't))
				    (prin1-to-string new-name)))))
	 fcn)))
