;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
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

(in-package :pcl)

(eval-when (compile load eval)
(defparameter checking-or-caching-list
  '((T NIL (CLASS) NIL)
    (T NIL (CLASS CLASS) NIL)
    (T NIL (CLASS CLASS CLASS) NIL)
    (T NIL (CLASS CLASS T) NIL)
    (T NIL (CLASS CLASS T T) NIL)
    (T NIL (CLASS CLASS T T T) NIL)
    (T NIL (CLASS T) NIL)
    (T NIL (CLASS T T) NIL)
    (T NIL (CLASS T T T) NIL)
    (T NIL (CLASS T T T T) NIL)
    (T NIL (CLASS T T T T T) NIL)
    (T NIL (CLASS T T T T T T) NIL)
    (T NIL (T CLASS) NIL)
    (T NIL (T CLASS T) NIL)
    (T NIL (T T CLASS) NIL)
    (T NIL (CLASS) T)
    (T NIL (CLASS CLASS) T)
    (T NIL (CLASS T) T)
    (T NIL (CLASS T T) T)
    (T NIL (CLASS T T T) T)
    (T NIL (T CLASS) T)
    (T T (CLASS) NIL)
    (T T (CLASS CLASS) NIL)
    (T T (CLASS CLASS CLASS) NIL)
    (NIL NIL (CLASS) NIL)
    (NIL NIL (CLASS CLASS) NIL)
    (NIL NIL (CLASS CLASS T) NIL)
    (NIL NIL (CLASS CLASS T T) NIL)
    (NIL NIL (CLASS T) NIL)
    (NIL NIL (T CLASS T) NIL)
    (NIL NIL (CLASS) T)
    (NIL NIL (CLASS CLASS) T))))

(defmacro make-checking-or-caching-function-list ()
  `(list ,@(mapcar #'(lambda (key)
		       `(cons ',key (emit-checking-or-caching-macro ,@key)))
		   checking-or-caching-list)))

(defun initialize-checking-or-caching-function-list ()
  (setq checking-or-caching-function-list
	(make-checking-or-caching-function-list)))

(initialize-checking-or-caching-function-list)
