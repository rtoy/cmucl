;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file contains the DEFMACRO function that is part of the
;;; standard Spice Lisp environment.  For the version that runs in
;;; Maclisp, see file DEFMACRO.LSP.
;;;
;;; Written by Scott Fahlman.
;;;
;;; Ugly code, since I can't create macros here and need to stay close to
;;; Maclisp, so that it will be easy to create a derivitive version to use
;;; in the cross compiler.  Even without this, there's so much going on
;;; in the arglist that the code has to be hairy.
;;;
(in-package 'lisp)
(export '(defmacro deftype))

;;; The following specials are used for communication during argument-list
;;; parsing for a macro or macro-like form.

(proclaim '(special %arg-count %min-args %restp %let-list
		    %keyword-tests *keyword-package*
		    %env-arg-name %env-arg-used))

;;; The following is an ugly way of getting an optional arg passed in to
;;; Analyze1.  Bootstrapping problems in Maclisp force me to do this.

(defvar *default-default* nil)
(defvar *key-finder* 'find-keyword)

;;; Parse-Defmacro  --  Semi-Public
;;;
;;;    Provides a clean interface to ANALYZE1
;;;
(defun parse-defmacro (arglist whole code errloc &key (path `(cdr ,whole))
				((:environment %env-arg-name)) error-string
				(doc-string-allowed t)
				((:default-default *default-default*) nil)
				((:key-finder *key-finder*) 'find-keyword))
  "For use by macros and macro-like forms that must parse some form
  according to a defmacro-like argument list, ARGLIST.  The first value
  returned is a LET* form which binds things and then evalutes the
  specified CODE.  WHOLE is the variable which is bound to the entire
  arglist, or NIL if &whole is illegal.  ERRLOC is the name of the function
  being worked on, for use in error messages.  The second value is a list
  of ignore declarations for the WHOLE and ENVIRONMENT vars, if appropriate.

  PATH is an access expression for getting to the object to be parsed,
  which defaults to the CDR of WHOLE.
  
  ENVIRONMENT is the place where the macroexpansion environment
  may be found.  If not supplied, then no &environment arg is allowed.

  ERROR-STRING is used as the argument to error if an incorrect number of
  arguments are supplied.  The additional error arguments are ERRLOC and
  the number of arguments supplied.  If not supplied, then no argument count
  error checking is done.

  DOC-STRING-ALLOWED indicates whether a doc-string should be parsed out of
  the body.  If one is found, it is returned as the third value.
  
  DEFAULT-DEFAULT is the default value for unsupplied arguments, which defaults
  to NIL.

  KEY-FINDER the function used to do keyword lookup.  It defaults to a function
  that does the right thing.

  The fourth and fifth values are the minimum and maximum number of arguments
  allowed, in case you care about that kind of thing.  The fifth value is NIL
  if there is no upper limit."
  (multiple-value-bind (body local-decs doc)
		       (parse-body code nil doc-string-allowed)

    (let ((%arg-count 0) (%min-args 0)
	  (%restp nil) (%let-list nil)
	  (%keyword-tests nil)
	  (%env-arg-used nil))
      (analyze1 arglist path errloc whole)
    
      (let ((arg-test (if error-string (defmacro-arg-test whole)))
	    (body
	     `(let* ,(nreverse %let-list)
		,@ local-decs
		(progn
		 ,@ %keyword-tests
		 ,@ body))))
	(values
	 (if arg-test
	     `(if ,arg-test
		  (error ,error-string ',errloc (length ,path))
		  ,body)
	     body)
	 ;; Wrong if no error check and arglist composed entirely of &environment
	 ;; args, but anyone who does that deserves to lose...
	 `(,@(unless (or arg-test arglist) `((declare (ignore ,whole))))
	   ,@(when (and %env-arg-name (not %env-arg-used))
	       `((declare (ignore ,%env-arg-name)))))
	 doc
	 %min-args
	 (if %restp nil %arg-count))))))

;;; ANALYZE1 is implemented as a finite-state machine that steps
;;; through the legal parts of an arglist in order: required, optional,
;;; rest, key, and aux.  The results are accumulated in a set of special
;;; variables: %let-list, %arg-count, %min-args, %restp, and %keyword-tests.
;;;
;;; ANALYZE1 is called by ANALYZE-ARGLIST to do the work for required and
;;; optional args.  It calls other functions if &rest, &key, or &aux are
;;; encountered.

(defun analyze1 (arglist path errloc whole)
  (do ((args arglist (cdr args))
       (optionalp nil)
       a temp)
      ((atom args)
       (cond ((null args) nil)
	     ;; Varlist is dotted, treat as &rest arg and exit.
	     (t (push (list args path) %let-list)
		(setq %restp t))))
    (setq a (car args))
    (cond ((eq a '&whole)
	   (cond ((and whole (cdr args) (symbolp (cadr args)))
		  (push (list (cadr args) whole) %let-list)
		  (setq %restp t)
		  (setq args (cdr args)))
		 (t (error "Illegal or ill-formed &whole arg in ~S." errloc))))
	  ((eq a '&environment)
	   (cond ((and %env-arg-name (cdr args) (symbolp (cadr args)))
		  (push `(,(cadr args) ,%env-arg-name) %let-list)
		  (setq %env-arg-used t)
		  (setq args (cdr args)))
		 (t (error "Illegal or ill-formed &environment arg in ~S."
			   errloc))))
	  ((eq a '&optional)
	   (and optionalp
		(cerror "Ignore it."
			"Redundant &optional flag in varlist of ~S." errloc))
	   (setq optionalp t))
	  ((or (eq a '&rest) (eq a '&body))
	   (return (analyze-rest (cdr args) path errloc whole)))
	  ((eq a '&key)
	   ;; Create a rest-arg, then do keyword analysis.
	   (setq temp (gensym))
	   (setq %restp t)
	   (push (list temp path) %let-list)
	   (return (analyze-key (cdr args) temp errloc)))
	  ((eq a '&allow-other-keys)
	   (cerror "Ignore it."
		   "Stray &ALLOW-OTHER-KEYS in arglist of ~S." errloc))
	  ((eq a '&aux)
	   (return (analyze-aux (cdr args) errloc)))
	  ((not optionalp)
	   (setq %min-args (1+ %min-args))
	   (setq %arg-count (1+ %arg-count))
	   (cond ((symbolp a)
		  (push `(,a (car ,path)) %let-list))
		 ((atom a)
		  (cerror "Ignore this item."
			  "Non-symbol variable name in ~S." errloc))
		 (t (let ((%min-args 0) (%arg-count 0) (%restp nil)
			  (new-whole (gensym)))
		      (push (list new-whole `(car ,path)) %let-list)
		      (analyze1 a new-whole errloc new-whole))))
	   (setq path `(cdr ,path)))
	  ;; It's an optional arg.
	  (t (setq %arg-count (1+ %arg-count))
	     (cond ((symbolp a)
		    ;; Just a symbol.  Bind to car of path or default.
		    (push `(,a (cond (,path (car ,path))
				     (t ,*default-default*)))
			  %let-list))
		   ((atom a)
		    (cerror "Ignore this item."
			    "Non-symbol variable name in ~S." errloc))
		   ((symbolp (car a))
		    ;; Car of list is a symbol.  Bind to car of path or
		    ;; to default value.
		    (push `(,(car a)
			    (cond (,path (car ,path))
				  (t ,(cond ((> (length a) 1) (cadr a))
					    (t *default-default*)))))
			  %let-list)
		    ;; Handle supplied-p variable, if any.
		    (and (> (length a) 2)
			 (push `(,(caddr a) (not (null ,path))) %let-list)))
		   ;; Then destructure arg against contents of this gensym.
		   (t (setq temp (gensym))
		      (push `(,temp
			      (cond (,path (car ,path))
				    (t ,(cond ((cddr a) (cadr a))
					      (t *default-default*)))))
			    %let-list)
		      (let ((%min-args 0) (%arg-count 0) (%restp nil))
			(analyze1 (car a) temp errloc nil))
		      ;; Handle supplied-p variable if any.
		      (and (> (length a) 2)
			   (push `(,(caddr a) (not (null ,path))) %let-list))))
	     (setq path `(cdr ,path))))))


;;; This deals with the portion of the arglist following any &rest flag.

(defun analyze-rest (arglist path errloc whole)
  (when (atom arglist)
    (error "Bad &rest or &body arg in ~S." errloc))
  (prog ((rest-arg (car arglist))
	 (more (cdr arglist)))
    (cond ((symbolp rest-arg)
	   (push (list rest-arg path) %let-list))
	  ((and (consp rest-arg) (> (length (the list rest-arg)) 1))
	   (unless %env-arg-name
	     (error "Hairy &body not allowed when no environment available."))
	   (let ((decls-var (second rest-arg))
		 (doc-var (third rest-arg))
		 (n-body (gensym)) (n-decls (gensym)) (n-doc (gensym)))
	     (setq rest-arg (first rest-arg))
	     (when doc-var (push doc-var %let-list))
	     (push decls-var %let-list)
	     (push `(,rest-arg
		     (multiple-value-bind (,n-body ,n-decls ,n-doc)
					  (parse-body ,path ,%env-arg-name
						      ,(not (null doc-var)))
		       (setq ,decls-var ,n-decls)
		       ,(if doc-var `(setq ,doc-var ,n-doc) n-doc)
		       ,n-body))
		   %let-list)))
	  (t
	   (error "Bad &rest or &body arg in ~S." errloc)))
		 
    (setq %restp t)
    TRY-AGAIN
    (cond ((null more) nil)
	  ((atom more)
	   (cerror "Ignore the illegal terminator."
		   "Dotted arglist terminator after &rest arg in ~S." errloc))
	  ((eq (car more) '&key)
	   (analyze-key (cdr more) rest-arg errloc))
	  ((eq (car more) '&aux)
	   (analyze-aux (cdr more) errloc))
	  ((eq (car more) '&allow-other-keys)
	   (cerror "Ignore it."
		   "Stray &ALLOW-OTHER-KEYS in arglist of ~S." errloc))
	  ((eq (cadr arglist) '&whole)
	   (cond ((and whole (cdr more) (symbolp (cadr more)))
		  (push (list (cadr more) whole) %let-list)
		  (setq more (cddr more))
		  (go try-again))
		 (t (error "Ill-formed or illegal &whole arg in ~S."
			   errloc))))
	  ((eq (cadr arglist) '&environment)
	   (cond ((and %env-arg-name (cdr more) (symbolp (cadr more)))
		  (push `(,(cadr more) ,%env-arg-name) %let-list)
		  (setq %env-arg-used t)
		  (setq more (cddr more))
		  (go try-again))
		 (t (error "Ill-formed or illegal &environment arg in ~S."
			   errloc)))))))

;;; Analyze stuff following &aux.

(defun analyze-aux (arglist errloc)
  (do ((args arglist (cdr args)))
      ((null args))
    (cond ((atom args)
	   (cerror "Ignore the illegal terminator."
		   "Dotted arglist after &AUX in ~S." errloc)
	   (return nil))
	  ((atom (car args))
	   (push (list (car args) nil) %let-list))
	  (t (push (list (caar args) (cadar args)) %let-list)))))


;;; Handle analysis of keywords, perhaps with destructuring over the keyword
;;; variable.  Assumes the remainder of the calling form has already been
;;; bound to the variable passed in as RESTVAR.

(defun analyze-key (arglist restvar errloc)
  (let ((temp (gensym))
	(check-keywords t)
	(keywords-seen nil))
    (push temp %let-list)
    (do ((args arglist (cdr args))
	 a k sp-var temp1)
	((atom args)
	 (cond ((null args) nil)
	       (t (cerror "Ignore the illegal terminator."
			  "Dotted arglist after &key in ~S." errloc))))
      (setq a (car args))
      (cond ((eq a '&allow-other-keys)
	     (setq check-keywords nil))
	    ((eq a '&aux)
	     (return (analyze-aux (cdr args) errloc)))
	    ;; Just a top-level variable.  Make matching keyword.
	    ((symbolp a)
	     (setq k (make-keyword a))
	     (push `(,a (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			       (car ,temp))
			      (t nil)))
		   %let-list)
	     (push k keywords-seen))
	    ;; Filter out error that might choke defmacro.
	    ((atom a)
	     (cerror "Ignore this item."
		     "~S -- non-symbol variable name in arglist of ~S."
		     a errloc))
	    ;; Deal with the common case: (var [init [svar]]) 
	    ((symbolp (car a))
	     (setq k (make-keyword (car a)))
	     ;; Deal with supplied-p variable, if any.
	     (cond ((and (cddr a) (symbolp (caddr a)))
		    (setq sp-var (caddr a))
		    (push (list sp-var nil) %let-list))
		   (t (setq sp-var nil)))
	     (push `(,(car a)
		     (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			    ,@(and sp-var `((setq ,sp-var t)))
			    (car ,temp))
			   (t ,(cadr a))))
		   %let-list)
	     (push k keywords-seen))
	    ;; Filter out more error cases that might kill defmacro.
	    ((or (atom (car a)) (not (keywordp (caar a))) (atom (cdar a)))
	     (cerror "Ignore this item."
		     "~S -- ill-formed keyword arg in ~S." (car a) errloc))
	    ;; Next case is ((:key var) [init [supplied-p]]).
	    ((symbolp (cadar a))
	     (setq k (caar a))
	     ;; Deal with supplied-p variable, if any.
	     (cond ((and (cddr a) (symbolp (caddr a)))
		    (setq sp-var (caddr a))
		    (push (list sp-var nil) %let-list))
		   (t (setq sp-var nil)))
	     (push `(,(cadar a)
		     (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			    ,@(and sp-var `((setq ,sp-var t)))
			    (car ,temp))
			   (t ,(cadr a))))
		   %let-list)
	     (push k keywords-seen))
	    ;; Same case, but must destructure the "variable".
	    (t (setq k (caar a))
	       (setq temp1 (gensym))
	       (cond ((and (cddr a) (symbolp (caddr a)))
		      (setq sp-var (caddr a))
		      (push (list sp-var nil) %let-list))
		     (t (setq sp-var nil)))
	       (push `(,temp1
		       (cond ((setq ,temp (,*key-finder* ',k ,restvar))
			      ,@(and sp-var `((setq ,sp-var t)))
			      (car ,temp))
			     (t ,(cadr a))))
		     %let-list)
	       (push k keywords-seen)
	       (let ((%min-args 0) (%arg-count 0) (%restp nil))
		      (analyze1 (cadar a) temp1 errloc nil)))))
    (and check-keywords
	 (push `(keyword-test ,restvar ',keywords-seen) %keyword-tests))))
	    

;;; Functions that must be around when the macros produced by DEFMACRO are
;;; expanded.

(defun make-keyword (s)
  "Takes a non-keyword symbol S and returns the corresponding keyword."
  (intern (symbol-name s) *keyword-package*))


(defun find-keyword (keyword keylist)
  "If keyword is present in the keylist, return a list of its argument.
  Else, return NIL."
  (do ((l keylist (cddr l)))
      ((atom l) nil)
    (cond ((atom (cdr l))
	   (cerror "Stick a NIL on the end and go on."
		   "Unpaired item in keyword portion of macro call.")
	   (rplacd l (list nil))
	   (return nil))
	  ((eq (car l) keyword) (return (list (cadr l)))))))


(defun keyword-test (keylist legal)
  "Check whether all keywords in a form are legal.  KEYLIST is the portion
  of the calling form containing keywords.  LEGAL is the list of legal
  keywords.  If the keyword :allow-other-keyws is present in KEYLIST,
  just return without complaining about anything."
  (cond ((memq ':allow-other-keys keylist) nil)
	(t (do ((kl keylist (cddr kl)))
	       ((atom kl) nil)
	     (cond ((memq (car kl) legal))
		   (t (cerror "Ignore it."
			      "~S illegal or unknown keyword." (car kl))))))))

;;; Return a form which tests whether an illegal number of arguments 
;;; have been supplied.  Args is the name of the variable to which
;;; the arglist is bound.
;;;
(defun defmacro-arg-test (args)
  (cond ((and (zerop %min-args) %restp) nil)
	((zerop %min-args)
	 `(> (length ,args) ,(1+ %arg-count)))
	(%restp
	 `(< (length ,args) ,(1+ %min-args)))
	((= %min-args %arg-count)
	 `(not (= (length ,args) ,(1+ %min-args))))
	(t
	 `(or (> (length ,args) ,(1+ %arg-count))
	      (< (length ,args) ,(1+ %min-args))))))
