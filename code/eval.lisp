;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/eval.lisp,v 1.7 1990/10/11 18:17:18 ram Exp $
;;;    
(in-package "LISP")
(export '(eval constantp quote proclaim
	  eval-when progn prog1 prog2 let let*
	  do do* dotimes dolist progv and or cond if the
	  macro-function special-form-p *macroexpand-hook*
	  macroexpand-1 macroexpand block return-from
	  return function setq psetq apply funcall
	  compiler-let progv flet labels macrolet
	  mapcar maplist mapc mapl mapcan mapcon
	  tagbody prog prog* go 
	  values multiple-values-limit
	  values-list multiple-value-list multiple-value-call
	  multiple-value-prog1 multiple-value-bind multiple-value-setq
	  catch unwind-protect throw defun
	  lambda-list-keywords call-arguments-limit lambda-parameters-limit
	  function-lambda-expression
          ;;
          ;; Declaration symbols referenced in the cold load.
          declare special 
	  ;;
	  ;; Magical markers...
	  lambda &optional &rest &key &aux &body &whole
	  &allow-other-keys &environment))

#| Not implemented:
*evalhook* *applyhook* evalhook applyhook 
|#

(export '(eval::interpreted-function-p
	  eval::interpreted-function-lambda-expression)
	"EVAL")
(import '(eval::*eval-stack-top*))

(in-package 'system)
(export '(parse-body find-if-in-closure))

(in-package "LISP")


(defconstant lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment)
  "Keywords that you can put in a lambda-list, supposing you should want
  to do such a thing.")

(defconstant call-arguments-limit most-positive-fixnum
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including rest args.")

(defconstant lambda-parameters-limit most-positive-fixnum
  "The exclusive upper bound on the number of parameters which may be specifed
  in a given lambda list.  This is actually the limit on required and optional
  parameters.  With &key and &aux you can get more.")

(defconstant multiple-values-limit most-positive-fixnum
  "The exclusive upper bound on the number of multiple-values that you can
  have.")



;;;; EVAL and friends.

;;;
;;; This flag is used by EVAL-WHEN to keep track of when code has already been
;;; evaluated so that it can avoid multiple evaluation of nested EVAL-WHEN
;;; (COMPILE)s.
(defvar *already-evaled-this* nil)

;;;
;;; This needs to be initialized in the cold load, since the top-level catcher
;;; will always restore the initial value.
(defvar *eval-stack-top* 0)

;;; EVAL  --  Public
;;;
;;;    Pick off a few easy cases, and call INTERNAL-EVAL for the rest.  If
;;; *ALREADY-EVALED-THIS* is true, then we bind it to NIL before doing a call
;;; so that the effect is confined to the lexical scope of the EVAL-WHEN.
;;;
(defun eval (original-exp)
  "Evaluates its single arg in a null lexical environment, returns the
  result or results."
  (let ((exp (macroexpand original-exp)))
    (typecase exp
      (symbol (symbol-value exp))
      (list
       (let ((name (first exp))
	     (args (1- (length exp))))
	 (case name
	   (function
	    (unless (= args 1)
	      (error "Wrong number of args to FUNCTION:~% ~S." exp))
	    (let ((name (second exp)))
	      (if (or (atom name)
		      (and (consp name)
			   (eq (car name) 'setf)))
		  (fdefinition name)
		  (eval:make-interpreted-function name))))
	   (quote
	    (unless (= args 1)
	      (error "Wrong number of args to QUOTE:~% ~S." exp))
	    (second exp))
	   (setq
	    (unless (evenp args)
	      (error "Odd number of args to SETQ:~% ~S." exp))
	    (unless (zerop args)
	      (do ((name (cdr exp) (cddr name)))
		  ((null name)
		   (do ((args (cdr exp) (cddr args)))
		       ((null (cddr args))
			(set (first args) (eval (second args))))
		     (set (first args) (eval (second args)))))
		(unless (eq (info variable kind (first name)) :special)
		  (return (eval:internal-eval original-exp))))))
	   ((progn)
	    (when (> args 0)
	      (dolist (x (butlast (rest exp)) (eval (car (last exp))))
		(eval x))))
	   (t
	    (if (and (symbolp name)
		     (eq (info function kind name) :function))
		(collect ((args))
		  (dolist (arg (rest exp))
		    (args (eval arg)))
		  (if *already-evaled-this*
		      (let ((*already-evaled-this* nil))
			(apply (symbol-function name) (args)))
		      (apply (symbol-function name) (args))))
		(eval:internal-eval original-exp))))))
      (t
       exp))))


;;; INTERPRETED-FUNCTION-P  --  Interface
;;;
;;;    This is defined here so that the printer &c can call it before the full
;;; interpreter is loaded.
;;;
(defun eval:interpreted-function-p (x)
  (and (functionp x)
       (= (get-type x) vm:closure-header-type)
       (fboundp 'eval::leaf-value)
       (let ((code-component (di::function-code-header (%closure-function x))))
	 (or (eq (di::function-code-header #'eval::leaf-value)
		 code-component)
	     (eq (di::function-code-header #'eval:make-interpreted-function)
		 code-component)))))


;;; FUNCTION-LAMBDA-EXPRESSION  --  Public
;;;
;;;    If interpreted, use the interpreter interface.  Otherwise, see if it was
;;; compiled with COMPILE.  If that fails, check for an inline expansion.
;;;
(defun function-lambda-expression (fun)
  "Given a function, return three values:
   1] A lambda expression that could be used to define the function, or NIL if
      the definition isn't available.
   2] NIL if the function was definitely defined in a null lexical environment,
      and T otherwise.
   3] Some object that \"names\" the function.  Although this is allowed to be
      any object, CMU CL always returns a valid function name or a string."
  (declare (type function fun))
  (if (eval:interpreted-function-p fun)
      (eval:interpreted-function-lambda-expression fun)
      (let* ((fun (%primitive c::function-self fun))
	     (name (%primitive c::function-name fun))
	     (code (di::function-code-header fun))
	     (info (di::code-debug-info code)))
	(if info
	    (let ((source (first (c::compiled-debug-info-source info))))
	      (cond ((and (eq (c::debug-source-from source) :lisp)
			  (eq (c::debug-source-info source) fun))
		     (values (second (svref (c::debug-source-name source) 0))
			     nil name))
		    ((stringp name)
		     (values nil t name))
		    (t
		     (let ((exp (info function inline-expansion name)))
		       (if exp
			   (values exp nil name)
			   (values nil t name))))))
	    (values nil t name)))))


;;; FIND-IF-IN-CLOSURE  --  Interface
;;;
;;;    Like FIND-IF, only we do it on a compiled closure's environment.
;;;
(defun find-if-in-closure (test fun)
  (dotimes (index (1- (get-closure-length fun)))
    (let ((elt (%closure-index-ref fun index)))
      (when (funcall test elt)
	(return elt)))))


;;;; Syntactic environment access:

(defun special-form-p (symbol)
  "If the symbol globally names a special form, returns the definition in a
  mysterious internal format (a FEXPR), else returns NIL."
  (declare (symbol symbol))
  (eq (info function kind symbol) :special-form))

(defvar *macroexpand-hook* 'funcall
  "The value of this variable must be a function that can take three
  arguments, a macro expander function, the macro form to be expanded,
  and the lexical environment to expand in.  The function should
  return the expanded form.  This function is called by MACROEXPAND-1
  whenever a runtime expansion is needed.  Initially this is set to
  FUNCALL.")


;;; Macroexpand-1  --  Public
;;;
;;;    The Env is a LEXENV or NIL (the null environment.)
;;;
(defun macroexpand-1 (form &optional env)
  "If form is a macro, expands it once.  Returns two values, the
  expanded form and a T-or-NIL flag indicating whether the form was,
  in fact, a macro.  Env is the lexical environment to expand in,
  which defaults to the null environment."
  (let ((fenv (when env (c::lexenv-functions env))))
    (if (and (consp form) (symbolp (car form)))
	(let ((local-def (cdr (assoc (car form) fenv))))
	  (if local-def
	      (if (and (consp local-def) (eq (car local-def) 'MACRO))
		  (values (funcall *macroexpand-hook* (cdr local-def)
				   form env)
			  t)
		  (values form nil))
	      (let ((global-def (macro-function (car form))))
		(if global-def
		    (values (funcall *macroexpand-hook* global-def form env)
			    t)
		    (values form nil)))))
	(values form nil))))


(defun macroexpand (form &optional env)
  "If Form is a macro call, then the form is expanded until the result is not
  a macro.  Returns as multiple values, the form after any expansion has
  been done and T if expansion was done, or NIL otherwise.  Env is the
  lexical environment to expand in, which defaults to the null environment."
  (prog (flag)
    (multiple-value-setq (form flag) (macroexpand-1 form env))
    (unless flag (return (values form nil)))
    loop
    (multiple-value-setq (form flag) (macroexpand-1 form env))
    (if flag (go loop) (return (values form t)))))


(defun macro-function (symbol)
  "If the symbol globally names a macro, returns the expansion function,
  else returns NIL."
  (declare (symbol symbol))
  (if (eq (info function kind symbol) :macro)
      (info function macro-function symbol)
      nil))


(defun (setf macro-function) (function symbol)
  (declare (symbol symbol) (type function function))

  (when (eq (info function kind symbol) :special-form)
    (error "~S names a special form." symbol))

  (setf (info function kind symbol) :macro)
  (setf (info function macro-function symbol) function)
  (setf (symbol-function symbol)
	#'(lambda (&rest args) (declare (ignore args))
	    (error "Cannot funcall macro functions.")))
  function)


(defun constantp (object)
  "True of any Lisp object that has a constant value: types that eval to
  themselves, keywords, constants, and list whose car is QUOTE."
  (typecase object
    (number t)
    (character t)
    (array t)
    (symbol
     (eq (info variable kind object) :constant))
    (list (eq (car object) 'quote))))


;;; Function invocation:

(defun apply (function arg &rest args)
  "Applies FUNCTION to a list of arguments produced by evaluating ARGS in
  the manner of LIST*.  That is, a list is made of the values of all but the
  last argument, appended to the value of the last argument, which must be a
  list."
  (cond ((atom args)
	 (apply function arg))
	((atom (cdr args))
	 (apply function (cons arg (car args))))
	(t (do* ((a1 args a2)
		 (a2 (cdr args) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (apply function (cons arg args)))))))


(defun funcall (function &rest arguments)
  "Calls Function with the given Arguments."
  (apply function arguments))



;;; Multiple-Value forms:

(defun values (&rest values)
  "Returns all of its arguments, in order, as values."
  (values-list values))

(defun values-list (list)
  "Returns all of the elements of List, in order, as values."
  (values-list list))
