;;; -*- Log: code.log; Package: Extensions -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Spice Lisp extensions to the language.
;;;
;;; Letf written by Steven Handerson.
;;;
;;; **********************************************************************
(in-package "EXTENSIONS")

(export '(letf* letf dovector deletef indenting-further
		read-char-no-edit listen-skip-whitespace concat-pnames
		iterate once-only collect do-anonymous undefined-value))

(import 'lisp::whitespace-char-p)


;;; Undefined-Value  --  Public
;;;
;;;    This is here until we figure out what to do with it.
;;;
(proclaim '(inline undefined-value))
(defun undefined-value ()
  '%undefined%)


(defun skip-whitespace (&optional (stream *standard-input*))
  (loop (let ((char (read-char stream)))
	  (if (not (lisp::whitespacep char))
	      (return (unread-char char stream))))))

  
(defun listen-skip-whitespace (&optional (stream *standard-input*))
  "See listen.  Any whitespace in the input stream will be flushed."
  (do ((char (read-char-no-hang stream nil nil nil)
	     (read-char-no-hang stream nil nil nil)))
      ((null char) nil)
    (cond ((not (whitespace-char-p char))
	   (unread-char char stream)
	   (return T)))))

;;; These macros waste time as opposed to space.

(defmacro letf* (bindings &body body &environment env)
  "Does what one might expect, saving the old values and setting the generalized
  variables to the new values in sequence.  Unwind-protects and get-setf-method
  are used to preserve the semantics one might expect in analogy to let*,
  and the once-only evaluation of subforms."
  (labels ((do-bindings
	    (bindings)
	    (cond ((null bindings) body)
		  (t (multiple-value-bind (dummies vals newval setter getter)
					  (lisp::foo-get-setf-method (caar bindings) env)
		       (let ((save (gensym)))
			 `((let* (,@(mapcar #'list dummies vals)
				  (,(car newval) ,(cadar bindings))
				  (,save ,getter))
			     (unwind-protect
			       (progn ,setter
				      ,@(do-bindings (cdr bindings)))
			       (setq ,(car newval) ,save)
			       ,setter)))))))))
    (car (do-bindings bindings))))


(defmacro letf (bindings &body body &environment env)
  "Like letf*, but evaluates all the implicit subforms and new values of all
  the implied setfs before altering any values.  However, the store forms
  (see get-setf-method) must still be evaluated in sequence.  Uses unwind-
  protects to protect the environment."
  (let (temps)
    (labels
      ((do-bindings
	(bindings)
	(cond ((null bindings) body)
	      (t (let ((binding (car bindings)))
		   (multiple-value-bind (dummies vals newval setter getter)
					(lisp::foo-get-setf-method (car binding) env)
		     (let ((save (gensym)))
		       (mapcar #'(lambda (a b) (push (list a b) temps))
			       dummies vals) 
		       (push (list save getter) temps)
		       (push (list (car newval) (cadr binding)) temps)
		       `((unwind-protect
			   (progn ,setter
				  ,@(do-bindings (cdr bindings)))
			   (setq ,(car newval) ,save)
			   ,setter)))))))))
      (let ((form (car (do-bindings bindings))))
	`(let* ,(nreverse temps)
	   ,form)))))


(define-setf-method logbitp (index int &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
		       (lisp::foo-get-setf-method int env)
    (let ((ind (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,ind ,@temps)
	      `(,index
		,@vals)
	      (list store)
	      `(let ((,stemp
		      (dpb (if ,store 1 0) (byte 1 ,ind) ,access-form)))
		 ,store-form
		 ,store)
	      `(logbitp ,ind ,access-form)))))


;;; Indenting-Further is a user-level macro which may be used to locally increment
;;; the indentation of a stream.

(defmacro indenting-further (stream more &rest body)
  "Causes the output of the indenting Stream to indent More spaces.  More is
  evaluated twice."
  `(unwind-protect
     (progn
      (incf (lisp::indenting-stream-indentation ,stream) ,more)
      ,@body)
     (decf (lisp::indenting-stream-indentation ,stream) ,more)))


;;; Deletef

(defmacro deletef (elt list &rest keys &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
		       (lisp::foo-get-setf-method list env)
    (let ((eltsym (gensym))
	  (listsym (gensym)))
      `(let* ((,eltsym ,elt)
	      ,@(mapcar #'list dummies vals)
	      (,listsym ,getter)
	      (,(car newval) (delete ,eltsym ,listsym ,@keys)))
	 ,setter))))


(defmacro dovector ((elt vector) &rest forms)
  "Just like dolist, but with one-dimensional arrays."
  (let ((index (gensym))
	(length (gensym))
	(vec (gensym)))
    `(let ((,vec ,vector))
       (do ((,index 0 (1+ ,index))
	    (,length (length ,vec)))
	   ((>= ,index ,length) nil)
	 (let ((,elt (aref ,vec ,index)))
	   ,@forms)))))


(eval-when (compile load eval)
  (defun concat-pnames (name1 name2)
    (if name1
	(intern (concatenate 'simple-string (symbol-name name1)
			     (symbol-name name2)))
	name2)))


;;; Iterate  --  Public
;;;
;;;    The ultimate iteration macro...
;;;
(defmacro iterate (name binds &body body)
  "Iterate Name ({(Var Initial-Value)}*) Declaration* Form*
  This is syntactic sugar for Labels.  It creates a local function Name with
  the specified Vars as its arguments and the Declarations and Forms as its
  body.  This function is then called with the Initial-Values, and the result
  of the call is return from the macro."
  (dolist (x binds)
    (unless (and (listp x)
		 (= (length x) 2))
      (error "Malformed iterate variable spec: ~S." x)))
  
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))


;;;; The Collect macro:

;;; Collect-Normal-Expander  --  Internal
;;;
;;;    This function does the real work of macroexpansion for normal collection
;;; macros.  N-Value is the name of the variable which holds the current
;;; value.  Fun is the function which does collection.  Forms is the list of
;;; forms whose values we are supposed to collect.
;;;
(defun collect-normal-expander (n-value fun forms)
  `(progn
    ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
    ,n-value))

;;; Collect-List-Expander  --  Internal
;;;
;;;    This function deals with the list collection case.  N-Tail is the pointer
;;; to the current tail of the list, which is NIL if the list is empty.
;;;
(defun collect-list-expander (n-value n-tail forms)
  (let ((n-res (gensym)))
    `(progn
      ,@(mapcar #'(lambda (form)
		    `(let ((,n-res (cons ,form nil)))
		       (cond (,n-tail
			      (setf (cdr ,n-tail) ,n-res)
			      (setq ,n-tail ,n-res))
			     (t
			      (setq ,n-tail ,n-res  ,n-value ,n-res)))))
		forms)
      ,n-value)))


;;; Collect  --  Public
;;;
;;;    The ultimate collection macro...
;;;
(defmacro collect (collections &body body)
  "Collect ({(Name [Initial-Value] [Function])}*) {Form}*
  Collect some values somehow.  Each of the collections specifies a bunch of
  things which collected during the evaluation of the body of the form.  The
  name of the collection is used to define a local macro, a la MACROLET.
  Within the body, this macro will evaluate each of its arguments and collect
  the result, returning the current value after the collection is done.  The
  body is evaluated as a PROGN; to get the final values when you are done, just
  call the collection macro with no arguments.

  Initial-Value is the value that the collection starts out with, which
  defaults to NIL.  Function is the function which does the collection.  It is
  a function which will accept two arguments: the value to be collected and the
  current collection.  The result of the function is made the new value for the
  collection.  As a totally magical special-case, the Function may be Collect,
  which tells us to build a list in forward order; this is the default.  If an
  Initial-Value is supplied for Collect, the stuff will be rplacd'd onto the
  end.  Note that Function may be anything that can appear in the functional
  position, including macros and lambdas."

  (let ((macros ())
	(binds ()))
    (dolist (spec collections)
      (unless (<= 1 (length spec) 3)
	(error "Malformed collection specifier: ~S." spec))
      (let ((n-value (gensym))
	    (name (first spec))
	    (default (second spec))
	    (kind (or (third spec) 'collect)))
	(push `(,n-value ,default) binds)
	(if (eq kind 'collect)
	    (let ((n-tail (gensym)))
	      (if default
		  (push `(,n-tail (last ,n-value)) binds)
		  (push n-tail binds))
	      (push `(,name (&rest args)
			    (collect-list-expander ',n-value ',n-tail args))
		    macros))
	    (push `(,name (&rest args)
			  (collect-normal-expander ',n-value ',kind args))
		  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))


;;;; The Once-Only macro:

;;; Once-Only  --  Interface
;;;
;;;    Once-Only is a utility useful in writing source transforms and macros.
;;; It provides an easy way to wrap a let around some code to ensure that some
;;; forms are only evaluated once.
;;;
(defmacro once-only (specs &body body)
  "Once-Only ({(Var Value-Expression)}*) Form*
  Create Let which evaluates each Value-Expression, binding a temporary
  variable to the result, and wrapping the Let around the result of the
  evaluation of Body.  Within the body, each Var is bound to the corresponding
  temporary variable.  If the Value-Expression is a constant, then we just pass
  it through."
  (let ((n-binds (gensym))
	(n-temp (gensym)))
    (collect ((names)
	      (temp-binds))
      (dolist (spec specs)
	(when (/= (length spec) 2)
	  (error "Malformed Once-Only binding spec: ~S." spec))
	(let ((name (first spec))
	      (exp (second spec)))
	  (names `(,name ,exp))
	  (temp-binds
	   `(let ((,n-temp (gensym)))
	      (,n-binds `(,,n-temp ,,name))
	      (setq ,name ,n-temp)))))
      `(let ,(names)
	 (collect ((,n-binds))
	   ,@(temp-binds)
	   (list 'let (,n-binds) (progn ,@body)))))))


;;;; DO-ANONYMOUS:

;;; ### Bootstrap hack...  Renamed to avoid clobbering function in bootstrap
;;; environment.
;;;
(defun lisp::do-do-body (varlist endlist code decl bind step name block)
  (let* ((inits ())
	 (steps ())
	 (l1 (gensym))
	 (l2 (gensym)))
    ;; Check for illegal old-style do.
    (when (or (not (listp varlist)) (atom endlist))
      (error "Ill-formed ~S -- possibly illegal old style DO?" name))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
	    ((listp v)
	     (unless (symbolp (first v))
	       (error "~S step variable is not a symbol: ~S" name (first v)))
	     (case (length v)
	       (1 (push (first v) inits))
	       (2 (push v inits))
	       (3 (push (list (first v) (second v)) inits)
		  (setq steps (list* (third v) (first v) steps)))
	       (t (error "~S is an illegal form for a ~S varlist." v name))))
	    (t (error "~S is an illegal form for a ~S varlist." v name))))
    ;; And finally construct the new form.
    `(block ,BLOCK
       (,bind ,(nreverse inits)
	,@decl
	(tagbody
	 (go ,L2)
	 ,L1
	 ,@code
	 (,step ,@(nreverse steps))
  	 ,L2 
	 (unless ,(car endlist) (go ,L1))
	 (return-from ,BLOCK (progn ,@(cdr endlist))))))))


(defmacro do-anonymous (varlist endlist &body (body decls))
  "DO-ANONYMOUS ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Like DO, but has no implicit NIL block.  Each Var is initialized in parallel
  to the value of the specified Init form.  On subsequent iterations, the Vars
  are assigned the value of the Step form (if any) in paralell.  The Test is
  evaluated before each evaluation of the body Forms.  When the Test is true,
  the the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO."
  (lisp::do-do-body varlist endlist body decls 'let 'psetq
		    'do-anonymous (gensym)))
