;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains code which knows about both the type representation
;;; and the compiler IR1 representation.  This stuff is used for doing type
;;; checking.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;; These are the functions that are to be called when a problem is detected.
;;; They are passed format arguments.  If null, we don't do anything.  The
;;; error function is called when something is definitely incorrect.  The
;;; warning function is called when it is somehow impossible to tell if the
;;; call is correct.
;;;
(defvar *error-function*)
(defvar *warning-function*)

;;; The function that we use for type checking.  The derived type is the first
;;; argument and the type we are testing against is the second argument.  The
;;; function should return values like Csubtypep. 
;;;
(defvar *test-function*)

(proclaim '(type (or function null) *error-function* *warning-function
		 *test-function*))

;;; *lossage-detected* is set if a definite incompatibility is detected.
;;; *slime-detected* is set if we can't tell whether the call is compatible or
;;; not.
;;;
(defvar *lossage-detected*)
(defvar *slime-detected*)


;;; Note-Lossage, Note-Slime  --  Internal
;;;
;;;    Signal a warning if appropriate and set the *lossage-detected* flag.
;;;
(proclaim '(ftype (function (string &rest t) void) note-lossage note-slime))
(defun note-lossage (format-string &rest format-args)
  (setq *lossage-detected* t)
  (when *error-function*
    (apply *error-function* format-string format-args)))
;;;
(defun note-slime (format-string &rest format-args)
  (setq *slime-detected* t)
  (when *warning-function*
    (apply *warning-function* format-string format-args)))


(proclaim '(special *compiler-error-context*))


;;;; Stuff for checking a call against a function type.

;;; Valid-Function-Use  --  Interface
;;;
;;;    Determine whether a use of a function is consistent with its type.  The
;;; first value is true if the call is thought to be valid, and the second
;;; value is true when the first value is definitely correct.
;;;
;;; The Argument-Test function is used to determine whether an argument type
;;; matches the type we are checking against.  Similarly, the Result-Test is
;;; used to determine whether the result type matches the specified result.
;;;
;;; Unlike the argument test, the result test may be called on values or
;;; function types.  If Strict-Result is true, then the Node-Derived-Type is
;;; always used.  If Strict-Result is false and Cont's Type-Check is true, then
;;; the Node-Derived-Type is intersected with the Cont's Asserted-Type.
;;;
;;; The error and warning functions are functions that are called to explain
;;; the result.  We bind *compiler-error-context* to the combination node so
;;; that Compiler-Warning and related functions will do the right thing if
;;; they are supplied.
;;;
(defun valid-function-use (call type &key
				((:argument-test *test-function*) #'csubtypep)
				(result-test #'values-subtypep)
				(strict-result nil)
				((:error-function *error-function*))
				((:warning-function *warning-function*)))
  (declare (function result-test) (type combination call)
	   (type function-type type))
  (let* ((*lossage-detected* nil)
	 (*slime-detected* nil)
	 (*compiler-error-context* call)
	 (args (combination-args call))
	 (nargs (length args))
	 (required (function-type-required type))
	 (min-args (length required))
	 (optional (function-type-optional type))
	 (max-args (+ min-args (length optional)))
	 (rest (function-type-rest type))
	 (keyp (function-type-keyp type)))
    
    (cond
     ((function-type-wild-args type)
      (do ((i 1 (1+ i))
	   (arg args (cdr arg)))
	  ((null arg))
	(check-arg (car arg) *wild-type* i)))
     ((< nargs min-args)
      (note-lossage
       "Function called with ~R argument~:P, but wants at least ~R."
       nargs min-args))
     ((<= nargs max-args)
      (check-fixed-and-rest args (append required optional) rest))
     ((not (or keyp rest))
      (note-lossage
       "Function called with ~R argument~:P, but wants at most ~R."
       nargs max-args))
     ((and keyp (oddp (- nargs max-args)))
      (note-lossage
       "Function has an odd number of arguments in the keyword portion."))
     (t
      (check-fixed-and-rest args (append required optional) rest)
      (when keyp
	(check-keywords args max-args type))))
    
    (let* ((dtype (node-derived-type call))
	   (return-type (function-type-returns type))
	   (cont (node-cont call))
	   (out-type
	    (if (or strict-result (not (continuation-type-check cont)))
		dtype
		(values-type-intersection (continuation-asserted-type cont)
					  dtype))))
      (multiple-value-bind (int win)
			   (funcall result-test out-type return-type)
	(cond ((not win)
	       (note-slime "Can't tell whether the result is a ~S."
			   (type-specifier return-type)))
	      ((not int)
	       (note-lossage "The result is a ~S, not a ~S."
			     (type-specifier out-type)
			     (type-specifier return-type)))))) 
    
    (cond (*lossage-detected* (values nil t))
	  (*slime-detected* (values nil nil))
	  (t (values t nil)))))


;;; Check-Arg-Type  --  Internal
;;;
;;;    Check that the derived type of the continuation Cont is compatible with
;;; Type.  N is the arg number, for error message purposes.  We return true if
;;; arg is definitely o.k.  If the type is a magic CONSTANT-TYPE, then we check
;;; for the argument being a constant value of the specified type.  If there is
;;; a manfest type error (DERIVED-TYPE = NIL), then we flame about the asserted
;;; type even when our type is satisfied under the test.
;;;
(defun check-arg-type (cont type n)
  (declare (type continuation cont) (type ctype type) (type index n))
  (cond
   ((not (constant-type-p type))
    (let ((ctype (continuation-type cont)))
      (multiple-value-bind (int win)
			   (funcall *test-function* ctype type)
	(cond ((not win)
	       (note-slime "Can't tell whether the ~:R argument is a ~S." n
			   (type-specifier type))
	       nil)
	      ((not int)
	       (note-lossage "The ~:R argument is a ~S, not a ~S." n
			     (type-specifier ctype)
			     (type-specifier type))
	       nil)
	      ((eq ctype *empty-type*)
	       (note-lossage "The ~:R argument is a ~S, not a ~S." n
			     (type-specifier (continuation-proven-type cont))
			     (type-specifier
			      (continuation-asserted-type cont)))
	       nil)
	      (t t)))))
    ((not (constant-continuation-p cont))
     (note-slime "The ~:R argument is not a constant." n)
     nil)
    (t
     (let ((val (continuation-value cont))
	   (type (constant-type-type type)))
       (multiple-value-bind (res win)
			    (ctypep val type)
	 (cond ((not win)
		(note-slime "Can't tell whether the ~:R argument is a ~
		             constant ~S:~%  ~S"
			    n (type-specifier type) val)
		nil)
	       ((not res)
		(note-lossage "The ~:R argument is not a constant ~S:~%  ~S"
			      n (type-specifier type) val)
		nil)
	       (t t)))))))

  
;;; Check-Fixed-And-Rest  --  Internal
;;;
;;;    Check that each of the type of each supplied argument intersects with
;;; the type specified for that argument.  If we can't tell, then we complain
;;; about the slime.
;;;
(proclaim '(function check-fixed-and-rest (list list (or type null)) void))
(defun check-fixed-and-rest (args types rest)
  (do ((arg args (cdr arg))
       (type types (cdr type))
       (n 1 (1+ n)))
      ((or (null type) (null arg))
       (when rest
	 (dolist (arg arg)
	   (check-arg-type arg rest n)
	   (incf n))))
    (declare (fixnum n))
    (check-arg-type (car arg) (car type) n)))


;;; Check-Keywords  --  Internal
;;;
;;;    Check that the keyword args are of the correct type.  Each keyword
;;; should be known and the corresponding argument should be of the correct
;;; type.  If the keyword isn't a constant, then we can't tell, so we note
;;; slime.
;;;
(proclaim '(function check-keywords (list fixnum function-type) void))
(defun check-keywords (args pre-key type)
  (do ((key (nthcdr pre-key args) (cddr key))
       (n pre-key (+ n 2)))
      ((null key))
    (declare (fixnum n))
    (let ((k (car key)))
      (check-arg-type k (specifier-type 'symbol) n)
      (cond ((not (check-arg-type k (specifier-type 'keyword) n)))
	    ((not (constant-continuation-p k))
	     (note-slime "The keyword for the ~:R argument is not a constant."
			 n))
	    (t
	     (let* ((name (continuation-value k))
		    (info (find name (function-type-keywords type)
				:key #'key-info-name)))
	       (cond ((not info)
		      (unless (function-type-allowp type)
			(note-lossage "~S is not a known argument keyword."
				      name)))
		     (t
		      (check-arg-type (second key) (key-info-type info)
				      n)))))))))


;;; Lambda-Result-Type  --  Internal
;;;
;;;    Guess the return type of a Lambda.  We just return the derived type of
;;; the result continuation, assuming that IR1 optimize and Type check have
;;; made this be a good description of the return type.
;;;
(proclaim '(function lambda-result-type (lambda) type))
(defun lambda-result-type (lambda)
  (let ((ret (lambda-return lambda)))
    (if ret
	(continuation-derived-type (return-result ret))
	*empty-type*)))


;;; Definition-Type  --  Interface
;;;
;;;    Construct a function type from a definition.
;;;
;;; Due to the lack of a (list x) type specifier, we can't reconstruct the
;;; &rest type.
;;;
(proclaim '(function definition-type (functional) function-type))
(defun definition-type (functional)
  (if (lambda-p functional)
      (make-function-type
       :required (mapcar #'leaf-type (lambda-vars functional))
       :returns (lambda-result-type functional))
      (let ((rest nil))
	(collect ((req)
		  (opt)
		  (keys))
	  (dolist (arg (optional-dispatch-arglist functional))
	    (let ((info (lambda-var-arg-info arg))
		  (type (leaf-type arg)))
	      (if info
		  (ecase (arg-info-kind info)
		    (:required (req type))
		    (:optional (opt type))
		    (:keyword
		     (keys (make-key-info :name (arg-info-keyword info)
					  :type type)))
		    (:rest
		     (setq rest *universal-type*)))
		  (req type))))
	  
	  (make-function-type
	   :required (req)  :optional (opt)  :rest rest  :keywords (keys)
	   :keyp (optional-dispatch-keyp functional)
	   :allowp (optional-dispatch-allowp functional)
	   :returns (lambda-result-type
		     (optional-dispatch-main-entry functional)))))))



;;;; Approximate function types:
;;;
;;;    Approximate function types provide a condensed representation of all the
;;; different ways that a function has been used.  If we have no declared or
;;; defined type for a function, then we build an approximate function type
;;; by examining each use of the function.  When we encounter a definition or
;;; proclamation, we can check the actual type for compatibity with the
;;; previous uses.


(defstruct (approximate-function-type)
  ;;
  ;; The smallest and largest numbers of arguments that this function has been
  ;; called with.
  (min-args call-arguments-limit :type fixnum)
  (max-args 0 :type fixnum)
  ;;
  ;; A list of lists of the all the types that have been used in each argument
  ;; position.
  (types () :type list)
  ;;
  ;; A list of the Approximate-Key-Info structures describing all the things
  ;; that looked like keyword arguments.  There are distinct structures
  ;; describing each argument position in which the keyword appeared.
  (keys () :type list))


(defstruct (approximate-key-info)
  ;;
  ;; The keyword name of this argument.
  (name nil :type keyword)
  ;;
  ;; The position at which this keyword appeared.  0 if it appeared as the
  ;; first argument, etc.
  (position nil :type fixnum)
  ;;
  ;; A list of all the argument types that have been used with this keyword.
  (types nil :type list)
  ;;
  ;; True if this keyword has appeared only in calls with an obvious
  ;; :allow-other-keys.
  (allowp nil :type (member t nil)))


;;; Note-Function-Use  --  Interface
;;;
;;;    Return an Approximate-Function-Type representing the context of Call.
;;; If Type is supplied and not null, then we merge the information into the
;;; information already accumulated in Type.
;;;
(proclaim '(function note-function-use
		     (combination &optional (or approximate-function-type null))
		     approximate-function-type))
(defun note-function-use (call &optional type)
  (let* ((type (or type (make-approximate-function-type)))
	 (types (approximate-function-type-types type))
	 (args (combination-args call))
	 (nargs (length args))
	 (allowp (find-if #'(lambda (x)
			      (and (constant-continuation-p x)
				   (eq (continuation-value x) :allow-other-keys)))
			  args)))

    (setf (approximate-function-type-min-args type)
	  (min (approximate-function-type-min-args type) nargs))
    (setf (approximate-function-type-max-args type)
	  (max (approximate-function-type-max-args type) nargs))

    (do ((old types (cdr old))
	 (arg args (cdr arg)))
	((null old)
	 (setf (approximate-function-type-types type)
	       (nconc types
		      (mapcar #'(lambda (x)
				  (list (continuation-type x)))
			      arg))))
      (when (null arg) (return))
      (pushnew (continuation-type (car arg))
	       (car old)
	       :test #'type=))

    (collect ((keys (approximate-function-type-keys type) cons))
      (do ((arg args (cdr arg))
	   (pos 0 (1+ pos)))
	  ((or (null arg) (null (cdr arg)))
	   (setf (approximate-function-type-keys type) (keys)))
	(let ((key (first arg))
	      (val (second arg)))
	  (when (constant-continuation-p key)
	    (let ((name (continuation-value key)))
	      (when (keywordp name)
		(let ((old (find-if
			    #'(lambda (x)
				(and (eq (approximate-key-info-name x) name)
				     (= (approximate-key-info-position x)
					pos)))
			    (keys)))
		      (val-type (continuation-type val))) 
		  (cond (old
			 (pushnew val-type
				  (approximate-key-info-types old)
				  :test #'type=)
			 (unless allowp
			   (setf (approximate-key-info-allowp old) nil)))
			(t
			 (keys (make-approximate-key-info
				:name name  :position pos  :allowp allowp
				:types (list val-type))))))))))))
    type))


;;; Valid-Approximate-Type  --  Interface
;;;
;;;    Similar to Valid-Function-Use, but checks an Approximate-Function-Type
;;; against a real function type.
;;;
(proclaim '(function valid-approximate-type
		     (approximate-function-type function-type &optional
						function function function)
		     (values boolean boolean)))
(defun valid-approximate-type (call-type type &optional
					 (*test-function* #'types-intersect)
					 (*error-function* #'compiler-warning)
					 (*warning-function* #'compiler-note))
  (let* ((*lossage-detected* nil)
	 (*slime-detected* nil)
	 (required (function-type-required type))
	 (min-args (length required))
	 (optional (function-type-optional type))
	 (max-args (+ min-args (length optional)))
	 (rest (function-type-rest type))
	 (keyp (function-type-keyp type)))

    (when (function-type-wild-args type)
      (return-from valid-approximate-type (values t t)))

    (let ((call-min (approximate-function-type-min-args call-type)))
      (when (< call-min min-args)
	(note-lossage
	 "Function previously called with ~R argument~:P, but wants at least ~R."
	 call-min min-args)))

    (let ((call-max (approximate-function-type-max-args call-type)))
      (cond ((<= call-max max-args))
	    ((not (or keyp rest))
	     (note-lossage
	      "Function previously called with ~R argument~:P, but wants at most ~R."
	      call-max max-args))
	    ((and keyp (oddp (- call-max max-args)))
	     (note-lossage
	      "Function previously called with an odd number of arguments in ~
	      the keyword portion.")))

      (when (and keyp (> call-max max-args))
	(check-approximate-keywords call-type max-args type)))

    (check-approximate-fixed-and-rest call-type (append required optional)
				      rest)

    (cond (*lossage-detected* (values nil t))
	  (*slime-detected* (values nil nil))
	  (t (values t t)))))


;;; Check-Approximate-Fixed-And-Rest  --  Internal
;;;
;;;    Check that each of the types used at each arg position is compatible
;;; with the actual type.
;;;
(proclaim '(function check-approximate-fixed-and-rest
		     (approximate-function-type list (or type null))
		     void))
(defun check-approximate-fixed-and-rest (call-type fixed rest)
  (do ((types (approximate-function-type-types call-type) (cdr types))
       (n 1 (1+ n))
       (arg fixed (cdr arg)))
      ((null types))
    (let ((decl-type (or (car arg) rest)))
      (unless decl-type (return))
      (check-approximate-arg-type (car types) decl-type "~R" n))))


;;; Check-Approximate-Arg-Type  --  Internal
;;;
;;;    Check that each of the call-types is compatible with Decl-Type,
;;; complaining if not or if we can't tell.
;;;
(proclaim '(function check-approximate-arg-type (list type string &rest t) void))
(defun check-approximate-arg-type (call-types decl-type context &rest args)
  (let ((losers *empty-type*))
    (dolist (ctype call-types)
      (multiple-value-bind (int win)
			   (funcall *test-function* ctype decl-type)
	(cond
	 ((not win)
	  (note-slime "Can't tell whether previous ~? argument type ~S is a ~S."
		      context args (type-specifier ctype) (type-specifier decl-type)))
	 ((not int)
	  (setq losers (type-union ctype losers))))))

    (unless (eq losers *empty-type*)
      (note-lossage "~:(~?~) argument should be a ~S but was a ~S in a previous call."
		    context args (type-specifier decl-type) (type-specifier losers)))))


;;; Check-Approximate-Keywords  --  Internal
;;;
;;;    Check the types of each manifest keyword that appears in a keyword
;;; argument position.  Check the validity of all keys that appeared in valid
;;; keyword positions.
;;;
;;; ### We could check the Approximate-Function-Type-Types to make sure that
;;; all arguments in keyword positions were manifest keywords.
;;;
(defun check-approximate-keywords (call-type max-args type)
  (let ((call-keys (approximate-function-type-keys call-type))
	(keys (function-type-keywords type)))
    (dolist (key keys)
      (let ((name (key-info-name key)))
	(collect ((types nil append))
	  (dolist (call-key call-keys)
	    (let ((pos (approximate-key-info-position call-key)))
	      (when (and (eq (approximate-key-info-name call-key) name)
			 (> pos max-args) (evenp (- pos max-args)))
		(types (approximate-key-info-types call-key)))))
	  (check-approximate-arg-type (types) (key-info-type key) "~S" name))))
    
    (unless (function-type-allowp type)
      (collect ((names () adjoin))
	(dolist (call-key call-keys)
	  (let ((pos (approximate-key-info-position call-key)))
	    (when (and (> pos max-args) (evenp (- pos max-args))
		       (not (approximate-key-info-allowp call-key)))
	      (names (approximate-key-info-name call-key)))))

	(dolist (name (names))
	  (unless (find name keys :key #'key-info-name)
	    (note-lossage "Function previously called with unknown argument keyword ~S."
		  name)))))))


;;;; Redefinition checking:
;;;
;;;    When we encounter a 

#|


;;; Valid-Redefinition  --  Interface
;;;
;;;    Check for reasonablness of redefining a function of type Old as type
;;; New.
;;;
(proclaim '(function valid-redefinition (function-type function-type) ???))
(defun valid-redefinition (old new)
  ...)


;;; Assert-Definition-Type  --  Interface
;;;
;;;    Propagate type constraints from Type to the variables and result of
;;; Functional.
;;;
(proclaim '(function assert-definition-type (functional type) void))
(defun assert-definition-type (functional type)
  ...)
|#

