;;; -*- Package: KERNEL; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/type.lisp,v 1.21 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definition of non-CLASS types (e.g. subtypes of
;;; interesting BUILT-IN-CLASSes) and the interfaces to the type system.
;;; Common Lisp type specifiers are parsed into a somewhat canonical internal
;;; type representation that supports type union, intersection, etc.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "KERNEL")
(use-package "ALIEN-INTERNALS")

(export '(function-type-nargs code-component code-component-p lra lra-p))
(export '(make-alien-type-type alien-type-type
	  alien-type-type-p alien-type-type-alien-type
	  *unparse-function-type-simplify*))
(import 'c-call:void)
(export 'void)

(in-package "EXTENSIONS")
(export '(constant-argument instance *use-implementation-types*))

(in-package "KERNEL")

(export '(extract-function-type))

(with-cold-load-init-forms)

;;; ### Remaining incorrectnesses:
;;;
;;; Type-Union (and the OR type) doesn't properly canonicalize an exhaustive
;;; partition or coalesce contiguous ranges of numeric types.
;;;
;;; There are all sorts of nasty problems with open bounds on float types (and
;;; probably float types in general.)
;;;
;;; ratio and bignum are not recognized as numeric types.

;;;
(defvar *use-implementation-types* t
  "*Use-Implementation-Types* is a semi-public flag which determines how
   restrictive we are in determining type membership.  If two types are the
   same in the implementation, then we will consider them them the same when
   this switch is on.  When it is off, we try to be as restrictive as the
   language allows, allowing us to detect more errors.  Currently, this only
   affects array types.")

(cold-load-init (setq *use-implementation-types* t))
(proclaim '(type boolean *use-implementation-types*))

;;; DELEGATE-COMPLEX-{SUBTYPEP-ARG2,INTERSECTION}  --  Interface
;;;
;;;    These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree of the
;;; type graph (i.e. there is no simple way for any other type class to be a
;;; subtype.)  There are always still complex ways, namely UNION and MEMBER
;;; types, so we must give TYPE1's method a chance to run, instead of
;;; immediately returning NIL, T.
;;;
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
	 (type-class-complex-subtypep-arg1
	  (type-class-info type1))))
    (if subtypep-arg1
	(funcall subtypep-arg1 type1 type2)
	(values nil t))))
;;;
(defun delegate-complex-intersection (type1 type2)
  (let ((method (type-class-complex-intersection (type-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection)))
	(funcall method type2 type1)
	(vanilla-intersection type1 type2))))

;;; HAS-SUPERCLASSES-COMPLEX-SUBTYPEP-ARG1  --  Internal
;;;
;;;    Used by DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1 method.  Info is
;;; a list of conses (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).  Will
;;; never be called with a hairy type as type2, since the hairy type type2
;;; method gets first crack.
;;;
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  (values
   (and (typep type2 'class)
	(dolist (x info nil)
	  (when (or (not (cdr x))
		    (csubtypep type1 (specifier-type (cdr x))))
	    (return
	     (or (eq type2 (car x))
		 (let ((inherits (layout-inherits (class-layout (car x)))))
		   (dotimes (i (length inherits) nil)
		     (when (eq type2 (layout-class (svref inherits i)))
		       (return t)))))))))
   t))

(eval-when (compile eval)
;;; DEFINE-SUPERCLASSES  --  Interface
;;;
;;;    Takes a list of specs of the form (superclass &optional guard).
;;; Consider one spec (with no guard): any instance of type-class is also a
;;; subtype of SUPERCLASS and of any of its superclasses.  If there are
;;; multiple specs, then some will have guards.  We choose the first spec whose
;;; guard is a supertype of TYPE1 and use its superclass.  In effect, a
;;; sequence of guards G0, G1, G2 is actually G0, (and G1 (not G0)),
;;; (and G2 (not (or G0 G1))).
;;;
(defmacro define-superclasses (type-class &rest specs)
  (let ((info
	 (mapcar #'(lambda (spec)
		     (destructuring-bind (super &optional guard)
					 spec
		       (cons (find-class super) guard)))
		 specs)))
    `(cold-load-init
      (setf (type-class-complex-subtypep-arg1
	     (type-class-or-lose ',type-class))
	    #'(lambda (type1 type2)
		(has-superclasses-complex-subtypep-arg1 type1 type2 ',info)))
       
       (setf (type-class-complex-subtypep-arg2
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-subtypep-arg2)
       
       (setf (type-class-complex-intersection
	      (type-class-or-lose ',type-class))
	     #'delegate-complex-intersection))))

); eval-when (compile eval)


;;;; Function and Values types.
;;;
;;;    Pretty much all of the general type operations are illegal on VALUES
;;; types, since we can't discriminate using them, do SUBTYPEP, etc.  FUNCTION
;;; types are acceptable to the normal type operations, but are generally
;;; considered to be equivalent to FUNCTION.  These really aren't true types in
;;; any type theoretic sense, but we still parse them into CTYPE structures for
;;; two reasons:
;;; -- Parsing and unparsing work the same way, and indeed we can't tell
;;;    whether a type is a function or values type without parsing it.
;;; -- Many of the places that can be annotated with real types can also be
;;;    annotated function or values types.


;;; The Args-Type structure is used both to represent Values types and
;;; and Function types.
;;;
(defstruct (args-type (:include ctype)
		      (:print-function %print-type))
  ;;
  ;; Lists of the type for each required and optional argument.
  (required nil :type list)
  (optional nil :type list)
  ;;
  ;; The type for the rest arg.  NIL if there is no rest arg.
  (rest nil :type (or ctype null))
  ;;
  ;; True if keyword arguments are specified.
  (keyp nil :type boolean)
  ;;
  ;; List of key-info structures describing the keyword arguments.
  (keywords nil :type list)
  ;;
  ;; True if other keywords are allowed.
  (allowp nil :type boolean))

(defstruct (key-info (:pure t))
  ;;
  ;; The keyword.
  (name (required-argument) :type keyword)
  ;;
  ;; Type of this argument.
  (type (required-argument) :type ctype))


(define-type-class values)

(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
		    (type1 type2)
  (declare (ignore type2))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
		    (type1 type2)
  (declare (ignore type1))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type2)))

(defstruct (values-type
	    (:include args-type
		      (:class-info (type-class-or-lose 'values)))
	    (:print-function %print-type)))

(define-type-method (values :unparse) (type)
  (cons 'values (unparse-args-types type)))


;;; TYPE=-LIST  --  Internal
;;;
;;;    Return true if List1 and List2 have the same elements in the same
;;; positions according to TYPE=.  We return NIL, NIL if there is an uncertain
;;; comparison. 
;;;
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
	   (values nil t)
	   (values t t)))
    (multiple-value-bind (val win)
			 (type= (first types1) (first types2))
      (unless win
	(return (values nil nil)))
      (unless val
	(return (values nil t))))))


(define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-type-rest type1))
	(rest2 (args-type-rest type2)))
    (cond ((or (args-type-keyp type1) (args-type-keyp type2)
	       (args-type-allowp type1) (args-type-allowp type2))
	   (values nil nil))
	  ((and rest1 rest2 (type/= rest1 rest2))
	   (type= rest1 rest2))
	  ((or rest1 rest2)
	   (values nil t))
	  (t
	   (multiple-value-bind (req-val req-win)
				(type=-list (values-type-required type1)
					    (values-type-required type2))
	     (multiple-value-bind (opt-val opt-win)
				  (type=-list (values-type-optional type1)
					      (values-type-optional type2))
	       (values (and req-val opt-val) (and req-win opt-win))))))))

(define-type-class function)

(defstruct (function-type
	    (:include args-type
		      (class-info (type-class-or-lose 'function)))
	    (:print-function %print-type))
  ;;
  ;; True if the arguments are unrestrictive, i.e. *.
  (wild-args nil :type boolean)
  ;;
  ;; Type describing the return values.  This is a values type
  ;; when multiple values were specified for the return.
  (returns (required-argument) :type ctype))


;;; A flag that we can bind to cause complex function types to be unparsed as
;;; FUNCTION.  Useful when we want a type that we can pass to TYPEP.
;;;
(defvar *unparse-function-type-simplify*)
(cold-load-init (setq *unparse-function-type-simplify* nil))


(define-type-method (function :unparse) (type)
  (if *unparse-function-type-simplify*
      'function
      (list 'function
	    (if (function-type-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (function-type-returns type)))))


;;; Since all function types are equivalent to FUNCTION, they are all subtypes
;;; of each other.
;;;
(define-type-method (function :simple-subtypep) (type1 type2)
  (declare (ignore type1 type2))
  (values t t))

(define-superclasses function (function))

;;; The union or intersection of two FUNCTION types is FUNCTION.
;;;
(define-type-method (function :simple-union) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))
;;;
(define-type-method (function :simple-intersection) (type1 type2)
  (declare (ignore type1 type2))
  (values (specifier-type 'function) t))


;;; ### Not very real, but good enough for redefining transforms according to
;;; type:
;;;
(define-type-method (function :simple-=) (type1 type2)
  (values (equalp type1 type2) t))


(define-type-class constant values)

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARGUMENT "type
;;; specifier", which is only meaningful in function argument type specifiers
;;; used within the compiler.
;;;
(defstruct (constant-type (:include ctype
				    (class-info (type-class-or-lose 'constant)))
			  (:print-function %print-type))
  ;;
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (required-argument) :type ctype))

(define-type-method (constant :unparse) (type)
  `(constant-argument ,(type-specifier (constant-type-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(def-type-translator constant-argument (type)
  (make-constant-type :type (specifier-type type)))


;;; Parse-Args-Types  --  Internal
;;;
;;;    Given a lambda-list like values type specification and a Args-Type
;;; structure, fill in the slots in the structure accordingly.  This is used
;;; for both FUNCTION and VALUES types.
;;;
(proclaim '(function parse-args-types (list args-type) void))
(defun parse-args-types (lambda-list result)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
		       (parse-lambda-list lambda-list)
    (when aux
      (error "&Aux in a FUNCTION or VALUES type: ~S." lambda-list))
    (setf (args-type-required result) (mapcar #'specifier-type required))
    (setf (args-type-optional result) (mapcar #'specifier-type optional))
    (setf (args-type-rest result) (if restp (specifier-type rest) nil))
    (setf (args-type-keyp result) keyp)
    (collect ((key-info))
      (dolist (key keys)
	(when (or (atom key) (/= (length key) 2))
	  (error "Keyword type description is not a two-list: ~S." key))
	(let ((kwd (first key)))
	  (when (find kwd (key-info) :key #'key-info-name)
	    (error "Repeated keyword ~S in lambda list: ~S." kwd lambda-list))
	  (key-info (make-key-info :name kwd
				   :type (specifier-type (second key))))))
      (setf (args-type-keywords result) (key-info)))
    (setf (args-type-allowp result) allowp)))


;;; Unparse-Args-Types  --  Internal
;;;
;;;    Return the lambda-list like type specification corresponding
;;; to a Args-Type.
;;;
(proclaim '(function unparse-args-types (args-type) list))
(defun unparse-args-types (type)
  (collect ((result))

    (dolist (arg (args-type-required type))
      (result (type-specifier arg)))

    (when (args-type-optional type)
      (result '&optional)
      (dolist (arg (args-type-optional type))
	(result (type-specifier arg))))

    (when (args-type-rest type)
      (result '&rest)
      (result (type-specifier (args-type-rest type))))

    (when (args-type-keyp type)
      (result '&key)
      (dolist (key (args-type-keywords type))
	(result (list (key-info-name key)
		      (type-specifier (key-info-type key))))))

    (when (args-type-allowp type)
      (result '&allow-other-keys))

    (result)))


(def-type-translator function (&optional args result)
  (let ((res (make-function-type
	      :returns (values-specifier-type result))))
    (if (eq args '*)
	(setf (function-type-wild-args res) t)
	(parse-args-types args res))
    res))


(def-type-translator values (&rest values)
  (let ((res (make-values-type)))
    (parse-args-types values res)
    res))


;;;; Values types interfaces:
;;;
;;;    We provide a few special operations that can be meaningfully used on
;;; values types (as well as on any other type.)
;;;

;;; Single-Value-Type  --  Interface
;;;
;;;    Return the type of the first value indicated by Type.  This is used by
;;; people who don't want to have to deal with values types.
;;;
(declaim (freeze-type values-type) (inline single-value-type))
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((values-type-p type)
	 (or (car (args-type-required type))
	     (car (args-type-optional type))
	     (args-type-rest type)
	     *universal-type*))
	((eq type *wild-type*)
	 *universal-type*)
	(t
	 type)))


;;; FUNCTION-TYPE-NARGS  --  Interface
;;;
;;;    Return the minmum number of arguments that a function can be called
;;; with, and the maximum number or NIL.  If not a function type, return
;;; NIL, NIL.
;;;
(defun function-type-nargs (type)
  (declare (type ctype type))
  (if (function-type-p type)
      (let ((fixed (length (args-type-required type))))
	(if (or (args-type-rest type)
		(args-type-keyp type)
		(args-type-allowp type))
	    (values fixed nil)
	    (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))


;;; Values-Types  --  Interface
;;;
;;;    Determine if Type corresponds to a definite number of values.  The first
;;; value is a list of the types for each value, and the second value is the
;;; number of values.  If the number of values is not fixed, then return NIL
;;; and :Unknown.
;;;
(defun values-types (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
	 (values nil :unknown))
	((not (values-type-p type))
	 (values (list type) 1))
	((or (args-type-optional type)
	     (args-type-rest type)
	     (args-type-keyp type)
	     (args-type-allowp type))
	 (values nil :unknown))
	(t
	 (let ((req (args-type-required type)))
	   (values (mapcar #'single-value-type req) (length req))))))


;;; Values-Type-Types  --  Internal
;;;
;;;    Return two values:
;;; 1] A list of all the positional (fixed and optional) types.
;;; 2] The rest type (if any).  If keywords allowed, *universal-type*.  If no
;;;    keywords or rest, *empty-type*.
;;;
(defun values-type-types (type)
  (declare (type values-type type))
  (values (append (args-type-required type)
		  (args-type-optional type))
	  (cond ((args-type-keyp type) *universal-type*)
		((args-type-rest type))
		(t
		 *empty-type*))))


;;; Fixed-Values-Op  --  Internal
;;;
;;;    Return a list of Operation applied to the types in Types1 and Types2,
;;; padding with Rest2 as needed.  Types1 must not be shorter than Types2.  The
;;; second value is T if Operation always returned a true second value.
;;;
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar #'(lambda (t1 t2)
			(multiple-value-bind (res win)
					     (funcall operation t1 t2)
			  (unless win (setq exact nil))
			  res))
		    types1
		    (append types2
			    (make-list (- (length types1) (length types2))
				       :initial-element rest2)))
	    exact)))


;;; Coerce-To-Values  --  Internal
;;;
;;; If Type isn't a values type, then make it into one:
;;;    <type>  ==>  (values type &rest t)
;;;
(defun coerce-to-values (type)
  (declare (type ctype type))
  (if (values-type-p type)
      type
      (make-values-type :required (list type) :rest *universal-type*)))


;;; Args-Type-Op  --  Internal
;;;
;;;    Do the specified Operation on Type1 and Type2, which may be any type,
;;; including Values types.  With values types such as:
;;;    (values a0 a1)
;;;    (values b0 b1)
;;;
;;; We compute the more useful result:
;;;    (values (<operation> a0 b0) (<operation> a1 b1))
;;;
;;; Rather than the precise result:
;;;    (<operation> (values a0 a1) (values b0 b1))
;;;
;;; This has the virtue of always keeping the values type specifier outermost,
;;; and retains all of the information that is really useful for static type
;;; analysis.  We want to know what is always true of each value independently.
;;; It is worthless to know that IF the first value is B0 then the second will
;;; be B1.
;;;
;;; If the values count signatures differ, then we produce result with the
;;; required value count chosen by Nreq when applied to the number of required
;;; values in type1 and type2.  Any &key values become &rest T (anyone who uses
;;; keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if Operation
;;; returned true as its second value each time we called it.  Since we
;;; approximate the intersection of values types, the second value being true
;;; doesn't mean the result is exact.
;;;
(defun args-type-op (type1 type2 operation nreq)
  (declare (type ctype type1 type2) (type function operation nreq))
  (if (or (values-type-p type1) (values-type-p type2))
      (let ((type1 (coerce-to-values type1))
	    (type2 (coerce-to-values type2)))
	(multiple-value-bind (types1 rest1)
			     (values-type-types type1)
	  (multiple-value-bind (types2 rest2)
			       (values-type-types type2)
	    (multiple-value-bind (rest rest-exact)
				 (funcall operation rest1 rest2)
	      (multiple-value-bind
		  (res res-exact)
		  (if (< (length types1) (length types2))
		      (fixed-values-op types2 types1 rest1 operation)
		      (fixed-values-op types1 types2 rest2 operation))
		(let* ((req (funcall nreq
				     (length (args-type-required type1))
				     (length (args-type-required type2))))
		       (required (subseq res 0 req))
		       (opt (subseq res req))
		       (opt-last (position rest opt :test-not #'type=
					   :from-end t)))
		  (if (find *empty-type* required :test #'type=)
		      (values *empty-type* t)
		      (values (make-values-type
			       :required required
			       :optional (if opt-last
					     (subseq opt 0 (1+ opt-last))
					     ())
			       :rest (if (eq rest *empty-type*) nil rest))
			      (and rest-exact res-exact)))))))))
      (funcall operation type1 type2)))


;;; Values-Type-Union, Values-Type-Intersection  --  Interface
;;;
;;;    Do a union or intersection operation on types that might be values
;;; types.  The result is optimized for utility rather than exactness, but it
;;; is guaranteed that it will be no smaller (more restrictive) than the
;;; precise result.
;;;
(defun-cached (values-type-union :hash-function type-cache-hash
				 :hash-bits 8
				 :default nil
				 :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
	((eq type1 *empty-type*) type2)
	((eq type2 *empty-type*) type1)
	(t
	 (values (args-type-op type1 type2 #'type-union #'min)))))
;;;
(defun-cached (values-type-intersection :hash-function type-cache-hash
					:hash-bits 8
					:values 2
					:default (values nil :empty)
					:init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values type2 t))
	((eq type2 *wild-type*) (values type1 t))
	(t
	 (args-type-op type1 type2 #'type-intersection #'max))))


;;; Values-Types-Intersect  --  Interface
;;;
;;;    Like Types-Intersect, except that it sort of works on values types.
;;; Note that due to the semantics of Values-Type-Intersection, this might
;;; return {T, T} when there isn't really any intersection (?).
;;;
(defun values-types-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	 (values t t))
	((or (values-type-p type1) (values-type-p type2))
	 (multiple-value-bind (res win)
			      (values-type-intersection type1 type2)
	   (values (not (eq res *empty-type*))
		   win)))
	(t
	 (types-intersect type1 type2))))


;;; Values-Subtypep  --  Interface
;;;
;;;    A subtypep-like operation that can be used on any types, including
;;; values types.
;;;
(defun-cached (values-subtypep :hash-function type-cache-hash
			       :hash-bits 8
			       :values 2
			       :default (values nil :empty)
			       :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type2 *wild-type*) (values t t))
	((eq type1 *wild-type*)
	 (values (eq type2 *universal-type*) t))
	((not (values-types-intersect type1 type2))
	 (values nil t))
	(t
	 (if (or (values-type-p type1) (values-type-p type2))
	     (let ((type1 (coerce-to-values type1))
		   (type2 (coerce-to-values type2)))
	       (multiple-value-bind (types1 rest1)
				    (values-type-types type1)
		 (multiple-value-bind (types2 rest2)
				      (values-type-types type2)
		   (cond ((< (length (values-type-required type1))
			     (length (values-type-required type2)))
			  (values nil t))
			 ((< (length types1) (length types2))
			  (values nil nil))
			 ((or (values-type-keyp type1)
			      (values-type-keyp type2))
			  (values nil nil))
			 (t
			  (do ((t1 types1 (rest t1))
			       (t2 types2 (rest t2)))
			      ((null t2)
			       (csubtypep rest1 rest2))
			    (multiple-value-bind
				(res win-p)
				(csubtypep (first t1) (first t2))
			      (unless win-p
				(return (values nil nil)))
			      (unless res
				(return (values nil t))))))))))
	     (csubtypep type1 type2)))))
						       

;;;; Type method interfaces:

;;; Csubtypep  --  Interface
;;;
;;;    Like subtypep, only works on Type structures.
;;;
(defun-cached (csubtypep :hash-function type-cache-hash
			 :hash-bits 8
			 :values 2
			 :default (values nil :empty)
			 :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 type2)
	     (eq type1 *empty-type*)
	     (eq type2 *wild-type*))
	 (values t t))
	((or (eq type1 *wild-type*)
	     (eq type2 *empty-type*))
	 (values nil t))
	(t
	 (invoke-type-method :simple-subtypep :complex-subtypep-arg2
			     type1 type2
			     :complex-arg1 :complex-subtypep-arg1))))

(declaim (start-block))

;;; Type=  --  Interface
;;;
;;;    If two types are definitely equivalent, return true.  The second value
;;; indicates whether the first value is definitely correct.  This should only
;;; fail in the presence of Hairy types.
;;;
(defun-cached (type= :hash-function type-cache-hash
		     :hash-bits 8
		     :values 2
		     :default (values nil :empty)
		     :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values t t)
      (invoke-type-method :simple-= :complex-= type1 type2)))


;;; TYPE/=  --  Interface
;;;
;;;    Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL.  This is useful in cases where the
;;; conservative assumption is =.
;;;
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win)
		       (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

(declaim (end-block))

;;; Type-Union  --  Interface
;;;
;;;    Find a type which includes both types.  Any inexactness is represented
;;; by the fuzzy element types; we return a single value that is precise to the
;;; best of our knowledge.  This result is simplified into the canonical form,
;;; thus is not a UNION type unless there is no other way to represent the
;;; result.
;;; 
(defun-cached (type-union :hash-function type-cache-hash
			  :hash-bits 8
			  :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      type1
      (let ((res (invoke-type-method :simple-union :complex-union
				     type1 type2
				     :default :vanilla)))
	(cond ((eq res :vanilla)
	       (or (vanilla-union type1 type2)
		   (make-union-type (list type1 type2))))
	      (res)
	      (t
	       (make-union-type (list type1 type2)))))))


;;; Type-Intersection  --  Interface
;;;
;;;    Return as restrictive a type as we can discover that is no more
;;; restrictive than the intersection of Type1 and Type2.  The second value is
;;; true if the result is exact.  At worst, we randomly return one of the
;;; arguments as the first value (trying not to return a hairy type).
;;;
(defun-cached (type-intersection :hash-function type-cache-hash
				 :hash-bits 8
				 :values 2
				 :default (values nil :empty)
				 :init-form cold-load-init)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values type1 t)
      (invoke-type-method :simple-intersection :complex-intersection
			  type1 type2
			  :default (values *empty-type* t))))


;;; Types-Intersect  --  Interface
;;;
;;;    The first value is true unless the types don't intersect.  The second
;;; value is true if the first value is definitely correct.  NIL is considered
;;; to intersect with any type.  If T is a subtype of either type, then we also
;;; return T, T.  This way we consider hairy types to intersect with T.
;;;
(defun types-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (multiple-value-bind (val winp)
			   (type-intersection type1 type2)
	(cond ((not winp)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq val *empty-type*) (values nil t))
	      (t (values t t))))))


;;; Type-Specifier  --  Interface
;;;
;;;    Return a Common Lisp type specifier corresponding to this type.
;;;
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class-info type)) type))


;;; VALUES-SPECIFIER-TYPE  --  Interface
;;;
;;;    Return the type structure corresponding to a type specifier.  We pick
;;; off Structure types as a special case.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a type is
;;; defined (or redefined).
;;;
(defun-cached (values-specifier-type
	       :hash-function (lambda (x)
				(the fixnum
				     (logand (the fixnum (cache-hash-eq x))
					     #x3FF)))
	       :hash-bits 10
	       :init-form cold-load-init)
	      ((orig eq))
  (or (info type builtin orig)
      (let ((spec (type-expand orig)))
	(cond
	 ((and (not (eq spec orig))
	       (info type builtin spec)))
	 ((eq (info type kind spec) :instance)
	  (find-class spec))
	 ((typep spec 'class)
	  (if (typep spec 'built-in-class)
	      (or (built-in-class-translation spec) spec)
	      spec))
	 (t
	  (let* ((lspec (if (atom spec) (list spec) spec))
		 (fun (info type translator (car lspec))))
	    (cond (fun (funcall fun lspec))
		  ((or (and (consp spec) (symbolp (car spec)))
		       (symbolp spec))
		   (when *type-system-initialized*
		     (signal 'parse-unknown-type :specifier spec))
		   ;;
		   ;; Inhibit caching...
		   (return-from values-specifier-type
				(make-unknown-type :specifier spec)))
		  (t
		   (error "Bad thing to be a type specifier: ~S." spec)))))))))


;;; SPECIFIER-TYPE  --  Interface
;;;
;;;    Like VALUES-SPECIFIER-TYPE, except that we guarantee to never return a
;;; VALUES type.
;;; 
(defun specifier-type (x)
  (let ((res (values-specifier-type x)))
    (when (values-type-p res)
      (error "VALUES type illegal in this context:~%  ~S" x))
    res))


;;; Type-Expand  --  Interface
;;;
;;;    Similar to Macroexpand, but expands deftypes.  We don't bother returning
;;; a second value.
;;;
(defun type-expand (form)
  (let ((def (cond ((symbolp form)
		    (info type expander form))
		   ((and (consp form) (symbolp (car form)))
		    (info type expander (car form)))
		   (t nil))))
    (if def
	(type-expand (funcall def (if (consp form) form (list form))))
	form)))


;;; Precompute-Types  --  Interface
;;;
;;;    Take a list of type specifiers, compute the translation and define it as
;;; a builtin type.
;;;
(proclaim '(function precompute-types (list) void)) 
(defun precompute-types (specs)
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (unless (unknown-type-p res)
	(setf (info type builtin spec) res)
	(setf (info type kind spec) :primitive)))))


;;;; Builtin types.

;;; The NAMED-TYPE is used to represent *, T and NIL.  These types must be
;;; super or sub types of all types, not just classes and * & NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
;;;
(defstruct (named-type (:include ctype
				 (:class-info (type-class-or-lose 'named)))
		       (:print-function %print-type))
  (name nil :type symbol))

(define-type-class named)

(defvar *wild-type*)
(defvar *empty-type*)
(defvar *universal-type*)

(cold-load-init
 (macrolet ((frob (name var)
	      `(progn
		 (setq ,var (make-named-type :name ',name))
		 (setf (info type kind ',name) :primitive)
		 (setf (info type builtin ',name) ,var))))
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*)))

(define-type-method (named :simple-=) (type1 type2)
  (values (eq type1 type2) t))

(define-type-method (named :simple-subtypep) (type1 type2)
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  (assert (not (hairy-type-p type2)))
  (values (eq type1 *empty-type*) t))

(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (if (hairy-type-p type1)
      (values nil nil)
      (values (not (eq type2 *empty-type*)) t)))

(define-type-method (named :complex-intersection) (type1 type2)
  (vanilla-intersection type1 type2))

(define-type-method (named :unparse) (x)
  (named-type-name x))


;;;; Hairy and unknown types:

;;; The Hairy-Type represents anything too wierd to be described reasonably or
;;; to be useful, such as AND, NOT and SATISFIES and unknown types.  We just
;;; remember the original type spec.
;;;
(defstruct (hairy-type (:include ctype
				 (:class-info (type-class-or-lose 'hairy))
				 (:enumerable t))
		       (:print-function %print-type)
		       (:pure nil))
  ;;
  ;; The Common Lisp type-specifier.
  (specifier nil :type t))

(define-type-class hairy)

(define-type-method (hairy :unparse) (x) (hairy-type-specifier x))

(define-type-method (hairy :complex-subtypep-arg1 :complex-subtypep-arg2
			   :complex-=)
		    (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(define-type-method (hairy :simple-intersection :complex-intersection)
		    (type1 type2)
  (declare (ignore type2))
  (values type1 nil))

(define-type-method (hairy :complex-union) (type1 type2)
  (make-union-type (list type1 type2)))

(define-type-method (hairy :simple-= :simple-subtypep) (type1 type2)
  (if (equal (hairy-type-specifier type1)
	     (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))

(def-type-translator not (&whole x type)
  (declare (ignore type))
  (make-hairy-type :specifier x))

(def-type-translator satisfies (&whole x fun)
  (declare (ignore fun))
  (make-hairy-type :specifier x))


;;; An UNKNOWN-TYPE is a type not known to the type system (not yet defined).
;;; We make this distinction since we don't want to complain about types that
;;; are hairy but defined.
;;;
(defstruct (unknown-type (:include hairy-type)))


;;;; Numeric types.

;;; A list of all the float formats, in order of decreasing precision.
;;;
(eval-when (compile load eval)
  (defconstant float-formats
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
;;;
(deftype float-format () `(member ,@float-formats))


;;; The Numeric-Type is used to represent all numeric types, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
				   (:class-info (type-class-or-lose 'number)))
			 (:print-function %print-type))
  ;;
  ;; The kind of numeric type we have.  NIL if not specified (just NUMBER or
  ;; COMPLEX).
  (class nil :type (member integer rational float nil))
  ;;
  ;; Format for a float type.  NIL if not specified or not a float.  Formats
  ;; which don't exist in a given implementation don't appear here.
  (format nil :type (or float-format null))
  ;;
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER.)
  (complexp :real :type (member :real :complex nil))
  ;;
  ;; The upper and lower bounds on the value.  If null, there is no bound.  If
  ;; a list of a number, the bound is exclusive.  Integer types never have
  ;; exclusive bounds.
  (low nil :type (or number cons null))
  (high nil :type (or number cons null)))


(define-type-class number)

(define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-type-class type1) (numeric-type-class type2))
	(eq (numeric-type-format type1) (numeric-type-format type2))
	(eq (numeric-type-complexp type1) (numeric-type-complexp type2))
	(equal (numeric-type-low type1) (numeric-type-low type2))
	(equal (numeric-type-high type1) (numeric-type-high type2)))
   t))

(define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-type-complexp type))
	 (low (numeric-type-low type))
	 (high (numeric-type-high type))
	 (base (case (numeric-type-class type)
		 (integer 'integer)
		 (rational 'rational)
		 (float (or (numeric-type-format type) 'float))
		 (t 'real))))
    (let ((base+bounds
	   (cond ((and (eq base 'integer) high low)
		  (let ((high-count (logcount high))
			(high-length (integer-length high)))
		    (cond ((= low 0)
			   (cond ((= high 0) '(integer 0 0))
				 ((= high 1) 'bit)
				 ((and (= high-count high-length)
				       (plusp high-length))
				  `(unsigned-byte ,high-length))
				 (t
				  `(mod ,(1+ high)))))
			  ((and (= low vm:target-most-negative-fixnum)
				(= high vm:target-most-positive-fixnum))
			   'fixnum)
			  ((and (= low (lognot high))
				(= high-count high-length)
				(> high-count 0))
			   `(signed-byte ,(1+ high-length)))
			  (t
			   `(integer ,low ,high)))))
		 (high `(,base ,(or low '*) ,high))
		 (low
		  (if (and (eq base 'integer) (= low 0))
		      'unsigned-byte
		      `(,base ,low)))
		 (t base))))
      (ecase complexp
	(:real
	 base+bounds)
	(:complex
	 (if (eq base+bounds 'real)
	     'complex
	     `(complex ,base+bounds)))
	((nil)
	 (assert (eq base+bounds 'real))
	 'number)))))

;;; Numeric-Bound-Test  --  Internal
;;;
;;;    Return true if X is "less than or equal" to Y, taking open bounds into
;;; consideration.  Closed is the predicate used to test the bound on a closed
;;; interval (e.g. <=), and Open is the predicate used on open bounds (e.g. <).
;;; Y is considered to be the outside bound, in the sense that if it is
;;; infinite (NIL), then the test suceeds, whereas if X is infinite, then the
;;; test fails (unless Y is also infinite).
;;;
;;;    This is for comparing bounds of the same kind, e.g. upper and upper.
;;; Use Numeric-Bound-Test* for different kinds of bounds.
;;;
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) nil)
	 ((consp ,x)
	  (if (consp ,y)
	      (,closed (car ,x) (car ,y))
	      (,closed (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))


;;; Numeric-Bound-Test*  --  Internal
;;;
;;;    Used to compare upper and lower bounds.  This is different from the
;;; same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we return true
;;;    if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval, causing
;;;    us to use the Open test for those cases as well.
;;;
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) t)
	 ((consp ,x)
	  (if (consp ,y)
	      (,open (car ,x) (car ,y))
	      (,open (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))


;;; Numeric-Bound-Max  --  Internal
;;;
;;;    Return whichever of the numeric bounds X and Y is "maximal" according to
;;; the predicates Closed (e.g. >=) and Open (e.g. >).  This is only meaningful
;;; for maximizing like bounds, i.e. upper and upper.  If Max-P is true, then
;;; we return NIL if X or Y is NIL, otherwise we return the other arg.
;;;
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
	      (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
	   ((not ,n-y) ,(if max-p nil n-x))
	   ((consp ,n-x)
	    (if (consp ,n-y)
		(if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
		(if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
	   (t
	    (if (consp ,n-y)
		(if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
		(if (,closed ,n-y ,n-x) ,n-y ,n-x))))))


(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-type-class type1))
	(class2 (numeric-type-class type2))
	(complexp2 (numeric-type-complexp type2))
	(format2 (numeric-type-format type2))
	(low1 (numeric-type-low type1))
	(high1 (numeric-type-high type1))
	(low2 (numeric-type-low type2))
	(high2 (numeric-type-high type2)))
    ;;
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-type-complexp type1) complexp2)
		    (null complexp2)))
	   (values nil t))
	  ;;
	  ;; If the classes are specified and different, the types are
	  ;; disjoint unless type2 is rational and type1 is integer.
	  ((not (or (eq class1 class2) (null class2)
		    (and (eq class1 'integer) (eq class2 'rational))))
	   (values nil t))
	  ;;
	  ;; If the float formats are specified and different, the types
	  ;; are disjoint.
	  ((not (or (eq (numeric-type-format type1) format2)
		    (null format2)))
	   (values nil t))
	  ;;
	  ;; Check the bounds.
	  ((and (numeric-bound-test low1 low2 >= >)
		(numeric-bound-test high1 high2 <= <))
	   (values t t))
	  (t
	   (values nil t)))))

(define-superclasses number (generic-number))

;;; NUMERIC-TYPES-ADJACENT  --  Internal
;;;
;;;    If the high bound of Low is adjacent to the low bound of High, then
;;; return T, otherwise NIL.
;;;
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
	(high-bound (numeric-type-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	  ((consp low-bound)
	   (eql (car low-bound) high-bound))
	  ((consp high-bound)
	   (eql (car high-bound) low-bound))
	  ((and (eq (numeric-type-class low) 'integer)
		(eq (numeric-type-class high) 'integer))
	   (eql (1+ low-bound) high-bound))
	  (t
	   nil))))


;;; NUMBER :SIMPLE-UNION method  -- Internal
;;;
;;; Return the a numeric type that is a supertype for both type1 and type2.
;;; 
;;; ### Note: we give up early, so keep from dropping lots of information on
;;; the floor by returning overly general types.
;;;
(define-type-method (number :simple-union) (type1 type2)
  (declare (type numeric-type type1 type2))
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (let ((class1 (numeric-type-class type1))
	       (format1 (numeric-type-format type1))
	       (complexp1 (numeric-type-complexp type1))
	       (class2 (numeric-type-class type2))
	       (format2 (numeric-type-format type2))
	       (complexp2 (numeric-type-complexp type2)))
	   (when (and (eq class1 class2)
		      (eq format1 format2)
		      (eq complexp1 complexp2)
		      (or (numeric-types-intersect type1 type2)
			  (numeric-types-adjacent type1 type2)
			  (numeric-types-adjacent type2 type1)))
	     (make-numeric-type
	      :class class1
	      :format format1
	      :complexp complexp1
	      :low (numeric-bound-max (numeric-type-low type1)
				      (numeric-type-low type2)
				      < <= t)
	      :high (numeric-bound-max (numeric-type-high type1)
				       (numeric-type-high type2)
				       > >= t)))))))


(cold-load-init
  (setf (info type kind 'number) :primitive)
  (setf (info type builtin 'number)
	(make-numeric-type :complexp nil)))


(def-type-translator complex (&optional spec)
  (if (eq spec '*)
      (make-numeric-type :complexp :complex)
      (let ((type (specifier-type spec)))
	(unless (numeric-type-p type)
	  (error "Component type for Complex is not numeric: ~S." spec))
	(when (eq (numeric-type-complexp type) :complex)
	  (error "Component type for Complex is complex: ~S." spec))

	(let ((res (copy-numeric-type type)))
	  (setf (numeric-type-complexp res) :complex)
	  res))))


;;; Check-Bound  --  Internal
;;;
;;;    Check that X is a well-formed numeric bound of the specified Type.
;;; If X is *, return NIL, otherwise return the bound.
;;;
(defmacro check-bound (x type)
  `(cond ((eq ,x '*) nil)
	 ((or (typep ,x ',type)
	      (and (consp ,x) (typep (car ,x) ',type) (null (cdr ,x))))
	  ,x)
	 (t
	  (error "Bound is not *, a ~A or a list of a ~A: ~S" ',type ',type ,x))))

(def-type-translator integer (&optional low high)
  (let* ((l (check-bound low integer))
	 (lb (if (consp l) (1+ (car l)) l))
	 (h (check-bound high integer))
	 (hb (if (consp h) (1- (car h)) h)))
    (when (and hb lb (< hb lb))
      (error "Lower bound ~S is greater than upper bound ~S." l h))
    (make-numeric-type :class 'integer  :complexp :real
		       :enumerable (not (null (and l h)))
		       :low lb
		       :high hb)))

(deftype mod (n)
  (unless (and (integerp n) (> n 0))
    (error "Bad N specified for MOD type specifier: ~S." n))
  `(integer 0 ,(1- n)))

(deftype signed-byte (&optional s)
  (cond ((eq s '*) 'integer)
	((and (integerp s) (> s 1))
	 (let ((bound (ash 1 (1- s))))
	   `(integer ,(- bound) ,(1- bound))))
	(t
	 (error "Bad size specified for SIGNED-BYTE type specifier: ~S." s))))

(deftype unsigned-byte (&optional s)
  (cond ((eq s '*) '(integer 0))
	((and (integerp s) (> s 0))
	 `(integer 0 ,(1- (ash 1 s))))
	(t
	 (error "Bad size specified for UNSIGNED-BYTE type specifier: ~S." s))))


(defmacro def-bounded-type (type class format)
  `(def-type-translator ,type (&optional low high)
     (let ((lb (check-bound low ,type))
	   (hb (check-bound high ,type)))
       (unless (numeric-bound-test* lb hb <= <)
	 (error "Lower bound ~S is not less than upper bound ~S." low high))
       (make-numeric-type :class ',class :format ',format :low lb :high hb))))

(def-bounded-type rational rational nil)
(def-bounded-type float float nil)
(def-bounded-type real nil nil)

(defmacro define-float-format (f)
  `(def-bounded-type ,f float ,f))

(define-float-format short-float)
(define-float-format single-float)
(define-float-format double-float)
(define-float-format long-float)

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-type type1 type2))
  (let* ((class1 (numeric-type-class type1))
	 (class2 (numeric-type-class type2))
	 (complexp1 (numeric-type-complexp type1))
	 (complexp2 (numeric-type-complexp type2))
	 (format1 (numeric-type-format type1))
	 (format2 (numeric-type-format type2))
	 (low1 (numeric-type-low type1))
	 (high1 (numeric-type-high type1))
	 (low2 (numeric-type-low type2))
	 (high2 (numeric-type-high type2)))
    ;;
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
		    (null complexp1) (null complexp2)))
	   nil)
	  ;;
	  ;; If either type is a float, then the other must either be specified
	  ;; to be a float or unspecified.  Otherwise, they are disjoint.
	  ((and (eq class1 'float) (not (member class2 '(float nil)))) nil)
	  ((and (eq class2 'float) (not (member class1 '(float nil)))) nil)
	  ;;
	  ;; If the float formats are specified and different, the types
	  ;; are disjoint.
	  ((not (or (eq format1 format2) (null format1) (null format2)))
	   nil)
	  (t
	   ;;
	   ;; Check the bounds.  This is a bit odd because we must always have
	   ;; the outer bound of the interval as the second arg.
	   (if (numeric-bound-test high1 high2 <= <)
	       (or (and (numeric-bound-test low1 low2 >= >)
			(numeric-bound-test* low1 high2 <= <))
		   (and (numeric-bound-test low2 low1 >= >)
			(numeric-bound-test* low2 high1 <= <)))
	       (or (and (numeric-bound-test* low2 high1 <= <)
			(numeric-bound-test low2 low1 >= >))
		   (and (numeric-bound-test high2 high1 <= <)
			(numeric-bound-test* high2 low1 >= >))))))))


;;; Round-Numeric-Bound  --  Internal
;;;
;;;    Take the numeric bound X and convert it into something that can be used
;;; as a bound in a numeric type with the specified Class and Format.  If up-p
;;; is true, then we round up as needed, otherwise we round down.  Up-p true
;;; implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by Numeric-Type-Intersection to mash the bound into the
;;; appropriate type number.  X may only be a float when Class is Float.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow or
;;; underflow.  This happens when the bound doesn't fit in the specified
;;; format.  In this case, we should really return the appropriate
;;; {Most | Least}-{Positive | Negative}-XXX-Float float of desired format.
;;; But these conditions aren't currently signalled in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we should
;;; probably convert it to a closed bound of the closest float in the specified
;;; format.  In general, open float bounds are fucked.
;;;
(defun round-numeric-bound (x class format up-p)
  (if x
      (let ((cx (if (consp x) (car x) x)))
	(ecase class
	  ((nil rational) x)
	  (integer
	   (if (and (consp x) (integerp cx))
	       (if up-p (1+ cx) (1- cx))
	       (if up-p (ceiling cx) (floor cx))))
	  (float
	   (let ((res (if format (coerce cx format) (float cx))))
	     (if (consp x) (list res) res)))))
      nil))


;;; Number :Simple-Intersection type method  --  Internal
;;;
;;;    Handle the case of Type-Intersection on two numeric types.  We use
;;; Types-Intersect to throw out the case of types with no intersection.  If an
;;; attribute in Type1 is unspecified, then we use Type2's attribute, which
;;; must be at least as restrictive.  If the types intersect, then the only
;;; attributes that can be specified and different are the class and the
;;; bounds.
;;;
;;;    When the class differs, we use the more restrictive class.  The only
;;; interesting case is rational/integer, since rational includes integer.
;;;
;;;    We make the result lower (upper) bound the maximum (minimum) of the
;;; argument lower (upper) bounds.  We convert the bounds into the
;;; appropriate numeric type before maximizing.  This avoids possible confusion
;;; due to mixed-type comparisons (but I think the result is the same).
;;;
(define-type-method (number :simple-intersection) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
      (let* ((class1 (numeric-type-class type1))
	     (class2 (numeric-type-class type2))
	     (class (ecase class1
		      ((nil) class2)
		      ((integer float) class1)
		      (rational (if (eq class2 'integer) 'integer 'rational))))
	     (format (or (numeric-type-format type1)
			 (numeric-type-format type2))))
	(values
	 (make-numeric-type
	  :class class
	  :format format
	  :complexp (or (numeric-type-complexp type1)
			(numeric-type-complexp type2))
	  :low (numeric-bound-max
		(round-numeric-bound (numeric-type-low type1)
				     class format t)
		(round-numeric-bound (numeric-type-low type2)
				     class format t)
		>= > nil)
	  :high (numeric-bound-max
		 (round-numeric-bound (numeric-type-high type1)
				      class format nil)
		 (round-numeric-bound (numeric-type-high type2)
				      class format nil)
		 <= < nil))
	 t))
      (values *empty-type* t)))


;;; Float-Format-Max  --  Interface
;;;
;;;    Given two float formats, return the one with more precision.  If either
;;; one is null, return NIL.
;;;
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f float-formats (error "Bad float format: ~S." f1))
      (when (or (eq f f1) (eq f f2))
	(return f)))))


;;; Numeric-Contagion  --  Interface
;;;
;;;    Return the result of an operation on Type1 and Type2 according to the
;;; rules of numeric contagion.  This is always NUMBER, some float format
;;; (possibly complex) or RATIONAL.  Due to rational canonicalization, there
;;; isn't much we can do here with integers or rational complex numbers.
;;;
;;;    If either argument is not a Numeric-Type, then return NUMBER.  This is
;;; useful mainly for allowing types that are technically numbers, but not a
;;; Numeric-Type. 
;;;
(defun numeric-contagion (type1 type2)
  (if (and (numeric-type-p type1) (numeric-type-p type2))
      (let ((class1 (numeric-type-class type1))
	    (class2 (numeric-type-class type2))
	    (format1 (numeric-type-format type1))
	    (format2 (numeric-type-format type2))
	    (complexp1 (numeric-type-complexp type1))
	    (complexp2 (numeric-type-complexp type2)))
	(cond ((or (null complexp1)
		   (null complexp2))
	       (specifier-type 'number))
	      ((eq class1 'float)
	       (make-numeric-type
		:class 'float
		:format (ecase class2
			  (float (float-format-max format1 format2))
			  ((integer rational) format1)
			  ((nil) nil))
		:complexp (if (or (eq complexp1 :complex)
				  (eq complexp2 :complex))
			      :complex
			      :real)))
	      ((eq class2 'float) (numeric-contagion type2 type1))
	      ((and (eq complexp1 :real) (eq complexp2 :real))
	       (make-numeric-type
		:class (and class1 class2 'rational)
		:complexp :real))
	      (t
	       (specifier-type 'number))))
      (specifier-type 'number)))


;;;; Array types:

;;; The Array-Type is used to represent all array types, including things such
;;; as SIMPLE-STRING.
;;;
(defstruct (array-type (:include ctype
				 (:class-info (type-class-or-lose 'array)))
		       (:print-function %print-type))
  ;;
  ;; The dimensions of the array.  * if unspecified.  If a dimension is
  ;; unspecified, it is *.
  (dimensions '* :type (or list (member *)))
  ;;
  ;; Is this not a simple array type?
  (complexp '* :type (member t nil *))
  ;;
  ;; The element type as originally specified.
  (element-type (required-argument) :type ctype)
  ;;
  ;; The element type as it is specialized in this implementation.
  (specialized-element-type *wild-type* :type ctype))

(define-type-class array)


;;; Specialized-Element-Type-Maybe  --  Internal
;;;
;;;      What this does depends on the setting of the
;;; *use-implementation-types* switch.  If true, return the specialized element
;;; type, otherwise return the original element type.
;;;
(defun specialized-element-type-maybe (type)
  (declare (type array-type type))
  (if *use-implementation-types*
      (array-type-specialized-element-type type)
      (array-type-element-type type)))


(define-type-method (array :simple-=) (type1 type2)
  (values (and (equal (array-type-dimensions type1)
		      (array-type-dimensions type2))
	       (eq (array-type-complexp type1)
		   (array-type-complexp type2))
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	  t))


(define-type-method (array :unparse) (type)
  (let ((dims (array-type-dimensions type))
	(eltype (type-specifier (array-type-element-type type)))
	(complexp (array-type-complexp type)))
    (cond ((eq dims '*)
	   (if (eq eltype '*)
	       (if complexp 'array 'simple-array)
	       (if complexp `(array ,eltype) `(simple-array ,eltype))))
	  ((= (length dims) 1) 
	   (if complexp
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'bit-vector)
		     (base-char 'base-string)
		     (character 'string)
		     (* 'vector)
		     (t `(vector ,eltype)))
		   (case eltype
		     (bit `(bit-vector ,(car dims)))
		     (base-char `(base-string ,(car dims)))
		     (character `(string ,(car dims)))
		     (t `(vector ,eltype ,(car dims)))))
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'simple-bit-vector)
		     (base-char 'simple-base-string)
		     (character 'simple-string)
		     ((t) 'simple-vector)
		     (t `(simple-array ,eltype (*))))
		   (case eltype
		     (bit `(simple-bit-vector ,(car dims)))
		     (base-char `(simple-base-string ,(car dims)))
		     (character `(simple-string ,(car dims)))
		     ((t) `(simple-vector ,(car dims)))
		     (t `(simple-array ,eltype ,dims))))))
	  (t
	   (if complexp
	       `(array ,eltype ,dims)
	       `(simple-array ,eltype ,dims))))))


(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp2 (array-type-complexp type2)))
    ;;
    ;; See if dimensions are compatible.
    (cond ((not (or (eq dims2 '*)
		    (and (not (eq dims1 '*))
			 (= (length dims1) (length dims2))
			 (every #'(lambda (x y)
				    (or (eq y '*) (eql x y)))
				dims1 dims2))))
	   (values nil t))
	  ;;
	  ;; See if complexp is compatible.
	  ((not (or (eq complexp2 '*)
		    (eq (array-type-complexp type1) complexp2)))
	   (values nil t))
	  ;;
	  ;; If the type2 eltype is wild, we win.  Otherwise, the types must be
	  ;; identical.
	  ((or (eq (array-type-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	   (values t t))
	  (t
	   (values nil t)))))

(define-superclasses array
  (string string)
  (vector vector)
  (array))

(defun array-types-intersect (type1 type2)
  (declare (type array-type type1 type2))
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp1 (array-type-complexp type1))
	(complexp2 (array-type-complexp type2)))
    ;;
    ;; See if dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
		    (and (= (length dims1) (length dims2))
			 (every #'(lambda (x y)
				    (or (eq x '*) (eq y '*) (= x y)))
				dims1 dims2))))
	   (values nil t))
	  ;;
	  ;; See if complexp is compatible.
	  ((not (or (eq complexp1 '*) (eq complexp2 '*)
		    (eq complexp1 complexp2)))
	   (values nil t))
	  ;;
	  ;; If either element type is wild, then they intersect.  Otherwise,
	  ;; the types must be identical.
	  ((or (eq (array-type-element-type type1) *wild-type*)
	       (eq (array-type-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))

	   (values t t))
	  (t
	   (values nil t)))))


(define-type-method (array :simple-intersection) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
	    (dims2 (array-type-dimensions type2))
	    (complexp1 (array-type-complexp type1))
	    (complexp2 (array-type-complexp type2))
	    (eltype1 (array-type-element-type type1))
	    (eltype2 (array-type-element-type type2)))
	(values
	 (specialize-array-type
	  (make-array-type
	   :dimensions (cond ((eq dims1 '*) dims2)
			     ((eq dims2 '*) dims1)
			     (t
			      (mapcar #'(lambda (x y) (if (eq x '*) y x))
				      dims1 dims2)))
	   :complexp (if (eq complexp1 '*) complexp2 complexp1)
	   :element-type (if (eq eltype1 *wild-type*) eltype2 eltype1)))
	 t))
      (values *empty-type* t)))
  

;;; Check-Array-Dimensions  --  Internal
;;;
;;;    Check a supplied dimension list to determine if it is legal.
;;;
(defun check-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (error "Arrays can't have a negative number of dimensions: ~D." dims))
     (when (>= dims array-rank-limit)
       (error "Array type has too many dimensions: ~S." dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) array-rank-limit)
       (error "Array type has too many dimensions: ~S." dims))
     (dolist (dim dims)
       (unless (eq dim '*)
	 (unless (and (integerp dim)
		      (>= dim 0) (< dim array-dimension-limit))
	   (error "Bad dimension in array type: ~S." dim))))
     dims)
    (t
     (error "Array dimensions is not a list, integer or *:~%  ~S"
	    dims))))
	       
(def-type-translator array (&optional element-type dimensions)
  (specialize-array-type
   (make-array-type :dimensions (check-array-dimensions dimensions)
		    :element-type (specifier-type element-type))))

(def-type-translator simple-array (&optional element-type dimensions)
  (specialize-array-type
   (make-array-type :dimensions (check-array-dimensions dimensions)
		    :element-type (specifier-type element-type)
		    :complexp nil)))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))
(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))
(deftype string (&optional size)
  `(or (array character (,size))
       (base-string ,size)))
(deftype simple-string (&optional size)
  `(or (simple-array character (,size))
       (simple-base-string ,size)))

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))


;;;; Member types.

;;; The Member-Type represents uses of the MEMBER type specifier.  We bother
;;; with this at this level because MEMBER types are fairly important and union
;;; and intersection are well defined.

(defstruct (member-type (:include ctype
				  (:class-info (type-class-or-lose 'member))
				  (:enumerable t))
			(:print-function %print-type)
			(:pure nil))
  ;;
  ;; The things in the set, with no duplications.
  (members nil :type list))


(define-type-class member)

(define-type-method (member :unparse) (type)
  (let ((members (member-type-members type)))
    (if (equal members '(nil))
	'null
	`(member ,@members))))

(define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-type-members type1) (member-type-members type2))
	  t))


(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op ctypep type2 (member-type-members type1)
			   :list-first t)
	    t)))

;;; We punt if the odd type is enumerable and intersects with the member type.
;;; If not enumerable, then it is definitely not a subtype of the member type.
;;;
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
	((types-intersect type1 type2) (values nil nil))
	(t
	 (values nil t))))

(define-type-method (member :simple-intersection) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (cond ((subsetp mem1 mem2) type1)
		  ((subsetp mem2 mem1) type2)
		  (t
		   (let ((res (intersection mem1 mem2)))
		     (if res
			 (make-member-type :members res)
			 *empty-type*))))
	    t)))

(define-type-method (member :complex-intersection) (type1 type2)
  (block PUNT
    (collect ((members))
      (let ((mem2 (member-type-members type2)))
	(dolist (member mem2)
	  (multiple-value-bind (val win)
			       (ctypep member type1)
	    (unless win
	      (return-from PUNT (values type2 nil)))
	    (when val (members member))))

	(values (cond ((subsetp mem2 (members)) type2)
		      ((null (members)) *empty-type*)
		      (t
		       (make-member-type :members (members))))
		t)))))


;;; We don't need a :COMPLEX-UNION, since the only interesting case is a union
;;; type, and the member/union interaction is handled by the union type
;;; method.
(define-type-method (member :simple-union) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	  ((subsetp mem2 mem1) type1)
	  (t
	   (make-member-type :members (union mem1 mem2))))))


(define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (and (subsetp mem1 mem2) (subsetp mem2 mem1))
	    t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win)
			   (csubtypep type2 type1)
	(if (or val (not win))
	    (values nil nil)
	    (values nil t)))
      (values nil t)))


(def-type-translator member (&rest members)
  (let ((mem (remove-duplicates members)))
    (if mem
	(make-member-type :members mem)
	*empty-type*)))


;;;; Union types:

;;; The Union-Type represents uses of the OR type specifier which can't be
;;; canonicalized to something simpler.  Canonical form:
;;;
;;; 1] There is never more than one Member-Type component.
;;; 2] There are never any Union-Type components.
;;;
(defstruct (union-type (:include ctype
				 (:class-info (type-class-or-lose 'union)))
		       (:constructor %make-union-type (enumerable types))
		       (:print-function %print-type))
  ;;
  ;; The types in the union.
  (types nil :type list))


;;; MAKE-UNION-TYPE  --  Internal
;;;
;;;    Make a union type from the specifier types, setting ENUMERABLE in the
;;; result if all are enumerable.
;;;
(defun make-union-type (types)
  (declare (list types))
  (%make-union-type (every #'type-enumerable types) types))


(define-type-class union)


;;;    If List, then return that, otherwise the OR of the component types.
;;;
(define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (if (type= type (specifier-type 'list))
      'list
      `(or ,@(mapcar #'type-specifier (union-type-types type)))))



;;; Two union types are equal if every type in one is equal to some type in the
;;; other.
;;;
(define-type-method (union :simple-=) (type1 type2)
  (block PUNT
    (let ((types1 (union-type-types type1))
	  (types2 (union-type-types type2)))
      (values (and (dolist (type1 types1 t)
		     (unless (any-type-op type= type1 types2)
		       (return nil)))
		   (dolist (type2 types2 t)
		     (unless (any-type-op type= type2 types1)
		       (return nil))))
	      t))))


;;; Similarly, a union type is a subtype of another if every element of Type1
;;; is a subtype of some element of Type2.
;;;
(define-type-method (union :simple-subtypep) (type1 type2)
  (block PUNT
    (let ((types2 (union-type-types type2)))
      (values (dolist (type1 (union-type-types type1) t)
		(unless (any-type-op csubtypep type1 types2)
		  (return nil)))
	      t))))


(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op csubtypep type2 (union-type-types type1)
			   :list-first t)
	    t)))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (block PUNT
    (values (any-type-op csubtypep type1 (union-type-types type2)) t)))


(define-type-method (union :complex-union) (type1 type2)
  (let* ((class1 (type-class-info type1)))
    (collect ((res))
      (let ((this-type type1))
	(dolist (type (union-type-types type2)
		      (if (res)
			  (make-union-type (cons this-type (res)))
			  this-type))
	  (cond ((eq (type-class-info type) class1)
		 (let ((union (funcall (type-class-simple-union class1)
				       this-type type)))
		   (if union
		       (setq this-type union)
		       (res type))))
		((csubtypep type this-type))
		((csubtypep type1 type) (return type2))
		(t
		 (res type))))))))

;;; For the union of union types, we let the :COMPLEX-UNION method do the work.
;;;
(define-type-method (union :simple-union) (type1 type2)
  (let ((res type1))
    (dolist (t2 (union-type-types type2) res)
      (setq res (type-union res t2)))))


(define-type-method (union :simple-intersection :complex-intersection)
		    (type1 type2)
  (let ((res *empty-type*)
	(win t))
    (dolist (type (union-type-types type2) (values res win))
      (multiple-value-bind (int w)
			   (type-intersection type1 type)
	(setq res (type-union res int))
	(unless w (setq win nil))))))


(def-type-translator or (&rest types)
  (reduce #'type-union
	  (mapcar #'specifier-type types)
	  :initial-value *empty-type*))


;;;    We don't actually have intersection types, since the result of
;;; reasonable type intersections is always describable as a union of simple
;;; types.  If something is too hairy to fit this mold, then we make a hairy
;;; type.
(def-type-translator and (&whole spec &rest types)
  (let ((res *wild-type*))
    (dolist (type types res)
      (let ((ctype (specifier-type type)))
	(multiple-value-bind (int win)
			     (type-intersection res ctype)
	  (unless win
	    (return (make-hairy-type :specifier spec)))
	  (setq res int))))))


;;;; Alien-type types

(defstruct (alien-type-type
	    (:include ctype
		      (:class-info (type-class-or-lose 'alien)))
	    (:print-function %print-type)
	    (:constructor %make-alien-type-type (alien-type)))
  (alien-type nil :type alien-type))

(define-type-class alien)

(define-type-method (alien :unparse) (type)
  `(alien ,(unparse-alien-type (alien-type-type-alien-type type))))

(define-type-method (alien :simple-subtypep) (type1 type2)
  (values (alien-subtype-p (alien-type-type-alien-type type1)
			   (alien-type-type-alien-type type2))
	  t))

(define-superclasses alien (alien-value))

(define-type-method (alien :simple-=) (type1 type2)
  (let ((alien-type-1 (alien-type-type-alien-type type1))
	(alien-type-2 (alien-type-type-alien-type type2)))
    (values (or (eq alien-type-1 alien-type-2)
		(alien-type-= alien-type-1 alien-type-2))
	    t)))


(def-type-translator alien (&optional (alien-type nil))
  (typecase alien-type
    (null
     (make-alien-type-type))
    (alien-type
     (make-alien-type-type alien-type))
    (t
     (make-alien-type-type (parse-alien-type alien-type)))))

(defun make-alien-type-type (&optional alien-type)
  (if alien-type
      (let ((lisp-rep-type (compute-lisp-rep-type alien-type)))
	(if lisp-rep-type
	    (specifier-type lisp-rep-type)
	    (%make-alien-type-type alien-type)))
      *universal-type*))


;;; TYPE-DIFFERENCE  --  Interface
;;;
;;;    Return the type that describes all objects that are in X but not in Y.
;;; If we can't determine this type, then return NIL.
;;;
;;;    For now, we only are clever dealing with union and member types.  If
;;; either type is not a union type, then we pretend that it is a union of just
;;; one type.  What we do is remove from X all the types that are a subtype any
;;; type in Y.  If any type in X intersects with a type in Y but is not a
;;; subtype, then we give up.
;;;
;;;    We must also special-case any member type that appears in the union.  We
;;; remove from X's members all objects that are TYPEP to Y.  If Y has any
;;; members, we must be careful that none of those members are CTYPEP to any
;;; of Y's non-member types.  We give up in this case, since to compute that
;;; difference we would have to break the type from X into some collection of
;;; types that represents the type without that particular element.  This seems
;;; too hairy to be worthwhile, given its low utility.
;;;
(defun type-difference (x y)
  (let ((x-types (if (union-type-p x) (union-type-types x) (list x)))
	(y-types (if (union-type-p y) (union-type-types y) (list y))))
    (collect ((res))
      (dolist (x-type x-types)
	(if (member-type-p x-type)
	    (collect ((members))
	      (dolist (mem (member-type-members x-type))
		(multiple-value-bind (val win)
				     (ctypep mem y)
		  (unless win (return-from type-difference nil))
		  (unless val
		    (members mem))))
	      (when (members)
		(res (make-member-type :members (members)))))
	    (dolist (y-type y-types (res x-type))
	      (multiple-value-bind (val win)
				   (csubtypep x-type y-type)
		(unless win (return-from type-difference nil))
		(when val (return))
		(when (types-intersect x-type y-type)
		  (return-from type-difference nil))))))

      (let ((y-mem (find-if #'member-type-p y-types)))
	(when y-mem
	  (let ((members (member-type-members y-mem)))
	    (dolist (x-type x-types)
	      (unless (member-type-p x-type)
		(dolist (member members)
		  (multiple-value-bind (val win)
				       (ctypep member x-type)
		    (when (or (not win) val)
		      (return-from type-difference nil)))))))))

      (cond ((null (res)) *empty-type*)
	    ((null (rest (res))) (first (res)))
	    (t
	     (make-union-type (res)))))))


;;;; Miscellaneous interfaces:

;;; CLEAR-TYPE-CACHES  --  Interface
;;;
;;;    Clear memoization of all type system operations that can be altered by
;;; type definition/redefinition.
;;;
(defun clear-type-caches ()
  (when *type-system-initialized*
    (dolist (sym '(values-specifier-type-cache-clear
		   values-type-union-cache-clear
		   type-union-cache-clear
		   values-subtypep-cache-clear
		   csubtypep-cache-clear
		   type-intersection-cache-clear
		   values-type-intersection-cache-clear))
      (funcall (symbol-function sym))))
  (undefined-value))


;;; CTypep  --  Interface
;;;
;;;    If Type is a type that we can do a compile-time test on, then return the
;;; whether the object is of that type as the first value and second value
;;; true.  Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types, pick off FUNCTION and UNION types.  For
;;; structure types, we require that the type be defined in both the current
;;; and compiler environments, and that the INCLUDES be the same.
;;;
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-type named-type member-type array-type built-in-class)
     (values (%typep obj type) t))
    (class
     (if (if (csubtypep type (specifier-type 'funcallable-instance))
	     (funcallable-instance-p obj)
	     (%instancep obj))
	 (if (eq (class-layout type)
		 (info type compiler-layout (class-name type)))
	     (values (typep obj type) t)
	     (values nil nil))
	 (values nil t)))
    (union-type
     (dolist (mem (union-type-types type) (values nil t))
       (multiple-value-bind (val win)
			    (ctypep obj mem)
	 (unless win (return (values nil nil)))
	 (when val (return (values t t))))))
    (function-type
     (values (functionp obj) t))
    (unknown-type
     (values nil nil))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
	    (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
	 (and
	  (if (atom hairy-spec)
	      (values t t)
	      (dolist (spec (cdr hairy-spec) (values t t))
		(multiple-value-bind (res win)
				     (ctypep obj (specifier-type spec))
		  (unless win (return (values nil nil)))
		  (unless res (return (values nil t)))))))
	 (not
	  (multiple-value-bind
	      (res win)
	      (ctypep obj (specifier-type (cadr hairy-spec)))
	    (if win
		(values (not res) t)
		(values nil nil))))
	 (satisfies
	  (let ((fun (second hairy-spec)))
	    (cond ((and (consp fun) (eq (car fun) 'lambda))
		   (values (not (null (funcall (coerce fun 'function) obj)))
			   t))
		  ((and (symbolp fun) (fboundp fun))
		   (values (not (null (funcall fun obj))) t))
		  (t
		   (values nil nil))))))))))


;;; EXTRACT-FUNCTION-TYPE  --  Interface
;;;
;;;    Pull the type specifier out of a function object.
;;;
(defun extract-function-type (fun)
  (if (eval:interpreted-function-p fun)
      (eval:interpreted-function-type fun)
      (typecase fun
	(byte-function (byte-function-type fun))
	(byte-closure (byte-function-type (byte-closure-function fun)))
	(t
	 (specifier-type (%function-type (%closure-function fun)))))))


;;; Ctype-Of  --  Interface
;;;
;;;    Like Type-Of, only returns a Type structure instead of a type
;;; specifier.  We try to return the type most useful for type checking, rather
;;; than trying to come up with the one that the user might find most
;;; informative.
;;;
(proclaim '(function ctype-of (t) ctype))
(defun-cached (ctype-of
	       :hash-function (lambda (x)
				(the fixnum
				     (logand (the fixnum (cache-hash-eq x))
					     #x1FF)))
	       :hash-bits 9
	       :init-form cold-load-init)
	      ((x eq))
  (typecase x
    (function
     (if (funcallable-instance-p x)
	 (class-of x)
	 (extract-function-type x)))
    (symbol
     (make-member-type :members (list x)))
    (number
     (let* ((num (if (complexp x) (realpart x) x))
	    (res (make-numeric-type
		  :class (etypecase num
			   (integer 'integer)
			   (rational 'rational)
			   (float 'float))
		  :format (if (floatp num)
			      (float-format-name num)
			      nil))))
       (cond ((complexp x)
	      (setf (numeric-type-complexp res) :complex)
	      (let ((imag (imagpart x)))
		(setf (numeric-type-low res) (min num imag))
		(setf (numeric-type-high res) (max num imag))))
	     (t
	      (setf (numeric-type-low res) num)
	      (setf (numeric-type-high res) num)))
       res))
    (array
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-type :dimensions (array-dimensions x)
			:complexp (not (typep x 'simple-array))
			:element-type etype
			:specialized-element-type etype)))
    (t
     (class-of x))))


;;; Clear this cache on GC so that we don't hold onto too much garbage.
;;;
(pushnew 'ctype-of-cache-clear *before-gc-hooks*)


;;;; Standard Deftypes.

(deftype bit () '(integer 0 1))

(deftype compiled-function () 'function)

(deftype atom () '(not cons))

(deftype extended-char ()
  "Type of characters that aren't base-char's.  None in CMU CL."
  'nil)

(deftype standard-char ()
  "Type corresponding to the charaters required by the standard."
  '(member #\NEWLINE #\SPACE #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\,
	   #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\=
	   #\> #\?  #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
	   #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\]
	   #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
	   #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{
	   #\| #\} #\~))

(deftype keyword ()
  "Type for any keyword symbol."
  '(satisfies keywordp))


;;;; Some types that we use in defining the standard functions:
;;; 

;;;
;;; A type specifier.
(deftype type-specifier () '(or list symbol class))
;;;
;;; An index into an array.   Also used for sequence index. 
(deftype index () `(integer 0 (,array-dimension-limit)))
;;;
;;; Array rank, total size...
(deftype array-rank () `(integer 0 (,array-rank-limit)))
(deftype array-total-size () `(integer 0 (,array-total-size-limit)))
;;;
;;; Some thing legal in an evaluated context.
(deftype form () t)
;;;
;;; Maclisp compatibility...
(deftype stringlike () '(or string symbol))
(deftype stringable () '(or string symbol character))
;;;
;;; Save a little typing...
(deftype truth () '(member t))
;;;
;;; A thing legal in places where we want the name of a file.
(deftype filename () '(or string pathname))
;;;
;;; A legal arg to pathname functions.
(deftype pathnamelike () '(or string pathname stream))
;;;
;;; A thing returned by the irrational functions.  We assume that they never
;;; compute a rational result.
(deftype irrational () '(or float (complex float)))
;;;
;;; Character components:
(deftype char-code () `(integer 0 (,char-code-limit)))
;;;
;;; A consed sequence result.  If a vector, is a simple array.
(deftype consed-sequence () '(or list (simple-array * (*))))
;;;
;;; The :end arg to a sequence...
(deftype sequence-end () '(or null index))
;;;
;;; A valid argument to a stream function...
(deftype streamlike () '(or stream (member nil t)))
;;;
;;; A thing that can be passed to funcall & friends.
(deftype callable () '(or function symbol))

;;; Until we decide if and how to wedge this into the type system, make it
;;; equivalent to t.
;;;
(deftype void () t)


;;;; Cold loading initializations.

(emit-cold-load-defuns "TYPE")
