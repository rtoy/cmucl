;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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

(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/macros.lisp,v 1.7.2.2 2000/05/23 16:38:57 pw Exp $")
;;;
;;; Macros global variable definitions, and other random support stuff used
;;; by the rest of the system.
;;;
;;; For simplicity (not having to use eval-when a lot), this file must be
;;; loaded before it can be compiled.
;;;

(in-package :pcl)

(proclaim '(declaration
	    values ;;I use this so that Zwei can remind
	           ;;me what values a function returns.
	     
	    arglist ;;Tells me what the pretty arglist
	            ;;of something (which probably takes
		    ;;&rest args) is.

	    indentation     ;;Tells ZWEI how to indent things
			    ;;like defclass.
	     class
	     variable-rebinding
	     pcl-fast-call
	     method-name
	     method-lambda-list
	     ))

;;; Age old functions which CommonLisp cleaned-up away.  They probably exist
;;; in other packages in all CommonLisp implementations, but I will leave it
;;; to the compiler to optimize into calls to them.
;;;
;;; Common Lisp BUG:
;;;    Some Common Lisps define these in the Lisp package which causes
;;;    all sorts of lossage.  Common Lisp should explictly specify which
;;;    symbols appear in the Lisp package.
;;;
(eval-when (compile load eval)

(defmacro memq (item list) `(member ,item ,list :test #'eq))
(defmacro assq (item list) `(assoc ,item ,list :test #'eq))
(defmacro rassq (item list) `(rassoc ,item ,list :test #'eq))
(defmacro delq (item list) `(delete ,item ,list :test #'eq))
(defmacro posq (item list) `(position ,item ,list :test #'eq))
(defmacro neq (x y) `(not (eq ,x ,y)))


(defun make-caxr (n form)
  (if (< n 4)
      `(,(nth n '(car cadr caddr cadddr)) ,form)
      (make-caxr (- n 4) `(cddddr ,form))))

(defun make-cdxr (n form)
  (cond ((zerop n) form)
	((< n 5) `(,(nth n '(identity cdr cddr cdddr cddddr)) ,form))
	(t (make-cdxr (- n 4) `(cddddr ,form)))))
)

(defun true (&rest ignore) (declare (ignore ignore)) t)
(defun false (&rest ignore) (declare (ignore ignore)) nil)
(defun zero (&rest ignore) (declare (ignore ignore)) 0)

(defun make-plist (keys vals)
  (if (null vals)
      ()
      (list* (car keys)
	     (car vals)
	     (make-plist (cdr keys) (cdr vals)))))

(defun remtail (list tail)
  (if (eq list tail) () (cons (car list) (remtail (cdr list) tail))))

;;; ONCE-ONLY does the same thing as it does in zetalisp.  I should have just
;;; lifted it from there but I am honest.  Not only that but this one is
;;; written in Common Lisp.  I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
	      (let ,(mapcar #'list vars (reverse expand-time-val-forms))
		,@body)))
       `(let ,(mapcar #'list (reverse ,run-time-vars)
			     (reverse ,run-time-vals))
	  ,wrapped-body))))

(eval-when (compile load eval)
(defun extract-declarations (body &optional environment)
  ;;(declare (values documentation declarations body))
  (let (documentation declarations form)
    (when (and (stringp (car body))
	       (cdr body))
      (setq documentation (pop body)))
    (block outer
      (loop
	(when (null body) (return-from outer nil))
	(setq form (car body))
	(when (block inner
		(loop (cond ((not (listp form))
			     (return-from outer nil))
			    ((eq (car form) 'declare)
			     (return-from inner 't))
			    (t
			     (multiple-value-bind (newform macrop)
				  (macroexpand-1 form environment)
			       (if (or (not (eq newform form)) macrop)
				   (setq form newform)
				 (return-from outer nil)))))))
	  (pop body)
	  (dolist (declaration (cdr form))
	    (push declaration declarations)))))
    (values documentation
	    (and declarations `((declare ,.(nreverse declarations))))
	    body)))
)

(defun get-declaration (name declarations &optional default)
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
	(return-from get-declaration (cdr form))))))


(defvar *keyword-package* (find-package 'keyword))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) *keyword-package*))

(eval-when (compile load eval)

(defun string-append (&rest strings)
  (setq strings (copy-list strings))		;The explorer can't even
						;rplaca an &rest arg?
  (do ((string-loc strings (cdr string-loc)))
      ((null string-loc)
       (apply #'concatenate 'string strings))
    (rplaca string-loc (string (car string-loc)))))
)

(defun symbol-append (sym1 sym2 &optional (package *package*))
  (intern (string-append sym1 sym2) package))

(defmacro check-member (place list &key (test #'eql) (pretty-name place))
  (once-only (place list)
    `(or (member ,place ,list :test ,test)
         (error "The value of ~A, ~S is not one of ~S."
                ',pretty-name ,place ,list))))

(defmacro alist-entry (alist key make-entry-fn)
  (once-only (alist key)
    `(or (assq ,key ,alist)
	 (progn (setf ,alist (cons (,make-entry-fn ,key) ,alist))
		(car ,alist)))))

;;; A simple version of destructuring-bind.

;;; This does no more error checking than CAR and CDR themselves do.  Some
;;; attempt is made to be smart about preserving intermediate values.  It
;;; could be better, although the only remaining case should be easy for
;;; the compiler to spot since it compiles to PUSH POP.
;;;
;;; Common Lisp BUG:
;;;    Common Lisp should have destructuring-bind.
;;;    
(defmacro destructuring-bind (pattern form &body body)
  (multiple-value-bind (ignore declares body)
      (extract-declarations body)
    (declare (ignore ignore))
    (multiple-value-bind (setqs binds)
	(destructure pattern form)
      `(let ,binds
	 ,@declares
	 ,@setqs
	 (progn .destructure-form.)
	 . ,body))))

(eval-when (compile load eval)
(defun destructure (pattern form)
  ;;(declare (values setqs binds))
  (let ((*destructure-vars* ())
	(setqs ()))
    (declare (special *destructure-vars*))
    (setq *destructure-vars* '(.destructure-form.)
	  setqs (list `(setq .destructure-form. ,form))
	  form '.destructure-form.)
    (values (nconc setqs (nreverse (destructure-internal pattern form)))
	    (delete nil *destructure-vars*))))

(defun destructure-internal (pattern form)
  ;; When we are called, pattern must be a list.  Form should be a symbol
  ;; which we are free to setq containing the value to be destructured.
  ;; Optimizations are performed for the last element of pattern cases.
  ;; we assume that the compiler is smart about gensyms which are bound
  ;; but only for a short period of time.
  (declare (special *destructure-vars*))
  (let ((gensym (gensym))
	(pending-pops 0)
	(var nil)
	(setqs ()))
    (labels
        ((make-pop (var form pop-into)
	   (prog1 
	     (cond ((zerop pending-pops)
		    `(progn ,(and var `(setq ,var (car ,form)))
			    ,(and pop-into `(setq ,pop-into (cdr ,form)))))
		   ((null pop-into)
		    (and var `(setq ,var ,(make-caxr pending-pops form))))
		   (t
		    `(progn (setq ,pop-into ,(make-cdxr pending-pops form))
			    ,(and var `(setq ,var (pop ,pop-into))))))
	     (setq pending-pops 0))))
      (do ((pat pattern (cdr pat)))
	  ((null pat) ())
	(if (symbolp (setq var (car pat)))
	    (progn
	      (unless (memq var '(nil ignore))
			 (push var *destructure-vars*))	      
	      (cond ((null (cdr pat))
		     (push (make-pop var form ()) setqs))
		    ((symbolp (cdr pat))
		     (push (make-pop var form (cdr pat)) setqs)
		     (push (cdr pat) *destructure-vars*)
		     (return ()))
		    ((memq var '(nil ignore)) (incf pending-pops))
		    ((memq (cadr pat) '(nil ignore))
		     (push (make-pop var form ()) setqs)
		     (incf pending-pops 1))
		    (t
		     (push (make-pop var form form) setqs))))
	    (progn
	      (push `(let ((,gensym ()))
		       ,(make-pop gensym
				  form
				  (if (symbolp (cdr pat)) (cdr pat) form))
		       ,@(nreverse
			   (destructure-internal (car pat) gensym)))
		    setqs)
	      (when (symbolp (cdr pat))
		(push (cdr pat) *destructure-vars*)
		(return)))))
      setqs)))
)


(defmacro collecting-once (&key initial-value)
   `(let* ((head ,initial-value)
           (tail ,(and initial-value `(last head))))
          (values #'(lambda (value)
                           (if (null head)
                               (setq head (setq tail (list value)))
			       (unless (memq value head)
				 (setq tail
				       (cdr (rplacd tail (list value)))))))
		  #'(lambda nil head))))

(defmacro doplist ((key val) plist &body body &environment env)
  (multiple-value-bind (doc decls bod)
      (extract-declarations body env)
    (declare (ignore doc))
    `(let ((.plist-tail. ,plist) ,key ,val)
       ,@decls
       (loop (when (null .plist-tail.) (return nil))
	     (setq ,key (pop .plist-tail.))
	     (when (null .plist-tail.)
	       (error "Malformed plist in doplist, odd number of elements."))
	     (setq ,val (pop .plist-tail.))
	     (progn ,@bod)))))

(defmacro if* (condition true &rest false)
  `(if ,condition ,true (progn ,@false)))

(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  `(let ((,var nil)
         (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
           (if (consp .dolist-carefully.)
               (progn
                 (setq ,var (pop .dolist-carefully.))
                 ,@body)
               (,improper-list-handler)))))

  ;;   
;;;;;; printing-random-thing
  ;;
;;; Similar to printing-random-object in the lisp machine but much simpler
;;; and machine independent.
(defmacro printing-random-thing ((thing stream) &body body)
  `(print-unreadable-object (,thing ,stream :identity t) ,@body))

(defun printing-random-thing-internal (thing stream)
  (declare (ignore thing stream))
  nil)

  ;;   
;;;;;; 
  ;;

(defun capitalize-words (string &optional (dashes-p t))
  (let ((string (copy-seq (string string))))
    (declare (string string))
    (do* ((flag t flag)
	  (length (length string) length)
	  (char nil char)
	  (i 0 (+ i 1)))
	 ((= i length) string)
      (setq char (elt string i))
      (cond ((both-case-p char)
	     (if flag
		 (and (setq flag (lower-case-p char))
		      (setf (elt string i) (char-upcase char)))
		 (and (not flag) (setf (elt string i) (char-downcase char))))
	     (setq flag nil))
	    ((char-equal char #\-)
	     (setq flag t)
	     (unless dashes-p (setf (elt string i) #\space)))
	    (t (setq flag nil))))))

;;;
;;; FIND-CLASS
;;;
;;; This is documented in the CLOS specification.
;;;
(defvar *find-class* (make-hash-table :test #'eq))

(defun function-returning-nil (x)
  (declare (ignore x))
  nil)

(defmacro find-class-cell-class (cell)
  `(car ,cell))

(defmacro find-class-cell-predicate (cell)
  `(cadr ,cell))

(defmacro find-class-cell-make-instance-function-keys (cell)
  `(cddr ,cell))

(defmacro make-find-class-cell (class-name)
  (declare (ignore class-name))
  '(list* nil #'function-returning-nil nil))

(defun find-class-cell (symbol &optional dont-create-p)
  (or (gethash symbol *find-class*)
      (unless dont-create-p
	(unless (legal-class-name-p symbol)
	  (error "~S is not a legal class name." symbol))
	(setf (gethash symbol *find-class*) (make-find-class-cell symbol)))))

(defvar *create-classes-from-internal-structure-definitions-p* t)

(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (find-class-cell-class cell)
      (and *create-classes-from-internal-structure-definitions-p*
           (structure-type-p symbol)
           (find-structure-class symbol))
      (cond ((null errorp) nil)
	    ((legal-class-name-p symbol)
	     (error "No class named: ~S." symbol))
	    (t
	     (error "~S is not a legal class name." symbol)))))

(defun find-class-predicate-from-cell (symbol cell &optional (errorp t))
  (unless (find-class-cell-class cell)
    (find-class-from-cell symbol cell errorp))
  (find-class-cell-predicate cell))

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

(defun find-class (symbol &optional (errorp t) environment)
  "Returns the PCL class metaobject named by SYMBOL. An error of type
   SIMPLE-ERROR is signaled if the class does not exist unless ERRORP
   is NIL in which case NIL is returned. SYMBOL cannot be a keyword."
  (declare (ignore environment))
  (find-class-from-cell
   symbol (find-class-cell symbol t) errorp))

(defun find-class-predicate (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-predicate-from-cell 
   symbol (find-class-cell symbol errorp) errorp))

(defvar *boot-state* nil) ; duplicate defvar to defs.lisp

; Use this definition in any CL implementation supporting 
; both define-compiler-macro and load-time-value.
; Note that in CMU, lisp:find-class /= pcl:find-class
(define-compiler-macro find-class (&whole form
				   symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol) 
	   (legal-class-name-p (eval symbol))
	   (constantp errorp)
	   (member *boot-state* '(braid complete)))
      (let ((symbol (eval symbol))
	    (errorp (not (null (eval errorp))))
	    (class-cell (make-symbol "CLASS-CELL")))	
	`(let ((,class-cell (load-time-value (find-class-cell ',symbol))))
	   (or (find-class-cell-class ,class-cell)
	       ,(if errorp
		    `(find-class-from-cell ',symbol ,class-cell t)
		    `(and (kernel:class-cell-class 
			   ',(kernel:find-class-cell symbol))
			  (find-class-from-cell ',symbol ,class-cell nil))))))
      form))

(defun (setf find-class) (new-value symbol)
  (if (legal-class-name-p symbol)
      (let ((cell (find-class-cell symbol)))
	(setf (find-class-cell-class cell) new-value)
	(when (or (eq *boot-state* 'complete)
		  (eq *boot-state* 'braid))
	  (when (and new-value (class-wrapper new-value))
	    (setf (find-class-cell-predicate cell)
		  (symbol-function (class-predicate-name new-value))))
	  (when (and new-value (not (forward-referenced-class-p new-value)))

	    (dolist (keys+aok (find-class-cell-make-instance-function-keys cell))
	      (update-initialize-info-internal
	       (initialize-info new-value (car keys+aok) nil (cdr keys+aok))
	       'make-instance-function))))
	new-value)
      (error "~S is not a legal class name." symbol)))

(defun (setf find-class-predicate) (new-value symbol)
  (if (legal-class-name-p symbol)
      (setf (find-class-cell-predicate (find-class-cell symbol)) new-value)
      (error "~S is not a legal class name." symbol)))

(defmacro gathering1 (gatherer &body body)
  `(gathering ((.gathering1. ,gatherer))
     (macrolet ((gather1 (x) `(gather ,x .gathering1.)))
       ,@body)))


;;;
;;; These are augmented definitions of list-elements and list-tails from
;;; iterate.lisp.  These versions provide the extra :by keyword which can
;;; be used to specify the step function through the list.
;;;
(defmacro *list-elements (list &key (by #'cdr))
  `(let ((tail ,list))
     #'(lambda (finish)
	 (if (endp tail)
	     (funcall finish)
	     (prog1 (car tail)
	            (setq tail (funcall ,by tail)))))))

(defmacro *list-tails (list &key (by #'cdr))
   `(let ((tail ,list))
      #'(lambda (finish)
          (prog1 (if (endp tail)
		     (funcall finish)
		     tail)
	         (setq tail (funcall ,by tail))))))

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))


(defsetf slot-value set-slot-value)

(defvar *redefined-functions* nil)

(defmacro original-definition (name)
  `(get ,name ':definition-before-pcl))

(defun redefine-function (name new)
  (pushnew name *redefined-functions*)
  (unless (original-definition name)
    (setf (original-definition name)
	  (symbol-function name)))
  (setf (symbol-function name)
	(symbol-function new)))

