;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file implements the constraint propagation phase of the compiler,
;;; which uses global flow analysis to obtain dynamic type information.
;;; 
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(defstruct (constraint
	    (:include sset-element)
	    (:constructor make-constraint (number kind x y not-p)))
  ;;
  ;; The kind of constraint we have:
  ;;     
  ;; TYPEP
  ;;     X is a LAMBDA-VAR and Y is a CTYPE.  The value of X is constrained to
  ;;     be of type Y.
  ;;
  ;; >, <, =, EQL, EQ
  ;;     X is a LAMBDA-VAR Y is a LAMBDA-VAR or a CONSTANT.  The relation is
  ;;     asserted to hold.
  ;;
  (kind nil :type (member typep < > = eql eq))
  ;;
  ;; The operands to the relation.
  (x nil :type lambda-var)
  (y nil :type (or ctype lambda-var constant))
  ;;
  ;; If true, negates the sense of the constaint.  The relation is does *not*
  ;; hold.
  (not-p nil :type boolean))


(defvar *constraint-number*)

;;; FIND-CONSTRAINT  --  Interface
;;;
;;;    Return a constraint for the specified arguments.  We only create a new
;;; constraint if there isn't already an equivalent old one, guaranteeing that
;;; all equivalent constraints are EQ.  This shouldn't be called on lambda-vars
;;; with no CONSTRAINTS set.
;;;
(defun find-constraint (kind x y not-p)
  (declare (type lambda-var x) (type (or constant lambda-var ctype) y)
	   (type boolean not-p))
  (or (etypecase y
	(ctype
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (type= (constraint-y con) y))
	     (return con))))
	(constant
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (eq (constraint-y con) y))
	     (return con))))
	(lambda-var 
	 (do-elements (con (lambda-var-constraints x) nil)
	   (when (and (eq (constraint-kind con) kind)
		      (eq (constraint-not-p con) not-p)
		      (let ((cx (constraint-x con)))
			(eq (if (eq cx x)
				(constraint-y con)
				cx)
			    y)))
	     (return con)))))
      (let ((new (make-constraint (incf *constraint-number*) kind x y not-p)))
	(sset-adjoin new (lambda-var-constraints x))
	(when (lambda-var-p y)
	  (sset-adjoin new (lambda-var-constraints y)))
	new)))


;;; FIND-BLOCK-TYPE-CONSTRAINTS  --  Internal
;;;
;;;    Compute the initial flow analysis sets for Block:
;;; -- For any lambda-var ref with a type check, add that constraint.
;;; -- For any lambda-var set, delete all constraints on that var, and add
;;;    those constraints to the set nuked by this block.
;;;    
(defun find-block-type-constraints (block)
  (let ((gen (make-sset))
	(kill (make-sset)))
	
    (do-nodes (node cont block)
      (typecase node
	(ref
	 (when (continuation-type-check cont)
	   (let ((leaf (ref-leaf node)))
	     (when (and (lambda-var-p leaf)
			(lambda-var-constraints leaf))
	       (let* ((atype (continuation-derived-type cont))
		      (con (find-constraint 'typep leaf atype nil)))
		 (sset-adjoin con gen))))))
	(cset
	 (let ((var (set-var node)))
	   (when (lambda-var-p var)
	     (let ((cons (lambda-var-constraints var)))
	       (when cons
		 (sset-difference gen cons)
		 (sset-union kill cons))))))))

    (setf (block-gen block) gen)
    (setf (block-kill block) kill)
    (setf (block-out block) (copy-sset gen))
    (setf (block-type-asserted block) nil)
    (undefined-value)))


;;; GET-CONSTRAINTS-TYPE  --  Internal
;;;
;;;    Given the set of Constraints for a variable and the current set of
;;; restrictions from flow analysis In, return the best approximation of what
;;; the type of a reference would be.
;;;
(defun get-constraints-type (constraints in)
  (let ((var-cons (copy-sset constraints)))
    (sset-intersection var-cons in)
    (let ((res *universal-type*))
      (do-elements (con var-cons)
	(when (eq (constraint-kind con) 'typep)
	  (if (constraint-not-p con)
	      (let ((diff (type-difference res (constraint-y con))))
		(when diff
		  (setf res diff)))
	      (setq res (type-intersection res (constraint-y con))))))
      res)))

		 
;;; USE-RESULT-CONSTRAINTS  --  Internal
;;;
;;;    Deliver the results of constraint propagation to REFs in Block.  During
;;; this pass, we also do local constraint propagation by adding in constraints
;;; as we seem them during the pass through the block.
;;;
(defun use-result-constraints (block)
  (declare (type cblock block))
  (let ((in (block-in block)))
    (do-nodes (node cont block)
      (typecase node
	(ref
	 (let ((var (ref-leaf node)))
	   (when (lambda-var-p var)
	     (let ((con (lambda-var-constraints var)))
	       (when con
		 (derive-node-type node (get-constraints-type con in))
		 (when (continuation-type-check cont)
		   (sset-adjoin
		    (find-constraint 'typep var
				     (continuation-asserted-type cont)
				     nil)
		    in)))))))
	(cset
	 (let ((var (set-var node)))
	   (when (lambda-var-p var)
	     (let ((cons (lambda-var-constraints var)))
	       (when cons
		 (sset-difference in cons))))))))))


;;; CLOSURE-VAR-P  --  Internal
;;;
;;;    Return true if Var would have to be closed over if environment analysis
;;; ran now (i.e. if there are any uses that have a different home lambda than
;;; the var's home.)
;;;
(defun closure-var-p (var)
  (declare (type lambda-var var))
  (let ((home (lambda-home (lambda-var-home var))))
    (flet ((frob (l)
	     (dolist (node l nil)
	       (unless (eq (lambda-home (block-lambda (node-block node))) home)
		 (return t)))))
      (or (frob (leaf-refs var))
	  (frob (basic-var-sets var))))))


;;; INIT-VAR-CONSTRAINTS  --  Internal
;;;
;;;    Give an empty constraints set to any var that doesn't have one and isn't
;;; a set closure var.  Since a var that we previously rejected looks identical
;;; to one that is new, so we optimistically keep hoping that vars stop being
;;; closed over or lose their sets.
;;;
(defun init-var-constraints (component)
  (declare (type component component))
  (dolist (fun (component-lambdas component))
    (flet ((frob (x)
	     (dolist (var (lambda-vars x))
	       (unless (lambda-var-constraints var)
		 (when (or (null (lambda-var-sets var))
			   (not (closure-var-p var)))
		   (setf (lambda-var-constraints var) (make-sset)))))))
      (frob fun)
      (dolist (let (lambda-lets fun))
	(frob let)))))


;;; FLOW-PROPAGATE-CONSTRAINTS  --  Internal
;;;
(defun flow-propagate-constraints (block)
  (let* ((pred (block-pred block))
	 (in (copy-sset (block-out (first pred)))))
    (dolist (b (rest pred))
      (sset-intersection in (block-out b)))
    (setf (block-in block) in)
    (sset-union-of-difference (block-out block) in (block-kill block))))


;;; CONSTRAINT-PROPAGATE  --  Interface
;;;
(defun constraint-propagate (component)
  (declare (type component component))
  (init-var-constraints component)
  (do-blocks (block component)
    (when (block-type-asserted block)
      (find-block-type-constraints block)))
  (setf (block-out (component-head component)) (make-sset))

  (let ((did-something nil))
    (loop
      (do-blocks (block component)
	(when (flow-propagate-constraints block)
	  (setq did-something t)))

      (unless did-something (return))
      (setq did-something nil)))

  (do-blocks (block component)
    (use-result-constraints block))

  (undefined-value))
