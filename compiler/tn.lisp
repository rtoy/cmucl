;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains utilities used for creating and manipulating TNs, and
;;; some other more assorted IR2 utilities.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

;;; The component that is currently being compiled.  TNs are allocated in this
;;; component.
;;;
(defvar *compile-component*)


;;; Do-Packed-TNs  --  Interface
;;;
(defmacro do-packed-tns ((tn component &optional result) &body body)
  "Do-Packed-TNs (TN-Var Component [Result]) Declaration* Form*
  Iterate over all packed TNs allocated in Component."
  (let ((n-component (gensym)))
    `(let ((,n-component (component-info ,component)))
       (do ((,tn (ir2-component-normal-tns ,n-component) (tn-next ,tn)))
	   ((null ,tn))
	 ,@body)
       (do ((,tn (ir2-component-restricted-tns ,n-component) (tn-next ,tn)))
	   ((null ,tn))
	 ,@body)
       (do ((,tn (ir2-component-wired-tns ,n-component) (tn-next ,tn)))
	   ((null ,tn)
	    ,result)
	 ,@body))))


;;; Delete-Unreferenced-TNs  --  Interface
;;;
;;;    Remove all TNs with no references from the lists of unpacked TNs.  We
;;; null out the Offset so that nobody will mistake deleted wired TNs for
;;; properly packed TNs.
;;;
(defun delete-unreferenced-tns (component)
  (macrolet ((frob (name)
	       `(let ((prev nil))
		  (do ((tn ,name (tn-next tn)))
		      ((null tn))
		    (cond ((or (tn-reads tn) (tn-writes tn))
			   (setq prev tn))
			  (t
			   (if prev
			       (setf (tn-next prev) (tn-next tn))
			       (setf ,name (tn-next tn)))
			   (setf (tn-offset tn) nil)))))))
    (let ((2comp (component-info component)))
      (frob (ir2-component-normal-tns 2comp))
      (frob (ir2-component-restricted-tns 2comp))
      (frob (ir2-component-wired-tns 2comp))))
  (undefined-value))



;;;; TN Creation:

;;; Make-Normal-TN  --  Interface
;;;
;;;    Create a packed TN of the specified primitive-type in the
;;; *Compile-Component*.  We use the SCs from the primitive type to determine
;;; which SCs it can be packed in.
;;;
(defun make-normal-tn (type)
  (declare (type primitive-type type))
  (let* ((component (component-info *compile-component*))
	 (res (make-tn (incf (ir2-component-global-tn-counter component))
		       :normal type nil))
	 (costs (tn-costs res)))
    (dolist (scn (primitive-type-scs type))
      (setf (svref costs scn) 0))
    (push-in tn-next res (ir2-component-normal-tns component))
    res))


;;; Make-Environment-TN  --  Interface
;;;
;;;    Like Make-Normal-TN, but give it a :Environment kind and note it in the
;;; specified Environment.
;;;
(defun make-environment-tn (type env)
  (declare (type primitive-type type) (type environment env))
  (let ((res (make-normal-tn type)))
    (setf (tn-kind res) :environment)
    (push res (ir2-environment-live-tns (environment-info env)))
    res))


;;; Make-Restricted-TN  --  Interface
;;;
;;;    Create a packed TN restricted to some subset of the SCs normally allowed
;;; by Type.  SCs is a list of the legal SC numbers.
;;;
(defun make-restricted-tn (type scs)
  (declare (type primitive-type type) (type list scs))
  (let* ((component (component-info *compile-component*))
	 (res (make-tn (incf (ir2-component-global-tn-counter component))
		       :normal type nil))
	 (costs (tn-costs res)))
    (dolist (scn scs)
      (setf (svref costs scn) 0))
    (push-in tn-next res (ir2-component-restricted-tns component))
    res))


;;; Make-Wired-TN  --  Interface
;;;
;;;    Create a TN wired to a particular location in an SC.  We set the Offset
;;; and FSC to record where it goes, and then put it on the current component's
;;; Wired-TNs list.  Type is used to determine the move/coerce operations.
;;;
(defun make-wired-tn (type scn offset)
  (declare (type primitive-type type) (type sc-number scn)
	   (type unsigned-byte offset))
  (let* ((component (component-info *compile-component*))
	 (res (make-tn (incf (ir2-component-global-tn-counter component))
		       :normal type (svref *sc-numbers* scn))))
    (setf (tn-offset res) offset)
    (push-in tn-next res (ir2-component-wired-tns component))
    res))


;;; Make-Wired-Environment-TN  --  Interface
;;;
;;;    Like Make-Wired-TN, but give it a :Environment kind and note it in the
;;; specified Environment.
;;;
(defun make-wired-environment-tn (type scn offset env)
  (declare (type primitive-type type) (type sc-number scn)
	   (type unsigned-byte offset) (type environment env))
  (let ((res (make-wired-tn type scn offset)))
    (setf (tn-kind res) :environment)
    (push res (ir2-environment-live-tns (environment-info env)))
    res))


;;; Make-Constant-TN  --  Interface
;;;
;;;    Create a constant TN.  The implementation dependent
;;; Immediate-Constant-SC function is used to determine whether the constant
;;; has an immediate representation.
;;;
(defun make-constant-tn (constant)
  (declare (type constant constant))
  (let* ((component (component-info *compile-component*))
	 (immed (immediate-constant-sc (constant-value constant)))
	 (sc (svref *sc-numbers* (or immed (sc-number-or-lose 'constant))))
	 (res (make-tn 0 :constant (primitive-type (leaf-type constant)) sc)))
    (unless immed
      (let ((constants (ir2-component-constants component)))
	(setf (tn-offset res) (fill-pointer constants))
	(vector-push-extend constant constants)))
    (push-in tn-next res (ir2-component-constant-tns component))
    (setf (tn-leaf res) constant)
    res))


;;; Make-Load-Time-Constant-TN  --  Internal
;;;
;;;    Return a load-time constant TN with the specified Kind and Info.  If the
;;; desired Constants entry already exists, then reuse it, otherwise allocate a
;;; new load-time constant slot.
;;;
(defun make-load-time-constant-tn (kind info)
  (declare (type keyword kind))
  (let* ((component (component-info *compile-component*))
	 (res (make-tn 0 :constant *any-primitive-type*
		       (svref *sc-numbers* (sc-number-or-lose 'constant))))
	 (constants (ir2-component-constants component)))

    (do ((i 0 (1+ i)))
	((= i (length constants))
	 (setf (tn-offset res) i)
	 (vector-push-extend (cons kind info) constants))
      (let ((entry (aref constants i)))
	(when (and (consp entry)
		   (eq (car entry) kind)
		   (eq (cdr entry) info))
	  (setf (tn-offset res) i))))

    (push-in tn-next res (ir2-component-constant-tns component))
    res))  


;;;; TN referencing:

;;; Reference-TN  --  Interface
;;;
;;;    Make a TN-Ref that references TN and return it.  Write-P should be true
;;; if this is a write reference, otherwise false.  All we do other than
;;; calling the constructor is add the reference to the TN's references.
;;;
(proclaim '(inline reference-tn))
(defun reference-tn (tn write-p)
  (declare (type tn tn) (type boolean write-p))
  (let ((res (make-tn-ref tn write-p)))
    (if write-p
	(push-in tn-ref-next res (tn-writes tn))
	(push-in tn-ref-next res (tn-reads tn)))
    res))


;;; Reference-TN-List  --  Interface
;;;
;;;    Make TN-Refs to reference each TN in TNs, linked together by
;;; TN-Ref-Across.  Write-P is the Write-P value for the refs.  More is 
;;; stuck in the TN-Ref-Across of the ref for the last TN, or returned as the
;;; result if there are no TNs.
;;;
(defun reference-tn-list (tns write-p &optional more)
  (declare (list tns) (type boolean write-p) (type (or tn-ref null) more))
  (if tns
      (let* ((first (reference-tn (first tns) write-p))
	     (prev first))
	(dolist (tn (rest tns))
	  (let ((res (reference-tn tn write-p)))
	    (setf (tn-ref-across prev) res)
	    (setq prev res)))
	(setf (tn-ref-across prev) more)
	first)
      more))


;;; Delete-TN-Ref  --  Interface
;;;
;;;    Remove Ref from the references for its associated TN.
;;;
(defun delete-tn-ref (ref)
  (declare (type tn-ref ref))
  (if (tn-ref-write-p ref)
      (deletef-in tn-ref-next (tn-writes (tn-ref-tn ref)) ref)
      (deletef-in tn-ref-next (tn-reads (tn-ref-tn ref)) ref))
  (undefined-value))


;;; Change-TN-Ref-TN  --  Interface
;;;
;;;    Do stuff to change the TN referenced by Ref.  We remove Ref from it's
;;; old TN's refs, add ref to TN's refs, and set the TN-Ref-TN.
;;;
(defun change-tn-ref-tn (ref tn)
  (declare (type tn-ref ref) (type tn tn))
  (delete-tn-ref ref)
  (setf (tn-ref-tn ref) tn)
  (if (tn-ref-write-p ref)
      (push-in tn-ref-next ref (tn-writes tn))
      (push-in tn-ref-next ref (tn-reads tn)))
  (undefined-value))


;;;; Random utilities:

;;; Make-N-TNs  --  Interface
;;;
;;;    Return a list of N normal TNs of the specified primitive type.
;;;
(defun make-n-tns (n ptype)
  (declare (type unsigned-byte n) (type primitive-type ptype))
  (collect ((res))
    (dotimes (i n)
      (res (make-normal-tn ptype)))
    (res)))


;;; Location=  --  Interface
;;;
;;;    Return true if X and Y are packed in the same location, false otherwise.
;;; This is false if either operand is constant.
;;;
(defun location= (x y)
  (declare (type tn x y))
  (and (eq (sc-sb (tn-sc x)) (sc-sb (tn-sc y)))
       (eql (tn-offset x) (tn-offset y))
       (not (or (eq (tn-kind x) :constant)
		(eq (tn-kind y) :constant)))))


;;; TN-Value  --  Interface
;;;
;;;    Return the value of an immediate constant TN.
;;;
(defun tn-value (tn)
  (declare (type tn tn))
  (assert (member (tn-kind tn) '(:constant :cached-constant)))
  (assert (/= (sc-number (tn-sc tn)) (sc-number-or-lose 'constant)))
  (constant-value (tn-leaf tn)))


;;; Force-TN-To-Stack  --  Interface
;;;
;;;    Force TN not to be allocated in a register by clearing the cost for each
;;; SC that has a Save-SC.
;;;
(defun force-tn-to-stack (tn)
  (declare (type tn tn))
  (let ((costs (tn-costs tn)))
    (dotimes (i sc-number-limit)
      (when (svref *save-scs* i)
	(setf (svref costs i) nil))))
  (undefined-value))


;;; TN-Environment  --  Interface
;;;
;;;    Return some IR2-Environment that TN is referenced in.  TN must have at
;;; least one reference (either read or write.)  Note that some TNs are
;;; referenced in multiple environments.
;;;
(defun tn-environment (tn)
  (declare (type tn tn))
  (let ((ref (or (tn-reads tn) (tn-writes tn))))
    (assert ref)
    (environment-info
     (lambda-environment
      (block-lambda
       (ir2-block-block (vop-block (tn-ref-vop ref))))))))
