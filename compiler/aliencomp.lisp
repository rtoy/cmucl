;;; -*- Log: C.Log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains transforms and other stuff used to compile Alien
;;; definitions.  There are two main parts: A fairly general numeric
;;; constant-folding facility, and the transforms that implement the
;;; the Alien primitives.
;;;
;;;
(in-package 'c)

(in-package 'lisp)
(import '(
	  ct-a-val-sap ct-a-val-type ct-a-val-offset ct-a-val-size
	  ct-a-val-p ct-a-val make-ct-a-val ct-a-val-alien
	  check<= check= %alien-indirect %aligned-sap
	  naturalize-integer deport-integer naturalize-boolean deport-boolean
	  sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-sap
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32
	  %set-sap-ref-8 %set-sap-ref-16 %set-sap-ref-32 %set-sap-ref-sap
	  sap+ %set-alien-access %set-sap-ref-single %set-sap-ref-double
	  %set-sap-ref-descriptor
	  
	  *alien-eval-when* make-alien alien-type alien-size alien-address
	  copy-alien dispose-alien defalien alien-value
	  alien-bind defoperator alien-index alien-indirect
	  bits bytes words long-words port perq-string
	  boolean defenumeration enumeration
	  system-area-pointer pointer alien alien-access
	  alien-assign alien-sap define-alien-stack
	  with-stack-alien null-terminated-string c-procedure
	  record-size
	  )
	(find-package "C"))

(in-package 'c)


;;;; Pre-pass:
;;;
;;;    We do a pre-pass over Alien expressions at source-transformation time.
;;; This pass does compile-time Alien type checking, and also composes the
;;; Alien operators, producing separate expressions that compute the address,
;;; type and total offset and size of the expression.

 
;;; Analyze-Alien-Expression  --  Internal
;;;
;;;    This function grovels an alien expression and return a bunch of values
;;; that tell how to evaluate the thing.  Result-Type is the alien-type that
;;; the expression must evaluate to.  The values returned are:
;;;  0] A list of let*-bindings that need to be made.
;;;  1] Some stuff that should be evaluated within the bindings.  This
;;;      stuff is error-checks and stuff like that.  This is in reverse
;;;      order.
;;;  2] A ct-a-val structure describing the size, sap and offset.
;;;
;;; If the expression is not of the correct type or is somehow malformed, then
;;; we call Compiler-Error, which aborts out.  If the expression cannot be
;;; shown to be of the correct type then a runtime check is generated.  When
;;; Result-Type is Nil we don't pay any attention to the type.
;;;
(defun analyze-alien-expression (result-type exp)
  (if (atom exp)
      (if (and (symbolp exp)
	       (or (ct-a-val-p (lexenv-find exp variables))
		   (info variable alien-value exp)))
	  (grovel-alien-var result-type exp)
	  (grovel-random-alien result-type exp))
      (let* ((fun (car exp))
	     (info (info function alien-operator fun)))
	(cond
	 ((eq fun 'alien-value)
	  (grovel-alien-var result-type (cadr exp)))
	 ((eq fun 'alien-index)
	  (when result-type
	    (compiler-error "Alien-Index used where a type is required:~% ~S"
			    exp))
	  (grovel-alien-index exp))
	 ((eq fun 'alien-indirect)
	  (when result-type
	    (compiler-error "Alien-Indirect used where a type is required:~% ~S"
			    exp))
	  (grovel-alien-indirect exp))
	 (info
	  (grovel-alien-operator result-type exp info))
	 (t
	  (grovel-random-alien result-type exp))))))


;;; Ignore-Unreferenced-Vars  --  Internal
;;;
;;;    Return an Ignorable declaration for all of the vars in the let-binding
;;; list Binds.
;;;
(defun ignore-unreferenced-vars (binds)
  (declare (list binds))
  `(declare (ignorable ,@(mapcar #'car binds))))


;;; Grovel-Alien-Index  --  Internal
;;;
;;;    Analyze a call to Alien-Index, a la Analyze-Alien-Expression.  We
;;; throw a call to Check-Alien-Bounds into the Stuff to do the bounds
;;; checking.
;;;
(defun grovel-alien-index (exp)
  (unless (= (length exp) 4)
    (compiler-error "Wrong number of arguments to Alien-Index:~% ~S" exp))
  
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil (second exp))
    (let* ((offset (ct-a-val-offset res))
	   (noffset `(+ ,offset ,(third exp)))
	   (size (ct-a-val-size res))
	   (nsize (fourth exp)))
      (values
       binds
       `((check<= (+ ,noffset ,nsize) (+ ,offset ,size))
	 ,@stuff)
       (make-ct-a-val
	:sap (ct-a-val-sap res)
	:offset noffset
	:size nsize)))))


;;; Grovel-Alien-Indirect  --  Internal
;;;
;;;    Handle an Alien-Indirect.  We transform to a %Alien-Indirect that looks
;;; at the offset and size.  If the offset is long-word aligned and the size is
;;; 32, then we transform to a SAP-System-Ref, otherwise we give up and allow
;;; the error check to be done at run-time.
;;;
(defun grovel-alien-indirect (exp)
  (unless (= (length exp) 3)
    (compiler-error "Wrong number of arguments to Alien-Indirect:~% ~S" exp))

  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil (second exp))
    (values
     binds
     stuff
     (make-ct-a-val
      :sap `(%alien-indirect ,(ct-a-val-size res)
			     ,(ct-a-val-sap res)
			     ,(ct-a-val-offset res)
			     ',exp)
      :offset 0
      :size (third exp)))))

(defknown %alien-indirect (index t index t) t (flushable))

(deftransform %alien-indirect ((size sap offset exp))
  (unless (>= (find-alignment offset) 5)
    (give-up "Offset may not be long-word aligned:~% ~S"
	     (continuation-value exp)))

  (unless (and (constant-continuation-p size)
	       (eql (continuation-value size) 32))
    (give-up "Size may not be 32:~% ~S" (continuation-value exp)))

  '(sap-ref-sap sap (ash offset -5)))


;;; Grovel-Alien-Var  --  Internal
;;;
;;;    Look the var up in the LEXENV-VARIABLES, and then check if it is a
;;; global alien-variable.  If it ain't there or is the the wrong type then
;;; flame out.
;;;
(defun grovel-alien-var (type var)
  (let* ((local-def (lexenv-find var variables))
	 (res (cond ((ct-a-val-p local-def) local-def)
		    ((or (not local-def)
			 (and (global-var-p local-def)
			      (eq (global-var-kind local-def) :special)))
		     (info variable alien-value var))
		    (t
		     (compiler-error "Not an Alien variable: ~S." var)))))
    (unless res
      (compiler-error "Undefined Alien variable: ~S" var))
    (when (and type (not (equal (ct-a-val-type res) type)))
      (compiler-error "Alien type of ~S is ~S, not ~S."
		      var (ct-a-val-type res) type))

    (values () () res)))


;;; Grovel-Alien-Operator  --  Internal
;;;
;;;    Handle a call to an alien operator.  Analyze each Alien valued argument,
;;; throwing the result on *aenv* during the processing of the body.  We always
;;; bind lisp arguments, letting the optimizer do any cleverness.  All we do to
;;; the body is strip off any stuff if there is a progn, and then call
;;; Analyze-Alien-Expression on the body with Nil as the result-type.
;;;
(defun grovel-alien-operator (type exp info)
  (unless (or (not type)
	      (equal (lisp::alien-info-result-type info) type))
    (compiler-error "Results in a ~S alien value, not a ~S:~% ~S"
		    (lisp::alien-info-result-type info) type exp))
  
  (unless (= (lisp::alien-info-num-args info) (length (cdr exp)))
    (compiler-error "Alien operator not called with ~D args:~% ~S"
		    (lisp::alien-info-num-args info) exp))
  
  (do* ((args (cdr exp) (cdr args))
	(dums ())
	(*lexical-environment* *lexical-environment*)
	(num 0 (1+ num))
	(types (lisp::alien-info-arg-types info))
	(atype (cdr (assoc num types)) (cdr (assoc num types)))
	(stuff ())
	(binds ()))
       ((null args)
	(let ((body (apply (lisp::alien-info-function info)
			   (nreverse dums))))
	  (when (and (consp body)
		     (eq (car body) 'progn))
	    (push (butlast body) stuff)
	    (setq body (car (last body))))
	  (multiple-value-bind (b s r)
			       (analyze-alien-expression nil body)
	    (when r
	      (setq r (make-ct-a-val
		       :type (lisp::alien-info-result-type info)
		       :sap (ct-a-val-sap r)
		       :offset (ct-a-val-offset r)
		       :size (ct-a-val-size r))))
	    (values
	     (nconc b binds)
	     (nconc s stuff)
	     r))))
    (declare (list dums))
    (cond (atype
	   (multiple-value-bind (b s val)
				(analyze-alien-expression atype (car args))
	     (setq binds (nconc binds b)  stuff (nconc stuff s))
	     (unless val (return nil))
	     (let ((var (gensym)))
	       (setq *lexical-environment*
		     (make-lexenv :variables (list (cons var val))))
	       (push var dums))))
	  (t
	   (let ((var (gensym)))
	     (push var dums)
	     (setq binds (nconc binds `((,var ,(car args))))))))))


;;; Grovel-Random-Alien  --  Internal
;;;
;;;    Flame out if the form is some random atom, otherwise bind a var
;;; to Check-Alien-Type of it, and return forms to access the fields
;;; of the Alien-Value.
;;;
(defun grovel-random-alien (type exp)
  (when (and (atom exp) (not (symbolp exp)))
    (compiler-error "~S is a bad thing to be an Alien-Value." exp))
  (cond ((and (symbolp exp) (info variable alien-value exp))
	 (values () () (info variable alien-value exp)))
	(t
	 (when (policy nil (> speed brevity))
	   (compiler-note "Not a known Alien expression:~% ~S." exp))

	 (let ((var (gensym))
	       (sap (gensym))
	       (size (gensym))
	       (offset (gensym)))
	   (values
	    `((,sap (lisp::alien-value-sap ,var))
	      (,offset (lisp::alien-value-offset ,var))
	      (,size (lisp::alien-value-size ,var))
	      (,var ,(if type
			 `(lisp::check-alien-type ,exp ',type)
			 exp)))
	    ()
	    (make-ct-a-val
	     :type type
	     :sap sap
	     :offset offset
	     :size size
	     :alien var))))))


;;;; Miscellaneous internal frobs:

(defknown sap+ (system-area-pointer integer) system-area-pointer
  (movable flushable))
(deftransform sap+ ((sap offset))
  (unless (and (constant-continuation-p offset)
	       (eql (continuation-value offset) 0))
    (give-up))
  'sap)

(defknown sap- (system-area-pointer system-area-pointer) (signed-byte 32)
  (movable flushable))

(defknown sap-int (system-area-pointer) (unsigned-byte 32) (movable flushable))
(defknown int-sap ((unsigned-byte 32)) system-area-pointer (movable))

(defknown %aligned-sap (system-area-pointer unsigned-byte t) system-area-pointer
  (movable flushable))
(deftransform %aligned-sap ((sap offset form))
  (unless (> (find-alignment offset) 3)
    (give-up))
  '(sap+ sap (ash offset -3)))

(defknown (check<= check=) (index index) void (movable))

;;; Compile-Time-Check  --  Internal
;;;
;;;    If the operands are constant, then test them using OP.  If it succeeds,
;;; transform to NIL, otherwise warn and give-up.
;;;
(defun compile-time-check (x y op)
  (unless (and (constant-continuation-p x)
	       (constant-continuation-p y))
    (give-up))
  (let ((x (continuation-value x))
	(y (continuation-value y)))
    (unless (funcall (symbol-function op) x y)
      (compiler-warning "~S not ~S to ~S at compile time." x op y)
      (give-up)))
  'nil)

(deftransform check<= ((x y)) (compile-time-check x y '<=))
(deftransform check= ((x y)) (compile-time-check x y '=))

(defknown record-size (symbol) index (movable foldable flushable))

(defknown sap-ref-8 (system-area-pointer index) (unsigned-byte 8)
  (flushable))
(defknown sap-ref-16 (system-area-pointer index) (unsigned-byte 16)
  (flushable))
(defknown sap-ref-32 (system-area-pointer index) (unsigned-byte 32)
  (flushable))

(defknown signed-sap-ref-8 (system-area-pointer index) (signed-byte 8)
  (flushable))
(defknown signed-sap-ref-16 (system-area-pointer index) (signed-byte 16)
  (flushable))
(defknown signed-sap-ref-32 (system-area-pointer index) (signed-byte 32)
  (flushable))

(defknown %set-sap-ref-8
	  (system-area-pointer index (or (unsigned-byte 8) (signed-byte 8)))
  (or (unsigned-byte 8) (signed-byte 8)) ())
(defknown %set-sap-ref-16
	  (system-area-pointer index (or (unsigned-byte 16) (signed-byte 16)))
  (or (unsigned-byte 16) (signed-byte 16)) ())
(defknown %set-sap-ref-32
	  (system-area-pointer index (or (unsigned-byte 32) (signed-byte 32)))
  (or (unsigned-byte 32) (signed-byte 32)) ())

(defknown sap-ref-sap (system-area-pointer index) system-area-pointer
  (flushable))
(defknown %set-sap-ref-sap (system-area-pointer index system-area-pointer)
  system-area-pointer
  ())

(defknown sap-ref-single (system-area-pointer index) single-float
  (flushable))
(defknown sap-ref-double (system-area-pointer index) double-float
  (flushable))

(defknown %set-sap-ref-single
	  (system-area-pointer index single-float) single-float
  ())
(defknown %set-sap-ref-double
	  (system-area-pointer index double-float) double-float
  ())


;;;; Alien variable special forms:

;;; Alien-Bind IR1 convert  --  Internal
;;;
(def-ir1-translator alien-bind ((binds &body body &whole source) start cont)
  (if binds
      (let ((bind (first binds)))
	(unless (<= 2 (length bind) 4)
	  (compiler-error "Malformed Alien-Bind specifier:~% ~S" bind))
	(let ((var (first bind))
	      (val (second bind))
	      (typ (third bind))
	      (aligned (fourth bind)))
	  
	  (multiple-value-bind (init-lets stuff res)
			       (analyze-alien-expression typ val)
	    (unless (ct-a-val-type res)
	      (compiler-error "Must specify type, since it is not apparent ~
                     	       from the value:~% ~S"
			      bind))
	    (let* ((offset (ct-a-val-offset res))
		   (sap (ct-a-val-sap  res))
		   (size (ct-a-val-size res))
		   (n-size (gensym))
		   (n-sap (gensym))
		   (n-offset (gensym))
		   (a-val (make-ct-a-val :type (ct-a-val-type res)
					 :offset (if aligned 0 n-offset)
					 :size n-size
					 :sap n-sap
					 :alien (ct-a-val-alien res)))
		   (lets `(,@(reverse init-lets)
			   (,n-sap ,(if aligned
					`(%aligned-sap ,sap ,offset ',source)
					sap))
			   (,n-size ,size)
			   (,n-offset ,(if aligned 0 offset)))))
	      (ir1-convert
	       start cont
	       `(let* ,lets
		  ,(ignore-unreferenced-vars lets)
		  (compiler-let ((*lexical-environment*
				  (make-lexenv :variables
					       ',(acons var a-val nil))))
		    ,@(reverse stuff)
		    (alien-bind ,(rest binds)
		      ,@body))))))))
      (ir1-convert-progn-body start cont body)))


;;; With-Stack-Alien-Transform  --  Internal
;;;
;;;
(def-ir1-translator with-stack-alien (((var type &optional size)
				       &body forms) start cont)
  (unless size
    (let ((info (info alien-stack info type)))
      (unless info
	(compiler-error "~S is not the name of a declared alien stack." type))
      (setf type (lisp::stack-info-type info))
      (setf size (lisp::stack-info-size info))))
  (let* ((sap-var (gensym))
	 (*venv* (acons var
			(make-ct-a-val :type type
				       :size size
				       :sap sap-var
				       :offset 0)
			*venv*)))
    (ir1-convert start cont
      `(let ((,sap-var
	      (truly-the system-area-pointer
			 (%primitive alloc-number-stack-space
				     (ceiling ,size vm:byte-bits)))))
	 (declare (ignorable ,sap-var))
	 (multiple-value-prog1
	     (progn ,@forms)
	   (%primitive dealloc-number-stack-space
		       (ceiling ,size vm:byte-bits)))))))


;;;; CALL-FOREIGN-FUNCTION stuff.

(defknown ext::call-foreign-function (simple-base-string t t &rest t) t)

(defun alien-type-type (type)
  (let ((name (if (listp type) (car type) type)))
    (case name
      ((unsigned-byte signed-byte system-area-pointer
		      double-float single-float)
       (specifier-type type))
      (port
       (specifier-type '(unsigned-byte 32)))
      (t
       (error "Alien type ~S has no corresponding Lisp type." type)))))

(defoptimizer (ext::call-foreign-function derive-type)
	      ((name return-type arg-types &rest args))
  (assert (constant-continuation-p return-type))
  (assert (constant-continuation-p arg-types))
  (let ((return-type (continuation-value return-type))
	(arg-types (continuation-value arg-types)))
    (unless (= (length arg-types) (length args))
      (error "Different number of argument types (~D) from arguments (~D)."
	     (length arg-types)
	     (length args)))
    (dolist (type arg-types)
      (let ((arg (pop args)))
	(assert-continuation-type arg (alien-type-type type))))
    (if return-type
	(alien-type-type return-type)
	*universal-type*)))

(defun make-call-out-nsp-tn ()
  (make-wired-tn (primitive-type-or-lose 'positive-fixnum)
		 (sc-number-or-lose 'any-reg)
		 nsp-offset))

(defun make-call-out-argument-tns (arg-types)
  (let ((stack-frame-size 0)
	(did-int-arg nil)
	(float-args 0))
    (collect ((tns))
      (dolist (type arg-types)
	(let ((name (if (consp type) (car type) type)))
	  (ecase name
	    ((unsigned-byte port)
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
				     (sc-number-or-lose 'unsigned-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
				     (sc-number-or-lose 'unsigned-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (signed-byte
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
				     (sc-number-or-lose 'signed-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
				     (sc-number-or-lose 'signed-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (system-area-pointer
	     (if (< stack-frame-size 4)
		 (tns (make-wired-tn (primitive-type-or-lose
				      'system-area-pointer)
				     (sc-number-or-lose 'sap-reg)
				     (+ stack-frame-size 4)))
		 (tns (make-wired-tn (primitive-type-or-lose
				      'system-area-pointer)
				     (sc-number-or-lose 'sap-stack)
				     stack-frame-size)))
	     (incf stack-frame-size)
	     (setf did-int-arg t))
	    (double-float
	     ;; Round to a dual-word.
	     (setf stack-frame-size (logandc2 (1+ stack-frame-size) 1))
	     (cond ((>= stack-frame-size 4)
		    (tns (make-wired-tn (primitive-type-or-lose 'double-float)
					(sc-number-or-lose 'double-stack)
					stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (make-wired-tn (primitive-type-or-lose 'double-float)
					(sc-number-or-lose 'double-reg)
					(+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size 2)
	     (incf float-args))
	    (single-float
	     (cond ((>= stack-frame-size 4)
		    (tns (make-wired-tn (primitive-type-or-lose 'single-float)
					(sc-number-or-lose 'single-stack)
					stack-frame-size)))
		   ((and (not did-int-arg) (< float-args 2))
		    (tns (make-wired-tn (primitive-type-or-lose 'single-float)
				   (sc-number-or-lose 'single-reg)
				   (+ (* float-args 2) 12))))
		   (t
		    (error "Can't put floats in int regs yet.")))
	     (incf stack-frame-size)
	     (incf float-args)))))
      (values (tns)
	      (logandc2 (1+ stack-frame-size) 1)))))

(defun make-call-out-result-tn (type)
  (let ((name (if (consp type) (car type) type)))
    (ecase name
      ((unsigned-byte port)
       (make-wired-tn (primitive-type-or-lose 'unsigned-byte-32)
		      (sc-number-or-lose 'unsigned-reg)
		      2))
      (signed-byte
       (make-wired-tn (primitive-type-or-lose 'signed-byte-32)
		      (sc-number-or-lose 'signed-reg)
		      2))
      (system-area-pointer
       (make-wired-tn (primitive-type-or-lose 'system-area-pointer)
		      (sc-number-or-lose 'sap-reg)
		      2))
      (double-float
       (make-wired-tn (primitive-type-or-lose 'double-float)
		      (sc-number-or-lose 'double-reg)
		      0))
      (single-float
       (make-wired-tn (primitive-type-or-lose 'single-float)
		      (sc-number-or-lose 'single-reg)
		      0)))))

(defoptimizer (ext::call-foreign-function ltn-annotate)
	      ((name return-type arg-types &rest args) node policy)
  (setf (basic-combination-info node) :funny)
  (setf (node-tail-p node) nil)
  (dolist (arg args)
    (annotate-ordinary-continuation arg policy)))

(defoptimizer (ext::call-foreign-function ir2-convert)
	      ((name return-type arg-types &rest args) call block)
  (assert (constant-continuation-p name))
  (assert (constant-continuation-p return-type))
  (assert (constant-continuation-p arg-types))
  (let* ((name (continuation-value name))
	 (return-type (continuation-value return-type))
	 (arg-types (continuation-value arg-types))
	 (cont (node-cont call)))
    (multiple-value-bind (arg-tns stack-frame-size)
			 (make-call-out-argument-tns arg-types)
      (unless (zerop stack-frame-size)
	(let ((nsp (make-call-out-nsp-tn))
	      (args args))
	  (vop alloc-number-stack-space call block stack-frame-size nsp)
	  (dolist (tn arg-tns)
	    (let* ((arg (pop args))
		   (sc (tn-sc tn))
		   (scn (sc-number sc))
		   (temp-tn (make-representation-tn (tn-primitive-type tn)
						    scn)))
	      (assert arg)
	      (emit-move call block (continuation-tn call block arg) temp-tn)
	      (emit-move-arg-template call block
				      (svref (sc-move-arg-vops sc) scn)
				      temp-tn nsp tn)))
	  (assert (null args))))

      (let ((results (when return-type
		       (list (make-call-out-result-tn return-type)))))
	(vop* call-out call block
	      ((reference-tn-list arg-tns nil))
	      ((reference-tn-list results t))
	      name)
	(unless (zerop stack-frame-size)
	  (vop dealloc-number-stack-space call block stack-frame-size))
	(move-continuation-result call block results cont)))))


;;;; Transforms for basic Alien accessors.
;;;
;;;    Open-coding these guys is probably worthwhile, since they are used
;;; for real things.

;;; Alien-Address source transform  --  Internal
;;;
;;;    Divide out the out the offset, producing a ratio if the alien
;;; isn't byte aligned.
;;;
(def-source-transform alien-address (alien)
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil alien)
    `(let* ,(reverse binds)
       ,(ignore-unreferenced-vars binds)
       ,@(nreverse stuff)
       (+ (sap-int ,(ct-a-val-sap res))
	  (/ ,(ct-a-val-offset res) vm:byte-bits)))))


;;; Alien-SAP soruce transform  --  Internal
;;;
;;;
(def-source-transform alien-sap (alien &whole source)
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil alien)
    `(let* ,(reverse binds)
       ,(ignore-unreferenced-vars binds)
       ,@(nreverse stuff)
       (%aligned-sap ,(ct-a-val-sap res) ,(ct-a-val-offset res)
		     ',source))))


;;; Alien=>Lisp transform is defined in proclaim.lisp, since it is referenced
;;; by top-level code in cold load.
;;;
(dolist (x '(alien-index alien-indirect))
  (setf (info function source-transform x) #'alien=>lisp-transform))


;;; Alien-Value IR1 convert  --  Internal
;;;
;;;    Although all we do is call Alien=>Lisp-Transform, this must be a special
;;; form, since the transformation must be done even when functions wouldn't be
;;; transformed.
;;;
(def-ir1-translator alien-value ((x &whole form) start cont)
  x ; Ignore
  (ir1-convert start cont (alien=>lisp-transform form)))


;;;; Alien-Access:

;;; Alien-Access source transform  --  Internal
;;;
;;;    We analyze the alien expression, converting to a call to %Alien-Access
;;; with the alien parts as separate arguments.
;;;
(def-source-transform alien-access (alien &optional lisp-type &whole form)
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil alien)
    `(let* ,(reverse binds)
       ,(ignore-unreferenced-vars binds)
       ,@(nreverse stuff)
       (%alien-access ,(ct-a-val-sap res) ,(ct-a-val-offset res)
		      ,(ct-a-val-size res) ',(ct-a-val-type res)
		      ,lisp-type ',form))))

(defknown %alien-access
	  (system-area-pointer unsigned-byte unsigned-byte t t t) t
  (movable flushable))

;;; %Alien-Access transform  --  Internal
;;;
;;;    If we can figure out the alien and lisp types at compile time, then
;;; expand into the appropriate access form.
;;;
(deftransform %alien-access ((sap offset size type lisp-type form))
  (unless (constant-continuation-p lisp-type)
    (give-up "Lisp-Type not constant, so cannot open-code."))

  (let ((type (continuation-value type))
	(lisp-type (continuation-value lisp-type))
	(form (continuation-value form)))
    (unless type
      (give-up "Alien type unknown, so cannot open-code."))
  
    (let ((access (lisp::get-alien-access-method type lisp-type)))
      (funcall access 'sap 'offset 'size type :read nil form))))


;;; %Set-Alien-Access source transform  --  Internal
;;;
;;;    Like the source transform for Alien-Access, only different.
;;;
(def-source-transform %set-alien-access (alien lisp-type
					  &optional (new-value nil nv-p)
					  &whole form)
  (multiple-value-bind (binds stuff res)
		       (analyze-alien-expression nil alien)
    `(let* ,(reverse binds)
       ,(ignore-unreferenced-vars binds)
       ,@(nreverse stuff)
       (%%set-alien-access ,(ct-a-val-sap res) ,(ct-a-val-offset res)
			   ,(ct-a-val-size res) ',(ct-a-val-type res)
			   ,(if nv-p lisp-type nil)
			   ,(if nv-p new-value lisp-type)
			   ',form))))

(defknown %%set-alien-access
	  (system-area-pointer unsigned-byte unsigned-byte t t t t) t)

;;; %%Set-Alien-Access transform  --  Internal
;;;
;;;    Like %Alien-Access transform.  The alien-access experts don't return the
;;; right value, cause it's a pain in the ass.  Since this is a Setf method, we
;;; gotta return the right value.
;;;
(deftransform %%set-alien-access ((sap offset size type lisp-type new-value
				       form))
  (unless (constant-continuation-p lisp-type)
    (give-up "Lisp-Type not constant, so cannot open-code."))

  (let ((type (continuation-value type))
	(lisp-type (continuation-value lisp-type))
	(form (continuation-value form)))
    (unless type
      (give-up "Alien type unknown, so cannot open-code."))
  
    (let ((access (lisp::get-alien-access-method type lisp-type)))
      `(progn
	 ,(funcall access 'sap 'offset 'size type :write 'new-value
		   form)
	 new-value))))


;;;; Alignment determination:

;;; Integer-Alignment  --  Internal
;;;
;;;    Returns the largest power of two which evenly divides its argument.
;;; If N is not an integer 0 is returned. 
;;;
(defun integer-alignment (n)
  (if (integerp n)
      (if (zerop n)
	  most-positive-fixnum
	  (do ((i 0 (1+ i))
	       (n (abs n) (ash n -1)))
	      ((not (zerop (logand n 1))) i)))
      0))


;;; Find-Alignment  --  Internal
;;;
;;;    This function is used to find out if offsets are some multiple of a
;;; power of two.  The largest exponent of two which evenly the value of Cont
;;; is returned.  0 is returned if we can't figure out anything, or the result
;;; isn't an integer.  If the value is known to be 0, then we return
;;; most-positive-fixnum.
;;;
(defun find-alignment (cont)
  (declare (type continuation cont))
  (let ((use (continuation-use cont)))
    (cond ((and (combination-p use)
		(= (length (combination-args use)) 2))
	   (let* ((name (continuation-function-name (combination-fun use)))
		  (args (combination-args use))
		  (x (first args))
		  (y (second args)))
	     (case name
	       ((+ -)
		(min (find-alignment x) (find-alignment y)))
	       (*
		(let ((itype (specifier-type 'integer)))
		  (if (and (csubtypep (continuation-type x) itype)
			   (csubtypep (continuation-type y) itype))
		      (+ (find-alignment x) (find-alignment y))
		      0)))
	       (ash
		(if (constant-continuation-p y)
		    (let ((val (continuation-value y)))
		      (if (integerp val)
			  (+ (find-alignment x) val)
			  0))
		    0))
	       (t
		0))))
	  ((constant-continuation-p cont)
	   (integer-alignment (continuation-value cont)))
	  (t
	   0))))

;;; Find-Bit-Offset  --  Internal
;;;
;;;    Returns the value of (rem Exp From) if this can be determined at
;;; compile-time, or Nil otherwise.  From must be a power of two.
;;;
(defun find-bit-offset (exp &optional (from 16))
  (declare (type continuation exp) (type unsigned-byte from))
  (let ((use (continuation-use exp)))
    (cond ((constant-continuation-p exp)
	   (rem (continuation-value exp) from))
	  ((>= (find-alignment exp) (1- (integer-length from)))
	   0)
	  ((and (combination-p use)
		(= (length (combination-args use)) 2))
	   (let* ((name (continuation-function-name (combination-fun use)))
		  (args (combination-args use))
		  (x (find-bit-offset (first args) from))
		  (y (find-bit-offset (second args) from)))
	     (when (and x y)
	       (case name
		 (+ (rem (+ x y) from))
		 (t nil)))))
	  (t
	   nil))))


;;;; Reading and writing integers.
;;;
;;;    This code is used by many of the Alien-Access experts, since many
;;; operations reduce to reading and writing integers.

;;; Sign-Extend  --  Internal
;;;
;;;    Wrap code around Form to sign-extend a Size-bit integer if Signed,
;;; otherwise just return arg.
;;;
(defun sign-extend (form size &optional (signed t))
  (if signed
      `(let ((res ,form))
	 (if (zerop (logand res ,(ash 1 (1- size))))
	     res
	     (logior res ,(ash -1 size))))
      form))


(defknown naturalize-integer
	  (t system-area-pointer unsigned-byte unsigned-byte t) integer
  (flushable))

;;; Naturalize-Integer Transform  --  Internal
;;;
;;;    Compile a call to Naturalize-Integer in some tense fashion.
;;; The number may be signed or unsigned.  The integer may be any size and
;;; alignment, as long as it fits within a word, otherwise it must be
;;; exactly 16 or 32 bits and be word-aligned.  The size must be a
;;; compile-time constant and is assumed to have been checked for
;;; correctness.
;;;
;;; ### Note that the alignment check will fail if the SAP isn't 32 bit
;;; aligned.  Now that we don't generally squeeze offsets into the sap, this is
;;; probably usually true.  Eventually, we should guarantee that the SAP is
;;; 32 bit aligned.
;;; 
(deftransform naturalize-integer ((signed sap offset size form))
  (unless (constant-continuation-p size)
    (give-up "Size not constant, so cannot open-code integer access:~%~S"
	     (continuation-value form)))
  
  (let ((align (find-alignment offset))
	(size (continuation-value size))
	(signed (continuation-value signed))
	(bits (find-bit-offset offset)))
    (cond
     ((and (= size 32) (> align 4))
      (if signed
	  '(signed-sap-ref-32 sap (ash offset -5))
	  '(sap-ref-32 sap (ash offset -5))))
     ((and (= size 16) (> align 3))
      (if signed
	  '(signed-sap-ref-16 sap (ash offset -4))
	  '(sap-ref-16 sap (ash offset -4))))
     ((and (= size 8) (> align 2))
      (if signed
	  '(signed-sap-ref-8 sap (ash offset -3))
	  '(sap-ref-8 sap (ash offset -3))))
     ((not bits)
      (give-up "Can't determine offset within word, so cannot open-code:~%~S"
	       (continuation-value form)))
     ((<= (+ bits size) 32)
      (sign-extend `(ldb (byte ,size ,bits)
			 (sap-ref-32 sap (ash offset -5)))
		   size signed))
     ((> size 32)
      (give-up "Accesses of ~D bits unsupported:~%~S"
	       size (continuation-value form)))
     (t
      ;; If the integer spans a boundry, how we combine the two partial results
      ;; depends on the target byte order.
      ;;
      ;; little-endian:
      ;;   0   1   2   3   4   5   6   7
      ;; 7      07      07      07      0
      ;; [  ][  ][  ][  ][  ][  ][  ][  ]
      ;; 210                          543
      ;;  We get the low bits from the high part of the first word and
      ;; the high bits from the low part of the second word.
      ;; 
      ;; big-endian: 
      ;;   0   1   2   3   4   5   6   7
      ;; 7      07      07      07      0
      ;; [  ][  ][  ][  ][  ][  ][  ][  ]
      ;;              543210
      ;;  We get the low bits from the high part of the second word
      ;; and the high bits from the low part of the first.
      ;;
      ;; Therefore, the only difference is which byte to use for what.
      ;; What bits to use from each byte is constant.
      ;; 
      ;; We sign extend the high bits instead of the entire value because
      ;; it has a higher probability of being a fixnum.
      ;; 
      (let* ((high-bits (- 32 bits))
	     (low-bits (- size high-bits)))
	(multiple-value-bind (low-byte high-byte)
			     (ecase vm:target-byte-order
			       (:little-endian
				(values 'offset '(1+ offset)))
			       (:big-endian
				(values '(1+ offset) 'offset)))
	  `(let ((offset (ash offset -5)))
	     (logior (ash ,(sign-extend `(ldb (byte ,high-bits 0)
					      (sap-ref-32 sap ,high-byte))
					size signed)
			  ,low-bits)
		     (ash (sap-ref-32 sap ,low-byte)
			  ,(- (- 32 low-bits)))))))))))


(defknown deport-integer
	  (t system-area-pointer unsigned-byte unsigned-byte integer t) void
  ())

;;; Deport-Integer transform  --  Internal
;;;
;;;    Similar to Naturalize-Integer transform.
;;;
(deftransform deport-integer ((signed sap offset size value form))
  (unless (constant-continuation-p size)
    (give-up "Size not constant, so cannot open-code integer access:~%~S"
	     (continuation-value form)))

  (let ((align (find-alignment offset))
	(size (continuation-value size))
	(bits (find-bit-offset offset)))
    (cond
     ((and (= size 32) (> align 4))
      '(setf (sap-ref-32 sap (ash offset -5)) value))
     ((and (= size 16) (> align 3))
      '(setf (sap-ref-16 sap (ash offset -4)) value))
     ((and (= size 8) (> align 2))
      '(setf (sap-ref-8 sap (ash offset -3)) value))
     ((not bits)
      (give-up "Can't determine offset within word, so cannot open-code:~%~S"
	       (continuation-value form)))
     ((<= (+ bits size) 32)
      `(setf (ldb (byte ,size ,bits) (sap-ref-32 sap (ash offset -5)))
	     value))
     ((> size 32)
      (give-up "Accesses of ~D bits unsupported:~%~S"
	       size (continuation-value form)))
     (t
      ;;
      ;; If the integer spans a 32 bit boundry, what we do depends on
      ;; the byte order.  See the comments in naturalize-integer.
      ;; 
      (let* ((high-bits (- 32 bits))
	     (low-bits (- size high-bits)))
	(multiple-value-bind (low-byte high-byte)
			     (ecase vm:target-byte-order
			       (:little-endian
				(values 'offset '(1+ offset)))
			       (:big-endian
				(values '(1+ offset) 'offset)))
	  `(let ((offset (ash offset -5)))
	     (setf (ldb (byte ,high-bits 0)
			(sap-ref-32 sap ,high-byte))
		   (ash value ,(- low-bits)))
	     (setf (ldb (byte ,low-bits ,(- 32 low-bits))
			(sap-ref-32 sap ,low-byte))
		   value))))))))


;;;; Boolean stuff:

(defknown naturalize-boolean (system-area-pointer index index t) boolean
  (flushable))
(defknown deport-boolean (system-area-pointer index index t t) void ())

;;; Naturalize and Deport Boolean transforms  --  Internal
;;; 
;;;     If the bit falls at a known position within a byte, then we can
;;; test and set it using a single logical operation instead of extracting the
;;; whole field.  If the value occupies a full byte/short/word, then then we
;;; just do the access, rather than messing around with masks.
;;;
;;; When setting, we must always set the entire field, so we can only be clever
;;; when the field is a single bit.
;;;
(deftransform naturalize-boolean ((sap offset size form))
  (unless (constant-continuation-p size)
    (give-up "Size not constant, so cannot open-code integer access:~% ~S"
	     (continuation-value form)))

  (let ((off (find-bit-offset offset 8))
	(size (continuation-value size)))
    (unless off
      (give-up "Can't determine offset within word, so cannot open-code:~% ~S"
	       (continuation-value form)))
    (if (and (>= (find-alignment offset) 3)
	     (member size '(8 16 32)))
	`(not (zerop (naturalize-integer nil sap offset ,size
					 ',(continuation-value form))))
	`(not (zerop (logand (sap-ref-8 sap (ash (+ offset ,(1- size)) -3))
			     ,(ash 1 (- 7 off))))))))
;;;
(deftransform deport-boolean ((sap offset size value form))
  (unless (constant-continuation-p size)
    (give-up "Size not constant, so cannot open-code integer access:~% ~S"
	     (continuation-value form)))

  (let ((off (find-bit-offset offset 8))
	(size (continuation-value size)))
    (unless off
      (give-up "Can't determine offset within word, so cannot open-code:~% ~S"
	       (continuation-value form)))
    (if (= size 1)
	`(let ((offset (ash offset -3)))
	   (setf (ldb (byte 1 ,off) (sap-ref-8 sap offset))
		 (if value 1 0)))
	`(deport-integer nil sap offset ,size (if value 1 0)
			 ',(continuation-value form)))))
