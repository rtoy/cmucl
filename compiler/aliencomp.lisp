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
	  sap-ref-8 sap-ref-16 sap-ref-32
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32
	  %set-alien-access
	  
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
	       (or (ct-a-val-p (cdr (assoc exp *venv*)))
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

  '(%primitive sap-system-ref sap (ash offset -4)))


;;; Grovel-Alien-Var  --  Internal
;;;
;;;    Look the var up in *venv*, and then check if it is a global
;;; alien-variable.  If it ain't there or is the the wrong type then flame out.
;;;
(defun grovel-alien-var (type var)
  (let* ((local-def (cdr (assoc var *venv*)))
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
    (compiler-error "Does not result in a ~S alien value:~% ~S" type exp))
  
  (unless (= (lisp::alien-info-num-args info) (length (cdr exp)))
    (compiler-error "Alien operator not called with ~D args:~% ~S"
		    (lisp::alien-info-num-args info) exp))
  
  (do* ((args (cdr exp) (cdr args))
	(dums ())
	(*venv* *venv*)
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
	       (push (cons var val) *venv*)
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

(defknown sap+ (t t) t (movable flushable))
(deftransform sap+ ((sap offset))
  (unless (and (constant-continuation-p offset)
	       (eql (continuation-value offset) 0))
    (give-up))
  'sap)

(defknown sap-int (t) unsigned-byte (movable flushable))
(defknown int-sap (unsigned-byte) t (movable flushable))

(defknown %aligned-sap (t t t) t (movable flushable))
(deftransform %aligned-sap ((sap offset form))
  (unless (> (find-alignment offset) 3)
    (give-up))
  '(sap+ sap offset))

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

(defknown sap-ref-8 (t index) (unsigned-byte 8) (flushable))
(defknown sap-ref-16 (t index) (unsigned-byte 16) (flushable))
(defknown sap-ref-32 (t index) (unsigned-byte 32) (flushable))
(defknown (setf sap-ref-8) (t index (unsigned-byte 8)) (unsigned-byte 8) ())
(defknown (setf sap-ref-16) (t index (unsigned-byte 16)) (unsigned-byte 16) ())
(defknown (setf sap-ref-32) (t index (unsigned-byte 32)) (unsigned-byte 32) ())


;;;; Alien variable special forms:

;;; Alien-Bind IR1 convert  --  Internal
;;;
(def-ir1-translator alien-bind ((binds &body body &whole source) start cont)
  (let ((*venv* *venv*))
    (collect ((lets nil nconc)
	      (stuff nil nconc))
      (dolist (bind binds)
	(unless (<= 2 (length bind) 4)
	  (compiler-error "Malformed Alien-Bind specifier:~% ~S" bind))
	(let ((var (first bind))
	      (val (second bind))
	      (typ (third bind))
	      (aligned (fourth bind)))

	  (multiple-value-bind (l s res)
			       (analyze-alien-expression typ val)
	    (unless (ct-a-val-type res)
	      (compiler-error "Must specify type, since it is not apparent ~
	                       from the value:~% ~S" bind))
	    (lets l)
	    (stuff s)
	    (let* ((offset (ct-a-val-offset res))
		   (sap (ct-a-val-sap  res))
		   (size (ct-a-val-size res))
		   (n-size (gensym))
		   (n-sap (gensym))
		   (n-offset (gensym)))
	      (lets `((,n-sap ,(if aligned
				   `(%aligned-sap ,sap ,offset ',source)
				   sap))
		      (,n-size ,size)
		      (,n-offset ,(if aligned 0 offset))))

	      (push (cons var
			  (make-ct-a-val :type (ct-a-val-type res)
					 :offset (if aligned 0 n-offset)
					 :size n-size
					 :sap n-sap
					 :alien (ct-a-val-alien res)))
		    *venv*)))))

    (ir1-convert start cont
		 `(let* ,(reverse (lets))
		    ,(ignore-unreferenced-vars (lets))
		    ,@(nreverse (stuff))
		    ,@body)))))


;;; With-Stack-Alien-Transform  --  Internal
;;;
;;;
(def-ir1-translator with-stack-alien (((var stack) &body forms) start cont)
  (let ((info (info alien-stack info stack)))
    (unless info
      (compiler-error "~S is not the name of a declared alien stack." stack))

    (let* ((n-current (lisp::stack-info-current info))
	   (n-sap (gensym))
	   (n-alien (gensym))
	   (*venv* (acons var
			  (make-ct-a-val :type (lisp::stack-info-type info)
					 :size (lisp::stack-info-size info)
					 :sap n-sap
					 :offset 0
					 :alien n-alien)
			  *venv*)))
      (ir1-convert start cont
		   `(let* ((,n-alien (or (car ,n-current)
					 (,(lisp::stack-info-grow info))))
			   (,n-sap (lisp::alien-value-sap ,n-alien))
			   (,n-current (cdr ,n-current)))
		      ,@forms)))))


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
       (+ (sap-int ,(ct-a-val-sap res)) (/ ,(ct-a-val-offset res) 16)))))


;;; Alien-SAP soruce transform  --  Internal
;;;
;;;
(def-source-transform alien-address (alien &whole source)
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

(defknown %alien-access (t unsigned-byte unsigned-byte t t t) t
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

(defknown %%set-alien-access (t unsigned-byte unsigned-byte t t t t) t)

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
	 (declare (fixnum res))
	 (if (zerop (logand res ,(ash 1 (1- size))))
	     res
	     (logior res ,(ash -1 size))))
      form))


(defknown naturalize-integer (t t unsigned-byte unsigned-byte t) integer
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
	(signed (continuation-value signed)))
    (cond
     ((or (and (> size 15) (< align 4))
	  (and (> size 16) (< align 5)))
      (give-up "Could not show ~D bit access to be word-aligned:~%~S"
	       size (continuation-value form)))
     ((= size 32)
      (if signed
	  '(%primitive signed-32bit-system-ref sap (ash offset -4))
	  '(%primitive unsigned-32bit-system-ref sap (ash offset -4))))
     ((= size 16)
      (if signed
	  '(%primitive signed-16bit-system-ref sap (ash offset -4))
	  '(%primitive 16bit-system-ref sap (ash offset -4))))
     ((> size 15)
      (compiler-warning "Access of ~D bit bytes is not supported:~%~S"
			size (continuation-value form))
      (give-up))
     ((and (> align 2) (= size 8))
      (sign-extend '(%primitive 8bit-system-ref sap (ash offset -3))
		   8 signed))
     (t
      (let ((bits (find-bit-offset offset)))
	(unless bits
	  (give-up "Can't determine offset within word, so cannot ~
	  open-code:~%~S"
		   (continuation-value form)))
	
	(if (>= (+ bits size) 16)
	    (let* ((hi-bits (- 16 bits))
		   (lo-bits (- size hi-bits)))
	      ;;
	      ;; If the integer spans a 16bit boundry, then the high bits in
	      ;; the integer are the low bits in the first word, and the low
	      ;; bits in the integer are the high bits in the next word.
	      (sign-extend
	       `(let ((offset (ash offset -4)))
		  (logior (ash (ldb (byte ,hi-bits 0)
				    (sap-ref-16 sap offset))
			       ,lo-bits)
			  (ash (sap-ref-16 sap (1+ offset))
			       ,(- (- 16 lo-bits)))))
	       size signed))
	    (sign-extend `(ldb (byte ,size ,bits)
			       (sap-ref-16 sap (ash offset -4)))
			 size signed)))))))



(defknown deport-integer (t t unsigned-byte unsigned-byte integer t) void
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
	(size (continuation-value size)))
    (cond
     ((or (and (> size 15) (< align 4))
	  (and (> size 16) (< align 5)))
      (give-up "Could not show ~D bit access to be word-aligned:~%~S"
	       size (continuation-value form)))
     ((= size 32)
      '(%primitive signed-32bit-system-set sap (ash offset -4) value))
     ((= size 16)
      '(%primitive 16bit-system-set sap (ash offset -4) value))
     ((> size 15)
      (compiler-warning "Access of ~D bit bytes is not supported:~%~S"
			size (continuation-value form))
      (give-up))
     ((and (> align 2) (= size 8))
      '(%primitive 8bit-system-set sap (ash offset -3) value))
     (t
      (let ((bits (find-bit-offset offset)))
	(unless bits
	  (give-up "Can't determine offset within word, so cannot ~
	            open-code:~%~S"
		   (continuation-value form)))
	
	(if (>= (+ bits size) 16)
	    (let* ((hi-bits (- 16 bits))
		   (lo-bits (- size hi-bits)))
	      ;;
	      ;; If the integer spans a 16bit boundry, then the high bits in
	      ;; the integer are the low bits in the first word, and the low
	      ;; bits in the integer are the high bits in the next word.
	      `(let ((offset (ash offset -4)))
		 (%primitive 16bit-system-set sap offset
			     (dpb (ash value ,(- lo-bits))
				  (byte ,hi-bits 0)
				  (sap-ref-16 sap offset)))
		 (%primitive 16bit-system-set sap (1+ offset)
			     (dpb value
				  (byte ,lo-bits ,(- 16 lo-bits))
				  (sap-ref-16 (1+ offset))))))
	    `(let ((offset (ash offset -4)))
	       (setf (sap-ref-16 sap offset)
		     (dpb value
			  (byte ,size ,bits)
			  (sap-ref-16 sap offset))))))))))


;;;; Boolean stuff:

(defknown naturalize-boolean (t index index t) boolean (flushable))
(defknown deport-boolean (t index index t t) void ())

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
	   (setf (sap-ref-8 sap offset)
		 (let ((old (sap-ref-8 sap offset)))
		   (if value
		       (logior old ,(ash 1 (- 7 off)))
		       (logand old ,(lognot (ash 1 (- 7 off))))))))
	`(deport-integer nil sap offset ,size (if value 1 0)
			 ',(continuation-value form)))))
