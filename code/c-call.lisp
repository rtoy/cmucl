;;; -*- Mode: Lisp; Package: EXTENSIONS; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains stuff used for calling out to C routines that are
;;; linked into the Lisp process.
;;;
;;; Written by Rob MacLachlan.
;;;
(in-package "EXTENSIONS" :use '("LISP" "SYSTEM"))
(import '(lisp::enumeration-info lisp::enumeration-info-size
				 lisp::enumeration-info-kind
				 lisp::enumeration-info-offset
				 lisp::enumeration-info-to
				 lisp::call-lisp-from-c))
(export '(c-sizeof def-c-type def-c-record def-c-array def-c-pointer
		   char unsigned-char short unsigned-short
		   int unsigned-int long unsigned-long
		   void def-c-routine def-c-variable
		   def-c-procedure))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

#+nil
(defvar foreign-routines-defined NIL
  "List of symbol/routine name pairs used to reset code pointers
  for foreign routines.")

#+nil
(defvar foreign-variables-defined NIL
  "List of symbol/variable name pairs used to reset pointers to
  foreign variables.")

(eval-when (compile load eval)

;;; The C-Type structure is used to represent aspects of C data representations
;;; that aren't directly implemented by Aliens.
;;;
(defstruct c-type
  ;;
  ;; A form which is a printable representation of this type.
  description
  ;;
  ;; The size in bits of objects of this type.  NIL if the size is variable.
  (size nil :type (or unsigned-byte null))
  ;;
  ;; The bit alignment used for allocating objects of this type.
  (alignment nil :type (integer 1 32)))

(defun %print-c-type (s stream d)
  (declare (ignore d))
  (format stream "#<C-Type ~S>" (c-type-description s)))


;;; A c-type which corresponds directly to an alien type.  The Description is
;;; the alien type.
;;;
(defstruct (primitive-type
	    (:include c-type)
	    (:print-function %print-c-type)))


;;; The Record-Type represnts a c structure type.
;;;
(defstruct (record-type
	    (:include c-type)
	    (:print-function %print-c-type))
  ;;
  ;; A list of the field descriptions for the fields in the record, in order of
  ;; increasing bit offset.
  (fields nil :type list))


;;; The Field-Info structure describes a single field in a record.  The Size
;;; recorded here may be larger than the Size in the Type, since more bits may
;;; be allocated to the field than are actually necessary to hold a value of
;;; that type.
;;;
(defstruct field-info
  ;;
  ;; The symbol name of this field.
  (name nil :type symbol)
  ;;
  ;; The c-type of this field.
  (type nil :type c-type)
  ;;
  ;; The bit offset from the start of the record that this field is located at.
  (offset nil :type unsigned-byte)
  ;;
  ;; The number of bits in this field.
  (size nil :type unsigned-byte))


;;; The Array-Type represents a C array type.
;;;
(defstruct (array-type
	    (:include c-type)
	    (:print-function %print-c-type))
  ;;
  ;; The c-type of the elements in the array.
  (element-type nil :type c-type)
  ;;
  ;; The number of bits used to store each element.  May be larger than the
  ;; Size in the Element-Type due to padding.
  (element-size nil :type unsigned-byte))


;;; The Pointer-Type represents a C pointer type.
;;;
(defstruct (pointer-type
	    (:include c-type)
	    (:print-function %print-c-type))
  ;;
  ;; The type of object pointed to.
  (to nil :type c-type))


;;; An EQ hashtable from the names of c-types to the structures describing them.
;;;
(defvar *c-type-names* (make-hash-table :test #'eq))

;;; Find-Alignment  --  Internal
;;;
;;;    Return the bit alignment for an object of the specified Size.
;;;
(proclaim '(function find-alignment (unsigned-byte) (integer 1 32)))
(defun find-alignment (size)
  (cond ((> size 16) 32)
	((> size 8) 16)
	((> size 1) 8)
	(t 1)))


;;; Align-Offset  --  Internal
;;;
;;;    Return Offset with enough added to bring it out to the specified
;;; Alignment.
;;;
(proclaim '(function align-offset (unsigned-byte (integer 1 32))
		     unsigned-byte))
(defun align-offset (offset alignment)
  (let ((extra (rem offset alignment)))
    (if (zerop extra) offset (+ offset (- alignment extra)))))


;;; Get-C-Type  --  Internal
;;;
;;;    Get the C-Type structure corresponding to the supplied Spec.  If the
;;; spec is a named type, then just return the info.  Otherwise, the Spec must
;;; be a primitive Alien type with an obvious size.  If we can't decide the
;;; Spec, we signal an error.
;;;
(proclaim '(function get-c-type (t) c-type))
(defun get-c-type (spec)
  (cond ((gethash spec *c-type-names*))
	((member spec
		 '(c-procedure short-float long-float null-terminated-string))
	 (let ((size (if (eq spec 'long-float) 64 32)))
	   (make-primitive-type
	    :description spec
	    :size size
	    :alignment (find-alignment size))))
	((and (listp spec) (> (length spec) 1)
	      (symbolp (first spec)))
	 (case (first spec)
	   ((signed-byte unsigned-byte)
	    (let ((size (second spec)))
	      (make-primitive-type
	       :description spec
	       :size size
	       :alignment (find-alignment size))))
	   (perq-string
	    (make-primitive-type
	     :description spec
	     :size (* (1+ (second spec)) 8)
	     :alignment 8))
	   (null-terminated-string
	    (make-primitive-type
	     :description spec
	     :size (* (second spec) 8)
	     :alignment 8))
	   (enumeration
	    (let* ((name (second spec))
		   (info (get name 'enumeration-info)))
	      (unless info
		(error "~S is not a defined enumeration." name))
	      (let ((size (enumeration-info-size info)))
		(make-primitive-type
		 :description spec
		 :size size
		 :alignment (find-alignment size)))))
	   (pointer
	    (make-primitive-type
	     :description spec
	     :size vm:word-bits
	     :alignment vm:word-bits))
	   (alien
	    (let ((size (third spec)))
	      (unless size
		(error "Must specify size in Alien C-Type: ~S." spec))
	      (make-primitive-type
	       :description spec
	       :size size
	       :alignment (find-alignment size))))
	   (t
	    (error "~S is not a known C-Type." spec))))
	(t (error "Losing C-Type: ~S." spec))))


); Eval-When (Compile Load Eval)


;;;; Exported type operations:

(eval-when (compile load eval)

;;; Symbolicate  --  Internal
;;;
;;;    Concatenate together the names of some strings and symbols, producing
;;; a symbol in the current package.
;;;
(proclaim '(function symbolicate (&rest (or string symbol)) symbol))
(defun symbolicate (&rest things)
  (values (intern (reduce #'(lambda (x y)
			      (concatenate 'string (string x) (string y)))
			  things))))

); Eval-When (Compile Load Eval)


;;; C-Sizeof  --  Public
;;;
(proclaim '(function c-sizeof (t) unsigned-byte))
(defun c-sizeof (spec)
  "Return the size in bits of the C-Type described by Spec."
  (c-type-size (get-c-type spec)))

#-new-compiler
(pushnew 'clc::fold-transform (get 'c-sizeof 'clc::clc-transforms))


;;; Def-C-Type  --  Public
;;;
(defmacro def-c-type (name spec)
  "Def-C-Type Name Spec
  Define Name to be an abbreviation for the C-Type indicated by Spec."
  `(eval-when (compile load eval)
     (setf (gethash ',name *c-type-names*) ',(get-c-type spec))
     ',name))


;;; Def-C-Record  --  Public
;;;
(defmacro def-c-record (name &rest fields)
  "Name {(Name Type)}*
  Define a record C-Type.  Name is the name of the type.  The Fields and Types
  specify the name and type of each field.  An Alien operator Name-Field is
  defined to select each field.  The Function Make-Name creates a dynamic alien
  of the appropriate size and type.  Also a pointer to name type is created, so
  that you can have pointers to a thing of type name in the definition of name.
  The name of the pointer type is *name."
  (let* ((info ())
	 (pname (symbolicate "*" name))
	 (pos 0)
	 (align 0)
	 (res (make-record-type
	       :description name
	       :size NIL
	       :alignment 1
	       :fields NIL))
	 (pres (make-pointer-type
		:description NIL
		:size 32
		:alignment 32
		:to res)))
    (setf (gethash name *c-type-names*) res)
    (setf (gethash pname *c-type-names*) pres)
    (dolist (field fields)
      (unless (= (length field) 2)
	(error "Malformed field specification: ~S." field))
      (let* ((ftype (second field))
	     (type (if (eq ftype pname) pname (get-c-type (second field))))
	     (size (if (eq ftype pname) 32 (c-type-size type)))
	     (start (align-offset pos (if (eq ftype pname) 32
					  (c-type-alignment type)))))
	(push (make-field-info :name (first field)
			       :type type
			       :offset start
			       :size size)
	      info)
	(unless size
	  (error "Variable size field ~A in record ~A not allowd."
		 (first field) name))
	(setq pos (+ start size))
	(setq align (max (find-alignment size) align))))

    (setf (record-type-size res) pos)
    (setf (record-type-alignment res) align)
    (setf (record-type-fields res) (nreverse info))
    (setf (pointer-type-description pres) `(alien ,name ,pos))
    `(progn
      (eval-when (compile load eval)
	(setf (gethash ',name *c-type-names*) ',res)
	(setf (gethash ',pname *c-type-names*) ',pres))
      (defun ,(symbolicate "MAKE-" name) ()
	(make-alien ',name ,(c-type-size res)))
      (defoperator (,(symbolicate "INDIRECT-" pname)
		    ,(record-type-description res))
		   ((pointer ,(c-type-description pres)))
	`(alien-indirect (alien-value ,pointer) ,,pos))
      ,@(define-record-operators res))))

(eval-when (compile load eval)

;;; Define-Record-Operators  --  Internal
;;;
;;;    Compute the operator definitions for accessing the fields in Record.
;;;
(proclaim '(function define-record-operators (record-type) list))
(defun define-record-operators (record)
  (let ((name (c-type-description record)))
    (mapcar #'(lambda (x)
		`(defoperator (,(symbolicate name "-" (field-info-name x))
			       ,(c-type-description (let ((type (field-info-type x)))
						      (if (structurep type) type
							  (get-c-type type)))))
			      ((rec ,name))
		  `(alien-index (alien-value ,rec)
				,,(field-info-offset x)
				,,(field-info-size x))))
	    (record-type-fields record))))

); Eval-When (Compile Load Eval)


;;; Def-C-Array  --  Public
;;;
(defmacro def-c-array (name element-type &optional size)
  "Def-C-Array Name Element-Type [Size]
  Define Name to be an array C-Type with the specified Element-Type.  If size
  is not specified, then it is a variable size array."
  (let* ((eltype (get-c-type element-type))
	 (elalign (c-type-alignment eltype))
	 (elsize (align-offset (c-type-size eltype) elalign))
	 (elts (eval size))
	 (res (make-array-type :description name
			       :size (if elts (* elsize elts) nil)
			       :alignment elalign
			       :element-type eltype
			       :element-size elsize)))
    `(progn
      (eval-when (compile load eval)
	(setf (gethash ',name *c-type-names*) ',res))

      (defun ,(symbolicate "MAKE-" name)
	     ,(if elts () '(size))
	(make-alien ',name ,(if elts (* elsize elts) `(* ,elsize size))))

      (defoperator (,(symbolicate name "-REF")
		    ,(c-type-description eltype))
		   ((array ,name) i)
	`(alien-index (alien-value ,array) (* ,,elsize ,i) ,,elsize)))))


;;; Def-C-Pointer  --  Public
;;;
(defmacro def-c-pointer (name to)
  "Def-C-Pointer Name To
  Define a pointer C-Type which points to an object of type To."
  (let* ((type (get-c-type to))
	 (res (make-pointer-type
	       :description `(alien ,(c-type-description type)
				    ,@(when (c-type-size type)
					`(,(c-type-size type))))
	       :size 32
	       :alignment 32
	       :to type)))
    `(progn
      (eval-when (compile load eval)
	(setf (gethash ',name *c-type-names*) ',res))
      (defoperator (,(symbolicate "INDIRECT-" name)
		    ,(c-type-description type))
		   ((pointer ,(c-type-description res))
		    ,@(unless (c-type-size type)
			'(size)))
	`(alien-indirect (alien-value ,pointer)
			 ,,(or (c-type-size type) 'size))))))

;;; Some trivial builtin types...
;;;  
(setf (gethash 'port *c-type-names*)
      (make-primitive-type :description 'port
			   :size 32
			   :alignment 32))

(setf (gethash 'string-char *c-type-names*)
      (make-primitive-type :description 'string-char
			   :size 8
			   :alignment 8))

(setf (gethash 'boolean *c-type-names*)
      (make-primitive-type :description 'boolean
			   :size 1
			   :alignment 1))

(setf (gethash 'system-area-pointer *c-type-names*)
      (make-primitive-type :description 'system-area-pointer
			   :size 32
			   :alignment 32))


;;; Some more standard types:

(def-c-type char (signed-byte 8))
(def-c-type unsigned-char (unsigned-byte 8))
(def-c-type short (signed-byte 16))
(def-c-type unsigned-short (unsigned-byte 16))
(def-c-type int (signed-byte 32))
(def-c-type unsigned-int (unsigned-byte 32))
(def-c-type long (signed-byte 32))
(def-c-type unsigned-long (unsigned-byte 32))

(def-c-pointer *char char)



(defstruct (routine-info
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (format stream "#<Routine-Info ~S>" (routine-info-name s)))))
  ;;
  ;; String name of the routine and symbol name of the interface function.
  (name "" :type string)
  (function-name nil :type symbol)
  ;;
  ;; List of all the doc strings.
  docs
  ;;
  ;; The number of words of arguments.
  (arg-size 0 :type unsigned-byte)
  ;;
  ;; The name of the record that describes all the stuff on the stack.
  (stack-record nil :type symbol)
  ;;
  ;; The name of the alien :copy args are allocated in.
  (copy-args-buffer nil :type (or null symbol))
  ;;
  ;; List of Arg-Info structures describing the args.
  (args nil :type list)
  ;;
  ;; The specified return-type for the function value.  Null if Void was
  ;; specified.
  (return-type nil :type (or c-type null))
  ;;
  ;; The SC the return value is passed in.
  return-sc
  ;;
  ;; The vop we should use to coerce the return value.
  return-move-vop
  ;;
  ;; The type for the return value, after the move vop and before any lisp
  ;; level coerce code.
  return-lisp-type
  ;;
  ;; Generate the lisp-level code to coerce the return value (if any)
  return-coerce-generator
  )

(defstruct arg-info
  ;;
  ;; Symbol name of the arg.
  (name nil :type symbol)
  ;;
  ;; C-Type describing the actual argument to the routine.
  (type nil :type c-type)
  ;;
  ;; Specified mode and options.
  mode
  options
  ;;
  ;; Where we pass this arg.  Either a storage class or :stack
  allocation
  ;;
  ;; Either the offset in the storage class or stack.
  offset
  ;;
  ;; Either the vop necessary to do the move, or the stack operator.
  accessor
  ;;
  ;; Either operator to get at the arg, or NIL if it's :in.
  buffer
  ;;
  ;; Either the name for :in, or some form for others.
  passing-form
  )


(defun anotate-foreign-call (info)
  (let ((offset 0))
    (dolist (arg (routine-info-args info))
      (setf (arg-info-allocation arg) :stack)
      (setf (arg-info-offset arg) offset)
      (let ((size (if (eq (arg-info-mode arg) :in)
		      (c-type-size (arg-info-type arg))
		      (c-sizeof 'system-area-pointer))))
	(setf offset (align-offset (+ offset size)
				   (find-alignment size)))))
    (setf (routine-info-arg-size info) offset))
  (multiple-value-bind
      (sc move-vop lisp-type)
      (let ((type (routine-info-return-type info)))
	(etypecase type
	  (null)
	  (primitive-type
	   (let* ((descr (c-type-description type))
		  (name (if (atom descr) descr (car descr)))
		  (arg (if (consp descr) (cadr descr))))
	     (ecase name
	       (signed-byte 
		(values 'c::signed-reg 'c::move descr))
	       (unsigned-byte
		(values 'c::unsigned-reg 'c::move descr))
	       (null-terminated-string
		(setf (routine-info-return-coerce-generator info)
		      #'(lambda (form)
			  `(import-string ,form)))
		(values 'c::sap-reg 'c::move 'system-area-pointer))
	       (boolean
		(setf (routine-info-return-coerce-generator info)
		      #'(lambda (form)
			  `(not (zerop ,form))))
		(values 'c::signed-reg 'c::move 'fixnum))
	       ((system-area-pointer alien)
		(when (eq name 'alien)
		  (unless (> (length descr) 2)
		    (error "Alien return types must include the size: ~S"
			   descr))
		  (setf (routine-info-return-coerce-generator info)
			#'(lambda (form)
			    `(make-alien ',arg ',(caddr descr) ,form))))
		(values 'c::sap-reg 'c::move 'system-area-pointer))
	       ((enumeration simple-string port short-float long-float)
		(error "Can't return ~S yet." descr)))))
	  (record-type
	   (error "Can't return ~S yet." (c-type-description type)))
	  ((or array-type pointer-type)
	   (unless (c-type-size type)
	     (error "Can't return arrays of unknown size: ~S"
		    (c-type-description type)))
	   (setf (routine-info-return-coerce-generator info)
		 #'(lambda (form)
		     `(make-alien ',(c-type-description type)
				  ',(c-type-size type)
				  ,form)))
	   (values 'c::sap-ref 'c::move 'system-area-pointer))))
    (setf (routine-info-return-sc info) sc)
    (setf (routine-info-return-move-vop info) move-vop)
    (setf (routine-info-return-lisp-type info) lisp-type))
  (undefined-value))

(defun pick-names (info)
  (setf (routine-info-stack-record info)
	(symbolicate (routine-info-function-name info) "-STACK-FRAME"))
  (dolist (arg (routine-info-args info))
    (unless (eq (arg-info-mode arg) :in)
      (setf (arg-info-buffer arg)
	    (symbolicate (routine-info-function-name info)
			 "-"
			 (arg-info-name arg)
			 "-BUFFER")))
    (when (eq (arg-info-allocation arg) :stack)
      (setf (arg-info-accessor arg)
	    (symbolicate (routine-info-function-name info)
			 "-"
			 (arg-info-name arg)
			 (if (eq (arg-info-mode arg) :in)
			     "-DATA"
			     "-POINTER"))))))

(defun compute-c-call-forms (info)
  (let ((lisp-args nil)
	(top-level-forms nil)
	(arg-set-forms nil)
	(result-get-forms nil))
    (dolist (arg (routine-info-args info))
      (cond ((arg-info-buffer arg)
	     (unless (pointer-type-p (arg-info-type arg))
	       (error "~S argument ~S must be a pointer type."
		      (arg-info-mode arg)
		      (arg-info-name arg)))
	     (let* ((type (pointer-type-to (arg-info-type arg)))
		    (size (c-type-size type))
		    (offset (align-offset (routine-info-arg-size info)
					  (find-alignment size))))
	       (setf (routine-info-arg-size info)
		     (+ offset size))
	       (push `(defoperator (,(arg-info-buffer arg)
				    ,(c-type-description
				      (pointer-type-to
				       (arg-info-type arg))))
				   ((foo ,(routine-info-stack-record info)))
			`(alien-index (alien-value ,foo) ,,offset ,,size))
		     top-level-forms)
	       (setf (arg-info-passing-form arg)
		     `(alien-sap (,(arg-info-buffer arg)
				  (alien-value stack))))))
	    (t
	     (setf (arg-info-passing-form arg) (arg-info-name arg))))

      (unless (eq (arg-info-mode arg) :out)
	(push (arg-info-name arg) lisp-args)
	(when (arg-info-buffer arg)
	  (push `(setf (alien-access (,(arg-info-buffer arg)
				      (alien-value stack)))
		       ,(arg-info-name arg))
		arg-set-forms)))

      (when (eq (arg-info-allocation arg) :stack)
	(let* ((type (arg-info-type arg))
	       (descr (c-type-description type))
	       (size (c-type-size type)))
	  (push `(defoperator (,(arg-info-accessor arg) ,descr)
			      ((foo ,(routine-info-stack-record info)))
		   `(alien-index (alien-value ,foo)
				 ,,(arg-info-offset arg)
				 ,,size))
		top-level-forms))
	(push
	 `(setf (alien-access (,(arg-info-accessor arg)
			       (alien-value stack))
			      ,@(when (or (not (eq (arg-info-mode arg) :in))
					  (pointer-type-p (arg-info-type arg)))
				  '('system-area-pointer)))
		,(arg-info-passing-form arg))
	 arg-set-forms))

      (when (member (arg-info-mode arg) '(:out :in-out))
	(push `(alien-access (,(arg-info-buffer arg) stack))
	      result-get-forms)))
	     
    (values (nreverse top-level-forms)
	    (nreverse lisp-args)
	    (nreverse arg-set-forms)
	    (nreverse result-get-forms))))
	    

(defmacro def-c-routine (name (return-type &key) &rest specs)
  "Def-C-Routine Name (Return-Type Option*)
                    {(Arg-Name Arg-Type [Mode] Arg-Option*)}*

  Define a foreign interface function for the routine with the specified string
  Name.  Normally the interface function is named by interning the uppercased
  name in the current package.  A different interface function name may be
  specified by using a list (Name Function-Name) in the place of Name.
  
  Return-Type is the C-Type for the C function return value.  Void may be used
  to specify a function with no result.

  The remaining forms specifiy individual arguments that are passed to the
  routine.  Arg-Name is a symbol that names the argument, primarily for
  documentation.  Arg-Type is the C-Type of the argument.  Mode specifies the
  say that the argument is passed.

  :In
        An :In argument is simply passed by value.  The value to be passed is
        obtained from argument(s) to the interface function.  No values are
        returned for :In arguments.  This is the default mode.

  :Out
        The specified argument type must be a pointer to a fixed sized object.
        A pointer to a preallocated object is passed to the routine, and the
        the object is accessed on return, with the value(s) being returned from
        the interface function.

  :Copy
        Similar to :In, except that the argument values are stored in on
        the stack, and a pointer to the object is passed instead of
        the values themselves.

  :In-Out
        A combination of :Out and :Copy.  A pointer to the argument is passed,
        with the object being initialized from supplied argument(s) and
        return value(s) being determined by accessing the object on return."

  (let ((info (make-routine-info)))
    (cond ((stringp name)
	   (setf (routine-info-name info) name)
	   (setf (routine-info-function-name info)
		 (intern (string-upcase name))))
	  ((and (listp name) (= (length name) 2)
		(stringp (first name)) (symbolp (second name)))
	   (setf (routine-info-name info) (first name))
	   (setf (routine-info-function-name info) (second name)))
	  (t
	   (error "Malformed routine name specification: ~S." name)))

    (let ((docs ())
	  (arg-info ()))
      (dolist (spec specs)
	(cond ((stringp spec)
	       (push spec docs))
	      ((and (listp spec) (>= (length spec) 2))
	       (let ((arg-name (first spec))
		     (arg-type (get-c-type (second spec)))
		     (mode (or (third spec) :in))
		     (options (cdddr spec)))
		 (when (oddp (length options))
		   (error "Odd number of options in ~S." spec))
		 (unless (symbolp arg-name)
		   (error "Arg name is not a symbol: ~S." arg-name))
		 (push (make-arg-info :name arg-name :type arg-type
				      :mode mode :options options)
		       arg-info)))
	      (t
	       (error "Bad argument spec: ~S." spec))))

      (setf (routine-info-docs info) (nreverse docs))
      (setf (routine-info-args info) (nreverse arg-info)))

    (unless (eq return-type 'void)
      (setf (routine-info-return-type info) (get-c-type return-type)))

    (anotate-foreign-call info)
    (pick-names info)

    (multiple-value-bind
	(top-level-forms lisp-args arg-set-forms result-get-forms)
	(compute-c-call-forms info)
      (let ((call-form `(call-foreign-function
			 ,(routine-info-name info)
			 ',info
			 ,@(mapcar #'arg-info-passing-form
				   (remove :stack (routine-info-args info)
					   :key #'arg-info-allocation)))))
	(when (routine-info-return-coerce-generator info)
	  (setf call-form
		(funcall (routine-info-return-coerce-generator info)
			 call-form)))
	`(progn
	   (compiler-let ((*alien-eval-when* '(compile eval)))
	     ,@top-level-forms)
	   (defun ,(routine-info-function-name info) ,lisp-args
	     ,@(or (routine-info-docs info)
		   (list (make-doc-string info)))
	     (declare (optimize (speed 3) (safety 0)))
	     (with-stack-alien (stack ,(routine-info-stack-record info)
				      ,(routine-info-arg-size info))
	       ,@arg-set-forms
	       ,@(if (null (routine-info-return-type info))
		     (if (null result-get-forms)
			 `(,call-form
			   (undefined-value))
			 `(,call-form
			   (values ,@result-get-forms)))
		     (if (null result-get-forms)
			 `(,call-form)
			 `((values ,call-form
				   ,@result-get-forms)))))))))))



;;; Make-Doc-String  --  Internal
;;;
;;;    Make a doc string for the interface routine described by Info.  Values
;;; is a list of the names of the by-reference return values.
;;;
(proclaim '(function make-doc-string (routine-info) string))
(defun make-doc-string (info)
  (let ((*print-pretty* t)
	(*print-case* :downcase)
	(values (mapcar #'arg-info-name
			(remove-if-not #'(lambda (mode)
					   (member mode '(:out :in-out)))
				       (routine-info-args info)
				       :key #'arg-info-mode))))
    (format nil "Interface to foreign routine ~S~:[; returns no values.~;~
	    ~:*, return values:~%  ~A~]"
	    (routine-info-name info)
	    (if (routine-info-return-type info)
		(cons 'return-value values)
		values))))



;;; Def-C-Variable defines a global C-Variable, so that it is available to
;;; Lisp.  It accepts the name of the variable (as a string) and the type
;;; of the variable.

(defmacro def-c-variable (name type)
  "Defines a foreign variable so that it is available from Lisp.
  Name should either be a string with the name of the foreign variable or
  a list of the string and the symbol to use as the alien variable. 
  Type is the foreign type of the variable."
  (multiple-value-bind
      (symbol name)
      (cond ((stringp name)
	     (values (intern (string-upcase name))
		     name))
	    ((and (consp name) (= (length name) 2)
		  (stringp (car name)) (symbolp (cadr name)))
	     (values (cadr name) (car name)))
	    (t
	     (error "Bogus name for def-c-variable: ~S.~%~
	     Should be either a string or a list of a string and symbol."
		    name)))
    (let* ((c-info (get-c-type type))
	   (c-type (if (primitive-type-p c-info)
		       (c-type-description c-info)
		       type))
	   (c-size (c-type-size c-info)))
      `(progn
	 (defparameter ,symbol
	   (make-alien ',c-type ,c-size
		       (truly-the system-area-pointer
				  (%primitive c::foreign-symbol-address
					      ,name))))
	 (eval-when ,*alien-eval-when*
	   (setf (info variable alien-value ',symbol)
		 (lisp::make-ct-a-val
		  :type ',c-type
		  :size ,c-size
		  :offset 0
		  :sap '(truly-the system-area-pointer
				   (%primitive c::foreign-symbol-address
					       ,name))
		  :alien ',symbol)))))))

#|
;;; Def-C-Procedure defines data structures etc. so that C can be passed
;;; a pointer to a C procedure object which when called will invoke a Lisp
;;; function.  Def-C-Procedure accepts three arguments: a symbol whose
;;; value is set to an object which can be written to alien-structures
;;; that will look like a c procedure object; a number which is a constant
;;; number of arguments that the C routine will be called with; and a Lisp
;;; function (an object callable as a function) which should accept the
;;; same number of arguments and will be called when the C procedure object
;;; is invoked.

(defmacro def-c-procedure (name nargs lfunc)
  "Assigns to the value of the symbol name  an object which can be
  passed to a foreign function as a procedure object.  Nargs is the
  number of arguments the procedure should accept.  Lfunc is the Lisp
  function will be called when the procedure is called.  Lfunc should
  be callable by apply."
  (let ((var (gensym)))
  `(defparameter ,name
     (let ((,var (%primitive alloc-static-g-vector 3)))
       (setf (svref ,var 0)
	     (int-sap (logior (ash clc::type-assembler-code
				   (+ clc::type-shift-16 16))
			      (get 'clc::call-lisp
				   'lisp::%loaded-address))))
       (setf (svref ,var 1) ,nargs)
       (setf (svref ,var 2) ,lfunc)
       ,var))))

;;; Call-Lisp-from-C is a Lisp function that gains control when a C
;;; function calls a procedure object that is defined as above.  It
;;; wraps an unwind-protect around the call to the function, so that
;;; if we throw passed it, the c-stack-information will be reset.

(defun call-lisp-from-c (old-c-stack procedure &rest args)
  (unwind-protect
    (let ((rv (apply (svref procedure 2) args)))
	(%primitive return-to-c old-c-stack rv))
    (%primitive reset-c-stack old-c-stack)))

;;; Reset-foreign-pointers goes through the list of all defined foreign
;;; functions and variables and sets the pointers to their new location.

(defun reset-foreign-pointers ()
  "Reset all the code and variable pointers that may have moved."
  (dolist (x foreign-routines-defined)
    (setf (symbol-value (car x)) (get-code-pointer (cdr x))))
  (dolist (x foreign-variables-defined)
    (setf (lisp::alien-value-sap (symbol-value (car x)))
	  (get-data-pointer (cdr x)))))
|#

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* nil))
