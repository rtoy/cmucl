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
		   def-c-procedure reset-foreign-pointers))

#-new-compiler
(eval-when (compile)
  (setq lisp::*bootstrap-defmacro* t))

(defvar foreign-routines-defined NIL
  "List of symbol/routine name pairs used to reset code pointers
  for foreign routines.")

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
	((memq spec '(c-procedure short-float long-float))
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
		   ((pointer ,pname))
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
		   ((pointer ,name)
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
  ;; Symbol name of the variable holding the code pointer for the routine.
  (code nil :type symbol)
  ;;
  ;; The number of words of arguments.
  (arg-size 0 :type unsigned-byte)
  ;;
  ;; The global Alien var that we build the arguments in.  This is also the
  ;; type of the Alien.
  (arg-alien nil :type symbol)
  ;;
  ;; The number of words of stack allocated for reference args.
  (result-size 0 :type unsigned-byte)
  ;;
  ;;  Similar to Arg-Alien, but we receive results in it.
  (result-alien nil :type symbol)
  ;;
  ;; List of Arg-Info structures.
  (args nil :type list)
  ;;
  ;; The specified return-type for the function value.  Null if Void was
  ;; specified.
  (return-type nil :type (or c-type null)))


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
  ;; Names of the operators that access this arg in the Args and Results
  ;; Aliens.
  (operator nil :type symbol)
  (result-operator nil :type symbol)
  )


;;;
;;;    A list of top-level forms that are emitted before the Defun.  This is
;;; built in reverse order.
;;;
(defvar *output-forms*)

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
        Similar to :In, except that the argument values are stored in a
        preallocated object, and a pointer to the object is passed instead of
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

    (let ((arg-info ()))
      (dolist (spec specs)
	(unless (and (listp spec) (>= (length spec) 2))
	  (error "Bad argument spec: ~S." spec))
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

      (setf (routine-info-args info) (nreverse arg-info)))

    (unless (eq return-type 'void)
      (setf (routine-info-return-type info) (get-c-type return-type)))

    (let ((*output-forms* ()))
      (link-routine-entry info)
      (allocate-arguments info)
      (multiple-value-bind (arg-names stores value-names values binds)
			   (access-arguments info)
	`(progn
	   (compiler-let ((*alien-eval-when* '(compile eval)))
	     ,@(nreverse *output-forms*))
	   (defun ,(routine-info-function-name info) ,arg-names
	     ,(make-doc-string info value-names)
	     (alien-bind (,@(when (routine-info-arg-alien info)
			      `((args ,(routine-info-arg-alien info))))
			    ,@(when (routine-info-result-alien info)
				`((results ,(routine-info-result-alien info)))))
			 ,@stores
		(let ((return-value
		       (%primitive call-foreign
				   ,(routine-info-code info)
				   ,(if (routine-info-arg-alien info)
					'(alien-sap (alien-value args)) 0)
				   ,(truncate (+ (routine-info-arg-size info)
						 31) 32))))
		  return-value
		  (let* ,binds
		    (values
		     ,@(when (routine-info-return-type info)
			 `(,(coerce-from-integer (routine-info-return-type info)
						 'return-value)))
		     ,@values))))))))))


;;; Allocate-Argument, Allocate-Result  --  Internal
;;;
;;;    Allocate storage for an argument of the specified Type for the routine
;;; specified by Info.  Name is the name of the argument.  Stuff is pushed onto
;;; *Output-Forms* as needed.  Allocate-Result is the same except that it
;;; allocates stuff in the result Alien.
;;;
(proclaim '(ftype (function (routine-info c-type symbol) symbol)
		  allocate-argument allocate-result))
(defun allocate-argument (info type name)
  (multiple-value-bind (operator size)
		       (allocate-field (routine-info-arg-alien info)
				       (routine-info-arg-size info)
				       type name)
    (setf (routine-info-arg-size info) size)
    operator))
;;;
(defun allocate-result (info type name)
  (multiple-value-bind (operator size)
		       (allocate-field (routine-info-result-alien info)
				       (routine-info-result-size info)
				       type name)
    (setf (routine-info-result-size info) size)
    operator))

;;; Allocate-Field  --  Internal
;;;
;;;    Pad Size up to the next 32 bit boundry, then create an operator that
;;; accesses a field of the specified type.  We return the operator name and
;;; the new amount of stuff allocated.
;;;
(proclaim '(function allocate-field (symbol unsigned-byte c-type symbol)
		     (values symbol unsigned-byte)))
(defun allocate-field (alien size type name)
  (let ((base (align-offset size 32))
	(opname (symbolicate alien "-" name))
	(ctsize (c-type-size type)))
    (unless ctsize
      (error "Cannot pass variable size argument: ~S." type))
    (if (< ctsize 32)
	(incf base (- 32 ctsize)))
    (push `(defoperator (,opname ,(c-type-description type))
			((alien ,alien))
	     `(alien-index (alien-value ,alien) ,,base
			   ,,ctsize))
	  *output-forms*)
    (values opname (+ ctsize base))))


;;; Allocate-Arguments  --  Internal
;;;
;;;    Allocate operators and Aliens for the arguments and results.
;;;
(proclaim '(function allocate-arguments (routine-info) void))
(defun allocate-arguments (info)
  (let ((name (routine-info-function-name info)))
    (setf (routine-info-arg-alien info) (symbolicate name "-args"))
    (setf (routine-info-result-alien info) (symbolicate name "-results")))

  (dolist (arg (routine-info-args info))
    (let ((type (arg-info-type arg))
	  (name (arg-info-name arg)))
      (setf (arg-info-operator arg)
	    (allocate-argument info type name))
      (ecase (arg-info-mode arg)
	(:in)
	((:copy :in-out :out)
	 (unless (pointer-type-p type)
	   (error "~S argument ~S, has non-pointer type."
		  (arg-info-mode arg) name))
	 (setf (arg-info-result-operator arg)
	       (allocate-result info (pointer-type-to type) name))
	 (push `(setf (alien-access (,(arg-info-operator arg)
				     ,(routine-info-arg-alien info))
				    'system-area-pointer)
		      (alien-sap (,(arg-info-result-operator arg)
				  ,(routine-info-result-alien info))))
	       *output-forms*)))))

  (macrolet ((foo (s n)
	       `(cond ((zerop (,s info))
		       (setf (,n info) nil))
		      (t
		       (setq *output-forms*
			     (nconc *output-forms*
				    `((defalien ,(,n info) ,(,n info) ,(,s info)))))))))
    (foo routine-info-arg-size routine-info-arg-alien)
    (foo routine-info-result-size routine-info-result-alien)))


;;; Access-Arguments  --  Internal
;;;
;;;    Return stuff to access the argument in a call to the routine specified
;;; by Info.  Values:
;;;
;;; 1] A list of the input argument names.
;;; 2] A list of input arg storing forms.
;;; 3] A list of the names of the result values.
;;; 4] A list of result value forms.
;;; 5] A list of let* bindings to make around the value forms.
;;;
(proclaim '(function access-arguments (routine-info)
		     (values list list list list)))
(defun access-arguments (info)
  (let ((arg-names ())
	(stores ())
	(value-names ())
	(values ())
	(binds ()))
    (dolist (arg (routine-info-args info))
      (let ((mode (arg-info-mode arg)))
	(when (eq mode :in)
	  (multiple-value-bind (form names)
			       (access-one-value
				(arg-info-type arg)
				:write
				`(,(arg-info-operator arg) (alien-value args))
				(arg-info-name arg))
	    (setq arg-names (nconc arg-names names))
	    (setq stores (nconc stores (list form)))))

	(when (member mode '(:copy :in-out))
	  (multiple-value-bind (form names)
			       (access-one-value
				(pointer-type-to (arg-info-type arg))
				:write
				`(,(arg-info-result-operator arg) (alien-value results))
				(arg-info-name arg))
	    (setq arg-names (nconc arg-names names))
	    (setq stores (nconc stores (list form)))))

	(when (member mode '(:out :in-out))
	  (multiple-value-bind (forms names b)
			       (access-one-value
				(pointer-type-to (arg-info-type arg))
				:read
				`(,(arg-info-result-operator arg) (alien-value results))
				(arg-info-name arg))
	    (setq value-names (nconc value-names names))
	    (setq values (nconc values forms))
	    (setq binds (nconc binds b))))))

    (values arg-names stores value-names values binds)))


;;; Access-One-Value  --  Internal
;;;
;;;    Read or write an alien value that is described by a c-type.
;;; Type	- The C-Type of the field to be accessed.
;;; Kind	- :read or :write
;;; Alien	- The Alien expression for the place to access.
;;; Name	- The name of the field to access.  If :Write, this variable is
;;;		  bound to the value to store.
;;;
;;; Returns values:
;;;  1] If :read, a list of forms which are to be the values for the arg
;;;     If :write, a form which does the store.
;;;  2] A list of the names of the values produced or arguments used.  In
;;;     the :read case, this is really just documentation.
;;;  3] If :read, a list of let* binding forms to make around the code.
;;;
(proclaim '(function access-one-value (c-type (member :read :write) t symbol)
		     (values list list list)))
(defun access-one-value (type kind alien name)
  (typecase type
    (primitive-type
     (values (if (eq kind :read)
		 `((alien-access ,alien))
		 `(setf (alien-access ,alien) ,name))
	     `(,name)))
    (pointer-type
     (values (if (eq kind :read)
		 `((alien-access ,alien 'alien))
		 `(setf (alien-access ,alien 'system-area-pointer) ,name))
	     `(,name)))
    (t
     (values (if (eq kind :read)
		 `((copy-alien ,alien))
		 `(alien-assign ,alien ,name))
	     `(,name)))))


;;; Coerce-From-Integer  --  Internal
;;;
;;;    Return a form that converts a 32bit signed integer into the kind of
;;; object specified by Type.
;;;
(proclaim '(function coerce-from-integer (c-type t) t))
(defun coerce-from-integer (type value)
  (typecase type
    (primitive-type
     (let ((desc (c-type-description type)))
       (if (atom desc)
	   (case desc
	     (port value)
	     (boolean `(not (zerop ,value)))
	     (string-char `(code-char ,value))
	     (system-area-pointer `(int-sap ,value))
	     (short-float `(int-sap
			    (logior (ash ,value (- clc::short-float-shift-16))
				    (ash clc::short-float-4bit-type
					 (- 32 clc::short-float-shift-16)))))
	     (t
	      (error "Don't know how to hack ~S return type." desc)))
	   (case (first desc)
	     (signed-byte value)
	     (unsigned-byte
	      (if (> (second desc) 31)
		  `(ldb (byte 32 0) ,value)
		  value))
	     (enumeration
	      (let ((info (get (cadr desc) 'enumeration-info)))
		(when (null info)
		  (error "~S is not a defined enumeration." desc))
		(ecase (enumeration-info-kind info)
		  (:vector
		   `(svref ,(enumeration-info-to info)
			   (+ ,(enumeration-info-offset info) ,value)))
		  (`(cdr (assoc ,value) ,(enumeration-info-to info))))))
	     (t
	      (error "Don't know how to hack ~S return type." desc))))))
    (pointer-type
     (let ((to (pointer-type-to type)))
       (unless (c-type-size to)
	 (error "Cannot return pointer to unknown size object."))
       (let ((tds (c-type-description to)))
	 (if (or (eq tds 'null-terminated-string)
		 (and (listp tds) (eq (car (the list tds))
				      'null-terminated-string)))
	     `(if (eq ,value 0) NIL
		  (let ((av (lisp::make-alien-value (int-sap ,value) 0
						    ,(c-type-size to)
						    ',tds)))
		    (alien-bind ((s av ,tds))
		      (alien-access (alien-value s)))))
	     `(if (eq ,value 0) NIL
		  (lisp::make-alien-value (int-sap ,value) 0 ,(c-type-size to)
					  ',tds))))))
    (t
     (error "Don't know how to hack ~S return type." type))))


;;; Make-Doc-String  --  Internal
;;;
;;;    Make a doc string for the interface routine described by Info.  Values
;;; is a list of the names of the by-reference return values.
;;;
(proclaim '(function make-doc-string (routine-info list) string))
(defun make-doc-string (info values)
  (let ((*print-pretty* t)
	(*print-case* :downcase))
    (format nil "Interface to foreign routine ~S~:[; returns no values.~;~
	    ~:*, return values:~%  ~A~]"
	    (routine-info-name info)
	    (if (routine-info-return-type info)
		(cons 'return-value values)
		values))))

;;; Link-Routine-Entry  --  Internal
;;;
;;;    Emit stuff needed to look up a foreign routine and stash the code
;;; pointer in a variable.
;;;
(proclaim '(function link-routine-entry (routine-info) void))
(defun link-routine-entry (info)
  (let ((name (symbolicate (routine-info-function-name info) "-code"))
	(item (gensym)))
    (push `(defparameter ,name (get-code-pointer ,(routine-info-name info)))
	  *output-forms*)
    (push `(let ((,item (cons ',name ',(routine-info-name info))))
	     (when (not (member ,item foreign-routines-defined :test #'equal))
	       (push ,item foreign-routines-defined)))
	  *output-forms*)
    (setf (routine-info-code info) name)))


;;; Def-C-Variable defines a global C-Variable, so that it is available to
;;; Lisp.  It accepts the name of the variable (as a string) and the type
;;; of the variable.

(defmacro def-c-variable (name type)
  "Defines a foreign variable so that it is available from Lisp.
  Name should be a string with the name of the foreign variable and
  type is the foreign type of the variable."
  (let* ((symbol (intern (string-upcase name)))
	 (c-info (get-c-type type))
	 (c-type (if (primitive-type-p c-info)
		     (c-type-description c-info)
		     type))
	 (c-size (c-type-size c-info))
	 (item (gensym)))
    `(let ((,item (cons ',symbol ',name)))
       (when (not (member ,item foreign-variables-defined :test #'equal))
	 (push ,item foreign-variables-defined))
       (defalien ,symbol ,c-type ,c-size
	 (lisp::sap-int (get-data-pointer ,name))))))

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
