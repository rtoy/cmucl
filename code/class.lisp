;;; -*- Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/class.lisp,v 1.32 1993/08/30 14:53:51 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains structures and functions for the maintenance of basic
;;; information about defined types.  Different object systems can be supported
;;; simultaneously.  Some of the functions here are nominally generic, and are
;;; overwritten when CLOS is loaded.
;;;
(in-package "KERNEL")

(export '(layout layout-hash layout-hash-length layout-hash-max
		 initialize-layout-hash layout-class layout-invalid
		 layout-inherits layout-inheritance-depth layout-length
		 layout-info layout-pure
		 layout-of structure-class-p
		 slot-class-print-function
		 structure-class-make-load-form-fun find-layout
		 class-proper-name class-layout class-state
		 class-direct-superclasses class-subclasses
		 class-pcl-class class-init register-layout
		 basic-structure-class slot-class funcallable-instance
		 funcallable-structure-class
		 make-funcallable-structure-class
		 funcallable-structure-class-p make-standard-class
		 random-pcl-class make-random-pcl-class
		 built-in-class-direct-superclasses
		 find-class-cell class-cell-name class-cell-class
		 make-layout make-undefined-class insured-find-class
		 redefine-layout-warning))

(in-package "LISP")
(export '(class structure-class standard-class class-name find-class class-of
		built-in-class))

(in-package "KERNEL")

(with-cold-load-init-forms)

;;; Table mapping class names to layouts for classes we have referenced but not
;;; yet loaded.  This is initialized from an ALIST created by Genesis
;;; describing the layouts it created at cold-load time.  *LAYOUT-HASH-INITS*
;;; is a list of all the layouts whose hash hasn't been initialized.  We delay
;;; this because the random-number generator needs top-level forms to work.
;;;
(defvar *forward-referenced-layouts*)
(defvar lisp::*initial-layouts*)
(defvar *layout-hash-inits*)
(cold-load-init
  (setq *forward-referenced-layouts* (make-hash-table :test #'equal))
  (setq *layout-hash-inits* ())
  (dolist (x lisp::*initial-layouts*)
    (setf (gethash (car x) *forward-referenced-layouts*) (cdr x))
    (push (cdr x) *layout-hash-inits*))
  (makunbound 'lisp::*initial-layouts*))


;;;; Class definition structures:

;;; The LAYOUT structure is pointed to by the first cell of instance (or
;;; structure) objects.  It represents what we need to know for type checking
;;; and garbage collection.  Whenever a class is incompatibly redefined, a new
;;; layout is allocated.  If two object's layouts are EQ, then they are exactly
;;; the same type.
;;;
;;; LAYOUTs are treated specially by the dumper so that genesis can help us
;;; bootstrap the type system.  Only layouts for named classes can be dumped.
;;; This is resolved at load-time to the current layout for the class of that
;;; name --- except that genesis simply ensures that only one layout is
;;; allocated for each class (interns the layouts by name.)  Only the INHERITS,
;;; INHERITANCE-DEPTH and LENGTH slots are dumped.  In normal load, these slots
;;; had better agree with any current loaded value.  In cold load, these slots
;;; are used to create the LAYOUT if it doesn't exist yet (other slots left
;;; unitialized.)
;;;
(defstruct (layout (:print-function
		    (lambda (s stream d)
		      (declare (ignore d))
		      (print-unreadable-object (s stream :identity t)
			(format stream "Layout for ~S~@[, Invalid=~S~]"
				(class-proper-name (layout-class s))
				(layout-invalid s)))))
		   (:make-load-form-fun :ignore-it)
		   (:constructor %make-layout))
  ;;
  ;; Some hash bits for this layout.  Sleazily accessed via %INSTANCE-REF, see
  ;; LAYOUT-HASH.
  (hash0 0 :type index)
  (hash1 0 :type index)
  (hash2 0 :type index)
  (hash3 0 :type index)
  (hash4 0 :type index)
  (hash5 0 :type index)
  (hash6 0 :type index)
  (hash7 0 :type index)
  ;;
  ;; The class this is a layout for.
  (class (required-argument) :type class)
  ;;
  ;; NIL if this is the latest layout for this class.  If non-null, then the
  ;; class was changed after this instance was created.  The exact value may
  ;; provide some information about what to do.
  (invalid nil)
  ;;
  ;; Vector of the layouts for all classes we inherit.  If hierarchical
  ;; these are in order from most general down to (but not including) this
  ;; class.
  (inherits #() :type simple-vector)
  ;;
  ;; Number of classes this class hierachically inherits
  ;; (length inherits), or -1 if not hierarchical.
  (inheritance-depth -1 :type (or index (integer -1 -1)))
  ;;
  ;; The number of top-level descriptor cells in each instance.
  (length 0 :type index)
  ;;
  ;; If this layout has some kind of compiler meta-info, then this is it.  If a
  ;; structure, then we store the DEFSTRUCT-DESCRIPTION here.
  (info nil)
  ;;
  ;; True if objects of this class are never modified to contain dynamic
  ;; pointers in their slots or constant-like substructure (hence can be copied
  ;; into read-only space by purify.)
  ;;
  ;; ### this slot is known to the C startup code.
  (pure nil :type boolean))

;;; MAKE-LAYOUT  --  Interface
;;;
;;;    Make a layout and initialize it.  If type system is initialized
;;; (actually, the top-level forms for RANDOM run), then initialize the hash
;;; now, otherwise, delay initialization.
;;;
(defun make-layout (&rest args &key &allow-other-keys)
  (let ((res (apply #'%make-layout args)))
    (if *type-system-initialized*
	(initialize-layout-hash res)
	(push res *layout-hash-inits*))
    res))

(defconstant layout-hash-length 8)
(declaim (inline layout-hash))
(defun layout-hash (layout i)
  (declare (type layout layout) (type index i))
  (truly-the index (%instance-ref layout (1+ i))))
(declaim (inline (setf layout-hash)))
(defun (setf layout-hash) (new-value layout i)
  (declare (type layout layout) (type index new-value i))
  (setf (%instance-ref layout (1+ i)) new-value))

(defconstant layout-hash-max (ash most-positive-fixnum -3)
  "The inclusive upper bound on LAYOUT-HASH values.")

(defvar *layout-hash-seed* nil)

;;; INITIALIZE-LAYOUT-HASH  --  Interface
;;;
;;;    Set the layout-hash slots to non-zero random numbers between 1 and
;;; layout-hash-max (inclusive.)  The layout is returned.
;;;
(defun initialize-layout-hash (layout)
  (let ((seed *layout-hash-seed*))
    (unless seed
      (setq seed (setq *layout-hash-seed* (make-random-state))))
    (dotimes (i layout-hash-length)
      (setf (layout-hash layout i)
	    (1+ (random layout-hash-max seed)))))
  layout)


;;; The CLASS structure is a supertype of all CLASS types.  A CLASS is also a
;;; CTYPE structure as recognized by the type system.
;;;
(defstruct (class
	    (:make-load-form-fun class-make-load-form-fun)
	    (:print-function %print-class)
	    (:include ctype
		      (:class-info (type-class-or-lose 'class)))
	    (:pure nil))
  ;;
  ;; Optional name, for printing.
  (name nil)
  ;;
  ;; Current layout for this class.  Null if not assigned yet.
  (layout nil :type (or layout null))
  ;;
  ;; How sure we are that this class won't be redefined.  If :READ-ONLY, we are
  ;; committed to not changing the effective slots or superclasses.  If
  ;; :SEALED, we can't even add subclasses.
  (state nil :type (member nil :read-only :sealed))
  ;;
  ;; Direct superclasses of this class.
  (direct-superclasses () :type list)
  ;;
  ;; Representation of all of the subclasses (direct or indirect) of this
  ;; class.  NIL if no subclasses or not initalized yet.  Otherwise, an EQ
  ;; hash-table mapping class-objects to the subclass layout that was in effect
  ;; at the time the subclass was created.
  (subclasses nil :type (or hash-table null))
  ;;
  ;; The PCL class object, or NIL if none assigned yet.
  (pcl-class nil))
;;;
(defun class-make-load-form-fun (class)
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (error
       "Can't use anonymous or undefined class as constant:~%  ~S"
       class))
    `(find-class ',name)))
;;;
(defun %print-class (s stream d)
  (declare (ignore d))
  (print-unreadable-object (s stream :identity t :type t)
    (format stream "~:[<anonymous>~;~:*~S~]~@[ (~(~A~))~]"
	    (class-name s) (class-state s))))


;;; The UNDEFINED-CLASS is a cookie we make up to stick in forward referenced
;;; layouts.  Users should never see them.
;;;
(defstruct (undefined-class
	    (:include class)
	    (:constructor make-undefined-class (name))))


;;; BUILT-IN-CLASS is used to represent the standard classes that aren't
;;; defined with DEFSTRUCT and other specially implemented primitve types whose
;;; only attribute is their name.
;;;
;;; Some BUILT-IN-CLASSes have a TRANSLATION, which means that they are
;;; effectively DEFTYPE'd to some other type (usually a union of other classes
;;; or a "primitive" type such as NUMBER, ARRAY, etc.)  This translation is
;;; done when type specifiers are parsed.  Type system operations (union,
;;; subtypep, etc.) should never encounter translated classes, only their
;;; translation.
;;;
(defstruct (built-in-class (:include class))
  ;;
  ;; Type we translate to on parsing.  If NIL, then this class stands on its
  ;; own.  Only :INITIALIZING during for a period during cold-load.  See below.
  (translation nil :type (or ctype (member nil :initializing))))

;;; Class with print function, but not necessarily a structure class.
;;; (CONDITIONs)
;;;
(defstruct (slot-class (:include class))
  ;;
  ;; Print function, or NIL if none.
  (print-function nil :type (or function symbol null)))

;;; STRUCTURE-CLASS represents what we need to know about structure classes.
;;; Non-structure "typed" defstructs are a special case, and don't have a
;;; corresponding class.
;;;
(defstruct (basic-structure-class (:include slot-class)))

(defstruct (structure-class (:include basic-structure-class))
  ;;
  ;; MAKE-LOAD-FORM method, or NIL if none. :J-D-I-N dumps the slots.
  ;; :IGNORE-IT is used for magic structures which the compiler inserts in IR1,
  ;; but that are never actually dumped.
  (make-load-form-fun nil :type (or function symbol
				    (member :just-dump-it-normally
					    :ignore-it
					    nil)))
  ;;
  ;; If true, a default keyword constructor for this structure.
  (constructor nil :type (or function null)))

;;; FUNCALLABLE-STRUCTURE-CLASS is used to represent funcallable structures,
;;; which are used to implement generic functions.
;;;
(defstruct (funcallable-structure-class (:include basic-structure-class)))


;;;; Class namespace:

;;; FIND-CLASS-CELL, CLASS-CELL-NAME, CLASS-CELL-CLASS  --  Interface
;;;
;;;    We use an indirection to allow forward referencing of class definitions
;;; with load-time resolution.
;;;
(defstruct (class-cell
	    (:constructor make-class-cell (name &optional class))
	    (:print-function
	     (lambda (s stream d)
	       (declare (ignore d))
	       (print-unreadable-object (s stream :type t) 
		 (prin1 (class-cell-name s) stream))))
	    (:make-load-form-fun
	     (lambda (cell)
	       `(find-class-cell ',(class-cell-name cell)))))
  ;;
  ;; Name of class we expect to find.
  (name nil :type symbol :read-only t)
  ;;
  ;; Class or NIL if not yet defined.
  (class nil :type (or class null)))

(defun find-class-cell (name)
  (or (info type class name)
      (setf (info type class name) (make-class-cell name))))


;;; FIND-CLASS  --  Public
;;;
(defun find-class (name &optional (errorp t) environment)
  "Return the class with the specified Name.  If ERRORP is false, then NIL is
   returned when no such class exists."
  (declare (type symbol name) (ignore environment))
  (let ((res (class-cell-class (find-class-cell name))))
    (if (or res (not errorp))
	res
	(error "Class not yet defined:~%  ~S" name))))
;;;
(defun (setf find-class) (new-value name)
  (declare (type class new-value))
  (ecase (info type kind name)
    ((nil))
    (:instance
     (let ((old (class-of (find-class name)))
	   (new (class-of new-value)))
       (unless (eq old new)
	 (warn "Changing meta-class of ~S from ~S to ~S."
	       name (class-name old) (class-name new)))))
    (:primitive
     (error "Illegal to redefine standard type ~S." name))
    (:defined
     (warn "Redefining DEFTYPE type to be a class: ~S."
	   name)
     (setf (info type expander name) nil)))

  (remhash name *forward-referenced-layouts*)
  (%note-type-defined name)
  (setf (info type kind name) :instance)
  (setf (class-cell-class (find-class-cell name)) new-value)
  (unless (eq (info type compiler-layout name)
	      (class-layout new-value))
    (setf (info type compiler-layout name) (class-layout new-value)))
  new-value)


;;; INSURED-FIND-CLASS  --  Interface
;;;
;;;    Called when we are about to define Name as a class meeting some
;;; predicate (such as a meta-class type test.)  The first result is always of
;;; the desired class.  The second result is any existing layout for this name.
;;;
(defun insured-find-class (name predicate constructor)
  (declare (function predicate constructor))
  (let* ((old (find-class name nil))
	 (res (if (and old (funcall predicate old))
		  old
		  (funcall constructor :name name)))
	 (found (or (gethash name *forward-referenced-layouts*)
		    (when old (class-layout old)))))
    (when found
      (setf (layout-class found) res))

    (values res found)))


;;; CLASS-PROPER-NAME  --  Exported
;;;
;;;    If the class has a proper name, return the name, otherwise return
;;; the class.
;;;
(defun class-proper-name (class)
  (declare (type class class))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name nil) class))
	name
	class)))


;;;; CLASS type operations:

(define-type-class class)

;;; Simple methods for TYPE= and SUBTYPEP should never be called when the two
;;; classes are equal, since there are EQ checks in those operations.
;;;
(define-type-method (class :simple-=) (type1 type2)
  (assert (not (eq type1 type2)))
  (values nil t))

(define-type-method (class :simple-subtypep) (class1 class2)
  (assert (not (eq class1 class2)))
  (let ((subclasses (class-subclasses class2)))
    (if (and subclasses (gethash class1 subclasses))
	(values t t)
	(values nil t))))


;;; SEALED-CLASS-INTERSECTION  --  Internal
;;;
;;;    When finding the intersection of a sealed class and some other class
;;; (not hierarchically related) the intersection is the union of the currently
;;; shared subclasses.
;;;
(defun sealed-class-intersection (sealed other)
  (declare (type class sealed other))
  (let ((s-sub (class-subclasses sealed))
	(o-sub (class-subclasses other)))
    (if (and s-sub o-sub)
	(collect ((res *empty-type* type-union))
	  (do-hash (subclass layout s-sub)
	    (declare (ignore layout))
	    (when (gethash subclass o-sub)
	      (res (specifier-type subclass))))
	  (values (res) t))
	(values *empty-type* t))))

    
;;; If one is a subclass of the other, then that is the intersection, but we
;;; can only be sure the intersection is otherwise empty if they are structure
;;; classes, since a subclass of both might be defined.  If either class is
;;; sealed, we can eliminate this possibility.
;;;
(define-type-method (class :simple-intersection) (class1 class2)
  (declare (type class class1 class2))
  (cond ((eq class1 class2) class1)
	((let ((subclasses (class-subclasses class2)))
	   (and subclasses (gethash class1 subclasses)))
	 (values class1 t))
	((let ((subclasses (class-subclasses class1)))
	   (and subclasses (gethash class2 subclasses)))
	 (values class2 t))
	((or (basic-structure-class-p class1)
	     (basic-structure-class-p class2))
	 (values *empty-type* t))
	((eq (class-state class1) :sealed)
	 (sealed-class-intersection class1 class2))
	((eq (class-state class2) :sealed)
	 (sealed-class-intersection class2 class1))
	(t
	 (values class1 nil))))

(define-type-method (class :unparse) (type)
  (class-proper-name type))


;;;; Built-in classes & class-of:
;;;
;;;    The BUILT-IN-CLASSES list is a data structure which configures the
;;; creation of all the built-in classes.  It contains all the info that we
;;; need to maintain the mapping between classes, compile-time types and
;;; run-time type codes.  These options are defined:
;;;
;;; :TRANSLATION (default none)
;;;     When this class is "parsed" as a type specifier, it is translated into
;;;     the specified internal type representation, rather than being left as a
;;;     class.  This is used for types which we want to canonicalize to
;;;     some other kind of type object because in general we want to be able to
;;;     include more information than just the class (e.g. for numeric types.)
;;;     Default none.
;;;
;;; :ENUMERABLE (default NIL)
;;;     The value of the :ENUMERABLE slot in the created class.  Meaningless in
;;;     translated classes.
;;;
;;; :STATE (default :SEALED)
;;;     The value of CLASS-STATE which we want on completion, indicating
;;;     whether subclasses can be created at run-time.
;;;
;;; :HIERARCHICAL (default T unless any of the inherits are non-hierarchical)
;;;     True if we can assign this class a unique INHERITANCE-DEPTH.
;;;
;;; :CODES (default none)
;;;     Run-time type codes which should be translated back to this class by
;;;     CLASS-OF.  Unspecified for abstract classes.
;;;
;;; :INHERITS (default this class & T)
;;;     The class-precedence list for this class, with this class and T
;;;     implicit.
;;;
;;; :DIRECT-SUPERCLASSES (default to head of CPL)
;;;     List of the direct superclasses of this class.
;;; 

(defvar built-in-classes)
(cold-load-init
  (setq built-in-classes
	'((t :state :read-only :translation t)
	  (character :enumerable t :translation base-char)
	  (base-char :enumerable t :inherits (character)
		     :codes (#.vm:base-char-type))
	  
	  (symbol :codes (#.vm:symbol-header-type))
	  
	  (instance :state :read-only)
	  
	  (system-area-pointer :codes (#.vm:sap-type))
	  (weak-pointer :codes (#.vm:weak-pointer-type))
	  (scavenger-hook #+gengc :codes #+gengc (#.vm:scavenger-hook-type))
	  (code-component :codes (#.vm:code-header-type))
	  #-gengc (lra :codes (#.vm:return-pc-header-type))
	  (fdefn :codes (#.vm:fdefn-type))
	  (random-class) ; Used for unknown type codes.
	  
	  (function
	   :codes
	   (#.vm:byte-code-closure-type
	    #.vm:byte-code-function-type
	    #.vm:closure-header-type  #.vm:function-header-type)
	   :state :read-only)
	  (funcallable-instance :inherits (function)  :state :read-only)
	  
	  (collection :hierarchical nil  :state :read-only)
	  (explicit-key-collection :state :read-only  :inherits (collection))
	  (mutable-collection :state :read-only  :inherits (collection))
	  (mutable-explicit-key-collection
	   :state :read-only
	   :direct-superclasses (explicit-key-collection mutable-collection)
	   :inherits (explicit-key-collection mutable-collection collection))
	  (generic-sequence :state :read-only  :inherits (collection))
	  (mutable-sequence
	   :state :read-only
	   :direct-superclasses (mutable-collection generic-sequence)
	   :inherits (mutable-collection generic-sequence collection))
	  (generic-array
	   :state :read-only
	   :inherits (mutable-sequence mutable-collection generic-sequence
		      collection))
	  (generic-vector
	   :state :read-only
	   :inherits (generic-array mutable-sequence mutable-collection
		      generic-sequence collection))
	  (array
	   :translation array :codes (#.vm:complex-array-type)
	   :inherits (generic-array mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array
	   :translation simple-array  :codes (#.vm:simple-array-type)
	   :inherits (array generic-array mutable-sequence mutable-collection
		      generic-sequence collection))
	  (sequence
	   :translation (or cons (member nil) vector)
	   :inherits (mutable-sequence mutable-collection generic-sequence
		      collection))
	  (vector
	   :translation vector  :codes (#.vm:complex-vector-type)
	   :direct-superclasses (generic-vector array sequence)
	   :inherits (generic-vector array generic-array sequence
		      mutable-sequence mutable-collection generic-sequence
		      collection))
	  (simple-vector
	   :translation simple-vector  :codes (#.vm:simple-vector-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (bit-vector
	   :translation bit-vector  :codes (#.vm:complex-bit-vector-type)
	   :inherits (vector generic-vector array generic-array sequence
		      mutable-sequence mutable-collection generic-sequence
		      collection))
	  (simple-bit-vector
	   :translation simple-bit-vector  :codes (#.vm:simple-bit-vector-type)
	   :direct-superclasses (bit-vector simple-array)
	   :inherits (bit-vector vector generic-vector simple-array array
		      generic-array sequence mutable-sequence
		      mutable-collection generic-sequence collection))
	  (simple-array-unsigned-byte-2
	   :translation (simple-array (unsigned-byte 2) (*))
	   :codes (#.vm:simple-array-unsigned-byte-2-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-unsigned-byte-4
	   :translation (simple-array (unsigned-byte 4) (*))
	   :codes (#.vm:simple-array-unsigned-byte-4-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-unsigned-byte-8
	   :translation (simple-array (unsigned-byte 8) (*))
	   :codes (#.vm:simple-array-unsigned-byte-8-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-unsigned-byte-16
	   :translation (simple-array (unsigned-byte 16) (*))
	   :codes (#.vm:simple-array-unsigned-byte-16-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-unsigned-byte-32
	   :translation (simple-array (unsigned-byte 32) (*))
	   :codes (#.vm:simple-array-unsigned-byte-32-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-single-float
	   :translation (simple-array single-float (*))
	   :codes (#.vm:simple-array-single-float-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-array-double-float
	   :translation (simple-array double-float (*))
	   :codes (#.vm:simple-array-double-float-type)
	   :direct-superclasses (vector simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence mutable-sequence mutable-collection
		      generic-sequence collection))
	  (generic-string
	   :state :read-only
	   :inherits (mutable-sequence mutable-collection generic-sequence
		      collection))
	  (string
	   :translation string  :codes (#.vm:complex-string-type)
	   :direct-superclasses (vector generic-string)
	   :inherits (vector generic-vector array generic-array sequence
		      generic-string mutable-sequence mutable-collection
		      generic-sequence collection))
	  (simple-string
	   :translation simple-string  :codes (#.vm:simple-string-type)
	   :direct-superclasses (string simple-array)
	   :inherits (vector generic-vector simple-array array generic-array
		      sequence generic-string mutable-sequence
		      mutable-collection generic-sequence collection))

	  (generic-number :state :read-only)
	  (number :translation number :inherits (generic-number))
	  (complex :translation complex :inherits (number generic-number)
		   :codes (#.vm:complex-type))
	  (real :translation real :inherits (number generic-number))
	  (float :translation float :inherits (real number generic-number))
	  (single-float
	   :translation single-float
	   :inherits (float real number generic-number)
	   :codes (#.vm:single-float-type))
	  (double-float
	   :translation double-float
	   :inherits (float real number generic-number)
	   :codes (#.vm:double-float-type))
	  (rational
	   :translation rational
	   :inherits (real number generic-number))
	  (ratio
	   :translation (and rational (not integer))
	   :inherits (rational real number generic-number)
	   :codes (#.vm:ratio-type))
	  (integer
	   :translation integer
	   :inherits (rational real number generic-number))
	  (fixnum
	   :translation (integer #.vm:target-most-negative-fixnum
				 #.vm:target-most-positive-fixnum)
	   :inherits (integer rational real number generic-number)
	   :codes (#.vm:even-fixnum-type #.vm:odd-fixnum-type))
	  (bignum
	   :translation (and integer (not fixnum))
	   :inherits (integer rational real number generic-number)
	   :codes (#.vm:bignum-type))
	  
	  (list :translation (or cons (member nil))
		:inherits (sequence mutable-sequence mutable-collection
			   generic-sequence collection))
	  (cons :codes (#.vm:list-pointer-type)
		:inherits (list sequence mutable-sequence mutable-collection
			   generic-sequence collection))
	  (null :translation (member nil)
		:inherits (list sequence mutable-sequence mutable-collection
			   generic-sequence collection symbol)
		:direct-superclasses (list symbol)))))

;;; See also type-init.lisp where we finish setting up the translations for
;;; built-in types.
;;;
(cold-load-init
  (dolist (x built-in-classes)
    (destructuring-bind (name &key (translation nil trans-p) inherits codes
			      enumerable state (hierarchical t)
			      (direct-superclasses
			       (if inherits (list (car inherits)) '(t))))
			x
      (declare (ignore codes state translation))
      (let ((inherits (if (eq name 't)
			  ()
			  (cons 't (reverse inherits))))
	    (class (make-built-in-class
		    :enumerable enumerable
		    :name name
		    :translation (if trans-p :initializing nil)
		    :direct-superclasses
		    (if (eq name 't)
			()
			(mapcar #'find-class direct-superclasses)))))
	(setf (info type kind name) :primitive)
	(setf (class-cell-class (find-class-cell name)) class)
	(unless trans-p
	  (setf (info type builtin name) class))
	(let* ((inheritance-depth (if hierarchical (length inherits) -1))
	       (inherit-layouts
		(map 'vector
		     #'(lambda (x)
			 (let ((super-layout (class-layout (find-class x))))
			   (when (= (layout-inheritance-depth super-layout) -1)
			     (setf inheritance-depth -1))
			   super-layout))
		     inherits)))
	  (register-layout
	   (find-layout name 0 inherit-layouts inheritance-depth)
	   :invalidate nil))))))


;;; Now that we have set up the class heterarchy, seal the sealed classes.
;;; This must be done after the subclasses have been set up.
;;;
(cold-load-init
  (dolist (x built-in-classes)
    (destructuring-bind (name &key (state :sealed) &allow-other-keys) x
      (setf (class-state (find-class name)) state))))


;;; A vector that maps type codes to layouts, used for quickly finding the
;;; layouts of built-in classes.
;;;
(defvar built-in-class-codes)
(cold-load-init
  (setq built-in-class-codes
	(let ((res (make-array 256 :initial-element
			       '#.(class-layout (find-class 'random-class)))))
	  (dolist (x built-in-classes res)
	    (destructuring-bind (name &key codes &allow-other-keys)
				x
	      (let ((layout (class-layout (find-class name))))
		(dolist (code codes)
		  (setf (svref res code) layout))))))))


;;; LAYOUT-OF  --  Exported
;;;
;;;    Return the layout for an object.  This is the basic operation for
;;; finding out the "type" of an object, and is used for generic function
;;; dispatch.  The standard doesn't seem to say as much as it should about what
;;; this returns for built-in objects.  For example, it seems that we must
;;; return NULL rather than LIST when X is NIL so that GF's can specialize on
;;; NULL.
;;;
(declaim (inline layout-of))
(defun layout-of (x)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((%instancep x) (%instance-layout x))
	((funcallable-instance-p x) (%funcallable-instance-layout x))
	((null x) '#.(class-layout (find-class 'null)))
	(t (svref built-in-class-codes (get-type x)))))


;;; CLASS-OF  --  Public
;;;
(declaim (inline class-of))
(defun class-of (object)
  "Return the class of the supplied object, which may be any Lisp object, not
   just a CLOS STANDARD-OBJECT."
  (layout-class (layout-of object)))


;;;; Class definition/redefinition:

;;; MODIFY-CLASS  --  Internal
;;;
;;;    Called whenever we are altering a class.  Clear type system caches and
;;; warn if read-only.
;;;
(defun modify-class (class)
  (clear-type-caches)
  (when (member (class-state class) '(:read-only :frozen))
    (warn "Modifing ~(~A~) class ~S; making it writable."
	  (class-state class) (class-name class))
    (setf (class-state class) nil)))


;;; INVALIDATE-LAYOUT  --  Internal
;;;
;;;    Mark a layout as invalid.  Depth -1 causes unsafe structure type tests
;;; to fail.  Remove class from all superclasses (might not be registered, so
;;; might not be in subclasses of the nominal superclasses.)
;;;
(defun invalidate-layout (layout)
  (declare (type layout layout))
  (setf (layout-invalid layout) :invalid)
  (setf (layout-inheritance-depth layout) -1)
  (let ((inherits (layout-inherits layout))
	(class (layout-class layout)))
    (modify-class class)
    (dotimes (i (length inherits))
      (let* ((super (svref inherits i))
	     (subs (class-subclasses (layout-class super))))
	(when subs
	  (remhash class subs)))))
  (undefined-value))


;;; REGISTER-LAYOUT  --  Interface
;;;
;;;    Record Layout as the layout for its class, adding it as a subtype of all
;;; superclasses.  This is the operation that "installs" a layout for a class
;;; in the type system, clobbering any old layout.  However, this does not
;;; modify the class namespace; that is a separate operation (think anonymous
;;; classes.)
;;; -- If INVALIDATE, then all the layouts for any old definition
;;;    and subclasses are invalidated, and the SUBCLASSES slot is cleared.
;;  -- If DESTRUCT-LAYOUT, then this is some old layout, and is to be
;;;    destructively modified to hold the same type information.
;;;
(defun register-layout (layout &key (invalidate t) destruct-layout)
  (declare (type layout layout) (type (or layout null) destruct-layout))
  (let* ((class (layout-class layout))
	 (class-layout (class-layout class))
	 (subclasses (class-subclasses class)))
    (assert (not (eq class-layout layout)))
    (when class-layout
      (modify-class class)
      (when subclasses
	(do-hash (c l subclasses)
	  (modify-class c)
	  (when invalidate (invalidate-layout l))))
      (when invalidate
	(invalidate-layout class-layout)
	(setf (class-subclasses class) nil)))
    
    (cond (destruct-layout
	   (setf (layout-invalid destruct-layout) nil)
	   (setf (layout-inherits destruct-layout) (layout-inherits layout))
	   (setf (layout-inheritance-depth destruct-layout)
		 (layout-inheritance-depth layout))
	   (setf (layout-length destruct-layout) (layout-length layout))
	   (setf (layout-info destruct-layout) (layout-info layout))
	   (setf (class-layout class) destruct-layout))
	  (t
	   (setf (layout-invalid layout) nil)
	   (setf (class-layout class) layout)))

    (let ((inherits (layout-inherits layout)))
      (dotimes (i (length inherits))
	(let* ((super (layout-class (svref inherits i)))
	       (subclasses (or (class-subclasses super)
			       (setf (class-subclasses super)
				     (make-hash-table :test #'eq)))))
	  (when (and (eq (class-state super) :sealed)
		     (not (gethash class subclasses)))
	    (warn "Subclassing sealed class ~S; unsealing it."
		  (class-name super))
	    (setf (class-state super) :read-only))
	  (setf (gethash class subclasses)
		(or destruct-layout layout))))))

    (undefined-value))


;;; LAYOUT-PROPER-NAME  --  Internal
;;;
(defun layout-proper-name (layout)
  (class-proper-name (layout-class layout)))


;;; REDEFINE-LAYOUT-WARNING  --  Interface
;;;
;;;    If layouts Old and New differ in any interesting way, then give a
;;; warning and return T.
;;;
(defun redefine-layout-warning (old old-context new new-context)
  (declare (type layout old new) (simple-string old-context new-context))
  (when (typep (layout-class old) 'undefined-class)
    (setf (layout-class old) (layout-class new)))
  (assert (eq (layout-class old) (layout-class new)))
  (let ((name (layout-proper-name old)))
    (or (let ((oldi (layout-inherits old))
	      (newi (layout-inherits new)))
	  (or (when (mismatch oldi newi :key #'layout-proper-name)
		(warn
		 "Change in superclasses of class ~S:~%  ~
		  ~A superclasses: ~S~%  ~
		  ~A superclasses: ~S"
		 name
		 old-context (map 'list #'layout-proper-name oldi)
		 new-context (map 'list #'layout-proper-name newi))
		t)
	      (let ((diff (mismatch oldi newi)))
		(when diff
		  (warn
		   "In class ~S:~%  ~
		    ~:(~A~) definition of superclass ~S incompatible with~%  ~
		    ~A definition."
		   name old-context (layout-proper-name (svref oldi diff))
		   new-context)
		  t))))
	(let ((old-len (layout-length old))
	      (new-len (layout-length new)))
	  (unless (= old-len new-len)
	    (warn "Change in instance length of class ~S:~%  ~
		   ~A length: ~D~%  ~
		   ~A length: ~D"
		  name
		  old-context old-len
		  new-context new-len)
	    t))
	(when (/= (layout-inheritance-depth old)
		  (layout-inheritance-depth new))
	  (warn "Change in the inheritance structure of class ~S~%  ~
		 between the ~A definition and the ~A definition."
		name old-context new-context)
	  t))))


;;; FIND-LAYOUT  --  Interface
;;;
;;;    Used by the loader to forward-reference layouts for classes whose
;;; definitions may not have been loaded yet.  This allows type tests to be
;;; loaded when the type definition hasn't been loaded yet.  Name is the class
;;; name, Length is the length of instances, Inherits is a simple-vector of the
;;; layouts for the classes it inherits, and Depth is the Inheritance-Depth.
;;;
;;;    If we can't find any existing layout, then we create a new one with the
;;; supplied information, storing it in *FORWARD-REFERENCED-LAYOUTS*.  If we
;;; can find the layout, then return it, after checking for compatibility.  If
;;; incompatible, we allow the layout to be replaced, altered or left alone.
;;;
(defun find-layout (name length inherits depth)
  (declare (type index length) (simple-vector inherits)
	   (type (or index (integer -1 -1)) depth))
  (let* ((class (or (find-class name nil)
		    (make-undefined-class name)))
	 (old (or (class-layout class)
		  (gethash name *forward-referenced-layouts*)))
	 (res (make-layout :class class
			   :invalid :undefined
			   :inherits inherits
			   :inheritance-depth depth
			   :length length)))
    (cond ((not old)
	   (setf (gethash name *forward-referenced-layouts*) res))
	  ((not *type-system-initialized*)
	   (setf (layout-class old) class)
	   old)
	  ((redefine-layout-warning old "current" res "compile time")
	   (restart-case
	       (error "Loading a reference to class ~S when the compile~
		       ~%  time definition was incompatible with the current ~
		       one."
		      name)
	     (continue ()
	       :report "Invalidate current definition."
	       (warn "New definition of ~S must be loaded eventually." name)
	       (invalidate-layout old)
	       (setf (gethash name *forward-referenced-layouts*) res))
	     (clobber-it ()
	       :report "Smash current layout, preserving old code."
	       (warn "Any old ~S instances will be in a bad way.~@
		      I hope you know what you're doing..."
		     name)
	       (setf (layout-inherits old) inherits)
	       (setf (layout-inheritance-depth old) depth)
	       (setf (layout-length old) length)
	       old)
	     (use-current ()
	       :report "Ignore the incompatibility, leave class alone."
	       (warn "Assuming the current definition of ~S is correct, and~@
		      that the loaded code doesn't care about the ~
		      incompatibility."
		     name)
	       old)))
	  (t old))))


;;; PCL stuff:

(defstruct (std-class (:include class)))
(defstruct (standard-class (:include std-class)))
(defstruct (random-pcl-class (:include std-class)))


;;;; Cold loading initializations.

(defun class-finalize ()
  (dolist (layout *layout-hash-inits*)
    (initialize-layout-hash layout))
  (makunbound '*layout-hash-inits*)
  (do-hash (name layout *forward-referenced-layouts*)
    (let ((class (find-class name nil)))
      (cond ((not class)
	     (setf (layout-class layout) (make-undefined-class name)))
	    ((eq (class-layout class) layout)
	     (remhash name *forward-referenced-layouts*))
	    (t
	     (warn "Something strange with forward layout for ~S:~%  ~S"
		   name layout))))))

(emit-cold-load-defuns "CLASS")
