;;; -*- Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/class.lisp,v 1.1 1993/01/15 15:26:33 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains structures and functions for the maintenance of basic
;;; information about defined types.  Different object systems can be supported
;;; simultaneously.  Some of the functions here are nominally generic, and are
;;; overwritten when CLOS is loaded.
;;;
(in-package "KERNEL")

(export '(layout layout-hash layout-hash-length layout-class layout-invalid
		 layout-inherits layout-inheritance-depth layout-length
		 structure-class-print-function
		 structure-class-make-load-form-fun find-layout))

(in-package "LISP")
(export '(class structure-class class-name find-class))

(in-package "KERNEL")

;;; Table mapping class names to layouts for classes we have referenced but not
;;; yet loaded.
;;; 
;;; ### Initialize to list of all layouts created by genesis.  Removed when we
;;; actually see the definition in top-level code.
;;; 
(defvar *forward-referenced-layouts* (make-hash-table :test #'equal))


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
(defstruct (layout (:print-function %print-layout)
		   (:make-load-form-fun :ignore-it))
  ;;
  ;; Some hash bits for this layout.  Sleazily accessed via STRUCTURE-REF, see
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
  (info nil))

(defconstant layout-hash-length 8)
(declaim (inline layout-hash))
(defun layout-hash (layout i)
  (declare (type layout layout) (type index i))
  (truly-the index (structure-ref layout (1+ i))))


;;; The CLASS structure is a supertype of all CLASS types.  A CLASS is also a
;;; CTYPE structure as recognized by the type system.
;;;
(defstruct (class
	    (:make-load-form-fun class-make-load-form-fun)
	    (:include ctype
		      (:class-info (type-class-or-lose 'class))))
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
  ;; Representation of all of the subclasses (direct or indirect) of this
  ;; class.  NIL if no subclasses or not initalized yet.  Otherwise, an EQ
  ;; hash-table mapping class-objects to the subclass layout that was in effect
  ;; at the time the subclass was created.
  (subclasses nil :type (or hash-table null)))
;;;
(defun class-make-load-form-fun (class)
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (compiler-error
       "Can't use anonymous or undefined class as constant:~%  ~S"
       class))
    `(find-class ',name)))


;;; The UNDEFINED-CLASS is a cookie we make up to stick in forward referenced
;;; layouts.  Users should never see them.
;;;
(defstruct (undefined-class
	    (:include class)
	    (:constructor make-undefined-class (name))))


;;; BUILT-IN-CLASS is used to represent the standard classes that aren't
;;; defined with DEFSTRUCT and other specially implemented primitve types whose
;;; only attribute is their name.  Some standard classes are 
;;;
(defstruct (built-in-class
	    (:include class))
  ;;
  ;; Translation of this class to some other composite type (usually a union of
  ;; other classes.)  This translation is done when type specifiers are parsed.
  ;; Internally we should never encounter translated classes, only their
  ;; translation.  If NIL, then this class stands on its own.  
  (translation nil :type (or ctype null))
  ;;
  ;; List of names of type-classes which are subtypes of this class.  This is
  ;; mutually exclusive with TRANSLATION.
  (type-classes nil :type list))


;;; STRUCTURE-CLASS represents what we need to know about structure classes.
;;; Non-structure "typed" defstructs are a special case, and don't have a
;;; corresponding class.
;;;
(defstruct (structure-class (:include class))
  ;;
  ;; Structure print function, or NIL if none.
  (print-function nil :type (or function null))
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


;;;; Class namespace:

;;; FIND-CLASS  --  Public
;;;
(defun find-class (name &optional (errorp t) environment)
  "Return the class with the specified Name.  If ERRORP is false, then NIL is
   returned when no such class exists."
  (declare (type symbol name) (ignore environment))
  (let ((res (info type class name)))
    (if (or res (not errorp))
	res
	(error "Class not yet defined:~%  ~S" name))))
;;;
(defun (setf find-class) (new-value name)
  (ecase (info type kind name)
    (nil)
    (:structure
     (let ((old (class-of (info type class name)))
	   (new (class-of new-value)))
       (unless (eq old new)
	 (compiler-warning "Changing meta-class of ~S from ~S to ~S."
			   name (class-name old) (class-name new)))))
    (:primitive
     (compiler-error "Illegal to redefine standard type ~S." name))
    (:defined
     (compiler-warning "Redefining DEFTYPE type to be a class: ~S."
		       name)
     (setf (info type expander name) nil)))

  (remhash name *forward-referenced-layouts*)
  (%note-type-defined name)
  (setf (info type kind name) :structure)
  (setf (info type class name) new-value))


;;; INSURED-FIND-CLASS  --  Interface
;;;
;;;    Called when we are about to define Name as class with the specified
;;; Meta-Class.  The first result is always of the desired class.  The second
;;; result is any existing layout for this name.
;;;
(defun insured-find-class (name meta-class constructor)
  (let* ((name (dd-name info))
	 (old (info type class name))
	 (res (if (and old (eq (class-of old) meta-class))
		  old
		  (funcall constructor :name name)))
	 (found (or (gethash name *forward-referenced-layouts*)
		    (when old (class-layout old)))))
    (when found
      (setf (layout-class found) res))

    (values res found)))


;;;; Class type operations:

(define-type-class class)

(define-type-method (instance :simple-subtypep) (class1 class2)
  (if (eq class1 class2)
      (values t t)
      (let ((subclasses (class-subclasses class2)))
	(if (and subclasses (gethash class1 subclasses))
	    (values t t)
	    (values nil t)))))


;;; SEALED-CLASS-INTERSECTION  --  Internal
;;;
;;;    When finding the intersection of a sealed class and some other class
;;; (not hierarchically related) the intersection is the union of the currently
;;; shared subclasses.
;;;
(defun sealed-class-intersection (sealed other)
  (let ((s-sub (class-subclasses sealed))
	(o-sub (class-subclasses other)))
    (if (and s-sub o-sub)
	(collect ((res *empty-type* type-union))
	  (do-hash (subclass layout s-sub)
	    (declare (ignore layout))
	    (when (gethash subclass o-sub)
	      (res (specifier-type class))))
	  (values (res) t))
	(values *empty-type* t))))

    
;;; If one is a subclass of the other, then that is the intersection, but we
;;; can only be sure the intersection is otherwise empty if they are structure
;;; classes, since a subclass of both might be defined.  If either class is
;;; sealed, we can eliminate this possibility.
;;;
(define-type-method (instance :simple-intersection) (class1 class2)
  (declare (type class class1 class2))
  (cond ((eq class1 class2) type1)
	((let ((subclasses (class-subclasses class2)))
	   (and subclasses (gethash class1 subclasses)))
	 (values type1 t))
	((let ((subclasses (class-subclasses class1)))
	   (and subclasses (gethash class2 subclasses)))
	 (values type2 t))
	((or (structure-class-p class1)
	     (structure-class-p class2))
	 (values *empty-type* t))
	((eq (class-state class1) :sealed)
	 (sealed-class-intersection class1 class2))
	((eq (class-state class2) :sealed)
	 (sealed-class-intersection class2 class1))
	(t
	 (values type1 nil))))


(define-type-method (instance :complex-subtypep-arg2) (type1 type2)
  (declare (type instance-type type2))
  (values (or (eq type1 *wild-type*)
	      (and (eq (type-class-name (type-class-info type1)) 'alien)
		   (eq (class-name type2) 'alien-value)))
	  t))

(define-type-method (instance :unparse) (type)
  (class-proper-name type))

(define-type-method (instance :simple-=) (type1 type2)
  (values (eq type1 type2) t))


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
;;; to fail.
;;;
(defun invalidate-layout (layout)
  (setf (layout-invalid layout) t)
  (setf (layout-inheritance-depth layout) -1))


;;; REGISTER-LAYOUT  --  Interface
;;;
;;;    Record Layout as the layout for its class, adding it as a subtype of all
;;; superclasses.  This is the operation that "installs" a layout for a class
;;; in the type system, clobbering any old layout.  However, this does not
;;; modify the class namespace; that is a separate operation (think anonymous
;;; classes.)
;;; -- If INVALIDATE-P, then all the layouts for any old definition
;;;    and subclasses are invalidated, and the SUBCLASSES slot is cleared.
;;  -- If DESTRUCT-P, then there must be an old layout, and this old layout is
;;;    destructively modified to hold the same type information.
;;;
(defun register-layout (layout invalidate-p destruct-p)
  (let* ((class (layout-class layout))
	 (class-layout (class-layout class)))
    (assert (not (eq class-layout layout)))
    (when class-layout
      (modify-class class)
      (do-hash (c l (class-subclasses class))
	(modify-class c)
	(when invalidate-p (invalidate-layout l)))
      (when invalidate-p
	(invalidate-layout layout)
	(setf (class-subclasses class) nil)))
    
    (cond (destruct-p
	   (setf (layout-invalid class-layout) (layout-invalid layout))
	   (setf (layout-inherits class-layout) (layout-inherits layout))
	   (setf (layout-inheritance-depth class-layout)
		 (layout-inheritance-depth layout))
	   (setf (layout-length class-layout) (layout-length layout))
	   (setf (layout-info class-layout) (layout-info layout)))
	  (t
	   (setf (class-layout class) layout)))

    (let ((inherits (layout-inherits layout)))
      (dotimes (i (length inherits))
	(let* ((super (svref inherits i))
	       (subclasses (or (class-subclasses super)
			       (setf (class-subclasses super)
				     (make-hash-table :test #'eq)))))
	  (when (and (eq (class-state super) :sealed)
		     (not (gethash class subclasses)))
	    (warn "Subclassing sealed class ~S; unsealing it."
		  (class-name super))
	    (setf (class-state super) :read-only))
	  (setf (gethash class subclasses)
		(if destruct-p class-layout layout))))))

    (undefined-value))


;;; LAYOUT-PROPER-NAME  --  Internal
;;;
;;;    Return something we can print to unambiguously describe the class for a
;;; layout.  If the class has a proper name, return the name, otherwise return
;;; the class.
;;;
(defun layout-name (x)
  (let* ((class (layout-class x))
	 (name (class-name class)))
    (if (and name (eq (find-class name) class))
	name
	class)))


;;; REDEFINE-LAYOUT-WARNING  --  Interface
;;;
;;;    If layouts Old and New differ in any interesting way, then give a
;;; warning and return T.
;;;
(defun redefine-layout-warning (old old-context new new-context)
  (assert (eq (layout-class old) (layout-class new) class))
  (let ((name (layout-proper-name old)))
    (or (let ((oldi (layout-inherits old))
	      (newi (layout-inherits new)))
	  (or (when (mismatch oldi newi :key #'layout-proper-name)
		(compiler-warning
		 "Change in superclasses of class ~S:~@  ~
		  ~A superclasses: ~S~%  ~
		  ~A superclasses: ~S"
		 name
		 old-context (map 'list #'layout-proper-name oldi)
		 new-context (map 'list #'layout-proper-name newi))
		t)
	      (let ((diff (mismatch oldi newi)))
		(when diff
		  (compiler-warning
		   "In class ~S:~%  ~
		    ~A definition of superclass ~S incompatible with ~
		    ~A definition."
		   name old-context (layout-name (svref oldi diff))
		   new-context)
		  t))))
	(let ((old-len (layout-length old))
	      (new-len (layout-length new)))
	  (unless (= old-len new-len)
	    (compiler-warning "Change in instance length of class ~S:~%  ~
			       ~A length: ~D~%  ~
			       ~A length: ~D"
			      name
			      old-context old-len
			      new-context new-len)
	    t))
	(when (/= (layout-inheritance-depth old)
		  (layout-inheritance-depth new))
	  (compiler-warning "Change in the inheritance structure of class ~S~@
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
  (let* ((class (or (info type class name)
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
	  ((redefine-layout-warning old "current" res "compile time")
	   (restart-case
	       (error "Loading a reference to class ~S when the compile ~@
		       time definition was incompatible with the current ~
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
	       (setf (layout-inherits-depth old) depth)
	       (setf (layout-inheritance-depth old) inherits)
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


;;;; Built-in classes:

;;; ### If any of these layouts are referenced as constants in the cold load,
;;; then genesis must pre-intern the layouts, and we would actually create the
;;; classes here.  Probably needs to be an init-function that runs before
;;; top-level forms.

;;; ### special-case instance, funcallable-instance, null for LAYOUT-OF
;;;
(defconstant built-in-classes
  '((t :subclasses (number array member function structure alien))
    (character :enumerable t)
    (base-char :inherits (character) :codes (#.vm:base-char-type)
	       :enumerable t)
    (extended-char :inherits (character) :translation nil :enumerable t)
    (standard-char :inherits (base-char character) :enumerable t)

    (array :tranlation array
	   :codes
	   (#.vm:complex-array-type
	    #.vm:simple-array-type
	    #.vm:simple-array-double-float-type
	    #.vm:simple-array-single-float-type
	    #.vm:simple-array-unsigned-byte-2-type
	    #.vm:simple-array-unsigned-byte-4-type
	    #.vm:simple-array-unsigned-byte-8-type
	    #.vm:simple-array-unsigned-byte-16-type
	    #.vm:simple-array-unsigned-byte-32-type))
    (sequence :translation (or cons (member nil) vector))
    (symbol :codes (#.vm:symbol-header-type))
    (keyword :inherits (symbol))
    (system-area-pointer)
    (weak-pointer)
    (scavenger-hook)
    (code-component)
    (lra)
    (fdefn)

    (function
     :codes
     (#.vm:byte-code-closure-type #.vm:byte-code-function-type
      #.vm:closure-header-type  #.vm:function-header-type)
     :subclasses (function))
    (generic-function :inherits (function)
		      :subclasses (function)
		      :codes (#.vm:funcallable-instance-header-type))

    (vector :translation vector :inherits (array sequence)
	    :codes (#.vm:complex-vector-type #.vm:simple-vector-type))
    (bit-vector
     :translation bit-vector  :inherits (vector array sequence)
     :codes (#.vm:complex-bit-vector-type #.vm:simple-bit-vector-type))
    (string
     :translation string  :inherits (vector array sequence)
     :codes (#.vm:complex-string-type #.vm:simple-string-type))

    (number :translation number)
    (complex :translation complex :inherits (number)
	     :codes (#.vm:complex-type))
    (float :translation float :inherits (number))
    (single-float
     :translation single-float :inherits (float number)
     :codes (#.vm:single-float-type))
    (double-float
     :translation double-float  :inherits (float number)
     :codes (#.vm:double-float-type))
    (rational :translation rational :inherits (number))
    (ratio
     :translation (and rational (not integer))
     :inherits (rational number)
     :codes (#.vm:ratio-type))
    (integer
     :translation integer  :inherits (rational number)
     :codes (#.vm:bignum-type  #.vm:even-fixnum-type #.vm:odd-fixnum-type))

    (list :translation (or cons (member nil)) :inherits (sequence))
    (cons :inherits (list sequence) :codes (#.vm:list-type))
    (null :translation (member nil) :inherits (symbol list sequence))))

(dolist (x built-in-classes)
  (let* ((name (first x))
	 (class (make-built-in-class name (specifier-type name)))
	 (layout (make-layout
		  :class class
		  :inherits (map 'vector #'find-layout (reverse (rest x))))))
    (register-layout layout nil nil)
    (setf (info type class name) class)))

(dolist (x built-in-classes)
  (setf (class-state (find-class (first x))) :frozen))

(defconstant built-in-class-codes
  (let ((res (make-array 256 :initial-element nil)))
    (dolist (x built-in-classes)
      (let ((layout (class-layout (find-class (first x)))))
	(dolist (code (third x))
	  (setf (svref res code) layout))))))

(defun layout-of (x)
  (cond ((%instancep x) (%instance-layout x))
	((null x) (load-time-value (class-layout (find-class 'null))))
	((svref built-in-class-codes (vm:get-type x)))
	(t
	 (error "Some strange object: ~S" x))))

(defun class-of (x) (class-layout (layout-of x)))
