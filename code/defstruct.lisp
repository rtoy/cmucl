;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/defstruct.lisp,v 1.37 1992/12/15 19:45:07 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Defstruct structure definition package (Mark II).
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package "C")

(in-package "LISP")
(export '(defstruct copy-structure))

(in-package :c)

;;; Always compile safe.  This code isn't very careful about protecting itself.
;;; Note: we only do this at compile time because defstruct gets cold-loaded
;;; before enough stuff to handle the declaim has been set up.
(eval-when (compile)
  (declaim (optimize (safety 1))))


;;;; Structure frobbing primitives.

(defun make-structure (length)
  "Allocate a new structure with LENGTH data slots."
  (declare (type index length))
  (make-structure length))

(defun structure-length (structure)
  "Given a structure, return its length."
  (declare (type structure structure))
  (structure-length structure))

(defun structure-ref (struct index)
  "Return the value from the INDEXth slot of STRUCT.  0 corresponds to the
  type.  This is SETFable."
  (structure-ref struct index))

(defun structure-set (struct index new-value)
  "Set the INDEXth slot of STRUCT to NEW-VALUE."
  (setf (structure-ref struct index) new-value))

(defsetf structure-ref structure-set)



;;; This version of Defstruct is implemented using Defstruct, and is free of
;;; Maclisp compatability nonsense.  For bootstrapping, you're on your own.

(defun print-defstruct-description (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Defstruct-Description for ~S>" (dd-name structure)))

;;; DSD-Name  --  Internal
;;;
;;;    Return the the name of a defstruct slot as a symbol.  We store it
;;; as a string to avoid creating lots of worthless symbols at load time.
;;;
(defun dsd-name (dsd)
  (intern (string (dsd-%name dsd))
	  (if (dsd-accessor dsd)
	      (symbol-package (dsd-accessor dsd))
	      *package*)))

(defun print-defstruct-slot-description (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Defstruct-Slot-Description for ~S>" (dsd-name structure)))



;;; The legendary macro itself.

(defmacro defstruct (name-and-options &rest slot-descriptions)
  "Defstruct {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
  Define the structure type Name.  See the manual for details."
  (let* ((defstruct (parse-name-and-options name-and-options))
	 (name (dd-name defstruct)))
    (parse-slot-descriptions defstruct slot-descriptions)
    (if (eq (dd-type defstruct) 'structure)
	`(progn
	   (%defstruct ',defstruct)
	   (%compiler-defstruct ',defstruct)
	   ,@(define-constructors defstruct)
	   ,@(define-boa-constructors defstruct)
	   ;;
	   ;; So the print function is in the right lexical environment, and
	   ;; can be compiled...
	   ,@(let ((pf (dd-print-function defstruct)))
	       (when pf
		 `((setf (info type printer ',name)
			 ,(if (symbolp pf)
			      `',pf
			      `#',pf)))))
	   ,@(let ((mlff (dd-make-load-form-fun defstruct)))
	       (when mlff
		 `((setf (info type load-form-maker ',name)
			 ,(if (symbolp mlff)
			      `',mlff
			      `#',mlff)))))
	   ',name)
	`(progn
	   (eval-when (compile load eval)
	     (setf (info type kind ',name) nil)
	     (setf (info type structure-info ',name) ',defstruct))
	   ,@(define-constructors defstruct)
	   ,@(define-boa-constructors defstruct)
	   ,@(define-predicate defstruct)
	   ,@(define-accessors defstruct)
	   ,@(define-copier defstruct)
	   ',name))))
	   

;;;; Parsing:

(defun parse-name-and-options (name-and-options)
  (if (atom name-and-options)
      (setq name-and-options (list name-and-options)))
  (do* ((options (cdr name-and-options) (cdr options))
	(name (car name-and-options))
	(print-function nil)
	(pf-supplied-p)
	(conc-name (concat-pnames name '-))
	(constructors '())
	(constructor-opt-p nil)
	(boa-constructors '())
	(copier (concat-pnames 'copy- name))
	(predicate (concat-pnames name '-p))
	(include)
	(saw-type)
	(type 'structure)
	(saw-named)
	(offset 0)
	(make-load-form-fun nil)
	(make-load-form-fun-p nil))
       ((null options)
	(let ((named (if saw-type saw-named t)))
	  (make-defstruct-description
	   :name name
	   :conc-name conc-name
	   :constructors
	   (if constructor-opt-p
	       (nreverse constructors)
	       (list (concat-pnames 'make- name)))
	   :boa-constructors boa-constructors
	   :copier copier
	   :predicate predicate
	   :include include
	   :print-function print-function
	   :type type
	   :length (if named 1 0)
	   :lisp-type (cond ((eq type 'structure) 'simple-vector)
			    ((eq type 'vector) 'simple-vector)
			    ((eq type 'list) 'list)
			    ((and (listp type) (eq (car type) 'vector))
			     (cons 'simple-array (cdr type)))
			    (t (error "~S is a bad :TYPE for Defstruct." type)))
	   :named named
	   :offset offset
	   :make-load-form-fun make-load-form-fun)))
    (if (atom (car options))
	(case (car options)
	  (:constructor
	   (setf constructor-opt-p t)
	   (setf constructors (list (concat-pnames 'make- name))))
	  (:copier)
	  (:predicate)
	  (:named (setq saw-named t))
	  (t (error "The Defstruct option ~S cannot be used with 0 arguments."
		    (car options))))
	(let ((option (caar options))
	      (args (cdar options)))
	  (case option
	    (:conc-name
	     (setq conc-name (car args))
	     (unless (symbolp conc-name)
	       (setq conc-name (make-symbol (string conc-name)))))
	    (:constructor
	     (setf constructor-opt-p t)
	     (let ((lambda-list (cdr args))
		   (constructor-name (car args))
		   (no-explicit-nil-name (not args)))
	       ;; Constructor-name may be nil because args has one element, the
	       ;; explicit name of nil.  In this situation, don't make a
	       ;; default constructor.  If args itself is nil, then we make a
	       ;; default constructor.
	       (cond (lambda-list
		      (push args boa-constructors))
		     (constructor-name
		      (push constructor-name constructors))
		     (no-explicit-nil-name
		      (push (concat-pnames 'make- name) constructors)))))
	    (:copier (setq copier (car args)))
	    (:predicate (setq predicate (car args)))
	    (:include
	     (setf include args)
	     (let* ((name (car include))
		    (included-structure
		     (info type structure-info name)))
	       (unless included-structure
		 (error "Cannot find description of structure ~S to use for ~
		         inclusion."
			name))
	       (unless pf-supplied-p
		 (setf print-function
		       (dd-print-function included-structure)))
	       (unless make-load-form-fun-p
		 (setf make-load-form-fun
		       (dd-make-load-form-fun included-structure)))))
	    (:print-function
	     (setf print-function (car args))
	     (setf pf-supplied-p t))
	    (:type (setf saw-type t type (car args)))
	    (:named (error "The Defstruct option :NAMED takes no arguments."))
	    (:initial-offset (setf offset (car args)))
	    (:make-load-form-fun
	     (setf make-load-form-fun (car args))
	     (setf make-load-form-fun-p t))
	    (t (error "~S is an unknown Defstruct option." option)))))))



;;;; Stuff to parse slot descriptions.

;;; PARSE-1-DSD  --  Internal
;;;
;;;    Parse a slot description for DEFSTRUCT and add it to the description.
;;; If supplied, ISLOT is a pre-initialized DSD that we modify to get the new
;;; slot.  This is supplied when handling included slots.  If the new accessor
;;; name is already an accessor for same slot in some included structure, then
;;; set the DSD-ACCESSOR to NIL so that we don't clobber the more general
;;; accessor.
;;;
(defun parse-1-dsd (defstruct spec &optional
		     (islot (make-defstruct-slot-description
			     :%name "" :index 0 :type t)))
  (multiple-value-bind
      (name default default-p type type-p read-only ro-p)
      (cond
       ((listp spec)
	(destructuring-bind (name &optional (default nil default-p)
				  &key (type nil type-p) (read-only nil ro-p))
			    spec
	  (values name default default-p type type-p read-only ro-p)))
       (t
	(when (keywordp spec)
	  (warn "Keyword slot name indicates possible syntax ~
		 error in DEFSTRUCT -- ~S."
		spec))
	spec))
    (when (find name (dd-slots defstruct) :test #'string= :key #'dsd-%name)
      (error "Duplicate slot name ~S." name))
    (setf (dsd-%name islot) (string name))
    (setf (dd-slots defstruct) (nconc (dd-slots defstruct) (list islot)))

    (let* ((aname (concat-pnames (dd-conc-name defstruct) name))
	   (existing (info function accessor-for aname)))
      (if (and existing
	       (string= (dsd-name (find aname (dd-slots existing)
					:key #'dsd-accessor))
			name)
	       (member (dd-name existing) (dd-includes defstruct)))
	  (setf (dsd-accessor islot) nil)
	  (setf (dsd-accessor islot) aname)))
    
    (when default-p
      (setf (dsd-default islot) default))
    (when type-p
      (setf (dsd-type islot) type))
    (when ro-p
      (setf (dsd-read-only islot) read-only))
    (setf (dsd-index islot) (dd-length defstruct))
    (incf (dd-length defstruct)))
  (undefined-value))


;;; PARSE-SLOT-DESCRIPTIONS parses the slot descriptions (surprise) and does
;;; any structure inclusion that needs to be done.
;;;
(defun parse-slot-descriptions (defstruct slots)
  ;; First strip off any doc string and stash it in the Defstruct.
  (when (stringp (car slots))
    (setf (dd-doc defstruct) (car slots))
    (setq slots (cdr slots)))
  ;; Then include stuff.  We add unparsed items to the start of the Slots.
  (when (dd-include defstruct)
    (destructuring-bind (included-name &rest modified-slots)
			(dd-include defstruct)
      (let ((included-thing
	     (or (info type structure-info included-name)
		 (error "Cannot find description of structure ~S ~
			 to use for inclusion."
			included-name))))
	(setf (dd-includes defstruct)
	      (cons (dd-name included-thing) (dd-includes included-thing)))
	(incf (dd-offset defstruct) (dd-offset included-thing))
	(incf (dd-length defstruct) (dd-offset defstruct))
	(dolist (islot (dd-slots included-thing))
	  (let* ((iname (dsd-name islot))
		 (modified (or (find iname modified-slots
				     :key #'(lambda (x) (if (atom x) x (car x)))
				     :test #'string=)
			       `(,iname))))
	    (parse-1-dsd defstruct modified
			 (copy-defstruct-slot-description islot)))))))
  
  ;; Finally parse the slots into Slot-Description objects.
  (dolist (slot slots)
    (parse-1-dsd defstruct slot))
  (undefined-value))


;;;; Default structure access and copiers:
;;;
;;;    In the normal case of structures that have a real type (i.e. no :Type
;;; option was specified), we want to optimize things for space as well as
;;; speed, since there can be thousands of defined slot accesors.
;;;
;;;    What we do is defined the accessors and copier as closures over
;;; general-case code.  Since the compiler will normally open-code accesors,
;;; the (minor) efficiency penalty is not a concern.

;;; Typep-To-Structure  --  Internal
;;;
;;;    Return true if Obj is an object of the structure type specified by Info.
;;; This is called by the accessor closures, which have a handle on the type's
;;; Defstruct-Description.
;;;
#+new-compiler
(proclaim '(inline typep-to-structure))
#+new-compiler
(defun typep-to-structure (obj info)
  (declare (type defstruct-description info) (inline member))
  (and (structurep obj)
       (let ((name (structure-ref obj 0)))
	 (or (eq name (dd-name info))
	     (member name (dd-included-by info) :test #'eq)))))


;;; %REDEFINE-DEFSTRUCT  --  Internal
;;;
;;;    This function is called when we are redefining a structure from Old to
;;; New.  If the slots are different, we flame loudly, but give the luser a
;;; chance to proceed.  We flame especially loudly if there are structures that
;;; include this one.  If proceeded, we FMAKUNBOUND all the old accessors.  If
;;; the redefinition is not incompatible, we make the INCLUDED-BY of the new
;;; definition be the same as the old one.
;;;
(defun %redefine-defstruct (old new)
  (declare (type defstruct-description old new))
  (cond
   ((and (equalp (dd-slots old) (dd-slots new))
	 (equal (dd-includes old) (dd-includes new)))
    (setf (dd-included-by new) (dd-included-by old)))
   (t
    (let ((name (dd-name old))
	  (included-by (dd-included-by old)))
      (cerror
       "Recklessly proceed with wanton disregard for Lisp and limb."
       "Structure ~S is being incompatibly redefined.  If proceeded, you must~@
       recompile all uses of this structure's accessors.~:[~;~@
       ~S is included by these structures:~
       ~%  ~S~@
       You must also recompile these DEFSTRUCTs and all the uses of their ~
       accessors.~]"
       name included-by name included-by)

      (dolist (slot (dd-slots old))
	(fmakunbound (dsd-accessor slot))
	(unless (dsd-read-only slot)
	  (fmakunbound `(setf ,(dsd-accessor slot))))))))

  (undefined-value))

#+new-compiler
;;; %Defstruct  --  Internal
;;;
;;;    Do miscellaneous load-time actions for the structure described by Info.
;;; Define setters, accessors, copier, predicate, documentation, instantiate
;;; definition in load-time env.  This is only called for default structures.
;;;
(defun %defstruct (info)
  (declare (type defstruct-description info))
  (let* ((name (dd-name info))
	 (old (info type defined-structure-info name)))
    ;;
    ;; Don't flame about dd structures, since they are hackishly defined in
    ;; type-boot...
    (when (and old
	       (not (member name '(defstruct-description
				   defstruct-slot-description))))
      (%redefine-defstruct old info))
    
    (setf (info type defined-structure-info name) info)
    (dolist (include (dd-includes info))
      (let ((iinfo (info type defined-structure-info include)))
	(unless iinfo
	  (error "~S includes ~S, but it is not defined." name include))
	(pushnew name (dd-included-by iinfo)))))
    
  (dolist (slot (dd-slots info))
    (let ((dsd slot))
      (when (dsd-accessor slot)
	(setf (symbol-function (dsd-accessor slot))
	      #'(lambda (structure)
		  (declare (optimize (speed 3) (safety 0)))
		  (unless (typep-to-structure structure info)
		    (error "Structure for accessor ~S is not a ~S:~% ~S"
			   (dsd-accessor dsd) (dd-name info) structure))
		  (structure-ref structure (dsd-index dsd))))
      
	(unless (dsd-read-only slot)
	  (setf (fdefinition `(setf ,(dsd-accessor slot)))
		#'(lambda (new-value structure)
		    (declare (optimize (speed 3) (safety 0)))
		    (unless (typep-to-structure structure info)
		      (error "Structure for setter ~S is not a ~S:~% ~S"
			     `(setf ,(dsd-accessor dsd)) (dd-name info)
			     structure))
		    (unless (typep new-value (dsd-type dsd))
		      (error "New-Value for setter ~S is not a ~S:~% ~S."
			     `(setf ,(dsd-accessor dsd)) (dsd-type dsd)
			     new-value))
		    (setf (structure-ref structure (dsd-index dsd))
			  new-value)))))))

  (when (dd-predicate info)
    (setf (symbol-function (dd-predicate info))
	  #'(lambda (object)
	      (declare (optimize (speed 3) (safety 0)))
	      (if (typep-to-structure object info) t nil))))

  (when (dd-copier info)
    (setf (symbol-function (dd-copier info))
	  #'(lambda (structure)
	      (declare (optimize (speed 3) (safety 0)))
	      (unless (typep-to-structure structure info)
		(error "Structure for copier ~S is not a ~S:~% ~S"
		       (dd-copier info) (dd-name info) structure))

	      (let* ((len (dd-length info))
		     (res (make-structure len)))
		(declare (type structure-index len))
		(dotimes (i len)
		  (declare (type structure-index i))
		  (setf (structure-ref res i)
			(structure-ref structure i)))
		res))))
  (when (dd-doc info)
    (setf (documentation (dd-name info) 'type) (dd-doc info))))


;;; COPY-STRUCTURE  --  Public
;;;
;;;    Copy any old kind of structure.
;;;
(defun copy-structure (structure)
  "Return a copy of Structure with the same (EQL) slot values."
  (declare (type structure structure))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((len (structure-length structure))
	   (res (make-structure len)))
      (declare (type structure-index len))
      (dotimes (i len)
	(declare (type structure-index i))
	(setf (structure-ref res i)
	      (structure-ref structure i)))
      res)))


;;; Define-Accessors returns a list of function definitions for accessing and
;;; setting the slots of the a typed Defstruct.  The functions are proclaimed
;;; to be inline, and the types of their arguments and results are declared as
;;; well.  We count on the compiler to do clever things with Elt.

(defun define-accessors (defstruct)
  (do ((slots (dd-slots defstruct) (cdr slots))
       (stuff '())
       (type (dd-lisp-type defstruct)))
      ((null slots) stuff)
    (let* ((slot (car slots))
	   (name (dsd-accessor slot))
	   (index (dsd-index slot))
	   (slot-type (dsd-type slot)))
      (push
       `(progn
	  (proclaim '(inline ,name (setf ,name)))
	  (defun ,name (structure)
	    (declare (type ,type structure))
	    (the ,slot-type (elt structure ,index)))
	  ,@(unless (dsd-read-only slot)
	      `((defun (setf ,name) (new-value structure)
		  (declare (type ,type structure) (type ,slot-type new-value))
		  (setf (elt structure ,index) new-value)))))
       stuff))))


;;; Define-Constructors returns a definition for the constructor function of
;;; the given Defstruct.  If the structure is implemented as a vector and is
;;; named, we structurify it.  If the structure is a vector of some specialized
;;; type, we can't use the Vector function.
;;;
(defun define-constructors (defstruct)
  (let ((cons-names (dd-constructors defstruct)))
    (when cons-names
      (let* ((name (first cons-names))
	     (initial-cruft
	      (if (dd-named defstruct)
		  (make-list (1+ (dd-offset defstruct))
			     :initial-element `',(dd-name defstruct))
		  (make-list (dd-offset defstruct))))
	     (slots (dd-slots defstruct))
	     (names (mapcar #'dsd-name slots))
	     (args (mapcar #'(lambda (slot)
			       `(,(dsd-name slot) ,(dsd-default slot)))
			   slots)))
	`((defun ,name ,(if args `(&key ,@args))
	    (declare
	     ,@(mapcar #'(lambda (slot)
			   `(type ,(dsd-type slot) ,(dsd-name slot)))
		       slots))
	    ,(case (dd-type defstruct)
	       (list
		`(list ,@initial-cruft ,@names))
	       (structure
		(let ((temp (gensym)))
		  `(let ((,temp (make-structure ,(dd-length defstruct))))
		     (declare (type structure ,temp))
		     (setf (structure-ref ,temp 0) ',(dd-name defstruct))
		     ,@(mapcar #'(lambda (slot)
				   `(setf (structure-ref ,temp
							 ,(dsd-index slot))
					  ,(dsd-name slot)))
			       slots)
		     (truly-the ,(dd-name defstruct) ,temp))))
	       (vector
		`(vector ,@initial-cruft ,@names))
	       (t
		(do ((sluts slots (cdr sluts))
		     (sets '())
		     (temp (gensym)))
		    ((null sluts)
		     `(let ((,temp (make-array
				    ,(dd-length defstruct)
				    :element-type
				    ',(cadr (dd-lisp-type defstruct)))))
			,@(when (dd-named defstruct)
			    `(setf (aref ,temp ,(dd-offset defstruct))
				   ',(dd-name defstruct)))
			,@sets
			,temp))
		  (let ((slot (car sluts)))
		    (push `(setf (aref ,temp ,(dsd-index slot))
				 ,(dsd-name slot))
			  sets))))))
	  ,@(mapcar #'(lambda (other-name)
			`(setf (fdefinition ',other-name) #',name))
		    (rest cons-names)))))))


;;;; Support for By-Order-Argument Constructors.

;;; FIND-LEGAL-SLOT   --  Internal
;;;
;;;    Given a defstruct description and a slot name, return the corresponding
;;; slot if it exists, or signal an error if not.
;;;
(defun find-legal-slot (defstruct name)
  (or (find name (dd-slots defstruct) :key #'dsd-name :test #'string=)
      (error "~S is not a defined slot name in the ~S structure."
	     name (dd-name defstruct))))


;;; Define-Boa-Constructors defines positional constructor functions.  We
;;; generate code to set each variable not specified in the arglist to the
;;; default given in the Defstruct.  We just slap required args in, as with
;;; rest args and aux args.  Optionals are treated a little differently.  Those
;;; that aren't supplied with a default in the arg list are mashed so that
;;; their default in the arglist is the corresponding default from the
;;; Defstruct.
;;;
(defun define-boa-constructors (defstruct)
  (do* ((boas (dd-boa-constructors defstruct) (cdr boas))
	(name (car (car boas)) (car (car boas)))
	(args (copy-list (cadr (car boas))) (copy-list (cadr (car boas))))
	(slots (dd-slots defstruct) (dd-slots defstruct))
	(slots-in-arglist '() '())
	(defuns '()))
       ((null boas) defuns)
    ;; Find the slots in the arglist and hack the defaultless optionals.
    (do ((args args (cdr args))
	 (arg-kind 'required))
	((null args))
      (let ((arg (car args)))
	(cond ((not (atom arg))
	       (push (find-legal-slot defstruct (car arg)) slots-in-arglist))
	      ((member arg '(&optional &rest &aux &key) :test #'eq)
	       (setq arg-kind arg))
	      (t
	       (case arg-kind
		 ((required &rest &aux)
		  (push (find-legal-slot defstruct arg) slots-in-arglist))
		 ((&optional &key)
		  (let ((dsd (find-legal-slot defstruct arg)))
		    (push dsd slots-in-arglist)
		    (rplaca args (list arg (dsd-default dsd))))))))))
    
    ;; Then make a list that can be used with a (list ...) or (vector...).
    (let ((initial-cruft
	   (if (dd-named defstruct)
	       (make-list (1+ (dd-offset defstruct))
			  :initial-element `',(dd-name defstruct))
	       (make-list (dd-offset defstruct))))
	  (thing (mapcar #'(lambda (slot)
			     (if (member slot slots-in-arglist
					 :test #'eq)
				 (dsd-name slot)
				 (dsd-default slot)))
			 slots)))
      (push
       `(defun ,name ,args
	  (declare
	   ,@(mapcar #'(lambda (slot)
			 `(type ,(dsd-type slot) ,(dsd-name slot)))
		     slots-in-arglist))
	  ,(case (dd-type defstruct)
	     (list
	      `(list ,@initial-cruft ,@thing))
	     (structure
	      (let ((temp (gensym)))
		`(let ((,temp (make-structure ,(dd-length defstruct))))
		   (declare (type structure ,temp))
		   (setf (structure-ref ,temp 0) ',(dd-name defstruct))
		   ,@(mapcar #'(lambda (slot thing)
				 `(setf (structure-ref ,temp
						       ,(dsd-index slot))
					,thing))
			     slots thing)
		   (truly-the ,(dd-name defstruct) ,temp))))
	     (vector
	      `(vector ,@initial-cruft ,@thing))
	     (t
	      (do ((things thing (cdr things))
		   (index 0 (1+ index))
		   (sets '())
		   (temp (gensym)))
		  ((null things)
		   `(let ((,temp (make-array
				  ,(dd-length defstruct)
				  :element-type
				  ',(cadr (dd-lisp-type defstruct)))))
		      ,@(when (dd-named defstruct)
			  `(setf (aref ,temp ,(dd-offset defstruct))
				 ',(dd-name defstruct)))
		      ,@sets
		      ,temp))
		(push `(setf (aref ,temp index) ,(car things))
		      sets)))))
       defuns))))

;;; Define-Copier returns the definition for a copier function of a typed
;;; Defstruct if one is desired.

(defun define-copier (defstruct)
  (when (dd-copier defstruct)
    `((defun ,(dd-copier defstruct) (structure)
	(declare (type ,(dd-lisp-type defstruct) structure))
	(subseq structure 0 ,(dd-length defstruct))))))


;;; Define-Predicate returns a definition for a predicate function if one is
;;; desired.  This is only called for typed structures, since the default
;;; structure predicate is implemented as a closure. 

(defun define-predicate (defstruct)
  (let ((name (dd-name defstruct))
	(pred (dd-predicate defstruct)))
    (when (and pred (dd-named defstruct))
      (let ((ltype (dd-lisp-type defstruct)))
	`((defun ,pred (object)
	    (and (typep object ',ltype)
		 (eq (elt (the ,ltype object) ,(dd-offset defstruct))
		     ',name))))))))


;;; Random sorts of stuff.

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (let* ((type (structure-ref structure 0))
	 (dd (info type defined-structure-info type)))
    (if *print-pretty*
	(pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
	  (prin1 type stream)
	  (let ((slots (dd-slots dd)))
	    (when slots
	      (write-char #\space stream)
	      (pprint-indent :block 2 stream)
	      (pprint-newline :linear stream)
	      (loop
		(pprint-pop)
		(let ((slot (pop slots)))
		  (write-char #\: stream)
		  (output-symbol-name (dsd-%name slot) stream)
		  (write-char #\space stream)
		  (pprint-newline :miser stream)
		  (output-object (structure-ref structure (dsd-index slot))
				 stream)
		  (when (null slots)
		    (return))
		  (write-char #\space stream)
		  (pprint-newline :linear stream))))))
	(descend-into (stream)
	  (write-string "#S(" stream)
	  (prin1 type stream)
	  (do ((index 1 (1+ index))
	       (length (structure-length structure))
	       (slots (dd-slots dd) (cdr slots)))
	      ((or (= index length)
		   (and *print-length*
			(= index *print-length*)))
	       (if (= index length)
		   (write-string ")" stream)
		   (write-string "...)" stream)))
	    (declare (type index index))
	    (write-char #\space stream)
	    (write-char #\: stream)
	    (output-symbol-name (dsd-%name (car slots)) stream)
	    (write-char #\space stream)
	    (output-object (structure-ref structure index) stream))))))


(defun make-structure-load-form (structure)
  (declare (type structure structure))
  (let* ((type (structure-ref structure 0))
	 (fun (info type load-form-maker type)))
    (etypecase fun
      ((member :just-dump-it-normally :ignore-it)
       fun)
      (null
       (error "Structures of type ~S cannot be dumped as constants." type))
      (function
       (funcall fun structure))
      (symbol
       (funcall (symbol-function fun) structure)))))
