;;; -*- Log: code.log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/defstruct.lisp,v 1.10 1990/09/06 17:53:14 wlott Exp $
;;;
;;; Defstruct structure definition package (Mark II).
;;; Written by Skef Wholey and Rob MacLachlan.
;;;
(in-package 'c)
(export '(lisp::defstruct) "LISP")

;;; Note: STRUCTURIFY is defined in struct.lisp.  It converts a simple-vector
;;; into a structure.


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
  (intern (string (dsd-%name dsd)) (symbol-package (dsd-accessor dsd))))

(defun print-defstruct-slot-description (structure stream depth)
  (declare (ignore depth))
  (format stream "#<Defstruct-Slot-Description for ~S>" (dsd-name structure)))



;;; The legendary macro itself.

;;; ### Bootstrap hack...
;;; Install this definition only into the new compiler's environment so that we
;;; don't break the bootstrap environment.
;;;
(compiler-let ((lisp::*bootstrap-defmacro* t))

(defmacro defstruct (name-and-options &rest slot-descriptions)
  "Defstruct {Name | (Name Option*)} {Slot | (Slot [Default] {Key Value}*)}
  Define the structure type Name.  See the manual for details."
  (let* ((defstruct (parse-name-and-options name-and-options))
	 (name (dd-name defstruct)))
    (parse-slot-descriptions defstruct slot-descriptions)
    (if (eq (dd-type defstruct) 'structure)
	`(progn
	   (%compiler-defstruct ',defstruct)
	   ,@(define-constructor defstruct)
	   ,@(define-boa-constructors defstruct)
	   
	   ;;
	   ;; So the print function is in the right lexical environment, and
	   ;; can be compiled...
	   (let ((new ',defstruct))
	     ,@(let ((pf (dd-print-function defstruct)))
		 (when pf
		   `((setf (info type printer ',name)
			   ,(if (symbolp pf)
				`',pf
				`#',pf)))))
	     (%defstruct new))
	   ',name)
	`(progn
	   (eval-when (compile load eval)
	     (setf (info type kind ',name) nil)
	     (setf (info type structure-info ',name) ',defstruct))
	   ,@(define-constructor defstruct)
	   ,@(define-boa-constructors defstruct)
	   ,@(define-predicate defstruct)
	   ,@(define-accessors defstruct)
	   ,@(define-copier defstruct)
	   ',name))))

); Compiler-Let
	   

;;;; Parsing:

(defun parse-name-and-options (name-and-options)
  (if (atom name-and-options)
      (setq name-and-options (list name-and-options)))
  (do* ((options (cdr name-and-options) (cdr options))
	(name (car name-and-options))
	(print-function nil)
	(pf-supplied-p)
	(conc-name (concat-pnames name '-))
	(constructor (concat-pnames 'make- name))
	(saw-constructor)
	(boa-constructors '())
	(copier (concat-pnames 'copy- name))
	(predicate (concat-pnames name '-p))
	(include)
	(saw-type)
	(type 'structure)
	(saw-named)
	(offset 0))
       ((null options)
	(make-defstruct-description
	 :name name
	 :conc-name conc-name
	 :constructor constructor
	 :boa-constructors boa-constructors
	 :copier copier
	 :predicate predicate
	 :include include
	 :print-function print-function
	 :type type
	 :lisp-type (cond ((eq type 'structure) 'simple-vector)
			  ((eq type 'vector) 'simple-vector)
			  ((eq type 'list) 'list)
			  ((and (listp type) (eq (car type) 'vector))
			   (cons 'simple-array (cdr type)))
			  (t (error "~S is a bad :TYPE for Defstruct." type)))
	 :named (if saw-type saw-named t)
	 :offset offset))
    (if (atom (car options))
	(case (car options)
	  (:constructor (setq saw-constructor t
			      constructor (concat-pnames 'make- name)))
	  (:copier)
	  (:predicate)
	  (:named (setq saw-named t))
	  (t (error "The Defstruct option ~S cannot be used with 0 arguments."
		    (car options))))
	(let ((option (caar options))
	      (args (cdar options)))
	  (case option
	    (:conc-name (setq conc-name (car args)))
	    (:constructor (cond ((cdr args)
				 (unless saw-constructor
				   (setq constructor nil))
				 (push args boa-constructors))
				(t
				 (setq saw-constructor t)
				 (setq constructor
				       (or (car args)
					   (concat-pnames 'make- name))))))
	    (:copier (setq copier (car args)))
	    (:predicate (setq predicate (car args)))
	    (:include
	     (setf include args)
	     (let* ((name (car include))
		    (included-structure
		     (info type structure-info name))
		    (included-print-function
		     (if included-structure
			 (dd-print-function included-structure))))
	       (unless included-structure
		 (error "Cannot find description of structure ~S to use for ~
		         inclusion."
			name))
	       (unless pf-supplied-p
		 (setf print-function included-print-function))))
	    (:print-function
	     (setf print-function (car args))
	     (setf pf-supplied-p t))
	    (:type (setf saw-type t type (car args)))
	    (:named (error "The Defstruct option :NAMED takes no arguments."))
	    (:initial-offset (setf offset (car args)))
	    (t (error "~S is an unknown Defstruct option." option)))))))



;;;; Stuff to parse slot descriptions.

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
    (let* ((included-name (car (dd-include defstruct)))
	   (included-thing (info type structure-info included-name))
	   (modified-slots (cdr (dd-include defstruct))))
      (unless included-thing
	(error "Cannot find description of structure ~S to use for inclusion."
	       included-name))
      (setf (dd-includes defstruct)
	    (cons (dd-name included-thing) (dd-includes included-thing)))
      (setf (dd-offset defstruct) (dd-offset included-thing))
      (do* ((islots (mapcar #'(lambda (slot)
				`(,(dsd-name slot) ,(dsd-default slot)
				  :type ,(dsd-type slot)
				  :read-only ,(dsd-read-only slot)))
			    (dd-slots included-thing)))
	    (islots* islots (cdr islots*)))
	   ((null islots*)
	    (setq slots (nconc islots slots)))
	(let* ((islot (car islots*))
	       (modifiee (find (car islot) modified-slots
			       :key #'(lambda (x) (if (atom x) x (car x)))
			       :test #'string=)))
	  (when modifiee
	    (cond ((symbolp modifiee)
		   ;; If it's just a symbol, nilify the default.
		   (setf (cadr islot) nil))
		  ((listp modifiee)
		   ;; If it's a list, parse new defaults and options.
		   (setf (cadr islot) (cadr modifiee))
		   (when (cddr modifiee)
		     (do ((options (cddr modifiee) (cddr options)))
			 ((null options))
		       (case (car options)
			 (:type
			  (setf (cadddr islot) (cadr options)))
			 (:read-only
			  (setf (cadr (cddddr islot)) (cadr options)))
			 (t
			  (error "Bad option in included slot spec: ~S."
				 (car options)))))))))))))
  ;; Finally parse the slots into Slot-Description objects.
  (do ((slots slots (cdr slots))
       (index (+ (dd-offset defstruct) (if (dd-named defstruct) 1 0))
	      (1+ index))
       (descriptions ()))
      ((null slots)
       (setf (dd-length defstruct) index)
       (setf (dd-slots defstruct) (nreverse descriptions)))
    (let* ((slot (car slots))
	   (name (if (atom slot) slot (car slot))))
      (when (keywordp name)
	(warn "Keyword slot name indicates possible syntax error in DEFSTRUCT ~
	       -- ~S."
	      name))
      (push
       (if (atom slot)
	   (make-defstruct-slot-description
	    :%name (string name)
	    :index index
	    :accessor (concat-pnames (dd-conc-name defstruct) name)
	    :type t)
	   (do ((options (cddr slot) (cddr options))
		(default (cadr slot))
		(type t)
		(read-only nil))
	       ((null options)
		(make-defstruct-slot-description
		 :%name (string name)
		 :index index
		 :accessor (concat-pnames (dd-conc-name defstruct) name)
		 :default default
		 :type type
		 :read-only read-only))
	     (case (car options)
	       (:type (setq type (cadr options)))
	       (:read-only (setq read-only (cadr options))))))
       descriptions))))



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
       (let ((name (%primitive structure-ref obj 0)))
	 (or (eq name (dd-name info))
	     (member name (dd-included-by info) :test #'eq)))))

#+new-compiler
;;; %Defstruct  --  Internal
;;;
;;;    Do miscellaneous load-time actions for the structure described by Info.
;;; Define setters, accessors, copier, predicate, documentation, instantiate
;;; definition in load-time env.  This is only called for default structures.
;;;
(defun %defstruct (info)
  (declare (type defstruct-description info))
  (setf (info type defined-structure-info (dd-name info)) info)
  
  (dolist (slot (dd-slots info))
    (let ((dsd slot))
      (setf (symbol-function (dsd-accessor slot))
	    #'(lambda (structure)
		(declare (optimize (speed 3) (safety 0)))
		(unless (typep-to-structure structure info)
		  (error "Structure for accessor ~S is not a ~S:~% ~S"
			 (dsd-accessor dsd) (dd-name info) structure))
		(%primitive structure-index-ref structure (dsd-index dsd))))
      
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
		  (%primitive structure-index-set structure (dsd-index dsd)
			      new-value))))))

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

	      (let ((len (dd-length info)))
		(declare (fixnum len))
		(do ((i 1 (1+ i))
		     (res (%primitive alloc-g-vector len nil)))
		    ((= i len)
		     (%primitive structure-set res (dd-name info) 0)
		     (structurify res))
		  (declare (fixnum i))
		  (%primitive structure-index-set res i
			      (%primitive structure-index-ref structure i)))))))
  (when (dd-doc info)
    (setf (documentation (dd-name info) 'type) (dd-doc info))))


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


;;; Define-Constructor returns a definition for the constructor function of the
;;; given Defstruct.  If the structure is implemented as a vector and is named,
;;; we structurify it.  If the structure is a vector of some specialized type,
;;; we can't use the Vector function.
;;;
;;; If we are defining safe accessors, we also check the types of the values to
;;; make sure that they are legal.
;;;
(defun define-constructor (defstruct)
  (let ((name (dd-constructor defstruct)))
    (when name
      (let* ((initial-cruft
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
		`(truly-the ,(dd-name defstruct)
			    (structurify
			     (vector ,@initial-cruft ,@names))))
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
			  sets)))))))))))



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
	      `(truly-the ,(dd-name defstruct)
			  (structurify (vector ,@initial-cruft ,@thing))))
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


;;; Structure-Predicate  --  Internal
;;;
;;;    The typep transform in typetran calls this function when it encounters
;;; an unknown symbol type specifier.  If the referred-to type is in fact a
;;; structure type that has a predicate, then we open-code the normal case of
;;; an exact match, and otherwise call the predicate.
;;;
(defun structure-predicate (object type)
  (let ((def (info type structure-info type)))
    (if (and def (eq (dd-type def) 'structure) (dd-predicate def))
	`(and (structurep ,object)
	      (if (eq (%primitive structure-ref ,object 0) ',type)
		  t
		  (,(dd-predicate def) ,object)))
	`(lisp::structure-typep ,object ',type))))


;;; Random sorts of stuff.

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (write-string "#S(" stream)
  (prin1 (svref structure 0) stream)
  (do ((index 1 (1+ index))
       (length (length structure))
       (slots (dd-slots (info type defined-structure-info (svref structure 0)))
	      (cdr slots)))
      ((or (= index length)
	   (and *print-length*
		(= index *print-length*)))
       (if (= index length)
	   (write-string ")" stream)
	   (write-string "...)" stream)))
    (write-char #\space stream)
    (prin1 (dsd-name (car slots)) stream)
    (write-char #\space stream)
    (prin1 (svref structure index) stream)))
