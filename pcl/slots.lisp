;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(in-package 'pcl)

;;;
;;; These four functions work on std-instances and fsc-instances.  These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class.  Many of these methods call these four functions.
;;;

#||
(proclaim '(inline get-wrapper get-slots))

(defun get-wrapper (inst)
  (cond ((std-instance-p inst) (std-instance-wrapper inst))
	((fsc-instance-p inst) (fsc-instance-wrapper inst))
	(t (error "What kind of instance is this?"))))

(defun get-slots (inst)
  (cond ((std-instance-p inst) (std-instance-slots inst))
	((fsc-instance-p inst) (fsc-instance-slots inst))
	(t (error "What kind of instance is this?"))))
||#

(defmacro get-wrapper (inst)
  `(cond ((std-instance-p ,inst) (std-instance-wrapper ,inst))
	 ((fsc-instance-p ,inst) (fsc-instance-wrapper ,inst))
	 (t (error "What kind of instance is this?"))))

(defmacro get-slots (inst)
  `(cond ((std-instance-p ,inst) (std-instance-slots ,inst))
	 ((fsc-instance-p ,inst) (fsc-instance-slots ,inst))
	 (t (error "What kind of instance is this?"))))

(defmacro get-slots-or-nil (inst)
  `(cond ((std-instance-p ,inst) (std-instance-slots ,inst))
	 ((fsc-instance-p ,inst) (fsc-instance-slots ,inst))
	 (t nil)))

(defun set-wrapper (inst new)
  (cond ((std-instance-p inst)
	 (setf (std-instance-wrapper inst) new))
	((fsc-instance-p inst)
	 (setf (fsc-instance-wrapper inst) new))
	(t
	 (error "What kind of instance is this?"))))

(defun set-slots (inst new)
  (cond ((std-instance-p inst)
	 (setf (std-instance-slots inst) new))
	((fsc-instance-p inst)
	 (setf (fsc-instance-slots inst) new))
	(t
	 (error "What kind of instance is this?"))))






(defun get-class-slot-value-1 (object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper) object slot-name 'slot-value)
	(if (eq (cdr entry) *slot-unbound*)
	    (slot-unbound (wrapper-class wrapper) object slot-name)
	    (cdr entry)))))

(defun set-class-slot-value-1 (new-value object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper)
		      object
		      slot-name
		      'setf
		      new-value)
	(setf (cdr entry) new-value))))

(defmethod class-slot-value ((class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (get-class-slot-value-1 prototype wrapper slot-name)))

(defmethod (setf class-slot-value) (nv (class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (set-class-slot-value-1 nv prototype wrapper slot-name)))


(defmacro slot-symbol (slot-name type)
  `(if (and (symbolp ,slot-name) (symbol-package ,slot-name))
       (intern (format nil "~A ~A slot ~a" 
		       (package-name (symbol-package ,slot-name))
		       (symbol-name ,slot-name)
		       ,(symbol-name type))
	        *slot-accessor-name-package*)
       (progn 
	 (error "non-symbol and non-interned symbol slot name accessors~
                 are not yet implemented")
	 ;;(make-symbol (format nil "~a ~a" ,slot-name ,type))
	 )))

(defun slot-reader-symbol (slot-name)
  (slot-symbol slot-name reader))

(defun slot-writer-symbol (slot-name)
  (slot-symbol slot-name writer))

(defun slot-boundp-symbol (slot-name)
  (slot-symbol slot-name boundp))

(defun find-slot-definition (class slot-name)
  (dolist (slot (class-slots class) nil)
    (when (eql slot-name (slot-definition-name slot))
      (return slot))))

(defun no-slot (slot-name sym)
  (error "No class has a slot named ~S (~s has no function binding)
          (or maybe your files were compiled with an old version of PCL:~
          try recompiling.)"
	 slot-name sym))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-value)
	(slot-value-using-class class object slot-definition))))

(setf (symbol-function 'slot-value-normal) #'slot-value)

(defmacro accessor-slot-value (object slot-name)
  (unless (constantp slot-name)
    (error "~s requires its slot-name argument to be a constant" 
	   'accessor-slot-value))
  (let* ((slot-name (eval slot-name))
	 (sym (slot-reader-symbol slot-name)))
    `(function-funcall (if (fboundp ',sym)
			   (symbol-function ',sym)
			   (no-slot ',slot-name ',sym))
                       ,object)))

(define-compiler-macro slot-value (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-value ,object-form ,slot-name-form)
      `(slot-value-normal ,object-form ,slot-name-form)))

(defun set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'setf)
	(setf (slot-value-using-class class object slot-definition) 
	      new-value))))

(setf (symbol-function 'set-slot-value-normal) #'set-slot-value)

(defmacro accessor-set-slot-value (object slot-name new-value &environment env)
  (unless (constantp slot-name)
    (error "~s requires its slot-name argument to be a constant" 
	   'accessor-set-slot-value))
  (setq object (macroexpand object env))
  (setq slot-name (macroexpand slot-name env))
  (let* ((slot-name (eval slot-name))
	 (bindings (unless (or (constantp new-value) (atom new-value))
		     (let ((object-var (gensym)))
		       (prog1 `((,object-var ,object))
			 (setq object object-var)))))
	 (sym (slot-writer-symbol slot-name))
	 (form `(function-funcall (if (fboundp ',sym)
				      (symbol-function ',sym)
				      (no-slot ',slot-name ',sym))
		                  ,new-value
		                  ,object)))
    (if bindings
	`(let ,bindings ,form)
	form)))

(define-compiler-macro set-slot-value (object-form slot-name-form new-value-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-set-slot-value ,object-form ,slot-name-form ,new-value-form)
      `(set-slot-value-normal ,object-form ,slot-name-form ,new-value-form)))

(defconstant *optimize-slot-boundp* nil)

(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-boundp)
	(slot-boundp-using-class class object slot-definition))))

(setf (symbol-function 'slot-boundp-normal) #'slot-boundp)

(defun get-boundp-function1 (symbol slot-name)
  (setf (symbol-function symbol) (get-boundp-function slot-name)))

(defmacro accessor-slot-boundp (object slot-name)
  (unless (constantp slot-name)
    (error "~s requires its slot-name argument to be a constant" 
	   'accessor-slot-boundp))
  (let* ((slot-name (eval slot-name))
	 (sym (slot-boundp-symbol slot-name)))
    (if (not *optimize-slot-boundp*)
	`(slot-boundp-normal ,object ',slot-name)
	`(function-funcall (if (fboundp ',sym)
			       (symbol-function ',sym)
			       (no-slot ',slot-name ',sym))
	                   ,object))))

(define-compiler-macro slot-boundp (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-boundp ,object-form ,slot-name-form)
      `(slot-boundp-normal ,object-form ,slot-name-form)))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'slot-makunbound)
        (slot-makunbound-using-class class object slot-definition))))

(defun slot-exists-p (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (and slot-definition
	 (slot-exists-p-using-class class object slot-definition))))

;;;
;;; This isn't documented, but is used within PCL in a number of print
;;; object methods (see named-object-print-function).
;;; 
(defun slot-value-or-default (object slot-name &optional (default "unbound"))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))


;;;
;;; 
;;; 
(defun standard-instance-access (instance location)
  (%svref (std-instance-slots instance) location))

(defun funcallable-standard-instance-access (instance location)
  (%svref (fsc-instance-slots instance) location))

(defmethod slot-value-using-class ((class std-class)
                                   (object standard-object)
                                   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (unless (eq 't (wrapper-state (std-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%svref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%svref (fsc-instance-slots object) location))
			 (t (error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
			  slotd 'slot-value-using-class)))))
    (if (eq value *slot-unbound*)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
	   (new-value (class std-class)
		      (object standard-object)
		      (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (unless (eq 't (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%svref (std-instance-slots object) location) new-value))
	     ((fsc-instance-p object)
	      (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%svref (fsc-instance-slots object) location) new-value))
	     (t (error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) new-value))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
	      slotd '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
	   ((class std-class) 
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum 
		   (cond ((std-instance-p object)
			  (unless (eq 't (wrapper-state (std-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%svref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
			    (check-wrapper-validity object))
			  (%svref (fsc-instance-slots object) location))
			 (t (error "What kind of instance is this?"))))
		  (cons
		   (cdr location))
		  (t
		   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
			  slotd 'slot-boundp-using-class)))))
    (not (eq value *slot-unbound*))))

(defmethod slot-makunbound-using-class
	   ((class std-class)
	    (object standard-object) 
	    (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
	      (unless (eq 't (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%svref (std-instance-slots object) location) *slot-unbound*))
	     ((fsc-instance-p object)
	      (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
	      (setf (%svref (fsc-instance-slots object) location) *slot-unbound*))
	     (t (error "What kind of instance is this?"))))
      (cons
       (setf (cdr location) *slot-unbound*))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
	      slotd 'slot-makunbound-using-class))))
  nil)

(defmethod slot-exists-p-using-class
	   ((class std-class)
	    (object standard-object)
	    (slotd standard-effective-slot-definition))
  t)

(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-reader-function slotd)))
    #+cmu (declare (type function function))
    (funcall function object)))

(defmethod (setf slot-value-using-class)
    (new-value (class structure-class)
	       (object structure-object)
	       (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-writer-function slotd)))
    #+cmu (declare (type function function))
    (funcall function new-value object)))

(defmethod slot-boundp-using-class
	   ((class structure-class) 
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class
	   ((class structure-class)
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  (error "Structure slots can't be unbound"))


(defmethod slot-missing
	   ((class t) instance slot-name operation &optional new-value)
  (error "When attempting to ~A,~%the slot ~S is missing from the object ~S."
	 (ecase operation
	   (slot-value "read the slot's value (slot-value)")
	   (setf (format nil
			 "set the slot's value to ~S (setf of slot-value)"
			 new-value))
	   (slot-boundp "test to see if slot is bound (slot-boundp)")
	   (slot-makunbound "make the slot unbound (slot-makunbound)"))
	 slot-name
	 instance))

(defmethod slot-unbound ((class t) instance slot-name)
  (error "The slot ~S is unbound in the object ~S." slot-name instance))


(defun structure-slot-boundp (object)
  (declare (ignore object))
  t)

(defun get-optimized-std-accessor-method-function (class slotd name)
  (if (structure-class-p class)
      (ecase name
	(reader (slot-definition-internal-reader-function slotd))
	(writer (slot-definition-internal-writer-function slotd))
	(boundp #'structure-slot-boundp))
      (let* ((fsc-p (cond ((standard-class-p class) nil)
			  ((funcallable-standard-class-p class) t)
			  (t (error "~S is not a standard-class" class))))
	     (slot-name (slot-definition-name slotd))
	     (index (slot-definition-location slotd))
	     (function (ecase name
			 (reader 'make-optimized-std-reader-method-function)
			 (writer 'make-optimized-std-writer-method-function)
			 (boundp 'make-optimized-std-boundp-method-function)))
	     (value (funcall function fsc-p slot-name index)))
	(values value index))))

(defun make-optimized-std-reader-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 #'(lambda (instance)
		     (let ((value (%svref (fsc-instance-slots instance) index)))
		       (if (eq value *slot-unbound*)
			   (slot-unbound (class-of instance) instance slot-name)
			   value)))
		 #'(lambda (instance)
		     (let ((value (%svref (std-instance-slots instance) index)))
		       (if (eq value *slot-unbound*)
			   (slot-unbound (class-of instance) instance slot-name)
			   value)))))
     (cons   #'(lambda (instance)
		 (let ((value (cdr index)))
		   (if (eq value *slot-unbound*)
		       (slot-unbound (class-of instance) instance slot-name)
		       value)))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 #'(lambda (nv instance)
		     (setf (%svref (fsc-instance-slots instance) index) nv))
		 #'(lambda (nv instance)
		     (setf (%svref (std-instance-slots instance) index) nv))))
     (cons   #'(lambda (nv instance)
		 (declare (ignore instance))
		 (setf (cdr index) nv))))
   `(writer ,slot-name)))

(defun make-optimized-std-boundp-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (if fsc-p
		 #'(lambda (instance)
		     (not (eq *slot-unbound*
			      (%svref (fsc-instance-slots instance) index))))
		 #'(lambda (instance)
		     (not (eq *slot-unbound* 
			      (%svref (std-instance-slots instance) index))))))
     (cons   #'(lambda (instance)
		 (declare (ignore instance))
		 (not (eq *slot-unbound* (cdr index))))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function (function)
  #+cmu (declare (type function function))
  #'(lambda (class object slotd)
      (declare (ignore class slotd))
      (funcall function object)))

(defun make-optimized-structure-setf-slot-value-using-class-method-function (function)
  #+cmu (declare (type function function))
  #'(lambda (nv class object slotd)
      (declare (ignore class slotd))
      (funcall function nv object)))

(defun optimized-structure-slot-boundp-using-class-method-function (class object slotd)
  (declare (ignore class object slotd))
  t)

(defun get-optimized-std-slot-value-using-class-method-function (class slotd name)
  (if (structure-class-p class)
      (ecase name
	(reader (make-optimized-structure-slot-value-using-class-method-function
		 (slot-definition-internal-reader-function slotd)))
	(writer (make-optimized-structure-setf-slot-value-using-class-method-function
		 (slot-definition-internal-writer-function slotd)))
	(boundp #'optimized-structure-slot-boundp-using-class-method-function))
      (let* ((fsc-p (cond ((standard-class-p class) nil)
			  ((funcallable-standard-class-p class) t)
			  (t (error "~S is not a standard-class" class))))
	     (slot-name (slot-definition-name slotd))
	     (index (slot-definition-location slotd))
	     (function 
	      (ecase name
		(reader 
		 #'make-optimized-std-slot-value-using-class-method-function)
		(writer 
		 #'make-optimized-std-setf-slot-value-using-class-method-function)
		(boundp 
		 #'make-optimized-std-slot-boundp-using-class-method-function))))
	#+cmu (declare (type function function))
	(values (funcall function fsc-p slot-name index) index))))

(defun make-optimized-std-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (etypecase index
    (fixnum (if fsc-p
		#'(lambda (class instance slotd)
		    (declare (ignore slotd))
		    (unless (fsc-instance-p instance) (error "not fsc"))
		    (let ((value (%svref (fsc-instance-slots instance) index)))
		      (if (eq value *slot-unbound*)
			  (slot-unbound class instance slot-name)
			  value)))
		#'(lambda (class instance slotd)
		    (declare (ignore slotd))
		    (unless (std-instance-p instance) (error "not std"))
		    (let ((value (%svref (std-instance-slots instance) index)))
		      (if (eq value *slot-unbound*)
			  (slot-unbound class instance slot-name)
			  value)))))
    (cons   #'(lambda (class instance slotd)
		(declare (ignore slotd))
		(let ((value (cdr index)))
		  (if (eq value *slot-unbound*)
		      (slot-unbound class instance slot-name)
		      value))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		#'(lambda (nv class instance slotd)
		    (declare (ignore class slotd))
		    (setf (%svref (fsc-instance-slots instance) index) nv))
		#'(lambda (nv class instance slotd)
		    (declare (ignore class slotd))
		    (setf (%svref (std-instance-slots instance) index) nv))))
    (cons   #'(lambda (nv class instance slotd)
		(declare (ignore class instance slotd))
		(setf (cdr index) nv)))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		#'(lambda (class instance slotd)
		    (declare (ignore class slotd))
		    (not (eq *slot-unbound* 
			     (%svref (fsc-instance-slots instance) index))))
		#'(lambda (class instance slotd)
		    (declare (ignore class slotd))
		    (not (eq *slot-unbound* 
			     (%svref (std-instance-slots instance) index))))))
    (cons   #'(lambda (class instance slotd)
		(declare (ignore class instance slotd))
		(not (eq *slot-unbound* (cdr index)))))))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  #+cmu (declare (type function sdfun))
  (set-function-name
   (case name
     (reader #'(lambda (instance) (funcall sdfun class instance slotd)))
     (writer #'(lambda (nv instance) (funcall sdfun nv class instance slotd)))
     (boundp #'(lambda (instance) (funcall sdfun class instance slotd))))
   `(,name ,(class-name class) ,(slot-definition-name slotd))))

(defun make-internal-reader-method-function (slot-name)
  #'(lambda (instance)
      (let ((wrapper (cond ((std-instance-p instance) 
			    (std-instance-wrapper instance))
			   ((fsc-instance-p instance) 
			    (fsc-instance-wrapper instance)))))
	(if wrapper
	    (let* ((class (wrapper-class wrapper))
		   (index (or (instance-slot-index wrapper slot-name)
			      (assq slot-name (wrapper-class-slots wrapper)))))
	      (typecase index
		(fixnum 	
		 (let ((value (%svref (get-slots instance) index)))
		   (if (eq value *slot-unbound*)
		       (slot-unbound (class-of instance) instance slot-name)
		       value)))
		(cons
		 (let ((value (cdr index)))
		   (if (eq value *slot-unbound*)
		       (slot-unbound (class-of instance) instance slot-name)
		       value)))
		(t
		 (error "The wrapper for class ~S does not have the slot ~S"
			class slot-name))))
	    (slot-value instance slot-name)))))




(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let* ((class-wrapper (class-wrapper class))
	 (instance (%allocate-instance--class
		     (class-no-of-instance-slots class))))
    (setf (std-instance-wrapper instance) class-wrapper)
    instance))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
	(funcall constructor)
	(error "Can't allocate an instance of class ~S" (class-name class)))))

