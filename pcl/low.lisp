;;;-*-Mode:LISP; Package: PCL -*-
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

(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/low.lisp,v 1.17 2002/09/07 13:28:46 pmai Exp $")

;;; 
;;; This file contains optimized low-level constructs for PCL.
;;; 

(in-package :pcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *optimize-speed*
  '(optimize (speed 3) (safety 0) (ext:inhibit-warnings 3) #+small (debug 0.5))
  "This variable is used in various places in PCL as an optimization
declaration.")
)

;;; Various macros that include necessary declarations for maximum
;;; performance.

(defmacro %svref (vector index)
  `(locally (declare #.*optimize-speed*
		     (inline svref))
	    (svref (the simple-vector ,vector) (the fixnum ,index))))

(defsetf %svref %set-svref)

(defmacro %set-svref (vector index new-value)
  `(locally (declare #.*optimize-speed*
		     (inline svref))
     (setf (svref (the simple-vector ,vector) (the fixnum ,index))
	   ,new-value)))

;;;
;;; With-Pcl-Lock
;;;
;;; Evaluate the body in such a way that no other code that is
;;; running PCL can be run during that evaluation.
;;;
;;; Note that the MP version, which uses a PCL-specific lock
;;; is rather experimental, in that it is not currently clear
;;; if the code inside with-pcl-lock only has to prevent other
;;; threads from entering such sections, or if it really has to
;;; prevent _ALL_ other PCL code (e.g. GF invocations, etc.)
;;; from running.  If the latter then we really need to identify
;;; all places that need to acquire the PCL lock, if we are going to
;;; support multiple concurrent threads/processes on SMP machines.
;;; 
;;; For the moment we do the experimental thing, and fix any bugs
;;; that occur as a result of this.             -- PRM 2002-09-06
;;;

#-MP
(defmacro with-pcl-lock (&body body)
  `(sys:without-interrupts ,@body))

#+MP
(defvar *global-pcl-lock* (mp:make-lock "Global PCL Lock"))

#+MP
(defmacro with-pcl-lock (&body body)
  `(mp:with-lock-held (*global-pcl-lock*)
     ,@body))



;;;
;;; FUNCTION-ARGLIST
;;;
;;; Given something which is functionp, function-arglist should return the
;;; argument list for it.  PCL does not count on having this available, but
;;; MAKE-SPECIALIZABLE works much better if it is available.

(defun function-arglist (fcn)
  "Returns the argument list of a compiled function, if possible."
  (cond ((symbolp fcn)
         (when (fboundp fcn)
           (function-arglist (symbol-function fcn))))
        ((eval:interpreted-function-p fcn)
         (eval:interpreted-function-arglist fcn))
        ((functionp fcn)
         (let ((lambda-expr (function-lambda-expression fcn)))
           (if lambda-expr
               (cadr lambda-expr)
               (let* ((function (kernel:%closure-function fcn))
		      (arglist-string (kernel:%function-arglist function)))
		 (when arglist-string
		       (values (read-from-string arglist-string)))))))))

;;;
;;; set-function-name
;;; When given a function should give this function the name <new-name>.
;;; Note that <new-name> is sometimes a list.  Some lisps get the upset
;;; in the tummy when they start thinking about functions which have
;;; lists as names.  To deal with that there is intern-function-name
;;; which takes a list spec for a function name and turns it into a symbol
;;; if need be.
;;;
;;; When given a funcallable instance, set-function-name MUST side-effect
;;; that FIN to give it the name.  When given any other kind of function
;;; set-function-name is allowed to return new function which is the 'same'
;;; except that it has the name.
;;;
;;; In all cases, set-function-name must return the new (or same) function.
;;; 
(defun set-function-name (fcn new-name)
  "Set the name of a compiled function object and return the function."
  (declare (special *boot-state* *the-class-standard-generic-function*))
  (cond ((symbolp fcn)
         (set-function-name (symbol-function fcn) new-name))
        ((funcallable-instance-p fcn)
	 (if (if (eq *boot-state* 'complete)
		 (typep fcn 'generic-function)
		 (eq (class-of fcn) *the-class-standard-generic-function*))
	     (setf (kernel:%funcallable-instance-info fcn 1) new-name)
	     (typecase fcn
	       (kernel:byte-closure
		(set-function-name (kernel:byte-closure-function fcn)
				   new-name))
	       (kernel:byte-function
		(setf (kernel:byte-function-name fcn) new-name))
	       (eval:interpreted-function
		(setf (eval:interpreted-function-name fcn) new-name))))
         fcn)
        (t fcn)))

;;;
;;; Note: While we don't need intern-function-name for
;;; set-function-name, this is used in boot.lisp.
;;;
(defun intern-function-name (name)
  (cond ((symbolp name) name)
	((listp name)
	 (intern (let ((*package* *the-pcl-package*)
		       (*print-case* :upcase)
		       (*print-pretty* nil)
		       (*print-gensym* 't))
		   (format nil "~S" name))
		 *the-pcl-package*))))


;;;
;;; COMPILE-LAMBDA
;;;
;;; This is called by PCL to compile generated code (i.e. lambda
;;; forms).
;;;
(defvar *compile-lambda-break-p* nil
  "PCL debugging aid that breaks into the debugger each time
`compile-lambda' is invoked.")

(defvar *compile-lambda-silent-p* t
  "If true (the default), then `compile-lambda' will try to silence
the compiler as completely as possible.  Currently this means that
`*compile-print*' will be bound to nil during compilation.")

(defun compile-lambda (lambda-form)
  (when *compile-lambda-break-p* (break))
  (let ((*compile-print* (if *compile-lambda-silent-p* nil *compile-print*)))
    (compile nil lambda-form)))

;;;
;;; This macro will precompile various PCL-generated code fragments,
;;; so that those won't have to be compiled lazily at run-time.  For
;;; correct usage the invokation of `precompile-random-code-segments'
;;; needs to be put in a file, which is compiled via `compile-file',
;;; and then loaded.
;;;

(defmacro precompile-random-code-segments (&optional system)
  `(progn
     (eval-when (:compile-toplevel)
       (update-dispatch-dfuns)
       (compile-iis-functions nil))
     (precompile-function-generators ,system)
     (precompile-dfun-constructors ,system)
     (precompile-iis-functions ,system)
     (eval-when (:load-toplevel)
       (compile-iis-functions t))))


;;;; STD-INSTANCE

;;; Under CMU17 conditional, STD-INSTANCE-P is only used to discriminate
;;; between functions (including FINs) and normal instances, so we can return
;;; true on structures also.  A few uses of (or std-instance-p fsc-instance-p)
;;; are changed to pcl-instance-p.
;;;
(defmacro std-instance-p (x)
  `(kernel:%instancep ,x))

;;; PCL-INSTANCE-P is implemented via a compiler transform so that the
;;; test can be optimised away when the result is known, such as is
;;; typically the case during slot access within methods, see
;;; get-slots-or-nil below.

(in-package "C")

(defknown pcl::pcl-instance-p (t) boolean
  (movable foldable flushable explicit-check))

(deftransform pcl::pcl-instance-p ((object))
  (let* ((otype (continuation-type object))
	 (std-obj (specifier-type 'pcl::std-object)))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((csubtypep otype std-obj) 't)
      ((not (types-intersect otype std-obj)) 'nil)
      (t
       `(typep (kernel:layout-of object) 'pcl::wrapper)))))

(in-package "PCL")

;;; Definition for interpreted code.
(defun pcl-instance-p (x)
  (typep (kernel:layout-of x) 'wrapper))


;;; We define this as STANDARD-INSTANCE, since we're going to clobber the
;;; layout with some standard-instance layout as soon as we make it, and we
;;; want the accesor to still be type-correct.
;;;
(defstruct (standard-instance
	    (:predicate nil)
	    (:constructor %%allocate-instance--class ())
	    (:alternate-metaclass kernel:instance lisp:standard-class
				  kernel:make-standard-class))
  (slots nil))

;;; Both of these operations "work" on structures, which allows the above
;;; weakening of std-instance-p.
;;;
(defmacro std-instance-slots (x) `(kernel:%instance-ref ,x 1))
(defmacro std-instance-wrapper (x) `(kernel:%instance-layout ,x))

(defmacro built-in-or-structure-wrapper (x) `(kernel:layout-of ,x))

(defmacro get-wrapper (inst)
  (ext:once-only ((wrapper `(wrapper-of ,inst)))
    `(progn
       (assert (typep ,wrapper 'wrapper) () "What kind of instance is this?")
       ,wrapper)))

(defmacro get-instance-wrapper-or-nil (inst)
  (ext:once-only ((wrapper `(wrapper-of ,inst)))
    `(if (typep ,wrapper 'wrapper)
	 ,wrapper
	 nil)))

(defmacro get-slots (inst)
  `(cond ((std-instance-p ,inst) (std-instance-slots ,inst))
	 ((fsc-instance-p ,inst) (fsc-instance-slots ,inst))
	 (t (error "What kind of instance is this?"))))

(defmacro get-slots-or-nil (inst)
  (ext:once-only ((n-inst inst))
    `(when (pcl-instance-p ,n-inst)
       (if (std-instance-p ,n-inst)
	   (std-instance-slots ,n-inst)
	   (fsc-instance-slots ,n-inst)))))

(defun print-std-instance (instance stream depth) ;A temporary definition used
  (declare (ignore depth))		          ;for debugging the bootstrap
  (printing-random-thing (instance stream)        ;code of PCL (See high.lisp).
    (let ((class (class-of instance)))
      (if (or (eq class (find-class 'standard-class nil))
	      (eq class (find-class 'funcallable-standard-class nil))
	      (eq class (find-class 'built-in-class nil)))
	  (format stream "~a ~a" (early-class-name class)
		  (early-class-name instance))
	  (format stream "~a" (early-class-name class))))))

;;; Slot access itself

(defmacro %instance-ref (slots index)
  `(%svref ,slots ,index))

(defmacro instance-ref (slots index)
  `(svref ,slots ,index))

;;;
;;; This is the value that we stick into a slot to tell us that it is unbound.
;;; It may seem gross, but for performance reasons, we make this an interned
;;; symbol.  That means that the fast check to see if a slot is unbound is to
;;; say (EQ <val> '..SLOT-UNBOUND..).  That is considerably faster than looking
;;; at the value of a special variable.  Be careful, there are places in the
;;; code which actually use ..slot-unbound.. rather than this variable.  So
;;; much for modularity.
;;;
;;; Actually at the current point in time it seems that all code now
;;; uses *slot-unbound* instead of ..slot-unbound.. itself.
;;; 
(defconstant *slot-unbound* '..slot-unbound..)


;;;; Structure-instance stuff:

(defun structure-instance-p (x)
  (typep x 'lisp:structure-object))

(defun structurep (x)
  (typep x 'lisp:structure-object))

(defun structure-type (x)
  (lisp:class-name (kernel:layout-class (kernel:%instance-layout x))))


(defun structure-type-p (type)
  (and (symbolp type)
       (let ((class  (lisp:find-class type nil)))
	 (and class
	      ;; class may not be complete if created by
	      ;; inform-type-system-aboutd-std-class
	      (kernel:class-layout class)
	      (typep (kernel:layout-info (kernel:class-layout class))
		     'kernel:defstruct-description)))))

(defun get-structure-dd (type)
  (kernel:layout-info (kernel:class-layout (lisp:find-class type))))

(defun structure-type-included-type-name (type)
  (let ((include (kernel::dd-include (get-structure-dd type))))
    (if (consp include)
	(car include)
	include)))

(defun structure-type-slot-description-list (type)
  (nthcdr (length (let ((include (structure-type-included-type-name type)))
		    (and include (kernel:dd-slots (get-structure-dd include)))))
	  (kernel:dd-slots (get-structure-dd type))))

(defun structure-slotd-name (slotd)
  (kernel:dsd-name slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (kernel:dsd-accessor slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (kernel:dsd-accessor slotd)))

(defun structure-slotd-writer-function (slotd)
  (unless (kernel:dsd-read-only slotd)
    (fdefinition `(setf ,(kernel:dsd-accessor slotd)))))

(defun structure-slotd-type (slotd)
  (kernel:dsd-type slotd))

(defun structure-slotd-init-form (slotd)
  (kernel::dsd-default slotd))


;;;
;;; Extractor for source context information, which is used by the
;;; compiler to indicate progress and context information for error
;;; reporting.
;;;

(in-package "C")

(def-source-context pcl:defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
	`(pcl:defmethod ,name ,@(subseq stuff 0 arg-pos)
	   ,(nth-value 2 (pcl::parse-specialized-lambda-list
			  (elt stuff arg-pos))))
	`(pcl:defmethod ,name "<illegal syntax>"))))
