;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the public domain, and is provided 'as is'.

(file-comment
  "$Header: src/pcl/cmucl-documentation.lisp $")
;;;
;;; **********************************************************************
;;;
;;; ANSI CL documentation for CMUCL
;;; 

(in-package :pcl)

;;; Note some cases are handled by the documentation methods in
;;; std-class.lisp.

;;; Functions, macros, and special forms.
(defmethod documentation ((x function) (doc-type (eql 't)))
  (lisp::function-doc x))

(defmethod documentation ((x function) (doc-type (eql 'function)))
  (lisp::function-doc x))

(defmethod documentation ((x list) (doc-type (eql 'function)))
  (when (valid-function-name-p x)
    (if (eq (car x) 'setf)
	(or (values (info setf documentation (cadr x)))
	    (and (fboundp x)
		 (documentation (fdefinition x) t)))
	(or (values (info function documentation x))
	    (and (fboundp x)
		 (documentation (fdefinition x) t))))))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (or (values (info function documentation x))
      ;; Try the pcl function documentation.
      (and (fboundp x) (documentation (fdefinition x) 't))))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (values (info setf documentation x)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (unless (valid-function-name-p x)
    (simple-program-error _"Invalid function name ~s" x))
  (if (eq 'setf (cadr x))
      (progn
	#+(or)
	(when new-value
	  (format t "Setting function ~S domain to ~A~%"
		(cadr x) intl::*default-domain*))
	(setf (info setf textdomain (cadr x)) intl::*default-domain*)
	(setf (info setf documentation (cadr x)) new-value))
      (progn
	#+(or)
	(when new-value
	  (format t "Setting function ~S domain to ~A~%"
		(cadr x) intl::*default-domain*))
	(setf (info function textdomain x) intl::*default-domain*)
	(setf (info function documentation x) new-value)))
  new-value)

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  #+(or)
  (when new-value
    (format t "Setting function ~S domain to ~A~%" x intl::*default-domain*))
  (setf (info function textdomain x) intl::*default-domain*)
  (setf (info function documentation x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  #+(or)
  (when new-value
    (format t "Setting function ~S domain to ~A~%" x intl::*default-domain*))
  (setf (info function textdomain x) intl::*default-domain*)
  (setf (info function documentation x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  #+(or)
  (when new-value
    (format t "Setting function ~S domain to ~A~%" x intl::*default-domain*))
  (setf (info function textdomain x) intl::*default-domain*)
  (setf (info function documentation x) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  #+(or)
  (when new-value
    (format t "Setting setf function ~S domain to ~A~%" x intl::*default-domain*))
  (setf (info setf textdomain x) intl::*default-domain*)
  (setf (info setf documentation x) new-value))

;;; Packages.
(defmethod documentation ((x package) (doc-type (eql 't)))
  (lisp::package-doc-string x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (lisp::package-doc-string x) new-value))

;;; Types, classes, and structure names.
(defmethod documentation ((x kernel::structure-class) (doc-type (eql 't)))
  (values (info type documentation (kernel:%class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (values (info type documentation (class-name x))))

(defmethod documentation ((x kernel::standard-class) (doc-type (eql 't)))
  (or (values (info type documentation (kernel:%class-name x)))
      (let ((pcl-class (kernel:%class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x kernel::structure-class) (doc-type (eql 'type)))
  (values (info type documentation (kernel:%class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (values (info type documentation (class-name x))))

(defmethod documentation ((x kernel::standard-class) (doc-type (eql 'type)))
  (or (values (info type documentation (kernel:%class-name x)))
      (let ((pcl-class (kernel:%class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (or (values (info type documentation x))
      (let ((class (find-class x nil)))
	(when class
	  (plist-value class 'documentation)))))

#+nil
(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (when (eq (info type kind x) :instance)
    (values (info type documentation x))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (cond ((eq (info type kind x) :instance)
	 (values (info type documentation x)))
	((info typed-structure info x)
	 (values (info typed-structure documentation x)))
	(t
	 nil)))

(defmethod (setf documentation) (new-value (x kernel::structure-class) (doc-type (eql 't)))
  (setf (info type textdomain (kernel:%class-name x)) intl::*default-domain*)
  (setf (info type documentation (kernel:%class-name x)) new-value))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 't)))
  (setf (info type textdomain x) intl::*default-domain*)
  (setf (info type documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value (x kernel::structure-class) (doc-type (eql 'type)))
  (setf (info type textdomain x) intl::*default-domain*)
  (setf (info type documentation (kernel:%class-name x)) new-value))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 'type)))
  (setf (info type textdomain x) intl::*default-domain*)
  (setf (info type documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (if (or (structure-type-p x) (condition-type-p x))
      (progn
	(setf (info type textdomain x) intl::*default-domain*)
	(setf (info type documentation x) new-value))
      (let ((class (find-class x nil)))
	(if class
	    (setf (plist-value class 'documentation) new-value)
	    (progn
	      (setf (info type textdomain x) intl::*default-domain*)
	      (setf (info type documentation x) new-value))))))

#+nil
(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'structure)))
  (unless (eq (info type kind x) :instance)
    (simple-program-error _"~@<~S is not the name of a structure type.~@:>" x))
  (setf (info type documentation x) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'structure)))
  (cond ((eq (info type kind x) :instance)
	 (setf (info type textdomain x) intl::*default-domain*)
	 (setf (info type documentation x) new-value))
	((info typed-structure info x)
	 (setf (info typed-structure textdomain x) intl::*default-domain*)
	 (setf (info typed-structure documentation x) new-value))
	(t
	 (simple-program-error _"~@<~S is not the name of a structure type.~@:>" x))))

;;; Variables.
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (info variable documentation x)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  #+(or)
  (format t "Setting variable ~S domain to ~A~%" x intl::*default-domain*)
  (setf (info variable textdomain x) intl::*default-domain*)
  (setf (info variable documentation x) new-value))

;;; Compiler macros
(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (when (valid-function-name-p x)
    (if (eq (car x) 'setf)
	(cdr (assoc doc-type (values (info random-documentation stuff (cadr x)))))
	(cdr (assoc doc-type (values (info random-documentation stuff x)))))))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (when (valid-function-name-p x)
    (if (eq (car x) 'setf)
	(set-random-documentation (cadr x) doc-type new-value)
	(set-random-documentation x doc-type new-value)))
  new-value)

;;; CMUCL random documentation. Compiler-macro documentation is stored
;;; as random-documentation and handled here.
(defmethod documentation ((x symbol) (doc-type symbol))
  (cdr (assoc doc-type
	      (values (info random-documentation stuff x)))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type symbol))
  (set-random-documentation x doc-type new-value)
  new-value)

;;; Define AROUND methods to translate the docstring.
(macrolet
    ((frob (dt)
	`(defmethod documentation :around ((x t) (doc-type (eql ',dt)))
	   (let ((doc (call-next-method))
		 (domain (info ,dt :textdomain x)))
	     (or (and doc (intl:dgettext domain doc))
		 doc)))))
  (frob function)
  (frob setf)
  (frob type)
  (frob variable))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (let ((doc (call-next-method))
	(domain (cond ((eq (info type kind x) :instance)
		       (values (info type textdomain x)))
		      ((info typed-structure info x)
		       (values (info typed-structure textdomain x)))
		      (t
		       nil))))
    (or (and doc (intl:dgettext domain doc))
	doc)))

  

;;; Replace the minimal documentation function with the PCL version
;;; when loaded.
(eval-when (:load-toplevel)
  (setf (symbol-function 'lisp:documentation) #'documentation)
  (setf (documentation 'documentation 'function)
    _N"Returns the documentation string of Doc-Type for X, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T.")
  (setf (fdefinition '(setf lisp:documentation)) #'(setf documentation)))
