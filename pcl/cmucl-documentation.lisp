;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/pcl/cmucl-documentation.lisp,v 1.7 1998/06/21 10:02:07 dtc Exp $")
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
  (when (eq (car x) 'setf)	; Give-up if not a setf function name.
    (or (values (ext:info setf documentation (cadr x)))
	;; Try the pcl function documentation.
	(and (fboundp x) (documentation (fdefinition x) 't)))))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (or (values (ext:info function documentation x))
      ;; Try the pcl function documentation.
      (and (fboundp x) (documentation (fdefinition x) 't))))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (values (ext:info setf documentation x)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (setf (ext:info setf documentation (cadr x)) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (setf (ext:info function documentation x) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (setf (ext:info setf documentation x) new-value))

;;; Packages.
(defmethod documentation ((x package) (doc-type (eql 't)))
  (lisp::package-doc-string x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (setf (lisp::package-doc-string x) new-value))

;;; Types, classes, and structure names.
(defmethod documentation ((x lisp:structure-class) (doc-type (eql 't)))
  (values (ext:info type documentation (lisp:class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (values (ext:info type documentation (class-name x))))

(defmethod documentation ((x lisp:standard-class) (doc-type (eql 't)))
  (or (values (ext:info type documentation (lisp:class-name x)))
      (let ((pcl-class (kernel:class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x lisp:structure-class) (doc-type (eql 'type)))
  (values (ext:info type documentation (lisp:class-name x))))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (values (ext:info type documentation (class-name x))))

(defmethod documentation ((x lisp:standard-class) (doc-type (eql 'type)))
  (or (values (ext:info type documentation (lisp:class-name x)))
      (let ((pcl-class (kernel:class-pcl-class x)))
	(and pcl-class (plist-value pcl-class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (or (values (ext:info type documentation x))
      (let ((class (find-class x nil)))
	(when class
	  (plist-value class 'documentation)))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (when (eq (ext:info type kind x) :instance)
    (values (ext:info type documentation x))))

(defmethod (setf documentation) (new-value (x lisp:structure-class) (doc-type (eql 't)))
  (setf (ext:info type documentation (lisp:class-name x)) new-value))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 't)))
  (setf (ext:info type documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value (x lisp:structure-class) (doc-type (eql 'type)))
  (setf (ext:info type documentation (lisp:class-name x)) new-value))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 'type)))
  (setf (ext:info type documentation (class-name x)) new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (if (structure-type-p x)	; Catch structures first.
      (setf (ext:info type documentation x) new-value)
      (let ((class (find-class x nil)))
	(if class
	    (setf (plist-value class 'documentation) new-value)
	    (setf (ext:info type documentation x) new-value)))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'structure)))
  (unless (eq (ext:info type kind x) :instance)
    (error "~S is not the name of a structure type." x))
  (setf (ext:info type documentation x) new-value))

;;; Variables.
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (ext:info variable documentation x)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  (setf (ext:info variable documentation x) new-value))

;;; CMUCL random documentation. Compiler-macro documentation is stored
;;; as random-documentation and handled here.
(defmethod documentation ((x symbol) (doc-type symbol))
  (cdr (assoc doc-type
	      (values (ext:info random-documentation stuff x)))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type symbol))
  (let ((pair (assoc doc-type (ext:info random-documentation stuff x))))
    (if pair
	(setf (cdr pair) new-value)
	(push (cons doc-type new-value)
	      (ext:info random-documentation stuff x))))
  new-value)

;;; Replace the minimal documentation function with the PCL version
;;; when loaded.
(eval-when (load)
  (setf (symbol-function 'lisp:documentation) #'documentation)
  (setf (documentation 'documentation 'function)
    "Returns the documentation string of Doc-Type for X, or NIL if
  none exists.  System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE,
  SETF, and T.")
  (setf (fdefinition '(setf lisp:documentation)) #'(setf documentation)))
