;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (Scott.Fahlman@cs.cmu.edu). 
;;; **********************************************************************
;;;
;;; This is the describe mechanism for Common Lisp.
;;;
;;; Written by Skef Wholey or Rob MacLachlan originally.
;;; Cleaned up, reorganized, and enhanced by Blaine Burks.
;;;
;;; This should be done better using CLOS more effectively once CMU Common
;;; Lisp is brought up to the new standard.  The TYPECASE in DESCRIBE-AUX
;;; should be unnecessary.	-- Bill Chiles
;;;

(in-package "LISP")

(export '(describe *describe-level* *describe-verbose* *describe-print-level*
		   *describe-print-length* *describe-indentation*))


;;;; DESCRIBE public switches.

(defvar *describe-level* 2
  "Depth of recursive descriptions allowed.")

(defvar *describe-verbose* nil
  "If non-nil, descriptions may provide interpretations of information and
  pointers to additional information.  Normally nil.")

(defvar *describe-print-level* 2
  "*print-level* gets bound to this inside describe.  If null, use
  *print-level*")

(defvar *describe-print-length* 5
  "*print-length* gets bound to this inside describe.  If null, use
  *print-length*.")

(defvar *describe-indentation* 3
  "Number of spaces that sets off each line of a recursive description.")

(defvar *in-describe* nil
  "Used to tell whether we are doing a recursive describe.")
(defvar *current-describe-level* 0
  "Used to implement recursive description cutoff.  Don't touch.")
(defvar *describe-output* nil
  "An output stream used by Describe for indenting and stuff.")
(defvar *used-documentation* nil "Documentation already described.")
(defvar *described-objects* nil
  "List of all objects describe within the current top-level call to describe.")
(defvar *current-describe-object* nil
  "The last object passed to describe.")

;;; DESCRIBE sets up the output stream and calls DESCRIBE-AUX, which does the
;;; hard stuff.
;;;
(defun describe (x &optional (stream *standard-output*))
  "Prints a description of the object X.
  See also *describe-level*, defdescribe, *describe-verbose*,
  *describe-print-level*, *describe-print-length*, and *describe-indentation*."
  (unless *describe-output*
    (setq *describe-output* (make-indenting-stream *standard-output*)))
  (cond (*in-describe*
	 (unless (or (eq x nil) (eq x t))
	   (let ((*current-describe-level* (1+ *current-describe-level*))
		 (*current-describe-object* x))
	     (indenting-further *describe-output* *describe-indentation*
	       (describe-aux x)))))
	(t
	 (setf (indenting-stream-stream *describe-output*) stream)
	 (let ((*standard-output* *describe-output*)
	       (*print-level* (or *describe-print-level* *print-level*))
	       (*print-length* (or *describe-print-length* *print-length*))
	       (*used-documentation* ())
	       (*described-objects* ())
	       (*in-describe* t)
	       (*current-describe-object* x))
	   (describe-aux x))
	 (values))))

;;; DESCRIBE-AUX does different things for each type.  The order of the
;;; TYPECASE branches matters with respect to:
;;;    - symbols and functions until the new standard makes them disjoint.
;;;    - packages and structure since packages are structures.
;;; We punt a given call if the current level is greater than *describe-level*,
;;; or if we detect an object into which we have already descended.
;;;
(defun describe-aux (x)
  (when (or (not (integerp *describe-level*))
	    (minusp *describe-level*))
    (error "*describe-level* should be a nonnegative integer - ~A."
	   *describe-level*))
  (when (or (>= *current-describe-level* *describe-level*)
	    (member x *described-objects*))
    (return-from describe-aux x))
  (push x *described-objects*)
  (typecase x
    (symbol (describe-symbol x))
    (function (describe-function x))
    (package (describe-package x))
    (hash-table (describe-hash-table x))
    (structure (describe-structure x))
    (array (describe-array x))
    (fixnum (describe-fixnum x))
    (cons
     (if (and (eq (car x) 'setf) (consp (cdr x)) (null (cddr x))
	      (symbolp (cadr x))
	      (fboundp x))
	 (describe-function (fdefinition x))
	 (default-describe x)))
    (t (default-describe x)))
  x)



;;;; Implementation properties.

;;; This supresses random garbage that users probably don't want to see.
;;;
(defparameter *implementation-properties*
  '(%loaded-address))


;;;; DESCRIBE methods.

;;; DESC-DOC prints the specified kind of documentation about the given Symbol.
;;;
(defun desc-doc (symbol name string)
  (let ((doc (documentation symbol name)))
    (when (and doc (not (member doc *used-documentation*)))
      (push doc *used-documentation*)
      (format t "~&~A~&  ~A" string doc))))

	  
(defun default-describe (x)
  (format t "~&~S is a ~S." x (type-of x)))

(defun describe-symbol (x)
  (let ((package (symbol-package x)))
    (if package
	(multiple-value-bind (symbol status)
			     (find-symbol (symbol-name x) package)
	  (declare (ignore symbol))
	  (format t "~&~A is an ~A symbol in the ~A package." x
		  (string-downcase (symbol-name status))
		  (package-name (symbol-package x))))
	(format t "~&~A is an uninterned symbol." x)))
  ;;
  ;; Describe the value cell.
  (let* ((kind (info variable kind x))
	 (wot (ecase kind
		(:special "special variable")
		(:constant "constant")
		(:global "undefined variable"))))
    (cond
     ((boundp x)
      (let ((value (symbol-value x)))
	(format t "~&It is a ~A; its value is ~S." wot value)
	(describe value)))
     ((not (eq kind :global))
      (format t "~&It is a ~A; no current value." wot)))

    (when (eq (info variable where-from x) :declared)
      (format t "~&Its declared type is ~S."
	      (type-specifier (info variable type x))))

    (desc-doc x 'variable
	      (format nil "~:[Variable~;Constant~] documentation:"
		      (eq kind :constant))))
  ;;
  ;; Describe the function cell.
  (cond ((macro-function x)
	 (let ((fun (macro-function x)))
	   (format t "~&Its macroexpansion function is ~A." fun)
	   (describe-function fun t)
	   (desc-doc x 'function "Macro documentation:")))
	((special-form-p x)
	 (desc-doc x 'function "Special form documentation:"))
	((fboundp x)
	 (describe-function (fdefinition x))))
  ;;
  ;; Print other documentation.
  (desc-doc x 'structure "Documentation on the structure:")
  (desc-doc x 'type "Documentation on the type:")
  (desc-doc x 'setf "Documentation on the SETF form:")
  (dolist (assoc (info random-documentation stuff x))
    (unless (member (cdr assoc) *used-documentation*)
      (format t "~&Documentation on the ~(~A~):~%~A" (car assoc) (cdr assoc))))
  ;;
  ;; Print out properties, possibly ignoring implementation details.
  (do ((plist (symbol-plist X) (cddr plist)))
      ((null plist) ())
    (unless (member (car plist) *implementation-properties*)
      (format t "~&Its ~S property is ~S." (car plist) (cadr plist))
      (describe (cadr plist)))))

(defun describe-structure (x)
  (cond ((and (fboundp 'pcl::std-instance-p)
	      (pcl::std-instance-p x))
	 (pcl::describe-object x *standard-output*))
	(t
	 (format t "~&~S is a structure of type ~A." x (c::structure-ref x 0))
	 (dolist (slot (cddr (inspect::describe-parts x)))
	   (format t "~%~A: ~S." (car slot) (cdr slot))))))

(defun describe-array (x)
  (let ((rank (array-rank x)))
    (cond ((> rank 1)
	   (format t "~&~S is " x)
	   (write-string (if (%array-displaced-p x) "a displaced" "an"))
	   (format t " array of rank ~A." rank)
	   (format t "~%Its dimensions are ~S." (array-dimensions x)))
	  (t
	   (format t "~&~S is a ~:[~;displaced ~]vector of length ~D." x
		   (and (array-header-p x) (%array-displaced-p x)) (length x))
	   (if (array-has-fill-pointer-p x)
	       (format t "~&It has a fill pointer, currently ~d"
		       (fill-pointer x))
	       (format t "~&It has no fill pointer."))))
  (format t "~&Its element type is ~S." (array-element-type x))))

(defmacro describe-function-arg-list (object test output)
  `(progn
     (print-for-describe ,object)
     (if ,test
	 (write-string " is called with zero arguments.")
	 (indenting-further *standard-output* 2
	   (format t " can be called with these arguments:~%")
	   ,output))))

(defun describe-function (x &optional macro-p)
  (declare (type function x))
  (case (get-type x)
    (#.vm:closure-header-type
     (describe-function-lex-closure x macro-p))
    ((#.vm:function-header-type #.vm:closure-function-header-type)
     (describe-function-compiled x macro-p))
    (#.vm:funcallable-instance-header-type
     (pcl::describe-object x *standard-output*))
    (t
     (format t "~&It is an unknown type of function."))))

(defun describe-function-compiled (x macro-p)
  (let ((args (%function-header-arglist x)))
    (describe-function-arg-list
     *current-describe-object* (string= args "()") (write-string args)))

  (unless macro-p
    (let ((name (%function-header-name x)))
      (let ((*print-level* nil)
	    (*print-length* nil))
	(multiple-value-bind
	    (type where)
	    (if (or (symbolp name) (and (listp name) (eq (car name) 'setf)))
		(values (type-specifier (info function type name))
			(info function where-from name))
		(values (%function-header-type x)
			:defined))
	  (when (consp type)
	    (format t "~&Its ~(~A~) argument types are:~%  ~S"
		    where (second type))
	    (format t "~&Its result type is:~%  ~S" (third type)))))

      (let ((inlinep (info function inlinep name)))
	(when inlinep
	  (format t "~&It is currently declared ~(~A~);~
	             ~:[no~;~] expansion is available."
		  inlinep (info function inline-expansion name))))
    
      (when (symbolp name)
	(desc-doc name 'function "Function Documention:"))))
  
  (let ((info (di::code-debug-info (di::function-code-header x))))
    (when info
      (let ((sources (c::compiled-debug-info-source info)))
	(format t "~&On ~A it was compiled from:"
		(format-universal-time nil
				       (c::debug-source-compiled
					(first sources))))
	(dolist (source sources)
	  (let ((name (c::debug-source-name source)))
	    (ecase (c::debug-source-from source)
	      (:file
	       (format t "~&~A~%  Created: " (namestring name))
	       (ext:format-universal-time t (c::debug-source-created source))
	       (let ((comment (c::debug-source-comment source)))
		 (when comment
		   (format t "~&  Comment: ~A" comment))))
	      (:stream (format t "~&~S" name))
	      (:lisp (format t "~&~S" name)))))))))

(defun describe-function-lex-closure (x macro-p)
  (print-for-describe x)
  (format t " is a lexical closure.~%")
  (describe-function-compiled (%closure-function x) macro-p)
  (format t "~&Its lexical environment is:")
  (indenting-further *standard-output* 8)
  (dotimes (i (- (get-closure-length x) (1- vm:closure-info-offset)))
    (format t "~&~D: ~S" i (%closure-index-ref x i))))


(defun print-for-describe (x &optional (freshp t))
  (when freshp (fresh-line))
  (cond ((symbolp x)
	 (write-string (symbol-name x)))
	(t
	 (princ x))))

(defun describe-fixnum (x)
  (cond ((not (or *describe-verbose* (zerop *current-describe-level*))))
	((primep x)
	 (format t "~&It is a prime number."))
	(t
	 (format t "~&It is a composite number."))))

(defun describe-hash-table (x)
  (format t "~&~S is an ~a hash table." x (hash-table-kind x))
  (format t "~&Its size is ~d buckets." (hash-table-size x))
  (format t "~&Its rehash-size is ~d." (hash-table-rehash-size x))
  (format t "~&Its rehash-threshold is ~d."
	  (hash-table-rehash-threshold x))
  (format t "~&It currently holds ~d entries."
	  (hash-table-number-entries x)))

(defun describe-package (x)
  (describe-structure x)
  (let* ((internal (package-internal-symbols x))
	 (internal-count (- (package-hashtable-size internal)
			    (package-hashtable-free internal)))
	 (external (package-external-symbols x))
	 (external-count (- (package-hashtable-size external)
			    (package-hashtable-free external))))
    (format t "~&~d symbols total: ~d internal and ~d external."
	     (+ internal-count external-count) internal-count external-count)))

