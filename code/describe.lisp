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

(export '(describe *describe-level* *describe-verbose*
		   *describe-implementation-details* *describe-print-level*
		   *describe-print-length* *describe-indentation*))


;;;; DESCRIBE public switches.

(defvar *describe-level* 2
  "Depth of recursive descriptions allowed.")

(defvar *describe-verbose* nil
  "If non-nil, descriptions may provide interpretations of information and
  pointers to additional information.  Normally nil.")

(defvar *describe-implementation-details* nil
  "If non-null, normally concealed implementation information won't be.")

(defvar *describe-print-level* 2
  "*print-level* gets bound to this inside describe.")

(defvar *describe-print-length* 5
  "*print-length gets bound to this inside describe.")

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
  *describe-implementation-details*, *describe-print-level*,
  *describe-print-length*, and *describe-indentation*."
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
	       (*print-level* *describe-print-level*)
	       (*print-length* *describe-print-length*)
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
    (t (default-describe x)))
  x)



;;;; Implementation properties.

;;; This supresses random garbage that users probably don't want to see.
;;;
(defparameter *implementation-properties*
  '(%loaded-address
    ;;
    ;; Documentation properties:
    %var-documentation %fun-documentation %struct-documentation
    %type-documentation %setf-documentation %documentation))


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
  (when (boundp x)
    (let ((value (symbol-value x))
	  (constantp (constantp x)))
      (cond ((get x 'globally-special)
	     (if constantp
		 (format t "~&It is a constant; its value is ~S." value)
		 (format t "~&It is a special variable; ~
			    its current binding is ~S."
			 value)))
	    (t
	     (fresh-line)
	     (write-string "Its value is ")
	     (print-for-describe value nil)))
      (desc-doc x 'variable
		(format nil "~:[Variable~;Constant~] Documentation:"
			constantp))
      (describe value)))
  ;;
  ;; Describe the function cell.
  (cond ((macro-function x)
	 (let ((fun (macro-function x)))
	   (format t "~&Its macroexpansion function is ~A." fun)
	   (describe-function fun)
	   (desc-doc x 'function "Macro Documentation:")))
	((fboundp x)
	 (describe-function (symbol-function x))))
  ;;
  ;; Print other documentation.
  (desc-doc x 'structure "Documentation on the structure:")
  (desc-doc x 'type "Documentation on the type:")
  (desc-doc x 'setf "Documentation on the SETF form:")
  (dolist (assoc (get x '%documentation))
    (unless (member (cdr assoc) *used-documentation*)
      (format t "~&Documentation on the ~(~A~):~%~A" (car assoc) (cdr assoc))))
  ;;
  ;; Print out properties, possibly ignoring implementation details.
  (do ((plist (symbol-plist X) (cddr plist))
       (properties-to-ignore (if *describe-implementation-details*
				 nil
				 *implementation-properties*)))
      ((null plist) ())
    (unless (member (car plist) properties-to-ignore)
      (format t "~&Its ~S property is ~S." (car plist) (cadr plist))
      (describe (cadr plist)))))

(defun describe-structure (x)
  (format t "~&~S is a structure of type ~A." x (svref x 0))
  (dolist (slot (cddr (inspect::describe-parts x)))
    (format t "~%~A: ~S." (car slot) (cdr slot))))

(defun describe-array (x)
  (let ((rank (array-rank x)))
    (cond ((> rank 1)
	   (format t "~&~S is " x)
	   (write-string (if (%displacedp x) "a displaced" "an"))
	   (format t " array of rank ~A." rank)
	   (format t "~%Its dimensions are ~S." (array-dimensions x)))
	  (t
	   (format t "~&~S is a ~:[~;displaced ~]vector of length ~D." x
		   (%displacedp x) (length x))
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

(defun describe-function (x)
  (case (%primitive get-vector-subtype x)
    (#.%function-entry-subtype
     (describe-function-compiled x))
    (#.%function-closure-subtype
     (describe-function-lex-closure x))
    (t
     (format t "~&It is an unknown type of function."))))

(defun describe-function-compiled (x)
  (let ((args (%primitive header-ref x %function-entry-arglist-slot)))
    (describe-function-arg-list
     *current-describe-object* (string= args "()") (write-string args)))
  (let ((*print-level* nil)
	(*print-length* nil)
	(type (%primitive header-ref x %function-entry-type-slot)))
    (format t "~&Its argument types are:~%  ~S" (second type))
    (format t "~&Its result type is:~%  ~S" (third type)))
  
  (let ((name (%primitive header-ref x %function-name-slot)))
    (when (symbolp name)
      (desc-doc 'function name "Function Documention:")))
  
  (let ((info (%primitive header-ref
			  (%primitive header-ref x
				      %function-entry-constants-slot)
			  %function-constants-debug-info-slot)))
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
	       (format t "~&~A " (namestring name))
	       (ext:format-universal-time t (c::debug-source-created source)))
	      (:stream (format t "~&~S" name))
	      (:lisp (format t "~&~S" name)))))))))

(defun describe-function-lex-closure (x)
  (print-for-describe x)
  (format t " is a lexical closure.~%")
  (format t "~&Its lexical environment is:")
  (indenting-further *standard-output* 8
    (do ((i %function-closure-variables-offset (1+ i)))
	((= i (%primitive header-length x)))
      (format t "~&~D: ~S"
	      (- i %function-closure-variables-offset)
	      (%primitive header-ref x i))))
  (describe-function-compiled (%primitive header-ref x %function-name-slot)))


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

