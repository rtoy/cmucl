;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/package.lisp,v 1.34 1993/11/30 17:46:32 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;;     Package stuff and stuff like that.
;;;
;;; Re-Written by Rob MacLachlan.  Earlier version written by
;;; Lee Schumacher.  Apropos & iteration macros courtesy of Skef Wholey.
;;; Defpackage by Dan Zigmond.  With-Package-Iterator by Blaine Burks. 
;;; Defpackage and do-mumble-symbols macros re-written by William Lott.
;;;
(in-package "LISP")
(export '(package packagep *package* make-package in-package find-package
	  package-name package-nicknames rename-package delete-package
	  package-use-list package-used-by-list package-shadowing-symbols
	  list-all-packages intern find-symbol unintern export
	  unexport import shadowing-import shadow use-package
	  unuse-package find-all-symbols do-symbols with-package-iterator
	  do-external-symbols do-all-symbols apropos apropos-list defpackage))

(in-package "EXTENSIONS")
(export '(*keyword-package* *lisp-package* *default-package-use-list*
			    map-apropos))

(in-package "KERNEL")
(export '(%in-package old-in-package))

(in-package "LISP")

(defvar *default-package-use-list* '("COMMON-LISP")
  "The list of packages to use by default of no :USE argument is supplied
   to MAKE-PACKAGE or other package creation forms.")


(defstruct (package
	    (:constructor internal-make-package)
	    (:predicate packagep)
	    (:print-function %print-package)
	    (:make-load-form-fun
	     (lambda (package)
	       (values `(package-or-lose ',(package-name package))
		       nil))))
  "Standard structure for the description of a package.  Consists of 
   a list of all hash tables, the name of the package, the nicknames of
   the package, the use-list for the package, the used-by- list, hash-
   tables for the internal and external symbols, and a list of the
   shadowing symbols."
  (tables (list nil))	; A list of all the hashtables for inherited symbols.
  ;;
  ;; The string name of the package.
  (%name nil :type (or simple-string null))
  ;;
  ;; List of nickname strings.
  (%nicknames () :type list)
  ;;
  ;; List of packages we use.
  (%use-list () :type list)
  ;;
  ;; List of packages that use this package.
  (%used-by-list () :type list)
  ;;
  ;; Hashtables of internal & external symbols.
  (internal-symbols (required-argument) :type package-hashtable)
  (external-symbols (required-argument) :type package-hashtable)
  ;;
  ;; List of shadowing symbols.
  (%shadowing-symbols () :type list))


(defun %print-package (s stream d)
  (declare (ignore d) (stream stream))
  (if (package-%name s)
      (multiple-value-bind (iu it) (internal-symbol-count s)
	(multiple-value-bind (eu et) (external-symbol-count s)
	  (print-unreadable-object (s stream)
	    (format stream "The ~A package, ~D/~D internal, ~D/~D external"
		    (package-%name s) iu it eu et))))
      (print-unreadable-object (s stream :identity t)
	(format stream "deleted package"))))

;;; Can get the name (NIL) of a deleted package.
;;;
(defun package-name (x)
  (package-%name (if (packagep x) x (package-or-lose x))))

(macrolet ((frob (ext real)
	     `(defun ,ext (x) (,real (package-or-lose x)))))
  (frob package-nicknames package-%nicknames)
  (frob package-use-list package-%use-list)
  (frob package-used-by-list package-%used-by-list)
  (frob package-shadowing-symbols package-%shadowing-symbols))

(defvar *package* () "The current package.")

;;; An equal hashtable from package names to packages.
;;;
(defvar *package-names* (make-hash-table :test #'equal))


;;; Lots of people want the keyword package and Lisp package without a lot
;;; of fuss, so we give them their own variables.
;;;
(defvar *lisp-package*)
(defvar *keyword-package*)


;;; This magical variable is T during initialization so Use-Package's of packages
;;; that don't yet exist quietly win.  Such packages are thrown onto the list
;;; *Deferred-Use-Packages* so that this can be fixed up later.

(defvar *in-package-init* nil)
(defvar *deferred-use-packages* nil)

;;; Find-Package  --  Public
;;;
;;;
(defun find-package (name)
  "Find the package having the specified name."
  (values (gethash (string name) *package-names*)))

;;; Package-Listify  --  Internal
;;;
;;;    Return a list of packages given a package-or-string-or-symbol or
;;; list thereof, or die trying.
;;;
(defun package-listify (thing)
  (let ((res ()))
    (dolist (thing (if (listp thing) thing (list thing)) res)
      (push (package-or-lose thing) res))))

;;; PACKAGE-NAMIFY  --  Internal
;;;
;;;    Make a package name into a simple-string.
;;;
(defun package-namify (n)
  (if (symbolp n)
      (symbol-name n)
      (coerce n 'simple-string)))

;;; Package-Or-Lose  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package.
;;;
(defun package-or-lose (thing)
  (cond ((packagep thing)
	 (unless (package-%name thing)
	   (error "Can't do anything to a deleted package: ~S" thing))
	 thing)
	(t
	 (let ((thing (package-namify thing)))
	   (cond ((gethash thing *package-names*))
		 (t
		  (cerror "Make this package."
			  "~S is not the name of a package." thing)
		  (make-package thing)))))))


;;;; Package-Hashtables
;;;
;;;    Packages are implemented using a special kind of hashtable.  It is
;;; an open hashtable with a parallel 8-bit I-vector of hash-codes.  The
;;; primary purpose of the hash for each entry is to reduce paging by
;;; allowing collisions and misses to be detected without paging in the
;;; symbol and pname for an entry.  If the hash for an entry doesn't
;;; match that for the symbol that we are looking for, then we can
;;; go on without touching the symbol, pname, or even hastable vector.
;;;    It turns out that, contrary to my expectations, paging is a very
;;; important consideration the design of the package representation.
;;; Using a similar scheme without the entry hash, the fasloader was
;;; spending more than half its time paging in INTERN.
;;;    The hash code also indicates the status of an entry.  If it zero,
;;; the the entry is unused.  If it is one, then it is deleted.
;;; Double-hashing is used for collision resolution.

(deftype hash-vector () '(simple-array (unsigned-byte 8) (*)))

(defstruct (package-hashtable
	    (:constructor internal-make-package-hashtable ())
	    (:copier nil)
	    (:print-function
	     (lambda (table stream d)
	       (declare (ignore d) (stream stream))
	       (format stream
		       "#<Package-Hashtable: Size = ~D, Free = ~D, Deleted = ~D>"
		       (package-hashtable-size table)
		       (package-hashtable-free table)
		       (package-hashtable-deleted table)))))
  ;;
  ;; The g-vector of symbols.
  (table nil :type (or simple-vector null))
  ;;
  ;; The i-vector of pname hash values.
  (hash nil :type (or hash-vector null))
  ;;
  ;; The maximum number of entries allowed.
  (size 0 :type index)
  ;;
  ;; The entries that can be made before we have to rehash.
  (free 0 :type index)
  ;;
  ;; The number of deleted entries.
  (deleted 0 :type index))


;;; The maximum density we allow in a package hashtable.
;;;
(defparameter package-rehash-threshold 3/4)

;;; Entry-Hash  --  Internal
;;;
;;;    Compute a number from the sxhash of the pname and the length which
;;; must be between 2 and 255.
;;;
(defmacro entry-hash (length sxhash)
  `(the fixnum
	(+ (the fixnum
		(rem (the fixnum
			  (logxor ,length
				  ,sxhash
				  (the fixnum (ash ,sxhash -8))
				  (the fixnum (ash ,sxhash -16))
				  (the fixnum (ash ,sxhash -19))))
		     254))
	   2)))

;;; Make-Package-Hashtable  --  Internal
;;;
;;;    Make a package hashtable having a prime number of entries at least
;;; as great as (/ size package-rehash-threshold).  If Res is supplied,
;;; then it is destructively modified to produce the result.  This is
;;; useful when changing the size, since there are many pointers to
;;; the hashtable.
;;;
(defun make-package-hashtable (size &optional
				    (res (internal-make-package-hashtable)))
  (do ((n (logior (truncate size package-rehash-threshold) 1)
	  (+ n 2)))
      ((primep n)
       (setf (package-hashtable-table res)
	     (make-array n))
       (setf (package-hashtable-hash res)
	     (make-array n :element-type '(unsigned-byte 8)
			 :initial-element 0))
       (let ((size (truncate (* n package-rehash-threshold))))
	 (setf (package-hashtable-size res) size)
	 (setf (package-hashtable-free res) size))
       (setf (package-hashtable-deleted res) 0)
       res)
    (declare (fixnum n))))


;;; Internal-Symbol-Count, External-Symbols-Count  --  Internal
;;;
;;;    Return internal and external symbols.  Used by Genesis and stuff.
;;;
(flet ((stuff (table)
	 (let ((size (the fixnum
			  (- (the fixnum (package-hashtable-size table))
			     (the fixnum
				  (package-hashtable-deleted table))))))
	   (declare (fixnum size))
	   (values (the fixnum
			(- size
			   (the fixnum
				(package-hashtable-free table))))
		   size))))

  (defun internal-symbol-count (package)
    (stuff (package-internal-symbols package)))

  (defun external-symbol-count (package)
    (stuff (package-external-symbols package))))


;;; Add-Symbol  --  Internal
;;;
;;;    Add a symbol to a package hashtable.  The symbol is assumed
;;; not to be present.
;;;
(defun add-symbol (table symbol)
  (let* ((vec (package-hashtable-table table))
	 (hash (package-hashtable-hash table))
	 (len (length vec))
	 (sxhash (%sxhash-simple-string (symbol-name symbol)))
	 (h2 (the fixnum (1+ (the fixnum (rem sxhash
					      (the fixnum (- len 2))))))))
    (declare (simple-vector vec)
	     (type (simple-array (unsigned-byte 8)) hash)
	     (fixnum len sxhash h2))
    (cond ((zerop (the fixnum (package-hashtable-free table)))
	   (make-package-hashtable (the fixnum
					(* (the fixnum
						(package-hashtable-size table))
					   2))
				   table)
	   (add-symbol table symbol)
	   (dotimes (i len)
	     (declare (fixnum i))
	     (when (> (the fixnum (aref hash i)) 1)
	       (add-symbol table (svref vec i)))))
	  (t
	   (do ((i (rem sxhash len) (rem (+ i h2) len)))
	       ((< (the fixnum (aref hash i)) 2)
		(if (zerop (the fixnum (aref hash i)))
		    (decf (the fixnum (package-hashtable-free table)))
		    (decf (the fixnum (package-hashtable-deleted table))))
		(setf (svref vec i) symbol)
		(setf (aref hash i)
		      (entry-hash (length (the simple-string
					       (symbol-name symbol)))
				  sxhash)))
	     (declare (fixnum i)))))))

;;; With-Symbol  --  Internal
;;;
;;;    Find where the symbol named String is stored in Table.  Index-Var
;;; is bound to the index, or NIL if it is not present.  Symbol-Var
;;; is bound to the symbol.  Length and Hash are the length and sxhash
;;; of String.  Entry-Hash is the entry-hash of the string and length.
;;;
(defmacro with-symbol ((index-var symbol-var table string length sxhash
				  entry-hash)
		       &body forms)
  (let ((vec (gensym)) (hash (gensym)) (len (gensym)) (h2 (gensym))
	(name (gensym)) (name-len (gensym)) (ehash (gensym)))
    `(let* ((,vec (package-hashtable-table ,table))
	    (,hash (package-hashtable-hash ,table))
	    (,len (length ,vec))
	    (,h2 (1+ (the index (rem (the index ,sxhash)
				      (the index (- ,len 2)))))))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,hash)
		(simple-vector ,vec)
		(type index ,len ,h2))
       (prog ((,index-var (rem (the index ,sxhash) ,len))
	      ,symbol-var ,ehash)
	 (declare (type (or index null) ,index-var))
	 LOOP
	 (setq ,ehash (aref ,hash ,index-var))
	 (cond ((eql ,ehash ,entry-hash)
		(setq ,symbol-var (svref ,vec ,index-var))
		(let* ((,name (symbol-name ,symbol-var))
		       (,name-len (length ,name)))
		  (declare (simple-string ,name)
			   (type index ,name-len))
		  (when (and (= ,name-len ,length)
			     (string= ,string ,name  :end1 ,length
				      :end2 ,name-len))
		    (go DOIT))))
	       ((zerop ,ehash)
		(setq ,index-var nil)
		(go DOIT)))
	 (setq ,index-var (+ ,index-var ,h2))
	 (when (>= ,index-var ,len)
	   (setq ,index-var (- ,index-var ,len)))
	 (go LOOP)
	 DOIT
	 (return (progn ,@forms))))))

;;; Nuke-Symbol  --  Internal
;;;
;;;    Delete the entry for String in Table.  The entry must exist.
;;;
(defun nuke-symbol (table string)
  (declare (simple-string string))
  (let* ((length (length string))
	 (hash (%sxhash-simple-string string))
	 (ehash (entry-hash length hash)))
    (declare (type index length hash))
    (with-symbol (index symbol table string length hash ehash)
      (setf (aref (package-hashtable-hash table) index) 1)
      (setf (aref (package-hashtable-table table) index) nil)
      (incf (package-hashtable-deleted table)))))

;;;; Iteration macros.

(defmacro do-symbols ((var &optional (package '*package*) result-form)
		      &body body)
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var) ,@body))
	 (let* ((package (package-or-lose ,package))
		(shadows (package-%shadowing-symbols package)))
	   (flet ((iterate-over-hash-table (table ignore)
		    (let ((hash-vec (package-hashtable-hash table))
			  (sym-vec (package-hashtable-table table)))
		      (declare (type (simple-array (unsigned-byte 8) (*))
				     hash-vec)
			       (type simple-vector sym-vec))
		      (dotimes (i (length sym-vec))
			(when (>= (aref hash-vec i) 2)
			  (let ((sym (aref sym-vec i)))
			    (declare (inline member))
			    (unless (member sym ignore :test #'eq)
			      (,flet-name sym))))))))
	     (iterate-over-hash-table (package-internal-symbols package) nil)
	     (iterate-over-hash-table (package-external-symbols package) nil)
	     (dolist (use (package-%use-list package))
	       (iterate-over-hash-table (package-external-symbols use)
					shadows)))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,result-form))))

(defmacro do-external-symbols ((var &optional (package '*package*) result-form)
			       &body body)
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var)
		,@body))
	 (let* ((package (package-or-lose ,package))
		(table (package-external-symbols package))
		(hash-vec (package-hashtable-hash table))
		(sym-vec (package-hashtable-table table)))
	   (declare (type (simple-array (unsigned-byte 8) (*))
			  hash-vec)
		    (type simple-vector sym-vec))
	   (dotimes (i (length sym-vec))
	     (when (>= (aref hash-vec i) 2)
	       (,flet-name (aref sym-vec i))))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,result-form))))

(defmacro do-all-symbols ((var &optional result-form) &body body)
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (let ((flet-name (gensym "DO-SYMBOLS-")))
    `(block nil
       (flet ((,flet-name (,var)
		,@body))
	 (dolist (package (list-all-packages))
	   (flet ((iterate-over-hash-table (table)
		    (let ((hash-vec (package-hashtable-hash table))
			  (sym-vec (package-hashtable-table table)))
		      (declare (type (simple-array (unsigned-byte 8) (*))
				     hash-vec)
			       (type simple-vector sym-vec))
		      (dotimes (i (length sym-vec))
			(when (>= (aref hash-vec i) 2)
			  (,flet-name (aref sym-vec i)))))))
	     (iterate-over-hash-table (package-internal-symbols package))
	     (iterate-over-hash-table (package-external-symbols package)))))
       (let ((,var nil))
	 (declare (ignorable ,var))
	 ,result-form))))


;;;; WITH-PACKAGE-ITERATOR

(defmacro with-package-iterator ((mname package-list &rest symbol-types)
				 &body body)
  (let* ((packages (gensym))
	 (these-packages (gensym))
	 (ordered-types (let ((res nil))
			  (dolist (kind '(:inherited :external :internal)
					res)
			    (when (member kind symbol-types)
			      (push kind res)))))  ; Order symbol-types.
	 (counter (gensym))
	 (kind (gensym))
	 (hash-vector (gensym))
	 (vector (gensym))
	 (package-use-list (gensym))
	 (init-macro (gensym))
	 (end-test-macro (gensym))
	 (real-symbol-p (gensym))
	 (BLOCK (gensym)))
    `(let* ((,these-packages ,package-list)
	    (,packages `,(mapcar #'(lambda (package)
				     (if (packagep package)
					 package
					 (find-package package)))
				 (if (consp ,these-packages)
				     ,these-packages
				     (list ,these-packages))))
	    (,counter nil)
	    (,kind (car ,packages))
	    (,hash-vector nil)
	    (,vector nil)
	    (,package-use-list nil))
       ,(if (member :inherited ordered-types)
	    `(setf ,package-use-list (package-%use-list (car ,packages)))
	    `(declare (ignore ,package-use-list)))
       (macrolet ((,init-macro (next-kind)
 	 (let ((symbols (gensym)))
	   `(progn
	      (setf ,',kind ,next-kind)
	      (setf ,',counter nil)
	      ,(case next-kind
		 (:internal
		  `(let ((,symbols (package-internal-symbols
				    (car ,',packages))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))
		 (:external
		  `(let ((,symbols (package-external-symbols
				    (car ,',packages))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))
		 (:inherited
		  `(let ((,symbols (package-external-symbols
				    (car ,',package-use-list))))
		     (setf ,',vector (package-hashtable-table ,symbols))
		     (setf ,',hash-vector (package-hashtable-hash ,symbols))))))))
		  (,end-test-macro (this-kind)
		     `,(let ((next-kind (cadr (member this-kind
						      ',ordered-types))))
			 (if next-kind
			     `(,',init-macro ,next-kind)
			     `(if (endp (setf ,',packages (cdr ,',packages)))
				  (return-from ,',BLOCK)
				  (,',init-macro ,(car ',ordered-types)))))))
	 (when ,packages
	   ,(when (null symbol-types)
	      (error "Must supply at least one of :internal, :external, or ~
	      :inherited."))
	   ,(dolist (symbol symbol-types)
	      (unless (member symbol '(:internal :external :inherited))
		(error "~S is not one of :internal, :external, or :inherited."
		       symbol)))
	   (,init-macro ,(car ordered-types))
	   (flet ((,real-symbol-p (number)
		    (> number 1)))
	     (macrolet ((,mname ()
	      `(block ,',BLOCK
		 (loop
		   (case ,',kind
		     ,@(when (member :internal ',ordered-types)
			 `((:internal
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :internal)))))
		     ,@(when (member :external ',ordered-types)
			 `((:external
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (if ,',counter
				(return-from ,',BLOCK
				 (values t (svref ,',vector ,',counter)
					 ,',kind (car ,',packages)))
				(,',end-test-macro :external)))))
		     ,@(when (member :inherited ',ordered-types)
			 `((:inherited
			    (setf ,',counter
				  (position-if #',',real-symbol-p ,',hash-vector
					       :start (if ,',counter
							  (1+ ,',counter)
							  0)))
			    (cond (,',counter
				   (return-from
				    ,',BLOCK
				    (values t (svref ,',vector ,',counter)
					    ,',kind (car ,',packages))))
				  (t
				   (setf ,',package-use-list
					 (cdr ,',package-use-list))
				   (cond ((endp ,',package-use-list)
					  (setf ,',packages (cdr ,',packages))
					  (when (endp ,',packages)
					    (return-from ,',BLOCK))
					  (setf ,',package-use-list
						(package-%use-list
						 (car ,',packages)))
					  (,',init-macro ,(car ',ordered-types)))
					 (t (,',init-macro :inherited)
					    (setf ,',counter nil)))))))))))))
	       ,@body)))))))


;;;; DEFPACKAGE:

(defmacro defpackage (package &rest options)
  "Defines a new package called PACKAGE.  Each of OPTIONS should be one of the
   following:
     (:NICKNAMES {package-name}*)
     (:SIZE <integer>)
     (:SHADOW {symbol-name}*)
     (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
     (:USE {package-name}*)
     (:IMPORT-FROM <package-name> {symbol-name}*)
     (:INTERN {symbol-name}*)
     (:EXPORT {symbol-name}*)
   All options except :SIZE can be used multiple times."
  (let ((nicknames nil)
	(size nil)
	(shadows nil)
	(shadowing-imports nil)
	(use nil)
	(use-p nil)
	(imports nil)
	(interns nil)
	(exports nil)
	(incomming nil)
	(outgoing nil))
    (dolist (option options)
      (unless (consp option)
	(error "Bogus DEFPACKAGE option: ~S" option))
      (case (car option)
	(:nicknames
	 (let ((new (stringify-names (cdr option) "package")))
	   (setf nicknames (append-unique new nicknames :nicknames))))
	(:size
	 (cond (size
		(error "Can't specify :SIZE twice."))
	       ((and (consp (cdr option))
		     (typep (second option) 'unsigned-byte))
		(setf size (second option)))
	       (t
		(error "Bogus :SIZE, must be a positive integer: ~S"
		       (second option)))))
	(:shadow
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf incomming (append-unique new incomming :shadow))
	   (setf shadows (append shadows new))))
	(:shadowing-import-from
	 (let ((package-name (stringify-name (second option) "package"))
	       (names (stringify-names (cddr option) "symbol")))
	   (setf incomming (append-unique names incomming :shadowing-import))
	   (let ((assoc (assoc package-name shadowing-imports
			       :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
		 (setf shadowing-imports
		       (acons package-name names shadowing-imports))))))
	(:use
	 (let ((new (stringify-names (cdr option) "package")))
	   (setf use (append-unique new nicknames :use))
	   (setf use-p t)))
	(:import-from
	 (let ((package-name (stringify-name (second option) "package"))
	       (names (stringify-names (cddr option) "symbol")))
	   (setf incomming (append-unique names incomming :import-from))
	   (let ((assoc (assoc package-name imports
			       :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
		 (setf imports (acons package-name names imports))))))
	(:intern
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf incomming (append-unique new incomming :intern))
	   (setf outgoing (append-unique new incomming :intern))
	   (setf interns (append interns new))))
	(:export
	 (let ((new (stringify-names (cdr option) "symbol")))
	   (setf outgoing (append-unique new incomming :export))
	   (setf exports (append exports new))))
	(t
	 (error "Bogus DEFPACKAGE option: ~S" option))))
    `(eval-when (compile load eval)
       (%defpackage ,(stringify-name package "package") ',nicknames ',size
		    ',shadows ',shadowing-imports ',(if use-p use :default)
		    ',imports ',interns ',exports))))

(defun stringify-name (name kind)
  (typecase name
    (simple-string name)
    (string (coerce name 'simple-string))
    (symbol (symbol-name name))
    (t
     (error "Bogus ~A name: ~S" kind name))))

(defun stringify-names (names kind)
  (mapcar #'(lambda (name)
	      (stringify-name name kind))
	  names))

(defun append-unique (new old list)
  (dolist (name new)
    (if (member name old)
	(cerror "Ignore it."
		"Duplicate name in the ~S list: ~A" list name)
	(push name old)))
  old)

(defun %defpackage (name nicknames size shadows shadowing-imports
			 use imports interns exports)
  (declare (type simple-base-string name)
	   (type list nicknames shadows shadowing-imports
		 imports interns exports)
	   (type (or list (member :default)) use))
  (let ((package (or (find-package name)
		     (progn
		       (when (eq use :default)
			 (setf use *default-package-use-list*))
		       (make-package name
				     :use nil
				     :internal-symbols (or size 10)
				     :external-symbols (length exports))))))
    (unless (string= (the string (package-name package)) name)
      (error "~A is a nick-name for the package ~A"
	     name (package-name name)))
    (enter-new-nicknames package nicknames)
    ;; Shadows and Shadowing-imports.
    (let ((old-shadows (package-%shadowing-symbols package)))
      (shadow shadows package)
      (dolist (sym-name shadows)
	(setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
      (dolist (simports-from shadowing-imports)
	(let ((other-package (package-or-lose (car simports-from))))
	  (dolist (sym-name (cdr simports-from))
	    (let ((sym (find-or-make-symbol sym-name other-package)))
	      (shadowing-import sym package)
	      (setf old-shadows (remove sym old-shadows))))))
      (when old-shadows
	(warn "~A also shadows the following symbols:~%  ~S"
	      name old-shadows)))
    ;; Use
    (unless (eq use :default)
      (let ((old-use-list (package-use-list package))
	    (new-use-list (mapcar #'package-or-lose use)))
	(use-package (set-difference new-use-list old-use-list) package)
	(let ((laterize (set-difference old-use-list new-use-list)))
	  (when laterize
	    (unuse-package laterize package)
	    (warn "~A used to use the following packages:~%  ~S" laterize)))))
    ;; Import and Intern.
    (dolist (sym-name interns)
      (intern sym-name package))
    (dolist (imports-from imports)
      (let ((other-package (package-or-lose (car imports-from))))
	(dolist (sym-name (cdr imports-from))
	  (import (find-or-make-symbol sym-name other-package) package))))
    ;; Exports.
    (let ((old-exports nil)
	  (exports (mapcar #'(lambda (sym-name) (intern sym-name package))
			   exports)))
      (do-external-symbols (sym package)
	(push sym old-exports))
      (export exports package)
      (let ((diff (set-difference old-exports exports)))
	(when diff
	  (warn "~A also exports the following symbols:~%  ~S"
		name diff))))
    package))

(defun find-or-make-symbol (name package)
  (multiple-value-bind
      (symbol how)
      (find-symbol name package)
    (cond (how
	   symbol)
	  (t
	   (cerror "INTERN it."
		   "~A does not contain a symbol ~A"
		   (package-name package)
		   name)
	   (intern name package)))))


;;; Enter-New-Nicknames  --  Internal
;;;
;;;    Enter any new Nicknames for Package into *package-names*.
;;; If there is a conflict then give the user a chance to do
;;; something about it.
;;;
(defun enter-new-nicknames (package nicknames)
  (check-type nicknames list)
  (dolist (n nicknames)
    (let* ((n (package-namify n))
	   (found (gethash n *package-names*)))
      (cond ((not found)
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))
	    ((eq found package))
	    ((string= (the string (package-%name found)) n)
	     (cerror "Ignore this nickname."
		     "~S is a package name, so it cannot be a nickname for ~S."
		     n (package-%name package)))
	    (t
	     (cerror "Redefine this nickname."
		     "~S is already a nickname for ~S."
		     n (package-%name found))
	     (setf (gethash n *package-names*) package)
	     (push n (package-%nicknames package)))))))


;;; Make-Package  --  Public
;;;
;;;    Check for package name conflicts in name and nicknames, then
;;; make the package.  Do a use-package for each thing in the use list
;;; so that checking for conflicting exports among used packages is done.
;;;
(defun make-package (name &key (use *default-package-use-list*) nicknames
			  (internal-symbols 10) (external-symbols 10))
  "Makes a new package having the specified Name and Nicknames.  The
  package will inherit all external symbols from each package in
  the use list.  :Internal-Symbols and :External-Symbols are
  estimates for the number of internal and external symbols which
  will ultimately be present in the package."
  (when (find-package name)
    (error "A package named ~S already exists" name))
  (let* ((name (package-namify name))
	 (package (internal-make-package
		   :%name name
		   :internal-symbols (make-package-hashtable internal-symbols)
		   :external-symbols (make-package-hashtable external-symbols))))
    (if *in-package-init*
	(push (list use package) *deferred-use-packages*)
	(use-package use package))
    (enter-new-nicknames package nicknames)
    (setf (gethash name *package-names*) package)))

;;; Old-In-Package  --  Sorta Public.
;;;
;;;    Like Make-Package, only different.  Should go away someday.
;;;
(defun old-in-package (name &rest keys &key nicknames use)
  "Sets *package* to package with given name, creating the package if
   it does not exist.  If the package already exists then it is modified
   to agree with the :Use and :Nicknames arguments.  Any new nicknames
   are added without removing any old ones not specified.  If any package
   in the :Use list is not currently used, then it is added to the use
   list."
  (let ((package (find-package name)))
    (cond
     (package
      (if *in-package-init*
	  (push (list use package) *deferred-use-packages*)
	  (use-package use package))
      (enter-new-nicknames package nicknames)
      (setq *package* package))
     (t
      (setq *package* (apply #'make-package name keys))))))

;;; IN-PACKAGE -- public.
;;; 
(defmacro in-package (package &rest noise)
  (cond ((or noise
	     (not (or (stringp package) (symbolp package))))
	 (warn "Old-style IN-PACKAGE.")
	 `(old-in-package ,package ,@noise))
	(t
	 `(%in-package ',(stringify-name package "package")))))
;;;
(defun %in-package (name)
  (setf *package* (package-or-lose name)))

;;; Rename-Package  --  Public
;;;
;;;    Change the name if we can, blast any old nicknames and then
;;; add in any new ones.
;;;
(defun rename-package (package name &optional (nicknames ()))
  "Changes the name and nicknames for a package."
  (let* ((package (package-or-lose package))
	 (name (string name))
	 (found (find-package name)))
    (unless (or (not found) (eq found package))
      (error "A package named ~S already exists." name))
    (remhash (package-%name package) *package-names*)
    (dolist (n (package-%nicknames package))
      (remhash n *package-names*))
     (setf (package-%name package) name)
    (setf (gethash name *package-names*) package)
    (setf (package-%nicknames package) ())
    (enter-new-nicknames package nicknames)
    package))

;;; Delete-Package -- Public
;;;
(defun delete-package (package-or-name)
  "Delete the package-or-name from the package system data structures."
  (let ((package (if (packagep package-or-name)
		     package-or-name
		     (find-package package-or-name))))
    (cond ((not package)
	   (cerror "Return NIL" "No package of name ~S." package-or-name)
	   nil)
	  ((not (package-name package)) nil)
	  (t
	   (let ((use-list (package-used-by-list package)))
	     (when use-list
	       (cerror "Remove dependency in other packages."
		       "Package ~S is used by package(s):~%  ~S"
		       (package-name package) (mapcar #'package-name use-list))
	       (dolist (p use-list)
		 (unuse-package package p))))
	   (dolist (used (package-use-list package))
	     (unuse-package used package))
	   (do-symbols (sym package)
	     (unintern sym package))
	   (remhash (package-name package) *package-names*)
	   (dolist (nick (package-nicknames package))
	     (remhash nick *package-names*))
	   (setf (package-%name package) nil)
	   t))))

;;; List-All-Packages  --  Public
;;;
;;;
(defun list-all-packages ()
  "Returns a list of all existing packages."
  (let ((res ()))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (pushnew v res))
	     *package-names*)
    res))

;;; Intern  --  Public
;;;
;;;    Simple-stringify the name and call intern*.
;;;
(defun intern (name &optional package)
  "Returns a symbol having the specified name, creating it if necessary."
  (let ((name (if (simple-string-p name) name (coerce name 'simple-string))))
    (declare (simple-string name))
    (intern* name (length name)
	     (if package (package-or-lose package) *package*))))

;;; Find-Symbol  --  Public
;;;
;;;    Ditto.
;;;
(defun find-symbol (name &optional package)
  "Returns the symbol named String in Package.  If such a symbol is found
  then the second value is :internal, :external or :inherited to indicate
  how the symbol is accessible.  If no symbol is found then both values
  are NIL."
  (let ((name (if (simple-string-p name) name (coerce name 'simple-string))))
    (declare (simple-string name))
    (find-symbol* name (length name)
		  (if package (package-or-lose package) *package*))))

;;; Intern*  --  Internal
;;;
;;;    If the symbol doesn't exist then create it, special-casing
;;; the keyword package.
;;;
(defun intern* (name length package)
  (declare (simple-string name))
  (multiple-value-bind (symbol where) (find-symbol* name length package)
    (if where
	(values symbol where)
	(let ((symbol (make-symbol (subseq name 0 length))))
	  (%set-symbol-package symbol package)
	  (cond ((eq package *keyword-package*)
		 (add-symbol (package-external-symbols package) symbol)
		 (%set-symbol-value symbol symbol))
		(t
		 (add-symbol (package-internal-symbols package) symbol)))
	  (values symbol nil)))))

;;; Find-Symbol*  --  Internal
;;;
;;;    Check internal and external symbols, then scan down the list
;;; of hashtables for inherited symbols.  When an inherited symbol
;;; is found pull that table to the beginning of the list.
;;;
(defun find-symbol* (string length package)
  (declare (simple-string string)
	   (type index length))
  (let* ((hash (%sxhash-simple-substring string length))
	 (ehash (entry-hash length hash)))
    (declare (type index hash ehash))
    (with-symbol (found symbol (package-internal-symbols package)
			string length hash ehash)
      (when found
	(return-from find-symbol* (values symbol :internal))))
    (with-symbol (found symbol (package-external-symbols package)
			string length hash ehash)
      (when found
	(return-from find-symbol* (values symbol :external))))
    (let ((head (package-tables package)))
      (do ((prev head table)
	   (table (cdr head) (cdr table)))
	  ((null table) (values nil nil))
	(with-symbol (found symbol (car table) string length hash ehash)
	  (when found
	    (unless (eq prev head)
	      (shiftf (cdr prev) (cdr table) (cdr head) table))
	    (return-from find-symbol* (values symbol :inherited))))))))

;;; Find-External-Symbol  --  Internal
;;;
;;;    Similar to Find-Symbol, but only looks for an external symbol.
;;; This is used for fast name-conflict checking in this file and symbol
;;; printing in the printer.
;;;
(defun find-external-symbol (string package)
  (declare (simple-string string))
  (let* ((length (length string))
	 (hash (%sxhash-simple-string string))
	 (ehash (entry-hash length hash)))
    (declare (type index length hash))
    (with-symbol (found symbol (package-external-symbols package)
			string length hash ehash)
      (values symbol found))))

;;; Unintern  --  Public
;;;
;;;    If we are uninterning a shadowing symbol, then a name conflict can
;;; result, otherwise just nuke the symbol.
;;;
(defun unintern (symbol &optional (package *package*))
  "Makes Symbol no longer present in Package.  If Symbol was present
  then T is returned, otherwise NIL.  If Package is Symbol's home
  package, then it is made uninterned."
  (let* ((package (package-or-lose package))
	 (name (symbol-name symbol))
	 (shadowing-symbols (package-%shadowing-symbols package)))
    (declare (list shadowing-symbols) (simple-string name))
    ;;
    ;; If a name conflict is revealed, give use a chance to shadowing-import
    ;; one of the accessible symbols.
    (when (member symbol shadowing-symbols)
      (let ((cset ()))
	(dolist (p (package-%use-list package))
	  (multiple-value-bind (s w) (find-external-symbol name p)
	    (when w (pushnew s cset))))
	(when (cdr cset)
	  (loop
	   (cerror
	    "prompt for a symbol to shadowing-import."
	    "Uninterning symbol ~S causes name conflict among these symbols:~%~S"
	    symbol cset)
	   (write-string "Symbol to shadowing-import: " *query-io*)
	   (let ((sym (read *query-io*)))
	     (cond
	      ((not (symbolp sym))
	       (format *query-io* "~S is not a symbol."))
	      ((not (member sym cset))
	       (format *query-io* "~S is not one of the conflicting symbols."))
	      (t
	       (shadowing-import sym package)
	       (return-from unintern t)))))))
      (setf (package-%shadowing-symbols package)
	    (remove symbol shadowing-symbols)))

    (multiple-value-bind (s w) (find-symbol name package)
      (declare (ignore s))
      (cond ((or (eq w :internal) (eq w :external))
	     (nuke-symbol (if (eq w :internal)
			      (package-internal-symbols package)
			      (package-external-symbols package))
			  name)
	     (if (eq (symbol-package symbol) package)
		 (%set-symbol-package symbol nil))
	     t)
	    (t nil)))))

;;; Symbol-Listify  --  Internal
;;;
;;;    Take a symbol-or-list-of-symbols and return a list, checking types.
;;;
(defun symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s) (error "~S is not a symbol." s)))
	 thing)
	((symbolp thing) (list thing))
	(t
	 (error "~S is neither a symbol nor a list of symbols." thing))))

;;; Moby-Unintern  --  Internal
;;;
;;;    Like Unintern, but if symbol is inherited chases down the
;;; package it is inherited from and uninterns it there.  Used
;;; for name-conflict resolution.  Shadowing symbols are not
;;; uninterned since they do not cause conflicts.
;;;
(defun moby-unintern (symbol package)
  (unless (member symbol (package-%shadowing-symbols package))
    (or (unintern symbol package)
	(let ((name (symbol-name symbol)))
	  (multiple-value-bind (s w) (find-symbol name package)
	    (declare (ignore s))
	    (when (eq w :inherited)
	      (dolist (q (package-%use-list package))
		(multiple-value-bind (u x) (find-external-symbol name q)
		  (declare (ignore u))
		  (when x
		    (unintern symbol q)
		    (return t))))))))))

;;; Export  --  Public
;;;
;;;    Do more stuff.
;;;
(defun export (symbols &optional (package *package*))
  "Exports Symbols from Package, checking that no name conflicts result."
  (let ((package (package-or-lose package))
	(syms ()))
    ;;
    ;; Punt any symbols that are already external.
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w)
			   (find-external-symbol (symbol-name sym) package)
	(declare (ignore s))
	(unless (or w (member sym syms)) (push sym syms))))
    ;;
    ;; Find symbols and packages with conflicts.
    (let ((used-by (package-%used-by-list package))
	  (cpackages ())
	  (cset ()))
      (dolist (sym syms)
	(let ((name (symbol-name sym)))
	  (dolist (p used-by)
	    (multiple-value-bind (s w) (find-symbol name p)
	      (when (and w (not (eq s sym))
			 (not (member s (package-%shadowing-symbols p))))
		(pushnew sym cset)
		(pushnew p cpackages))))))
      (when cset
	(restart-case
	    (error "Exporting these symbols from the ~A package:~%~S~%~
		    results in name conflicts with these packages:~%~{~A ~}"
		   (package-%name package) cset (mapcar #'package-%name cpackages))
	  (unintern-conflicting-symbols ()
	   :report "Unintern conflicting symbols."
	   (dolist (p cpackages)
	     (dolist (sym cset)
	       (moby-unintern sym p))))
	  (skip-exporting-these-symbols ()
	   :report "Skip exporting conflicting symbols."
	   (setq syms (nset-difference syms cset))))))
    ;;
    ;; Check that all symbols are accessible.  If not, ask to import them.
    (let ((missing ())
	  (imports ()))
      (dolist (sym syms)
	(multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	  (cond ((not (and w (eq s sym))) (push sym missing))
		((eq w :inherited) (push sym imports)))))
      (when missing
	(cerror "Import these symbols into the ~A package."
		"These symbols are not accessible in the ~A package:~%~S"
		(package-%name package) missing)
	(import missing package))
      (import imports package))
    ;;
    ;; And now, three pages later, we export the suckers.
    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(nuke-symbol internal (symbol-name sym))
	(add-symbol external sym)))
    t))

;;; Unexport  --  Public
;;;
;;;    Check that all symbols are accessible, then move from external to
;;; internal.
;;;
(defun unexport (symbols &optional (package *package*))
  "Makes Symbols no longer exported from Package."
  (let ((package (package-or-lose package))
	(syms ()))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((or (not w) (not (eq s sym)))
	       (error "~S is not accessible in the ~A package."
		      sym (package-%name package)))
	      ((eq w :external) (pushnew sym syms)))))

    (let ((internal (package-internal-symbols package))
	  (external (package-external-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)
	(nuke-symbol external (symbol-name sym))))
    t))

;;; Import  --  Public
;;;
;;;    Check for name conflic caused by the import and let the user 
;;; shadowing-import if there is.
;;;
(defun import (symbols &optional (package *package*))
  "Make Symbols accessible as internal symbols in Package.  If a symbol
  is already accessible then it has no effect.  If a name conflict
  would result from the importation, then a correctable error is signalled."
  (let ((package (package-or-lose package))
	(symbols (symbol-listify symbols))
	(syms ())
	(cset ()))
    (dolist (sym symbols)
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(cond ((not w)
	       (let ((found (member sym syms :test #'string=)))
		 (if found
		     (when (not (eq (car found) sym))
		       (push sym cset))
		     (push sym syms))))
	      ((not (eq s sym)) (push sym cset))
	      ((eq w :inherited) (push sym syms)))))
    (when cset
      (cerror
       "Import these symbols with Shadowing-Import."
       "Importing these symbols into the ~A package causes a name conflict:~%~S"
       (package-%name package) cset))
    ;;
    ;; Add the new symbols to the internal hashtable.
    (let ((internal (package-internal-symbols package)))
      (dolist (sym syms)
	(add-symbol internal sym)))
    ;;
    ;; If any of the symbols are uninterned, make them be owned by Package.
    (dolist (sym symbols)
      (unless (symbol-package sym) (%set-symbol-package sym package)))
    (shadowing-import cset package)))

;;; Shadowing-Import  --  Public
;;;
;;;    If a conflicting symbol is present, unintern it, otherwise just
;;; stick the symbol in.
;;;
(defun shadowing-import (symbols &optional (package *package*))
  "Import Symbols into package, disregarding any name conflict.  If
  a symbol of the same name is present, then it is uninterned.
  The symbols are added to the Package-Shadowing-Symbols."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (sym (symbol-listify symbols))
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) package)
	(unless (and w (not (eq w :inherited)) (eq s sym))
	  (when (or (eq w :internal) (eq w :external))
	    ;;
	    ;; If it was shadowed, we don't want Unintern to flame out...
	    (setf (package-%shadowing-symbols package)
		  (remove s (the list (package-%shadowing-symbols package))))
	    (unintern s package))
	  (add-symbol internal sym))
	(pushnew sym (package-%shadowing-symbols package)))))
  t)


;;; Shadow  --  Public
;;;
;;;
(defun shadow (symbols &optional (package *package*))
  "Make an internal symbol in Package with the same name as each of the
  specified symbols, adding the new symbols to the Package-Shadowing-Symbols.
  If a symbol with the given name is already present in Package, then
  the existing symbol is placed in the shadowing symbols list if it is
  not already present."
  (let* ((package (package-or-lose package))
	 (internal (package-internal-symbols package)))
    (dolist (name (mapcar #'string
			  (if (listp symbols) symbols (list symbols))))
      (multiple-value-bind (s w) (find-symbol name package)
	(when (or (not w) (eq w :inherited))
	  (setq s (make-symbol name))
	  (%set-symbol-package s package)
	  (add-symbol internal s))
	(pushnew s (package-%shadowing-symbols package)))))
  t)



;;; Use-Package  --  Public
;;;
;;;    Do stuff to use a package, with all kinds of fun name-conflict
;;; checking.
;;;
(defun use-package (packages-to-use &optional (package *package*))
  "Add all the Package-To-Use to the use list for Package so that
  the external symbols of the used packages are accessible as internal
  symbols in Package."
  (let ((packages (package-listify packages-to-use))
	(package (package-or-lose package)))
    ;;
    ;; Loop over each package, use'ing one at a time...
    (dolist (pkg packages)
      (unless (member pkg (package-%use-list package))
	(let ((cset ())
	      (shadowing-symbols (package-%shadowing-symbols package))
	      (use-list (package-%use-list package)))
	  ;;
	  ;;   If the number of symbols already accessible is less than the
	  ;; number to be inherited then it is faster to run the test the
	  ;; other way.  This is particularly valuable in the case of
	  ;; a new package use'ing Lisp.
	  (cond
	   ((< (+ (internal-symbol-count package)
		  (external-symbol-count package)
		  (let ((res 0))
		    (dolist (p use-list res)
		      (incf res (external-symbol-count p)))))
	       (external-symbol-count pkg))
	    (do-symbols (sym package)
	      (multiple-value-bind (s w)
				   (find-external-symbol (symbol-name sym) pkg)
		(when (and w (not (eq s sym))
			   (not (member sym shadowing-symbols)))
		  (push sym cset))))
	    (dolist (p use-list)
	      (do-external-symbols (sym p)
		(multiple-value-bind (s w)
				     (find-external-symbol (symbol-name sym)
							   pkg)
		  (when (and w (not (eq s sym))
			     (not (member (find-symbol (symbol-name sym)
						       package)
					  shadowing-symbols)))
		    (push sym cset))))))
	   (t
	    (do-external-symbols (sym pkg)
	      (multiple-value-bind (s w)
				   (find-symbol (symbol-name sym) package)
		(when (and w (not (eq s sym))
			   (not (member s shadowing-symbols)))
		  (push s cset))))))
	  
	  (when cset
	    (cerror
	     "unintern the conflicting symbols in the ~2*~A package."
	     "Use'ing package ~A results in name conflicts for these symbols:~%~S"
	     (package-%name pkg) cset (package-%name package))
	    (dolist (s cset) (moby-unintern s package))))

	(push pkg (package-%use-list package))
	(push (package-external-symbols pkg) (cdr (package-tables package)))
	(push package (package-%used-by-list pkg)))))
  t)

;;; Unuse-Package  --  Public
;;;
;;;
(defun unuse-package (packages-to-unuse &optional (package *package*))
  "Remove Packages-To-Unuse from the use list for Package."
  (let ((package (package-or-lose package)))
    (dolist (p (package-listify packages-to-unuse))
      (setf (package-%use-list package)
	    (remove p (the list (package-%use-list package))))
      (setf (package-tables package)
	    (delete (package-external-symbols p)
		    (the list (package-tables package))))
      (setf (package-%used-by-list p)
	    (remove package (the list (package-%used-by-list p)))))
    t))

;;; Find-All-Symbols --  Public
;;;
;;;
(defun find-all-symbols (string-or-symbol)
  "Return a list of all symbols in the system having the specified name."
  (let ((string (string string-or-symbol))
	(res ()))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (multiple-value-bind (s w) (find-symbol string v)
		   (when w (pushnew s res))))
	     *package-names*)
    res))


;;; Apropos and Apropos-List.

(defun briefly-describe-symbol (symbol)
  (fresh-line)
  (prin1 symbol)
  (when (boundp symbol)
    (write-string ", value: ")
    (prin1 (symbol-value symbol)))
  (if (fboundp symbol)
      (write-string " (defined)")))

(defun apropos-search (symbol string)
  (declare (simple-string string))
  (do* ((index 0 (1+ index))
	(name (symbol-name symbol))
	(length (length string))
	(terminus (- (length name) length)))
       ((> index terminus)
	nil)
    (declare (simple-string name)
	     (type index index terminus length))
    (if (do ((jndex 0 (1+ jndex))
	     (kndex index (1+ kndex)))
	    ((= jndex length)
	     t)
	  (declare (fixnum jndex kndex))
	  (let ((char (schar name kndex)))
	    (unless (char= (schar string jndex) (char-upcase char))
	      (return nil))))
	(return t))))

;;; MAP-APROPOS -- public (extension).
;;;
(defun map-apropos (fun string &optional package external-only)
  "Call FUN with each symbol that contains STRING.
  If PACKAGE is supplied then only use symbols present in
  that package.  If EXTERNAL-ONLY is true then only use
  symbols exported from the specified package."
  (let ((string (string-upcase string)))
    (declare (simple-string string))
    (flet ((apropos-in-package (package)
             (if external-only
                 (do-external-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol)))
                 (do-symbols (symbol package)
                   (if (and (eq (symbol-package symbol) package)
                            (apropos-search symbol string))
                       (funcall fun symbol))))))
      (if (null package)
          (mapc #'apropos-in-package (list-all-packages))
          (apropos-in-package (package-or-lose package))))
    nil))

;;; APROPOS -- public.
;;; 
(defun apropos (string &optional package external-only)
  "Briefly describe all symbols which contain the specified String.
  If Package is supplied then only describe symbols present in
  that package.  If External-Only is true then only describe
  external symbols in the specified package."
  (map-apropos #'briefly-describe-symbol string package external-only)
  (values))

;;; APROPOS-LIST -- public.
;;; 
(defun apropos-list (string &optional package external-only)
  "Identical to Apropos, except that it returns a list of the symbols
  found instead of describing them."
  (collect ((result))
    (map-apropos #'(lambda (symbol)
		     (result symbol))
		 string package external-only)
    (result)))



;;; Initialization.

;;; The cold loader (Genesis) makes the data structure in *initial-symbols*.
;;; We grovel over it, making the specified packages and interning the
;;; symbols.  For a description of the format of *initial-symbols* see
;;; the Genesis source.

(defvar *initial-symbols*)

(defun package-init ()
  (let ((*in-package-init* t))
    (dolist (spec *initial-symbols*)
      (let* ((pkg (apply #'make-package (first spec)))
	     (internal (package-internal-symbols pkg))
	     (external (package-external-symbols pkg)))
	;;
	;; Put internal symbols in the internal hashtable and set package.
	(dolist (symbol (second spec))
	  (add-symbol internal symbol)
	  (%set-symbol-package symbol pkg))
	;;
	;; External symbols same, only go in external table.
	(dolist (symbol (third spec))
	  (add-symbol external symbol)
	  (%set-symbol-package symbol pkg))
	;;
	;; Don't set package for Imported symbols.
	(dolist (symbol (fourth spec))
	  (add-symbol internal symbol))
	(dolist (symbol (fifth spec))
	  (add-symbol external symbol))
	;;
	;; Put shadowing symbols in the shadowing symbols list.
	(setf (package-%shadowing-symbols pkg) (sixth spec))))

    (makunbound '*initial-symbols*) ; So it gets GC'ed.
    
    ;; Make some other packages that should be around in the cold load:
    (make-package "COMMON-LISP-USER" :nicknames '("CL-USER" "USER"))

    ;; Now do the *deferred-use-packages*:
    (dolist (args *deferred-use-packages*)
      (apply #'use-package args))
    (makunbound '*deferred-use-packages*)

    (setq *lisp-package* (find-package "LISP"))
    (setq *keyword-package* (find-package "KEYWORD"))

    ;; For the kernel core image wizards, set the package to *Lisp-Package*.
    (setq *package* *lisp-package*)))
