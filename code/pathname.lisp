;;; -*- Package: LISP -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pathname.lisp,v 1.3 1991/12/18 22:35:03 wlott Exp $")
;;;
;;; **********************************************************************
;;;
;;; Machine/filesystem independent pathname functions for CMU Common Lisp.
;;;
;;; Written by William Lott
;;; Earlier version written by Jim Large and Rob MacLachlan
;;;
;;; **********************************************************************

(in-package "LISP")

(export '(pathname pathnamep logical-pathname logical-pathname-p
	  parse-namestring merge-pathnames make-pathname
	  pathname-host pathname-device pathname-directory pathname-name
	  pathname-type pathname-version namestring file-namestring
	  directory-namestring host-namestring enough-namestring
	  wild-pathname-p pathname-match-p translate-pathname
	  translate-logical-pathname logical-pathname-translations
	  load-logical-pathname-translations *default-pathname-defaults*))

(in-package "EXTENSIONS")
(export '(search-list clear-search-list enumerate-search-list))

(in-package "LISP")



;;;; Structures and types.

(defstruct (pathname
	    (:conc-name %pathname-)
	    (:print-function %print-pathname)
	    (:constructor
	     %make-pathname (host device directory name type version))
	    (:predicate pathnamep)
	    (:make-load-form-fun :just-dump-it-normally))
  "Pathname is the structure of the file pathname.  It consists of a
   host, a device, a directory, a name, and a type."
  (host nil :type (or host null))
  (device nil :type (member nil :unspecific))
  (directory nil :type list)
  (name nil :type (or simple-string pattern null))
  (type nil :type (or simple-string pattern null (member :unspecific)))
  (version nil :type (or integer null (member :newest :wild))))

(defun %print-pathname (pathname stream depth)
  (declare (ignore depth))
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (cond (namestring
	   (format stream "#p~S" namestring))
	  (*print-readably*
	   (error "~S Cannot be printed readably." pathname))
	  (*print-pretty*
	   (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
	     (funcall (formatter
		       "~2IUnprintable pathname: ~_Host=~S, ~_Device=~S, ~_~
			Directory=~:/LISP:PPRINT-FILL/, ~_Name=~S, ~_~
			Type=~S, ~_Version=~S")
		      stream
		      (%pathname-host pathname)
		      (%pathname-device pathname)
		      (%pathname-directory pathname)
		      (%pathname-name pathname)
		      (%pathname-type pathname)
		      (%pathname-version pathname))))
	  (t
	   (funcall (formatter "#<Unprintable pathname, Host=~S, Device=~S, ~
				Directory=~S, File=~S, Name=~S, Version=~S>")
		    stream
		    (%pathname-host pathname)
		    (%pathname-device pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))))

(defstruct (host
	    (:print-function %print-host))
  (parse (required-argument) :type function)
  (unparse (required-argument) :type function)
  (unparse-host (required-argument) :type function)
  (unparse-directory (required-argument) :type function)
  (unparse-file (required-argument) :type function)
  (unparse-enough (required-argument) :type function)
  (customary-case (required-argument) :type (member :upper :lower)))

(defun %print-host (host stream depth)
  (declare (ignore depth))
  (print-unreadable-object (host stream :type t :identity t)))


;;;; Patterns

(defstruct (pattern
	    (:print-function %print-pattern)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-pattern (pieces)))
  (pieces nil :type list))

(defun %print-pattern (pattern stream depth)
  (declare (ignore depth))
  (print-unreadable-object (pattern stream :type t)
    (if *print-pretty*
	(let ((*print-escape* t))
	  (pprint-fill stream (pattern-pieces pattern) nil))
	(prin1 (pattern-pieces pattern) stream))))

(defun pattern= (pattern1 pattern2)
  (declare (type pattern pattern1 pattern2))
  (let ((pieces1 (pattern-pieces pattern1))
	(pieces2 (pattern-pieces pattern2)))
    (and (= (length pieces1) (length pieces2))
	 (every #'(lambda (piece1 piece2)
		    (typecase piece1
		      (simple-string
		       (and (simple-string-p piece2)
			    (string= piece1 piece2)))
		      (cons
		       (and (consp piece2)
			    (eq (car piece1) (car piece2))
			    (string= (cdr piece1) (cdr piece2))))
		      (t
		       (eq piece1 piece2))))
		pieces1
		pieces2))))



;;;; Utilities.

(defun compare-component (this that)
  (or (eql this that)
      (typecase this
	(simple-string
	 (and (simple-string-p that)
	      (string= this that)))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (compare-component (car this) (car that))
	      (compare-component (cdr this) (cdr that)))))))


;;;; Logical namestrings

#|
(defstruct (logical-host
	    (:include host
		      (:parse #'parse-logical-namestring)
		      ...)
	    (:print-function %print-logical-host))
  name
  translations)

(deftype logical-pathname ()
  '(satisfies logical-pathname-p))

(defun logical-pathname-p (thing)
  "Return T if THING is a LOGICAL-PATHNAME, and NIL if not."
  (and (pathnamep thing)
       (logical-host-p (%pathname-host thing))))
|#


;;;; Pathname functions.

(defvar *default-pathname-defaults*)

(defun pathname= (pathname1 pathname2)
  (and (eq (%pathname-host pathname1)
	   (%pathname-host pathname2))
       (compare-component (%pathname-device pathname1)
			  (%pathname-device pathname2))
       (compare-component (%pathname-directory pathname1)
			  (%pathname-directory pathname2))
       (compare-component (%pathname-name pathname1)
			  (%pathname-name pathname2))
       (compare-component (%pathname-type pathname1)
			  (%pathname-type pathname2))
       (compare-component (%pathname-version pathname1)
			  (%pathname-version pathname2))))

(defmacro with-pathname ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (etypecase ,var
		   (pathname ,var)
		   (string (parse-namestring ,var))
		   (stream (file-name ,var))))))
     ,@body))

(defun %print-namestring-parse-error (condition stream)
  (format stream "Parse error in namestring: ~?~%  ~A~%  ~V@T^"
	  (namestring-parse-error-complaint condition)
	  (namestring-parse-error-arguments condition)
	  (namestring-parse-error-namestring condition)
	  (namestring-parse-error-offset condition)))

(define-condition namestring-parse-error (error)
  ((complaint (required-argument))
   (arguments nil)
   (namestring (required-argument))
   (offset (required-argument)))
  (:report %print-namestring-parse-error))

(defun %parse-namestring (namestr start end host junk-allowed)
  (declare (type string namestr)
	   (type index start end)
	   (type host host)
	   (values (or null pathname) index))
  (cond (junk-allowed
	 (handler-case (%parse-namestring namestr start end host nil)
	   (namestring-parse-error (condition)
	     (values nil
		     (namestring-parse-error-offset condition)))))
	((simple-string-p namestr)
	 (multiple-value-bind
	     (new-host device directory file type version)
	     (funcall (host-parse host) namestr start end)
	   (values (%make-pathname (or new-host host)
				   device directory file type version)
		   end)))
	(t
	 (%parse-namestring (coerce namestr 'simple-string)
			    start end host nil))))

(defun parse-namestring (thing
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  (declare (type pathnamelike thing)
	   (type (or null host) host)
	   (type pathnamelike defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or null (not null)) junk-allowed)
	   (values pathname index))
  (if (stringp thing)
      (%parse-namestring thing start (or end (length thing))
			 (or host
			     (with-pathname (defaults defaults)
			       (%pathname-host defaults)))
			 junk-allowed)
      (with-pathname (pathname thing)
	(when host
	  (unless (eq host (%pathname-host pathname))
	    (error "Hosts do not match: ~S and ~S."
		   host
		   (%pathname-host pathname))))
	(values pathname start))))

(defun pathname (thing)
  (declare (type pathnamelike thing))
  (with-pathname (pathname thing)
    pathname))

(defun maybe-diddle-case (thing diddle-p)
  (declare (type (or list pattern simple-base-string (member :unspecific))
		 thing)
	   (values (or list pattern simple-base-string (member :unspecific))))
  (if diddle-p
      (labels ((check-for (pred in)
		 (etypecase in
		   (pattern
		    (dolist (piece (pattern-pieces in))
		      (when (typecase piece
			      (simple-string
			       (check-for pred piece))
			      (cons
			       (case (car in)
				 (:character-set
				  (check-for pred (cdr in))))))
			(return t))))
		   (list
		    (dolist (x in)
		      (when (check-for pred x)
			(return t))))
		   (simple-base-string
		    (dotimes (i (length in))
		      (when (funcall pred (schar in i))
			(return t))))
		   ((member :unspecific)
		    nil)))
	       (diddle-with (fun thing)
		 (etypecase thing
		   (pattern
		    (make-pattern
		     (mapcar #'(lambda (piece)
				 (typecase piece
				   (simple-base-string
				    (funcall fun thing))
				   (cons
				    (case (car piece)
				      (:character-set
				       (cons :character-set
					     (funcall fun (cdr piece))))
				      (t
				       piece)))
				   (t
				    piece)))
			     (pattern-pieces thing))))
		   (list
		    (mapcar fun thing))
		   (simple-base-string
		    (funcall fun thing)))))
	(let ((any-uppers (check-for #'upper-case-p thing))
	      (any-lowers (check-for #'lower-case-p thing)))
	  (cond ((and any-uppers any-lowers)
		 ;; Mixed case, stays the same.
		 thing)
	    (any-uppers
	     ;; All uppercase, becomes all lower case.
	     (diddle-with #'string-downcase thing))
	    (any-lowers
	     ;; All lowercase, becomes all upper case.
	     (diddle-with #'string-upcase thing))
	    (t
	     ;; No letters?  I guess just leave it.
	     thing))))
      thing))

(defun merge-directories (dir1 dir2 diddle-case)
  (if (eq (car dir1) :absolute)
      dir1
      (let ((results nil))
	(flet ((add (dir)
		 (if (and (eq dir :back)
			  results
			  (not (eq (car results) :back)))
		     (pop results)
		     (push dir results))))
	  (dolist (dir (maybe-diddle-case dir2 diddle-case))
	    (add dir))
	  (dolist (dir (cdr dir1))
	    (add dir)))
	(reverse results))))

(defun merge-pathnames (pathname
			&optional
			(defaults *default-pathname-defaults*)
			(default-version :newest))
  (with-pathname (defaults defaults)
    (let ((pathname (let ((*default-pathname-defaults* defaults))
		      (pathname pathname))))
      (let* ((default-host (%pathname-host defaults))
	     (pathname-host (%pathname-host pathname))
	     (diddle-case
	      (and default-host pathname-host
		   (not (eq (host-customary-case default-host)
			    (host-customary-case pathname-host))))))
	(%make-pathname (or pathname-host default-host)
			(or (%pathname-device pathname)
			    (maybe-diddle-case (%pathname-device defaults)
					       diddle-case))
			(merge-directories (%pathname-directory pathname)
					   (%pathname-directory defaults)
					   diddle-case)
			(or (%pathname-name pathname)
			    (maybe-diddle-case (%pathname-name defaults)
					       diddle-case))
			(or (%pathname-type pathname)
			    (maybe-diddle-case (%pathname-type defaults)
					       diddle-case))
			(or (%pathname-version pathname)
			    default-version))))))

(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    (list
     (collect ((results))
       (ecase (pop directory)
	 (:absolute
	  (results :absolute)
	  (when (search-list-p (car directory))
	    (results (pop directory))))
	 (:relative
	  (results :relative)))
       (dolist (piece directory)
	 (cond ((eq piece :wild)
		(results (make-pattern (list :multi-char-wild))))
	       ((eq piece :wild-inferiors)
		(error ":WILD-INFERIORS not supported."))
	       ((member piece '(:up :back))
		(results piece))
	       ((or (simple-string-p piece) (pattern-p piece))
		(results (maybe-diddle-case piece diddle-case)))
	       ((stringp piece)
		(results (maybe-diddle-case (coerce piece 'simple-string)
					    diddle-case)))
	       (t
		(error "~S is not allowed as a directory component." piece))))
       (results)))
    (simple-string
     `(:absolute
       ,(maybe-diddle-case directory diddle-case)))
    (string
     `(:absolute
       ,(maybe-diddle-case (coerce directory 'simple-string)
			   diddle-case)))))

(defun make-pathname (&key host device directory name type version
			   defaults (case :local))
  (declare (type (or host null) host)
	   (type (member nil :unspecific) device)
	   (type (or list string pattern (member :wild)) directory)
	   (type (or null string pattern (member :wild)) name)
	   (type (or null string pattern (member :wild)) type)
	   (type (or null integer (member :wild :newest)) version)
	   (type (or pathnamelike null) defaults)
	   (type (member :common :local) case))
  (let* ((defaults (if defaults
		       (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 (host (or host default-host))
	 (diddle-args (and (eq case :common)
			   (eq (host-customary-case host) :lower)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host)))))
    (macrolet ((pick (var field)
		 `(cond ((eq ,var :wild)
			 (make-pattern (list :multi-char-wild)))
			((or (simple-string-p ,var)
			     (pattern-p ,var))
			 (maybe-diddle-case ,var diddle-args))
			((stringp ,var)
			 (maybe-diddle-case (coerce ,var 'simple-string)
					    diddle-args))
			(,var
			 (maybe-diddle-case ,var diddle-args))
			(defaults
			 (maybe-diddle-case (,field defaults)
					    diddle-defaults))
			(t
			 nil))))
      (%make-pathname
       host
       (or device (if defaults (%pathname-device defaults)))
       (let ((dir (import-directory directory diddle-args)))
	 (if defaults
	     (merge-directories dir
				(%pathname-directory defaults)
				diddle-defaults)
	     dir))
       (pick name %pathname-name)
       (pick type %pathname-type)
       (cond
	   (version version)
	   (defaults (%pathname-version defaults))
	   (t nil))))))

(defun pathname-host (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

(defun pathname-device (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-device pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-directory (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-directory pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-name (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-name pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-type (pathname &key (case :local))
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-type pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

(defun pathname-version (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))

(defun namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun host-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-host host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun directory-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-directory host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun file-namestring (pathname)
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-file host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

(defun enough-namestring (pathname
			  &optional (defaults *default-pathname-defaults*))
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (with-pathname (defaults defaults)
	    (funcall (host-unparse-enough host) pathname defaults))
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))


;;;; Wild pathnames.

(defun wild-pathname-p (pathname &optional field-key)
  (declare (type pathnamelike pathname)
	   (type (member nil :host :device :directory :name :type :version)
		 field-key))
  (with-pathname (pathname pathname)
    (ecase field-key
      ((nil)
       (or (wild-pathname-p pathname :host)
	   (wild-pathname-p pathname :device)
	   (wild-pathname-p pathname :directory)
	   (wild-pathname-p pathname :name)
	   (wild-pathname-p pathname :type)
	   (wild-pathname-p pathname :version)))
      (:host
       (pattern-p (%pathname-host pathname)))
      (:device
       (pattern-p (%pathname-host pathname)))
      (:directory
       (some #'pattern-p (%pathname-directory pathname)))
      (:name
       (pattern-p (%pathname-name pathname)))
      (:type
       (pattern-p (%pathname-type pathname)))
      (:version
       (eq (%pathname-version pathname) :wild)))))

(defun pattern-matches (pattern string)
  (declare (type pattern pattern)
	   (type simple-string string))
  (let ((len (length string)))
    (labels ((maybe-prepend (subs cur-sub chars)
	       (if cur-sub
		   (let* ((len (length chars))
			  (new (make-string len))
			  (index len))
		     (dolist (char chars)
		       (setf (schar new (decf index)) char))
		     (cons new subs))
		   subs))
	     (matches (pieces start subs cur-sub chars)
	       (if (null pieces)
		   (if (= start len)
		       (values t (maybe-prepend subs cur-sub chars))
		       (values nil nil))
		   (let ((piece (car pieces)))
		     (etypecase piece
		       (simple-string
			(let ((end (+ start (length piece))))
			  (and (<= end len)
			       (string= piece string
					:start2 start :end2 end)
			       (matches (cdr pieces) end
					(maybe-prepend subs cur-sub chars)
					nil nil))))
		       (list
			(ecase (car piece)
			  (:character-set
			   (and (< start len)
				(let ((char (schar string start)))
				  (if (find char (cdr piece) :test #'char=)
				      (matches (cdr pieces) (1+ start) subs t
					       (cons char chars))))))))
		       ((member :single-char-wild)
			(and (< start len)
			     (matches (cdr pieces) (1+ start) subs t
				      (cons (schar string start) chars))))
		       ((member :multi-char-wild)
			(multiple-value-bind
			    (won new-subs)
			    (matches (cdr pieces) start subs t chars)
			  (if won
			      (values t new-subs)
			      (and (< start len)
				   (matches pieces (1+ start) subs t
					    (cons (schar string start)
						  chars)))))))))))
      (multiple-value-bind
	  (won subs)
	  (matches (pattern-pieces pattern) 0 nil nil nil)
	(values won (reverse subs))))))

(defun components-match (this that)
  (or (eq this that)
      (typecase this
	(simple-string
	 (typecase that
	   (pattern
	    (values (pattern-matches that this)))
	   (simple-string
	    (string= this that))))
	(pattern
	 (and (pattern-p that)
	      (pattern= this that)))
	(cons
	 (and (consp that)
	      (components-match (car this) (car that))
	      (components-match (cdr this) (cdr that))))
	((member :back :up :unspecific nil)
	 (and (pattern-p that)
	      (equal (pattern-pieces that) '(:multi-char-wild)))))))

(defun pathname-match-p (pathname wildname)
  (with-pathname (pathname pathname)
    (with-pathname (wildname wildname)
      (macrolet ((frob (field)
		   `(or (null (,field wildname))
			(components-match (,field pathname)
					  (,field wildname)))))
	(and (frob %pathname-host)
	     (frob %pathname-device)
	     (frob %pathname-directory)
	     (frob %pathname-name)
	     (frob %pathname-type)
	     (or (null (%pathname-version wildname))
	       (eq (%pathname-version wildname) :wild)
	       (eql (%pathname-version pathname)
		    (%pathname-version wildname))))))))

(defun substitute-into (pattern subs)
  (declare (type pattern pattern)
	   (type list subs))
  (let ((in-wildcard nil)
	(pieces nil)
	(strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
	     (push piece strings)
	     (setf in-wildcard nil))
	    (in-wildcard)
	    ((null subs))
	    (t
	     (let ((sub (pop subs)))
	       (etypecase sub
		 (pattern
		  (when strings
		    (push (apply #'concatenate 'simple-string
				 (nreverse strings))
			  pieces))
		  (dolist (piece (pattern-pieces sub))
		    (push piece pieces)))
		 (simple-string
		  (push sub strings))))
	     (setf in-wildcard t))))
    (when strings
      (push (apply #'concatenate 'simple-string
		   (nreverse strings))
	    pieces))
    (if (and pieces
	     (simple-string-p (car pieces))
	     (null (cdr pieces)))
	(car pieces)
	(make-pattern (nreverse pieces)))))

(defun translate-component (source from to)
  (typecase to
    (pattern
     (if (pattern-p from)
	 (typecase source
	   (pattern
	    (if (pattern= from source)
		source
		:error))
	   (simple-string
	    (multiple-value-bind
		(won subs)
		(pattern-matches from source)
	      (if won
		  (values (substitute-into to subs))
		  :error)))
	   (t
	    :error))
	 source))
    ((member nil :wild)
     source)
    (t
     (if (components-match source from)
	 to
	 :error))))

(defun translate-directories (source from to)
  (if (null to)
      source
      (let ((subs nil))
	(loop
	  for from-part in from
	  for source-part in source
	  do (when (pattern-p from-part)
	       (typecase source-part
		 (pattern
		  (if (pattern= from-part source-part)
		      (setf subs (append subs (list source-part)))
		      (return-from translate-directories :error)))
		 (simple-string
		  (multiple-value-bind
		      (won new-subs)
		      (pattern-matches from-part source-part)
		    (if won
			(setf subs (append subs new-subs))
			(return-from translate-directories :error))))
		 ((member :back :up)
		  (if (equal (pattern-pieces from-part)
			     '(:multi-char-wild))
		      (setf subs (append subs (list source-part)))
		      (return-from translate-directories :error)))
		 (t
		  (return-from translate-directories :error)))))
	(mapcar #'(lambda (to-part)
		    (if (pattern-p to-part)
			(if (or (eq (car subs) :up) (eq (car subs) :back))
			    (if (equal (pattern-pieces to-part)
				       '(:multi-char-wild))
				(pop subs)
				(error "Can't splice ~S into the middle of a ~
					wildcard pattern."
				       (car subs)))
			    (multiple-value-bind
				(new new-subs)
				(substitute-into to-part subs)
			      (setf subs new-subs)
			      new))
			to-part))
		to))))

(defun translate-pathname (source from-wildname to-wildname &key)
  (declare (type pathnamelike source from-wildname to-wildname))
  (with-pathname (source source)
    (with-pathname (from from-wildname)
      (with-pathname (to to-wildname)
	(macrolet ((frob (field)
		     `(let ((result (translate-component (,field source)
							 (,field from)
							 (,field to))))
			(if (eq result :error)
			    (error "~S doesn't match ~S" source from)
			    result))))
	  (%make-pathname (frob %pathname-host)
			  (frob %pathname-device)
			  (let ((result (translate-directories
					 (%pathname-directory source)
					 (%pathname-directory from)
					 (%pathname-directory to))))
			    (if (eq result :error)
				(error "~S doesn't match ~S" source from)
				result))
			  (frob %pathname-name)
			  (frob %pathname-type)
			  (frob %pathname-version)))))))


;;;; Search lists.

;;; The SEARCH-LIST structure.
;;; 
(defstruct (search-list
	    (:print-function %print-search-list)
	    (:make-load-form-fun
	     (lambda (search-list)
	       (values `(intern-search-list ',(search-list-name search-list))
		       nil))))
  ;;
  ;; The name of this search-list.  Always stored in lowercase.
  (name (required-argument) :type simple-string)
  ;;
  ;; T if this search-list has been defined.  Otherwise NIL.
  (defined nil :type (member t nil))
  ;;
  ;; The list of expansions for this search-list.  Each expansion is the list
  ;; of directory components to use in place of this search-list.
  (expansions nil :type list))

(defun %print-search-list (sl stream depth)
  (declare (ignore depth))
  (print-unreadable-object (sl stream :type t)
    (write-string (search-list-name sl) stream)))

;;; *SEARCH-LISTS* -- internal.
;;;
;;; Hash table mapping search-list names to search-list structures.
;;; 
(defvar *search-lists* (make-hash-table :test #'equal))

;;; INTERN-SEARCH-LIST -- internal interface.
;;;
;;; When search-lists are encountered in namestrings, they are converted to
;;; search-list structures right then, instead of waiting until the search
;;; list used.  This allows us to verify ahead of time that there are no
;;; circularities and makes expansion much quicker.
;;; 
(defun intern-search-list (name)
  (let ((name (string-downcase name)))
    (or (gethash name *search-lists*)
	(let ((new (make-search-list :name name)))
	  (setf (gethash name *search-lists*) new)
	  new))))

;;; CLEAR-SEARCH-LIST -- public.
;;;
;;; Clear the definition.  Note: we can't remove it from the hash-table
;;; because there may be pathnames still refering to it.  So we just clear
;;; out the expansions and ste defined to NIL.
;;; 
(defun clear-search-list (name)
  "Clear the current definition for the search-list NAME.  Returns T if such
   a definition existed, and NIL if not."
  (let* ((name (string-downcase name))
	 (search-list (gethash name *search-lists*)))
    (when (and search-list (search-list-defined search-list))
      (setf (search-list-defined search-list) nil)
      (setf (search-list-expansions search-list) nil)
      t)))

;;; CLEAR-ALL-SEARCH-LISTS -- sorta public.
;;;
;;; Again, we can't actually remove the entries from the hash-table, so we
;;; just mark them as being undefined.
;;;
(defun clear-all-search-lists ()
  "Clear the definition for all search-lists.  Only use this if you know
   what you are doing."
  (maphash #'(lambda (name search-list)
	       (declare (ignore name))
	       (setf (search-list-defined search-list) nil)
	       (setf (search-list-expansions search-list) nil))
	   *search-lists*)
  nil)

;;; EXTRACT-SEARCH-LIST -- internal.
;;;
;;; Extract the search-list from PATHNAME and return it.  If PATHNAME
;;; doesn't start with a search-list, then either error (if FLAME-IF-NONE
;;; is true) or return NIL (if FLAME-IF-NONE is false).
;;; 
(defun extract-search-list (pathname flame-if-none)
  (with-pathname (pathname pathname)
    (let* ((directory (%pathname-directory pathname))
	   (search-list (cadr directory)))
      (cond ((search-list-p search-list)
	     search-list)
	    (flame-if-none
	     (error "~S doesn't start with a search-list."))
	    (t
	     nil)))))

;;; SEARCH-LIST -- public.
;;;
;;; We have to convert the internal form of the search-list back into a
;;; bunch of pathnames.
;;; 
(defun search-list (pathname)
  "Return the expansions for the search-list starting PATHNAME.  If PATHNAME
   does not start with a search-list, then an error is signaled.  If
   the search-list has not been defined yet, then an error is signaled.
   The expansion for a search-list can be set with SETF."
  (with-pathname (pathname pathname)
    (let ((search-list (extract-search-list pathname t))
	  (host (pathname-host pathname)))
      (if (search-list-defined search-list)
	  (mapcar #'(lambda (directory)
		      (make-pathname :host host
				     :directory (cons :absolute directory)))
		  (search-list-expansions search-list))
	  (error "Search list ~S has not been defined yet." pathname)))))

;;; %SET-SEARCH-LIST -- public setf method
;;;
;;; Set the expansion for the search-list in PATHNAME.  If this would result
;;; in any circularities, we flame out.  If anything goes wrong, we leave the
;;; old defintion intact.
;;; 
(defun %set-search-list (pathname values)
  (let ((search-list (extract-search-list pathname t)))
    (labels
	((check (target-list path)
	   (when (eq search-list target-list)
	     (error "That would result in a circularity:~%  ~
		     ~A~{ -> ~A~} -> ~A"
		    (search-list-name search-list)
		    (reverse path)
		    (search-list-name target-list)))
	   (when (search-list-p target-list)
	     (push (search-list-name target-list) path)
	     (dolist (expansion (search-list-expansions target-list))
	       (check (car expansion) path))))
	 (convert (pathname)
	   (with-pathname (pathname pathname)
	     (when (or (pathname-name pathname)
		       (pathname-type pathname)
		       (pathname-version pathname))
	       (error "Search-lists cannot expand into pathnames that have ~
		       a name, type, or ~%version specified:~%  ~S"
		      pathname))
	     (let ((directory (pathname-directory pathname)))
	       (unless (eq (car directory) :absolute)
		 (error "Search-lists cannot expand into relative ~
			 pathnames:~%  ~S"
			pathname))
	       (let ((expansion (cdr directory)))
		 (check (car expansion) nil)
		 expansion)))))
      (setf (search-list-expansions search-list)
	    (if (listp values)
	      (mapcar #'convert values)
	      (list (convert values)))))
    (setf (search-list-defined search-list) t))
  values)

;;; ENUMERATE-SEARCH-LIST -- public.
;;; 
(defmacro enumerate-search-list ((var pathname &optional result) &body body)
  "Execute BODY with VAR bound to each successive possible expansion for
   PATHNAME and then return RESULT.  Note: if PATHNAME does not contain a
   search-list, then BODY is executed exactly once.  Everything is wrapped
   in a block named NIL, so RETURN can be used to terminate early.  Note:
   VAR is *not* bound inside of RESULT."
  (let ((body-name (gensym)))
    `(block nil
       (flet ((,body-name (,var)
		,@body))
	 (%enumerate-search-list ,pathname #',body-name)
	 ,result))))

(defun %enumerate-search-list (pathname function)
  (let ((search-list (extract-search-list pathname nil)))
    (cond
     ((not search-list)
      (funcall function pathname))
     ((not (search-list-defined search-list))
      (error "Undefined search list: ~A"
	     (search-list-name search-list)))
     (t
      (let ((tail (cddr (pathname-directory pathname))))
	(dolist (expansion
		 (search-list-expansions search-list))
	  (%enumerate-search-list (make-pathname :defaults pathname
						 :directory
						 (cons :absolute
						       (append expansion
							       tail)))
				  function)))))))
