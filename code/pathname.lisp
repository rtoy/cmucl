;;; -*- Package: LISP -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pathname.lisp,v 1.14 1992/09/03 12:58:42 phg Exp $")
;;;
;;; **********************************************************************
;;;
;;; Machine/filesystem independent pathname functions for CMU Common Lisp.
;;;
;;; Written by William Lott, enhancements for logical-pathnames
;;; written by Paul Gleichauf.
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
(export '(search-list search-list-defined-p clear-search-list
		      enumerate-search-list))

(in-package "LISP")


;;;; Structures and types.

;;; Pathname structure holds the essential properties of the parsed path.

(defstruct (pathname
	    (:conc-name %pathname-)
	    (:print-function %print-pathname)
	    (:constructor
	     %make-pathname (host device directory name type version))
	    (:predicate pathnamep)
	    (:make-load-form-fun :just-dump-it-normally))
  ;; Slot holds the host, at present either a UNIX or logical host.
  (host nil :type (or host null))
  ;; Device is the name of a logical or physical device holding files.
  (device nil :type (member nil :unspecific))
  ;; A list of strings that are the component subdirectory components.
  (directory nil :type list)
  ;; The filename.
  (name nil :type (or simple-string pattern null))
  ;; The type extension of the file.
  (type nil :type (or simple-string pattern null (member :unspecific)))
  ;; The version number of the file, a positive integer, but not supported
  ;; on standard UNIX filesystems.
  (version nil :type (or integer null (member :newest :wild))))

;;; %PRINT-PATHNAME -- Internal
;;;
;;;   The printed representation of the pathname structure.
;;;
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

;;;; HOST structure

;;;   The host structure holds the functions that both parse the pathname
;;; information into sturcture slot entries, and after translation the inverse
;;; (unparse) functions.
;;;
(defstruct (host
	    (:print-function %print-host))
  (parse (required-argument) :type function)
  (unparse (required-argument) :type function)
  (unparse-host (required-argument) :type function)
  (unparse-directory (required-argument) :type function)
  (unparse-file (required-argument) :type function)
  (unparse-enough (required-argument) :type function)
  (customary-case (required-argument) :type (member :upper :lower)))

;;; %PRINT-HOST -- Internal
;;;
(defun %print-host (host stream depth)
  (declare (ignore depth))
  (print-unreadable-object (host stream :type t :identity t)))


;;;; Patterns

;;;   Patterns are a list of entries and wildcards used for pattern matches
;;; of translations.

(defstruct (pattern
	    (:print-function %print-pattern)
	    (:make-load-form-fun :just-dump-it-normally)
	    (:constructor make-pattern (pieces)))
  (pieces nil :type list))

;;; %PRINT-PATTERN -- Internal
;;;
(defun %print-pattern (pattern stream depth)
  (declare (ignore depth))
  (print-unreadable-object (pattern stream :type t)
    (if *print-pretty*
	(let ((*print-escape* t))
	  (pprint-fill stream (pattern-pieces pattern) nil))
	(prin1 (pattern-pieces pattern) stream))))

;;; PATTERN= -- Internal
;;;
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

;;; PATTERN-MATCHES -- Internal
;;;
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

;;; COMPONENTS-MATCH -- Internal
;;;
;;;   Wilds in to are matched against from where both are either lists
;;; containing :wild and :wild-inferiors, patterns or strings.
;;; FROM = :WILD-INFERIORS or :WILD handled separately for directory
;;; component. Not communative.
;;;
(defun components-match (from to)
  (or (eq from to)
      (typecase from
	(simple-base-string
	 (typecase to
	   (pattern
	    (values (pattern-matches to from)))
	   (simple-base-string
	    (string-equal from to))))
	(pattern
	 (and (pattern-p to) (pattern= from to)))
	((member :wild) ; :WILD component matches any string, or pattern or NIL.
	 (or (stringp to)
	     (logical-host-p to)
	     (pattern-p to)
	     (member to '(nil :unspecific :wild :wild-inferiors))))
	(cons ; Watch for wildcards.
	 (and (consp from)
	      (let ((from1 (first from))
		    (from2 nil)
		    (to1 (first to)))
		(typecase from1
		  ((member :wild)
		   (or (stringp to1)
		       (pattern-p to1)
		       (not to1)
		       (eq to1 :unspecific)))
		  ((member :wild-inferiors)
		   (setf from2 (second from))
		   (cond ((not from2)
			  ;; Nothing left of from, hence anything else in to
			  ;; matches :wild-inferiors. 
			  t)
			 ((components-match
			   (rest (rest from))
			   (rest (member from2 to :test #'equal))))))
		  (keyword ; :unspecific, :up, :back
		   (and (keywordp to1)
			(eq from1 to1)
			(components-match (rest from) (rest to))))
		  (string
		   (and (stringp to1)
			(string-equal from1 to1)
			(components-match (rest from) (rest to))))))))
	((member :back :up :unspecific nil)
	 (and (pattern-p from)
	      (equal (pattern-pieces from) '(:multi-char-wild)))))))


;;;; Utilities.

;;; COMPARE-COMPONENT  -- Internal
;;;
;;; A predicate for comparing two pathname slot component sub-entries.
;;;
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


;;;; Pathname functions.

;;;   Implementation determined defaults to pathname slots.

(defvar *default-pathname-defaults*)

;;; PATHNAME= -- Internal
;;;
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

;;; WITH-PATHNAME -- Internal
;;;   Converts the var, a pathname designator (a pathname, or string, or 
;;; stream), into a pathname.
;;;
(defmacro with-pathname ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (etypecase ,var
		   (pathname ,var)
		   (string (parse-namestring ,var))
		   (stream (parse-namestring (file-name ,var)))))))
     ,@body))


;;; PATHNAME -- Interface
;;;
(defun pathname (thing)
  "Convert thing (a pathname, string or stream) into a pathname."
  (declare (type pathnamelike thing))
  (with-pathname (pathname thing)
    pathname))

;;; MAYBE-DIDDLE-CASE  -- Internal
;;;
;;;   Change the case of thing if diddle-p T.
;;;
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
		   ((member :unspecific :up :absolute :relative)
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
		    (funcall fun thing))
		   ((member :unspecific :up :absolute :relative)
		    thing))))
	(let ((any-uppers (check-for #'upper-case-p thing))
	      (any-lowers (check-for #'lower-case-p thing)))
	  (cond ((and any-uppers any-lowers)
		 ;; Mixed case, stays the same.
		 thing)
		(any-uppers
		 ;; All uppercase, becomes all lower case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-downcase x)
						x)) thing))
		(any-lowers
		 ;; All lowercase, becomes all upper case.
		 (diddle-with #'(lambda (x) (if (stringp x)
						(string-upcase x)
						x)) thing))
		(t
		 ;; No letters?  I guess just leave it.
		 thing))))
      thing))

;;; MERGE-DIRECTORIES -- Internal
;;;
(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
	  (null dir2))
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

;;; MERGE-PATHNAMES -- Interface
;;;
(defun merge-pathnames (pathname
			&optional
			(defaults *default-pathname-defaults*)
			(default-version :newest))
  "Construct a filled in pathname by completing the unspecified components
   from the defaults."
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

;;; IMPORT-DIRECTORY -- Internal
;;;
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
		(results piece))
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

;;; MAKE-PATHNAME -- Interface
;;;
(defun make-pathname (&key (host nil hostp)
			   (device nil devp)
			   (directory nil dirp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults (case :local))
  "Makes a new pathname from the component arguments.  Note that host is a host-
   structure."
  (declare (type (or host null) host)
	   (type (member nil :unspecific) device)
	   (type (or list string pattern (member :wild)) directory)
	   (type (or null string pattern (member :wild)) name)
	   (type (or null string pattern (member :unspecific :wild)) type)
	   (type (or null integer (member :unspecific :wild :newest)) version)
	   (type (or pathnamelike null) defaults)
	   (type (member :common :local) case))
  (let* ((defaults (if defaults
		       (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 (host (if hostp host default-host))
	 (diddle-args (and (eq case :common)
			   (eq (host-customary-case host) :lower)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host)))))
    (macrolet ((pick (var varp field)
		 `(cond ((eq ,var :wild)
			 (make-pattern (list :multi-char-wild)))
			((or (simple-string-p ,var)
			     (pattern-p ,var))
			 (maybe-diddle-case ,var diddle-args))
			((stringp ,var)
			 (maybe-diddle-case (coerce ,var 'simple-string)
					    diddle-args))
			(,varp
			 (maybe-diddle-case ,var diddle-args))
			(defaults
			 (maybe-diddle-case (,field defaults)
					    diddle-defaults))
			(t
			 nil))))
      (%make-pathname
       host
       (if devp device (if defaults (%pathname-device defaults)))
       (let ((dir (import-directory directory diddle-args)))
	 (if (and defaults (not dirp))
	     (merge-directories dir
				(%pathname-directory defaults)
				diddle-defaults)
	     dir))
       (pick name namep %pathname-name)
       (pick type typep %pathname-type)
       (cond
	 (versionp version)
	 (defaults (%pathname-version defaults))
	 (t nil))))))

;;; PATHNAME-HOST -- Interface
;;;
(defun pathname-host (pathname &key (case :local))
  "Accessor for the pathname's host."
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

;;; PATHNAME-DEVICE -- Interface
;;;
(defun pathname-device (pathname &key (case :local))
  "Accessor for pathname's device."
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-device pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-DIRECTORY -- Interface
;;;
(defun pathname-directory (pathname &key (case :local))
  "Accessor for the pathname's directory list."
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-directory pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))
;;; PATHNAME-NAME -- Interface
;;;
(defun pathname-name (pathname &key (case :local))
  "Accessor for the pathname's name."
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-name pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))

;;; PATHNAME-TYPE
;;;
(defun pathname-type (pathname &key (case :local))
  "Accessor for the pathname's name."
  (declare (type pathnamelike pathname)
	   (type (member :local :common) case))
  (with-pathname (pathname pathname)
    (maybe-diddle-case (%pathname-type pathname)
		       (and (eq case :common)
			    (eq (host-customary-case
				 (%pathname-host pathname))
				:lower)))))
;;; PATHNAME-VERSION
;;;
(defun pathname-version (pathname)
  "Accessor for the pathname's version."
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))


;;;; Namestrings

;;; %PRINT-NAMESTRING-PARSE-ERROR -- Internal
;;;
(defun %print-namestring-parse-error (condition stream)
  (format stream "Parse error in namestring: ~?~%  ~A~%  ~V@T^"
	  (namestring-parse-error-complaint condition)
	  (namestring-parse-error-arguments condition)
	  (namestring-parse-error-namestring condition)
	  (namestring-parse-error-offset condition)))

(define-condition namestring-parse-error (error)
  ((complaint :init-form (required-argument))
   (arguments :init-form nil)
   (namestring :init-form (required-argument))
   (offset :init-form (required-argument)))
  (:report %print-namestring-parse-error))

;;; %PARSE-NAMESTRING -- Internal
;;;
(defun %parse-namestring (namestr start end host junk-allowed)
  (declare (type string namestr)
	   (type index start end)
	   (type host host)
	   (values (or null pathname) index))
  (cond (junk-allowed
	 (handler-case (%parse-namestring namestr start end host nil)
	   (namestring-parse-error (condition)
	       (values nil (namestring-parse-error-offset condition)))))
	((simple-string-p namestr)
	 (multiple-value-bind
	     (new-host device directory file type version)
	     (funcall (host-parse host) namestr start end)
	   (values (%make-pathname (or new-host host)
				   device
				   directory
				   file
				   type
				   version)
		   end)))
	(t
	 (%parse-namestring (coerce namestr 'simple-string)
			    start end host nil))))

;;; PARSE-NAMESTRING -- Interface
;;;
(defun parse-namestring (thing
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  "Converts thing, a pathname designator, into a pathname structure, returns
   the printed representation."
  (declare (type (or simple-base-string stream pathname) thing)
	   (type (or null host) host)
	   (type pathnamelike defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or null (not null)) junk-allowed)
	   (values (or null pathname) index))
  (cond ((stringp thing)
	 (let* ((end1 (or end (length thing)))
		(things-host nil)
		(hosts-name (when host
			      (funcall (host-parse host) thing start end1))))
	   (setf things-host
		 (maybe-extract-logical-host thing start end1))
	   (when (and host things-host) ; A logical host and host are defined.
	     (unless (string= things-host hosts-name)
	       (error "Hosts do not match: ~S in ~S and ~S."
		      things-host thing host)))
	   (if things-host
	       (unless (gethash (string-downcase things-host) *search-lists*)
		 ;; Not a search-list name, make it a logical-host name.
		 (setf host (intern-logical-host things-host))))
	   (%parse-namestring thing start end1
			      (or host
				  (with-pathname (defaults defaults)
						 (%pathname-host defaults)))
			      junk-allowed)))
	((pathnamep thing)
	 (when host
	   (unless (eq host (%pathname-host thing))
	     (error "Hosts do not match: ~S and ~S."
		    host
		    (%pathname-host thing))))
	 (values thing start))
	((streamp thing)
	 (let ((host-name (funcall (host-unparse-host host) host))
	       (stream-type (type-of thing))
	       (stream-host-name (host-namestring thing)))
	   (unless (or (eq stream-type 'fd-stream)
		       ;;********Change fd-stream to file-stream in sources too.
		       (eq stream-type 'synonym-stream))
	     (error "Stream ~S was created with other than OPEN, WITH-OPEN-FILE~
		     or MAKE-SYNONYM-FILE." thing))
	   (unless (string-equal stream-host-name host-name)
	     (error "Hosts do not match: ~S and ~S."
		    host
		    host-name)))
	 (values thing start))))

;;; NAMESTRING -- Interface
;;;
(defun namestring (pathname)
  "Construct the full (name)string form of the pathname."
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (cond ((logical-host-p host)
	     (funcall (logical-host-unparse host) pathname))
	    ((host-p host)
	     (funcall (host-unparse host) pathname))
	    (t
	     (error
	      "Cannot determine the namestring for pathnames with no ~
	       host:~%  ~S" pathname))))))

;;; HOST-NAMESTRING -- Interface
;;;
(defun host-namestring (pathname)
  "Returns a string representation of the name of the host in the pathname."
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-host host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; DIRECTORY-NAMESTRING -- Interface
;;;
(defun directory-namestring (pathname)
  "Returns a string representation of the directories used in the pathname."
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-directory host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; FILE-NAMESTRING -- Interface
;;;
(defun file-namestring (pathname)
  "Returns a string representation of the name used in the pathname."
  (declare (type pathnamelike pathname))
  (with-pathname (pathname pathname)
    (let ((host (%pathname-host pathname)))
      (if host
	  (funcall (host-unparse-file host) pathname)
	  (error
	   "Cannot determine the namestring for pathnames with no host:~%  ~S"
	   pathname)))))

;;; ENOUGH-NAMESTRING -- Interface
;;;
(defun enough-namestring (pathname
			  &optional (defaults *default-pathname-defaults*))
  "Returns an abbreviated pathname sufficent to identify the pathname relative
   to the defaults."
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

;;; WILD-PATHNAME-P -- Interface
;;;
(defun wild-pathname-p (pathname &optional field-key)
  "Predicate for determining whether pathname contains any wildcards."
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

;;; PATHNAME-MATCH -- Interface
;;;
(defun pathname-match-p (pathname wildname)
  "Pathname matches the wildname template?"
  (with-pathname (pathname pathname)
    (with-pathname (wildname wildname)
      (macrolet ((frob (field)
		   `(or (null (,field wildname))
			(components-match (,field wildname)
					  (,field pathname)))))
	(and (frob %pathname-host)
	     (frob %pathname-device)
	     (frob %pathname-directory)
	     (frob %pathname-name)
	     (frob %pathname-type)
	     (or (null (%pathname-version wildname))
		 (eq (%pathname-version wildname) :wild)
		 (eql (%pathname-version pathname)
		      (%pathname-version wildname))))))))

;;; SUBSTITUTE-INTO -- Internal
;;;
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

;;; TRANSLATE-COMPONENT -- Internal
;;;
;;;   Use the source as a pattern to fill the from path and form the to path.
;;;
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

;;; TRANSLATE-DIRECTORIES -- Internal
;;;
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

;;; TRANSLATE-PATHNAME -- Interface
;;;
(defun translate-pathname (source from-wildname to-wildname &key)
  "Use the source pathname to translate the from-wildname's wild and
   unspecified elements into a completed to-pathname based on the to-wildname."
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
  (%expansions (%primitive c:make-value-cell nil)));  :type list))

(defun search-list-expansions (x)
  (%primitive c:value-cell-ref (search-list-%expansions x)))

(defun (setf search-list-expansions) (val x)
  (%primitive c:value-cell-set (search-list-%expansions x) val))

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
	     (error "~S doesn't start with a search-list." pathname))
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

;;; SEARCH-LIST-DEFINED-P -- public.
;;; 
(defun search-list-defined-p (pathname)
  "Returns T if the search-list starting PATHNAME is currently defined, and
   NIL otherwise.  An error is signaled if PATHNAME does not start with a
   search-list."
  (with-pathname (pathname pathname)
    (search-list-defined (extract-search-list pathname t))))

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
	       (let ((expansion
		      (if directory
			  (ecase (car directory)
			    (:absolute (cdr directory))
			    (:relative (cons (intern-search-list "default")
					     (cdr directory))))
			  (list (intern-search-list "default")))))
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


;;;;  Logical pathname support. ANSI 92-102 specification.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Logical pathnames have the following format:
;;;
;;; logical-namestring ::=
;;;         [host ":"] [";"] {directory ";"}* [name] ["." type ["." version]]
;;;
;;; host ::= word
;;; directory ::= word | wildcard-word | **
;;; name ::= word | wildcard-word
;;; type ::= word | wildcard-word
;;; version ::= pos-int | newest | NEWEST | *
;;; word ::= {uppercase-letter | digit | -}+
;;; wildcard-word ::= [word] '* {word '*}* [word]
;;; pos-int ::= integer > 0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Logical pathnames are a subclass of pathnames and can use the same
;; data structures with the device slot necessarily nil.  The current lack of
;; an integrated efficient CLOS means that the classes are mimiced using
;; structures.  They follow the pattern set by search-lists, a CMUCL specific
;; extension.

(defstruct (logical-host
	    (:include host
		      (:parse #'parse-logical-namestring)
		      (:unparse #'unparse-logical-namestring)
		      (:unparse-host #'unparse-logical-host)
		      (:unparse-directory #'unparse-logical-directory)
		      (:unparse-file #'unparse-logical-file)
		      (:unparse-enough #'identity)
		      (:customary-case :upper)))
  (name "" :type simple-string)
  (translations nil :type list)
  (canon-transls nil :type list))

(deftype logical-pathname ()
  '(satisfies logical-pathname-p))

;;; LOGICAL-PATHNAME-P -- Public
;;;
(defun logical-pathname-p (thing)
  "Return T if THING is a LOGICAL-PATHNAME object."
  (and (pathnamep thing)
       (logical-host-p (%pathname-host thing))))

;;; *LOGICAL-PATHNAMES* --internal.
;;;
;;; Hash table searching maps a logical-pathname's host to their physical
;;; pathname translation.

(defvar *logical-pathnames* (make-hash-table :test #'equal))

(define-condition logical-namestring-parse-error (error)
  ((complaint :init-form (required-argument))
   (arguments :init-form nil)
   (namestring :init-form (required-argument))
   (offset :init-form (required-argument)))
  (:report %print-namestring-parse-error))

;;; MAYBE-MAKE-LOGICAL-PATTERN -- Internal
;;;
(defun maybe-make-logical-pattern (namestr start end)
  "Take the ; reduced strings and break them into words and wildcard-words."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (collect ((pattern))
    (let ((last-regular-char nil)
	  (look-ahead+1 nil)
	  (index start)
	  (char nil))
      (flet ((flush-pending-regulars ()
	       (when last-regular-char
		 (pattern (subseq namestr last-regular-char index))
		 (setf last-regular-char nil))))
	(loop
	  (when (>= index end)
		(return)) 
	  (setf char (schar namestr index))
	  (cond ((or (char= #\. char) (char= #\; char)) ; End of pattern piece.
		 (flush-pending-regulars))
		((or (char= #\- char) ; Hyphen is a legal word character.
		     (alphanumericp char))    ; Building a word.
		 (unless last-regular-char
		   (setf last-regular-char index)))
		((char= #\* char) ; Wildcard word, :wild or wildcard-inferior.
		 (if (<= end index)
		     (setf look-ahead+1 nil)
		     (setf look-ahead+1 (schar namestr (1+ index))))
		 (cond ((or (char= #\. look-ahead+1)
			    (char= #\; look-ahead+1))
			(flush-pending-regulars)
			(pattern :wild)
			(incf index)) ; skip * and ;
		       ((and (char= #\* look-ahead+1)
			     (char= #\; (schar namestr (+ 2 index))))
			(pattern :wild-inferiors)
			(setq last-regular-char nil)
			(incf index 2)) ; skip ** and ;
		       (t ; wildcard-word, keep going
			(flush-pending-regulars)
			(pattern :wild)
			(incf index)
			(unless last-regular-char
			  (setf last-regular-char index))
			)))
		(t (error "Incorrect logical pathname syntax.")))
	  (incf index))
	(flush-pending-regulars))
    (cond ((null (pattern))
	   "")
	  ((and (null (cdr (pattern)))
		(simple-string-p (car (pattern))))
	   (car (pattern)))
	  ((= 1 (length (pattern)))
	   (let ((elmt (first (pattern))))
	     (if (or (eq elmt :wild) (eq elmt :wild-inferiors))
		 elmt)))
	  (t
	   (make-pattern (pattern)))))))

;;; INTERN-LOGICAL-HOST
;;;
(defun intern-logical-host (name)
  (declare (simple-string name)
	   (values logical-host))
  (let ((name (string-upcase name)))
    (or (gethash name *logical-pathnames*)
	(let ((new (make-logical-host :name name)))
	  (setf (gethash name *logical-pathnames*) new)
	  new))))

;;; EXTRACT-LOGICAL-NAME-TYPE-AND-VERSION
;;;
(defun extract-logical-name-type-and-version (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((last-dot (position #\. namestr :start (1+ start) :end end
			     :from-end t))
	 (second-to-last-dot (and last-dot
				  (position #\. namestr :start (1+ start)
					    :end last-dot :from-end t)))
	 (version :newest))
    ;; If there is a second-to-last dot, check to see if there is a valid
    ;; version after the last dot.
    (when second-to-last-dot
      (cond ((and (= (+ last-dot 2) end)
		  (char= (schar namestr (1+ last-dot)) #\*))
	     (setf version :wild))
	    ((and (< (1+ last-dot) end)
		  (do ((index (1+ last-dot) (1+ index)))
		      ((= index end) t)
		    (unless (char<= #\0 (schar namestr index) #\9)
		      (return nil))))
	     (setf version
		   (parse-integer namestr :start (1+ last-dot) :end end)))
	    (t
	     (setf second-to-last-dot nil))))
    (cond (second-to-last-dot
	   (values (maybe-make-logical-pattern
		    namestr start second-to-last-dot)
		   (maybe-make-logical-pattern
		    namestr (1+ second-to-last-dot) last-dot)
		   version))
	  (last-dot
	   (values (maybe-make-logical-pattern namestr start last-dot)
		   (maybe-make-logical-pattern namestr (1+ last-dot) end)
		   version))
	  (t
	   (values (maybe-make-logical-pattern namestr start end)
		   nil
		   version)))))

;;; LOGICAL-WORD-P -- Internal
;;;
;;;    Predicate for testing whether the syntax of the word is consistent
;;; with the form of a logical host.
;;;
(defun logical-word-p (word)
  (declare (type simple-base-string word)
	   (values boolean))
  (let ((ch nil))
    (dotimes (i (length word))
      (setf ch (schar word i))
      (unless (or (alphanumericp ch) (eq ch #\-))
	(return-from logical-word-p nil))))
  t)

;;; MAYBE-EXTRACT-LOGICAL-HOST -- Internal
;;;    Verify whether there is a logical host prefix in the namestr. If one is
;;; found return its name and the index of the remainder of the namestring.
;;; If not return nil.
;;;
(defun maybe-extract-logical-host (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start)
	   (type index start end)
	   (values (or (member :wild) simple-base-string null) (or null index)))
  (let ((colon-pos (position #\: namestr :start start :end end)))
    (if colon-pos
	(let ((host (subseq namestr start colon-pos)))
	  (cond ((logical-word-p host)
		 (return-from maybe-extract-logical-host
			      (values (string-upcase host) (1+ colon-pos))))
		((string= host "*")
		 (return-from maybe-extract-logical-host
			      (values :wild (1+ colon-pos))))))
	;; Implied host
	(values nil 0))))

;;; PARSE-LOGICAL-NAMESTRING  -- Internal
;;;
;;;   Break up a logical-namestring into its constituent parts.
;;;
(defun parse-logical-namestring (namestr start end)

  (declare (type simple-base-string namestr)
	   (type index start end)
	   (values (or logical-host null)
		   (or (member nil :unspecific) simple-base-string)
		   list
		   (or simple-base-string list pattern (member :wild))
		   (or simple-string pattern null (member :unspecific :wild))
		   (or integer null (member :newest :wild))))
  (multiple-value-bind ; Parse for :
      (host place)
      (maybe-extract-logical-host namestr start end)
    (typecase host
      (keyword t) ; :wild for example.
      (simple-string ; Already a search-list element?
       (unless (gethash (string-downcase host) *search-lists*)
	  (setf host (intern-logical-host host))))
      (null nil))
    (multiple-value-bind
	(absolute pieces) 
	(split-at-slashes namestr place end #\;)
      ;; Logical paths follow opposite convention of physical pathnames.
      (setf absolute (not absolute)) 
      (multiple-value-bind (name type version)
			   (let* ((tail (car (last pieces)))
				  (tail-start (car tail))
				  (tail-end (cdr tail)))
			     (unless (= tail-start tail-end)
			       (setf pieces (butlast pieces))
			       (extract-logical-name-type-and-version
				namestr tail-start tail-end)))
	;; Now we have everything we want.  So return it.
	(values host
		:unspecific
		(collect ((dirs))
		  (dolist (piece pieces)
		    (let ((piece-start (car piece))
			  (piece-end (cdr piece)))
		      (unless (= piece-start piece-end)
			(let ((dir (maybe-make-logical-pattern namestr
							       piece-start
							       piece-end)))
			  (if (and (simple-string-p dir)
				   (string= dir ".."))
			      (dirs :up)
			      (dirs dir))))))
		  (cond (absolute
			 (cons :absolute (dirs)))
			((dirs)
			 (cons :relative (dirs)))
			(t
			 nil)))
		name
		type
		version)))))

;;; UNPARSE-LOGICAL-DIRECTORY-LIST -- Internal
;;;
(defun unparse-logical-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
	   (when directory
	     (ecase (pop directory)
	       (:absolute
		;; Nothing special.
		)
	       (:relative
		(pieces ";")
		))
	     (dolist (dir directory)
	       (cond ((or (stringp dir) (pattern-p dir))
		      (pieces (unparse-logical-piece dir))
		      (pieces ";"))
		     ((eq dir :wild)
		      (pieces "*;"))
		     ((eq dir :wild-inferiors)
		      (pieces "**;"))
		     (t
		      (error "Invalid directory component: ~S" dir)))))
	   (apply #'concatenate 'simple-string (pieces))))

;;; UNPARSE-LOGICAL-DIRECTORY -- Internal
;;;
(defun unparse-logical-directory (pathname)
  (declare (type pathname pathname))
  (unparse-logical-directory-list (%pathname-directory pathname)))

;;; UNPARSE-LOGICAL-PIECE -- Internal
;;;
(defun unparse-logical-piece (thing)
  (etypecase thing
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   (#\*
	    (incf dstlen))))
       (let ((result (make-string dstlen))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       (#\*
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (collect ((strings))
	      (dolist (piece (pattern-pieces thing))
		(typecase piece
		  (simple-string
		   (strings piece))
		  (keyword
		   (cond ((eq piece :wild-inferiors)
			  (strings "**"))
			 ((eq piece :wild)
			  (strings "*"))
			 (t (error "Invalid keyword: ~S" piece))))
		  (t
		   (error "Invalid pattern piece: ~S" piece))))
	      (apply #'concatenate
		     'simple-string
		     (strings))))))

;;; UNPARSE-LOGICAL-FILE -- Internal
;;;
(defun unparse-logical-file (pathname)
  (declare (type pathname pathname))
  (declare (type pathname pathname))
  (unparse-unix-file pathname))

;;; UNPARSE-LOGICAL-HOST -- Internal
;;;
(defun unparse-logical-host (pathname)
  (declare (type logical-pathname pathname))
  (logical-host-name (%pathname-host pathname)))

;;; UNPARSE-LOGICAL-NAMESTRING -- Internal
;;;
(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
	       (unparse-logical-host pathname) ":"
	       (unparse-logical-directory pathname)
	       (unparse-logical-file pathname)))

;;; LOGICAL-PATHNAME -- Public
;;;
;;; Logical-pathname must signal a type error of type type-error.
;;;
(defun logical-pathname (pathspec)
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname string stream) pathspec)
	   (values logical-pathname))
  ;; Decide whether to typedef logical-pathname, logical-pathname-string,
  ;; or streams for which the pathname function returns a logical-pathname.
  (cond ((logical-pathname-p pathspec) pathspec)
	((stringp pathspec)
	 (if (maybe-extract-logical-host pathspec 0 (length pathspec))
	     (pathname pathspec)
	     (error "Pathspec is not a logical pathname prefaced by <host>:.")))
	((streamp pathspec)
	 (if (logical-pathname-p pathspec)
	     (pathname pathspec)
	     (error "Stream ~S is not a logical-pathname." pathspec)))
	(t
	 (error "~S is not either ~%
		 a logical-pathname object, or~%
		 a logical pathname namestring, or~%
		 a stream named by a logical pathname." pathspec))))

;;; TRANSLATIONS-TEST-P
;;;
;;;   Verify that the list of translations consists of lists and prepare
;;; canonical translations from the pathnames.
;;;
(defun translations-test-p (transl-list host)
  (declare (type logical-host host)
	   (type list transl-list)
	   (values boolean))
  (let ((can-transls nil))
    (setf can-transls (make-list (length transl-list))
	  (logical-host-canon-transls host) can-transls)
    (do* ((i 0 (1+ i))
	  (tr (nth i transl-list) (nth i transl-list))
	  (from-path (first tr) (first tr))
	  (to-path (second tr) (second tr))
	  (c-tr (nth i can-transls) (nth i can-transls)))
	 ((<= (length transl-list) i))
      (setf c-tr (make-list 2))
      (if (logical-pathname-p from-path)
	(setf (first c-tr) from-path)
	(setf (first c-tr) (parse-namestring from-path host)))
      (if (pathnamep to-path)
	  (setf (second c-tr) to-path)
	  (setf (second c-tr) (parse-namestring to-path)))
      ;; Verify form of translations.
      (unless (and (or (logical-pathname-p from-path)
		       (first c-tr))
		   (second c-tr))
	(return-from translations-test-p nil))		       
      (setf (nth i can-transls) c-tr)))
  (setf (logical-host-translations host) transl-list)
  t)

;;; LOGICAL-PATHNAME-TRANSLATIONS -- Public
;;;
(defun logical-pathname-translations (host)
  "Return the (logical) host object argument's list of translations."
  (declare (type (or simple-base-string logical-host) host)
	   (values list))
  (etypecase host
    (simple-string
     (setf host (string-upcase host))
     (let ((host-struc (gethash host *logical-pathnames*)))
       (if host-struc
	   (logical-host-translations host-struc)
	   (error "HOST ~S is not defined." host))))
    (logical-host
     (logical-host-translations host))))

;;; (SETF LOGICAL-PATHNAME-TRANSLATIONS) -- Public
;;;
(defun (setf logical-pathname-translations) (translations host)
  "Set the translations list for the logical host argument.
   Return translations."
  (declare (type (or simple-base-string logical-host) host)
	   (type list translations)
	   (values list))
  (typecase host 
    (simple-base-string
     (setf host (string-upcase host))
     (multiple-value-bind
	 (hash-host xst?)
	 (gethash host *logical-pathnames*)
       (unless xst?
	 (intern-logical-host host)
	 (setf hash-host (gethash host *logical-pathnames*)))
       (unless (translations-test-p translations hash-host)
	 (error "Translations ~S is not a list of pairs of from-, ~
		 to-pathnames." translations)))
     translations)
    (t
     (unless (translations-test-p translations host)
       (error "Translations ~S is not a list of pairs of from-, ~
	       to-pathnames." translations))
     translations)))

;;; The search mechanism for loading pathname translations uses the CMUCL
;;; extension of search-lists.  The user can add to the library: search-list
;;; using setf.  The file for translations should have the name defined by
;;; the hostname (a string) and with type component "translations".

;;; SAVE-LOGICAL-PATHNAME-TRANSLATIONS -- Public
;;;
(defun save-logical-pathname-translations (host directory)
  "Save the translations for host in the file named host in
   the directory argument. This is an internal convenience function and
   not part of the ANSI standard."
  (declare (type simple-base-string host directory))
  (setf host (string-upcase host))
  (let* ((p-name (make-pathname :directory (%pathname-directory
					    (pathname directory))
				:name host
				:type "translations"
				:version :newest))
	 (new-stuff (gethash host *logical-pathnames*))
	 (new-transl (logical-host-translations new-stuff)))
	(with-open-file (out-str p-name
				 :direction :output
				 :if-exists :new-version
				 :if-does-not-exist :create)
	  (write new-transl :stream out-str)
	  (format t "Created a new version of the file:~%   ~
		     ~S~% ~
		     containing logical-pathname translations:~%   ~
		     ~S~% ~
		     for the host:~%   ~
		     ~S.~%" p-name new-transl host))))

;;; Define a SYS area for system dependent logical translations, should we
;;; ever want to use them. ########### Decision still need to made whether
;;; to take advantage of this area.

#|
(progn
  (intern-logical-host "SYS")
  (save-logical-pathname-translations "SYS" "library:"))
|#
;;; LOAD-LOGICAL-PATHNAME-TRANSLATIONS -- Public
;;;
(defun load-logical-pathname-translations (host)
  "Search for a logical pathname named host, if not already defined. If already
   defined no attempt to find or load a definition is attempted and NIL is
   returned. If host is not already defined, but definition is found and loaded
   successfully, T is returned, else error."
  (declare (type simple-base-string host)
	   (values boolean))
  (setf host (string-upcase host))
  (let ((p-name nil)
	(p-trans nil))
    (multiple-value-bind
	(log-host xst?)
	(gethash host *logical-pathnames*)
      (if xst?
	  ;; host already has a set of defined translations.
	  (return-from load-logical-pathname-translations nil)
	  (enumerate-search-list (p "library:")
 	     (setf p-name (make-pathname :host (%pathname-host p)
					 :directory (%pathname-directory p)
					 :device (%pathname-device p)
					 :name host
					 :type "translations"
					 :version :newest))
	     (if (member p-name (directory p) :test #'pathname=)
		 (with-open-file (in-str p-name
					 :direction :input
					 :if-does-not-exist :error)
		   (setf p-trans (read in-str))
		   (setf log-host (intern-logical-host host))
		   (format t ";; Loading ~S~%" p-name)
		   (unless (translations-test-p p-trans log-host)
		     (error "Translations ~S is not a list of pairs of from-, ~
			     to-pathnames." p-trans))
		   (format t ";; Loading done.~%")
		   (return-from load-logical-pathname-translations t))))))))

;;; COMPILE-FILE-PATHNAME -- Public
;;;
(defun compile-file-pathname (file-path &key output-file)
  (declare (type (or string stream pathname logical-pathname) file-path)
	   (type (or string stream pathname logical-pathname) output-file)
	   (values pathname))
  (with-pathname (path file-path)
     (cond ((and (logical-pathname-p path) (not output-file))
	    (make-pathname :host (%pathname-host path)
			   :directory (%pathname-directory path)
			   :device (%pathname-device path)
			   :name (%pathname-name path)
			   :type (c:backend-fasl-file-type c:*backend*)))
	   ((logical-pathname-p path)
	    (translate-logical-pathname path))
	   (t file-path))))

;;; TRANSLATE-WILD-P -- Internal
;;;
(defmacro translate-wild-p (to-obj)
  "Translate :wild?"
  (declare (type keyword to-obj))
  `(etypecase ,to-obj
     ((or (member :wild :unspecific nil :up :back)
	  string
	  pattern)
      t)))

;;; INTERMEDIATE-REP -- Internal
;;;
(defun intermediate-rep (from to)
  "A logical component transition function that translates from one argument
   to the other. This function is specific to the CMUCL implementation."
  (declare (type (or logical-host host simple-base-string pattern symbol list)
		 from)
	   (type (or logical-host host simple-base-string pattern symbol list)
		 to)
	   (values
	    (or logical-host host simple-base-string pattern list symbol)))
  (etypecase from
    (logical-host
     (if (or (host-p to) (logical-host-p to))
	 to))
    (host
     (if (host-p to)
	 to))
    (simple-base-string 
     (etypecase to
       (pattern
	(multiple-value-bind
	    (won subs)
	    (pattern-matches to from)
	  (if won
	      (values (substitute-into to subs))
	      (error "String ~S failed to match pattern ~S" from to))))
       (simple-base-string to)
       ((member nil :wild :wild-inferiors) from)))
    (pattern
     (etypecase to
       (pattern
	(if (pattern= to from)
	    to
	    (error "Patterns ~S and ~S do not match.")))))
    ((member :absolute :relative)
     (if (eq to from)
	 to
	 (error "The directory bases (FROM = ~S, TO = ~S) for the logical ~%~
		 pathname translation are not consistently relative or absolute." from to)))
    ((member :wild)
     (etypecase to
       ((or string
	    pattern
	    (member nil :unspecific :newest :wild :wild-inferiors))
	to)))
    ((member :wild-inferiors) ; Only when single directory component.
     (etypecase to
       ((or string pattern cons (member nil :unspecific :wild :wild-inferiors))
	to)))
    ((member :unspecific nil)
     from)
    ((member :newest)
     (case to
       (:wild from)
       (:unspecific :unspecific)
       (:newest to)
       ((member nil) from)))))
    
(proclaim '(inline translate-logical-component))

;;; TRANSLATE-LOGICAL-COMPONENT -- Internal
;;;
(defun translate-logical-component (source from to)
  (intermediate-rep (intermediate-rep source from) to))

;;; TRANSLATE-LOGICAL-DIRECTORY  -- Internal
;;;
;;;   Translate logical directories within the UNIX heirarchical file system.
;;;
(defun translate-logical-directory (source from to)
  ;; Handle unfilled components.
  (if (or (eql source :UNSPECIFIC)
	  (eql from :UNSPECIFIC)
	  (eql to :UNSPECIFIC))
      (return-from translate-logical-directory :UNSPECIFIC))
  (if (or (not source) (not from) (not to))
      (return-from translate-logical-directory nil))
  ;; Handle directory component abbreviated as a wildcard.
  (if (member source '(:WILD :WILD-INFERIORS))
      (setf source '(:ABSOLUTE :WILD-INFERIORS)))
  (if (member source '(:WILD :WILD-INFERIORS))
      (setf source '(:ABSOLUTE :WILD-INFERIORS)))
  (if (member source '(:WILD :WILD-INFERIORS))
      (setf to '(:ABSOLUTE :WILD-INFERIORS)))
  ;; Make two stage translation, storing the intermediate results in ires
  ;; and finally returned in the list rres.
  (let ((ires nil)
	(rres nil)
	(dummy nil)
	(slen (length source))
	(flen (length from))
	(tlen (length to)))
    (do* ((i 0 (1+ i))
	  (j 0 (1+ j))
	  (k 0)
	  (s-el (nth i source) (nth i source))
	  (s-next-el nil)
	  (f-el (nth j from) (nth j from)))
	 ((<= slen i))
      (cond ((eq s-el :wild-inferiors)
	     (setf s-next-el (nth (+ 1 i) source)) ; NIL if beyond end.
	     (if (setf k (position s-next-el from :start (1+ j)))
		 ;; Found it, splice this portion into ires.
		 (setf ires
		       (append ires
			       (subseq from j (1- k))))
		       j (1- k))
		 (progn
		   ;; Either did not find next source element in from,
		   ;; or was nil.
		   (setf ires
			 (append ires
				 (subseq from j flen)))
		   (unless (= i (1- slen))
		     (error "Source ~S inconsistent with from translation ~
			     ~S~%." source from)))))
	    (t 
	     (setf ires (append ires (list (intermediate-rep s-el f-el)))))))
    ;; Remember to add leftover elements of from.
    (if (< slen flen)
	(setf ires (append ires (last from (- flen slen)))))
    (do* ((i 0 (1+ i))
	  (j 0 (1+ j))
	  (k 0)
	  (irlen (length ires))
	  (ir-el (nth i ires) (nth i ires))
	  (ir-next-el nil)
	  (t-el (nth j to) (nth j to)))
	 ((<= tlen i))
      ;; Remember to add leftover elements of to.
      (cond ((eq ir-el :wild-inferiors)
	     (setf ir-next-el (nth (+ 1 i) ires)) ; NIL if beyond end.
	     (if (setf k (position ir-next-el from :start (1+ j)))
		 ;; Found it, splice this portion into rres.
		 (setf rres
		       (append rres
			       (subseq from j (1- k)))
		       j (1- k))
		 (progn
		   ;; Either did not find next source element in from,
		   ;; or was nil.
		   (setf rres
			 (append rres
				 (subseq from j tlen)))
		   (unless (= i (1- irlen))
		     (error "Intermediate path ~S inconsistent with to~
			     translation ~S~%." ires to)))))
	    (t (if (setf dummy (intermediate-rep ir-el t-el))
		   (setf rres (append rres (list dummy)))))))
    (if (< flen tlen)
	(setf rres (append rres (last to (- tlen flen)))))
    rres))

;;; A physical-pathname is a pathname that does not contain any wildcards,
;;; but is not a logical-pathname. 

(deftype physical-pathname ()
  '(and (satisfies pathname-p)
	(not (or (satisfies wild-pathname-p)
		 (satisfies logical-pathname-p)))))

;;; TRANSLATE-LOGICAL-PATHNAME  -- Public
;;;
(defun translate-logical-pathname (pathname &key)
  "Translates pathname to a physical pathname, which is returned."
  (declare (type logical-pathname pathname))
  (with-pathname (source pathname)
    (etypecase source
      (physical-pathname source)
      (logical-pathname 
       (let ((source-host (%pathname-host source))
	     (result-path nil))
	 (unless (gethash
		  (funcall (logical-host-unparse-host source-host) source)
		  *logical-pathnames*)
	   (error "The logical host ~S is not defined.~%"
				    (logical-host-name source-host)))
	 (dolist (src-transl (logical-host-canon-transls source-host)
			     (error "~S has no matching translation for ~
				     logical host ~S.~%"
				    pathname (logical-host-name source-host)))
	   (when (pathname-match-p source (first src-transl))
	     (macrolet ((frob (field)
			  `(let* ((from (first src-transl))
				  (to (second src-transl))
				  (result (translate-logical-component
					   (,field source)
					   (,field from)
					   (,field to))))
				 result)))
	       (setf result-path
		     (%make-pathname (frob %pathname-host)
				     :unspecific
				     (let* ((from (first src-transl))
					    (to (second src-transl))
					    (result
					     (translate-logical-directory
					      (%pathname-directory source)
					      (%pathname-directory from)
					      (%pathname-directory to))))
				       (if (eq result :error)
					   (error "~S doesn't match ~S"
						  source from)
					   result))
				     (frob %pathname-name)
				     (frob %pathname-type)
				     (frob %pathname-version))))
	     (etypecase result-path
	       (logical-pathname 
		(translate-logical-pathname result-path))
	       (physical-pathname
		(return-from translate-logical-pathname result-path))))))))))




