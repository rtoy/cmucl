;;; -*- Package: LISP -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/pathname.lisp,v 1.16 1993/07/15 18:02:46 phg Exp $")
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


;;;; HOST structures

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

(defstruct (logical-host
	    (:include host
		      (:parse #'parse-logical-namestring)
		      (:unparse #'unparse-logical-namestring)
		      (:unparse-host #'unparse-logical-host)
		      (:unparse-directory #'unparse-logical-directory)
		      (:unparse-file #'unparse-logical-file)
		      (:unparse-enough #'identity)
		      (:customary-case :upper)))
  (name "" :type simple-base-string)
  (translations nil :type list)
  (canon-transls nil :type list))


;;;; Pathname structures

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
  (device nil :type (or null (member :unspecific)))
  ;; A list of strings that are the component subdirectory components.
  (directory nil :type list)
  ;; The filename.
  (name nil :type (or simple-string pattern null (member :wild)))
  ;; The type extension of the file.
  (type nil :type (or simple-string pattern null (member :wild :unspecific)))
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
;;; Physical pathnames include all these slots and a device slot.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Logical pathnames are a subclass of pathname, their class relations are
;; mimiced using structures for efficency.

(defstruct (logical-pathname
	    (:conc-name %logical-pathname-)
	    (:print-function %print-logical-pathname)
	    (:include pathname)
	    (:constructor
	     %make-logical-pathname (host device directory name type version))
	    (:predicate logical-pathname-p)
	    (:make-load-form-fun :just-dump-it-normally)))

;;; %PRINT-LOGICAL-PATHNAME -- Internal
;;;
;;;   The printed representation of the logical-pathname structure.
;;; The potential conflict with search-lists requires isolating the printed
;;; representation to use the i/o macro #.(logical-pathname <path-designator>).
;;;
(defun %print-logical-pathname (pathname stream depth)
  (declare (ignore depth))
  (let ((namestring (handler-case (namestring pathname)
		      (error nil))))
    (cond (namestring
	   (format stream "#.(logical-pathname ~S)" namestring))
	  (*print-readably*
	   (error "~S Cannot be printed readably." pathname))
	  (*print-pretty*
	   (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
	     (funcall (formatter
		       "~2IUnprintable pathname: ~_Host=~S, ~_~
			Directory=~:/LISP:PPRINT-FILL/, ~_Name=~S, ~_~
			Type=~S, ~_Version=~S")
		      stream
		      (%pathname-host pathname)
		      (%pathname-directory pathname)
		      (%pathname-name pathname)
		      (%pathname-type pathname)
		      (%pathname-version pathname))))
	  (t
	   (funcall (formatter "#<Unprintable pathname, Host=~S,  ~
				Directory=~S, File=~S, Name=~S, Version=~S>")
		    stream
		    (%pathname-host pathname)
		    (%pathname-directory pathname)
		    (%pathname-name pathname)
		    (%pathname-type pathname)
		    (%pathname-version pathname))))))

;;; *LOGICAL-HOSTS* --internal.
;;;
;;; Hash table searching maps a logical-pathname's host to their physical
;;; pathname translation.

(defvar *logical-hosts* (make-hash-table :test #'equal))

;;; PATHSPEC -- internal type
;;;
(deftype path-designator ()
  "A path specification, either a string, stream or pathname."
  '(or simple-base-string stream pathname))


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
;;;   If the string matches the pattern returns the multiple valuse T and a
;;; list of the matched strings.
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
		       ((member :wild :multi-char-wild)
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

;;; VERIFY-WORD-CHAR-P -- Internal
;;;
(defun verify-word-char-p (ch)
  (if (or (eq ch #\-)
	  (and (char<= #\A ch) (char<= ch #\Z))
	  (and (char<= #\0 ch) (char<= ch #\9)))
      t
      nil))

(defun verify-wild-word-char-p (ch)
  (if (or (and (char<= #\A ch) (char<= ch #\Z))
	  (and (char<= #\0 ch) (char<= ch #\9)))
      t
      nil))

;;; VERIFY-WORD-P -- Internal
;;;
(defun verify-word-p (wd)
  (declare (type simple-base-string wd))
  (let ((ch nil))
    (dotimes (j (length wd))
      (setf ch (schar wd j))
      (unless (verify-word-char-p ch)
	(error "~S is not a wildcard word, it contains an illegal character ~
		~S" wd ch))))
  t)

;;; HOSTS-MATCH-P -- Internal
;;;
;;;   Predicate for host matching.  No :wild hosts permitted.
;;;
(defun hosts-match-p (from-host to-host)
  (declare (type (or host simple-base-string) from-host to-host))
  (typecase from-host
    (logical-host ; Subclass on logical-host first.
     (typecase to-host
       (logical-host
	(eq from-host to-host))
       (host
	nil)
       (simple-base-string
	(eq from-host (gethash (string-upcase to-host) *logical-hosts*)))))
    (host
     (typecase to-host
       (logical-host
	nil)
       (host
	(eq from-host to-host))
       (simple-base-string
	(eq from-host (gethash (string-upcase to-host) *logical-hosts*)))))
    (simple-base-string
     (verify-word-p from-host)
     (typecase to-host
       (logical-host
	(eq to-host (gethash (string-upcase from-host) *logical-hosts*)))
       (simple-base-string
	(verify-word-p to-host)
	(string-equal from-host to-host))))))

;;; WILDCARD-WORD-PARSE -- Internal
;;;
;;;   Parse a potential wildcard-word for its subcomponents as a pattern,
;;; and return an error if the syntax is inconsistent.
;;;
(defun wildcard-word-parse (wd)
  (declare (type simple-base-string wd)
	   (values (or simple-base-string pattern)))
  (let* ((c nil)
	 (*-p (position #\* wd))
	 (start 0)
	 (end (length wd))
	 (end-1 (1- end))
	 (piece nil)
	 (pat nil))
    (when (and (not *-p) (verify-word-p wd))
      (return-from wildcard-word-parse wd))
    (dotimes (j end)
      (setf c (schar wd j))
     (when (eq c #\*)
	;; Finish the preceeding word, place in pattern.
	(setf piece (subseq wd start j))
	(when (< 0 (length piece))
	  (push piece pat))
	(push :wild pat)
	(setf *-p t
	      start (1+ j))
	(when (and (< j end-1) (eq (schar wd (1+ j)) #\*))
	      (error "~S is not a wildcard word, it contains a **." wd)))
      ;; Verify c is a legitimate wildcard character.
      (unless (verify-wild-word-char-p c)
	(error "~S is not a wildcard word, it contains an illegal character: ~
		~S" wd c))
      (when (= j end-1)
	(setf piece (subseq wd start (1+ j)))
	(when (< 0 (length piece))
	  (push piece pat))))
    (values (make-pattern (nreverse pat)))))

;;; COMPONENTS-MATCH -- Internal
;;;
;;;   Wilds in "to" are matched against "from" where both are strings,
;;; patterns or lists containing :wild and :wild-inferiors.
;;; FROM = :WILD-INFERIORS or :WILD handled separately for directory
;;; component. Not communative. Result is a Boolean or a member result.
;;;
(defun components-match (from to)
  (declare (type (or simple-base-string symbol pattern cons fixnum) from)
	   (type (or simple-base-string symbol pattern cons fixnum) to))
  (or (eq from to)
      (typecase from
	(simple-base-string
	 ;; Match can either be a identical pattern modulo wildcards or the
	 ;; same string.
	 (typecase to
	   (pattern 
	    (values (pattern-matches to from)))
	   (simple-base-string
	    (string-equal from to))))
	(pattern
	 ;; Match is a identical pattern.
	 (and (pattern-p to) (pattern= from to)))
	((member :wild)
	 ;; :WILD component matches any string, or pattern or NIL.
	 (or (stringp to)
	     (logical-host-p to)
	     (pattern-p to)
	     (member to '(nil :unspecific :newest :wild :wild-inferiors))))
	((member :newest)
	 ;; :newest matches itself, a wildcard or a positive integer version
	 ;; number.
	 (or (member to '(:wild :newest)) (and (integerp to) (plusp to))))
	(cons ;; A list that may include wildcards.
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
  (declare (type pathname pathname1)
	   (type pathname pathname2))
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
;;;   Converts the expr, a pathname designator (a pathname, or string, or 
;;; stream), into a pathname.
;;;
(defmacro with-pathname ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (etypecase ,var
		   (pathname ,var)
		   (string (parse-namestring ,var))
		   (stream (parse-namestring (file-name ,var)))))))
     ,@body))

;;; WITH-HOST -- Internal
;;;
;;; Converts the var, a host or string name for a host, into a logical-host
;;; structure or nil if not defined.
;;;
(defmacro with-host ((var expr) &body body)
  `(let ((,var (let ((,var ,expr))
		 (typecase ,var
		   (logical-host ,var)
		   (string (gethash ,var *logical-hosts*))
		   (t nil)))))
     ,@body))

;;; PATHNAME -- Interface
;;;
(defun pathname (thing)
  "Convert thing (a pathname, string or stream) into a pathname."
  (declare (type path-designator thing))
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
  (declare (type path-designator pathname)
	   (type path-designator defaults)
	   (values pathname))
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
(defun make-pathname (&key host
			   (device nil devp)
			   (directory nil dirp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults
			   (case :local))
  "Makes a new pathname from the component arguments.  Note that host is a host-
   structure."
  (declare (type (or host null) host)
	   (type (member nil :unspecific) device)
	   (type (or list string pattern (member :wild)) directory)
	   (type (or null string pattern (member :wild)) name)
	   (type (or null string pattern (member :unspecific :wild)) type)
	   (type (or null integer (member :unspecific :wild :newest)) version)
	   (type (or path-designator null) defaults)
	   (type (member :common :local) case))
  (let* ((defaults (when defaults
		     (with-pathname (defaults defaults) defaults)))
	 (default-host (if defaults
			   (%pathname-host defaults)
			   (pathname-host *default-pathname-defaults*)))
	 (host (or host default-host))
	 (diddle-args (and (eq case :common)
			   (eq (host-customary-case host) :lower)))
	 (diddle-defaults
	  (not (eq (host-customary-case host)
		   (host-customary-case default-host))))
	 (dev (if devp device (if defaults (%pathname-device defaults))))
	 (dir (import-directory directory diddle-args))
	 (ver (cond
	       (versionp version)
	       (defaults (%pathname-version defaults))
	       (t nil))))
    (when (and defaults (not dirp))
      (setf dir
	    (merge-directories dir
			       (%pathname-directory defaults)
			       diddle-defaults)))
    
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
      (if (logical-host-p host)
	  (%make-logical-pathname
	   host
	   nil
	   dir
	   (pick name namep %pathname-name)
	   (pick type typep %pathname-type)
	   ver)
	  (%make-pathname
	   host
	   dev
	   dir
	   (pick name namep %pathname-name)
	   (pick type typep %pathname-type)
	   ver)))))

;;; PATHNAME-HOST -- Interface
;;;
(defun pathname-host (pathname &key (case :local))
  "Accessor for the pathname's host."
  (declare (type path-designator pathname)
	   (type (member :local :common) case)
	   (values host)
	   (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

;;; PATHNAME-DEVICE -- Interface
;;;
(defun pathname-device (pathname &key (case :local))
  "Accessor for pathname's device."
  (declare (type path-designator pathname)
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
  (declare (type path-designator pathname)
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
  (declare (type path-designator pathname)
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
  (declare (type path-designator pathname)
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
  (declare (type path-designator pathname))
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

;;; %PARSE-PHYSICAL-NAMESTRING -- Internal
;;;
(defun %parse-physical-namestring (namestr things-host start end junk-allowed)
  (declare (type host things-host)
	   (type string namestr)
	   (type index start end))
  (cond (junk-allowed
	 (handler-case
	     (%parse-physical-namestring namestr things-host start end nil)
	   (namestring-parse-error (condition)
	     (values nil (namestring-parse-error-offset condition)))))
	((simple-string-p namestr)
	 (multiple-value-bind
	     (new-host device directory file type version)
	     (funcall (host-parse things-host) namestr start end)
	   (declare (ignore new-host))
	   (values
	    (%make-pathname things-host device directory file type version)
	    end)))
	(t
	 (%parse-physical-namestring (coerce namestr 'simple-base-string)
				     things-host
				     start end nil))))

;;; %PARSE-LOGICAL-NAMESTRING -- Internal
;;;
(defun %parse-logical-namestring (namestr things-host start end junk-allowed)
  (declare (type logical-host things-host)
	   (type string namestr)
	   (type index start end))
  (cond (junk-allowed
	 (handler-case
	     (%parse-logical-namestring namestr things-host start end nil)
	   (namestring-parse-error
	    (condition)
	    (values nil (namestring-parse-error-offset condition)))))
	((simple-string-p namestr)
	 (multiple-value-bind
	     (lpath end)
	     (parse-logical-namestring namestr :host things-host
				       :start start :end end)
	   (values lpath end)))
	(t
	 (%parse-logical-namestring (coerce namestr 'simple-base-string)
				    things-host
				    start end nil))))

;;; EXTRACT-PATH-PREFIX -- Internal
;;;
;;;   Extract the host or search-list prefix from the beginning of the
;;; namestring, use it to return the host structure, the colon-position
;;; in the namestring for further search, and whether the namestring specifies
;;; a logical namestring.
;;;
(defun extract-path-prefix (namestr start end host defaults)
  (declare (type simple-base-string namestr)
	   (type index start end)
	   (type (or null host) host)
	   (type pathname defaults)
	   (values host index (or t null)))
  (let* ((colon-pos (position #\: namestr :start start :end end))
	 (host (if host host (%pathname-host defaults)))
	 (host-temp nil)
	 (lpathp nil)
	 (prefix-str nil))
    (cond ((logical-host-p host)
	   (setf lpathp t)
	   (logical-host-name host))
	  (t
	   (funcall (host-unparse-host host) host)))
    (unless colon-pos ; No logical host or search-list prefix to namestr.
      (return-from extract-path-prefix (values host 0 lpathp)))
    (setf prefix-str (subseq namestr start colon-pos)
	  lpathp (logical-word-p prefix-str))
    (cond (lpathp ; If a legitimate logical host name prefix exists, use it.
	   (setf host-temp (gethash prefix-str *logical-hosts*))
	   (unless (and prefix-str host-temp)
	     (error "The logical-host ~S is not defined." prefix-str))
	   (setf host host-temp))
	  (t 
	   (unless (gethash prefix-str *search-lists*)
	     (error "The prefix ~S to the pathname string ~S is not a ~
		     registered search-list." prefix-str namestr))))
    (values host colon-pos lpathp)))

;;; PARSE-NAMESTRING -- Interface
;;;
(defun parse-namestring (thing
			 &optional host (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  "Converts pathname, a pathname designator, into a pathname structure,
   for a physical pathname, returns the printed representation. Host may be
   a physical host structure or host namestring."
  (declare (type path-designator thing)
	   (type (or null host) host)
	   (type pathname defaults)
	   (type index start)
	   (type (or index null) end)
	   (type (or t null) junk-allowed)
	   (values (or null pathname) (or null index)))
  (let* ((end1 (or end (length thing)))
	 (things-host nil)
	 (colon-pos nil)
	 (lpathp nil)) 
    (typecase thing
      (simple-base-string 
       (multiple-value-setq
	   (things-host colon-pos lpathp)
	 (extract-path-prefix thing start end1 host defaults))
       (if lpathp
	   (%parse-logical-namestring thing
				      things-host
				      colon-pos
				      end1
				      junk-allowed)
	   (%parse-physical-namestring thing
				       things-host
				       start
				       end1
				       junk-allowed)))
      (pathname ; structure type
       (let* ((host (if host host (%pathname-host defaults)))
	      (hosts-name (funcall (host-unparse-host host) host)))
	 (unless (eq hosts-name (%pathname-host thing))
	   (error "Hosts do not match: ~S and ~S."
		  hosts-name (%pathname-host thing))))
       (values thing start))
      (stream
       (let* ((stream-type (type-of thing))
	      (things-host-name (host-namestring thing))
	      (host (if host host (%pathname-host defaults)))
	      (hosts-name (funcall (host-unparse-host host) host)))
	 (unless (or (eq stream-type 'fd-stream)
		     ;;######Change fd-stream to file-stream in sources too.
		     (eq stream-type 'synonym-stream))
	   (error "Stream ~S was created with other than OPEN, WITH-OPEN-FILE~
		   or MAKE-SYNONYM-FILE." thing))
	 (unless (string-equal hosts-name things-host-name)
	   (error "Hosts do not match: ~S and ~S."
		  hosts-name things-host-name)))
       (values (file-name thing) start)))))

;;; NAMESTRING -- Interface
;;;
(defun namestring (pathname)
  "Construct the full (name)string form of the pathname."
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
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
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
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
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
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
  (declare (type path-designator pathname)
	   (values (or null simple-base-string)))
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
  (declare (type path-designator pathname))
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
  (declare (type path-designator pathname)
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
      (:host (pattern-p (%pathname-host pathname)))
      (:device (pattern-p (%pathname-host pathname)))
      (:directory (some #'pattern-p (%pathname-directory pathname)))
      (:name (pattern-p (%pathname-name pathname)))
      (:type (pattern-p (%pathname-type pathname)))
      (:version (eq (%pathname-version pathname) :wild)))))

;;; PATHNAME-MATCH -- Interface
;;;
(defun pathname-match-p (in-pathname in-wildname)
  "Pathname matches the wildname template?"
  (declare (type path-designator in-pathname))
  (with-pathname (pathname in-pathname)
    (with-pathname (wildname in-wildname)
      (macrolet ((frob (field)
		   `(or (null (,field wildname))
			(components-match (,field wildname)
					  (,field pathname)))))
	(and (or (null (%pathname-host wildname))
		 (components-match (logical-host-name
				    (%pathname-host wildname))
				   (logical-host-name
				    (%pathname-host pathname))))
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
;;;   Place the substitutions into the pattern and return the string or
;;; pattern that results. The case argument allows for the use of a :lower
;;; case to enable a UNIX and implementation specific translation of uppercase
;;; characters in logical-namestrings into lower case physical namestrings.
;;;
(defun substitute-into (pattern subs &key (case :common))
  (declare (type pattern pattern)
	   (type list subs)
	   (values (or simple-base-string pattern)))
  (let ((in-wildcard nil)
	(pieces nil)
	(strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
	     (if (eq case 'lower)
		 (push (string-downcase piece) strings)
		 (push piece strings))
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
		    (if (and (stringp piece) (eq case 'lower))
			(push (string-downcase piece) pieces)
			(push piece pieces))))
		 (simple-string
		  (if (eq case 'lower)
		      (push (string-downcase sub) strings)
		      (push sub strings)))))
	     (setf in-wildcard t))))
    (when strings
      (push (apply #'concatenate 'simple-string (nreverse strings))
	    pieces))
    (if (and pieces (simple-string-p (car pieces)) (null (cdr pieces)))
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
  (declare (type path-designator source from-wildname to-wildname))
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
;;;;  As logical-pathname translations are loaded they are canonicalized as
;;;;  patterns to enable rapid efficent translation into physical pathnames.

(define-condition logical-namestring-parse-error (error)
  ((complaint :init-form (required-argument))
   (arguments :init-form nil)
   (namestring :init-form (required-argument))
   (offset :init-form (required-argument)))
  (:report %print-namestring-parse-error))

;;; MAYBE-MAKE-LOGICAL-PATTERN -- Internal
;;;
;;;  Take the ; reduced strings and break them into words and wildcard-words.
;;;
(defun maybe-make-logical-pattern (namestr start end)
  (declare (type (or symbol simple-base-string) namestr)
	   (type index start end)
	   (values (or null symbol pattern simple-base-string)))
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
		((verify-word-char-p char) ; Building a word.
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

;;; INTERN-LOGICAL-HOST -- Internal
;;;
;;;   The name is a string. Put it in the hash table, return the logical-host.
;;;
(defun intern-logical-host (name)
  (declare (simple-string name)
	   (values logical-host))
  (unless (logical-word-p name)
    (error "Hostname ~S is not a legitimate logical word ~%
	       (consisting of uppercase letters, digits and hyphens)." name))
  (or (gethash name *logical-hosts*)
      (let ((new (make-logical-host :name name)))
	(setf (gethash name *logical-hosts*) new)
	new)))

;;; EXTRACT-LOGICAL-NAME-TYPE-AND-VERSION -- Internal
;;;
;;;   Return a set of three elements that can be any of patterns, strings,
;;; keywords, and integers.
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
	   (values (or t null)))
  (let ((ch nil))
    (dotimes (i (length word))
      (setf ch (schar word i))
      (unless (or (upper-case-p ch) (digit-char-p ch) (eq ch #\-))
	(return-from logical-word-p nil))))
  t)

;;; MAYBE-EXTRACT-LOGICAL-HOST -- Internal
;;;    Verify whether there is a logical host or search-list prefix in the
;;; namestr. If one is found return its name and the index of the remainder of
;;; the namestring.  If not return nil.
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
			      (values host (1+ colon-pos))))
		((string= host "*")
		 (return-from maybe-extract-logical-host
			      (values :wild (1+ colon-pos))))
		(t (error "Host component ~S in namestring ~S is neither a ~
			   wildcard (*),~%or a word formed from capital ~
			   letters, digits and hyphens." host namestr))))
	;; Implied host
	(values nil 0))))

;;; DECIDE-LOGICAL-HOST -- Internal
;;;
(defun decide-logical-host (host path-host defaults-host)
  (declare (type (or null host simple-base-string stream)
		 host path-host defaults-host)
	   (values (or null logical-host)))
  (with-host (host-struc host)
     (with-host (path-host-struc path-host)
	(with-host (defaults-host-struc defaults-host)
	  (if host-struc
	      host-struc
	      (if path-host-struc
		  path-host-struc
		  (if defaults-host-struc
		      defaults-host-struc
		      (error "None of ~S, ~S, or ~S is a logical-host"
			     host path-host defaults-host))))))))

;;; PARSE-LOGICAL-NAMESTRING  -- Internal
;;;
;;;   Break up a logical-namestring, always a string, into its constituent
;;; parts.
;;;
(defun parse-logical-namestring (namestr
				 &key
				 host
				 (defaults *default-pathname-defaults*)
				 (start 0)
				 (end (length namestr)))
  (declare (type simple-base-string namestr)
	   (type index start end)
	   (type (or null simple-base-string logical-host stream) host)
	   (type pathname defaults)
	   (values (or null logical-pathname) (or null index)))
  (let ((namestring (string-upcase namestr))
	(default-host (pathname-host defaults)))
    ;; Parse for : prefixed hostname if present in namestr.
    (multiple-value-bind (namestr-host place)
			 (maybe-extract-logical-host namestring start end)
      ;; The explicit host argument is a logical host, or the host's name
      ;; or the defaults provide the host, in that order.
      (setf host (decide-logical-host host namestr-host default-host))
      (multiple-value-bind (absolute pieces)
			   (split-at-slashes namestring place end #\;)
	;; Logical paths follow opposite convention of physical pathnames.
	(setf absolute (not absolute)) 
	(multiple-value-bind (name type version)
			     (let* ((tail (car (last pieces)))
				    (tail-start (car tail))
				    (tail-end (cdr tail)))
			       (unless (= tail-start tail-end)
				 (setf pieces (butlast pieces))
				 (extract-logical-name-type-and-version
				  namestring tail-start tail-end)))
	  ;; Now we have everything we want.  Construct a logical pathname.
	  (%make-logical-pathname
	   host
	   nil
	   (collect ((dirs))
		    (dolist (piece pieces)
		      (let ((piece-start (car piece))
			    (piece-end (cdr piece)))
			(unless (= piece-start piece-end)
			  (let ((dir
				 (maybe-make-logical-pattern namestring
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
	   version))))))

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
  (declare (type logical-pathname pathname))
  (unparse-logical-directory-list (%logical-pathname-directory pathname)))

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
  (unparse-unix-file pathname))

;;; UNPARSE-LOGICAL-HOST -- Internal
;;;
(defun unparse-logical-host (pathname)
  (declare (type logical-pathname pathname))
  (logical-host-name (%logical-pathname-host pathname)))

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
;;; Logical-pathname must signal an error of type type-error.
;;;
(defun logical-pathname (pathspec)
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname simple-base-string stream) pathspec)
	   (values logical-pathname))
  ;; Decide whether to typedef logical-pathname, logical-pathname-string,
  ;; or streams for which the pathname function returns a logical-pathname.
  (etypecase pathspec
    (logical-pathname pathspec)
    (simple-base-string
     (let* ((l-pathspec (length pathspec))
	    (pathspec-host
	     (maybe-extract-logical-host pathspec 0 l-pathspec)))
       (if pathspec-host
	   (parse-logical-namestring pathspec :host pathspec-host
				     :start 0 :end l-pathspec)
	   (error "Path specification ~S is not a logical pathname ~
		   prefaced by <host>:." pathspec))))
    (stream
     (let ((stream-type (type-of pathspec))
	   (path-file (file-name pathspec)))
       (unless (or (eq stream-type 'fd-stream)
		   (eq stream-type 'synonym-stream))
	 (error "Stream ~S was created with other than OPEN, WITH-OPEN-FILE~
		 or MAKE-SYNONYM-FILE." pathspec))
       (parse-logical-namestring path-file
				 :start 0 :end (length path-file))))))

;;; TRANSLATIONS-TEST-P -- Internal
;;;
;;;   Verify that the list of translations consists of lists and prepare
;;; canonical translations (parse pathnames and expand out wildcards into
;;; patterns).
;;;
(defun translations-test-p (transl-list host)
  (declare (type logical-host host)
	   (type list transl-list)
	   (values (or t null)))
  (let ((can-transls (make-list (length transl-list))); Canonical translations.
	(c-tr nil))
    (setf (logical-host-canon-transls host) can-transls)
    (do* ((i 0 (1+ i))
	  (tr (nth i transl-list) (nth i transl-list))
	  (from-path (first tr) (first tr))
	  (to-path (second tr) (second tr)))
	 ((<= (length transl-list) i))
      (setf c-tr (make-list 2))
      (if (logical-pathname-p from-path)
	(setf (first c-tr) from-path)
	(setf (first c-tr) (parse-namestring from-path host)))
      (if (pathnamep to-path)
	  (setf (second c-tr) to-path)
	  (setf (second c-tr) (parse-namestring to-path)))
      ;; Verify form of translations.
      (unless (and (or (logical-pathname-p from-path) (first c-tr))
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
     (let ((host-struc (gethash (string-upcase host) *logical-hosts*)))
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
  (setf host (string-upcase host))
  (typecase host 
    (simple-base-string
     (unless (logical-word-p host)
       (error "Hostname ~S is not a legitimate logical word ~%
	       (consisting of uppercase letters, digits and hyphens)." host))
     (multiple-value-bind
	 (hash-host xst?)
	 (gethash host *logical-hosts*)
       (unless xst?
	 (setf hash-host (intern-logical-host host)))
       (unless (translations-test-p translations hash-host)
	 (error "Translations ~S is not a list of pairs of from-, ~
		 to-pathnames." translations)))
     translations)
    (t
     (format t "TRANSLATIONS-TEST-P args = ~S, ~S~%" translations host)
     (unless (translations-test-p translations host)
       (error "Translations ~S is not a list of pairs of from- and ~
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
	 (new-stuff (gethash host *logical-hosts*))
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
#|
;;; Define a SYS area for system dependent logical translations, should we
;;; ever want to use them. Not currently used in CMUCL.

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
	   (values (or t null)))
  (setf host (string-upcase host))
  (let ((p-name nil)
	(p-trans nil))
    (multiple-value-bind
	(log-host xst?)
	(gethash host *logical-hosts*)
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
  (declare (type path-designator file-path)
	   (type (or null pathname) output-file)
	   (values (or null pathname)))
  (if (logical-pathname-p file-path)
      (if output-file
	  (translate-logical-pathname file-path)
	  (%make-logical-pathname
	   (or (%logical-pathname-host file-path)
	       (%pathname-host *default-pathname-defaults*))
	   nil
	   (or (%logical-pathname-directory file-path)
	       (%pathname-directory *default-pathname-defaults*))
	   (or (%logical-pathname-name file-path)
	       (%pathname-name *default-pathname-defaults*))
	   (c:backend-fasl-file-type c:*backend*)
	   (%pathname-version *default-pathname-defaults*)))
      (with-pathname (path file-path)
		     path)))

;;; TRANSLATE-LOGICAL-HOST -- Internal
;;;
(defun translate-logical-host (path-host from-host to-host)
  "Pathname must contain a logical host or wild cards."
  (declare (type (or logical-host host) path-host from-host to-host))
  (cond ((or (eq path-host from-host) (eq from-host :wild)) to-host)
	(t (throw 'next-translation nil))))

(defmacro translate-absolute-relative (src from to)
  "Translate :ABSOLUTE and RELATIVE keywords."
  `(if (eq ,src ,from)
	  ,to
	  (throw 'next-translation nil)))

(defmacro cleanup-to (from-context to-context result)
  `(unless ,from-context
     (setf ,result (append (reverse ,to-context) ,result))))

;;; TRANSLATE-DIR-ELEMENTS -- Internal
;;;
;;;   The translation of pathnames occurs in two stages, the first produces an
;;; intermediate result upon which the second is repeated.
;;; The pathname result is a copy of the to element with each missing or
;;; wildcard field filled in by a portion of from and placed in result.
;;; If the to field is a :wild or :wild-inferiors, it is copied without any
;;; further action. Wildcard-inferiors in the from field set the wild-inf-flag
;;; and push the to field element onto the result, continuing until a match is
;;; found.
;;;
(defun translate-dir-elements (from from-context to to-context result
				    &optional wild-inf-flag)
  "Translations are based on the element types of from and to, which can
   recursively effect their repective contexts when they are :wild-inferiors."
  (declare (type
	    (or null (member :wild :wild-inferiors) pattern
		simple-base-string)
	    from to)
	   (type list from-context to-context)
	   (type (or null t) wild-inf-flag)
	   (values list list list)) 
  (let ((match-p nil)
	(matches nil))
    (typecase from
      (simple-base-string
       (typecase to
	 (simple-base-string
	  (cond (wild-inf-flag
		 (push (string-downcase to) result)
		 (multiple-value-setq (from-context to-context result)
		   (translate-dir-elements from from-context
					   (pop to-context) to-context t)))
		(t ; Clean up, include any untranslated to leftovers.
		 (push (string-downcase to) result)
		 (cleanup-to from-context to-context result))))
	 (pattern
	  (multiple-value-setq (match-p matches)
	    (pattern-matches to from))
	  (cond (match-p
		 (push (string-downcase from) result))
		(wild-inf-flag
		 (push (string-downcase from) result)
		 (multiple-value-setq (from-context to-context result)
		   (translate-dir-elements from from-context
					   (pop to-context) to-context t)))
		(t
		 (throw 'next-translation nil))))
	 ((member :wild :wild-inferiors)
	  ;; Clean up, include any untranslated to leftovers.
	  (push (string-downcase from) result)
	  (cleanup-to from-context to-context result))))
      (pattern
       (typecase to
	 (simple-base-string
	  (multiple-value-setq (match-p matches)
	    (pattern-matches from to))
	  (cond	(match-p 
		 (push (string-downcase to) result)
		 (cleanup-to from-context to-context result))
		(wild-inf-flag
		 (push (string-downcase to) result)
		 (multiple-value-setq (from-context to-context result)
		   (translate-dir-elements (pop from-context) from-context
					   to to-context t)))
		(t
		 (throw 'next-translation nil))))
	 (pattern
	  (cond ((and (pattern= from to) wild-inf-flag)
		 (push to result)
		 (multiple-value-setq (from-context to-context result)
		   (translate-dir-elements (pop from-context) from-context
					   to to-context t)))
		((pattern= from to)
		 ;; Clean up, include any untranslated to leftovers.
		 (push to result)
		 (cleanup-to from-context to-context result))
		(t
		 (throw 'next-translation nil))))
	 ((member :wild :wild-inferiors)
	  (push to result)
	  ;; Clean up, include any untranslated to leftovers.
	  (cleanup-to from-context to-context result))))
      ((member :wild)
       ;; Clean up, include any untranslated to leftovers.
       (push to result)
       (cleanup-to from-context to-context result))
      ((member :wild-inferiors)
       (push to result)
       (multiple-value-setq (from-context to-context result)
	 (translate-dir-elements (pop from-context) from-context
				 to to-context t))))
    (values from-context to-context result)))

;;; TRANSLATE-DIRECTORY-LISTS -- Internal
;;;
;;;   Translate through the lists of strings of subdirectories.
;;;
(defun translate-directory-lists (from to result)
  (declare (type list from to result)) 
  (let ((from-el (pop from))
	(to-el (pop to))) 
    (cond (from-el
	   ;; There remains an untranslated element, translate it and the rest.
	   (multiple-value-setq (from to result)
	     (translate-dir-elements from-el from to-el to result))
	   (translate-directory-lists from to result))
	  (t ; Done.
	   (setf result (reverse result))))))

;;; TRANSLATE-LOGICAL-DIRECTORY  -- Internal
;;;
;;;   Translate logical directories within the UNIX hierarchical file system,
;;; which does not directly support :wildcard-inferiors.  Here :wild-inferiors
;;; are allowed in a restricted form. The translation table is based on matching
;;; first the source (src) directory component with the from directory
;;; components, and if successful constructing result directory components.
;;; If this is successful, then the result is matched relative to the the to-dir
;;; and a possible translated result is generated.
;;;
(defun translate-logical-directory (src-dirs from-dirs to-dirs)
  (declare (type list src-dirs from-dirs to-dirs)
	   (values list))
  (let ((result-dirs nil)
	(transl-dirs nil))
    ;; Cope with possible null directory lists.
    (cond ((and (null src-dirs) (null from-dirs))
	   (return-from translate-logical-directory to-dirs))
	  ((or (null src-dirs) (null from-dirs))
	   (throw 'next-translation nil)))
    ;; Compute the intermediate result by matching the source-dirs
    ;; components with the from-dirs and placing the result in the result-dirs
    ;; if the match is successful, otherwise throw to the next translation.
    (setf result-dirs
	  (translate-directory-lists (rest src-dirs) (rest from-dirs)
				     result-dirs)
	  transl-dirs
	  (translate-directory-lists result-dirs (rest to-dirs)
				     transl-dirs))
    (setf result-dirs (translate-absolute-relative
		       (first src-dirs) (first from-dirs) transl-dirs)) 
    (if result-dirs
	(push (translate-absolute-relative
	       (first src-dirs) (first from-dirs) (first to-dirs))
	      transl-dirs))
    transl-dirs))

;;; TRANSLATE-LOGICAL-COMP-ELEMENT -- Internal
;;;
(defun translate-logical-comp-element (from to)
  (declare (type (or pattern simple-base-string fixnum symbol null) from to))
  (let ((match-p nil)
	(matches nil))
    (typecase from
      (simple-base-string 
       (typecase to
	 (simple-base-string
	  (string-downcase to))
	 (fixnum
	  (throw 'next-translation nil))
	 ((member :newest :wild nil)
	  (string-downcase from))
	 (pattern
	  (multiple-value-setq (match-p matches)
	    (pattern-matches to from))
	  (if match-p
	      (substitute-into to matches :case :lower)
	      (throw 'next-translation nil)))))
      (fixnum
       (typecase to
	 (fixnum
	  (if (<= from to)
	      to
	      from))
	 ((member :newest :wild nil)
	  to)
	 (pattern
	  (case (first (pattern-pieces to))
	    ((or :wild :newest) to)
	    (t (throw 'next-translation nil))))))
      ((member :wild :newest nil)
       to)
      (pattern
       (typecase to
	 (simple-base-string
	  (multiple-value-setq (match-p matches)
	    (pattern-matches from to))
	  (if match-p
	      (substitute-into from matches :case :lower)
	      (throw 'next-translation nil)))
	 (fixnum
	  (case (first (pattern-pieces from))
	    ((or :wild :newest) to)
	    (t (throw 'next-translation nil))))
	 ((member :newest :wild nil)
	  to)
	 (pattern
	  (if (pattern= from to)
	      to
	      (throw 'next-translation nil))))))))

;;; TRANSLATE-LOGICAL-COMPONENT -- Internal
;;;
(defun translate-logical-component (src from to)
  (declare (type (or pattern simple-base-string fixnum symbol null) from to))
  (translate-logical-comp-element (translate-logical-comp-element src from) to))

;;; TRANSLATE-LOGICAL-PATHNAME  -- Public
;;;
(defun translate-logical-pathname (pathname &key)
  "Translates pathname to a physical pathname, which is returned."
  (declare (type path-designator pathname)
	   (values (or null pathname)))
  (with-pathname (source pathname) 
     (when (logical-pathname-p source)
       (let ((p-host (%pathname-host source))
	     (from nil)
	     (to nil)
	     (tr-host nil)
	     (tr-dir nil)
	     (tr-name nil)
	     (tr-type nil)
	     (tr-version nil)
	     (result-path nil)
	     (src-transl nil)
	     (i 0))
	 (declare (type fixnum i)
		  (type (or pathname null) result-path))
	 ;; Verify that the logical-host is defined.
	 (unless (gethash (funcall (logical-host-unparse-host p-host) source)
			  *logical-hosts*)
	   (error "The logical host ~S is not defined.~%"
		  (logical-host-name p-host)))
	 ;; Scan the pathname translations, and if none is found signal error.
	 (loop
	   (catch 'next-translation
	     (setf src-transl (nth i (logical-host-canon-transls p-host)))
	     (incf i)
	     (unless src-transl
	       (error "~S has no matching translation for logical host ~S.~%"
		      pathname (logical-host-name p-host)))
	     (setf from (first src-transl)
		   to (second src-transl))
	     (when (pathname-match-p pathname from) 
	       (setf tr-host (translate-logical-host
			      p-host
			      (%pathname-host from)
			      (%pathname-host to))
		     tr-dir (translate-logical-directory
			     (%pathname-directory source)
			     (%pathname-directory from)
			     (%pathname-directory to))
		     tr-name (translate-logical-component
			      (%pathname-name source)
			      (%pathname-name from)
			      (%pathname-name to))
		     tr-type (translate-logical-component
			      (%pathname-type source)
			      (%pathname-type from)
			      (%pathname-type to))
		     tr-version (translate-logical-component
				 (%pathname-version source)
				 (%pathname-version from)
				 (%pathname-version to))
		     result-path (%make-pathname tr-host
						 :unspecific
						 tr-dir
						 tr-name
						 tr-type
						 tr-version))
	       (etypecase result-path
		 (logical-pathname 
		  (translate-logical-pathname result-path))
		 (pathname 
		  (return-from translate-logical-pathname result-path))
		 (null
		  (error "The logical path ~S could not be translated."
			 pathname))))))))))



