;;; -*- Log: code.log; Package: Lisp -*-
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/filesys.lisp,v 1.41 1997/01/18 14:30:48 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; File system interface functions.  This file is pretty UNIX specific.
;;;
;;; Written by William Lott
;;;
;;; **********************************************************************

(in-package "LISP")

(export '(truename probe-file user-homedir-pathname directory
          rename-file delete-file file-write-date file-author))

(use-package "EXTENSIONS")

(in-package "EXTENSIONS")
(export '(print-directory complete-file ambiguous-files default-directory
			  file-writable unix-namestring))
(in-package "LISP")


;;;; Unix pathname host support.

;;; Unix namestrings have the following format:
;;;
;;; namestring := [ directory ] [ file [ type [ version ]]]
;;; directory := [ "/" | search-list ] { file "/" }*
;;; search-list := [^:/]*:
;;; file := [^/]*
;;; type := "." [^/.]*
;;; version := "." ([0-9]+ | "*")
;;;
;;; Note: this grammer is ambiguous.  The string foo.bar.5 can be parsed
;;; as either just the file specified or as specifying the file, type, and
;;; version.  Therefore, we use the following rules when confronted with
;;; an ambiguous file.type.version string:
;;;
;;; - If the first character is a dot, it's part of the file.  It is not
;;; considered a dot in the following rules.
;;;
;;; - If there is only one dot, it seperates the file and the type.
;;;
;;; - If there are multiple dots and the stuff following the last dot
;;; is a valid version, then that is the version and the stuff between
;;; the second to last dot and the last dot is the type.
;;;
;;; Wildcard characters:
;;;
;;; If the directory, file, type components contain any of the following
;;; characters, it is considered part of a wildcard pattern and has the
;;; following meaning.
;;;
;;; ? - matches any character
;;; * - matches any zero or more characters.
;;; [abc] - matches any of a, b, or c.
;;; {str1,str2,...,strn} - matches any of str1, str2, ..., or strn.
;;;
;;; Any of these special characters can be preceeded by a backslash to
;;; cause it to be treated as a regular character.
;;;

(defun remove-backslashes (namestr start end)
  "Remove and occurences of \\ from the string because we've already
   checked for whatever they may have been backslashed."
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let* ((result (make-string (- end start)))
	 (dst 0)
	 (quoted nil))
    (do ((src start (1+ src)))
	((= src end))
      (cond (quoted
	     (setf (schar result dst) (schar namestr src))
	     (setf quoted nil)
	     (incf dst))
	    (t
	     (let ((char (schar namestr src)))
	       (cond ((char= char #\\)
		      (setq quoted t))
		     (t
		      (setf (schar result dst) char)
		      (incf dst)))))))
    (when quoted
      (error 'namestring-parse-error
	     :complaint "Backslash in bad place."
	     :namestring namestr
	     :offset (1- end)))
    (shrink-vector result dst)))

(defvar *ignore-wildcards* nil)

(defun maybe-make-pattern (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (if *ignore-wildcards*
      (subseq namestr start end)
      (collect ((pattern))
	(let ((quoted nil)
	      (any-quotes nil)
	      (last-regular-char nil)
	      (index start))
	  (flet ((flush-pending-regulars ()
		   (when last-regular-char
		     (pattern (if any-quotes
				  (remove-backslashes namestr
						      last-regular-char
						      index)
				  (subseq namestr last-regular-char index)))
		     (setf any-quotes nil)
		     (setf last-regular-char nil))))
	    (loop
	      (when (>= index end)
		(return))
	      (let ((char (schar namestr index)))
		(cond (quoted
		       (incf index)
		       (setf quoted nil))
		      ((char= char #\\)
		       (setf quoted t)
		       (setf any-quotes t)
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index))
		      ((char= char #\?)
		       (flush-pending-regulars)
		       (pattern :single-char-wild)
		       (incf index))
		      ((char= char #\*)
		       (flush-pending-regulars)
		       (pattern :multi-char-wild)
		       (incf index))
		      ((char= char #\[)
		       (flush-pending-regulars)
		       (let ((close-bracket
			      (position #\] namestr :start index :end end)))
			 (unless close-bracket
			   (error 'namestring-parse-error
				  :complaint "``['' with no corresponding ``]''"
				  :namestring namestr
				  :offset index))
			 (pattern (list :character-set
					(subseq namestr
						(1+ index)
						close-bracket)))
			 (setf index (1+ close-bracket))))
		      (t
		       (unless last-regular-char
			 (setf last-regular-char index))
		       (incf index)))))
	    (flush-pending-regulars)))
	(cond ((null (pattern))
	       "")
	      ((null (cdr (pattern)))
	       (let ((piece (first (pattern))))
		 (typecase piece
		   ((member :multi-char-wild) :wild)
		   (simple-string piece)
		   (t
		    (make-pattern (pattern))))))
	      (t
	       (make-pattern (pattern)))))))

(defun extract-name-type-and-version (namestr start end)
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
	   (values (maybe-make-pattern namestr start second-to-last-dot)
		   (maybe-make-pattern namestr
				       (1+ second-to-last-dot)
				       last-dot)
		   version))
	  (last-dot
	   (values (maybe-make-pattern namestr start last-dot)
		   (maybe-make-pattern namestr (1+ last-dot) end)
		   version))
	  (t
	   (values (maybe-make-pattern namestr start end)
		   nil
		   version)))))

;;; Take a string and return a list of cons cells that mark the char
;;; separated subseq. The first value t if absolute directories location.
;;;
(defun split-at-slashes (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((absolute (and (/= start end)
		       (char= (schar namestr start) #\/))))
    (when absolute
      (incf start))
    ;; Next, split the remainder into slash seperated chunks.
    (collect ((pieces))
      (loop
	(let ((slash (position #\/ namestr :start start :end end)))
	  (pieces (cons start (or slash end)))
	  (unless slash
	    (return))
	  (setf start (1+ slash))))
      (values absolute (pieces)))))

(defun maybe-extract-search-list (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (let ((quoted nil))
    (do ((index start (1+ index)))
	((= index end)
	 (values nil start))
      (if quoted
	  (setf quoted nil)
	  (case (schar namestr index)
	    (#\\
	     (setf quoted t))
	    (#\:
	     (return (values (remove-backslashes namestr start index)
			     (1+ index)))))))))

(defun parse-unix-namestring (namestr start end)
  (declare (type simple-base-string namestr)
	   (type index start end))
  (multiple-value-bind
      (absolute pieces)
      (split-at-slashes namestr start end)
    (let ((search-list
	   (if absolute
	       nil
	       (let ((first (car pieces)))
		 (multiple-value-bind
		     (search-list new-start)
		     (maybe-extract-search-list namestr
						(car first) (cdr first))
		   (when search-list
		     (setf absolute t)
		     (setf (car first) new-start))
		   search-list)))))
      (multiple-value-bind
	  (name type version)
	  (let* ((tail (car (last pieces)))
		 (tail-start (car tail))
		 (tail-end (cdr tail)))
	    (unless (= tail-start tail-end)
	      (setf pieces (butlast pieces))
	      (extract-name-type-and-version namestr tail-start tail-end)))
	;; Now we have everything we want.  So return it.
	(values nil ; no host for unix namestrings.
		nil ; no devices for unix namestrings.
		(collect ((dirs))
		  (when search-list
		    (dirs (intern-search-list search-list)))
		  (dolist (piece pieces)
		    (let ((piece-start (car piece))
			  (piece-end (cdr piece)))
		      (unless (= piece-start piece-end)
			(cond ((string= namestr ".." :start1 piece-start
					:end1 piece-end)
			       (dirs :up))
			      ((string= namestr "**" :start1 piece-start
					:end1 piece-end)
			       (dirs :wild-inferiors))
			      (t
			       (dirs (maybe-make-pattern namestr
							 piece-start
							 piece-end)))))))
		  (cond (absolute
			 (cons :absolute (dirs)))
			((dirs)
			 (cons :relative (dirs)))
			(t
			 nil)))
		name
		type
		version)))))

(defun unparse-unix-host (pathname)
  (declare (type pathname pathname)
	   (ignore pathname))
  "Unix")

(defun unparse-unix-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    (simple-string
     (let* ((srclen (length thing))
	    (dstlen srclen))
       (dotimes (i srclen)
	 (case (schar thing i)
	   ((#\* #\? #\[)
	    (incf dstlen))))
       (let ((result (make-string dstlen))
	     (dst 0))
	 (dotimes (src srclen)
	   (let ((char (schar thing src)))
	     (case char
	       ((#\* #\? #\[)
		(setf (schar result dst) #\\)
		(incf dst)))
	     (setf (schar result dst) char)
	     (incf dst)))
	 result)))
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
	 (etypecase piece
	   (simple-string
	    (strings piece))
	   (symbol
	    (ecase piece
	      (:multi-char-wild
	       (strings "*"))
	      (:single-char-wild
	       (strings "?"))))
	   (cons
	    (case (car piece)
	      (:character-set
	       (strings "[")
	       (strings (cdr piece))
	       (strings "]"))
	      (t
	       (error "Invalid pattern piece: ~S" piece))))))
       (apply #'concatenate
	      'simple-string
	      (strings))))))

(defun unparse-unix-directory-list (directory)
  (declare (type list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
	(:absolute
	 (cond ((search-list-p (car directory))
		(pieces (search-list-name (pop directory)))
		(pieces ":"))
	       (t
		(pieces "/"))))
	(:relative
	 ;; Nothing special.
	 ))
      (dolist (dir directory)
	(typecase dir
	  ((member :up)
	   (pieces "../"))
	  ((member :back)
	   (error ":BACK cannot be represented in namestrings."))
	  ((member :wild-inferiors)
	   (pieces "**/"))
	  ((or simple-string pattern)
	   (pieces (unparse-unix-piece dir))
	   (pieces "/"))
	  (t
	   (error "Invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-unix-directory (pathname)
  (declare (type pathname pathname))
  (unparse-unix-directory-list (%pathname-directory pathname)))
  
(defun unparse-unix-file (pathname)
  (declare (type pathname pathname))
  (collect ((strings))
    (let* ((name (%pathname-name pathname))
	   (type (%pathname-type pathname))
	   (type-supplied (not (or (null type) (eq type :unspecific))))
	   (version (%pathname-version pathname))
	   (version-supplied (not (or (null version) (eq version :newest)))))
      (when name
	(strings (unparse-unix-piece name)))
      (when type-supplied
	(unless name
	  (error "Cannot specify the type without a file: ~S" pathname))
	(strings ".")
	(strings (unparse-unix-piece type)))
      (when version-supplied
	(unless type-supplied
	  (error "Cannot specify the version without a type: ~S" pathname))
	(strings (if (eq version :wild)
		     ".*"
		     (format nil ".~D" version)))))
    (apply #'concatenate 'simple-string (strings))))

(defun unparse-unix-namestring (pathname)
  (declare (type pathname pathname))
  (concatenate 'simple-string
	       (unparse-unix-directory pathname)
	       (unparse-unix-file pathname)))

(defun unparse-unix-enough (pathname defaults)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
	   (error "~S cannot be represented relative to ~S"
		  pathname defaults)))
    (collect ((strings))
      (let* ((pathname-directory (%pathname-directory pathname))
	     (defaults-directory (%pathname-directory defaults))
	     (prefix-len (length defaults-directory))
	     (result-dir
	      (cond ((and (> prefix-len 1)
			  (>= (length pathname-directory) prefix-len)
			  (compare-component (subseq pathname-directory
						     0 prefix-len)
					     defaults-directory))
		     ;; Pathname starts with a prefix of default.  So just
		     ;; use a relative directory from then on out.
		     (cons :relative (nthcdr prefix-len pathname-directory)))
		    ((eq (car pathname-directory) :absolute)
		     ;; We are an absolute pathname, so we can just use it.
		     pathname-directory)
		    (t
		     ;; We are a relative directory.  So we lose.
		     (lose)))))
	(strings (unparse-unix-directory-list result-dir)))
      (let* ((pathname-version (%pathname-version pathname))
	     (version-needed (and pathname-version
				  (not (eq pathname-version :newest))))
	     (pathname-type (%pathname-type pathname))
	     (type-needed (or version-needed
			      (and pathname-type
				   (not (eq pathname-type :unspecific)))))
	     (pathname-name (%pathname-name pathname))
	     (name-needed (or type-needed
			      (and pathname-name
				   (not (compare-component pathname-name
							   (%pathname-name
							    defaults)))))))
	(when name-needed
	  (unless pathname-name (lose))
	  (strings (unparse-unix-piece pathname-name)))
	(when type-needed
	  (when (or (null pathname-type) (eq pathname-type :unspecific))
	    (lose))
	  (strings ".")
	  (strings (unparse-unix-piece pathname-type)))
	(when version-needed
	  (typecase pathname-version
	    ((member :wild)
	     (strings ".*"))
	    (integer
	     (strings (format nil ".~D" pathname-version)))
	    (t
	     (lose)))))
      (apply #'concatenate 'simple-string (strings)))))


(defstruct (unix-host
	    (:include host
		      (:parse #'parse-unix-namestring)
		      (:unparse #'unparse-unix-namestring)
		      (:unparse-host #'unparse-unix-host)
		      (:unparse-directory #'unparse-unix-directory)
		      (:unparse-file #'unparse-unix-file)
		      (:unparse-enough #'unparse-unix-enough)
		      (:customary-case :lower))
	    (:make-load-form-fun make-unix-host-load-form))
  )

(defvar *unix-host* (make-unix-host))

(defun make-unix-host-load-form (host)
  (declare (ignore host))
  '*unix-host*)


;;;; Wildcard matching stuff.

(defmacro enumerate-matches ((var pathname &optional result
				  &key (verify-existance t))
			     &body body)
  (let ((body-name (gensym)))
    `(block nil
       (flet ((,body-name (,var)
		,@body))
	 (%enumerate-matches (pathname ,pathname)
			     ,verify-existance
			     #',body-name)
	 ,result))))

(defun %enumerate-matches (pathname verify-existance function)
  (when (pathname-type pathname)
    (unless (pathname-name pathname)
      (error "Cannot supply a type without a name:~%  ~S" pathname)))
  (when (and (integerp (pathname-version pathname))
	     (member (pathname-type pathname) '(nil :unspecific)))
    (error "Cannot supply a version without a type:~%  ~S" pathname))
  (let ((directory (pathname-directory pathname)))
    (if directory
	(ecase (car directory)
	  (:absolute
	   (%enumerate-directories "/" (cdr directory) pathname
				   verify-existance function))
	  (:relative
	   (%enumerate-directories "" (cdr directory) pathname
				   verify-existance function)))
	(%enumerate-files "" pathname verify-existance function))))

(defun %enumerate-directories (head tail pathname verify-existance function)
  (declare (simple-string head))
  (if tail
      (let ((piece (car tail)))
	(etypecase piece
	  (simple-string
	   (%enumerate-directories (concatenate 'string head piece "/")
				   (cdr tail) pathname verify-existance
				   function))
	  ((or pattern (member :wild :wild-inferiors))
	   (let ((dir (unix:open-dir head)))
	     (when dir
	       (unwind-protect
		   (loop
		     (let ((name (unix:read-dir dir)))
		       (cond ((null name)
			      (return))
			     ((string= name "."))
			     ((string= name ".."))
			     ((pattern-matches piece name)
			      (let ((subdir (concatenate 'string
							 head name "/")))
				(when (eq (unix:unix-file-kind subdir)
					  :directory)
				  (%enumerate-directories
				   subdir (cdr tail) pathname verify-existance
				   function)))))))
		 (unix:close-dir dir)))))
	  ((member :up)
	   (%enumerate-directories (concatenate 'string head "../")
				   (cdr tail) pathname verify-existance
				   function))))
      (%enumerate-files head pathname verify-existance function)))

(defun %enumerate-files (directory pathname verify-existance function)
  (declare (simple-string directory))
  (let ((name (%pathname-name pathname))
	(type (%pathname-type pathname))
	(version (%pathname-version pathname)))
    (cond ((member name '(nil :unspecific))
	   (when (or (not verify-existance)
		     (unix:unix-file-kind directory))
	     (funcall function directory)))
	  ((or (pattern-p name)
	       (pattern-p type)
	       (eq name :wild)
	       (eq type :wild))
	   (let ((dir (unix:open-dir directory)))
	     (when dir
	       (unwind-protect
		   (loop
		     (let ((file (unix:read-dir dir)))
		       (if file
			   (unless (or (string= file ".")
				       (string= file ".."))
			     (multiple-value-bind
				 (file-name file-type file-version)
				 (let ((*ignore-wildcards* t))
				   (extract-name-type-and-version
				    file 0 (length file)))
			       (when (and (components-match file-name name)
					  (components-match file-type type)
					  (components-match file-version
							    version))
				 (funcall function
					  (concatenate 'string
						       directory
						       file)))))
			   (return))))
		 (unix:close-dir dir)))))
	  (t
	   (let ((file (concatenate 'string directory name)))
	     (unless (or (null type) (eq type :unspecific))
	       (setf file (concatenate 'string file "." type)))
	     (unless (member version '(nil :newest :wild))
	       (setf file (concatenate 'string file "."
				       (quick-integer-to-string version))))
	     (when (or (not verify-existance)
		       (unix:unix-file-kind file t))
	       (funcall function file)))))))

(defun quick-integer-to-string (n)
  (declare (type integer n))
  (cond ((not (fixnump n))
	 (write-to-string n :base 10 :radix nil))
	((zerop n) "0")
	((eql n 1) "1")
	((minusp n)
	 (concatenate 'simple-string "-"
		      (the simple-string (quick-integer-to-string (- n)))))
	(t
	 (do* ((len (1+ (truncate (integer-length n) 3)))
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q n)
	       (r 0))
	      ((zerop q)
	       (incf i)
	       (replace res res :start2 i :end2 len)
	       (shrink-vector res (- len i)))
	   (declare (simple-string res)
		    (fixnum len i r q))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))


;;;; UNIX-NAMESTRING -- public
;;; 
(defun unix-namestring (pathname &optional (for-input t) executable-only)
  "Convert PATHNAME into a string that can be used with UNIX system calls.
   Search-lists and wild-cards are expanded."
  ;; toy@rtp.ericsson.se: Let unix-namestring also handle logical
  ;; pathnames too.
  (let ((path (let ((lpn (pathname pathname)))
		(if (logical-pathname-p lpn)
		    (namestring (translate-logical-pathname lpn))
		    pathname))))
  (enumerate-search-list
      (pathname path)
    (collect ((names))
      (enumerate-matches (name pathname nil :verify-existance for-input)
	(when (or (not executable-only)
		  (and (eq (unix:unix-file-kind name) :file)
		       (unix:unix-access name unix:x_ok)))
	  (names name)))
      (let ((names (names)))
	(when names
	  (when (cdr names)
	    (error "~S is ambiguous:~{~%  ~A~}" pathname names))
	  (return (car names))))))))


;;;; TRUENAME and PROBE-FILE.

;;; Truename  --  Public
;;;
;;; Another silly file function trivially different from another function.
;;;
(defun truename (pathname)
  "Return the pathname for the actual file described by the pathname
  An error is signalled if no such file exists."
  (let ((result (probe-file pathname)))
    (unless result
      (error "The file ~S does not exist." (namestring pathname)))
    result))

;;; Probe-File  --  Public
;;;
;;; If PATHNAME exists, return it's truename, otherwise NIL.
;;;
(defun probe-file (pathname)
  "Return a pathname which is the truename of the file if it exists, NIL
  otherwise."
  (let ((namestring (unix-namestring pathname t)))
    (when (and namestring (unix:unix-file-kind namestring))
      (let ((truename (unix:unix-resolve-links
		       (unix:unix-maybe-prepend-current-directory
			namestring))))
	(when truename
	  (let ((*ignore-wildcards* t))
	    (pathname (unix:unix-simplify-pathname truename))))))))


;;;; Other random operations.

;;; Rename-File  --  Public
;;;
(defun rename-file (file new-name)
  "Rename File to have the specified New-Name.  If file is a stream open to a
  file, then the associated file is renamed."
  (let* ((original (truename file))
	 (original-namestring (unix-namestring original t))
	 (new-name (merge-pathnames new-name original))
	 (new-namestring (unix-namestring new-name nil)))
    (unless new-namestring
      (error "~S can't be created." new-name))
    (multiple-value-bind (res error)
			 (unix:unix-rename original-namestring
					   new-namestring)
      (unless res
	(error "Failed to rename ~A to ~A: ~A"
	       original new-name (unix:get-unix-error-msg error)))
      (when (streamp file)
	(file-name file new-namestring))
      (values new-name original (truename new-name)))))

;;; Delete-File  --  Public
;;;
;;;    Delete the file, Man.
;;;
(defun delete-file (file)
  "Delete the specified file."
  (let ((namestring (unix-namestring file t)))
    (when (streamp file)
      (close file :abort t))
    (unless namestring
      (error "~S doesn't exist." file))

    (multiple-value-bind (res err) (unix:unix-unlink namestring)
      (unless res
	(error "Could not delete ~A: ~A."
	       namestring
	       (unix:get-unix-error-msg err)))))
  t)


;;; User-Homedir-Pathname  --  Public
;;;
;;;    Return Home:, which is set up for us at initialization time.
;;;
(defun user-homedir-pathname (&optional host)
  "Returns the home directory of the logged in user as a pathname.
  This is obtained from the logical name \"home:\"."
  (declare (ignore host))
  #p"home:")

;;; File-Write-Date  --  Public
;;;
(defun file-write-date (file)
  "Return file's creation date, or NIL if it doesn't exist."
  (let ((name (unix-namestring file t)))
    (when name
      (multiple-value-bind
	  (res dev ino mode nlink uid gid rdev size atime mtime)
	  (unix:unix-stat name)
	(declare (ignore dev ino mode nlink uid gid rdev size atime))
	(when res
	  (+ unix-to-universal-time mtime))))))

;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Returns the file author as a string, or nil if the author cannot be
   determined.  Signals an error if file doesn't exist."
  (let ((name (unix-namestring (pathname file) t)))
    (unless name
      (error "~S doesn't exist." file))
    (multiple-value-bind (winp dev ino mode nlink uid)
			 (unix:unix-stat name)
      (declare (ignore dev ino mode nlink))
      (if winp (lookup-login-name uid)))))



;;;; DIRECTORY.

;;; DIRECTORY  --  public.
;;; 
(defun directory (pathname &key (all t) (check-for-subdirs t)
			   (follow-links t))
  "Returns a list of pathnames, one for each file that matches the given
   pathname.  Supplying :ALL as nil causes this to ignore Unix dot files.  This
   never includes Unix dot and dot-dot in the result.  If :FOLLOW-LINKS is NIL,
   then symblolic links in the result are not expanded.  This is not the
   default because TRUENAME does follow links, and the result pathnames are
   defined to be the TRUENAME of the pathname (the truename of a link may well
   be in another directory.)"
  (let ((results nil))
    (enumerate-search-list
	(pathname (merge-pathnames pathname
				   (make-pathname :name :wild
						  :type :wild
						  :version :wild)))
      (enumerate-matches (name pathname)
	(when (or all
		  (let ((slash (position #\/ name :from-end t)))
		    (or (null slash)
			(= (1+ slash) (length name))
			(char/= (schar name (1+ slash)) #\.))))
	  (push name results))))
    (let ((*ignore-wildcards* t))
      (mapcar #'(lambda (name)
		  (let ((name (if (and check-for-subdirs
				       (eq (unix:unix-file-kind name)
					   :directory))
				  (concatenate 'string name "/")
				  name)))
		    (if follow-links (truename name) (pathname name))))
	      (sort (delete-duplicates results :test #'string=) #'string<)))))


;;;; Printing directories.

;;; PRINT-DIRECTORY is exported from the EXTENSIONS package.
;;; 
(defun print-directory (pathname &optional stream &key all verbose return-list)
  "Like Directory, but prints a terse, multi-coloumn directory listing
   instead of returning a list of pathnames.  When :all is supplied and
   non-nil, then Unix dot files are included too (as ls -a).  When :vervose
   is supplied and non-nil, then a long listing of miscellaneous
   information is output one file per line."
  (let ((*standard-output* (out-synonym-of stream))
	(pathname pathname))
    (if verbose
	(print-directory-verbose pathname all return-list)
	(print-directory-formatted pathname all return-list))))

(defun print-directory-verbose (pathname all return-list)
  (let ((contents (directory pathname :all all :check-for-subdirs nil
			     :follow-links nil))
	(result nil))
    (format t "Directory of ~A :~%" (namestring pathname))
    (dolist (file contents)
      (let* ((namestring (unix-namestring file))
	     (tail (subseq namestring
			   (1+ (or (position #\/ namestring
					     :from-end t
					     :test #'char=)
				   -1)))))
	(multiple-value-bind 
	    (reslt dev-or-err ino mode nlink uid gid rdev size atime mtime)
	    (unix:unix-stat namestring)
	  (declare (ignore ino gid rdev atime)
		   (fixnum uid mode))
	  (cond (reslt
		 ;;
		 ;; Print characters for file modes.
		 (macrolet ((frob (bit name &optional sbit sname negate)
			      `(if ,(if negate
					`(not (logbitp ,bit mode))
					`(logbitp ,bit mode))
				   ,(if sbit
					`(if (logbitp ,sbit mode)
					     (write-char ,sname)
					     (write-char ,name))
					`(write-char ,name))
				   (write-char #\-))))
		   (frob 15 #\d nil nil t)
		   (frob 8 #\r)
		   (frob 7 #\w)
		   (frob 6 #\x 11 #\s)
		   (frob 5 #\r)
		   (frob 4 #\w)
		   (frob 3 #\x 10 #\s)
		   (frob 2 #\r)
		   (frob 1 #\w)
		   (frob 0 #\x))
		 ;;
		 ;; Print the rest.
		 (multiple-value-bind (sec min hour date month year)
				      (get-decoded-time)
		   (declare (ignore sec min hour date month))
		   (format t "~2D ~8A ~8D ~12A ~A~@[/~]~%"
			   nlink
			   (or (lookup-login-name uid) uid)
			   size
			   (decode-universal-time-for-files mtime year)
			   tail
			   (= (logand mode unix:s-ifmt) unix:s-ifdir))))
		(t (format t "Couldn't stat ~A -- ~A.~%"
			   tail
			   (unix:get-unix-error-msg dev-or-err))))
	  (when return-list
	    (push (if (= (logand mode unix:s-ifmt) unix:s-ifdir)
		      (pathname (concatenate 'string namestring "/"))
		      file)
		  result)))))
    (nreverse result)))

(defun decode-universal-time-for-files (time current-year)
  (multiple-value-bind (sec min hour day month year)
		       (decode-universal-time (+ time unix-to-universal-time))
    (declare (ignore sec))
    (format nil "~A ~2,' D ~:[ ~D~;~*~2,'0D:~2,'0D~]"
	    (svref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
		      "Sep" "Oct" "Nov" "Dec")
		   (1- month))
	    day (= current-year year) year hour min)))

(defun print-directory-formatted (pathname all return-list)
  (let ((width (or (line-length *standard-output*) 80))
	(names ())
	(cnt 0)
	(max-len 0)
	(result (directory pathname :all all :follow-links nil)))
    (declare (list names) (fixnum max-len cnt))
    ;;
    ;; Get the data.
    (dolist (file result)
      (let* ((name (unix-namestring file))
	     (length (length name))
	     (end (if (and (plusp length)
			   (char= (schar name (1- length)) #\/))
		      (1- length)
		      length))
	     (slash-name (subseq name
				 (1+ (or (position #\/ name
						   :from-end t
						   :end end
						   :test #'char=)
					 -1))))
	     (len (length slash-name)))
	(declare (simple-string slash-name)
		 (fixnum len))
	(if (> len max-len) (setq max-len len))
	(incf cnt)
	(push slash-name names)))
    (setq names (nreverse names))
    ;;
    ;; Do the output.
    (let* ((col-width (1+ max-len))
	   (cols (max (truncate width col-width) 1))
	   (lines (ceiling cnt cols)))
      (declare (fixnum cols lines))
      (format t "Directory of ~A :~%" (namestring pathname))
      (dotimes (i lines)
	(declare (fixnum i))
	(dotimes (j cols)
	  (declare (fixnum j))
	  (let ((name (nth (+ i (the fixnum (* j lines))) names)))
	    (when name
	      (write-string name)
	      (unless (eql j (1- cols))
		(dotimes (i (- col-width (length (the simple-string name))))
		  (write-char #\space))))))
	(terpri)))
    (when return-list
      result)))



;;;; Translating uid's and gid's.

(defvar *uid-hash-table* (make-hash-table)
  "Hash table for keeping track of uid's and login names.")

;;; LOOKUP-LOGIN-NAME translates a user id into a login name.  Previous
;;; lookups are cached in a hash table since groveling the passwd(s) files
;;; is somewhat expensive.  The table may hold nil for id's that cannot
;;; be looked up since this means the files are searched in their entirety
;;; each time this id is translated.
;;; 
(defun lookup-login-name (uid)
  (multiple-value-bind (login-name foundp) (gethash uid *uid-hash-table*)
    (if foundp
	login-name
	(setf (gethash uid *uid-hash-table*)
	      (get-group-or-user-name :user uid)))))

(defvar *gid-hash-table* (make-hash-table)
  "Hash table for keeping track of gid's and group names.")

;;; LOOKUP-GROUP-NAME translates a group id into a group name.  Previous
;;; lookups are cached in a hash table since groveling the group(s) files
;;; is somewhat expensive.  The table may hold nil for id's that cannot
;;; be looked up since this means the files are searched in their entirety
;;; each time this id is translated.
;;; 
(defun lookup-group-name (gid)
  (multiple-value-bind (group-name foundp) (gethash gid *gid-hash-table*)
    (if foundp
	group-name
	(setf (gethash gid *gid-hash-table*)
	      (get-group-or-user-name :group gid)))))


;;; GET-GROUP-OR-USER-NAME first tries "/etc/passwd" ("/etc/group") since it is
;;; a much smaller file, contains all the local id's, and most uses probably
;;; involve id's on machines one would login into.  Then if necessary, we look
;;; in "/etc/passwds" ("/etc/groups") which is really long and has to be
;;; fetched over the net.
;;;
(defun get-group-or-user-name (group-or-user id)
  "Returns the simple-string user or group name of the user whose uid or gid
   is id, or NIL if no such user or group exists.  Group-or-user is either
   :group or :user."
  (let ((id-string (let ((*print-base* 10)) (prin1-to-string id))))
    (declare (simple-string id-string))
    (multiple-value-bind (file1 file2)
			 (ecase group-or-user
			   (:group (values "/etc/group" "/etc/groups"))
			   (:user (values "/etc/passwd" "/etc/passwd")))
      (or (get-group-or-user-name-aux id-string file1)
	  (get-group-or-user-name-aux id-string file2)))))

(defun get-group-or-user-name-aux (id-string passwd-file)
  (with-open-file (stream passwd-file)
    (loop
      (let ((entry (read-line stream nil)))
	(unless entry (return nil))
	(let ((name-end (position #\: (the simple-string entry)
				  :test #'char=)))
	  (when name-end
	    (let ((id-start (position #\: (the simple-string entry)
				      :start (1+ name-end) :test #'char=)))
	      (when id-start
		(incf id-start)
		(let ((id-end (position #\: (the simple-string entry)
					:start id-start :test #'char=)))
		  (when (and id-end
			     (string= id-string entry
				      :start2 id-start :end2 id-end))
		    (return (subseq entry 0 name-end))))))))))))


;;;; File completion.

;;; COMPLETE-FILE -- Public
;;;
(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
			       ignore-types)
  (let ((files (directory (complete-file-directory-arg pathname defaults)
			  :check-for-subdirs nil
			  :follow-links nil)))
    (cond ((null files)
	   (values nil nil))
	  ((null (cdr files))
	   (values (merge-pathnames (file-namestring (car files))
				    pathname)
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (simple-string-p
				       (pathname-type pathname))
				      (member (pathname-type pathname)
					      ignore-types
					      :test #'string=)))
			     files)))
	     (cond ((null good-files))
		   ((null (cdr good-files))
		    (return-from complete-file
				 (values (merge-pathnames (file-namestring
							   (car good-files))
							  pathname)
					 t)))
		   (t
		    (setf files good-files)))
	     (let ((common (file-namestring (car files))))
	       (dolist (file (cdr files))
		 (let ((name (file-namestring file)))
		   (dotimes (i (min (length common) (length name))
			       (when (< (length name) (length common))
				 (setf common name)))
		     (unless (char= (schar common i) (schar name i))
		       (setf common (subseq common 0 i))
		       (return)))))
	       (values (merge-pathnames common pathname)
		       nil)))))))

;;; COMPLETE-FILE-DIRECTORY-ARG -- Internal.
;;;
(defun complete-file-directory-arg (pathname defaults)
  (let* ((pathname (merge-pathnames pathname (directory-namestring defaults)))
	 (type (pathname-type pathname)))
    (flet ((append-multi-char-wild (thing)
	     (etypecase thing
	       (null :wild)
	       (pattern
		(make-pattern (append (pattern-pieces thing)
				      (list :multi-char-wild))))
	       (simple-string
		(make-pattern (list thing :multi-char-wild))))))
      (if (or (null type) (eq type :unspecific))
	  ;; There is no type.
	  (make-pathname :defaults pathname
	    :name (append-multi-char-wild (pathname-name pathname))
	    :type :wild)
	  ;; There already is a type, so just extend it.
	  (make-pathname :defaults pathname
	    :name (pathname-name pathname)
	    :type (append-multi-char-wild (pathname-type pathname)))))))

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname
			&optional (defaults *default-pathname-defaults*))
  "Return a list of all files which are possible completions of Pathname.
   We look in the directory specified by Defaults as well as looking down
   the search list."
  (directory (complete-file-directory-arg pathname defaults)
	     :follow-links nil
	     :check-for-subdirs nil))



;;; File-writable -- exported from extensions.
;;;
;;;   Determines whether the single argument (which should be a pathname)
;;;   can be written by the the current task.
;;;
(defun file-writable (name)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise."
  (let ((name (unix-namestring name nil)))
    (cond ((null name)
	   nil)
	  ((unix:unix-file-kind name)
	   (values (unix:unix-access name unix:w_ok)))
	  (t
	   (values
	    (unix:unix-access (subseq name
				      0
				      (or (position #\/ name :from-end t)
					  0))
			      (logior unix:w_ok unix:x_ok)))))))


;;; Pathname-Order  --  Internal
;;;
;;;    Predicate to order pathnames by.  Goes by name.
;;;
(defun pathname-order (x y)
  (let ((xn (%pathname-name x))
	(yn (%pathname-name y)))
    (if (and xn yn)
	(let ((res (string-lessp xn yn)))
	  (cond ((not res) nil)
		((= res (length (the simple-string xn))) t)
		((= res (length (the simple-string yn))) nil)
		(t t)))
	xn)))


;;; Default-Directory  --  Public
;;;
(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (multiple-value-bind (gr dir-or-error)
		       (unix:unix-current-directory)
    (if gr
	(let ((*ignore-wildcards* t))
	  (pathname (concatenate 'simple-string dir-or-error "/")))
	(error dir-or-error))))

;;; %Set-Default-Directory  --  Internal
;;; 
(defun %set-default-directory (new-val)
  (let ((namestring (unix-namestring new-val t)))
    (unless namestring
      (error "~S doesn't exist." new-val))
    (multiple-value-bind (gr error)
			 (unix:unix-chdir namestring)
      (if gr
	  (setf (search-list "default:") (default-directory))
	  (error (unix:get-unix-error-msg error))))
    new-val))
;;;
(defsetf default-directory %set-default-directory)

(defun filesys-init ()
  (setf *default-pathname-defaults*
	(%make-pathname *unix-host* nil nil nil nil :newest))
  (setf (search-list "default:") (default-directory))
  nil)
