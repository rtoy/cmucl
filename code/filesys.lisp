;;; -*- Log: code.log; Package: Lisp -*-
;;; ### Some day fix to accept :wild in any pathname component.
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/filesys.lisp,v 1.8 1991/02/08 13:32:39 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/filesys.lisp,v 1.8 1991/02/08 13:32:39 ram Exp $
;;;
;;; Ugly pathname functions for Spice Lisp.
;;;    these functions are part of the standard Spice Lisp environment.
;;;
;;; Written by Jim Large and Rob MacLachlan
;;;
;;; **********************************************************************

(in-package "LISP")

(export '(pathname pathnamep *default-pathname-defaults* truename
	  parse-namestring merge-pathnames make-pathname
	  pathname-host pathname-device pathname-directory
	  pathname-name pathname-type pathname-version
	  namestring file-namestring directory-namestring
	  host-namestring enough-namestring user-homedir-pathname
          probe-file rename-file delete-file file-write-date
	  file-author directory))

(use-package "EXTENSIONS")

(in-package "EXTENSIONS")
(export '(print-directory complete-file ambiguous-files default-directory
			  file-writable unix-namestring))
(in-package "LISP")



;;; Pathname structure


;;; *Default-Pathname-defaults* has all values unspecified except for the
;;;  host.  All pathnames must have a host.
(defvar *default-pathname-defaults* ()
  "Set to the default pathname-defaults pathname (Got that?)")

(defun filesys-init ()
  (setq *default-pathname-defaults*
	(%make-pathname "Mach" nil nil nil nil nil))
  (multiple-value-bind (won dir)
		       (mach:unix-current-directory)
    (when won
      (setf (search-list "default:") (list dir)))))


;;; The pathname type is defined with a defstruct.
;;; This declaration implicitly defines the common lisp function Pathnamep
(defstruct (pathname
	    (:conc-name %pathname-)
	    (:print-function %print-pathname)
	    (:constructor
	     %make-pathname (host device directory name type version))
	    (:predicate pathnamep))
  "Pathname is the structure of the file pathname.  It consists of a
   host, a device, a directory, a name, and a type."
  (host nil :type (or simple-string null))
  (device nil :type (or simple-string (member nil :absolute)))
  (directory nil :type (or simple-vector null))
  (name nil :type (or simple-string null))
  (type nil :type (or simple-string null))
  (version nil :type (or integer (member nil :newest))))

(defun %print-pathname (s stream d)
  (declare (ignore d))
  (format stream "#.(pathname ~S)" (namestring s)))

(defun make-pathname (&key defaults (host nil hostp) (device nil devicep)
			   (directory nil directoryp) (name nil namep)
			   (type nil typep) (version nil versionp))
  "Create a pathname from :host, :device, :directory, :name, :type and :version.
  If any field is ommitted, it is obtained from :defaults as though by 
  merge-pathnames."
  (if defaults
      (let ((defaults (pathname defaults)))
	(unless hostp
	  (setq host (%pathname-host defaults)))
	(unless devicep
	  (setq device (%pathname-device defaults)))
	(unless directoryp
	  (setq directory (%pathname-directory defaults)))
	(unless namep
	  (setq name (%pathname-name defaults)))
	(unless typep
	  (setq type (%pathname-type defaults)))
	(unless versionp
	  (setq version (%pathname-version defaults))))
      (unless hostp
	(setq host (%pathname-host *default-pathname-defaults*))))

  (when (stringp directory)
    (setq directory (%pathname-directory (parse-namestring directory))))
  (%make-pathname
   (if (stringp host) (coerce host 'simple-string) host)
   (if (stringp device) (coerce device 'simple-string) device)
   directory
   (if (stringp name) (coerce name 'simple-string) name)
   (if (stringp type) (coerce type 'simple-string) type)
   version))


;;; These can not be done by the accessors because the pathname arg may be
;;;  a string or a symbol or etc.

(defun pathname-host (pathname)
  "Returns the host slot of pathname.  Pathname may be a string, symbol,
  or stream."
  (%pathname-host (if (pathnamep pathname) pathname (pathname pathname))))

(defun pathname-device (pathname)
  "Returns the device slot of pathname.  Pathname may be a string, symbol,
  or stream."
  (%pathname-device (if (pathnamep pathname) pathname (pathname pathname))))

(defun pathname-directory (pathname)
  "Returns the directory slot of pathname.  Pathname may be a string,
  symbol, or stream."
  (%pathname-directory (if (pathnamep pathname) pathname (pathname pathname))))

(defun pathname-name (pathname)
  "Returns the name slot of pathname.  Pathname may be a string,
  symbol, or stream."
  (%pathname-name (if (pathnamep pathname) pathname (pathname pathname))))

(defun pathname-type (pathname)
  "Returns the type slot of pathname.  Pathname may be a string,
  symbol, or stream."
  (%pathname-type (if (pathnamep pathname) pathname (pathname pathname))))

(defun pathname-version (pathname)
  "Returns the version slot of pathname.  Pathname may be a string,
  symbol, or stream."
  (%pathname-version (if (pathnamep pathname) pathname (pathname pathname))))



;;;; PARSE-NAMESTRING and PATHNAME.

;;; SPLIT-FILENAME -- internal
;;;
;;; Splits the filename into the name and type.  If someone wants to change
;;; this yet again, just change this.
;;; 
(defun split-filename (filename)
  (declare (simple-string filename))
  (let ((posn (position #\. filename :from-end t)))
    (cond ((null posn)
	   (values filename nil))
	  ((or (zerop posn) (= posn (1- (length filename))))
	   (values filename ""))
	  (t
	   (values (subseq filename 0 posn)
		   (subseq filename (1+ posn)))))))

;;; DO-FILENAME-PARSE -- internal
;;;
;;; Split string into a logical name, a vector of directories, a file name and
;;; a file type.
;;;
(defun do-filename-parse (string &optional (start 0) end)
  (declare (simple-string string))
  (let ((end (or end (length string))))
    (let* ((directories nil)
	   (filename nil)
	   (absolutep (and (> end start) (eql (schar string start) #\/)))
	   (logical-name
	    (cond (absolutep
		   (setf start (position #\/ string :start start :end end
					 :test-not #'char=))
		   :absolute)
		  ((find #\: string
			 :start start
			 :end (or (position #\/ string :start start :end end)
				  end))
		   (let ((posn (position #\: string :start start)))
		     (prog1
			 (subseq string start posn)
		       (setf start (1+ posn))))))))
      (loop
	(unless (and start (> end start))
	  (return))
	(let ((next-slash (position #\/ string :start start :end end)))
	  (cond (next-slash
		 (push (subseq string start next-slash) directories)
		 (setf start
		       (position #\/ string :start next-slash :end end
				 :test-not #'char=)))
		(t
		 (setf filename (subseq string start end))
		 (return)))))
      (multiple-value-bind (name type)
			   (if filename (split-filename filename))
	(values (cond (logical-name logical-name)
		      (directories "Default"))
		(if (or logical-name directories)
		  (coerce (nreverse directories) 'vector))
		name
		type)))))

(defun parse-namestring (thing &optional host
			 (defaults *default-pathname-defaults*)
			 &key (start 0) end junk-allowed)
  "Convert THING (string, symbol, pathname, or stream) into a pathname."
  (declare (ignore junk-allowed))
  (let* ((host (or host (pathname-host defaults)))
	 (pathname
	  (etypecase thing
	    ((or string symbol)
	     (let ((string (coerce (string thing) 'simple-string)))
	       (multiple-value-bind (device directories name type)
				    (do-filename-parse string start end)
		 (unless end (setf end (length string)))
		 (make-pathname :host host
				:device device
				:directory directories
				:name name
				:type type))))
	    (pathname
	     (setf end start)
	     thing)
	    (stream
	     (setf end start)
	     (pathname (file-name thing))))))
    (unless (or (null host)
		(null (pathname-host pathname))
		(string-equal host (pathname-host pathname)))
      (cerror "Ignore it."
	      "Host mismatch in ~S: ~S isn't ~S"
	      'parse-namestring
	      (pathname-host pathname)
	      host))
    (values pathname end)))


(defun pathname (thing)
  "Turns thing into a pathname.  Thing may be a string, symbol, stream, or
   pathname."
  (values (parse-namestring thing)))



;;; Merge-Pathnames  --  Public
;;;
;;; Returns a new pathname whose fields are the same as the fields in PATHNAME
;;;  except that () fields are filled in from defaults.  Type and Version field
;;;  are only done if name field has to be done (see manual for explanation).
;;;
(defun merge-pathnames (pathname &optional
				 (defaults *default-pathname-defaults*)
				 default-version)
  "Fills in unspecified slots of Pathname from Defaults (defaults to
  *default-pathname-defaults*).  If the version remains unspecified,
  gets it from Default-Version."
  ;;
  ;; finish hairy argument defaulting
  (setq pathname (pathname pathname))
  (setq defaults (pathname defaults))
  ;;
  ;; make a new pathname
  (let ((name (%pathname-name pathname))
	(device (%pathname-device pathname)))
    (%make-pathname
     (or (%pathname-host pathname) (%pathname-host defaults))
     (or device (%pathname-device defaults))
     (or (%pathname-directory pathname) (%pathname-directory defaults))
     (or name (%pathname-name defaults))
     (or (%pathname-type pathname) (%pathname-type defaults))
     (or (%pathname-version pathname)
	 (if name
	     default-version
	     (or (%pathname-version defaults) default-version))))))


;;;; NAMESTRING and other stringification stuff.

;;; %Dirstring  --  Internal
;;;
;;; %Dirstring converts a vector of the form #("foo" "bar" ... "baz") into a
;;;  string of the form "foo/bar/ ... /baz/"

(defun %dirstring (dirlist)
  (declare (simple-vector dirlist))
  (let* ((numdirs (length dirlist))
	 (length numdirs))
    (declare (fixnum numdirs length))
    (dotimes (i numdirs)
      (incf length (the fixnum (length (svref dirlist i)))))
    (do ((result (make-string length))
	 (index 0 (1+ index))
	 (position 0))
	((= index numdirs) result)
      (declare (simple-string result))
      (let* ((string (svref dirlist index))
	     (len (length string))
	     (end (+ position len)))
	(declare (simple-string string)
		 (fixnum len end))
	(replace result string :start1 position  :end1 end  :end2 len)
	(setf (schar result end) #\/)
	(setq position (+ end 1))))))

(defun quick-integer-to-string (n)
  (cond ((zerop n) "0")
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
		    (fixnum len i r))
	   (multiple-value-setq (q r) (truncate q 10))
	   (setf (schar res i) (schar "0123456789" r))))))
	   
(defun %device-string (device)
  (cond ((eq device :absolute) "/")
	(device
	 (if (string-equal device "Default")
	     ""
	     (concatenate 'simple-string (the simple-string device) ":")))
	(T "")))

(defun namestring (pathname)
  "Returns the full form of PATHNAME as a string."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (directory (%pathname-directory pathname))
	 (name (%pathname-name pathname))
	 (type (%pathname-type pathname))
	 (result (%device-string (%pathname-device pathname))))
    (declare (simple-string result))
    (when directory
      (setq result (concatenate 'simple-string result
				(the simple-string (%dirstring directory)))))
    (when name
      (setq result (concatenate 'simple-string result 
				(the simple-string name))))
    (when (and type (not (zerop (length type))))
      (setq result (concatenate 'simple-string result "."
				(the simple-string type))))
    result))

(defun namestring-without-device (pathname)
  "NAMESTRING of pathname ignoring the device slot."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (directory (%pathname-directory pathname))
	 (name (%pathname-name pathname))
	 (type (%pathname-type pathname))
	 (result ""))
    (declare (simple-string result))
    (when directory
      (setq result (concatenate 'simple-string result
				(the simple-string (%dirstring directory)))))
    (when name
      (setq result (concatenate 'simple-string result 
				(the simple-string name))))
    (when (and type (not (zerop (length type))))
      (setq result (concatenate 'simple-string result "."
				(the simple-string type))))
    result))

;;; This function is somewhat bummed to make the Hemlock directory command
;;; is fast.
;;;
(defun file-namestring (pathname)
  "Returns the name, type, and version of PATHNAME as a string."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (name (%pathname-name pathname))
	 (type (%pathname-type pathname))
	 (result (or name "")))
    (declare (simple-string result))
    (if (and type (not (zerop (length type))))
	(concatenate 'simple-string result "." type)
	result)))

(defun directory-namestring (pathname)
  "Returns the device & directory parts of PATHNAME as a string."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (directory (%pathname-directory pathname))
 	 (result (%device-string (%pathname-device pathname))))
    (declare (simple-string result))
    (when directory
      (setq result (concatenate 'simple-string result
				(the simple-string (%dirstring directory)))))
    result))

(defun host-namestring (pathname)
  "Returns the host part of PATHNAME as a string."
  (%pathname-host (if (pathnamep pathname) pathname (pathname pathname))))


;;; Do-Search-List  --  Internal
;;;
;;;    Bind var in turn to each element of search list with the specifed
;;; name.
;;;
(defmacro do-search-list ((var name &optional exit-form) . body)
  "Do-Search-List (Var Name [Exit-Form]) {Form}*"
  `(dolist (,var (resolve-search-list ,name nil) ,exit-form)
     (declare (simple-string ,var))
     ,@body))

;;; UNIX-NAMESTRING -- public
;;; 
(defun unix-namestring (pathname &optional (for-input t))
  "Convert PATHNAME into a string that can be used with UNIX system calls."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (device (%pathname-device pathname)))
    (cond ((or (eq device :absolute)
	       (null device)
	       (string= device "Default"))
	   (namestring pathname))
	  (for-input
	   (let ((remainder (namestring-without-device pathname))
		 (first nil))
	     (do-search-list (entry device first)
	       (let ((name (concatenate 'simple-string entry remainder)))
		 (unless first
		   (setf first name))
		 (when (mach:unix-file-kind name)
		   (return name))))))
	  (t
	   (concatenate 'simple-string
			(car (resolve-search-list device t))
			(namestring-without-device pathname))))))



;;;; ENOUGH-NAMESTRING

(defun enough-namestring (pathname &optional
				   (defaults *default-pathname-defaults*))
  "Returns a string which uniquely identifies PATHNAME w.r.t. DEFAULTS." 
  (setq pathname (pathname pathname))
  (setq defaults (pathname defaults))
  (let* ((device (%pathname-device pathname))
	 (directory (%pathname-directory pathname))
	 (name (%pathname-name pathname))
	 (type (%pathname-type pathname))
	 (result "")
	 (need-name nil))
    (declare (simple-string result))
    (when (and device (string-not-equal device (%pathname-device defaults)))
      (setq result (%device-string device)))
    (when (and directory
	       (not (equalp directory (%pathname-directory defaults))))
      (setq result (concatenate 'simple-string result
				(the simple-string (%dirstring directory)))))
    (when (and name (string-not-equal name (%pathname-name defaults)))
      (setq result (concatenate 'simple-string result 
				(the simple-string name))
	    need-name t))
    (when (and type (or need-name
			(string-not-equal type (%pathname-type defaults))))
      (setq result (concatenate 'simple-string result "."
				(the simple-string type))))
    result))



;;;; TRUENAME and other stuff probing stuff.

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
  otherwise.  Returns NIL for directories and other non-file entries."
  (let ((namestring (unix-namestring pathname t)))
    (when (mach:unix-file-kind namestring)
      (let ((truename (mach:unix-resolve-links
		       (mach:unix-maybe-prepend-current-directory
			namestring))))
	(when truename
	  (pathname (mach:unix-simplify-pathname truename)))))))


;;;; Other random operations.

;;; Rename-File  --  Public
;;;
;;;    If File is a File-Stream, then rename the associated file if it exists,
;;; otherwise just change the name in the stream.  If not a file stream, then
;;; just rename the file.
;;;
(defun rename-file (file new-name)
  "Rename File to have the specified New-Name.  If file is a stream open to a
  file, then the associated file is renamed.  If the file does not yet exist
  then the file is created with the New-Name when the stream is closed."
  (let* ((original (truename file))
	 (original-namestring (namestring original))
	 (new-name (merge-pathnames new-name original))
	 (new-namestring (unix-namestring new-name nil)))
    (multiple-value-bind (res error)
			 (mach:unix-rename original-namestring
					   new-namestring)
      (unless res
	(error "Failed to rename ~A to ~A: ~A"
	       original new-name (mach:get-unix-error-msg error)))
      (when (streamp file)
	(file-name file new-namestring))
      (values new-name original (truename new-namestring)))))

;;; Delete-File  --  Public
;;;
;;;    Delete the file, Man.
;;;
(defun delete-file (file)
  "Delete the specified file."
  (let ((namestring (unix-namestring file t)))
    (when (streamp file)
      (close file :abort t))
    (when namestring
      (multiple-value-bind (res err) (mach:unix-unlink namestring)
	(unless res
	  (error "Could not delete ~A: ~A."
		 namestring
		 (mach:get-unix-error-msg err))))))
  t)


;;; User-Homedir-Pathname  --  Public
;;;
;;;    If the user wants a meaningful homedir, she has to define Home:.
;;; Someday, login may do this for us.  Since we must always return something,
;;; we just return Default: if it isn't defined.
;;;
(defun user-homedir-pathname (&optional host)
  "Returns the home directory of the logged in user as a pathname.
  This is obtained from the logical name \"home:\".  If this is not defined,
  then we return \"default:\""
  (declare (ignore host))
  (let ((home (cdr (assoc :home *environment-list* :test #'eq))))
    (if home
	(pathname (if (string-equal home "/") "/"
		      (concatenate 'simple-string home "/")))
	(let ((expansion (if (search-list "home:")
			     (resolve-search-list "home" t))))
	  (if expansion
	      (car expansion)
	      (make-pathname :device "default"))))))

;;; File-Write-Date  --  Public
;;;
(defun file-write-date (file)
  "Return file's creation date, or NIL if it doesn't exist."
  (multiple-value-bind (res dev ino mode nlink uid gid
			    rdev size atime mtime)
		       (mach:unix-stat (unix-namestring file t))
    (declare (ignore dev ino mode nlink uid gid rdev size atime))
    (when res
      (+ unix-to-universal-time mtime))))

;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Returns the file author as a string, or nil if the author cannot be
   determined.  Signals an error if file doesn't exist."
  (multiple-value-bind (winp dev ino mode nlink uid)
		       (mach:unix-stat (unix-namestring (pathname file) t))
    (declare (ignore dev ino mode nlink))
    (if winp (lookup-login-name uid))))



;;;; DIRECTORY.

;;; PARSE-PATTERN  --  internal.
;;;
;;; Parse-pattern extracts the name portion of pathname and converts it into
;;; a pattern usable in match-pattern-p.
;;; 
(defun parse-pattern (pathname)
  (let ((string (file-namestring pathname))
	(pattern nil)
	(last-regular-char nil)
	(index 0))
    (flet ((flush-pending-regulars ()
	     (when last-regular-char
	       (push (subseq string last-regular-char index) pattern))
	     (setf last-regular-char nil)))
      (loop
	(when (>= index (length string))
	  (return))
	(let ((char (schar string index)))
	  (cond ((char= char #\?)
		 (flush-pending-regulars)
		 (push :single-char-wild pattern)
		 (incf index))
		((char= char #\*)
		 (flush-pending-regulars)
		 (push :multi-char-wild pattern)
		 (incf index))
		((char= char #\[)
		 (flush-pending-regulars)
		 (let ((close-bracket (position #\] string :start index)))
		   (unless close-bracket
		     (error "``['' with no corresponding ``]'': ~S" string))
		   (push :character-set pattern)
		   (push (subseq string (1+ index) close-bracket)
			 pattern)
		   (setf index (1+ close-bracket))))
		(t
		 (unless last-regular-char
		   (setf last-regular-char index))
		 (incf index)))))
      (flush-pending-regulars))
    (nreverse pattern)))

;;; MATCH-PATTERN-P  --  internal.
;;;
;;; Determine if string (starting at start) matches pattern.
;;; 
(defun match-pattern-p (string pattern &optional (start 0))
  (cond ((null pattern)
	 (= start (length string)))
	((eq (car pattern) :single-char-wild)
	 (and (> (length string) start)
	      (match-pattern-p string (cdr pattern) (1+ start))))
	((eq (car pattern) :character-set)
	 (and (> (length string) start)
	      (find (schar string start) (cadr pattern))
	      (match-pattern-p string (cddr pattern) (1+ start))))
	((eq (car pattern) :multi-char-wild)
	 (do ((new-start (length string) (1- new-start)))
	     ((< new-start start) nil)
	   (when (match-pattern-p string (cdr pattern) new-start)
	     (return t))))
	((stringp (car pattern))
	 (let* ((expected (car pattern))
		(len (length expected))
		(new-start (+ start len)))
	   (and (>= (length string) new-start)
		(string= string expected :start1 start :end1 new-start)
		(match-pattern-p string (cdr pattern) new-start))))
	(t
	 (error "Bogus thing in pattern: ~S" (car pattern)))))

;;; MATCHING-FILES-IN-DIR  --  internal
;;;
;;; Return a list of all the files in the directory dirname that match
;;; pattern.  If all is nil, ignore files starting with a ``.''.
;;; 
(defun matching-files-in-dir (dirname pattern all)
  (let ((dir (mach:open-dir dirname)))
    (if dir
	(unwind-protect
	    (let ((results nil))
	      (loop
		(let ((name (mach:read-dir dir)))
		  (cond ((null name)
			 (return))
			((or (string= name ".")
			     (string= name "..")
			     (and (not all)
				  (char= (schar name 0) #\.))))
			((or (null pattern)
			     (match-pattern-p name pattern))
			 (if (zerop (length dirname))
			     (push name results)
			     (push (concatenate 'string dirname name)
				   results))))))
	      (values results t))
	  (mach:close-dir dir))
	(values nil nil))))

;;; DIRECTORY  --  public.
;;; 
(defun directory (pathname &key (all t) (check-for-subdirs t))
  "Returns a list of pathnames, one for each file that matches the given
   pathname.  Supplying :all as nil causes this to ignore Unix dot files.  This
   never includes Unix dot and dot-dot in the result."
  (let* ((pathname (if (pathnamep pathname) pathname (pathname pathname)))
	 (device (%pathname-device pathname))
	 (pattern (parse-pattern pathname))
	 (results nil)
	 (really-won nil))
    (if (or (eq device :absolute)
	    (null device)
	    (string= device "Default"))
	(multiple-value-setq (results really-won)
	  (matching-files-in-dir (directory-namestring pathname) pattern all))
	(let ((remainder (namestring-without-device
			  (directory-namestring pathname))))
	  (do-search-list (dir device)
	    (multiple-value-bind
		(files won)
		(matching-files-in-dir (concatenate 'simple-string
						    dir
						    remainder)
				       pattern
				       all)
	      (when won
		(setf really-won t))
	      (setf results (append results files))))))
    (unless really-won
      (error "Could not find ~S." pathname))
    (setf results
	  (sort (delete-duplicates results :test #'string=)
		#'string<))
    (mapcar #'(lambda (name)
		(if (and check-for-subdirs
			 (eq (mach:unix-file-kind name) :directory))
		    (pathname (concatenate 'string name "/"))
		    (pathname name)))
	    results)))


;;;; Printing directories.

;;; PRINT-DIRECTORY is exported from the EXTENSIONS package.
;;; 
(defun print-directory (pathname &optional stream &key all verbose return-list)
  "Like Directory, but prints a terse, multi-coloumn directory listing
   instead of returning a list of pathnames.  When :all is supplied and
   non-nil, then Unix dot files are included too (as ls -a).  When :vervose
   is supplied and non-nil, then a long listing of miscellaneous
   information is output one file per line."
  (setf pathname (pathname pathname))
  (let ((*standard-output* (out-synonym-of stream)))
    (if verbose
	(print-directory-verbose pathname all return-list)
	(print-directory-formatted pathname all return-list))))

(defun print-directory-verbose (pathname all return-list)
  (let ((contents (directory pathname :all all :check-for-subdirs nil))
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
	    (mach:unix-stat namestring)
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
			   (= (logand mode mach::s_ifmt) mach::s_ifdir))))
		(t (format t "Couldn't stat ~A -- ~A.~%"
			   tail
			   (mach:get-unix-error-msg dev-or-err))))
	  (when return-list
	    (push (if (= (logand mode mach::s_ifmt) mach::s_ifdir)
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
	(result (directory pathname :all all)))
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
		(tab-over 
		 (- col-width (length (the simple-string name))))))))
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

(defun complete-file (pathname &key (defaults *default-pathname-defaults*)
			       ignore-types)
  ;; Find all possible pathnames.
  (let ((files
	 (directory (concatenate 'string
				 (namestring (merge-pathnames pathname
							      defaults))
				 "*")
		    :check-for-subdirs nil)))
    (cond ((null files)
	   (values nil nil))
	  ((null (cdr files))
	   (values (merge-pathnames (file-namestring (car files))
				    pathname)
		   t))
	  (t
	   (let ((good-files
		  (delete-if #'(lambda (pathname)
				 (and (pathname-type pathname)
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

;;; Ambiguous-Files  --  Public
;;;
(defun ambiguous-files (pathname &optional defaults)
  "Return a list of all files which are possible completions of Pathname.
  We look in the directory specified by Defaults as well as looking down
  the search list."
  (directory (concatenate 'string
			  (namestring
			   (merge-pathnames pathname
					    (make-pathname :defaults defaults
							   :name nil
							   :type nil)))
			  "*")))


;;; File-writable -- exported from extensions.
;;;
;;;   Determines whether the single argument (which should be a pathname)
;;;   can be written by the the current task.

(defun file-writable (name)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise."
  (let ((truename (probe-file name)))
    (values
     (mach:unix-access
      (unix-namestring (or truename (directory-namestring name)) t)
      (if truename mach:w_ok (logior mach:w_ok mach:x_ok))))))


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
;;;    This fills in a hole in Common Lisp.  We return the first thing we
;;; find by doing a ResolveSearchList on Default.
;;; 
(defun default-directory ()
  "Returns the pathname for the default directory.  This is the place where
  a file will be written if no directory is specified.  This may be changed
  with setf."
  (multiple-value-bind (gr dir-or-error)
		       (mach:unix-current-directory)
    (if gr
	(pathname (concatenate 'simple-string dir-or-error "/"))
	(error dir-or-error))))

;;;
;;; Maybe this shouldn't go here...
(defsetf default-directory %set-default-directory)

;;; %Set-Default-Directory  --  Internal
;;;
;;;    The setf method for Default-Directory.  We actually set the environment
;;; variable Current which is by convention the head of the search list.
;;; 
(defun %set-default-directory (new-val)
  (multiple-value-bind (gr error)
		       (mach:unix-chdir (unix-namestring new-val t))
    (if gr
	(car (setf (search-list "default:")
		   (list (default-directory))))
	(error (mach:get-unix-error-msg error)))))
