;;; -*- Log: code.log; Package: Lisp -*-
;;; ### Some day fix to accept :wild in any pathname component.
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Ugly pathname functions for Spice Lisp.
;;;    these functions are part of the standard Spice Lisp environment.
;;;
;;; Written by Jim Large and Rob MacLachlan
;;;
;;; **********************************************************************
(in-package 'lisp)
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
			  file-writable))
(in-package 'lisp)


;;; Pathname structure


;;; *Default-Pathname-defaults* has all values unspecified except for the
;;;  host.  All pathnames must have a host.
(defvar *default-pathname-defaults* ()
  "Set to the default pathname-defaults pathname (Got that?)")

(defun filesys-init ()
  (setq *default-pathname-defaults* 
	(%make-pathname "Mach" nil nil nil nil nil)))


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



;;;; FSM Compiler

(eval-when (compile eval)
;;; These macros are used in the code emitted by deflex.  They refer
;;; to local vars in the defined function.

(defmacro fsm-scan ()
  '(progn
    (incf pointer)
    (setq current-char (if (>= pointer end)
			   :end
			   (char string pointer)))))

;;; fsm-emit expands into a form which pushes FLAG onto *deflex-tokens* and
;;;  then pushes a string containing the ACCUMULATED characters
(defmacro fsm-emit (flag)
  `(progn
    (vector-push-extend ,flag *deflex-tokens*)
    (vector-push-extend (subseq buffer 0 buffer-index) *deflex-tokens*)
    (setq buffer-index 0)))

;;; fsm-dotfix expands into a form which replaces the last EMITTED flag with
;;;  FLAG, and the last EMITTED token with its self + "." + the ACCUMULATED
;;;  characters.
(defmacro fsm-dotfix (flag)
  `(let ((prev-token (vector-pop *deflex-tokens*)))
     (setf (aref *deflex-tokens* (1- (fill-pointer *deflex-tokens*))) ,flag)
     (vector-push (concatenate 'simple-string
			       prev-token
			       "."
			       (subseq buffer 0 buffer-index))
		  *deflex-tokens*)
     (setq buffer-index 0)))


(defun deflex-arc (arc)
  (do ((set (nth 0 arc))
       (new-state (nth 2 arc))
       (actions (nth 3 arc) (if (member (car actions) '(EMIT DOTFIX))
				(cddr actions)
				(cdr actions)))
       (action-forms
	()
	(cons (case (car actions)
		(ACCUMULATE '(progn 
			      (setf (schar buffer buffer-index) current-char)
			      (incf buffer-index)))
		(SCAN '(fsm-scan))
		(EMIT `(fsm-emit ,(nth 1 actions)))
		(DOTFIX `(fsm-dotfix ,(nth 1 actions)))
		(t (error "Illegal action ~s" (car actions))))
	      action-forms)))
      ((null actions)
       `(when ,(cond ((eq set ':end) `(eq current-char ,set))
		     ((eq set T) T)
		     (t
		      `(and (not (eq current-char :end))
			    ,(if (characterp set)
				 `(char= current-char ,set)
				 `(find current-char
					(the simple-string ,set))))))
	  ,@(nreverse action-forms)
	  (go ,new-state)))))


(defun deflex-state (state)
  (do ((state-name (car state))
       (arc-list (cdr state) (cdr arc-list))
       (form-list () (cons (deflex-arc (car arc-list)) form-list)))
      ((null arc-list)
       `(,state-name
	 ,@(nreverse form-list)
	 (go JAM)))))

(defmacro deflex (name &rest states)
  (do ((states states (cdr states))
       (body-forms () (append body-forms (deflex-state (car states)))))
      ((null states)
       `(defun ,(concat-pnames 'lex- name)
	       (string &optional (start 0) (end (length string)))
	  (declare (simple-string string)
		   (fixnum start end))
	  (setf (fill-pointer *deflex-tokens*) 0)
	  (prog ((current-char (if (> end start)
				   (char string start) :end))
		 (pointer start)
		 (buffer *deflex-buffer*)
		 (buffer-index 0))
	    (declare (simple-string buffer))
	    ,@body-forms
	    
	    WIN (return (values T pointer))
	    JAM (return (values NIL pointer)))))))

); eval-when (compile eval)


(defvar *deflex-buffer* (make-string 100))
(defvar *deflex-tokens* (make-array 20 :adjustable t :fill-pointer 0))



;;;; FSM for lexing pathnames.

(defconstant ses-name-char
  "#@$-+*%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~")

(defconstant fs-whitespace ;whitespace in filenames
  " 	
")

;;; Lex-ses-file-spec separates a sesame file name into its components
;;;
;;;  STRING -- the string containing the file spec.
;;; &optional
;;;  START  -- the first character to look at (defaults to 0)
;;;  END    -- 1+ the last character to look at (defaluts to (length STRING))
;;;
;;; Returns multiple values.
;;;  1st -- () for failure or a list of tokens.
;;;  2nd -- the index of the character that stopped the parse
;;;
;;;    Tokens are represented by a keyword and a string in *deflex-tokens*.
;;; The first token is of type :logical-name (string is log name) or :absolute.
;;; Subsequent tokens are of type :directory-name, :file-name, :extension, or
;;; the string being a pathname component.
;;;
(deflex ses-file-spec
  ;;S1 waits for the first non whitespace character
  (S1 (fs-whitespace --> S1 (SCAN))
      (#\- --> JAM)			;- illegal as start of file name
      (#\/ --> S3 (EMIT :absolute SCAN));/ starts an absolute pathname
      (#\\ --> S2a (ACCUMULATE SCAN))   ;\ quotes char in name or logname
      (#\. --> S5 (EMIT :file-name SCAN))     ;. maybe ends file name
      (ses-name-char --> S2 (ACCUMULATE SCAN))  ;first char in a name or logname
      (:end --> WIN)			; Null filename is legal.
      )

  ;;S2 accumulates chars in a file name, logical name, or directory name.
  (S2 (#\: --> S3 (EMIT :logical-name SCAN))  ;: ends log-name, starts next name
      (#\/ --> S3 (EMIT :directory-name SCAN));/ ends directory name,starts next
      (#\\ --> S2a (ACCUMULATE SCAN))         ;\ quotes char in name
      (#\. --> S5 (EMIT :file-name SCAN))     ;. maybe ends file name
      (:end --> WIN (EMIT :file-name))
      (ses-name-char --> S2 (ACCUMULATE SCAN))
      (fs-whitespace --> S9 (EMIT :file-name SCAN))
      (T --> JAM (EMIT :file-name))
      )

  ;;S2a waits for any char following a \ found in S2
  (S2a (:END --> JAM)
       (T --> S2 (ACCUMULATE SCAN)))

  ;;S3 accumulates chars in a file name or directory name
  (S3 (#\/ --> JAM)
      (T --> S3b))
  (S3b (#\/ --> S3 (EMIT :directory-name SCAN));/ ends directory name,starts next
       (#\. --> S4 (EMIT :file-name SCAN))     ;. maybe ends file name
       (#\\ --> S3a (ACCUMULATE SCAN))         ;\ quotes char in name
       (:end --> WIN (EMIT :file-name))
       (ses-name-char --> S3b (ACCUMULATE SCAN))
       (fs-whitespace --> S9 (EMIT :file-name SCAN))
       (T --> JAM (EMIT :file-name))
       )

  ;;S3a waits for any char following a \ found in S3
  (S3a (:end --> JAM)
       (T --> S3b (ACCUMULATE SCAN)))

  ;;S4 waits for chars in a name following a dot.  If it encounters the end of
  ;; the pathname, then the chars are the extension.  Otherwise, the chars are
  ;; really part of the last token, so we mash them back on with dotfix.
  (S4 (#\/ --> S3 (DOTFIX :directory-name SCAN))
      (#\\ --> S4a (ACCUMULATE SCAN))
      (#\. --> S4 (DOTFIX :file-name SCAN))
      (:end --> WIN (EMIT :extension))
      (fs-whitespace --> S9 (EMIT :extension SCAN))
      (ses-name-char --> S4 (ACCUMULATE SCAN))
      (T --> JAM (EMIT :extension))
      )

  ;;S4a waits for any char following a \ found in S4
  (S4a (:end --> JAM)
       (T --> S4 (ACCUMULATE SCAN)))

  ;;S5 is the same as S4 but name may also be logical name
  (S5 (#\/ --> S3 (DOTFIX :directory-name SCAN))
      (#\: --> S3 (DOTFIX :logical-name SCAN))
      (#\\ --> S5a (ACCUMULATE SCAN))
      (#\. --> S5 (DOTFIX :file-name SCAN))
      (:end --> WIN (EMIT :extension))
      (fs-whitespace --> S9 (EMIT :extension SCAN))
      (ses-name-char --> S5 (ACCUMULATE SCAN))
      (T --> JAM (EMIT :extension))
      )

  ;;S5a waits for any char following a \ found in S5
  (S5a (:end --> JAM)
       (T --> S5 (ACCUMULATE SCAN)))

  ;;S9 eats trailing whitespace.
  (S9 (fs-whitespace --> S9 (SCAN))
      (:end --> WIN)
      (t --> JAM))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ses-tokens->pathname converts the contents of *deflex-tokens* into a pathname
;;; 
(defun ses-tokens->pathname ()
  (do ((end (fill-pointer *deflex-tokens*))
       (result (make-pathname))
       (dirlist ())
       (token-val ())
       (ix 0 (+ ix 2))
       (default t))
      ((>= ix end)
       (setf (%pathname-directory result)
	     (cond (dirlist
		    (when default (setf (%pathname-device result) "Default"))
		    (coerce (the list (nreverse dirlist)) 'simple-vector))
		   (default nil)
		   (t '#())))
       result)
    (declare (list dirlist)
	     (fixnum ix end))
    (setq token-val (aref *deflex-tokens* (1+ ix)))
    (case (aref *deflex-tokens* ix)
      (:ABSOLUTE
       (setf (%pathname-device result) :ABSOLUTE)
       (setq default nil))
      (:LOGICAL-NAME
       (setf (%pathname-device result) token-val)
       (setq default nil))
      (:DIRECTORY-NAME (push token-val dirlist))
      (:FILE-NAME
       (unless (zerop (length (the simple-string token-val)))
	 (setf (%pathname-name result) token-val)))
      (:EXTENSION (setf (%pathname-type result) token-val)))))



;;;; PARSE-NAMESTRING and PATHNAME.

(defun parse-namestring (thing &optional host 
			       (defaults *default-pathname-defaults*)
			       &key (start 0) end (junk-allowed nil))
  "Parses a string representation of a pathname into a pathname."
  (declare (ignore host defaults))
  (let* ((thing (typecase thing
		  (string (coerce thing 'simple-string))
		  (pathname (return-from parse-namestring (values thing start)))
		  (stream (file-name thing))
		  (symbol (symbol-name thing))
		  (t (error "This thing is a bad thing for parse-namestring: ~S"
			    thing))))
	 (end (or end (length thing))))
    (declare (simple-string thing))
    (multiple-value-bind (won next-field)
			 (lex-ses-file-spec thing start end)
      (unless (or won junk-allowed)
	(error "There's junk in this thing that you gave to parse-namestring:~%~
		~4T\"~A\"~%~0,1,V,'.@A" thing (+ next-field 5) #\^))
      (values (ses-tokens->pathname) next-field))))

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
	       (%primitive shrink-vector res (- len i)))
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
  (setq pathname (pathname pathname))
  (let* ((directory (%pathname-directory pathname))
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
    (when type
      (setq result (concatenate 'simple-string result "."
				(the simple-string type))))
    result))

(defun %ses-get-useful-name (pathname)
  "NAMESTRING of pathname ignoring the device slot."
  (setq pathname (pathname pathname))
  (let* ((directory (%pathname-directory pathname))
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
    (when type
      (setq result (concatenate 'simple-string result "."
				(the simple-string type))))
    result))

;;; This function is somewhat bummed to make the Hemlock directory command
;;; is fast.
;;;
(defun file-namestring (pathname)
  "Returns the name, type, and version of PATHNAME as a string."
  (unless (pathnamep pathname) (setq pathname (pathname pathname)))
  (let* ((name (%pathname-name pathname))
	 (type (%pathname-type pathname))
	 (result (or name "")))
    (declare (simple-string result))
    (if type
	(concatenate 'simple-string result "." type)
	result)))

(defun directory-namestring (pathname)
  "Returns the device & directory parts of PATHNAME as a string."
  (setq pathname (pathname pathname))
  (let* ((directory (%pathname-directory pathname))
 	 (result (%device-string (%pathname-device pathname))))
    (declare (simple-string result))
    (when directory
      (setq result (concatenate 'simple-string result
				(the simple-string (%dirstring directory)))))
    result))

(defun host-namestring (pathname)
  "Returns the host part of PATHNAME as a string."
  (setq pathname (pathname pathname))
  (%pathname-host pathname))



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
;;;    Another silly file function trivially different from another function.
;;;
(defun truename (pathname)
  "Return the pathname for the actual file described by the pathname
  An error is signalled if no such file exists."
  (let ((result (probe-file pathname)))
    (unless result
      (error "The file ~S does not exist." (namestring pathname)))
    result))

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


;;; Sub-Probe-File  --  Internal
;;;
;;;    Does the work of Probe-File, returning an additional value which
;;; indicates whether the name is really a file.
;;;
(defun sub-probe-file (pathname)
  (setq pathname (pathname pathname))
  (flet ((pathnamify (ns etype)
	   (declare (simple-string ns))
	   (case etype
	     (:entry_remote
	      (values (parse-namestring (concatenate 'simple-string ns "/"))
		      :entry_directory))
	     (t
	      (values (parse-namestring ns) etype)))))
    (let ((log-name (or (%pathname-device pathname) "default")))
      (if (eq log-name :absolute)
	  (let ((namestring (namestring pathname)))
	    (multiple-value-bind (name etype)
				 (mach:unix-subtestname namestring)
	      (if (null name) NIL
		  (pathnamify name etype))))
	  (if (null (%pathname-device pathname))
	      (multiple-value-bind (name etype)
				   (mach:unix-subtestname
				    (%ses-get-useful-name pathname))
		(if (null name) NIL (pathnamify name etype)))
	      (let ((namestring (%ses-get-useful-name pathname)))
		(do-search-list (entry log-name)
		  (let ((str (concatenate 'simple-string entry namestring)))
		    (declare (simple-string str))
		    (multiple-value-bind (name etype)
					 (mach:unix-subtestname str)
		      (when name
			(return (pathnamify name etype))))))))))))


;;; Probe-File  --  Public
;;;
;;;    Just call Sub-Probe-File and return nil when it isn't a file.
;;;
(defun probe-file (pathname)
  "Return a pathname which is the truename of the file if it exists, NIL
  otherwise.  Returns NIL for directories and other non-file entries."
  (multiple-value-bind (pn f)
		       (sub-probe-file pathname)
    (if (eq f :entry_file) pn)))



;;; Predict-Name  --  Internal
;;;
;;;    Predict-Name is a function used by Open to get an absolute pathname 
;;; for a file being opened.  Returns the truename of the file and
;;; whether it really exists or not.
;;;
(defun predict-name (file-name for-input)
  (let* ((pathname (pathname file-name))
	 (device (%pathname-device pathname))
	 (truename (probe-file pathname)))
    (cond ((eq device :absolute)
	   (if truename
	       (values (namestring truename) t)
	       ;; Try again in case file-name is a directory.
	       (predict-name-with-subtest (namestring pathname))))
	  ((and for-input truename)
	   (values (namestring truename) t))
	  (t
	   (let ((expansion (resolve-search-list (or device "default") t)))
	     (let ((name (concatenate 'simple-string (car expansion)
				      (%ses-get-useful-name pathname))))
	       (declare (simple-string name))
	       (predict-name-with-subtest name)))))))

(defun predict-name-with-subtest (name)
  (let ((gr (mach:unix-subtestname name)))
    (if gr
	(values gr t)
	(values (mach::simplify-file-name name) nil))))



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
  (if (streamp file)
      (let* ((name (file-name file))
	     (pn (parse-namestring name))
	     (npn (merge-pathnames new-name pn))
	     (new (predict-name npn nil)))
	(when (mach:quick-subtestname name)
	  (multiple-value-bind (res err) (mach:Unix-rename name new)
	    (if (null res) (error "Failed to rename ~A to ~A, unix error: ~A."
				  name new (mach:get-unix-error-msg err)))))
	(file-name file new)
	(values npn pn (parse-namestring new)))
      (let* ((pn (or (sub-probe-file file)
		     (error "File to rename does not exist: ~S" file)))
	     (npn (merge-pathnames new-name pn))
	     (new (predict-name npn nil)))
	(multiple-value-bind (res err) (mach:unix-rename (namestring pn) new)
	  (if res (values npn pn (parse-namestring new))
	      (error "Failed to rename ~A to ~A, unix error: ~A."
		     (namestring pn) new (mach:get-unix-error-msg err)))))))

;;; Delete-File  --  Public
;;;
;;;    Delete the file, Man.
;;;
(defun delete-file (file)
  "Delete the specified file."
  (let ((tn (sub-probe-file file)))
    (when (streamp file)
      (close file :abort t))
    (if tn
	(let ((ns (namestring tn)))
	  (multiple-value-bind (res err) (mach:unix-unlink ns)
	    (if (null res)
		(error "Failed to delete ~A, unix error: ~A."
		       ns (mach:get-unix-error-msg err)))))
	(unless (streamp file)
	  (error "File to be deleted does not exist: ~S" file)))) 
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
  (let ((tn (sub-probe-file file)))
    (when tn
      (multiple-value-bind (res dev ino mode nlink uid gid
				rdev size atime mtime)
			   (mach:unix-stat (namestring tn))
	(declare (ignore dev ino mode nlink uid gid rdev size atime))
	(if (null res) 0
	    (+ unix-to-universal-time mtime))))))

;;; File-Author  --  Public
;;;
(defun file-author (file)
  "Returns the file author as a string, or nil if the author cannot be
   determined.  Signals an error if file doesn't exist."
  (let ((filename (truename file)))
    (multiple-value-bind (winp dev ino mode nlink uid)
			 (mach:unix-stat (namestring filename))
      (declare (ignore dev ino mode nlink))
      (if winp (lookup-login-name uid)))))



;;;; DIRECTORY.

;;; DO-DIRECTORY searches a directory, binding the vars to the name and entry
;;; type.  Pattern and All are used in MACH:UNIX-SEARCH-DIRECTORY.
;;;
(defmacro do-directory ((name-var etype-var pattern &optional (all t) result)
			. body)
  "Do-Directory (Name Entry-Type Pattern [All] [Result]) {Form}*.
   If All is non-nil (the default), then Unix dot files to be processed.
   Unix dot and dot-dot are never processed."
  (let ((file (gensym))
	(res (gensym))
	(type (gensym))
	(p (gensym)))
    `(dolist (,file (mach:unix-search-directory ,pattern ,all)
		    ,result)
       (declare (simple-string ,file))
       (let* ((,p (position #\/ ,file :from-end t))
	      (,name-var
	       (if ,p
		   (subseq ,file (the fixnum (1+ (the fixnum ,p))))
		   ,file)))
	 (declare (simple-string ,name-var))
	 (unless (or (string= "." ,name-var) (string= ".." ,name-var))
	   (let ((,etype-var (multiple-value-bind (,res ,type)
						  (mach:quick-subtestname ,file)
			       (declare (ignore ,res))
			       ,type)))
	     ,@body))))))

(defun directory (pathname &key (all t))
  "Returns a list of pathnames, one for each file that matches the given
   pathname.  Supplying :all as nil causes this to ignore Unix dot files.  This
   never includes Unix dot and dot-dot in the result."
  (setq pathname (pathname pathname))
  (multiple-value-bind (dir pattern) (find-directory pathname)
    (let ((res ()))
      (do-directory (name etype pattern all)
	(if (eq etype :entry_file)
	    (let ((last-dot (position #\. name :from-end t)))
	      (push
	       (%make-pathname
		"Mach" :absolute dir
		(if last-dot (subseq name 0 last-dot) name)
		(if last-dot (subseq name (1+ last-dot)))
		nil)
	       res))
	    (push
	     (%make-pathname
	      "Mach" :absolute
	      (concatenate 'simple-vector dir (vector name))
	      nil nil nil)
	     res)))
      (nreverse res))))

;;; FIND-DIRECTORY returns an absolute directory vector for pathname as a
;;; first argument and an absolute namestring.  The namestring includes a
;;; trailing asterisk, wildcard, when the given pathname is immediately
;;; probe-able as a directory.
;;;
(defun find-directory (pathname)
  (multiple-value-bind (pn type)
		       (sub-probe-file pathname)
    (if pn
	(let ((ns (namestring pn)))
	  (declare (simple-string ns))
	  (case type
	    (:entry_directory
	     (when (char/= (schar ns (the fixnum (1- (length ns)))) #\/)
	       (setq ns (concatenate 'simple-string ns "/")))
	     (values (%pathname-directory (pathname ns))
		     (concatenate 'simple-string ns "*")))
	    (t (values (%pathname-directory pn) ns))))
	(multiple-value-bind (pn type)
			     (sub-probe-file
			      (make-pathname
			       :directory (%pathname-directory pathname)
			       :device (%pathname-device pathname)))
	  (unless pn
	    (error "Directory does not exist: ~S" pathname))
	  (let ((ns (namestring pn)))
	    (case type
	      (:entry_directory
	       (values
		(%pathname-directory pn)
		(concatenate 'simple-string ns (file-namestring pathname))))
	      (t
	       (error "~S is not a directory." pathname))))))))



;;;; Printing directories and determining file owner names.

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
  (multiple-value-bind (dir pattern) (find-directory pathname)
    (declare (ignore dir))
    (format t "Directory of ~A :~%" pattern)
    (let ((dir-name (directory-namestring pattern))
	  (result ()))
      (do-directory (name etype pattern all (nreverse result))
	(let ((slash-name (if (eq etype :entry_file)
			      name
			      (concatenate 'simple-string name "/"))))
	  (declare (simple-string slash-name))
	  (when return-list
	    (push (pathname (concatenate 'simple-string dir-name slash-name))
		  result))
	  (multiple-value-bind 
	      (reslt dev-or-err ino mode nlink uid gid rdev size atime mtime)
	      (mach:unix-stat (concatenate 'simple-string dir-name name))
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
		     (format t "~2D ~8A ~8D ~12A ~A~%"
			     nlink
			     (or (lookup-login-name uid) uid)
			     size
			     (decode-universal-time-for-files mtime year)
			     slash-name)))
		  (t (format t "Couldn't stat ~A -- ~A.~%"
			     slash-name
			     (mach:get-unix-error-msg dev-or-err))))))))))

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
	(result ()))
    (declare (list names) (fixnum max-len cnt))
    ;;
    ;; Get the data.
    (multiple-value-bind (dir pattern) (find-directory pathname)
      (declare (ignore dir))
      (do-directory (name etype pattern all)
	(let* ((slash-name (if (eql etype :entry_file)
			       name
			       (concatenate 'simple-string name "/")))
	       (len (length slash-name)))
	  (declare (simple-string slash-name)
		   (fixnum len))
	  (when return-list
	    (push (pathname (concatenate 'simple-string
					 (directory-namestring pattern)
					 slash-name))
		  result))
	  
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
	(format t "Directory of ~A :~%" pattern)
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
	  (terpri))))
    (when return-list (nreverse result))))



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



;;; Complete-One-File  --  Internal
;;;
;;;    Return as values a string and the greatest common prefix of all
;;; the files corresponding to pattern.
;;;
(defun complete-one-file (pattern default-type ignore-types)
  (let ((first nil)
	(length nil))
    (do-directory (name etype pattern nil)
      (declare (ignore etype))
      (let* ((last-dot (position #\. name :from-end t))
	     (type (if last-dot (subseq name (1+ last-dot)))))
	(cond ((and (not (string= type default-type))
		    (member type ignore-types :test #'string=)))
	      (first
	       (let ((msm (string-not-equal name first :end2 length)))
		 (when (and msm (< msm length))
		   (setq length msm))))
	      (t
	       (setq first name)
	       (setq length (length name))))))
    (values first length)))


;;; Complete-File  --  Public
;;;
;;;    If the pathname is absolute, just call Complete-One-File on and test
;;; whether the result is a file.  If a relative pathname, do it on each
;;; directory, accumulating the result.
;;;
(defun complete-file (pathname &key defaults ignore-types)
  "Attempt to complete Pathname as the name of a file.  If the resulting
  completion is unique, return T as the second value.  If there is no
  possible completion, return both values NIL."
  (setq pathname (pathname pathname))
  (setq defaults (if defaults (pathname defaults) *default-pathname-defaults*))
  (flet ((pathnamify (res len ambiguous pathname)
		     (values 
		      (make-pathname :device (%pathname-device pathname)
				     :directory (%pathname-directory pathname)
				     :defaults (parse-namestring (subseq res 0 len)))
		      (not ambiguous))))
    (let ((dev (or (%pathname-device pathname) "default"))
	  (default-type (%pathname-type defaults)))
      (if (eq dev :absolute)
	  (multiple-value-bind
	      (res len)
	      (complete-one-file (concatenate 'simple-string
					      (namestring pathname)
					      "*")
				 default-type ignore-types)
	    (declare (fixnum len))
	    (if res
		(pathnamify res len
			    (/= (the fixnum (length res)) len)
			    pathname)
		(values nil nil)))
	  (let ((namestring (%ses-get-useful-name pathname))		 
		(dirs (if (and (%pathname-directory defaults)
			       (string-equal dev "default"))
			  (list (directory-namestring defaults))
			  ()))
		(max most-positive-fixnum)
		(max-str nil)
		(ambiguous nil))
	    (declare (simple-string namestring))
	    (do-search-list (entry dev)
			    (pushnew entry dirs :test #'string-equal))
	    (dolist (entry dirs)
	      (let ((str (concatenate 'simple-string entry namestring "*")))
		(declare (simple-string str))
		(multiple-value-bind
		    (res len)
		    (complete-one-file str default-type ignore-types)
		  (when res
		    (unless ambiguous
		      (setq ambiguous (or max-str (/= (length res) len))))
		    (if max-str
			(setq max (or (string-not-equal res max-str :end1 len
							:end2 max)
				      max))
			(setq max len  max-str res))))))
	    (if max-str
		(pathnamify max-str max ambiguous pathname)
		(values nil nil)))))))

;;; File-writable -- exported from extensions.
;;;
;;;   Determines whether the single argument (which should be a pathname)
;;;   can be written by the the current task.

(defun file-writable (name)
  "File-writable accepts a pathname and returns T if the current
  process can write it, and NIL otherwise."
  (multiple-value-bind (tn exists) (predict-name name nil)
    (if exists
	(values (mach:unix-access tn mach:w_ok))
	(values (mach:unix-access (directory-namestring tn)
				  (logior mach:w_ok mach:x_ok))))))


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

;;; Ambiguous-Files  --  Public
;;;
;;;    If the pathname is absolute, just do a directory.  If it is relative,
;;; do a directory on each directory in the search-list and merge the results.
;;;
(defun ambiguous-files (pathname &optional defaults)
  "Return a list of all files which are possible completions of Pathname.
  We look in the directory specified by Defaults as well as looking down
  the search list."
  (setq pathname (pathname pathname)
	defaults (if defaults (pathname defaults) *default-pathname-defaults*))
  (let ((dev (or (%pathname-device pathname) "default")))
    (if (eq dev :absolute)
	(directory (concatenate 'simple-string (namestring pathname) "*"))
	(let ((namestring (%ses-get-useful-name pathname))		 
	      (dirs (if (and (%pathname-directory defaults)
			     (string-equal dev "default"))
			(list (directory-namestring defaults))
			()))
	      (res ()))
	  (declare (simple-string namestring))
	  (do-search-list (entry dev) (pushnew entry dirs :test #'string-equal))
	  (dolist (entry dirs)
	    (let ((str (concatenate 'simple-string entry namestring "*")))
	      (declare (simple-string str))
	      (setq res (merge 'list res (directory str)
			       #'pathname-order))))
	  res))))

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
	dir-or-error
	(error (mach:get-unix-error-msg dir-or-error)))))

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
		       (mach:unix-chdir (predict-name new-val nil))
    (if gr
	(car (setf (search-list "default:")
		   (cdr (multiple-value-list (mach:unix-current-directory)))))
	(error (mach:get-unix-error-msg error)))))
