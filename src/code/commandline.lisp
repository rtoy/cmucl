;;; -*- Mode: Lisp; Package: Extensions; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/commandline.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Stuff to eat the command line passed to us from the shell.
;;; Written by Bill Chiles.
;;;

(in-package "EXTENSIONS")

(intl:textdomain "cmucl")

(export '(*command-line-application-arguments* *command-line-words* *command-line-switches*
	  *command-switch-demons* *command-line-utility-name*
	  *command-line-strings* *batch-mode*
	  cmd-switch-string command-line-switch-p
	  cmd-switch-name cmd-switch-value cmd-switch-words command-line-switch
	  defswitch cmd-switch-arg get-command-line-switch))

(defvar *command-line-application-arguments* ()
  "A list of all the command line arguments after --")

(defvar *command-line-switches* ()
  "A list of cmd-switch's representing the arguments used to invoke
  this process.")

(defvar *command-line-utility-name* ""
  "The string name that was used to invoke this process.")

(defvar *command-line-words* ()
  "A list of words between the utility name and the first switch.")

(defvar *command-line-strings* ()
  "A list of strings obtained from the command line that invoked this process.")

(defvar *command-switch-demons* ()
  "An Alist of (\"argument-name\" . demon-function)")

(defvar *batch-mode* nil
  "When True runs lisp with its input coming from standard-input.
  If an error is detected returns error code 1, otherwise 0.")

(defstruct (command-line-switch (:conc-name cmd-switch-)
				(:constructor make-cmd-switch
					      (name value words))
				(:print-function print-command-line-switch))
  name         		;the name of the switch
  value                 ;the value of that switch
  words                 ;random words dangling between switches assigned to the
                        ;preceeding switch
  )

(defun print-command-line-switch (object stream n)
  (declare (ignore n))
  (write-string "#<Command Line Switch " stream)
  (prin1 (cmd-switch-name object) stream)
  (let ((value (cmd-switch-value object))
	(words (cmd-switch-words object)))
    (when (or value words) (write-string " -- " stream)
      (when value (prin1 value stream))
      (when words (prin1 words stream))))
  (write-string ">" stream))



;;;; Processing the command strings.

(defun process-command-strings (init-command-switches-p)
  (setq *command-line-words* nil)
  (setq *command-line-switches* nil)
  (let ((cmd-strings lisp::lisp-command-line-list)
	str)
    (declare (special lisp::lisp-command-line-list))
    ;; Set some initial variables.
    ;; 
    (setf *command-line-strings* (copy-list lisp::lisp-command-line-list))
    (setf *command-line-utility-name* (pop cmd-strings))
    (setq str (pop cmd-strings))
    ;; Set initial command line words.
    ;; 
    (loop
      (unless str (return nil))
      (unless (zerop (length (the simple-string str)))
	(when (char= (schar str 0) #\-) 
	  (setq *command-line-words* (reverse *command-line-words*))
	  (return nil))
	(push str *command-line-words*))
      (setq str (pop cmd-strings)))

    (when (string= str "--")
      ;; Handle the special case where -- is the first option.  The
      ;; code below interprets that incorrectly and I (rtoy) don't
      ;; want to mess with that, so just set up
      ;; *command-line-application-arguments* and return.
      (setf *command-line-application-arguments* cmd-strings)
      (return-from process-command-strings nil))
    
    ;; Set command line switches.
    ;;
    (loop
      (unless str
	(return (setf *command-line-switches*
		      (nreverse *command-line-switches*))))
      (let* ((position (position #\= (the simple-string str) :test #'char=))
	     (switch (subseq (the simple-string str) 1 position))
	     (value (if position
			(subseq (the simple-string str) (1+ position)
				(length (the simple-string str))))))
	(setq str (pop cmd-strings))
	;; Set this switch's words until the next switch.
	;; 
	(let (word-list)
	  (loop
	    (unless str
	      (when init-command-switches-p
		(push (make-cmd-switch switch value (nreverse word-list))
		      *command-line-switches*))
	      (return nil))
	    
	    (unless (zerop (length (the simple-string str)))
	      (when (char= #\- (schar str 0))
		(when init-command-switches-p
		  (push (make-cmd-switch switch value (nreverse word-list))
			*command-line-switches*))
		(when (and (= (length str) 2)
			   (char= #\- (schar str 1)))
		  ;; Gather up everything after --, and exit.
		  (setf *command-line-application-arguments* cmd-strings)
		  (setf str nil))
		(return nil))
	      (push str word-list))
	    (setq str (pop cmd-strings))))))))

(defun get-command-line-switch (sname)
  "Accepts the name of a switch as a string and returns the value of
  the switch.  If no value was specified, then any following words are
  returned.  If there are no following words, then t is returned.  If
  the switch was not specified, then nil is returned."
  (let* ((name (if (char= (schar sname 0) #\-) (subseq sname 1) sname))
	 (switch (find name *command-line-switches*
		       :test #'string-equal
		       :key #'cmd-switch-name)))
    (when switch
      (or (cmd-switch-value switch)
	  (cmd-switch-words switch)
	  T))))



;;;; Defining Switches and invoking demons.

(defvar *complain-about-illegal-switches* t
  "When set, invoking switch demons complains about illegal switches
  that have not been defined with DEFSWITCH.")

;;; This is a list of lists consisting of the legal switch names,
;;; switch description, and argument description.  The description and
;;; argument description can be NIL.  (Should probably do something
;;; better, but this is good enough for the little bit of processing
;;; that we need.)  DEFSWITCH sets this, and INVOKE-SWITCH-DEMONS
;;; makes sure all the switches it sees are on this list.
;;;
(defvar *legal-cmd-line-switches* nil)

;;; INVOKE-SWITCH-DEMONS cdrs down the list of *command-line-switches*.  For
;;; each switch, it checks to see if there is a switch demon with the same
;;; name.  If there is, then that demon is called as a function on the switch.
;;;
(defun invoke-switch-demons (&optional (switches *command-line-switches*)
					 (demons *command-switch-demons*))
  (flet ((invoke-demon (switch)
	   (let* ((name (cmd-switch-name switch))
		  (demon (cdr (assoc name demons :test #'string-equal))))
	     (cond (demon (funcall demon switch))
		   ((or (member name *legal-cmd-line-switches* :test #'string-equal :key #'car)
			(not *complain-about-illegal-switches*)))
		   (t (warn (intl:gettext "~S is an illegal switch") switch)))
	     (lisp::finish-standard-output-streams))))
    ;; We want to process -help (or --help) first, if it's given.
    ;; Since we're asking for help, we don't want to process any of
    ;; the other switches.
    (let ((maybe-help (or (find "help" switches :key #'cmd-switch-name :test #'string-equal)
			  (find "-help" switches :key #'cmd-switch-name :test #'string-equal))))
      (if maybe-help
	(invoke-demon maybe-help)
	(dolist (switch switches t)
	  (invoke-demon switch))))))

(defmacro defswitch (name &optional function docstring arg-name)
  "Associates function with the switch name in
  *command-switch-demons*.  Name is a simple-string that does not
  begin with a hyphen, unless the switch name really does begin with
  one.  Function is optional, but defining the switch is necessary to
  keep invoking switch demons from complaining about illegal switches.
  This can be inhibited with *complain-about-illegal-switches*.

  The optional arguments, arg-name and docstring, are used by -help to
  describe the switch.  Arg-name is a string naming the argument (if
  any) for the switch.  Docstring describe the switch."
  (let ((gname (gensym))
	(gfunction (gensym)))
    (when docstring
      (intl::note-translatable intl::*default-domain* docstring))
    (when arg-name
      (intl::note-translatable intl::*default-domain* arg-name))
    `(let ((,gname ,name)
	   (,gfunction ,function))
       (check-type ,gname simple-string)
       (check-type ,gfunction (or symbol function) (intl:gettext "a symbol or function"))
       (push (list ,gname ,docstring ,arg-name) *legal-cmd-line-switches*)
       (when ,gfunction
	 (push (cons ,gname ,gfunction) *command-switch-demons*)))))


(defun eval-switch-demon (switch)
  (let ((cmds (cmd-switch-arg switch)))
    (do ((length (length cmds))
	 (start 0))
	((>= start length))
      (multiple-value-bind (form next)
	  (read-from-string cmds nil nil :start start)
	(eval form)
	(lisp::finish-standard-output-streams)
	(setf start next)))))

;; Docstrings should have lines longer than 72 characters so that we
;; can print out the docstrings nicely on one line for help.
;;                                                                     | <-- char 72
(defswitch "eval" #'eval-switch-demon
  "Evaluate the specified Lisp expression during the start up
  sequence.  the value of the form will not be printed unless it is
  wrapped in a form that does output."
  "expression")

(defun load-switch-demon (switch)
  (load (cmd-switch-arg switch)))

(defswitch "load" #'load-switch-demon
  "Loads the specified file into Lisp before entering Lisp's
  read-eval-print loop."
  "filename")

(defun cmd-switch-arg (switch)
  (or (cmd-switch-value switch)
      (car (cmd-switch-words switch))
      (car *command-line-words*)))

(defswitch "core" nil
  "Specifies the suspended Lisp image ('core' file) to start up"
  "corefile")

(defswitch "init" nil
  "Specifies the name of a file containing user customizations that is
  to be loaded each time Lisp starts up (default ~/init or
  ~/.cmucl-init.lisp).  The loader loads any existing compiled binary
  or the lisp source if none."
  "filename")

(defswitch "noinit" nil
  "Suppresses loading of the init file and also prevents -edit from
  loading the Hemlock init file.")

(defswitch "nositeinit" nil
  "Suppresses loading of the site-init site specific initialization
  file.")

(defswitch "hinit" nil
  "Specifies the name of the Hemlock init file (default ~/hemlock-init
  or ~/.hemlock-init), which is loaded only when Hemlock is started."
  "filename")

(defswitch "batch" nil
  "Causes Lisp to run in batch mode where all input is directed from
  standard-input.  A unix return code of 0 is returned upon
  encountering an EOF, while any unhandled error condition will cause
  an immediate exit with a return code of 1, instead of entering the
  debugger.")

(defswitch "dynamic-space-size" nil
  "Specifies the number of megabytes that should be allocated to the
  heap.  If not specified, a platform-specific default is used.  If 0,
  the platform-specific maximum heap size is used.  The actual maximum
  allowed heap size is platform-specific."
  "megabytes")

(defswitch "read-only-space-size" nil
  "Specifies the number of megabytes that should be allocated for the
  read-only space.  If not specified, a platform-specific default is
  used.  The actual maximum allowed read-only size is
  platform-specific."
  "megabytes")

(defswitch "static-space-size" nil
  "Specifies the number of megabytes that should be allocated for the
  static space.  If not specified, a platform-specific default is
  used.  The actual maximum allowed static space size is
  platform-specific."
  "megabytes")

(defswitch "control-stack-size" nil
  "Specifies the number of megabytes that should be allocated for the
  control stack.  If not specified, a platform-specific default is
  used.  The actual maximum allowed control stack size is
  platform-specific."
  "megabytes")

(defswitch "binding-stack-size" nil
  "Specifies the number of megabytes that should be allocated for the
  binding stack.  If not specified, a platform-specific default is
  used.  The actual maximum allowed binding stack size is
  platform-specific."
  "megabytes")

(defswitch "lib" nil
  "A colon-separated list of directories to be used for the library:
  search-list."
  "libpath")

(defswitch "quiet" nil
  "Causes Lisp to start up silently, disabling printing of the herald
  and causing most unnecessary noise, like GC messages,load messages,
  etc. to be suppressed.")

(defswitch "debug-lisp-search" nil
  "Enables printing of messages indication how CMUCL is searching for
  its default core file.")

(defswitch "unidata" nil
  "Specify the unidata.bin file to be used."
  "filename")

(defun help-switch-demon (switch)
  (declare (ignore switch))
  (format t (intl:gettext "~&Usage: ~A <options>~2%") *command-line-utility-name*)
  (flet
      ((get-words (s)
	 (declare (string s))
	 ;; Return a list of all the words from S.  A word is defined
	 ;; as any sequence of characters separated from others by
	 ;; whitespace consisting of space, newline, tab, formfeed, or
	 ;; carriage return.
	 (let ((end (length s)))
	   (loop for left = 0 then (+ right 1)
		 for right = (or
			      (position-if #'(lambda (c)
					       (member c
						       '(#\space #\newline #\tab #\ff #\cr)))
					   s
					   :start left)
			      end)
		 ;; Collect the word bounded by left and right in a list.
		 unless (and (= right left))
		   collect (subseq s left right) into subseqs
		 ;; Keep going until we reach the end of the string.
		 until (>= right end)
		 finally (return subseqs)))))

    (dolist (s (sort *legal-cmd-line-switches* #'string<
		     :key #'car))
      (destructuring-bind (name doc arg)
	  s
	(format t "    -~A ~@[~A~]~%" name (if arg (intl:gettext arg)))
	;; Poor man's formatting of the help string
	(let ((*print-right-margin* 80))
	  ;; Extract all the words from the string and print them out
	  ;; one by one with a space between each, wrapping the output
	  ;; if needed.  Each line is indented by 8 spaces.
	  ;;
	  ;; "~@<       ~@;"
	  ;;    per-line prefix of spaces and pass the whole arg list
	  ;;    to this directive.
	  ;;
	  ;; "~{~A~^ ~}"
	  ;;    loop over each word and print out the word followed by
	  ;;    a space.
	  ;;
	  ;; "~:@>"
	  ;;    No suffix, and insert conditional newline after each
	  ;;    group of blanks if needed.
	  (format t "~@<        ~@;~{~A~^ ~}~:@>"
		  (get-words (intl:gettext doc))))
	(terpri))))
  (ext:quit))
  
(defswitch "help" #'help-switch-demon
  "Print out the command line options and exit")

(defswitch "-help" #'help-switch-demon
  "Same as -help.")

(defun version-switch-demon (switch)
  (declare (ignore switch))
  (format t "~A~%" (lisp-implementation-version))
  (ext:quit))

;; the switches "-version" and "--version" are never actually called
;; from lisp because main() handles it and returns before the lisp
;; initial function is ever run.  It's here so that -help will print
;; it out so the user knows about it.
(defswitch "version" #'version-switch-demon
  "Prints the cmucl version and exits, without loading the lisp core.")

;; Make --version work for the benefit of those who are accustomed to
;; GNU software.
(defswitch "-version" #'version-switch-demon
  "Prints the cmucl version and exits; same as -version")
