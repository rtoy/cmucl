;;; -*- Log: hemlock.log; Package: Hemlock-Internals -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Hemlock Echo Area stuff.
;;; Written by Skef Wholey and Rob MacLachlan.
;;; Modified by Bill Chiles.
;;;
(in-package 'hemlock-internals)
(export '(*echo-area-buffer* *echo-area-stream* *echo-area-window*
	  *parse-starting-mark* *parse-input-region*
	  *parse-verification-function* *parse-string-tables*
	  *parse-value-must-exist* *parse-default* *parse-default-string*
	  *parse-prompt* *parse-help* clear-echo-area message loud-message
	  prompt-for-buffer prompt-for-file prompt-for-integer
	  prompt-for-keyword prompt-for-expression prompt-for-string
	  prompt-for-variable prompt-for-yes-or-no prompt-for-y-or-n
	  prompt-for-character prompt-for-key *logical-character-names*
	  logical-char= logical-character-documentation
	  logical-character-name logical-character-characters
	  define-logical-character *parse-type* current-variable-tables))


(defmode "Echo Area" :major-p t)
(defvar *echo-area-buffer* (make-buffer "Echo Area" :modes '("Echo Area"))
  "Buffer used to hack text for the echo area.")
(defvar *echo-area-region* (buffer-region *echo-area-buffer*)
  "Internal thing that's the *echo-area-buffer*'s region.")
(defvar *echo-area-stream*
  (make-hemlock-output-stream (region-end *echo-area-region*) :full)
  "Buffered stream that prints into the echo area.")
(defvar *echo-area-window* ()
  "Window used to display stuff in the echo area.")
(defvar *parse-starting-mark*
  (copy-mark (buffer-point *echo-area-buffer*) :right-inserting)
  "Mark that points to the beginning of the text that'll be parsed.")
(defvar *parse-input-region*
  (region *parse-starting-mark* (region-end *echo-area-region*))
  "Region that contains the text typed in.")



;;;; Variables that control parsing:

(defvar *parse-verification-function* '%not-inside-a-parse
  "Function that verifies what's being parsed.")

;;; %Not-Inside-A-Parse  --  Internal
;;;
;;;    This function is called if someone does stuff in the echo area when
;;; we aren't inside a parse.  It tries to put them back in a reasonable place.
;;;
(defun %not-inside-a-parse (quaz)
  "Thing that's called when somehow we get called to confirm a parse that's
  not in progress."
  (declare (ignore quaz))
  (let* ((bufs (remove *echo-area-buffer* *buffer-list*))
	 (buf (or (find-if #'buffer-windows bufs)
		  (car bufs)
		  (make-buffer "Main"))))
    (setf (current-buffer) buf)
    (dolist (w *window-list*)
      (when (and (eq (window-buffer w) *echo-area-buffer*)
		 (not (eq w *echo-area-window*)))
	(setf (window-buffer w) buf)))
    (setf (current-window)
	  (or (car (buffer-windows buf))
	      (make-window (buffer-start-mark buf)))))
  (editor-error "Wham!  We tried to confirm a parse that wasn't in progress?"))

(defvar *parse-string-tables* ()
  "String tables being used in the current parse.")

(defvar *parse-value-must-exist* ()
  "You know.")

(defvar *parse-default* ()
  "When the user attempts to default a parse, we call the verification function
  on this string.  This is not the :Default argument to the prompting function,
  but rather a string representation of it.")

(defvar *parse-default-string* ()
  "String that we show the user to inform him of the default.  If this
  is NIL then we just use *Parse-Default*.")

(defvar *parse-prompt* ()
  "Prompt for the current parse.")

(defvar *parse-help* ()
  "Help string for the current parse.")

(defvar *parse-type* :string "A hack. :String, :File or :Keyword.") 



;;;; MESSAGE and CLEAR-ECHO-AREA:

(defhvar "Message Pause" "The number of seconds to pause after a Message."
  :value 0.5s0)

(defvar *last-message-time* 0
  "Internal-Real-Time the last time we displayed a message.") 

(defun maybe-wait ()
  (let* ((now (get-internal-real-time))
	 (delta (/ (float (- now *last-message-time*))
		   (float internal-time-units-per-second)))
	 (pause (value ed::message-pause)))
    (when (< delta pause)
      (sleep (- pause delta)))))

(defun clear-echo-area ()
  "You guessed it."
  (maybe-wait)
  (delete-region *echo-area-region*)
  (setf (buffer-modified *echo-area-buffer*) nil))

;;; Message  --  Public
;;;
;;;    Display the stuff on *echo-area-stream* and then wait.  Editor-Sleep
;;; will do a redisplay if appropriate.
;;;
(defun message (string &rest args)
  "Nicely display a message in the echo-area.
  Put the message on a fresh line and wait for \"Message Pause\" seconds
  to give the luser a chance to see it.  String and Args are a format 
  control string and format arguments, respectively."
  (maybe-wait)
  (cond ((eq *current-window* *echo-area-window*)
	 (let ((point (buffer-point *echo-area-buffer*)))
	   (with-mark ((m point :left-inserting))
	     (line-start m)
	     (with-output-to-mark (s m :full)
	       (apply #'format s string args)
	       (fresh-line s)))))
	(t
	 (let ((mark (region-end *echo-area-region*)))
	   (cond ((buffer-modified *echo-area-buffer*)
		  (clear-echo-area))
		 ((not (zerop (mark-charpos mark)))
		  (insert-character mark #\newline)
		  (unless (displayed-p mark *echo-area-window*)
		    (clear-echo-area))))
	   (apply #'format *echo-area-stream* string args)
	   (setf (buffer-modified *echo-area-buffer*) nil))))
  (force-output *echo-area-stream*)
  (setq *last-message-time* (get-internal-real-time))
  nil)


;;; LOUD-MESSAGE -- Public.
;;;    Like message, only more provocative.
;;;
(defun loud-message (&rest args)
  "This is the same as MESSAGE, but it beeps and clears the echo area before
   doing anything else."
  (beep)
  (clear-echo-area)
  (apply #'message args))



;;;; DISPLAY-PROMPT-NICELY and PARSE-FOR-SOMETHING.

(defun display-prompt-nicely (&optional (prompt *parse-prompt*)
					(default (or *parse-default-string*
						     *parse-default*)))
  (clear-echo-area)
  (let ((point (buffer-point *echo-area-buffer*)))
    (if (listp prompt)
	(apply #'format *echo-area-stream* prompt)
	(insert-string point prompt))
    (when default
      (insert-character point #\[)
      (insert-string point default)
      (insert-string point "] "))))

(defun parse-for-something ()
  (display-prompt-nicely)
  (let ((start-window (current-window)))
    (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
    (setf (current-window) *echo-area-window*)
    (unwind-protect
     (use-buffer *echo-area-buffer*
       (recursive-edit nil))
     (setf (current-window) start-window))))



;;;; Buffer prompting.

(defun prompt-for-buffer (&key ((:must-exist *parse-value-must-exist*) t)
			       default
			       ((:default-string *parse-default-string*))
			       ((:prompt *parse-prompt*) "Buffer: ")
			       ((:help *parse-help*) "Type a buffer name."))
  "Prompts for a buffer name and returns the corresponding buffer.  If
   :must-exist is nil, then return the input string.  This refuses to accept
   the empty string as input when no default is supplied.  :default-string
   may be used to supply a default buffer name even when :default is nil, but
   when :must-exist is non-nil, :default-string must be the name of an existing
   buffer."
    (let ((*parse-string-tables* (list *buffer-names*))
	  (*parse-type* :keyword)
	  (*parse-default* (cond
			    (default (buffer-name default))
			    (*parse-default-string*
			     (when (and *parse-value-must-exist*
					(not (getstring *parse-default-string*
							*buffer-names*)))
			       (error "Default-string must name an existing ~
				       buffer when must-exist is non-nil -- ~S."
				      *parse-default-string*))
			     *parse-default-string*)
			    (t nil)))
	  (*parse-verification-function* #'buffer-verification-function))
      (parse-for-something)))

(defun buffer-verification-function (string)
  (declare (simple-string string))
  (cond ((string= string "") nil)
	(*parse-value-must-exist*
	 (multiple-value-bind
	     (prefix key value field ambig)
	     (complete-string string *parse-string-tables*)
	   (declare (ignore field))
	   (ecase key
	     (:none nil)
	     ((:unique :complete)
	      (list value))
	     (:ambiguous
	      (delete-region *parse-input-region*)
	      (insert-string (region-start *parse-input-region*) prefix)
	      (let ((point (current-point)))
		(move-mark point (region-start *parse-input-region*))
		(unless (character-offset point ambig)
		  (buffer-end point)))
	      nil))))
	(t
	 (list (or (getstring string *buffer-names*) string)))))



;;;; File Prompting.

(defun prompt-for-file (&key ((:must-exist *parse-value-must-exist*) t)
			     default
			     ((:default-string *parse-default-string*))
			     ((:prompt *parse-prompt*) "Filename: ")
			     ((:help *parse-help*) "Type a file name."))
  "Prompts for a filename."
  (let ((*parse-verification-function* #'file-verification-function)
	(*parse-default* (if default (namestring default)))
	(*parse-type* :file))
    (parse-for-something)))

(defun file-verification-function (string)
  (let ((pn (pathname-or-lose string)))
    (if pn
	(let ((merge
	       (cond ((not *parse-default*) nil)
		     ((directoryp pn)
		      (merge-pathnames pn *parse-default*))
		     (t
		      (merge-pathnames
		       (prompting-merge-pathnames (directory-namestring pn)
						  (directory-namestring
						   *parse-default*))
		       (file-namestring pn))))))
	  (cond ((probe-file pn) (list pn))
		((and merge (probe-file merge)) (list merge))
		((not *parse-value-must-exist*) (list (or merge pn)))
		(t nil))))))

(defun prompting-merge-pathnames (pathname default-directory)
  "Merges pathname with default-directory.  If pathname is not absolute, it
   is assumed to be relative to default-directory.  The result is always a
   directory.  This works even when pathname is a logical name."
  (if (and pathname (string/= (namestring pathname) ""))
      (let ((pathname (pathname pathname))
	    (device (pathname-device pathname)))
	(if (and device
		 (not (eq device :absolute))
		 (not (string= device "Default")))
	    pathname
	    (merge-relative-pathnames pathname default-directory)))
      default-directory))

;;; PATHNAME-OR-LOSE tries to convert string to a pathname using
;;; PARSE-NAMESTRING.  If it succeeds, this returns the pathname.  Otherwise,
;;; this deletes the offending characters from *parse-input-region* and signals
;;; an editor-error.
;;;
(defun pathname-or-lose (string)
  (declare (simple-string string))
  (multiple-value-bind (pn idx)
		       (parse-namestring string nil *default-pathname-defaults*
					 :junk-allowed t)
    (cond (pn)
	  (t (delete-characters (region-end *echo-area-region*)
				(- idx (length string)))
	     nil))))



;;;; Keyword and variable prompting.

(defun prompt-for-keyword (*parse-string-tables* 
			   &key
			   ((:must-exist *parse-value-must-exist*) t)
			   ((:default *parse-default*))
			   ((:default-string *parse-default-string*))
			   ((:prompt *parse-prompt*) "Keyword: ")
			   ((:help *parse-help*) "Type a keyword."))
  "Prompts for a keyword using the String Tables."
  (let ((*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword))
    (parse-for-something)))

(defun prompt-for-variable (&key ((:must-exist *parse-value-must-exist*) t)
				 ((:default *parse-default*))
				 ((:default-string *parse-default-string*))
				 ((:prompt *parse-prompt*) "Variable: ")
				 ((:help *parse-help*)
				  "Type the name of a variable."))
  "Prompts for a variable defined in the current scheme of things."
  (let ((*parse-string-tables* (current-variable-tables))
	(*parse-verification-function* #'keyword-verification-function)
	(*parse-type* :keyword))
    (parse-for-something)))

(defun current-variable-tables ()
  "Returns a list of all the variable tables currently established globally,
   by the current buffer, and by any modes for the current buffer."
  (do ((tables (list (buffer-variables *current-buffer*)
		     *global-variable-names*)
	       (cons (hi::mode-object-variables (car mode)) tables))
       (mode (buffer-mode-objects *current-buffer*) (cdr mode)))
      ((null mode) tables)))

(defun keyword-verification-function (string)
  (declare (simple-string string))
  (multiple-value-bind
      (prefix key value field ambig)
      (complete-string string *parse-string-tables*)
    (declare (ignore field))
    (cond (*parse-value-must-exist*
	   (ecase key
	     (:none nil)
	     ((:unique :complete)
	      (list prefix value))
	     (:ambiguous
	      (delete-region *parse-input-region*)
	      (insert-string (region-start *parse-input-region*) prefix)
	      (let ((point (current-point)))
		(move-mark point (region-start *parse-input-region*))
		(unless (character-offset point ambig)
		  (buffer-end point)))
	      nil)))
	  (t
	   ;; HACK: If it doesn't have to exist, and the completion does not
	   ;; add anything, then return the completion's capitalization,
	   ;; instead of the user's input.
	   (list (if (= (length string) (length prefix)) prefix string))))))



;;;; Integer, expression, and string prompting.

(defun prompt-for-integer (&key ((:must-exist *parse-value-must-exist*) t)
				default
				((:default-string *parse-default-string*))
				((:prompt *parse-prompt*) "Integer: ")
				((:help *parse-help*) "Type an integer."))
  "Prompt for an integer.  If :must-exist is Nil, then we return as a string
  whatever was input if it is not a valid integer."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (let ((number (parse-integer string  :junk-allowed t)))
	       (if *parse-value-must-exist*
		   (if number (list number))
		   (list (or number string))))))
	(*parse-default* (if default (write-to-string default :base 10))))
    (parse-for-something)))


(defvar hemlock-eof '(())
  "An object that won't be EQ to anything read.")

(defun prompt-for-expression (&key ((:must-exist *parse-value-must-exist*) t)
				   (default nil defaultp)
				   ((:default-string *parse-default-string*))
				   ((:prompt *parse-prompt*) "Expression: ")
				   ((:help *parse-help*)
				    "Type a Lisp expression."))
  "Prompts for a Lisp expression."
  (let ((*parse-verification-function*
         #'(lambda (string)
	     (let ((expr (with-input-from-region (stream *parse-input-region*)
			   (handler-case (read stream nil hemlock-eof)
			     (error () hemlock-eof)))))
	       (if *parse-value-must-exist*
		   (if (not (eq expr hemlock-eof)) (values (list expr) t))
		   (if (eq expr hemlock-eof)
		       (list string) (values (list expr) t))))))
	(*parse-default* (if defaultp (prin1-to-string default))))
      (parse-for-something)))


(defun prompt-for-string (&key ((:default *parse-default*))
			       ((:default-string *parse-default-string*))
			       (trim ())
			       ((:prompt *parse-prompt*) "String: ")
			       ((:help *parse-help*) "Type a string."))
  "Prompts for a string.  If :trim is t, then leading and trailing whitespace
   is removed from input, otherwise it is interpreted as a Char-Bag argument
   to String-Trim."
  (let ((*parse-verification-function*
	 #'(lambda (string)
	     (list (string-trim (if (eq trim t) '(#\space #\tab) trim)
				string)))))
    (parse-for-something)))



;;;; Yes-or-no and y-or-n prompting.

(defvar *yes-or-no-string-table*
  (make-string-table :initial-contents '(("Yes" . t) ("No" . nil))))

(defun prompt-for-yes-or-no (&key ((:must-exist *parse-value-must-exist*) t)
				  (default nil defaultp)
				  ((:default-string *parse-default-string*))
				  ((:prompt *parse-prompt*) "Yes or No? ")
				  ((:help *parse-help*) "Type Yes or No."))
  "Prompts for Yes or No."
  (let* ((*parse-string-tables* (list *yes-or-no-string-table*))
	 (*parse-default* (if defaultp (if default "Yes" "No")))
	 (*parse-verification-function*
	  #'(lambda (string)
	      (multiple-value-bind
		  (prefix key value field ambig)
		  (complete-string string *parse-string-tables*)
		(declare (ignore prefix field ambig))
		(let ((won (or (eq key :complete) (eq key :unique))))
		  (if *parse-value-must-exist*
		      (if won (values (list value) t))
		      (list (if won (values value t) string)))))))
	 (*parse-type* :keyword))
    (parse-for-something)))

(defun prompt-for-y-or-n (&key ((:must-exist must-exist) t)
			       (default nil defaultp)
			       default-string
			       ((:prompt prompt) "Y or N? ")
			       ((:help *parse-help*) "Type Y or N."))
  "Prompts for Y or N."
  (let ((old-window (current-window)))
    (unwind-protect
      (progn
       (setf (current-window) *echo-area-window*)
       (display-prompt-nicely prompt (or default-string
					 (if defaultp (if default "Y" "N"))))
       (do ((char (read-char *editor-input*) (read-char *editor-input*)))
	   (())
	 (cond ((or (char= char #\y) (char= char #\Y))
		(return t))
	       ((or (char= char #\n) (char= char #\N))
		(return nil))
	       ((logical-char= char :confirm)
		(if defaultp
		    (return default)
		    (beep)))
	       ((logical-char= char :help)
		(ed::help-on-parse-command ()))
	       (t
		(unless must-exist (return char))
		(beep)))))
      (setf (current-window) old-window))))



;;;; Character and key prompting.

(defun prompt-for-character (&key (prompt "Character: ") (change-window t))
  "Prompts for a character."
  (prompt-for-character* prompt change-window))

(defun prompt-for-character* (prompt change-window)
  (let ((old-window (current-window)))
    (unwind-protect
      (progn
       (when change-window
	 (setf (current-window) *echo-area-window*))
       (display-prompt-nicely prompt)
       (read-char *editor-input* nil))
      (when change-window (setf (current-window) old-window)))))

(defvar *prompt-key* (make-array 10 :adjustable t :fill-pointer 0))
(defun prompt-for-key (&key ((:must-exist must-exist) t)
			    default default-string
			    (prompt "Key: ")
			    ((:help *parse-help*) "Type a key."))
  (let ((old-window (current-window))
	(string (if default
		    (or default-string
			(let ((l (coerce default 'list)))
			  (format nil "~:C~{ ~:C~}" (car l) (cdr l)))))))

    (unwind-protect
      (progn
       (setf (current-window) *echo-area-window*)
       (display-prompt-nicely prompt string)
       (setf (fill-pointer *prompt-key*) 0)
       (prog ((key *prompt-key*) char)
	 (declare (vector key))
	TOP
	 (setq char (read-char *editor-input*))
	 (cond ((logical-char= char :quote)
		(setq char (read-char *editor-input* nil)))
	       ((logical-char= char :confirm)
		(cond ((and default (zerop (length key)))
		       (let ((res (get-command default :current)))
			 (unless (commandp res) (go FLAME))
			 (return (values default res))))
		      ((and (not must-exist) (plusp (length key)))
		       (return (copy-seq key)))
		      (t 
		       (go FLAME))))
	       ((logical-char= char :help)
		(ed::help-on-parse-command ())
		(go TOP)))
	 (vector-push-extend char key)	 
	 (when must-exist
	   (let ((res (get-command key :current)))
	     (cond ((commandp res) 
		    (format *echo-area-stream* "~:C " char)
		    (return (values (copy-seq key) res)))
		   ((not (eq res :prefix))
		    (vector-pop key)
		    (go FLAME)))))
	 (format *echo-area-stream* "~:C " char)
	 (go TOP)
	FLAME
	 (beep)
	 (go TOP)))
      (force-output *echo-area-stream*)
      (setf (current-window) old-window))))



;;;; Logical character stuff.

(defvar *logical-character-names* (make-string-table)
  "This variable holds a string-table from logical-character names to the
  corresponding keywords.")

(defvar *real-to-logical-characters* (make-hash-table :test #'eql)
  "A hashtable from real characters to their corresponding logical
  character keywords.")

(defvar *logical-character-descriptors* (make-hash-table :test #'eq)
  "A hashtable from logical-characters to logical-character-descriptors.")

(defstruct (logical-character-descriptor
	    (:constructor make-logical-character-descriptor ()))
  name
  characters
  documentation)

;;; Logical-Char=  --  Public
;;;
;;;    Just look up the character in the hashtable.
;;;
(defun logical-char= (character keyword)
  "Return true if Character has been defined to have Keyword as its
  logical character.  The relation between logical and real characters
  is defined by using Setf on Logical-Char=.  If it is set to
  true then calling Logical-Char= with the same Character and
  Keyword, will result in truth.  Setting to false produces the opposite
  result.  See Define-Logical-Character and Command-Case."
  (not (null (memq keyword (gethash (char-upcase character)
				    *real-to-logical-characters*)))))

;;; Get-Logical-Char-Desc  --  Internal
;;;
;;;    Return the descriptor for the logical character Kwd, or signal
;;; an error if it isn't defined.
;;;
(defun get-logical-char-desc (kwd)
  (let ((res (gethash kwd *logical-character-descriptors*)))
    (unless res
      (error "~S is not a defined logical-character keyword." kwd))
    res))

;;; %Set-Logical-Char=  --  Internal
;;;
;;;    Add or remove a logical character link by adding to or deleting from
;;; the list in the from-char hashtable and the descriptor.
;;;
(defun %set-logical-char= (character keyword new-value)
  (let* ((character (char-upcase character))
	 (entry (get-logical-char-desc keyword)))
    (cond
     (new-value
      (pushnew keyword (gethash character *real-to-logical-characters*))
      (pushnew character (logical-character-descriptor-characters entry)))
     (t
      (setf (gethash character *real-to-logical-characters*)
	    (delete keyword (gethash character *real-to-logical-characters*)))
      (setf (logical-character-descriptor-characters entry)
	    (delete keyword (logical-character-descriptor-characters entry))))))
  new-value)

;;; Logical-Character-Documentation, Name, Characters  --  Public
;;;
;;;    Grab the right field out of the descriptor and return it.
;;;
(defun logical-character-documentation (keyword)
  "Return the documentation for the logical character Keyword."
  (logical-character-descriptor-documentation (get-logical-char-desc keyword)))
;;;
(defun logical-character-name (keyword)
  "Return the string name for the logical character Keyword."
  (logical-character-descriptor-name (get-logical-char-desc keyword)))
;;;
(defun logical-character-characters (keyword)
  "Return the list of characters for which Keyword is the logical character."
  (logical-character-descriptor-characters (get-logical-char-desc keyword)))

;;; Define-Logical-Character  --  Public
;;;
;;;    Make the entries in the two hashtables and the string-table.
;;;
(defun define-logical-character (name documentation)
  "Define a logical character having the specified Name and Documentation.
  See Logical-Char= and Command-Case."
  (check-type name string)
  (check-type documentation (or string function))
  (let* ((keyword (string-to-keyword name))
	 (entry (or (gethash keyword *logical-character-descriptors*)
		    (setf (gethash keyword *logical-character-descriptors*)
			  (make-logical-character-descriptor)))))
    (setf (logical-character-descriptor-name entry) name)
    (setf (logical-character-descriptor-documentation entry) documentation)
    (setf (getstring name *logical-character-names*) keyword)))



;;;; Some standard logical-characters:

(define-logical-character "Forward Search"
  "This character is used to indicate that a forward search should be made.")
(define-logical-character "Backward Search"
  "This character is used to indicate that a backward search should be made.")
(define-logical-character "Recursive Edit"
  "This character indicates that a recursive edit should be entered.")
(define-logical-character "Cancel"
  "This character is used  to cancel a previous character of input.")
(define-logical-character "Abort"
  "This character is used to abort the command in progress.")
(define-logical-character "Exit"
  "This character is used to exit normally the command in progress.")
(define-logical-character "Yes"
  "This character is used to indicate a positive response.")
(define-logical-character "No"
  "This character is used to indicate a negative response.")
(define-logical-character "Do All"
  "This character means do it as many times as you can.")
(define-logical-character "Do Once"
  "This character means, do it this time, then exit.")
(define-logical-character "Help"
  "This character is used to ask for help.")
(define-logical-character "Confirm"
  "This character is used to confirm some choice.")
(define-logical-character "Quote"
  "This character is used to quote the next character of input.")
(define-logical-character "Keep"
  "This character means exit but keep something around.")



;;;; COMMAND-CASE help message printing.

(defvar *my-string-output-stream* (make-string-output-stream))

(defun chars-to-string (chars)
  (do ((s *my-string-output-stream*)
       (chars chars (cdr chars)))
      ((null chars)
       (get-output-stream-string s))
    (let ((char (car chars)))
      (if (characterp char)
	  (print-pretty-character char s)
	  (do ((chars (logical-character-characters char) (cdr chars)))
	      ((null chars))
	    (print-pretty-character (car chars) s)
	    (unless (null (cdr chars))
	      (write-string ", " s))))
      (unless (null (cdr chars))
	(write-string ", " s)))))

;;; Command-Case-Help  --  Internal
;;;
;;;    Print out a help message derived from the options in a
;;; random-typeout window.
;;;
(defun command-case-help (help options)
  (let ((help (if (listp help)
		  (apply #'format nil help) help)))
    (with-pop-up-display (s)
      (write-string help s)
      (fresh-line s)
      (do ((o options (cdr o)))
	  ((null o))
	(let ((string (chars-to-string (caar o))))
	  (declare (simple-string string))
	  (cond ((= (length string) 1)
		 (write-char (char string 0) s)
		 (write-string "  - " s)
		 (write-line (cdar o) s))
		(t
		 (write-line string s)
		 (write-string "   - " s)
		 (write-line (cdar o) s))))))))
