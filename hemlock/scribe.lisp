;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
(in-package 'hemlock)



;;;; Variables.

(defvar *scribe-para-break-table* (make-hash-table :test #'equal)
  "A table of the Scribe commands that should be paragraph delimiters.")
;;;
(dolist (todo '("begin" "newpage" "make" "device" "caption" "tag" "end" 
		"chapter" "section" "appendix" "subsection" "paragraph"
		"unnumbered" "appendixsection" "prefacesection" "heading"
		"majorheading" "subheading")) 
  (setf (gethash todo *scribe-para-break-table*) t))

(defhvar "Open Paren Character"
  "The open bracket inserted by Scribe commands."
  :value #\[)

(defhvar "Close Paren Character"
  "The close bracket inserted by Scribe commands."
  :value #\])

(defhvar "Escape Character"
  "The escape character inserted by Scribe commands."
  :value #\@)

(defhvar "Scribe Bracket Table"
  "This table maps a Scribe brackets, open and close, to their opposing
   brackets."
  :value (make-array char-code-limit))
;;;
(mapc #'(lambda (x y)
	  (setf (svref (value scribe-bracket-table) (char-code x)) y)
	  (setf (svref (value scribe-bracket-table) (char-code y)) x))
      '(#\( #\[ #\{ #\<) '(#\) #\] #\} #\>))
;;;
(eval-when (compile eval)
  (defmacro opposing-bracket (bracket)
    `(svref (value scribe-bracket-table) (char-code ,bracket)))
) ;eval-when



;;;; "Scribe Syntax" Attribute.

(defattribute "Scribe Syntax" 
  "For Scribe Syntax, Possible types are:
  :ESCAPE           ; basically #\@.
  :OPEN-PAREN       ; Characters that open a Scribe paren:  #\[, #\{, #\(, #\<.
  :CLOSE-PAREN      ; Characters that close a Scribe paren:  #\], #\}, #\), #\>.
  :SPACE            ; Delimits end of a Scribe command.
  :NEWLINE          ; Delimits end of a Scribe command."
  'symbol nil)

(setf (character-attribute :SCRIBE-SYNTAX #\)) :CLOSE-PAREN) 
(setf (character-attribute :SCRIBE-SYNTAX #\]) :CLOSE-PAREN) 
(setf (character-attribute :SCRIBE-SYNTAX #\}) :CLOSE-PAREN) 
(setf (character-attribute :SCRIBE-SYNTAX #\>) :CLOSE-PAREN) 

(setf (character-attribute :SCRIBE-SYNTAX #\() :OPEN-PAREN)     
(setf (character-attribute :SCRIBE-SYNTAX #\[) :OPEN-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\{) :OPEN-PAREN)
(setf (character-attribute :SCRIBE-SYNTAX #\<) :OPEN-PAREN)

(setf (character-attribute :SCRIBE-SYNTAX #\Space)   :SPACE)
(setf (character-attribute :SCRIBE-SYNTAX #\Newline) :NEWLINE)
(setf (character-attribute :SCRIBE-SYNTAX #\@)       :ESCAPE)



;;;; "Scribe" mode and setup.

(defmode "Scribe" :major-p t)

(shadow-attribute :paragraph-delimiter #\@ 1 "Scribe")
(shadow-attribute :word-delimiter #\' 0 "Scribe")		;from Text Mode
(shadow-attribute :word-delimiter #\backspace 0 "Scribe")	;from Text Mode
(shadow-attribute :word-delimiter #\_ 0 "Scribe")		;from Text Mode

(define-file-type-hook ("mss") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Scribe"))



;;;; Commands.

(defcommand "Scribe Mode" (p)
  "Puts buffer in Scribe mode.  Sets up comment variables and has delimiter
   matching.  The definition of paragraphs is changed to know about scribe
   commands."
  "Puts buffer in Scribe mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Scribe"))

(defcommand "Select Scribe Warnings" (p)
  "Goes to the Scribe Warnings buffer if it exists."
  "Goes to the Scribe Warnings buffer if it exists."
  (declare (ignore p))
  (let ((buffer (getstring "Scribe Warnings" *buffer-names*)))
    (if buffer
	(change-to-buffer buffer)
	(editor-error "There is no Scribe Warnings buffer."))))

(defcommand "Add Scribe Paragraph Delimiter"
	    (p &optional
	       (word (prompt-for-string
		      :prompt "Scribe command: "
		      :help "Name of Scribe command to make delimit paragraphs."
		      :trim t)))
  "Prompts for a name to add to the table of commands that delimit paragraphs
   in Scribe mode.  If a prefix argument is supplied, then the command name is
   removed from the table."
  "Add or remove Word in the *scribe-para-break-table*, depending on P."
  (setf (gethash word *scribe-para-break-table*) (not p)))

(defcommand "List Scribe Paragraph Delimiters" (p)
  "Pops up a display of the Scribe commands that delimit paragraphs."
  "Pops up a display of the Scribe commands that delimit paragraphs."
  (declare (ignore p))
  (let (result)
    (maphash #'(lambda (k v)
		 (declare (ignore v))
		 (push k result))
	     *scribe-para-break-table*)
    (setf result (sort result #'string<))
    (with-pop-up-display (s :height (length result))
      (dolist (ele result) (write-line ele s)))))

(defcommand "Scribe Insert Bracket" (p)
  "Inserts a the bracket it is bound to and then shows the matching bracket."
  "Inserts a the bracket it is bound to and then shows the matching bracket."
  (declare (ignore p))
  (scribe-insert-paren (current-point) *last-character-typed*))


(defhvar "Scribe Command Table"
  "This is a character dispatching table indicating which Scribe command or
   environment to use."
  :value (make-hash-table)
  :mode "Scribe")

(defvar *scribe-directive-type-table*
  (make-string-table :initial-contents
		     '(("Command" . :command)
		       ("Environment" . :environment))))

(defcommand "Add Scribe Directive" (p &optional
				      (command-name nil command-name-p)
				      type key (mode "Scribe"))
  "Adds a new scribe function to put into \"Scribe Command Table\"."
  "Adds a new scribe function to put into \"Scribe Command Table\"."
  (declare (ignore p))
  (let ((command-name (if command-name-p
			  command-name
			  (or command-name
			      (prompt-for-string :help "Directive Name"
						 :prompt "Directive: ")))))
    (multiple-value-bind (ignore type)
			 (if type
			     (values nil type)
			     (prompt-for-keyword
			      (list *scribe-directive-type-table*)
			      :help "Enter Command or Environment."
			      :prompt "Command or Environment: "))
      (declare (ignore ignore))
      (let ((key (or key
		     (prompt-for-character :prompt "Dispatch Character: "))))
	(setf (gethash key (variable-value 'scribe-command-table :mode mode))
	      (cons type command-name))))))

(defcommand "Insert Scribe Directive" (p)
  "Prompts for a character to dispatch on.  Some indicate \"commands\" versus
   \"environments\".  Commands are wrapped around the previous or current word.
   If there is no previous word, the command is insert, leaving point between
   the brackets.  Environments are wrapped around the next or current
   paragraph, but when the region is active, this wraps the environment around
   the region.  Each uses \"Open Paren Character\" and \"Close Paren
   Character\"."
  "Wrap some text with some stuff."
  (declare (ignore p))
  (command-case (:bind key :prompt "Dispatch Character: ")
    (:help "help" 
	   (directive-help)
	   (reprompt))
    (t (let ((table-entry (gethash key (value scribe-command-table))))
	 (if (eq (car table-entry) :command)
	     (insert-scribe-directive (current-point) (cdr table-entry))
	     (enclose-with-environment (current-point) (cdr table-entry)))))))



;;;; "Insert Scribe Directive" support.

(defun directive-help ()
  (let ((commands ())
	(environments ()))
    (declare (list commands environments))
    (maphash #'(lambda (k v)
		 (if (eql (car v) :command)
		     (push (cons k (cdr v)) commands)
		     (push (cons k (cdr v)) environments)))
	     (variable-value 'Scribe-Command-Table :mode "Scribe"))
    (setq commands (sort commands #'string< :key #'cdr))
    (setq environments (sort environments #'string< :key #'cdr))
    (with-pop-up-display (s :height (1+ (max (length commands)
					     (length environments))))
      (format s "~2TCommands~47TEnvironments~%")
      (do ((commands commands (rest commands))
	   (environments environments (rest environments)))
	   ((and (endp commands) (endp environments)))
	(let* ((command (first commands))
	       (environment (first environments))
	       (cmd-char (first command))
	       (cmd-name (rest command))
	       (env-char (first environment))
	       (env-name (rest environment)))
	  (write-string "  " s)
	  (when cmd-char
	    (print-pretty-character cmd-char s)
	    (format s "~7T")
	    (write-string (or cmd-name "<prompts for command name>") s))
	  (when env-char
	    (format s "~47T")
	    (print-pretty-character env-char s)
	    (format s "~51T")
	    (write-string (or env-name "<prompts for command name>") s))
	  (terpri s))))))

;;; INSERT-SCRIBE-DIRECTIVE first looks for the current or previous word at
;;; mark.  Word-p says if we found one.  If mark is immediately before a word,
;;; we use that word instead of the previous.  This is because if mark
;;; corresponds to the CURRENT-POINT, the Hemlock cursor is displayed on the
;;; first character of the word making users think the mark is in the word
;;; instead of before it.  If we find a word, then we see if it already has
;;; the given command-string, and if it does, we extend the use of the command-
;;; string to the previous word.  At the end, if we hadn't found a word, we
;;; backup the mark one character to put it between the command brackets.
;;;
(defun insert-scribe-directive (mark &optional command-string)
  (with-mark ((word-start mark :left-inserting)
	      (word-end mark :left-inserting))
    (let ((open-paren-char (value open-paren-character))
	  (word-p (if (and (zerop (character-attribute
				   :word-delimiter
				   (next-character word-start)))
			   (= (character-attribute
			       :word-delimiter
			       (previous-character word-start))
			      1))
		      word-start
		      (word-offset word-start -1)))
	  (command-string (or command-string
			      (prompt-for-string
			       :trim t :prompt "Environment: "
			       :help "Name of environment to enclose with."))))
      (declare (simple-string command-string))
      (when word-p
	(word-offset (move-mark word-end word-start) 1)
	(when (test-char (next-character word-end) :scribe-syntax
			 :close-paren)
	  (with-mark ((command-start word-start)
		      (command-end word-end))
	    (balance-paren (mark-after command-end))
	    (word-offset (move-mark command-start command-end) -1)
	    (when (string= (the simple-string
				(region-to-string (region command-start
							  command-end)))
			   command-string)
	      (mark-before command-start)
	      (mark-after command-end)
	      (setf open-paren-char
		    (opposing-bracket (next-character word-end)))
	      (delete-region (region command-start command-end))
	      (delete-characters word-end)
	      (word-offset (move-mark word-start command-start) -1)))))
      (insert-character word-start (value escape-character))
      (insert-string word-start command-string)
      (insert-character word-start open-paren-char)
      (insert-character word-end (value close-paren-character))
      (unless word-p (mark-before mark)))))

(defun enclose-with-environment (mark &optional environment)
  (if (region-active-p)
      (let ((region (current-region)))
	(with-mark ((top (region-start region) :left-inserting)
		    (bottom (region-end region) :left-inserting))
	  (get-and-insert-environment top bottom environment)))
      (with-mark ((bottom-mark mark :left-inserting))
	(let ((paragraphp (paragraph-offset bottom-mark 1)))
	  (unless (or paragraphp
		      (and (last-line-p bottom-mark)
			   (end-line-p bottom-mark)
			   (not (blank-line-p (mark-line bottom-mark)))))
	    (editor-error "No paragraph to enclose."))
	  (with-mark ((top-mark bottom-mark :left-inserting))
	    (paragraph-offset top-mark -1)
	    (cond ((not (blank-line-p (mark-line top-mark)))
		   (insert-character top-mark #\Newline)
		   (mark-before top-mark))
		  (t
		   (insert-character top-mark #\Newline)))
	    (cond ((and (last-line-p bottom-mark)
			(not (blank-line-p (mark-line bottom-mark))))
		   (insert-character bottom-mark #\Newline))
		  (t
		   (insert-character bottom-mark #\Newline)
		   (mark-before bottom-mark)))
	    (get-and-insert-environment top-mark bottom-mark environment))))))

(defun get-and-insert-environment (top-mark bottom-mark environment)
  (let ((environment (or environment
			 (prompt-for-string
			  :trim t :prompt "Environment: "
			  :help "Name of environment to enclose with."))))
    (insert-environment top-mark "Begin" environment)
    (insert-environment bottom-mark "End" environment)))

(defun insert-environment (mark command environment)
  (let ((esc-char (value escape-character))
	(open-paren (value open-paren-character))
	(close-paren (value close-paren-character)))
      (insert-character mark esc-char)
      (insert-string mark command)
      (insert-character mark open-paren)
      (insert-string mark environment)
      (insert-character mark close-paren)))


(Add-Scribe-Directive-Command nil nil :Environment #\Control-\l)
(Add-Scribe-Directive-Command nil nil :Command #\Control-\w)
(Add-Scribe-Directive-Command nil "Begin" :Command #\b)
(Add-Scribe-Directive-Command nil "End" :Command #\e)
(Add-Scribe-Directive-Command nil "Center" :Environment #\c)
(Add-Scribe-Directive-Command nil "Description" :Environment #\d)
(Add-Scribe-Directive-Command nil "Display" :Environment #\Control-\d)
(Add-Scribe-Directive-Command nil "Enumerate" :Environment #\n)
(Add-Scribe-Directive-Command nil "Example" :Environment #\x)
(Add-Scribe-Directive-Command nil "FileExample" :Environment #\y)
(Add-Scribe-Directive-Command nil "FlushLeft" :Environment #\l)
(Add-Scribe-Directive-Command nil "FlushRight" :Environment #\r)
(Add-Scribe-Directive-Command nil "Format" :Environment #\f)
(Add-Scribe-Directive-Command nil "Group" :Environment #\g)
(Add-Scribe-Directive-Command nil "Itemize" :Environment #\Control-\i)
(Add-Scribe-Directive-Command nil "Multiple" :Environment #\m)
(Add-Scribe-Directive-Command nil "ProgramExample" :Environment #\p)
(Add-Scribe-Directive-Command nil "Quotation" :Environment #\q)
(Add-Scribe-Directive-Command nil "Text" :Environment #\t)
(Add-Scribe-Directive-Command nil "i" :Command #\i)
(Add-Scribe-Directive-Command nil "b" :Command #\Control-\b)
(Add-Scribe-Directive-Command nil "-" :Command #\-)
(Add-Scribe-Directive-Command nil "+" :Command #\+)
(Add-Scribe-Directive-Command nil "u" :Command #\Control-\j)
(Add-Scribe-Directive-Command nil "p" :Command #\Control-\p)
(Add-Scribe-Directive-Command nil "r" :Command #\Control-\r)
(Add-Scribe-Directive-Command nil "t" :Command #\Control-\t) 
(Add-Scribe-Directive-Command nil "g" :Command #\Control-\a)  
(Add-Scribe-Directive-Command nil "un" :Command #\Control-\n)
(Add-Scribe-Directive-Command nil "ux" :Command #\Control-\x) 
(Add-Scribe-Directive-Command nil "c" :Command #\Control-\k) 



;;;; Scribe paragraph delimiter function.

(defhvar "Paragraph Delimiter Function"
  "Scribe Mode's way of delimiting paragraphs."
  :mode "Scribe" 
  :value 'scribe-delim-para-function)

(defun scribe-delim-para-function (mark)
  "Returns whether there is a paragraph delimiting Scribe command on the
   current line.  Add or remove commands for this purpose with the command
   \"Add Scribe Paragraph Delimiter\"."
  (let ((next-char (next-character mark)))
    (when (paragraph-delimiter-attribute-p next-char)
      (if (eq (character-attribute :scribe-syntax next-char) :escape)
	  (with-mark ((begin mark)
		      (end mark))
	    (mark-after begin)
	    (if (scan-char end :scribe-syntax (or :space :newline :open-paren))
		(gethash (nstring-downcase (region-to-string (region begin end)))
			 *scribe-para-break-table*)
		(editor-error "Unable to find Scribe command ending.")))
	  t))))



;;;; Bracket matching.

(defun scribe-insert-paren (mark bracket-char)
  (insert-character mark bracket-char)
  (with-mark ((m mark))
    (if (balance-paren m)
	(when (value paren-pause-period)
	  (unless (show-mark m (current-window) (value paren-pause-period))
	    (clear-echo-area)
	    (message "~A" (line-string (mark-line m)))))
	(editor-error))))

;;; BALANCE-PAREN moves the mark to the matching open paren character, or
;;; returns nil.  The mark must be after the closing paren.
;;;
(defun balance-paren (mark)
  (with-mark ((m mark))
    (when (rev-scan-char m :scribe-syntax (or :open-paren :close-paren))
      (mark-before m)
      (let ((paren-count 1)
	    (first-paren (next-character m)))
	(loop
	  (unless (rev-scan-char m :scribe-syntax (or :open-paren :close-paren))
	    (return nil))
	  (if (test-char (previous-character m) :scribe-syntax :open-paren)
	      (setq paren-count (1- paren-count))
	      (setq paren-count (1+ paren-count)))
	  (when (< paren-count 0) (return nil))
	  (when (= paren-count 0) 
	    ;; OPPOSING-BRACKET calls VALUE (each time around the loop)
	    (cond ((char= (opposing-bracket (previous-character m)) first-paren)
		   (mark-before (move-mark mark m))
		   (return t))
		  (t (editor-error "Scribe paren mismatch."))))
	  (mark-before m))))))
