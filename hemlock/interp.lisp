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
;;;    Written by Rob MacLachlan
;;;
;;; This file contains the routines which define hemlock commands and
;;; the command interpreter.
;;;

(in-package "HEMLOCK-INTERNALS")

(export '(bind-key delete-key-binding get-command map-bindings
	  make-command command-name command-bindings last-command-type
	  prefix-argument exit-hemlock *invoke-hook* key-translation))


(defun %print-hcommand (obj stream depth)
  (declare (ignore depth))
  (write-string "#<Hemlock Command \"" stream)
  (write-string (command-name obj) stream)
  (write-string "\">" stream))



;;;; Key Tables:
;;;
;;;    A key table provides a way to translate a sequence of characters to some
;;; lisp object.  It is currently represented by a tree of vectors, with
;;; alternating levels being indexed by the bits and code.  We wrap a Key-Table
;;; structure around the table so that we can discriminate bewteen a key table
;;; and a value.
;;; 

(defstruct (key-table
	    (:print-function
	     (lambda (x stream y)
	       (declare (ignore x y))
	       (write-string "#<Key-Table>" stream))))
  (table (make-array command-char-bits-limit :initial-element nil)
	 :type simple-vector)) 


;;; GET-TABLE-ENTRY  --  Internal
;;;
;;;   Return the value found by walking down a tree of command tables as
;;; specified by a key.  If no such entry return NIL.
;;;
(defun get-table-entry (table key)
  (let ((current table))
    (dotimes (i (length key) current)
      (unless (key-table-p current) (return nil))
      (let* ((char (aref key i))
	     (bits-vec (key-table-table current))
	     (code-vec (svref bits-vec (key-char-bits char))))
	(unless code-vec (return nil))
	(setq current (svref code-vec (key-char-code char)))))))


;;; SET-TABLE-ENTRY  --  Internal
;;;
;;;    Set the entry for Key in Table to Val, creating new key tables as
;;; needed.
;;;
(defun set-table-entry (table key val)
  (do ((keylast (1- (length key)))
       (index 0 (1+ index))
       (current table))
      (())
    (let* ((char (aref key index))
	   (bits (key-char-bits char))
	   (code (key-char-code char))
	   (bits-vec (key-table-table current))
	   (code-vec (or (svref bits-vec bits)
			 (setf (svref bits-vec bits)
			       (make-array command-char-code-limit
					   :initial-element nil))))
	   (next (svref code-vec code)))
      (cond ((= index keylast)
	     (setf (svref code-vec code) val)
	     (return val))
	    ((key-table-p next)
	     (setq current next))
	    (t
	     (setq current (make-key-table))
	     (setf (svref code-vec code) current))))))


;;;; Key Translation:
;;;
;;;    Key translations are maintained using a key table.  If a value is an
;;; integer, then it is prefix bits to be OR'ed with the next character.  If it
;;; is a key, then we translate to that key.

(defvar *key-translations* (make-key-table))
(defvar *translate-key-temp* (make-array 10 :fill-pointer 0 :adjustable t))


;;; TRANSLATE-KEY  --  Internal
;;;
;;;    This is used internally to do key translations when we want the
;;; canonical representation for Key.  Result, if supplied, is an adjustable
;;; vector with a fill pointer.  We compute the output in this vector.  If the
;;; key ends in the prefix of a translation, we just return that part
;;; untranslated and return the second value true.
;;;
(defun translate-key (key &optional (result (make-array 4 :fill-pointer 0
							:adjustable t)))
  (let ((key-len (length key))
	(temp *translate-key-temp*)
	(start 0)
	(try-pos 0)
	(prefix 0))
    (setf (fill-pointer temp) 0)
    (setf (fill-pointer result) 0)
    (loop
      (when (= try-pos key-len) (return))
      (let ((ch (aref key try-pos)))
	(vector-push-extend (make-char ch (logior (char-bits ch) prefix))
			    temp)
	(setq prefix 0))
      (let ((entry (get-table-entry *key-translations* temp)))
	(unless (key-table-p entry)
	  (etypecase entry
	    (null
	     (vector-push-extend (aref temp 0) result)
	     (incf start))
	    (simple-vector
	     (dotimes (i (length entry))
	       (vector-push-extend (aref entry i) result))
	     (setq start (1+ try-pos)))
	    (integer
	     (setq start (1+ try-pos))
	     (when (= start key-len) (return))
	     (setq prefix (logior entry prefix))))
	  (setq try-pos start)
	  (setf (fill-pointer temp) 0))))

    (dotimes (i (length temp))
      (vector-push-extend (aref temp i) result))
    (values result (not (zerop (length temp))))))


;;; KEY-TRANSLATION  --  Public
;;;
;;;    Set the value, dealing with translating to and from symbolic bit names. 
;;;
(defun key-translation (key)
  "Return the key translation for Key, or NIL if there is none.  If Key is a
  prefix of a translation, then :Prefix is returned.  Whenever Key appears as a
  subsequence of a key argument to the binding manipulation functions, that
  portion will be replaced with the translation.  A key translation may also be
  a list (:Bits {Bit-Name}*).  In this case, the named bits will be set in the
  next character in the key being translated."
  (let ((entry (get-table-entry *key-translations* (crunch-key key))))
    (etypecase entry
      (key-table :prefix)
      ((or simple-vector null) entry)
      (integer
       (let ((ch (make-char #\? entry))
	     (res ()))
	 (dolist (bit all-bit-names)
	   (when (char-bit ch bit)
	     (push bit res)))
	 (cons :bits res))))))

(defsetf key-translation %set-key-translation
  "Set the key translation for a key.  If set to null, deletes any
  translation.")

;;; %SET-KEY-TRANSLATION  --  Internal
;;;
;;;    Setf inverse for Key-Translation.
;;;
(defun %set-key-translation (key new-value)
  (let ((entry (cond ((and (consp new-value) (eq (first new-value) :bits))
		      (let ((res #\?))
			(dolist (bit (rest new-value) (char-bits res))
			  (setf (char-bit res bit) t))))
		     ((null new-value) new-value)
		     (t
		      (crunch-key new-value)))))
    (set-table-entry *key-translations* (crunch-key key) entry)
    new-value))


;;;; Interface Utility Functions:

(defvar *global-command-table* (make-key-table)
  "The command table for global key bindings.")

;;; GET-RIGHT-TABLE  --  Internal
;;;
;;;    Return a key-table depending on "kind" and checking for errors.
;;;
(defun get-right-table (kind where)
  (case kind
     (:global
      (when where
	(error "Where argument ~S is meaningless for :global bindings."
	       where))
      *global-command-table*)
     (:mode (let ((mode (getstring where *mode-names*)))
	      (unless mode
		(error "~S is not a defined mode." where))
	      (mode-object-bindings mode)))
     (:buffer (unless (bufferp where)
		(error "~S is not a buffer." where))
	      (buffer-bindings where))
     (t (error "~S is not a valid binding type." kind))))


;;; CRUNCH-KEY  --  Internal
;;;
;;;    Take a key in one of the various specifications and turn it
;;; into the standard one: a simple-vector of characters.
;;;
(defun crunch-key (key)
  (typecase key
    (character (vector key))
    ((or list vector)
     (when (zerop (length key))
       (error "Zero length key is illegal."))
     (unless (every #'characterp key)
       (error "Key ~S has a non-character element." key))
     (coerce key 'simple-vector))
    (t
     (error "Key ~S is not a character or sequence." key))))


;;;; Exported Primitives:

(proclaim '(special *command-names*))

;;; BIND-KEY  --  Public
;;;
;;;    Put the command specified in the correct key table.
;;;
(defun bind-key (name key &optional (kind :global) where)
  "Bind a Hemlock command to some key somewhere.  Name is the string name
   of a Hemlock command, Key is either a character or a vector of characters.
   Kind is one of :Global, :Mode or :Buffer, Where is the mode name or buffer
   concerned.  Kind defaults to :Global."
  (let ((cmd (getstring name *command-names*))
	(table (get-right-table kind where))
	(key (copy-seq (translate-key (crunch-key key)))))
    (cond (cmd
	   (set-table-entry table key cmd)
	   (push (list key kind where) (command-%bindings cmd))
	   cmd)
	  (t
	   (with-simple-restart (continue "Go on, ignoring binding attempt.")
	     (error "~S is not a defined command." name))))))


;;; DELETE-KEY-BINDING  --  Public
;;;
;;;    Stick NIL in the key table specified.
;;;
(defun delete-key-binding (key &optional (kind :global) where)
  "Remove a Hemlock key binding somewhere.  Name is the string name of
  a Hemlock command, Key is either a character or a vector of
  characters.  Kind is one of :Global, :Mode or :Buffer, Where is the
  mode name or buffer concerned.  Kind defaults to :Global."
  (set-table-entry (get-right-table kind where)
		   (translate-key (crunch-key key))
		   nil))


;;; GET-CURRENT-BINDING  --  Internal
;;;
;;;    Look up a key in the current environment.
;;;
(defun get-current-binding (key)
  (let ((res (get-table-entry (buffer-bindings *current-buffer*) key)))
    (cond
     (res (values res nil))
     (t
      (do ((mode (buffer-mode-objects *current-buffer*) (cdr mode))
	   (t-bindings ()))
	  ((null mode)
	   (values (get-table-entry *global-command-table* key)
		   (nreverse t-bindings)))
	(declare (list t-bindings))
	(let ((res (get-table-entry (mode-object-bindings (car mode)) key)))
	  (when res
	    (if (mode-object-transparent-p (car mode))
		(push res t-bindings)
		(return (values res (nreverse t-bindings)))))))))))


;;; GET-COMMAND  --  Public
;;;
;;;    Look up the key binding, checking for :Prefix.
;;;
(defun get-command (key &optional (kind :global) where)
  "Return the command object for the command bound to key somewhere.
  If key is not bound return NIL, if Key is a prefix of a key-binding
  then reutrn :Prefix.  Name is the string name of a Hemlock command,
  Key is either a character or a vector of characters.  Kind is one of
  :Global, :Mode or :Buffer, Where is the mode name or buffer
  concerned.  Kind defaults to :Global."
  (multiple-value-bind (key prefix-p)
		       (translate-key (crunch-key key))
    (let ((entry (if (eq kind :current)
		     (get-current-binding key)
		     (get-table-entry (get-right-table kind where) key))))
      (etypecase entry
	(null (if prefix-p :prefix nil))
	(command entry)
	(key-table :prefix)))))


;;; MAP-BINDINGS  --  Public
;;;
;;;    map over a key table.
;;;
(defun map-bindings (fun kind &optional where)
  "Map Fun over the bindings in some place.  The function is passed the
  Key and the command to which it is bound."
  (sub-map-bindings fun (key-table-table (get-right-table kind where)) '#()))
;;;
(defun sub-map-bindings (fun tab key)
  (declare (simple-vector tab key))
  (let ((key (concatenate 'simple-vector key '#(#\space)))
	(index (length key)))
    (dotimes (bits command-char-bits-limit)
      (let ((vec (svref tab bits)))
	(when vec
	  (dotimes (code command-char-code-limit)
	    (setf (svref key index) (code-char code bits))
	    (let ((val (svref vec code)))
	      (cond ((null val))
		    ((commandp val)
		     (funcall fun key val))
		    (t
		     (sub-map-bindings fun (key-table-table val)
				       key))))))))))


;;; MAKE-COMMAND  --  Public
;;;
;;;    If the command is already defined then alter the command object, 
;;; otherwise make a new command object and enter it into the 
;;; *command-names*.
;;;
(defun make-command (name documentation function)
  "Create a new Hemlock command with Name and Documentation which is
  implemented by calling the function-value of the symbol Function"
  (let ((entry (getstring name *command-names*)))
    (cond
     (entry
      (setf (command-name entry) name)
      (setf (command-documentation entry) documentation)
      (setf (command-function entry) function))
     (t
      (setf (getstring name *command-names*)
	    (internal-make-command name documentation function))))))


;;; COMMAND-NAME, %SET-COMMAND-NAME  --  Public
;;;
;;;    Filter the slot, updating *command-names* if it is set. 
;;;
(defun command-name (command)
  "Returns the string which is the name of Command."
  (command-%name command))
;;;
(defun %set-command-name (command new-name)
  (check-type command command)
  (check-type new-name string)
  (setq new-name (coerce new-name 'simple-string))
  (delete-string (command-%name command) *command-names*)
  (setf (getstring new-name *command-names*) command)
  (setf (command-%name command) new-name))


;;; COMMAND-BINDINGS  --  Public
;;;
;;;    Check that all the supposed bindings really exists.  Bindings which
;;; were once made may have been overwritten.  It is easier to filter
;;; out bogus bindings here than to catch all the cases that can make a
;;; binding go away.
;;;
(defun binding= (b1 b2)
  (and (eq (second b1) (second b2))
       (equal (third b1) (third b2))
       (let* ((k1 (first b2))
	      (l1 (length k1))
	      (k2 (first b1)))
	 (declare (simple-vector k1 k2))
	 (if (= l1 (length k2))
	     (dotimes (i l1 t)
	       (when (char/= (svref k1 i) (svref k2 i))
		 (return nil)))))))
;;;
(defun command-bindings (command)
  "Return a list of lists of the form (key kind where) describing
  all the places there Command is bound."
  (check-type command command)
  (let (res)
    (declare (list res))
    (dolist (place (command-%bindings command))
      (let ((tab (case (cadr place)
		   (:global *global-command-table*)
		   (:mode
		    (let ((m (getstring (caddr place) *mode-names*)))
		      (when m (mode-object-bindings m))))
		   (t
		    (when (memq (caddr place) *buffer-list*)
		      (buffer-bindings (caddr place)))))))
	(when (and tab (eq (get-table-entry tab (car place)) command)
		   (not (find place res :test #'binding=)))
	  (push place res))))
    res))


(defvar *last-command-type* ()
  "The command-type of the last command invoked.")
(defvar *command-type-set* ()
  "True if the last command set the command-type.")

;;; LAST-COMMAND-TYPE  --  Public
;;;
;;;
(defun last-command-type ()
  "Return the command-type of the last command invoked.
  If no command-type has been set then return NIL.  Setting this with
  Setf sets the value for the next command."
  *last-command-type*)

;;; %SET-LAST-COMMAND-TYPE  --  Internal
;;;
;;;    Set the flag so we know not to clear the command-type.
;;;
(defun %set-last-command-type (type)
  (setq *last-command-type* type *command-type-set* t))


(defvar *prefix-argument* nil "The prefix argument or NIL.")
(defvar *prefix-argument-supplied* nil
  "Should be set by functions which supply a prefix argument.")

;;; PREFIX-ARGUMENT  --  Public
;;;
;;;
(defun prefix-argument ()
  "Return the current value of the prefix argument.  This can be set
  with Setf."
  *prefix-argument*)

;;; %SET-PREFIX-ARGUMENT  --  Internal
;;;
(defun %set-prefix-argument (argument)
  "Set the prefix argument for the next command to Argument."
  (unless (or (null argument) (integerp argument))
    (error "Prefix argument ~S is neither an integer nor Nil." argument))
  (setq *prefix-argument* argument  *prefix-argument-supplied* t))

;;;; The Command Loop:

;;; Buffers we use to read and translate keys.
;;;
(defvar *current-command* (make-array 10 :fill-pointer 0 :adjustable t))
(defvar *current-translation* (make-array 10 :fill-pointer 0 :adjustable t))

(defvar *invoke-hook* #'(lambda (command p)
			  (funcall (command-function command) p))
  "This function is called by the command interpreter when it wants to invoke a
  command.  The arguments are the command to invoke and the prefix argument.
  The default value just calls the Command-Function with the prefix argument.")


;;; %COMMAND-LOOP  --  Internal
;;;
;;;    Read commands from the terminal and execute them, forever.
;;;
(defun %command-loop ()
  (let  ((cmd *current-command*)
	 (trans *current-translation*)
	 (*last-command-type* nil)
	 (*command-type-set* nil)
	 (*prefix-argument* nil)
	 (*prefix-argument-supplied* nil))
    (declare (special *last-command-type* *command-type-set*
		      *prefix-argument* *prefix-argument-supplied*))
    (setf (fill-pointer cmd) 0)
    (handler-bind
	;; Bind this outside the invocation loop to save consing.
	((editor-error #'(lambda (condx)
			   (beep)
			   (let ((string (editor-error-format-string condx)))
			     (when string
			       (apply #'message string
				      (editor-error-format-arguments condx)))
			     (throw 'command-loop-catcher nil)))))
      (loop
	(unless (eq *current-buffer* *echo-area-buffer*)
	  (when (buffer-modified *echo-area-buffer*) (clear-echo-area))
	  (unless (or (zerop (length cmd))
		      (not (value ed::key-echo-delay)))
	    (editor-sleep (value ed::key-echo-delay))
	    (unless (listen *editor-input*)
	      (clear-echo-area)
	      (dotimes (i (length cmd))
		(print-pretty-character (aref cmd i) *echo-area-stream*)
		(write-char #\space *echo-area-stream*)))))
	(vector-push-extend (read-char *editor-input*) cmd)
	
	(multiple-value-bind (trans-result prefix-p)
			     (translate-key cmd trans)
	  (multiple-value-bind (res t-bindings)
			       (get-current-binding trans-result)
	    (cond
	     ((commandp res)
	      (let ((punt t))
		(catch 'command-loop-catcher
		  (dolist (c t-bindings)
		    (funcall *invoke-hook* c *prefix-argument*))
		  (funcall *invoke-hook* res *prefix-argument*)
		  (setf punt nil))
		(when punt (invoke-hook ed::command-abort-hook)))
	      (if *command-type-set*
		  (setq *command-type-set* nil)
		  (setq *last-command-type* nil))
	      (if *prefix-argument-supplied*
		  (setq *prefix-argument-supplied* nil)
		  (setq *prefix-argument* nil))
	      (setf (fill-pointer cmd) 0))
	     ((null res)
	      (unless prefix-p
		(beep)
		(setq *prefix-argument* nil)
		(setf (fill-pointer cmd) 0)))
	     ((not (key-table-p res))
	      (error "Bad thing in key table: ~S" res)))))))))


;;; EXIT-HEMLOCK  --  Public
;;;
(defun exit-hemlock (&optional (value t))
  "Exit from ED, returning the specified value."
  (throw 'hemlock-exit value))
