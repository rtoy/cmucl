;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; CMU Common Lisp printer.
;;;
;;; Written by Neal Feinberg, Bill Maddox, Steven Handerson, and Skef Wholey.
;;; Modified by various CMU Common Lisp maintainers.
;;;

(in-package "LISP")

(export '(*print-escape* *print-pretty* *print-circle* *print-base*
	  *print-radix* *print-case* *print-level* *print-length*
	  *print-array* *print-gensym* write prin1 print princ
	  write-to-string prin1-to-string princ-to-string))

(defvar *print-escape* T
  "Flag which indicates that slashification is on.  See the manual")
(defvar *print-pretty* T
  "Flag which indicates that pretty printing is to be used")
(defvar *print-base* 10.
  "The output base for integers and rationals.")
(defvar *print-radix* ()
  "This flag requests to verify base when printing rationals.")
(defvar *print-level* ()
  "How many levels deep to print.  Unlimited if null.")
(defvar *print-length* ()
  "How many elements to print on each level.  Unlimited if null.")
(defvar *print-circle* ()
  "Whether to worry about circular list structures. See the manual.")
(defvar *print-case* ':upcase
  "What kind of case the printer should use by default")
(defvar *print-array* T
  "Whether the array should print it's guts out")
(defvar *print-gensym* T
  "If true, symbols with no home package are printed with a #: prefix.
  If false, no prefix is printed.")


;;; Imported from reader.
;;;
(proclaim '(special *read-default-float-format*))

;;; From the package system.
;;;
(proclaim '(special *package* *keyword-package*))


;;; DOSTRING -- Internal.
;;;
;;; This macro returns code which maps over a string, binding VARIABLE to each
;;; successive character in the string INIT-FORM, and executing BODY with
;;; the variable so bound.  This function used to be part of Common Lisp, but
;;; is no more.  It lives on in the printer, though.
;;;
(defmacro dostring ((variable init-form terminate-form) &rest body)
  (let ((str (gensym))
	(end (gensym))
	(index (gensym)))
    `(let* ((,str ,init-form)
	    (,end (length (the string ,str)))
	    (,index 0))
       (declare (fixnum ,index ,end))
       (loop
	 (when (= ,index ,end) (return ,terminate-form))
	 (let ((,variable (char ,str ,index)))
	   ,@body)
	 (incf ,index)))))



;;;; Printing Functions

(proclaim '(inline setup-printer-state))

(defvar *previous-case* ()
  "What the previous case selection the printer was set to.")

;;; This variable contains the current definition of one of three symbol
;;; printers.  SETUP-PRINTER-STATE sets this variable.
;;;
(defvar *internal-symbol-output-function* nil)

;;; SETUP-PRINTER-STATE -- Internal.
;;;
;;; This function sets the internal global symbol
;;; *internal-symbol-output-function* to the right function depending on the
;;; value of *print-case*.  See the manual for details.  The print buffer
;;; stream is also reset.
;;;
(defun setup-printer-state ()
  (unless (eq *print-case* *previous-case*)
    (setq *previous-case* *print-case*)
    (setq *internal-symbol-output-function*
	  (case *print-case*
	    (:upcase #'output-uppercase-symbol)
	    (:downcase #'output-lowercase-symbol)
	    (:capitalize #'output-capitalize-symbol)
	    (T (let ((bad-case *print-case*))
		 (setq *print-case* :upcase)
		 (Error "Invalid *print-case* value: ~s" bad-case)))))))


(defun write (object &key
		     ((:stream  stream)		     *standard-output*)
		     ((:escape  *print-escape*)      *print-escape*)
		     ((:radix   *print-radix*)       *print-radix*)
		     ((:base    *print-base*)        *print-base*)
		     ((:circle  *print-circle*)      *print-circle*)
		     ((:pretty  *print-pretty*)      *print-pretty*)
		     ((:level   *print-level*)       *print-level*)
		     ((:length  *print-length*)      *print-length*)
		     ((:case    *print-case*)        *print-case*)
		     ((:array   *print-array*)       *print-array*)
		     ((:gensym  *print-gensym*)      *print-gensym*))
  "Outputs OBJECT to the specified stream, defaulting to *standard-output*"
  (setup-printer-state)
  (let ((stream (out-synonym-of stream)))
    (if *print-pretty*
	(output-pretty-object object stream)
	(output-object object stream)))
  object)

(defun prin1 (object &optional stream)
  "Outputs a mostly READable printed representation of OBJECT on the specified
  stream."
  (let ((stream (out-synonym-of stream)))
    (setup-printer-state)
    (let ((*print-escape* T))
      (if *print-pretty*
	  (output-pretty-object object stream)
	  (output-object object stream)))
    object))

(defun princ (object &optional stream)
  "Outputs an asthetic but not READable printed representation of OBJECT on the
  specified stream."
  (let ((stream (out-synonym-of stream)))
    (setup-printer-state)
    (let ((*print-escape* NIL))
      (if *print-pretty*
	  (output-pretty-object object stream)
	  (output-object object stream)))
    object))

(defun print (object &optional stream)
  "Outputs a terpri, the mostly READable printed represenation of OBJECT, and 
  space to the stream."
  (let ((stream (out-synonym-of stream)))
    (terpri stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))

(defun write-to-string (object &key
		     ((:escape  *print-escape*)      *print-escape*)
		     ((:radix   *print-radix*)       *print-radix*)
		     ((:base    *print-base*)        *print-base*)
		     ((:circle  *print-circle*)      *print-circle*)
		     ((:pretty  *print-pretty*)      *print-pretty*)
		     ((:level   *print-level*)       *print-level*)
		     ((:length  *print-length*)      *print-length*)
		     ((:case    *print-case*)        *print-case*)
		     ((:array   *print-array*)       *print-array*)
		     ((:gensym  *print-gensym*)      *print-gensym*))
  "Returns the printed representation of OBJECT as a string."
  (stringify-object object *print-escape*))

(defun prin1-to-string (object)
  "Returns the printed representation of OBJECT as a string with 
   slashification on."
  (stringify-object object t))

(defun princ-to-string (object)
  "Returns the printed representation of OBJECT as a string with
  slashification off."
  (stringify-object object nil))

(defvar *print-string-stream* (make-string-output-stream)
  "Holds the string stream for the x-TO-STRING functions.")

(defvar *in-stringify-object* ()
  "T if in the middle of stringify-object.")

;;; STRINGIFY-OBJECT -- Internal.
;;;
;;; This produces the printed representation of an object as a string.  The
;;; few ...-TO-STRING functions above call this.
;;;
(defun stringify-object (object &optional (*print-escape* nil))
  (let ((stream (if *in-stringify-object*
		    (make-string-output-stream)
		    *print-string-stream*))
	(*in-stringify-object* t))
    (setup-printer-state)
    (if *print-pretty*
	(output-pretty-object object stream)
	(output-object object stream 0))
    (get-output-stream-string stream)))



;;;; Central Print Functions.  

;;; OUTPUT-OBJECT -- Internal.
;;;
;;; This takes an object and outputs its printed representation to stream,
;;; which typically is the internal print stream.  This function is called
;;; recursively by the sub-functions which know how to print structures which
;;; can contain other lisp objects.
;;;
(defun output-object (object stream &optional (currlevel 0))
  "Outputs a string which is the printed representation of the given object."
  ;; First check and make sure we aren't too deep
  (declare (fixnum currlevel))
  (if (and (not (null *print-level*))
	   (not (= *print-level* 0))
	   (>= currlevel (the fixnum *print-level*)))
      (write-char #\# stream)
      (typecase object
	    (symbol
	     (if *print-escape*
		 (output-symbol object stream)
		 (case *print-case*
		   (:upcase (write-string (symbol-name object) stream))
		   (:downcase
		    (let ((name (symbol-name object)))
		      (declare (simple-string name))
		      (dotimes (i (length name))
			(write-char (char-downcase (char name i)) stream))))
		   (:capitalize
		    (write-string (string-capitalize (symbol-name object))
				  stream)))))
	    ;; If a list, go through element by element, being careful
	    ;; about not running over the printlength
	    (list
	     (output-list object stream (1+ currlevel)))
	    (string
	     (if *print-escape*
		 (quote-string object stream)
		 (write-string object stream)))
	    (integer
	     (output-integer object stream))
	    (float
	     (output-float object stream))
	    (ratio
	     (output-ratio object stream))
	    (complex
	     (output-complex object stream))
	    (structure
	     (output-structure object stream currlevel))
	    (character
	     (output-character object stream))
	    (vector
	     #+new-compiler
	     (if (eql (%primitive get-type object) system:%code-type)
		 (output-random object stream)
		 (output-vector object stream))
	     #-new-compiler
	     (output-vector object stream))
	    (array
	     (output-array object stream (1+ currlevel))) 
	    (t (output-random object stream)))))



;;;; Symbol Printing Subfunctions

(defun output-symbol (object stream)
  (let ((package (symbol-package object))
	(name (symbol-name object)))
    (cond
     ;; If the symbol's home package is the current one, then a
     ;; prefix is never necessary.
     ((eq package *package*))
     ;; If the symbol is in the keyword package, output a colon.
     ((eq package *keyword-package*)
      (write-char #\: stream))
     ;; Uninterned symbols print with a leading #:.
     ((null package)
      (when *print-gensym* (write-string "#:" stream)))
     (t
      (let ((found (car (memq package (package-use-list *package*)))))
	(multiple-value-bind (symbol externalp)
			     (find-external-symbol name package)
	  ;; If the symbol's home package is in our use list and is an external
	  ;; symbol there, then it needs no qualification.
	  (unless (and found externalp (eq symbol object))
	    (multiple-value-bind (symbol accessible)
				 (find-symbol name *package*)
	      ;; If we can find the symbol by looking it up, it need not be
	      ;; qualified.  This can happen if the symbol has been inherited
	      ;; from a package other than its home package.
	      (unless (and accessible (eq symbol object))
		(funcall *internal-symbol-output-function*
			 (package-name package)
			 stream)
		(if externalp
		    (write-char #\: stream)
		    (write-string "::" stream)))))))))
    (funcall *internal-symbol-output-function* name stream)))



;;;; Escaping symbols:

;;;    When we print symbols we have to figure out if they need to be
;;; printed with escape characters.  This isn't a whole lot easier than
;;; reading symbols in the first place.
;;;
;;; For each character, the value of the corresponding element is a fixnum
;;; with bits set corresponding to attributes that the character has.  This
;;; is also used by the character printer.
;;;
(defvar character-attributes
  (make-array char-code-limit :element-type '(unsigned-byte 8)
	      :initial-element 0))

(eval-when (compile load eval)

;;; Constants which are a bit-mask for each interesting character attribute.
;;;
(defconstant number-attribute	#b10)		; A numeric digit.
(defconstant letter-attribute	#b100)		; A upper-case letter.
(defconstant sign-attribute	#b1000)		; +-
(defconstant extension-attribute #b10000)	; ^_
(defconstant dot-attribute 	#b100000)	; .
(defconstant slash-attribute	#b1000000)	; /
(defconstant other-attribute	#b1)            ; Anything else legal.
(defconstant funny-attribute	#b10000000)	; Anything illegal.

(defconstant attribute-names
  '((number . number-attribute) (letter . letter-attribute)
    (sign . sign-attribute) (extension . extension-attribute)
    (dot . dot-attribute) (slash . slash-attribute)
    (other . other-attribute) (funny . funny-attribute)))

); Eval-When (compile load eval)

(flet ((set-bit (char bit)
	 (let ((code (char-code char)))
	   (setf (aref character-attributes code)
		 (logior bit (aref character-attributes code))))))

  (dolist (char '(#\! #\@ #\$ #\% #\& #\* #\= #\~ #\[ #\] #\{ #\}
		  #\? #\< #\>))
    (set-bit char other-attribute))

  (dotimes (i 10)
    (set-bit (digit-char i) number-attribute))

  (do ((code (char-code #\A) (1+ code))
       (end (char-code #\Z)))
      ((> code end))
    (declare (fixnum code end))
    (set-bit (code-char code) letter-attribute))

  (set-bit #\- sign-attribute)
  (set-bit #\+ sign-attribute)
  (set-bit #\^ extension-attribute)
  (set-bit #\_ extension-attribute)
  (set-bit #\. dot-attribute)
  (set-bit #\/ slash-attribute)

  ;; Make anything not explicitly allowed funny...
  (dotimes (i char-code-limit)
    (when (zerop (aref character-attributes i))
      (setf (aref character-attributes i) funny-attribute))))

;;; For each character, the value of the corresponding element is the lowest
;;; base in which that character is a digit.
;;;
(defvar digit-bases
  (make-array char-code-limit :element-type '(mod 37) :initial-element 36))

(dotimes (i 36)
  (let ((char (digit-char i 36)))
    (setf (aref digit-bases (char-code char)) i)))


;;; SYMBOL-QUOTEP  --  Internal
;;;
;;;    A FSM-like thingie that determines whether a symbol is a potential
;;; number or has evil characters in it.
;;;
(defun symbol-quotep (name)
  (declare (simple-string name))
  (macrolet ((advance (tag &optional (at-end t))
	       `(progn
		 (when (= index len)
		   ,(if at-end '(go TEST-SIGN) '(return nil)))
		 (setq current (schar name index)
		       code (char-code current)
		       bits (aref attributes code))
		 (incf index)
		 (go ,tag)))
	     (test (&rest attributes)
		`(not (zerop
		       (the fixnum
			    (logand
			     (logior ,@(mapcar
					#'(lambda (x)
					    (or (cdr (assoc x attribute-names))
						(error "Blast!")))
					attributes))
			     bits)))))
	     (digitp ()
	       `(< (the fixnum (aref bases code)) base)))

    (prog ((len (length name))
	   (attributes character-attributes)
	   (bases digit-bases)
	   (base *print-base*)
	   (index 0)
	   (bits 0)
	   (code 0)
	   current)
      (declare (fixnum len base index bits code))
      (advance START t)

     TEST-SIGN ; At end, see if it is a sign...
      (return (not (test sign)))

     OTHER ; Not potential number, see if funny chars...
      (return (not (null (%primitive find-character-with-attribute
				     name (1- index) len
				     attributes funny-attribute))))
     START
      (when (digitp)
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance DIGIT)))
      (when (test letter number other slash) (advance OTHER nil))
      (when (char= current #\.) (advance DOT-FOUND))
      (when (test sign extension) (advance START-STUFF nil))
      (return t)
		  
     DOT-FOUND ; Leading dots...
      (when (test letter) (advance START-DOT-MARKER nil))
      (when (digitp) (advance DOT-DIGIT))
      (when (test number other) (advance OTHER nil))
      (when (test extension slash sign) (advance START-DOT-STUFF nil))
      (when (char= current #\.) (advance DOT-FOUND))
      (return t)

     START-STUFF ; Leading stuff before any dot or digit.
      (when (digitp)
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance DIGIT)))
      (when (test number other) (advance OTHER nil))
      (when (test letter) (advance START-MARKER nil))
      (when (char= current #\.) (advance START-DOT-STUFF nil))
      (when (test sign extension slash) (advance START-STUFF nil))
      (return t)

     START-MARKER ; Number marker in leading stuff...
      (when (test letter) (advance OTHER nil))
      (go START-STUFF)

     START-DOT-STUFF ; Leading stuff containing dot w/o digit...
      (when (test letter) (advance START-DOT-STUFF nil))
      (when (digitp) (advance DOT-DIGIT))
      (when (test sign extension dot slash) (advance START-DOT-STUFF nil))
      (when (test number other) (advance OTHER nil))
      (return t)

     START-DOT-MARKER ; Number marker in leading stuff w/ dot..
      ;; Leading stuff containing dot w/o digit followed by letter...
      (when (test letter) (advance OTHER nil))
      (go START-DOT-STUFF)

     DOT-DIGIT ; In a thing with dots...
      (when (test letter) (advance DOT-MARKER))
      (when (digitp) (advance DOT-DIGIT))
      (when (test number other) (advance OTHER nil))
      (when (test sign extension dot slash) (advance DOT-DIGIT))
      (return t)

     DOT-MARKER ; Number maker in number with dot...
      (when (test letter) (advance OTHER nil))
      (go DOT-DIGIT)

     LAST-DIGIT-ALPHA ; Previous char is a letter digit...
      (when (or (digitp) (test sign slash))
	(advance ALPHA-DIGIT))
      (when (test letter number other dot) (advance OTHER nil))
      (return t)
      
     ALPHA-DIGIT ; Seen a digit which is a letter...
      (when (or (digitp) (test sign slash))
	(if (test letter)
	    (advance LAST-DIGIT-ALPHA)
	    (advance ALPHA-DIGIT)))
      (when (test letter) (advance ALPHA-MARKER))
      (when (test number other dot) (advance OTHER nil))
      (return t)

     ALPHA-MARKER ; Number marker in number with alpha digit...
      (when (test letter) (advance OTHER nil))
      (go ALPHA-DIGIT)

     DIGIT ; Seen only real numeric digits...
      (when (digitp)
	(if (test letter)
	    (advance ALPHA-DIGIT)
	    (advance DIGIT)))
      (when (test number other) (advance OTHER nil))
      (when (test letter) (advance MARKER)) 
      (when (test extension slash sign) (advance DIGIT))
      (when (char= current #\.) (advance DOT-DIGIT))
      (return t)

     MARKER ; Number marker in a numeric number...
      (when (test letter) (advance OTHER nil))
      (go DIGIT))))

;;;; Pathname hackery

;;; This function takes the pname of a symbol and adds slashes and/or 
;;; vertical bars to it to make it readable again.
;;; Special quoting characters are currently vertical bar and slash who's 
;;; role in life are to specially quote symbols.  Funny symbol characters
;;; are those who need special slashification when they are to be printed
;;; so they can be read in again.  These currently include such characters 
;;; as hash signs, colons of various sorts, etc.
;;; Now there are three different version: UPPERCASE, lowercase and Captialize.
;;; Check out the manual under the entry for *print-case* for details.

(eval-when (compile eval)
(defmacro symbol-quote-char-p (char)
  `(or (char= ,char #\\) (char= ,char #\|)))
); eval-when (compile eval)

(defun output-uppercase-symbol (pname stream)
  (declare (simple-string pname))
  (cond ((symbol-quotep pname)
	 (write-char #\| stream)
	 (dostring (char pname)
	   ;;If it needs slashing, do it.
	   (if (symbol-quote-char-p char)
	       (write-char #\\ stream))
	   (write-char char stream))
	 (write-char #\| stream))
	(t
	 (write-string pname stream))))

;;; See documentation for output-symbol-uppercase (above).
;;;
(defun output-lowercase-symbol (pname stream)
  (declare (simple-string pname))
  (cond ((symbol-quotep pname)
	 (write-char #\| stream)
	 (dostring (char pname)
	   (if (symbol-quote-char-p char)
	       (write-char #\\ stream))
	   (write-char char stream))
	 (write-char #\| stream))
	(t
	 (dostring (char pname)
	   (write-char (char-downcase char) stream)))))


(defun output-capitalize-symbol (pname stream)
  (declare (simple-string pname))
  (cond
   ((symbol-quotep pname)
    (write-char #\| stream)
    (dostring (char pname)
      (if (symbol-quote-char-p char)
	  (write-char #\\ stream))
      (write-char char stream))
    (write-char #\| stream))
   (t
    (do ((index 0 (1+ index))
	 (pname-length (length (the string pname)))
	 (prev-not-alpha t))
	((= index pname-length))
      (declare (fixnum index pname-length))
      (let ((char (char pname index)))
	(write-char (if prev-not-alpha char (char-downcase char)) stream)
	(setq prev-not-alpha (not (alpha-char-p char))))))))



;;;; Recursive Datatype Printing Subfunctions

(defun output-list (list stream &optional (currlevel 0))
  (write-char #\( stream)
  (do ((list list (cdr list))
       (currlength 0 (1+ currlength)))
      ((or (null list)
	   (and (not (null *print-length*))
		(>= currlength (the fixnum *print-length*))))
       (if (not (null list)) (write-string " ..." stream))
       (write-char #\) stream))
    (declare (fixnum currlength))
    ;;If we are not printing the first object, we should space first.
    (if (> currlength 0) (write-char #\space stream))
    ;;Print whatever the car of the list is, at this level.
    (output-object (car list) stream currlevel)
    (cond ((not (or (consp (cdr list))
		    (null (cdr list))))
	   (write-string " .  " stream)
	   (output-object (cdr list) stream currlevel)
	   (write-char #\) stream)
	   (return ())))))
 
(defun output-vector (vector stream &optional (currlevel 0))
  (declare (fixnum currlevel))
  (cond ((not *print-array*)
	 (output-terse-array vector stream currlevel))
	(T
	 (if (bit-vector-p vector)
	     (write-string "#*" stream)
	     (write-string "#(" stream))
	 (do ((currlength 0 (1+ currlength))
	      (vlength (length (the vector vector)))
	      (not-bit-vector-p (not (bit-vector-p vector))))
	     ((or (and (not (null *print-length*))
		       (>= currlength (the fixnum *print-length*)))
		  (= currlength vlength))
	      (if (not (= currlength vlength)) (write-string " ..." stream))
	      (if not-bit-vector-p
		  (write-char #\) stream)))
	   (declare (fixnum currlength vlength))
	 ;;Put a space before every element except the first
	 ;; and not in bit vectors.
	 (if (and (> currlength 0) not-bit-vector-p)
	     (write-char #\space stream))
	 ;;Output an element of the vector
	 (output-object (aref vector currlength) stream currlevel)))))

(defun output-array (array stream &optional (currlevel 0))
  "Outputs the printed representation of any array in either the #< or #A form."
  (let ((rank (array-rank array)))
    (cond ((not *print-array*)
	   (output-terse-array array stream rank))
	  (T
	   (output-array-guts array rank stream currlevel)))))

;;; Master function for outputing the #A form of an array
;;;
(defun output-array-guts (array rank stream currlevel)
  (write-char #\# stream)
  (let ((*print-base* 10))
    (output-integer rank stream))
  (write-char #\A stream)
  (with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (sub-output-array-guts data (array-dimensions array)
			   stream currlevel start)))

;;; Some Ideas stolen from Skef Wholey.
;;; Helping function for above.
(defun sub-output-array-guts (array dimensions stream currlevel index)
  (declare (fixnum currlevel index))
  (cond ((null dimensions)
	 (output-object (aref array index) stream currlevel)
	 (1+ index))
	((and (not (null *print-level*))
	      (>= currlevel (the fixnum *print-level*)))
	 (write-char #\# stream)
	 index)
	(t
	 (write-char #\( stream)
	 (do ((index index)
	      (times 0 (1+ times))
	      (limit (pop dimensions)))
	     ((or (= times limit)
		  (and (not (null *print-length*))
		       (= times *print-length*)))
	      (if (not (= times limit))
		  (write-string " ...)" stream)
		  (write-char #\) stream))
	      index)
	   (declare (fixnum index times limit))
	   (if (not (zerop times)) (write-char #\space stream))
	   (setq index
		 (sub-output-array-guts array dimensions
					stream (1+ currlevel) index))))))

;;; Used to output the #< form of any array.
;;;
(defun output-terse-array (array stream rank)
  (write-string "#<" stream)
  (cond ((vectorp array)
	 (if (bit-vector-p array)
	     (write-string "Bit-vector" stream)
	     (write-string "Vector" stream)))
	(T
	 (write-string "Array, rank " stream)
	 (output-integer rank stream)))
  (finish-random array stream))


;;; Structure Printing.  These days we can always pass the buck to the Defstruct
;;; code.

(defun output-structure (structure stream currlevel)
  (funcall (or (info type printer (svref structure 0))
	       #'c::default-structure-print)
	   structure stream currlevel))


;;;; Functions to help print strings.

;;; QUOTE-STRING -- Internal.
;;;
;;; This function outputs a string quoting characters sufficiently, so someone
;;; can read it in again.  Basically, put a slash in front of an character
;;; satisfying FROB.
;;;
(defun quote-string (string stream)
  (macrolet ((frob (char)
	       ;; Probably should look at readtable, but just do this for now.
	       `(or (char= ,char #\\)
		    (char= ,char #\"))))
    (write-char #\" stream)
    (dostring (char string)
      (when (frob char) (write-char #\\ stream))
      (write-char char stream))
    (write-char #\" stream)))


(defun whitespace-char-p (char)
  "Determines whether or not the character is considered whitespace."
  (or (char= char #\space)
      (char= char #\tab)
      (char= char #\return)
      (char= char #\linefeed)))

;;;; Integer, ratio, complex printing.

(defun output-integer (integer stream)
  (cond ((not (and (fixnump *print-base*) (> (the fixnum *print-base*) 1)))
	 (let ((obase *print-base*))
	   (setq *print-base* 10.)
	   (error "~A is not a reasonable value for *Print-Base*." obase)))
	;; Otherwise print the base
	(T (cond ((and (not (= *print-base* 10.))
		       *print-radix*)
		  ;; First print leading base information, if any.
		  (write-char #\# stream)
		  (write-char (case *print-base*
				(2.  #\b)
				(8.  #\o)
				(16. #\x)
				(T (let ((fixbase *print-base*)
					 (*print-base* 10.)
					 (*print-radix* ()))
				     (sub-output-integer fixbase stream))
				   #\r))
			      stream)))
	   ;; Then output a minus sign if the number is negative, then output
	   ;; the absolute value of the number.
	   (cond ((bignump integer) (print-bignum integer stream))
		 ((< integer 0)
		  (write-char #\- stream)
		  (sub-output-integer (- integer) stream))
		 (T (sub-output-integer integer stream)))
	   ;; Print any trailing base information, if any.
	   (if (and (= *print-base* 10.) *print-radix*)
	       (write-char #\. stream)))))

(defun sub-output-integer (integer stream)
  (let ((quotient ())
	(remainder ()))
    ;; Recurse until you have all the digits pushed on the stack.
    (if (not (zerop (multiple-value-setq (quotient remainder)
		      (truncate integer *print-base*))))
	(sub-output-integer quotient stream))
    ;; Then as each recursive call unwinds, turn the digit (in remainder) 
    ;; into a character and output the character.
    (write-char (int-char (if (and (> remainder 9.)
				   (> *print-base* 10.))
			      (+ (char-int #\A) (- remainder 10.))
			      (+ (char-int #\0) remainder)))
		stream)))


(defun output-ratio (ratio stream)
  (when *print-radix*
    (write-char #\# stream)
    (case *print-base*
      (2 (write-char #\b stream))
      (8 (write-char #\o stream))
      (16 (write-char #\x stream))
      (t (write *print-base* :stream stream :radix nil :base 10)))
    (write-char #\r stream))
  (let ((*print-radix* nil))
    (output-integer (numerator ratio) stream)
    (write-char #\/ stream)
    (output-integer (denominator ratio) stream)))

(defun output-complex (complex stream)
  (write-string "#C(" stream)
  (output-object (realpart complex) stream)
  (write-char #\space stream)
  (output-object (imagpart complex) stream)
  (write-char #\) stream))



;;;; Bignum printing

;;; Written by Steven Handerson
;;;  (based on Skef's idea)

;;; BIGNUM-FIXNUM-DIVIDE-INPLACE wants the divisor to be of integer-length 19
;;; or less.  1- the ideal power of the base for a divisor.
;;;
(defparameter *fixnum-power--1*
  '#(NIL NIL 17 10 8 7 6 5 5 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2
	 2))

;;; The base raised to the ideal power.
;;;
(defparameter *base-power*
  '#(NIL NIL 262144 177147 262144 390625 279936 117649 262144 59049 100000
	 161051 248832 371293 38416 50625 65536 83521 104976 130321 160000
	 194481 234256 279841 331776 390625 456976 19683 21952 24389 27000
	 29791 32768 35937 39304 42875))

(defun print-bignum (big stream)
  (bignum-print-aux (cond ((minusp big)
			   (write-char #\- stream)
			   (- big))
			  (t (copy-xnum big)))
		    stream)
  big)

(defun bignum-print-aux (big stream)
  (multiple-value-bind (newbig fix)
		       (bignum-fixnum-divide-inplace
			big (aref *base-power* *print-base*))
    (if (fixnump newbig)
	(sub-output-integer newbig stream)
	(bignum-print-aux newbig stream))
    (do ((zeros (aref *fixnum-power--1* *print-base*) (1- zeros))
	 (base-power *print-base* (* base-power *print-base*)))
	((> base-power fix)
	 (dotimes (i zeros) (write-char #\0 stream))
	 (sub-output-integer fix stream)))))



;;;; Floating Point printing
;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; WARNING: For efficiency, there is a single string object *digit-string*
;;; which is modified destructively and returned as the value of
;;; FLONUM-TO-STRING.  Thus the returned value is not valid across multiple 
;;; calls.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

(defvar *digits* "0123456789")

(defvar *digit-string*
  (make-array 50 :element-type 'string-char :fill-pointer 0 :adjustable t))

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (cond ((zerop x)
	 ;;zero is a special case which float-string cannot handle
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	     (values "." 1 t t 0)))
	(t
	  (setf (fill-pointer *digit-string*) 0)
	  (multiple-value-bind (sig exp)
			       (integer-decode-float x)
	    (if (typep x 'short-float)
		;;20 and 53 are the number of bits of information in the
		;;significand, less sign, of a short float and a long float
		;;respectively.
		(float-string sig exp 20 width fdigits scale fmin)
		(float-string sig exp 53 width fdigits scale fmin))))))

(defun float-string (fraction exponent precision width fdigits scale fmin)
  (let ((r fraction) (s 1) (m- 1) (m+ 1) (k 0)
	(digits 0) (decpnt 0) (cutoff nil) (roundup nil) u low high)
    ;;Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;;Rational arithmetic avoids loss of precision in subsequent calculations.
    (cond ((> exponent 0)
	   (setq r (ash fraction exponent))
	   (setq m- (ash 1 exponent))	   
	   (setq m+ m-))                   
	  ((< exponent 0)
	   (setq s (ash 1 (- exponent)))))
    ;;adjust the error bounds m+ and m- for unequal gaps
    (when (= fraction (ash 1 precision))
      (setq m+ (ash m+ 1))
      (setq r (ash r 1))
      (setq s (ash s 1)))
    ;;scale value by requested amount, and update error bounds
    (when scale
      (if (minusp scale)
	  (let ((scale-factor (expt 10 (- scale))))
	    (setq s (* s scale-factor)))
	  (let ((scale-factor (expt 10 scale)))
	    (setq r (* r scale-factor))
	    (setq m+ (* m+ scale-factor))
	    (setq m- (* m- scale-factor)))))
    ;;scale r and s and compute initial k, the base 10 logarithm of r
    (do ()
        ((>= r (ceiling s 10)))
      (decf k)
      (setq r (* r 10))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10)))
    (do ()(nil)
      (do ()
	  ((< (+ (ash r 1) m+) (ash s 1)))
	(setq s (* s 10))
	(incf k))
      ;;determine number of fraction digits to generate
      (cond (fdigits
	     ;;use specified number of fraction digits
	     (setq cutoff (- fdigits))
	     ;;don't allow less than fmin fraction digits
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin))))
	    (width
	     ;;use as many fraction digits as width will permit
             ;;but force at least fmin digits even if width will be exceeded
	     (if (< k 0)
		 (setq cutoff (- 1 width))
		 (setq cutoff (1+ (- k width))))
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin)))))
      ;;If we decided to cut off digit generation before precision has
      ;;been exhausted, rounding the last digit may cause a carry propagation.
      ;;We can prevent this, preserving left-to-right digit generation, with
      ;;a few magical adjustments to m- and m+.  Of course, correct rounding
      ;;is also preserved.
      (when (or fdigits width)
	(let ((a (- cutoff k))
	      (y s))
	  (if (>= a 0)
	      (dotimes (i a) (setq y (* y 10)))
	      (dotimes (i (- a)) (setq y (ceiling y 10))))
	  (setq m- (max y m-))
	  (setq m+ (max y m+))
	  (when (= m+ y) (setq roundup t))))
      (when (< (+ (ash r 1) m+) (ash s 1)) (return)))
    ;;zero-fill before fraction if no integer part
    (when (< k 0)
      (setq decpnt digits)
      (vector-push-extend #\. *digit-string*)
      (dotimes (i (- k))
	(incf digits) (vector-push-extend #\0 *digit-string*)))
    ;;generate the significant digits
    (do ()(nil)
      (decf k)
      (when (= k -1)
	(vector-push-extend #\. *digit-string*)
	(setq decpnt digits))
      (multiple-value-setq (u r) (truncate (* r 10) s))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10))
      (setq low (< (ash r 1) m-))
      (if roundup
	  (setq high (>= (ash r 1) (- (ash s 1) m+)))
	  (setq high (> (ash r 1) (- (ash s 1) m+))))
      ;;stop when either precision is exhausted or we have printed as many
      ;;fraction digits as permitted
      (when (or low high (and cutoff (<= k cutoff))) (return))
      (vector-push-extend (char *digits* u) *digit-string*)
      (incf digits))
    ;;if cutoff occured before first digit, then no digits generated at all
    (when (or (not cutoff) (>= k cutoff))
      ;;last digit may need rounding
      (vector-push-extend (char *digits*
				(cond ((and low (not high)) u)
				      ((and high (not low)) (1+ u))
				      (t (if (<= (ash r 1) s) u (1+ u)))))
			  *digit-string*)
      (incf digits))
    ;;zero-fill after integer part if no fraction
    (when (>= k 0)
      (dotimes (i k) (incf digits) (vector-push-extend #\0 *digit-string*))
      (vector-push-extend #\. *digit-string*)
      (setq decpnt digits))
    ;;add trailing zeroes to pad fraction if fdigits specified
    (when fdigits
      (dotimes (i (- fdigits (- digits decpnt)))
	(incf digits)
	(vector-push-extend #\0 *digit-string*)))
    ;;all done
    (values *digit-string* (1+ digits) (= decpnt 0) (= decpnt digits) decpnt)))


(defconstant short-log10-of-2 0.30103s0)

;;; Given a non-negative floating point number, SCALE-EXPONENT returns a
;;; new floating point number Z in the range (0.1, 1.0] and and exponent
;;; E such that Z * 10^E is (approximately) equal to the original number.
;;; There may be some loss of precision due the floating point representation.


;;;
(defun scale-exponent (x)
  (if (typep x 'short-float)
      (scale-expt-aux x 0.0s0 1.0s0 1.0s1 1.0s-1 short-log10-of-2)
      (scale-expt-aux x 0.0l0 1.0l0 %long-float-ten
		      %long-float-one-tenth long-log10-of-2)))


(defun scale-expt-aux (x zero one ten one-tenth log10-of-2)
  (multiple-value-bind (sig exponent)
		       (decode-float x)
    (declare (ignore sig))
    (if (= x zero)
	(values zero 1)
	(let* ((ex (round (* exponent log10-of-2)))
	       (x (if (minusp ex)		;For the end ranges.
		      (* x ten (expt ten (- -1 ex)))
		      (/ x ten (expt ten (1- ex))))))
	  (do ((d ten (* d ten))
	       (y x (/ x d))
	       (ex ex (1+ ex)))
	      ((< y one)
	       (do ((m ten (* m ten))
		    (z y (* z m))
		    (ex ex (1- ex)))
		   ((>= z one-tenth) (values z ex)))))))))


;;;; Entry point for the float printer.

;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE:  When a number is to be printed in exponential format, it is scaled
;;; in floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM-TO-STRING are lost.  The
;;; difficulty is that FLONUM-TO-STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  When we switch to
;;; IEEE format for long floats, this will significantly restrict the magnitude
;;; of the largest allowable float.  This combined with microcoded bignum
;;; arithmetic might make it attractive to handle exponential notation with
;;; the same accuracy as non-exponential notation, using the method described
;;; in the Steele and White paper.

(defun output-float (x stream)
  (if (typep x 'short-float)
      (output-float-aux x stream 1.0s-3 1.0s7)
      (output-float-aux x stream %long-float1l-3 %long-float1l7)))


(defun output-float-aux (x stream e-min e-max)
  (cond ((zerop x)
	 (write-string "0.0" stream)
	 (if (and (not (typep x *read-default-float-format*))
		  (not (and (eq *read-default-float-format* 'single-float)
			    (typep x 'short-float))))
	     (write-string (if (typep x 'short-float) "s0" "L0") stream)))
	(t (when (minusp x) 
	     (write-char #\- stream)
	     (setq x (- x)))
	   (if (and (>= x e-min) (< x e-max))
	       ;;free format
	       (multiple-value-bind (str len lpoint tpoint)
				    (flonum-to-string x)
		 (declare (ignore len))
		 (when lpoint (write-char #\0 stream))
		 (write-string str stream)
		 (when tpoint (write-char #\0 stream))
		 (if (and (not (typep x *read-default-float-format*))
			  (not (and (eq *read-default-float-format*
					'single-float)
				    (typep x 'short-float))))
		     (write-string (if (typep x 'short-float) "s0" "L0")
				   stream)))
	       ;;exponential format 
	       (multiple-value-bind (f ex)
				    (scale-exponent x)
		 (multiple-value-bind (str len lpoint tpoint)
				      (flonum-to-string f nil nil 1)
		   (declare (ignore len))
		   (when lpoint (write-char #\0 stream))
		   (write-string str stream)
		   (when tpoint (write-char #\0 stream))
		   (write-char (if (typep x *read-default-float-format*)
				   #\E
				   (if (typep x 'short-float) #\S #\L))
			       stream)
		   ;;must subtract 1 from exponent here, due to
		   ;;the scale factor of 1 in call to FLONUM-TO-STRING
		   (unless (minusp (1- ex)) (write-char #\+ stream))
		   (output-integer (1- ex) stream)))))))


;;;; Output Character

;;; FUNNY-CHARACTER-CHAR-P returns a predicate which determines whether a
;;; character must be slashified when being output.
;;;
(defmacro funny-character-char-p (char)
  `(and (not (zerop (char-bits ,char)))
	(not (zerop (logand (aref character-attributes (char-code ,char))
			    funny-attribute)))))

;;; OUTPUT-CHARACTER  --  Internal
;;;
;;;    If *print-escape* is false, just do a WRITE-CHAR, otherwise output
;;; any bits and then the character or name, escaping if necessary.  In
;;; either case, we blast the bits or font before writing the character
;;; itself to the stream.
;;;
(defun output-character (char stream)
  (let ((base (make-char char)))
    (if *print-escape*
	(let ((name (char-name base)))
	  (write-string "#\\" stream)
	  (macrolet ((frob (key string)
		       `(when (char-bit char ,key)
			  (write-string ,string stream))))
	    (frob :control "CONTROL-")
	    (frob :meta "META-")
	    (frob :super "SUPER-")
	    (frob :hyper "HYPER-"))
	  (cond (name (write-string name stream))
		(t 
		 (when (funny-character-char-p char)
		   (write-char #\\ stream))
		 (write-char base stream))))
	(write-char base stream))))



;;;; Random and Miscellaneous Print Subfunctions


;;; OUTPUT-FUNCTION-OBJECT outputs the main part of the printed 
;;; representation of function objects.  It is called from OUTPUT-RANDOM
;;; below.

(defun output-function-object (subr stream)
  (let ((name (%primitive header-ref subr %function-name-slot)))
    (case (%primitive get-vector-subtype subr)
      (#.%function-entry-subtype
       (if (stringp name)
	   (format stream "Internal Function ~S" name)
	   (format stream "Function ~S" name)))
      (#.%function-closure-subtype
       (if (eval:interpreted-function-p subr)
	   (multiple-value-bind
	       (def ignore name)
	       (eval:interpreted-function-lambda-expression subr)
	     (declare (ignore ignore))
	     (let ((*print-level* 3))
	       (format stream "Interpreted Function ~S" (or name def))))
	   (format stream "Closure ~S"
		   (%primitive header-ref name %function-name-slot))))
      (#.%function-closure-entry-subtype
       (format stream "Closure Entry ~S" name))
      (#.%function-constants-subtype
       (format stream "Function Constants ~S" name))
      (#.%function-value-cell-subtype
       (assert (= %function-value-cell-value-slot %function-name-slot))
       (format stream "Indirect Value Cell ~S" name))
      #|
      (#.%function-funcallable-instance-subtype
       (format stream "Funcallable Instance ~S" name))
      |#
      (t (error "Unknown function subtype.")))))


;;; FINISH-RANDOM is a helping function for OUTPUT-RANDOM below.  
;;; It outputs the numerical value of the low 28 bits of 
;;; RANDOM-OBJECT, enclosed in braces, followed by the closing
;;; angle-bracket (">") random objects have at the end.  This
;;; is used to distringuish random objects of the same type.

(defun finish-random (random-object stream)
  (write-string " {" stream)
  (let ((*print-base* 16))
    (output-integer (%primitive make-fixnum random-object) stream))
  (write-string "}>" stream))

;;; Functions Objects and other implmentation specific objects 
;;; are output here. 

(defun output-random (object stream)
  (write-string "#<" stream)
  (if (compiled-function-p object)
      (output-function-object object stream)
      (let ((type (%primitive get-type object)))
	(write-string "Pointer into Hell, level " stream)
	(sub-output-integer type stream)))
  (finish-random object stream))
