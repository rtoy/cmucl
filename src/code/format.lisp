;;; -*- Package: FORMAT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/format.lisp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT and FORMATTER for CMU Common Lisp.
;;;
;;; Written by William Lott, with lots of stuff stolen from the previous
;;; version by David Adam and later rewritten by Bill Maddox.
;;; 

(in-package "FORMAT")
(use-package "EXT")
(use-package "KERNEL")

(intl:textdomain "cmucl")

(in-package "LISP")
(export '(format formatter))

(in-package "FORMAT")

(defstruct (format-directive
	    (:print-function %print-format-directive))
  (string (required-argument) :type simple-string)
  (start (required-argument) :type (and unsigned-byte fixnum))
  (end (required-argument) :type (and unsigned-byte fixnum))
  (character (required-argument) :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun %print-format-directive (struct stream depth)
  (declare (ignore depth))
  (print-unreadable-object (struct stream)
    (write-string (format-directive-string struct) stream
		  :start (format-directive-start struct)
		  :end (format-directive-end struct))))

(defvar *format-directive-expanders*
  (make-array char-code-limit :initial-element nil))
(defvar *format-directive-interpreters*
  (make-array char-code-limit :initial-element nil))

(defun %print-format-error (condition stream)
  (cl:format stream
	     (intl:gettext "~:[~;Error in format: ~]~
	      ~?~@[~%  ~A~%  ~V@T^~]")
	     (format-error-print-banner condition)
	     (format-error-complaint condition)
	     (format-error-arguments condition)
	     (format-error-control-string condition)
	     (format-error-offset condition)))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

(define-condition format-error (error)
  ((complaint :reader format-error-complaint :initarg :complaint)
   (arguments :reader format-error-arguments :initarg :arguments :initform nil)
   (control-string :reader format-error-control-string
		   :initarg :control-string
		   :initform *default-format-error-control-string*) 
   (offset :reader format-error-offset :initarg :offset
	   :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
		 :initform t))
  (:report %print-format-error))


;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
	(end (length string))
	(result nil)
	(in-block nil)
	(pprint nil)
	(semi nil)
	(justification-semi 0))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
	(when (> next-directive index)
	  (push (subseq string index next-directive) result))
	(when (= next-directive end)
	  (return))
	(let* ((directive (parse-directive string next-directive))
	       (directive-char (format-directive-character directive)))
	  ;; We are looking for illegal combinations of format
	  ;; directives in the control string.  See the last paragraph
	  ;; of CLHS 22.3.5.2: "an error is also signaled if the
	  ;; ~<...~:;...~> form of ~<...~> is used in the same format
	  ;; string with ~W, ~_, ~<...~:>, ~I, or ~:T."
	  (cond ((char= #\< directive-char)
		 ;; Found a justification or logical block
		 (setf in-block t))
		((and in-block (char= #\; directive-char))
		 ;; Found a semi colon in a justification or logical block
		 (setf semi t))
		((char= #\> directive-char)
		 ;; End of justification or logical block.  Figure out which. 
		 (setf in-block nil)
		 (cond ((format-directive-colonp directive)
			;; A logical-block directive.  Note that fact, and also
			;; note that we don't care if we found any ~;
			;; directives in the block.
			(setf pprint t)
			(setf semi nil))
		       (semi
			;; A justification block with a ~; directive in it.
			(incf justification-semi))))
		((and (not in-block)
		      (or (and (char= #\T directive-char) (format-directive-colonp directive))
			  (char= #\W directive-char)
			  (char= #\_ directive-char)
			  (char= #\I directive-char)))
		 (setf pprint t)))
	  (push directive result)
	  (setf index (format-directive-end directive)))))
    (when (and pprint (plusp justification-semi))
      (error 'format-error
	     :complaint (intl:gettext "A justification directive cannot be in the same format string~%~
                         as ~~W, ~~I, ~~:T, or a logical-block directive.")
	     :control-string string
	     :offset 0))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
	(end (length string)))
    (flet ((get-char ()
	     (if (= posn end)
		 (error 'format-error
			:complaint (intl:gettext "String ended before directive was found.")
			:control-string string
			:offset start)
		 (schar string posn))))
      (loop
	(let ((char (get-char)))
	  (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
		 (multiple-value-bind
		     (param new-posn)
		     (parse-integer string :start posn :junk-allowed t)
		   (push (cons posn param) params)
		   (setf posn new-posn)
		   (case (get-char)
		     (#\,)
		     ((#\: #\@)
		      (decf posn))
		     (t
		      (return)))))
		((or (char= char #\v) (char= char #\V))
		 (push (cons posn :arg) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\#)
		 (push (cons posn :remaining) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\')
		 (incf posn)
		 (push (cons posn (get-char)) params)
		 (incf posn)
		 (unless (char= (get-char) #\,)
		   (decf posn)))
		((char= char #\,)
		 (push (cons posn nil) params))
		((char= char #\:)
		 (if colonp
		     (error 'format-error
			    :complaint (intl:gettext "Too many colons supplied.")
			    :control-string string
			    :offset posn)
		     (setf colonp t)))
		((char= char #\@)
		 (if atsignp
		     (error 'format-error
			    :complaint (intl:gettext "Too many at-signs supplied.")
			    :control-string string
			    :offset posn)
		     (setf atsignp t)))
		(t
		 (return))))
	(incf posn))
      (let ((char (get-char)))
	(when (char= char #\/)
	  (let ((closing-slash (position #\/ string :start (1+ posn))))
	    (if closing-slash
		(setf posn closing-slash)
		(error 'format-error
		       :complaint (intl:gettext "No matching closing slash.")
		       :control-string string
		       :offset posn))))
	(make-format-directive
	    :string string :start start :end (1+ posn)
	    :character (char-upcase char)
	    :colonp colonp :atsignp atsignp
	    :params (nreverse params))))))


;;;; Specials used to communicate information.

;;; *UP-UP-AND-OUT-ALLOWED* -- internal.
;;;
;;; Used both by the expansion stuff and the interpreter stuff.  When it is
;;; non-NIL, up-up-and-out (~:^) is allowed.  Otherwise, ~:^ isn't allowed.
;;;
(defvar *up-up-and-out-allowed* nil)

;;; *LOGICAL-BLOCK-POPPER* -- internal.
;;;
;;; Used by the interpreter stuff.  When it non-NIL, its a function that will
;;; invoke PPRINT-POP in the right lexical environemnt.
;;;
(defvar *logical-block-popper* nil)

;;; *EXPANDER-NEXT-ARG-MACRO* -- internal.
;;;
;;; Used by the expander stuff.  This is bindable so that ~<...~:>
;;; can change it.
;;;
(defvar *expander-next-arg-macro* 'expander-next-arg)

;;; *ONLY-SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
;;; 
(defvar *only-simple-args*)

;;; *ORIG-ARGS-AVAILABLE* -- internal.
;;;
;;; Used by the expander stuff.  We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (throw 'need-orig-args nil) and we try
;;; again with it bound to T.  If this is T, we don't try to do anything
;;; fancy with args.
;;; 
(defvar *orig-args-available* nil)

;;; *SIMPLE-ARGS* -- internal.
;;;
;;; Used by the expander stuff.  List of (symbol . offset) for simple args.
;;; 
(defvar *simple-args*)




;;;; FORMAT

(defun format (destination control-string &rest format-arguments)
  "Provides various facilities for formatting output.
  CONTROL-STRING contains a string to be output, possibly with embedded
  directives, which are flagged with the escape character \"~\".  Directives
  generally expand into additional text to be output, usually consuming one
  or more of the FORMAT-ARGUMENTS in the process.  A few useful directives
  are:
        ~A or ~nA     Prints one argument as if by PRINC
        ~S or ~nS     Prints one argument as if by PRIN1
        ~D or ~nD     Prints one argument as a decimal integer
        ~%            Does a TERPRI
        ~&            Does a FRESH-LINE

         where n is the width of the field in which the object is printed.
  
  DESTINATION controls where the result will go.  If DESTINATION is T, then
  the output is sent to the standard output stream.  If it is NIL, then the
  output is returned in a string as the value of the call.  Otherwise,
  DESTINATION must be a stream to which the output will be sent.

  Example:   (FORMAT NIL \"The answer is ~D.\" 10) => \"The answer is 10.\"

  FORMAT has many additional capabilities not described here.  Consult
  Section 22.3 (Formatted Output) of the ANSI Common Lisp standard for
  details."
  (etypecase destination
    (null
     (with-output-to-string (stream)
       (%format stream control-string format-arguments)))
    (string
     (with-output-to-string (stream destination)
       (%format stream control-string format-arguments)))
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    (stream
     (%format destination control-string format-arguments)
     nil)))

(defun %format (stream string-or-fun orig-args &optional (args orig-args))
  (if (functionp string-or-fun)
      (apply string-or-fun stream args)
      (catch 'up-and-out
	(let* ((string (etypecase string-or-fun
			 (simple-string
			  string-or-fun)
			 (string
			  (coerce string-or-fun 'simple-string))))
	       (*default-format-error-control-string* string)
	       (*logical-block-popper* nil))
	  (interpret-directive-list stream (tokenize-control-string string)
				    orig-args args)))))

(defun interpret-directive-list (stream directives orig-args args)
  (if directives
      (let ((directive (car directives)))
	(etypecase directive
	  (simple-string
	   (write-string directive stream)
	   (interpret-directive-list stream (cdr directives) orig-args args))
	  (format-directive
	   (multiple-value-bind
	       (new-directives new-args)
	       (let ((function
		      (svref *format-directive-interpreters*
			     (char-code (format-directive-character
					 directive))))
		     (*default-format-error-offset*
		      (1- (format-directive-end directive))))
		 (unless function
		   (error 'format-error
			  :complaint (intl:gettext "Unknown format directive.")))
		 (multiple-value-bind
		     (new-directives new-args)
		     (funcall function stream directive
			      (cdr directives) orig-args args)
		   (values new-directives new-args)))
	     (interpret-directive-list stream new-directives
				       orig-args new-args)))))
      args))


;;;; FORMATTER

(defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  (block nil
    (catch 'need-orig-args
      (let* ((*simple-args* nil)
	     (*only-simple-args* t)
	     (guts (expand-control-string control-string))
	     (args nil))
	(dolist (arg *simple-args*)
	  (push `(,(car arg)
		  (error
		   'format-error
		   :complaint (intl:gettext "Required argument missing")
		   :control-string ,control-string
		   :offset ,(cdr arg)))
		args))
	(return `(lambda (stream &optional ,@args &rest args)
		   ,guts
		   args))))
    (let ((*orig-args-available* t)
	  (*only-simple-args* nil))
      `(lambda (stream &rest orig-args)
	 (let ((args orig-args))
	   ,(expand-control-string control-string)
	   args)))))

(defun expand-control-string (string)
  (let* ((string (etypecase string
		   (simple-string
		    string)
		   (string
		    (coerce string 'simple-string))))
	 (*default-format-error-control-string* string)
	 (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
	(remaining-directives directives))
    (loop
      (unless remaining-directives
	(return))
      (multiple-value-bind
	  (form new-directives)
	  (expand-directive (car remaining-directives)
			    (cdr remaining-directives))
	(when form
          (push form results))
	(setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
	    (aref *format-directive-expanders*
		  (char-code (format-directive-character directive))))
	   (*default-format-error-offset*
	    (1- (format-directive-end directive))))
       (if expander
	   (funcall expander directive more-directives)
	   (error 'format-error
		  :complaint (intl:gettext "Unknown directive.")))))
    (simple-string
     (values `(write-string ,directive stream)
	     more-directives))))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
	,*default-format-error-control-string*
	,(or offset *default-format-error-offset*))
      (let ((symbol (gensym "FORMAT-ARG-")))
	(push (cons symbol (or offset *default-format-error-offset*))
	      *simple-args*)
	symbol)))

(defun need-hairy-args ()
  (when *only-simple-args*
    ))


;;;; Format directive definition macros and runtime support.

(defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
	      :complaint (intl:gettext "No more arguments.")
	      :control-string ,string
	      :offset ,offset)))

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
	      :complaint (intl:gettext "No more arguments.")
	      :control-string ,string
	      :offset ,offset))
     (pprint-pop)
     (pop args)))

(eval-when (:compile-toplevel :execute)

;;; NEXT-ARG -- internal.
;;;
;;; This macro is used to extract the next argument from the current arg list.
;;; This is the version used by format directive interpreters.
;;; 
(defmacro next-arg (&optional offset)
  `(progn
     (when (null args)
       (error 'format-error
	      :complaint (intl:gettext "No more arguments.")
	      ,@(when offset
		  `(:offset ,offset))))
     (when *logical-block-popper*
       (funcall *logical-block-popper*))
     (pop args)))

(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (cl:format nil
				       "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
				       char)))
	(directive (gensym))
	(directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (,directive ,directives)
	 ,@(if lambda-list
	       `((let ,(mapcar #'(lambda (var)
				   `(,var
				     (,(intern (concatenate
						'string
						"FORMAT-DIRECTIVE-"
						(symbol-name var))
					       (symbol-package 'foo))
				      ,directive)))
			       (butlast lambda-list))
		   ,@body))
	       `((declare (ignore ,directive ,directives))
		 ,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym))
	(declarations nil)
	(body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
	(unless (and (consp form) (eq (car form) 'declare))
	  (return))
	(push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
	       ,directives))))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
	(collect ((expander-bindings) (runtime-bindings))
		 (dolist (spec specs)
		   (destructuring-bind (var default) spec
		     (let ((symbol (gensym)))
		       (expander-bindings
			`(,var ',symbol))
		       (runtime-bindings
			`(list ',symbol
			       (let* ((param-and-offset (pop ,params))
				      (offset (car param-and-offset))
				      (param (cdr param-and-offset)))
				 (case param
				   (:arg `(or ,(expand-next-arg offset)
					      ,,default))
				   (:remaining
				    (setf *only-simple-args* nil)
				    '(length args))
				   ((nil) ,default)
				   (t param))))))))
		 `(let ,(expander-bindings)
		    `(let ,(list ,@(runtime-bindings))
		       ,@(if ,params
			     (error 'format-error
				    :complaint
				    (intl:gettext "Too many parameters, expected no more than ~D")
				    :arguments (list ,(length specs))
				    :offset (caar ,params)))
		       ,,@body)))
	`(progn
	   (when ,params
	     (error 'format-error
		    :complaint (intl:gettext "Too many parameters, expected no more than 0")
		    :offset (caar ,params)))
	   ,@body))))

(defmacro def-complex-format-interpreter (char lambda-list &body body)
  (let ((defun-name
	    (intern (cl:format nil "~:@(~:C~)-FORMAT-DIRECTIVE-INTERPRETER"
			       char)))
	(directive (gensym))
	(directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (stream ,directive ,directives orig-args args)
	 (declare (ignorable stream orig-args args))
	 ,@(if lambda-list
	       `((let ,(mapcar #'(lambda (var)
				   `(,var
				     (,(intern (concatenate
						'string
						"FORMAT-DIRECTIVE-"
						(symbol-name var))
					       (symbol-package 'foo))
				      ,directive)))
			       (butlast lambda-list))
		   (values (progn ,@body) args)))
	       `((declare (ignore ,directive ,directives))
		 ,@body)))
       (%set-format-directive-interpreter ,char #',defun-name))))

(defmacro def-format-interpreter (char lambda-list &body body)
  (let ((directives (gensym)))
    `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
       ,@body
       ,directives)))

(defmacro interpret-bind-defaults (specs params &body body)
  (once-only ((params params))
    (collect ((bindings))
      (dolist (spec specs)
	(destructuring-bind (var default) spec
	  (bindings `(,var (let* ((param-and-offset (pop ,params))
				  (offset (car param-and-offset))
				  (param (cdr param-and-offset)))
			     (case param
			       (:arg
				;; If the value of ~V is NIL, it's the
				;; same as if it weren't given at all.
				;; See CLHS 22.3.
				(or (next-arg offset) ,default))
			       (:remaining (length args))
			       ((nil) ,default)
			       (t param)))))))
      `(let* ,(bindings)
	 (when ,params
	   (error 'format-error
		  :complaint
		  (intl:gettext "Too many parameters, expected no more than ~D")
		  :arguments (list ,(length specs))
		  :offset (caar ,params)))
	 ,@body))))

); eval-when

(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
  char)

(defun %set-format-directive-interpreter (char fn)
  (setf (aref *format-directive-interpreters*
	      (char-code (char-upcase char)))
	fn)
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
	(if (format-directive-p next)
	    (let ((char (format-directive-character next)))
	      (if (or (char= kind char)
		      (and stop-at-semi (char= char #\;)))
		  (car directives)
		  (find-directive
		   (cdr (flet ((after (char)
				 (member (find-directive (cdr directives)
							 char
							 nil)
					 directives)))
			  (case char
			    (#\( (after #\)))
			    (#\< (after #\>))
			    (#\[ (after #\]))
			    (#\{ (after #\}))
			    (t directives))))
		   kind stop-at-semi)))
	    (find-directive (cdr directives) kind stop-at-semi)))))


;;;; Simple outputting noise.

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft
    (write-string string stream))
  (dotimes (i minpad)
    (write-char padchar stream))
  (and mincol minpad colinc
       (do ((chars (+ (length string) (max 0 minpad)) (+ chars colinc)))
	   ((>= chars mincol))
	 (dotimes (i colinc)
	   (write-char padchar stream))))
  (when padleft
    (write-string string stream)))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (princ-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-directive #\A (colonp atsignp params)
  (if params
      (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
			     (padchar #\space))
		     params
	`(format-princ stream ,(expand-next-arg) ',colonp ',atsignp
		       ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp
		   `(or ,(expand-next-arg) "()")
		   (expand-next-arg))
	      stream)))

(def-format-interpreter #\A (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				(padchar #\space))
		     params
	(format-princ stream (next-arg) colonp atsignp
		      mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (prin1-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
	 (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				(padchar #\space))
			params
	   `(format-prin1 stream ,(expand-next-arg) ,colonp ,atsignp
			  ,mincol ,colinc ,minpad ,padchar)))
	(colonp
	 `(let ((arg ,(expand-next-arg)))
	    (if arg
		(prin1 arg stream)
		(princ "()" stream))))
	(t
	 `(prin1 ,(expand-next-arg) stream))))

(def-format-interpreter #\S (colonp atsignp params)
  (cond (params
	 (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				   (padchar #\space))
			params
	   (format-prin1 stream (next-arg) colonp atsignp
			 mincol colinc minpad padchar)))
	(colonp
	 (let ((arg (next-arg)))
	   (if arg
	       (prin1 arg stream)
	       (princ "()" stream))))
	(t
	 (prin1 (next-arg) stream))))

(def-format-directive #\C (colonp atsignp params)
  (expand-bind-defaults () params
    (if colonp
	`(format-print-named-character ,(expand-next-arg) stream)
	(if atsignp
	    `(prin1 ,(expand-next-arg) stream)
	    `(write-char ,(expand-next-arg) stream)))))

(def-format-interpreter #\C (colonp atsignp params)
  (interpret-bind-defaults () params
    (if colonp
	(format-print-named-character (next-arg) stream)
	(if atsignp
	    (prin1 (next-arg) stream)
	    (write-char (next-arg) stream)))))

#-unicode
(defun format-print-named-character (char stream)
  (let* ((name (char-name char)))
    (cond (name
	   (write-string name stream))
	  ((<= 0 (char-code char) 31)
	   ;; Print control characters as "^"<char>
	   (write-char #\^ stream)
	   (write-char (code-char (+ 64 (char-code char))) stream))
	  (t
	   (write-char char stream)))))

#+unicode
(defun format-print-named-character (char stream)
  (cond ((and (graphic-char-p char)
	      (char/= char #\space))
	 ;; Graphic characters (except space) print the same as ~C.
	 (write-char char stream))
	(t
	 (let* ((name (char-name char)))
	   (write-string name stream)))))

(def-format-directive #\W (colonp atsignp params)
  (expand-bind-defaults () params
    (if (or colonp atsignp)
	`(let (,@(when colonp
		   '((*print-pretty* t)))
	       ,@(when atsignp
		   '((*print-level* nil)
		     (*print-length* nil))))
	   (output-object ,(expand-next-arg) stream))
	`(output-object ,(expand-next-arg) stream))))

(def-format-interpreter #\W (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((*print-pretty* (or colonp *print-pretty*))
	  (*print-level* (and atsignp *print-level*))
	  (*print-length* (and atsignp *print-length*)))
      (output-object (next-arg) stream))))


;;;; Integer outputting.

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
;;;
(defun format-print-integer (stream number print-commas-p print-sign-p
			     radix mincol padchar commachar commainterval)
  (let ((*print-base* radix)
	(*print-radix* nil))
    (if (integerp number)
	(let* ((text (princ-to-string (abs number)))
	       (commaed (if print-commas-p
			    (format-add-commas text commachar commainterval)
			    text))
	       (signed (cond ((minusp number)
			      (concatenate 'string "-" commaed))
			     (print-sign-p
			      (concatenate 'string "+" commaed))
			     (t commaed))))
	  ;; colinc = 1, minpad = 0, padleft = t
	  (format-write-field stream signed mincol 1 0 padchar t))
	(princ number stream))))

(defun format-add-commas (string commachar commainterval)
  (let ((length (length string)))
    (multiple-value-bind (commas extra)
			 (truncate (1- length) commainterval)
      (let ((new-string (make-string (+ length commas)))
	    (first-comma (1+ extra)))
	(replace new-string string :end1 first-comma :end2 first-comma)
	(do ((src first-comma (+ src commainterval))
	     (dst first-comma (+ dst commainterval 1)))
	    ((= src length))
	  (setf (schar new-string dst) commachar)
	  (replace new-string string :start1 (1+ dst)
		   :start2 src :end2 (+ src commainterval)))
	new-string))))

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
	  ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
	  params
	`(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
			       ,base ,mincol ,padchar ,commachar
			       ,commainterval))
      `(write ,(expand-next-arg) :stream stream :base ,base :radix nil
	      :escape nil)))

(defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
	   ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
	   params
	 (format-print-integer stream (next-arg) colonp atsignp ,base mincol
			       padchar commachar commainterval))
       (write (next-arg) :stream stream :base ,base :radix nil :escape nil)))

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-interpreter #\D (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-interpreter #\B (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-interpreter #\O (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-interpreter #\X (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-directive #\R (colonp atsignp params)
  (if params
      (expand-bind-defaults
	  ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
	   (commainterval 3))
	  params
	(let ((r-arg (gensym "R-ARG-")))
	  `(let ((,r-arg ,(expand-next-arg)))
	     (if ,base
		 (format-print-integer stream ,r-arg ,colonp ,atsignp
				       ,base ,mincol
				       ,padchar ,commachar ,commainterval)
		 (format-print-cardinal stream ,r-arg)))))
      (if atsignp
	  (if colonp
	      `(format-print-old-roman stream ,(expand-next-arg))
	      `(format-print-roman stream ,(expand-next-arg)))
	  (if colonp
	      `(format-print-ordinal stream ,(expand-next-arg))
	      `(format-print-cardinal stream ,(expand-next-arg))))))

(def-format-interpreter #\R (colonp atsignp params)
  (if params
      (interpret-bind-defaults
	  ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
	   (commainterval 3))
	  params
	(if base
	    (format-print-integer stream (next-arg) colonp atsignp base mincol
				  padchar commachar commainterval)
	    (format-print-cardinal stream (next-arg))))
      (if atsignp
	  (if colonp
	      (format-print-old-roman stream (next-arg))
	      (format-print-roman stream (next-arg)))
	  (if colonp
	      (format-print-ordinal stream (next-arg))
	      (format-print-cardinal stream (next-arg))))))


(defconstant cardinal-ones
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defconstant cardinal-tens
  #(nil nil "twenty" "thirty" "forty"
	"fifty" "sixty" "seventy" "eighty" "ninety"))

(defconstant cardinal-teens
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

;; See http://en.wikipedia.org/wiki/Names_of_large_numbers and also
;; http://home.hetnet.nl/~vanadovv/BignumbyN.html.  This list comes
;; from the latter link.
;;
;; Leading spaces are required to get everything printed out
;; correctly.
(defconstant cardinal-periods
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
    " quintillion" " sextillion" " septillion" " octillion" " nonillion"
    " decillion" " undecillion" " duodecillion" " tredecillion"
    " quattuordecillion" " quinquadecillion" " sedecillion" " septendecillion"
    " octodecillion" " novendecillion" " vigintillion" " unvigintillion"
    " duovigintillion" " tresvigintillion" " quattuorvigintillion"
    " quinquavigintillion" " sesvigintillion" " septemvigintillion"
    " octovigintillion" " novemvigintillion" " trigintillion"
    " untrigintillion" " duotrigintillion" " trestrigintillion"
    " quattuortrigintillion" " quinquatrigintillion" " sestrigintillion"
    " septentrigintillion" " octotrigintillion" " noventrigintillion"
    " quadragintillion" " unquadragintillion" " duoquadragintillion"
    " tresquadragintillion" " quattuorquadragintillion"
    " quinquaquadragintillion" " sesquadragintillion" " septenquadragintillion"
    " octoquadragintillion" " novenquadragintillion" " quinquagintillion"
    " unquinquagintillion" " duoquinquagintillion" " tresquinquagintillion"
    " quattuorquinquagintillion" " quinquaquinquagintillion"
    " sesquinquagintillion" " septenquinquagintillion" " octoquinquagintillion"
    " novenquinquagintillion" " sexagintillion" " unsexagintillion"
    " duosexagintillion" " tresexagintillion" " quattuorsexagintillion"
    " quinquasexagintillion" " sesexagintillion" " septensexagintillion"
    " octosexagintillion" " novensexagintillion" " septuagintillion"
    " unseptuagintillion" " duoseptuagintillion" " treseptuagintillion"
    " quattuorseptuagintillion" " quinquaseptuagintillion"
    " seseptuagintillion" " septenseptuagintillion" " octoseptuagintillion"
    " novenseptuagintillion" " octogintillion" " unoctogintillion"
    " duooctogintillion" " tresoctogintillion" " quattuoroctogintillion"
    " quinquaoctogintillion" " sexoctogintillion" " septemoctogintillion"
    " octooctogintillion" " novemoctogintillion" " nonagintillion"
    " unnonagintillion" " duononagintillion" " trenonagintillion"
    " quattuornonagintillion" " quinquanonagintillion" " senonagintillion"
    " septenonagintillion" " octononagintillion" " novenonagintillion"
    " centillion" " uncentillion" " duocentillion" " trescentillion"
    " quattuorcentillion" " quinquacentillion" " sexcentillion"
    " septencentillion" " octocentillion" " novencentillion" " decicentillion"
    " undecicentillion" " duodecicentillion" " tredecicentillion"
    " quattuordecicentillion" " quinquadecicentillion" " sedecicentillion"
    " septendecicentillion" " octodecicentillion" " novendecicentillion"
    " viginticentillion" " unviginticentillion" " duoviginticentillion"
    " tresviginticentillion" " quattuorviginticentillion"
    " quinquaviginticentillion" " sesviginticentillion"
    " septemviginticentillion" " octoviginticentillion"
    " novemviginticentillion" " trigintacentillion" " untrigintacentillion"
    " duotrigintacentillion" " trestrigintacentillion"
    " quattuortrigintacentillion" " quinquatrigintacentillion"
    " sestrigintacentillion" " septentrigintacentillion"
    " octotrigintacentillion" " noventrigintacentillion"
    " quadragintacentillion" " unquadragintacentillion"
    " duoquadragintacentillion" " tresquadragintacentillion"
    " quattuorquadragintacentillion" " quinquaquadragintacentillion"
    " sesquadragintacentillion" " septenquadragintacentillion"
    " octoquadragintacentillion" " novenquadragintacentillion"
    " quinquagintacentillion" " unquinquagintacentillion"
    " duoquinquagintacentillion" " tresquinquagintacentillion"
    " quattuorquinquagintacentillion" " quinquaquinquagintacentillion"
    " sesquinquagintacentillion" " septenquinquagintacentillion"
    " octoquinquagintacentillion" " novenquinquagintacentillion"
    " sexagintacentillion" " unsexagintacentillion" " duosexagintacentillion"
    " tresexagintacentillion" " quattuorsexagintacentillion"
    " quinquasexagintacentillion" " sesexagintacentillion"
    " septensexagintacentillion" " octosexagintacentillion"
    " novensexagintacentillion" " septuagintacentillion"
    " unseptuagintacentillion" " duoseptuagintacentillion"
    " treseptuagintacentillion" " quattuorseptuagintacentillion"
    " quinquaseptuagintacentillion" " seseptuagintacentillion"
    " septenseptuagintacentillion" " octoseptuagintacentillion"
    " novenseptuagintacentillion" " octogintacentillion"
    " unoctogintacentillion" " duooctogintacentillion"
    " tresoctogintacentillion" " quattuoroctogintacentillion"
    " quinquaoctogintacentillion" " sexoctogintacentillion"
    " septemoctogintacentillion" " octooctogintacentillion"
    " novemoctogintacentillion" " nonagintacentillion" " unnonagintacentillion"
    " duononagintacentillion" " trenonagintacentillion"
    " quattuornonagintacentillion" " quinquanonagintacentillion"
    " senonagintacentillion" " septenonagintacentillion"
    " octononagintacentillion" " novenonagintacentillion" " ducentillion"
    " unducentillion" " duoducentillion" " treducentillion"
    " quattuorducentillion" " quinquaducentillion" " seducentillion"
    " septenducentillion" " octoducentillion" " novenducentillion"
    " deciducentillion" " undeciducentillion" " duodeciducentillion"
    " tredeciducentillion" " quattuordeciducentillion"
    " quinquadeciducentillion" " sedeciducentillion" " septendeciducentillion"
    " octodeciducentillion" " novendeciducentillion" " vigintiducentillion"
    " unvigintiducentillion" " duovigintiducentillion"
    " tresvigintiducentillion" " quattuorvigintiducentillion"
    " quinquavigintiducentillion" " sesvigintiducentillion"
    " septemvigintiducentillion" " octovigintiducentillion"
    " novemvigintiducentillion" " trigintaducentillion"
    " untrigintaducentillion" " duotrigintaducentillion"
    " trestrigintaducentillion" " quattuortrigintaducentillion"
    " quinquatrigintaducentillion" " sestrigintaducentillion"
    " septentrigintaducentillion" " octotrigintaducentillion"
    " noventrigintaducentillion" " quadragintaducentillion"
    " unquadragintaducentillion" " duoquadragintaducentillion"
    " tresquadragintaducentillion" " quattuorquadragintaducentillion"
    " quinquaquadragintaducentillion" " sesquadragintaducentillion"
    " septenquadragintaducentillion" " octoquadragintaducentillion"
    " novenquadragintaducentillion" " quinquagintaducentillion"
    " unquinquagintaducentillion" " duoquinquagintaducentillion"
    " tresquinquagintaducentillion" " quattuorquinquagintaducentillion"
    " quinquaquinquagintaducentillion" " sesquinquagintaducentillion"
    " septenquinquagintaducentillion" " octoquinquagintaducentillion"
    " novenquinquagintaducentillion" " sexagintaducentillion"
    " unsexagintaducentillion" " duosexagintaducentillion"
    " tresexagintaducentillion" " quattuorsexagintaducentillion"
    " quinquasexagintaducentillion" " sesexagintaducentillion"
    " septensexagintaducentillion" " octosexagintaducentillion"
    " novensexagintaducentillion" " septuagintaducentillion"
    " unseptuagintaducentillion" " duoseptuagintaducentillion"
    " treseptuagintaducentillion" " quattuorseptuagintaducentillion"
    " quinquaseptuagintaducentillion" " seseptuagintaducentillion"
    " septenseptuagintaducentillion" " octoseptuagintaducentillion"
    " novenseptuagintaducentillion" " octogintaducentillion"
    " unoctogintaducentillion" " duooctogintaducentillion"
    " tresoctogintaducentillion" " quattuoroctogintaducentillion"
    " quinquaoctogintaducentillion" " sexoctogintaducentillion"
    " septemoctogintaducentillion" " octooctogintaducentillion"
    " novemoctogintaducentillion" " nonagintaducentillion"
    " unnonagintaducentillion" " duononagintaducentillion"
    " trenonagintaducentillion" " quattuornonagintaducentillion"
    " quinquanonagintaducentillion" " senonagintaducentillion"
    " septenonagintaducentillion" " octononagintaducentillion"
    " novenonagintaducentillion" " trecentillion" " untrecentillion"
    " duotrecentillion" " trestrecentillion" " quattuortrecentillion"
    " quinquatrecentillion" " sestrecentillion" " septentrecentillion"
    " octotrecentillion" " noventrecentillion" " decitrecentillion"
    " undecitrecentillion" " duodecitrecentillion" " tredecitrecentillion"
    " quattuordecitrecentillion" " quinquadecitrecentillion"
    " sedecitrecentillion" " septendecitrecentillion" " octodecitrecentillion"
    " novendecitrecentillion" " vigintitrecentillion" " unvigintitrecentillion"
    " duovigintitrecentillion" " tresvigintitrecentillion"
    " quattuorvigintitrecentillion" " quinquavigintitrecentillion"
    " sesvigintitrecentillion" " septemvigintitrecentillion"
    " octovigintitrecentillion" " novemvigintitrecentillion"
    " trigintatrecentillion" " untrigintatrecentillion"
    " duotrigintatrecentillion" " trestrigintatrecentillion"
    " quattuortrigintatrecentillion" " quinquatrigintatrecentillion"
    " sestrigintatrecentillion" " septentrigintatrecentillion"
    " octotrigintatrecentillion" " noventrigintatrecentillion"
    " quadragintatrecentillion" " unquadragintatrecentillion"
    " duoquadragintatrecentillion" " tresquadragintatrecentillion"
    " quattuorquadragintatrecentillion" " quinquaquadragintatrecentillion"
    " sesquadragintatrecentillion" " septenquadragintatrecentillion"
    " octoquadragintatrecentillion" " novenquadragintatrecentillion"
    " quinquagintatrecentillion" " unquinquagintatrecentillion"
    " duoquinquagintatrecentillion" " tresquinquagintatrecentillion"
    " quattuorquinquagintatrecentillion" " quinquaquinquagintatrecentillion"
    " sesquinquagintatrecentillion" " septenquinquagintatrecentillion"
    " octoquinquagintatrecentillion" " novenquinquagintatrecentillion"
    " sexagintatrecentillion" " unsexagintatrecentillion"
    " duosexagintatrecentillion" " tresexagintatrecentillion"
    " quattuorsexagintatrecentillion" " quinquasexagintatrecentillion"
    " sesexagintatrecentillion" " septensexagintatrecentillion"
    " octosexagintatrecentillion" " novensexagintatrecentillion"
    " septuagintatrecentillion" " unseptuagintatrecentillion"
    " duoseptuagintatrecentillion" " treseptuagintatrecentillion"
    " quattuorseptuagintatrecentillion" " quinquaseptuagintatrecentillion"
    " seseptuagintatrecentillion" " septenseptuagintatrecentillion"
    " octoseptuagintatrecentillion" " novenseptuagintatrecentillion"
    " octogintatrecentillion" " unoctogintatrecentillion"
    " duooctogintatrecentillion" " tresoctogintatrecentillion"
    " quattuoroctogintatrecentillion" " quinquaoctogintatrecentillion"
    " sexoctogintatrecentillion" " septemoctogintatrecentillion"
    " octooctogintatrecentillion" " novemoctogintatrecentillion"
    " nonagintatrecentillion" " unnonagintatrecentillion"
    " duononagintatrecentillion" " trenonagintatrecentillion"
    " quattuornonagintatrecentillion" " quinquanonagintatrecentillion"
    " senonagintatrecentillion" " septenonagintatrecentillion"
    " octononagintatrecentillion" " novenonagintatrecentillion"
    " quadringentillion" " unquadringentillion" " duoquadringentillion"
    " tresquadringentillion" " quattuorquadringentillion"
    " quinquaquadringentillion" " sesquadringentillion"
    " septenquadringentillion" " octoquadringentillion"
    " novenquadringentillion" " deciquadringentillion"
    " undeciquadringentillion" " duodeciquadringentillion"
    " tredeciquadringentillion" " quattuordeciquadringentillion"
    " quinquadeciquadringentillion" " sedeciquadringentillion"
    " septendeciquadringentillion" " octodeciquadringentillion"
    " novendeciquadringentillion" " vigintiquadringentillion"
    " unvigintiquadringentillion" " duovigintiquadringentillion"
    " tresvigintiquadringentillion" " quattuorvigintiquadringentillion"
    " quinquavigintiquadringentillion" " sesvigintiquadringentillion"
    " septemvigintiquadringentillion" " octovigintiquadringentillion"
    " novemvigintiquadringentillion" " trigintaquadringentillion"
    " untrigintaquadringentillion" " duotrigintaquadringentillion"
    " trestrigintaquadringentillion" " quattuortrigintaquadringentillion"
    " quinquatrigintaquadringentillion" " sestrigintaquadringentillion"
    " septentrigintaquadringentillion" " octotrigintaquadringentillion"
    " noventrigintaquadringentillion" " quadragintaquadringentillion"
    " unquadragintaquadringentillion" " duoquadragintaquadringentillion"
    " tresquadragintaquadringentillion" " quattuorquadragintaquadringentillion"
    " quinquaquadragintaquadringentillion" " sesquadragintaquadringentillion"
    " septenquadragintaquadringentillion" " octoquadragintaquadringentillion"
    " novenquadragintaquadringentillion" " quinquagintaquadringentillion"
    " unquinquagintaquadringentillion" " duoquinquagintaquadringentillion"
    " tresquinquagintaquadringentillion"
    " quattuorquinquagintaquadringentillion"
    " quinquaquinquagintaquadringentillion" " sesquinquagintaquadringentillion"
    " septenquinquagintaquadringentillion" " octoquinquagintaquadringentillion"
    " novenquinquagintaquadringentillion" " sexagintaquadringentillion"
    " unsexagintaquadringentillion" " duosexagintaquadringentillion"
    " tresexagintaquadringentillion" " quattuorsexagintaquadringentillion"
    " quinquasexagintaquadringentillion" " sesexagintaquadringentillion"
    " septensexagintaquadringentillion" " octosexagintaquadringentillion"
    " novensexagintaquadringentillion" " septuagintaquadringentillion"
    " unseptuagintaquadringentillion" " duoseptuagintaquadringentillion"
    " treseptuagintaquadringentillion" " quattuorseptuagintaquadringentillion"
    " quinquaseptuagintaquadringentillion" " seseptuagintaquadringentillion"
    " septenseptuagintaquadringentillion" " octoseptuagintaquadringentillion"
    " novenseptuagintaquadringentillion" " octogintaquadringentillion"
    " unoctogintaquadringentillion" " duooctogintaquadringentillion"
    " tresoctogintaquadringentillion" " quattuoroctogintaquadringentillion"
    " quinquaoctogintaquadringentillion" " sexoctogintaquadringentillion"
    " septemoctogintaquadringentillion" " octooctogintaquadringentillion"
    " novemoctogintaquadringentillion" " nonagintaquadringentillion"
    " unnonagintaquadringentillion" " duononagintaquadringentillion"
    " trenonagintaquadringentillion" " quattuornonagintaquadringentillion"
    " quinquanonagintaquadringentillion" " senonagintaquadringentillion"
    " septenonagintaquadringentillion" " octononagintaquadringentillion"
    " novenonagintaquadringentillion" " quingentillion" " unquingentillion"
    " duoquingentillion" " tresquingentillion" " quattuorquingentillion"
    " quinquaquingentillion" " sesquingentillion" " septenquingentillion"
    " octoquingentillion" " novenquingentillion" " deciquingentillion"
    " undeciquingentillion" " duodeciquingentillion" " tredeciquingentillion"
    " quattuordeciquingentillion" " quinquadeciquingentillion"
    " sedeciquingentillion" " septendeciquingentillion"
    " octodeciquingentillion" " novendeciquingentillion"
    " vigintiquingentillion" " unvigintiquingentillion"
    " duovigintiquingentillion" " tresvigintiquingentillion"
    " quattuorvigintiquingentillion" " quinquavigintiquingentillion"
    " sesvigintiquingentillion" " septemvigintiquingentillion"
    " octovigintiquingentillion" " novemvigintiquingentillion"
    " trigintaquingentillion" " untrigintaquingentillion"
    " duotrigintaquingentillion" " trestrigintaquingentillion"
    " quattuortrigintaquingentillion" " quinquatrigintaquingentillion"
    " sestrigintaquingentillion" " septentrigintaquingentillion"
    " octotrigintaquingentillion" " noventrigintaquingentillion"
    " quadragintaquingentillion" " unquadragintaquingentillion"
    " duoquadragintaquingentillion" " tresquadragintaquingentillion"
    " quattuorquadragintaquingentillion" " quinquaquadragintaquingentillion"
    " sesquadragintaquingentillion" " septenquadragintaquingentillion"
    " octoquadragintaquingentillion" " novenquadragintaquingentillion"
    " quinquagintaquingentillion" " unquinquagintaquingentillion"
    " duoquinquagintaquingentillion" " tresquinquagintaquingentillion"
    " quattuorquinquagintaquingentillion" " quinquaquinquagintaquingentillion"
    " sesquinquagintaquingentillion" " septenquinquagintaquingentillion"
    " octoquinquagintaquingentillion" " novenquinquagintaquingentillion"
    " sexagintaquingentillion" " unsexagintaquingentillion"
    " duosexagintaquingentillion" " tresexagintaquingentillion"
    " quattuorsexagintaquingentillion" " quinquasexagintaquingentillion"
    " sesexagintaquingentillion" " septensexagintaquingentillion"
    " octosexagintaquingentillion" " novensexagintaquingentillion"
    " septuagintaquingentillion" " unseptuagintaquingentillion"
    " duoseptuagintaquingentillion" " treseptuagintaquingentillion"
    " quattuorseptuagintaquingentillion" " quinquaseptuagintaquingentillion"
    " seseptuagintaquingentillion" " septenseptuagintaquingentillion"
    " octoseptuagintaquingentillion" " novenseptuagintaquingentillion"
    " octogintaquingentillion" " unoctogintaquingentillion"
    " duooctogintaquingentillion" " tresoctogintaquingentillion"
    " quattuoroctogintaquingentillion" " quinquaoctogintaquingentillion"
    " sexoctogintaquingentillion" " septemoctogintaquingentillion"
    " octooctogintaquingentillion" " novemoctogintaquingentillion"
    " nonagintaquingentillion" " unnonagintaquingentillion"
    " duononagintaquingentillion" " trenonagintaquingentillion"
    " quattuornonagintaquingentillion" " quinquanonagintaquingentillion"
    " senonagintaquingentillion" " septenonagintaquingentillion"
    " octononagintaquingentillion" " novenonagintaquingentillion"
    " sescentillion" " unsescentillion" " duosescentillion" " tresescentillion"
    " quattuorsescentillion" " quinquasescentillion" " sesescentillion"
    " septensescentillion" " octosescentillion" " novensescentillion"
    " decisescentillion" " undecisescentillion" " duodecisescentillion"
    " tredecisescentillion" " quattuordecisescentillion"
    " quinquadecisescentillion" " sedecisescentillion"
    " septendecisescentillion" " octodecisescentillion"
    " novendecisescentillion" " vigintisescentillion" " unvigintisescentillion"
    " duovigintisescentillion" " tresvigintisescentillion"
    " quattuorvigintisescentillion" " quinquavigintisescentillion"
    " sesvigintisescentillion" " septemvigintisescentillion"
    " octovigintisescentillion" " novemvigintisescentillion"
    " trigintasescentillion" " untrigintasescentillion"
    " duotrigintasescentillion" " trestrigintasescentillion"
    " quattuortrigintasescentillion" " quinquatrigintasescentillion"
    " sestrigintasescentillion" " septentrigintasescentillion"
    " octotrigintasescentillion" " noventrigintasescentillion"
    " quadragintasescentillion" " unquadragintasescentillion"
    " duoquadragintasescentillion" " tresquadragintasescentillion"
    " quattuorquadragintasescentillion" " quinquaquadragintasescentillion"
    " sesquadragintasescentillion" " septenquadragintasescentillion"
    " octoquadragintasescentillion" " novenquadragintasescentillion"
    " quinquagintasescentillion" " unquinquagintasescentillion"
    " duoquinquagintasescentillion" " tresquinquagintasescentillion"
    " quattuorquinquagintasescentillion" " quinquaquinquagintasescentillion"
    " sesquinquagintasescentillion" " septenquinquagintasescentillion"
    " octoquinquagintasescentillion" " novenquinquagintasescentillion"
    " sexagintasescentillion" " unsexagintasescentillion"
    " duosexagintasescentillion" " tresexagintasescentillion"
    " quattuorsexagintasescentillion" " quinquasexagintasescentillion"
    " sesexagintasescentillion" " septensexagintasescentillion"
    " octosexagintasescentillion" " novensexagintasescentillion"
    " septuagintasescentillion" " unseptuagintasescentillion"
    " duoseptuagintasescentillion" " treseptuagintasescentillion"
    " quattuorseptuagintasescentillion" " quinquaseptuagintasescentillion"
    " seseptuagintasescentillion" " septenseptuagintasescentillion"
    " octoseptuagintasescentillion" " novenseptuagintasescentillion"
    " octogintasescentillion" " unoctogintasescentillion"
    " duooctogintasescentillion" " tresoctogintasescentillion"
    " quattuoroctogintasescentillion" " quinquaoctogintasescentillion"
    " sexoctogintasescentillion" " septemoctogintasescentillion"
    " octooctogintasescentillion" " novemoctogintasescentillion"
    " nonagintasescentillion" " unnonagintasescentillion"
    " duononagintasescentillion" " trenonagintasescentillion"
    " quattuornonagintasescentillion" " quinquanonagintasescentillion"
    " senonagintasescentillion" " septenonagintasescentillion"
    " octononagintasescentillion" " novenonagintasescentillion"
    " septingentillion" " unseptingentillion" " duoseptingentillion"
    " treseptingentillion" " quattuorseptingentillion"
    " quinquaseptingentillion" " seseptingentillion" " septenseptingentillion"
    " octoseptingentillion" " novenseptingentillion" " deciseptingentillion"
    " undeciseptingentillion" " duodeciseptingentillion"
    " tredeciseptingentillion" " quattuordeciseptingentillion"
    " quinquadeciseptingentillion" " sedeciseptingentillion"
    " septendeciseptingentillion" " octodeciseptingentillion"
    " novendeciseptingentillion" " vigintiseptingentillion"
    " unvigintiseptingentillion" " duovigintiseptingentillion"
    " tresvigintiseptingentillion" " quattuorvigintiseptingentillion"
    " quinquavigintiseptingentillion" " sesvigintiseptingentillion"
    " septemvigintiseptingentillion" " octovigintiseptingentillion"
    " novemvigintiseptingentillion" " trigintaseptingentillion"
    " untrigintaseptingentillion" " duotrigintaseptingentillion"
    " trestrigintaseptingentillion" " quattuortrigintaseptingentillion"
    " quinquatrigintaseptingentillion" " sestrigintaseptingentillion"
    " septentrigintaseptingentillion" " octotrigintaseptingentillion"
    " noventrigintaseptingentillion" " quadragintaseptingentillion"
    " unquadragintaseptingentillion" " duoquadragintaseptingentillion"
    " tresquadragintaseptingentillion" " quattuorquadragintaseptingentillion"
    " quinquaquadragintaseptingentillion" " sesquadragintaseptingentillion"
    " septenquadragintaseptingentillion" " octoquadragintaseptingentillion"
    " novenquadragintaseptingentillion" " quinquagintaseptingentillion"
    " unquinquagintaseptingentillion" " duoquinquagintaseptingentillion"
    " tresquinquagintaseptingentillion" " quattuorquinquagintaseptingentillion"
    " quinquaquinquagintaseptingentillion" " sesquinquagintaseptingentillion"
    " septenquinquagintaseptingentillion" " octoquinquagintaseptingentillion"
    " novenquinquagintaseptingentillion" " sexagintaseptingentillion"
    " unsexagintaseptingentillion" " duosexagintaseptingentillion"
    " tresexagintaseptingentillion" " quattuorsexagintaseptingentillion"
    " quinquasexagintaseptingentillion" " sesexagintaseptingentillion"
    " septensexagintaseptingentillion" " octosexagintaseptingentillion"
    " novensexagintaseptingentillion" " septuagintaseptingentillion"
    " unseptuagintaseptingentillion" " duoseptuagintaseptingentillion"
    " treseptuagintaseptingentillion" " quattuorseptuagintaseptingentillion"
    " quinquaseptuagintaseptingentillion" " seseptuagintaseptingentillion"
    " septenseptuagintaseptingentillion" " octoseptuagintaseptingentillion"
    " novenseptuagintaseptingentillion" " octogintaseptingentillion"
    " unoctogintaseptingentillion" " duooctogintaseptingentillion"
    " tresoctogintaseptingentillion" " quattuoroctogintaseptingentillion"
    " quinquaoctogintaseptingentillion" " sexoctogintaseptingentillion"
    " septemoctogintaseptingentillion" " octooctogintaseptingentillion"
    " novemoctogintaseptingentillion" " nonagintaseptingentillion"
    " unnonagintaseptingentillion" " duononagintaseptingentillion"
    " trenonagintaseptingentillion" " quattuornonagintaseptingentillion"
    " quinquanonagintaseptingentillion" " senonagintaseptingentillion"
    " septenonagintaseptingentillion" " octononagintaseptingentillion"
    " novenonagintaseptingentillion" " octingentillion" " unoctingentillion"
    " duooctingentillion" " tresoctingentillion" " quattuoroctingentillion"
    " quinquaoctingentillion" " sexoctingentillion" " septemoctingentillion"
    " octooctingentillion" " novemoctingentillion" " decioctingentillion"
    " undecioctingentillion" " duodecioctingentillion"
    " tredecioctingentillion" " quattuordecioctingentillion"
    " quinquadecioctingentillion" " sedecioctingentillion"
    " septendecioctingentillion" " octodecioctingentillion"
    " novendecioctingentillion" " vigintioctingentillion"
    " unvigintioctingentillion" " duovigintioctingentillion"
    " tresvigintioctingentillion" " quattuorvigintioctingentillion"
    " quinquavigintioctingentillion" " sesvigintioctingentillion"
    " septemvigintioctingentillion" " octovigintioctingentillion"
    " novemvigintioctingentillion" " trigintaoctingentillion"
    " untrigintaoctingentillion" " duotrigintaoctingentillion"
    " trestrigintaoctingentillion" " quattuortrigintaoctingentillion"
    " quinquatrigintaoctingentillion" " sestrigintaoctingentillion"
    " septentrigintaoctingentillion" " octotrigintaoctingentillion"
    " noventrigintaoctingentillion" " quadragintaoctingentillion"
    " unquadragintaoctingentillion" " duoquadragintaoctingentillion"
    " tresquadragintaoctingentillion" " quattuorquadragintaoctingentillion"
    " quinquaquadragintaoctingentillion" " sesquadragintaoctingentillion"
    " septenquadragintaoctingentillion" " octoquadragintaoctingentillion"
    " novenquadragintaoctingentillion" " quinquagintaoctingentillion"
    " unquinquagintaoctingentillion" " duoquinquagintaoctingentillion"
    " tresquinquagintaoctingentillion" " quattuorquinquagintaoctingentillion"
    " quinquaquinquagintaoctingentillion" " sesquinquagintaoctingentillion"
    " septenquinquagintaoctingentillion" " octoquinquagintaoctingentillion"
    " novenquinquagintaoctingentillion" " sexagintaoctingentillion"
    " unsexagintaoctingentillion" " duosexagintaoctingentillion"
    " tresexagintaoctingentillion" " quattuorsexagintaoctingentillion"
    " quinquasexagintaoctingentillion" " sesexagintaoctingentillion"
    " septensexagintaoctingentillion" " octosexagintaoctingentillion"
    " novensexagintaoctingentillion" " septuagintaoctingentillion"
    " unseptuagintaoctingentillion" " duoseptuagintaoctingentillion"
    " treseptuagintaoctingentillion" " quattuorseptuagintaoctingentillion"
    " quinquaseptuagintaoctingentillion" " seseptuagintaoctingentillion"
    " septenseptuagintaoctingentillion" " octoseptuagintaoctingentillion"
    " novenseptuagintaoctingentillion" " octogintaoctingentillion"
    " unoctogintaoctingentillion" " duooctogintaoctingentillion"
    " tresoctogintaoctingentillion" " quattuoroctogintaoctingentillion"
    " quinquaoctogintaoctingentillion" " sexoctogintaoctingentillion"
    " septemoctogintaoctingentillion" " octooctogintaoctingentillion"
    " novemoctogintaoctingentillion" " nonagintaoctingentillion"
    " unnonagintaoctingentillion" " duononagintaoctingentillion"
    " trenonagintaoctingentillion" " quattuornonagintaoctingentillion"
    " quinquanonagintaoctingentillion" " senonagintaoctingentillion"
    " septenonagintaoctingentillion" " octononagintaoctingentillion"
    " novenonagintaoctingentillion" " nongentillion" " unnongentillion"
    " duonongentillion" " trenongentillion" " quattuornongentillion"
    " quinquanongentillion" " senongentillion" " septenongentillion"
    " octonongentillion" " novenongentillion" " decinongentillion"
    " undecinongentillion" " duodecinongentillion" " tredecinongentillion"
    " quattuordecinongentillion" " quinquadecinongentillion"
    " sedecinongentillion" " septendecinongentillion" " octodecinongentillion"
    " novendecinongentillion" " vigintinongentillion" " unvigintinongentillion"
    " duovigintinongentillion" " tresvigintinongentillion"
    " quattuorvigintinongentillion" " quinquavigintinongentillion"
    " sesvigintinongentillion" " septemvigintinongentillion"
    " octovigintinongentillion" " novemvigintinongentillion"
    " trigintanongentillion" " untrigintanongentillion"
    " duotrigintanongentillion" " trestrigintanongentillion"
    " quattuortrigintanongentillion" " quinquatrigintanongentillion"
    " sestrigintanongentillion" " septentrigintanongentillion"
    " octotrigintanongentillion" " noventrigintanongentillion"
    " quadragintanongentillion" " unquadragintanongentillion"
    " duoquadragintanongentillion" " tresquadragintanongentillion"
    " quattuorquadragintanongentillion" " quinquaquadragintanongentillion"
    " sesquadragintanongentillion" " septenquadragintanongentillion"
    " octoquadragintanongentillion" " novenquadragintanongentillion"
    " quinquagintanongentillion" " unquinquagintanongentillion"
    " duoquinquagintanongentillion" " tresquinquagintanongentillion"
    " quattuorquinquagintanongentillion" " quinquaquinquagintanongentillion"
    " sesquinquagintanongentillion" " septenquinquagintanongentillion"
    " octoquinquagintanongentillion" " novenquinquagintanongentillion"
    " sexagintanongentillion" " unsexagintanongentillion"
    " duosexagintanongentillion" " tresexagintanongentillion"
    " quattuorsexagintanongentillion" " quinquasexagintanongentillion"
    " sesexagintanongentillion" " septensexagintanongentillion"
    " octosexagintanongentillion" " novensexagintanongentillion"
    " septuagintanongentillion" " unseptuagintanongentillion"
    " duoseptuagintanongentillion" " treseptuagintanongentillion"
    " quattuorseptuagintanongentillion" " quinquaseptuagintanongentillion"
    " seseptuagintanongentillion" " septenseptuagintanongentillion"
    " octoseptuagintanongentillion" " novenseptuagintanongentillion"
    " octogintanongentillion" " unoctogintanongentillion"
    " duooctogintanongentillion" " tresoctogintanongentillion"
    " quattuoroctogintanongentillion" " quinquaoctogintanongentillion"
    " sexoctogintanongentillion" " septemoctogintanongentillion"
    " octooctogintanongentillion" " novemoctogintanongentillion"
    " nonagintanongentillion" " unnonagintanongentillion"
    " duononagintanongentillion" " trenonagintanongentillion"
    " quattuornonagintanongentillion" " quinquanonagintanongentillion"
    " senonagintanongentillion" " septenonagintanongentillion"
    " octononagintanongentillion" " novenonagintanongentillion"))

(defconstant ordinal-ones
  #(nil "first" "second" "third" "fourth"
	"fifth" "sixth" "seventh" "eighth" "ninth")
  "Table of ordinal ones-place digits in English")

(defconstant ordinal-tens 
  #(nil "tenth" "twentieth" "thirtieth" "fortieth"
	"fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth")
  "Table of ordinal tens-place digits in English")

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind 
      (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref cardinal-ones hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
	(write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones)
			   (truncate rem 10)
       (cond ((< 1 tens)
	      (write-string (svref cardinal-tens tens) stream)
	      (when (plusp ones)
		(write-char #\- stream)
		(write-string (svref cardinal-ones ones) stream)))
	     ((= tens 1)
	      (write-string (svref cardinal-teens ones) stream))
	     ((plusp ones)
	      (write-string (svref cardinal-ones ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
	 (write-string "negative " stream)
	 (format-print-cardinal-aux stream (- n) 0 n))
	((zerop n)
	 (write-string "zero" stream))
	(t
	 (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (< period (length cardinal-periods))
      (error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
	(write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref cardinal-periods period) stream))))

(defun format-print-ordinal (stream n)
  (when (minusp n)
    (write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind
	(top bot) (truncate number 100)
      (unless (zerop top)
	(format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
	(write-char #\space stream))
      (multiple-value-bind
	  (tens ones) (truncate bot 10)
	(cond ((= bot 12) (write-string "twelfth" stream))
	      ((= tens 1)
	       (write-string (svref cardinal-teens ones) stream);;;RAD
	       (write-string "th" stream))
	      ((and (zerop tens) (plusp ones))
	       (write-string (svref ordinal-ones ones) stream))
	      ((and (zerop ones)(plusp tens))
	       (write-string (svref ordinal-tens tens) stream))
	      ((plusp bot)
	       (write-string (svref cardinal-tens tens) stream)
	       (write-char #\- stream)
	       (write-string (svref ordinal-ones ones) stream))
	      ((plusp number)
	       (write-string "th" stream))
	      (t
	       (write-string "zeroth" stream)))))))

;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error (intl:gettext "Number too large to print in old Roman numerals: ~:D") n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error (intl:gettext "Number too large to print in Roman numerals: ~:D") n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val)
		     (cond ((<= (- cur-val cur-sub-val) i)
			    (write-char cur-sub-char stream)
			    (write-char cur-char stream)
			    (- i (- cur-val cur-sub-val)))
			   (t i))))))
	  ((zerop start))))


;;;; Plural.

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
    (let ((arg (cond
		((not colonp)
		 (expand-next-arg))
		(*orig-args-available*
		 `(if (eq orig-args args)
		      (error 'format-error
			     :complaint (intl:gettext "No previous argument.")
			     :offset ,(1- end))
		      (do ((arg-ptr orig-args (cdr arg-ptr)))
			  ((eq (cdr arg-ptr) args)
			   (car arg-ptr)))))
		(*only-simple-args*
		 (unless *simple-args*
		   (error 'format-error
			  :complaint (intl:gettext "No previous argument.")))
		 (caar *simple-args*))
		(t
		 (throw 'need-orig-args nil)))))
      (if atsignp
	  `(write-string (if (eql ,arg 1) "y" "ies") stream)
	  `(unless (eql ,arg 1) (write-char #\s stream))))))

(def-format-interpreter #\P (colonp atsignp params)
  (interpret-bind-defaults () params
    (let ((arg (if colonp
		   (if (eq orig-args args)
		       (error 'format-error
			      :complaint "No previous argument.")
		       (do ((arg-ptr orig-args (cdr arg-ptr)))
			   ((eq (cdr arg-ptr) args)
			    (car arg-ptr))))
		   (next-arg))))
      (if atsignp
	  (write-string (if (eql arg 1) "y" "ies") stream)
	  (unless (eql arg 1) (write-char #\s stream))))))


;;;; Floating point noise.

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (expand-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
    `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-interpreter #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (interpret-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
			   params
    (format-fixed stream (next-arg) w d k ovf pad atsignp)))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-fixed-aux stream number w d k ovf pad atsign)
	  (if (rationalp number)
	      (format-fixed-aux stream
				(coerce number 'single-float)
				w d k ovf pad atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))


;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
;;;
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (declare (type float number))
  (cond
   ((and (floatp number)
	 (or (float-infinity-p number)
	     (float-nan-p number)))
    (prin1 number stream)
    nil)
   (t
    (let ((spaceleft w))
      (when (and w (or atsign (minusp (float-sign number))))
	(decf spaceleft))
      (multiple-value-bind (str len lpoint tpoint)
	  (lisp::flonum-to-string (abs number) :width spaceleft :fdigits d
				  :scale k :allow-overflow-p nil)
	;;if caller specifically requested no fraction digits, suppress the
	;;optional trailing zero
	(when (and d (zerop d)) (setq tpoint nil))
	(when w 
	  (decf spaceleft len)
	  ;;optional leading zero
	  (when lpoint
	    (if (or (> spaceleft 0) tpoint) ;force at least one digit
		(decf spaceleft)
		(setq lpoint nil)))
	  ;;optional trailing zero
	  (when tpoint
	    (if (> spaceleft 0)
		(decf spaceleft)
		(setq tpoint nil))))
	(cond ((and w (< spaceleft 0) ovf)
	       ;;field width overflow
	       (dotimes (i w) (write-char ovf stream))
	       t)
	      (t
	       (when w (dotimes (i spaceleft) (write-char pad stream)))
	       (if (minusp (float-sign number))
		   (write-char #\- stream)
		   (if atsign (write-char #\+ stream)))
	       (when lpoint (write-char #\0 stream))
	       (write-string str stream)
	       (when tpoint (write-char #\0 stream))
	       nil)))))))

(def-format-directive #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
			 ,atsignp)))

(def-format-interpreter #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (interpret-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    (format-exponential stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-exp-aux stream number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-exp-aux stream
			      (coerce number 'single-float)
			      w d e k ovf pad marker atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))


(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
	(single-float #\f)
	(double-float #\d)
	(short-float #\s)
	(long-float #\l)
	#+double-double
	(double-double-float #\w))))

;; This is a modified version of scale in FLONUM-TO-DIGITS.  We only
;; want the exponent, so most things not needed for the computation of
;; the exponent has been removed.  We also implemented the
;; floating-point log approximation given in Burger and Dybvig.  This
;; is very noticeably faster for large and small numbers.  It is
;; slower for intermediate sized numbers.
(defun accurate-scale-exponent (v)
  (declare (type float v))
  (if (zerop v)
      1
      (let ((float-radix 2)		; b
	    (float-digits (float-digits v)) ; p
	    (min-e
	     (etypecase v
	       (single-float lisp::single-float-min-e)
	       (double-float lisp::double-float-min-e)
	       #+double-double
	       (double-double-float lisp::double-double-float-min-e))))
	(multiple-value-bind (f e)
	    (integer-decode-float v)
	  (let ( ;; FIXME: these even tests assume normal IEEE rounding
		;; mode.  I wonder if we should cater for non-normal?
		(high-ok (evenp f)))
	    ;; This scale function is basically the same as the one in
	    ;; FLONUM-TO-DIGITS, except we don't return the computed
	    ;; digits.  We only want the exponent.
	    (labels ((flog (x)
		       (declare (type (float (0.0)) x))
		       (let ((xd (etypecase x
				   (single-float
				    (float x 1d0))
				   (double-float
				    x)
				   #+double-double
				   (double-double-float
				    (double-double-hi x)))))
			 (ceiling (- (the (double-float -400d0 400d0) (log xd 10d0))
				     1d-10))))
		     (fixup (r s m+ k)
		       (if (if high-ok
			       (>= (+ r m+) s)
			       (> (+ r m+) s))
			   (+ k 1)
			   k))
		     (scale (r s m+)
		       (let* ((est (flog v))
			      (scale (the integer (aref lisp::*powers-of-ten* (abs est)))))
			 (if (>= est 0)
			     (fixup r (* s scale) m+ est)
			     (fixup (* r scale) s (* m+ scale) est)))))
	      (let (r s m+)
		(if (>= e 0)
		    (let* ((be (expt float-radix e))
			   (be1 (* be float-radix)))
		      (if (/= f (expt float-radix (1- float-digits)))
			  (setf r (* f be 2)
				s 2
				m+ be)
			  (setf r (* f be1 2)
				s (* float-radix 2)
				m+ be1)))
		    (if (or (= e min-e) 
			    (/= f (expt float-radix (1- float-digits))))
			(setf r (* f 2)
			      s (* (expt float-radix (- e)) 2)
			      m+ 1)
			(setf r (* f float-radix 2)
			      s (* (expt float-radix (- 1 e)) 2)
			      m+ float-radix)))
		(scale r s m+))))))))

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these conditions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

;;; toy@rtp.ericsson.se:  The Hyperspec seems to say that the exponent
;;; marker is always printed.  Make it so.  Also, the original version
;;; causes errors when printing infinities or NaN's.  The Hyperspec is
;;; silent here, so let's just print out infinities and NaN's instead
;;; of causing an error.
(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (and (floatp number)
	   (or (float-infinity-p number)
	       (float-nan-p number)))
      (prin1 number stream)
      (let* ((num-expt (accurate-scale-exponent (abs number)))
	     (expt (if (zerop number)
		       0
		       (- num-expt k)))
	     (estr (decimal-string (abs expt)))
	     (elen (if e (max (length estr) e) (length estr)))
	     (add-zero-p nil))
	(if (and w ovf e (> elen e))	;exponent overflow
	    (dotimes (i w)
	      (write-char ovf stream))
	    ;; The hairy case
	    (let* ((fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
		   (fmin (if (minusp k)
			     1
			     fdig))
		   (spaceleft (if w
				  (- w 2 elen
				     (if (or atsign (minusp (float-sign number)))
					 1 0))
				  nil)))
	      #+(or)
	      (progn
		(format t "fdig = ~A~%" fdig)
		(format t "fmin = ~A~%" fmin)
		(format t "spaceleft = ~A~%" spaceleft)
		(format t "expt = ~S~%" expt))

	      (multiple-value-bind (fstr flen lpoint tpoint point-pos roundoff)
		  (lisp::flonum-to-string (abs number)
					  :width spaceleft
					  :fdigits fdig
					  :scale k
					  :fmin fmin
					  :num-expt num-expt)
		(declare (ignore point-pos))
		#+(or)
		(progn
		  (format t "fstr = ~S~%" fstr)
		  (format t "flen = ~S~%" flen)
		  (format t "lp   = ~S~%" lpoint)
		  (format t "tp   = ~S~%" tpoint))

		(when (and d (zerop d)) (setq tpoint nil))
		(when w 
		  (decf spaceleft flen)
		  ;; See CLHS 22.3.3.2.  "If the parameter d is
		  ;; omitted, ... [and] if the fraction to be
		  ;; printed is zero then a single zero digit should
		  ;; appear after the decimal point."  So we need to
		  ;; subtract one from here because we're going to
		  ;; add an extra 0 digit later.
		  (when (and (null d) (char= (aref fstr (1- flen)) #\.))
		    (setf add-zero-p t)
		    (decf spaceleft))
		  (when lpoint
		    (if (or (> spaceleft 0) tpoint)
			(decf spaceleft)
			(setq lpoint nil)))
		  (when (and tpoint (<= spaceleft 0))
		    (setq tpoint nil)))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf stream)))
		      (t (when w
			   (dotimes (i spaceleft)
			     (write-char pad stream)))
			 (if (minusp (float-sign number))
			     (write-char #\- stream)
			     (if atsign (write-char #\+ stream)))
			 (when lpoint (write-char #\0 stream))
			 (write-string fstr stream)
			 ;; Add a zero if we need it.  Which means
			 ;; we figured out we need one above, or
			 ;; another condition.  Basically, append a
			 ;; zero if there are no width constraints
			 ;; and if the last char to print was a
			 ;; decimal (so the trailing fraction is
			 ;; zero.)
			 (when (or add-zero-p
				   (and (null w)
					(char= (aref fstr (1- flen)) #\.)))
			   ;; It's later and we're adding the zero
			   ;; digit.
			   (write-char #\0 stream))
			 (write-char (if marker
					 marker
					 (format-exponent-marker number))
				     stream)
			 (when roundoff
			   ;; Printed result has rounded the number up
			   ;; so that the exponent is one too small.
			   ;; Increase our printed exponent.
			   (incf expt)
			   (setf estr (decimal-string (abs expt))))
			 (write-char (if (minusp expt) #\- #\+) stream)
			 (when e 
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr)))
			     (write-char #\0 stream)))
			 (write-string estr stream))))))))
  (values))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-general stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(def-format-interpreter #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify the colon modifier with this directive.")))
  (interpret-bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    (format-general stream (next-arg) w d e k ovf pad mark atsignp)))

(defun format-general (stream number w d e k ovf pad marker atsign)
  (if (numberp number)
      (if (floatp number)
	  (format-general-aux stream number w d e k ovf pad marker atsign)
	  (if (rationalp number)
	      (format-general-aux stream
				  (coerce number 'single-float)
				  w d e k ovf pad marker atsign)
	      (format-write-field stream
				  (decimal-string number)
				  w 1 0 #\space t)))
      (format-princ stream number nil nil w 1 0 pad)))


;;; toy@rtp.ericsson.se:  Same change as for format-exp-aux.
(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (if (and (floatp number)
	   (or (float-infinity-p number)
	       (float-nan-p number)))
      (prin1 number stream)
      (let* ((n (accurate-scale-exponent (abs number)))
	     (orig-d d))
	;; Default d if omitted.  The procedure is taken directly from
	;; the definition given in the manual (CLHS 22.3.3.3), and is
	;; not very efficient, since we generate the digits twice.
	;; Future maintainers are encouraged to improve on this.
	;;
	;; It's also not very clear whether q in the spec is the
	;; number of significant digits or not.  I (rtoy) think it
	;; makes more sense if q is the number of significant digits.
	;; That way 1d300 isn't printed as 1 followed by 300 zeroes.
	;; Exponential notation would be used instead.
	
	(unless d
	  ;; flonum-to-digits doesn't like 0.0, so handle the special
	  ;; case here.  Set d to n so that dd = 0 <= d to use ~F
	  ;; format.
	  (if (zerop number)
	      (setq d n)
	      (let* ((q (length (nth-value 1 (lisp::flonum-to-digits (abs number))))))
		(setq d (max q (min n 7))))))
	(let* ((ee (if e (+ e 2) 4))
	       (ww (if w (- w ee) nil))
	       (dd (- d n)))
	  #+(or)
	  (progn
	    (format t "d  = ~A~%" d)
	    (format t "ee = ~A~%" ee)
	    (format t "ww = ~A~%" ww)
	    (format t "dd = ~A~%" dd))
	  (cond ((<= 0 dd d)
		 ;; Use dd fraction digits, even if that would cause
		 ;; the width to be exceeded.  We choose accuracy over
		 ;; width in this case.
		 (let* ((fill-char (if (format-fixed-aux stream number ww
							 dd
							 nil
							 ovf pad atsign)
				       ovf
				       #\space)))
		   (dotimes (i ee) (write-char fill-char stream))))
		(t
		 (format-exp-aux stream number w
				 orig-d
				 e (or k 1)
				 ovf pad marker atsign)))))))

(def-format-directive #\$ (colonp atsignp params)
  (expand-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    `(format-dollars stream ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
		     ,atsignp)))

(def-format-interpreter #\$ (colonp atsignp params)
  (interpret-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    (format-dollars stream (next-arg) d n w pad colonp atsignp)))

(defun format-dollars (stream number d n w pad colon atsign)
  (if (rationalp number) (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (minusp (float-sign number)) "-" (if atsign "+" "")))
	     (signlen (length signstr)))
	(multiple-value-bind (str strlen ig2 ig3 pointplace)
	    (lisp::flonum-to-string number :width nil :fdigits d :scale nil)
	  (declare (ignore ig2 ig3))
	  (when colon (write-string signstr stream))
	  (dotimes (i (- w signlen (max 0 (- n pointplace)) strlen))
	    (write-char pad stream))
	  (unless colon (write-string signstr stream))
	  (dotimes (i (- n pointplace)) (write-char #\0 stream))
	  (write-string str stream)))
      (format-write-field stream
			  (decimal-string number)
			  w 1 0 #\space t)))


;;;; line/page breaks and other stuff like that.

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (terpri stream)))
      '(terpri stream)))

(def-format-interpreter #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (terpri stream))))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(progn
	   (when (plusp ,count)
	     (fresh-line stream)
	     (dotimes (i (1- ,count))
	       (terpri stream)))))
      '(fresh-line stream)))

(def-format-interpreter #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (interpret-bind-defaults ((count 1)) params
    (when (plusp count)
      (fresh-line stream)
      (dotimes (i (1- count))
	(terpri stream)))))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char #\page stream)))
      '(write-char #\page stream)))

(def-format-interpreter #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\page stream))))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-format-interpreter #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify either colon or atsign for this directive.")))
  (interpret-bind-defaults ((count 1)) params
    (dotimes (i count)
      (write-char #\~ stream))))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify both colon and atsign for this directive.")))
  (values (expand-bind-defaults () params
	    (if atsignp
		'(write-char #\newline stream)
		nil))
	  (if (and (not colonp)
		   directives
		   (simple-string-p (car directives)))
	      (cons (string-left-trim '(#\space #\newline #\tab)
				      (car directives))
		    (cdr directives))
	      directives)))

(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint
	   (intl:gettext "Cannot specify both colon and atsign for this directive.")))
  (interpret-bind-defaults () params
    (when atsignp
      (write-char #\newline stream)))
  (if (and (not colonp)
	   directives
	   (simple-string-p (car directives)))
      (cons (string-left-trim '(#\space #\newline #\tab)
			      (car directives))
	    (cdr directives))
      directives))


;;;; Tab and simple pretty-printing noise.

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (expand-bind-defaults ((n 1) (m 1)) params
	`(pprint-tab ,(if atsignp :section-relative :section)
		     ,n ,m stream))
      (if atsignp
	  (expand-bind-defaults ((colrel 1) (colinc 1)) params
	    `(format-relative-tab stream ,colrel ,colinc))
	  (expand-bind-defaults ((colnum 1) (colinc 1)) params
	    `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-interpreter #\T (colonp atsignp params)
  (if colonp
      (interpret-bind-defaults ((n 1) (m 1)) params
	(pprint-tab (if atsignp :section-relative :section) n m stream))
      (if atsignp
	  (interpret-bind-defaults ((colrel 1) (colinc 1)) params
	    (format-relative-tab stream colrel colinc))
	  (interpret-bind-defaults ((colnum 1) (colinc 1)) params
	    (format-absolute-tab stream colnum colinc)))))

(defun output-spaces (stream n)
  (let ((spaces #.(make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces))
	(return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

;; CLHS 22.3.6.1 for relative tabulations says:
;;
;;   ... outputs COLREL spaces and then outputs the smallest
;;   non-negative number of additional spaces necessary to move the
;;   cursor to a column that is a multiple of COLINC.... If the
;;   current output column cannot be determined, however, then colinc
;;   is ignored, and exactly colrel spaces are output.
(defun format-relative-tab (stream colrel colinc)
  (if (pp:pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (flet ((advance-to-column ()
	       (let* ((cur (lisp::charpos stream))
		      (spaces (if (and cur (plusp colinc))
				  (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
				  colrel)))
		 (output-spaces stream spaces))))
	(lisp::stream-dispatch stream
	  ;; simple-stream
	  (advance-to-column)
	  ;; lisp-stream
	  (advance-to-column)
	  ;; fundamental-stream
	  (let ((cur (stream-line-column stream)))
	    (cond ((and cur (plusp colinc))
		   (stream-advance-to-column stream
					     (+ cur
						(* (floor (+ cur colrel) colinc)
						   colinc))))
		  (t
		   (stream-advance-to-column stream (+ cur colrel)))))))))

;; CLHS 22.3.6.1 says:
;;
;;   If the cursor is already at or beyond the column COLNUM, it will
;;   output spaces to move it to COLNUM + k*COLINC for the smallest
;;   positive integer k possible, unless COLINC is zero, in which case
;;   no spaces are output.
(defun format-absolute-tab (stream colnum colinc)
  (if (pp:pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (flet ((advance-to-column ()
	       (let ((cur (lisp::charpos stream)))
		 (cond ((null cur)
			(write-string "  " stream))
		       ((< cur colnum)
			(output-spaces stream (- colnum cur)))
		       (t
			(unless (zerop colinc)
			  (output-spaces stream
					 (- colinc (rem (- cur colnum) colinc)))))))))
	(lisp::stream-dispatch stream
	  ;; simple-stream. NOTE: Do we need to do soemthing better for
	  ;; simple streams?
	  (advance-to-column)
	  ;; lisp-stream
	  (advance-to-column)
	  ;; fundamental-stream
	  (let ((cur (stream-line-column stream)))
	    (cond ((null cur)
		   (write-string "  " stream))
		  ((< cur colnum)
		   (stream-advance-to-column stream colnum))
		  (t
		   (unless (zerop colinc)
		     (let ((k (ceiling (- cur colnum) colinc)))
		       (stream-advance-to-column stream
						 (+ colnum (* k colinc))))))))))))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
			  (if atsignp
			      :mandatory
			      :fill)
			  (if atsignp
			      :miser
			      :linear))
		     stream)))

(def-format-interpreter #\_ (colonp atsignp params)
  (interpret-bind-defaults () params
    (pprint-newline (if colonp
			(if atsignp
			    :mandatory
			    :fill)
			(if atsignp
			    :miser
			    :linear))
		    stream)))

(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the at-sign modifier.")))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

(def-format-interpreter #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the at-sign modifier.")))
  (interpret-bind-defaults ((n 0)) params
    (pprint-indent (if colonp :current :block) n stream)))


;;;; *

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint (intl:gettext "Cannot specify both colon and at-sign."))
	  (expand-bind-defaults ((posn 0)) params
	    (unless *orig-args-available*
	      (throw 'need-orig-args nil))
	    `(if (<= 0 ,posn (length orig-args))
		 (setf args (nthcdr ,posn orig-args))
		 (error 'format-error
			:complaint (intl:gettext "Index ~D out of bounds.  Should have been ~
				    between 0 and ~D.")
			:arguments (list ,posn (length orig-args))
			:offset ,(1- end)))))
      (if colonp
	  (expand-bind-defaults ((n 1)) params
	    (unless *orig-args-available*
	      (throw 'need-orig-args nil))
	    `(do ((cur-posn 0 (1+ cur-posn))
		  (arg-ptr orig-args (cdr arg-ptr)))
		 ((eq arg-ptr args)
		  (let ((new-posn (- cur-posn ,n)))
		    (if (<= 0 new-posn (length orig-args))
			(setf args (nthcdr new-posn orig-args))
			(error 'format-error
			       :complaint
			       (intl:gettext "Index ~D out of bounds.  Should have been ~
				between 0 and ~D.")
			       :arguments
			       (list new-posn (length orig-args))
			       :offset ,(1- end)))))))
	  (if params
	      (expand-bind-defaults ((n 1)) params
		(setf *only-simple-args* nil)
		`(dotimes (i ,n)
		   ,(expand-next-arg)))
	      (expand-next-arg)))))

(def-format-interpreter #\* (colonp atsignp params)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint (intl:gettext "Cannot specify both colon and at-sign."))
	  (interpret-bind-defaults ((posn 0)) params
	    (if (<= 0 posn (length orig-args))
		(setf args (nthcdr posn orig-args))
		(error 'format-error
		       :complaint (intl:gettext "Index ~D out of bounds.  Should have been ~
				   between 0 and ~D.")
		       :arguments (list posn (length orig-args))))))
      (if colonp
	  (interpret-bind-defaults ((n 1)) params
	    (do ((cur-posn 0 (1+ cur-posn))
		 (arg-ptr orig-args (cdr arg-ptr)))
		((eq arg-ptr args)
		 (let ((new-posn (- cur-posn n)))
		   (if (<= 0 new-posn (length orig-args))
		       (setf args (nthcdr new-posn orig-args))
		       (error 'format-error
			      :complaint
			      (intl:gettext "Index ~D out of bounds.  Should have been ~
			       between 0 and ~D.")
			      :arguments
			      (list new-posn (length orig-args))))))))
	  (interpret-bind-defaults ((n 1)) params
	    (dotimes (i n)
	      (next-arg))))))


;;;; Indirection.

(def-format-directive #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the colon modifier.")))
  (expand-bind-defaults () params
    `(handler-bind
	 ((format-error
	   #'(lambda (condition)
	       (error 'format-error
		      :complaint
		      (intl:gettext "~A~%while processing indirect format string:")
		      :arguments (list condition)
		      :print-banner nil
		      :control-string ,string
		      :offset ,(1- end)))))
       ,(if atsignp
	    (if *orig-args-available*
		`(setf args (%format stream ,(expand-next-arg) orig-args args))
		(throw 'need-orig-args nil))
	    `(%format stream ,(expand-next-arg) ,(expand-next-arg))))))

(def-format-interpreter #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the colon modifier.")))
  (interpret-bind-defaults () params
    (handler-bind
	((format-error
	  #'(lambda (condition)
	      (error 'format-error
		     :complaint
		     (intl:gettext "~A~%while processing indirect format string:")
		     :arguments (list condition)
		     :print-banner nil
		     :control-string string
		     :offset (1- end)))))
      (if atsignp
	  (setf args (%format stream (next-arg) orig-args args))
	  (%format stream (next-arg) (next-arg))))))


;;;; Capitalization.

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint (intl:gettext "No corresponding close paren.")))
    (let* ((posn (position close directives))
	   (before (subseq directives 0 posn))
	   (after (nthcdr (1+ posn) directives)))
      (values
       (expand-bind-defaults () params
	 `(let ((stream (make-case-frob-stream stream
					       ,(if colonp
						    (if atsignp
							:upcase
							:capitalize)
						    (if atsignp
							:capitalize-first
							:downcase)))))
	    ,@(expand-directive-list before)))
       after))))

(def-complex-format-interpreter #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint (intl:gettext "No corresponding close paren.")))
    (interpret-bind-defaults () params
      (let* ((posn (position close directives))
	     (before (subseq directives 0 posn))
	     (after (nthcdr (1+ posn) directives))
	     (stream (make-case-frob-stream stream
					    (if colonp
						(if atsignp
						    :upcase
						    :capitalize)
						(if atsignp
						    :capitalize-first
						    :downcase)))))
	(setf args (interpret-directive-list stream before orig-args args))
	after))))

(def-complex-format-directive #\) ()
  (error 'format-error
	 :complaint (intl:gettext "No corresponding open paren.")))

(def-complex-format-interpreter #\) ()
  (error 'format-error
	 :complaint (intl:gettext "No corresponding open paren.")))


;;;; Conditionals

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
	(last-semi-with-colon-p nil)
	(remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
	(unless close-or-semi
	  (error 'format-error
		 :complaint (intl:gettext "No corresponding close bracket.")))
	(let ((posn (position close-or-semi remaining)))
	  (push (subseq remaining 0 posn) sublists)
	  (setf remaining (nthcdr (1+ posn) remaining))
	  (when (char= (format-directive-character close-or-semi) #\])
	    (return))
	  (setf last-semi-with-colon-p
		(format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind
      (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (if atsignp
	 (if colonp
	     (error 'format-error
		    :complaint
		    (intl:gettext "Cannot specify both the colon and at-sign modifiers."))
	     (if (cdr sublists)
		 (error 'format-error
			:complaint
			(intl:gettext "Can only specify one section"))
		 (expand-bind-defaults () params
		   (expand-maybe-conditional (car sublists)))))
	 (if colonp
	     (if (= (length sublists) 2)
		 (progn
		   (when last-semi-with-colon-p
		     (error 'format-error
			    :complaint (intl:gettext "~~:; directive not effective in ~~:[")))
		   (expand-bind-defaults () params
		     (expand-true-false-conditional (car sublists)
						    (cadr sublists))))
		 (error 'format-error
			:complaint
			(intl:gettext "Must specify exactly two sections.")))
	     (expand-bind-defaults ((index nil)) params
	       (setf *only-simple-args* nil)
	       (let ((clauses nil)
		     (case `(or ,index ,(expand-next-arg))))
		 (when last-semi-with-colon-p
		   (push `(t ,@(expand-directive-list (pop sublists)))
			 clauses))
		 (let ((count (length sublists)))
		   (dolist (sublist sublists)
		     (push `(,(decf count)
			     ,@(expand-directive-list sublist))
			   clauses)))
		 `(case ,case ,@clauses)))))
     remaining)))

(defun expand-maybe-conditional (sublist)
  (flet ((hairy ()
	   `(let ((prev-args args)
		  (arg ,(expand-next-arg)))
	      (when arg
		(setf args prev-args)
		,@(expand-directive-list sublist)))))
    (if *only-simple-args*
	(multiple-value-bind
	    (guts new-args)
	    (let ((*simple-args* *simple-args*))
	      (values (expand-directive-list sublist)
		      *simple-args*))
	  (cond ((and new-args (eq *simple-args* (cdr new-args)))
		 (setf *simple-args* new-args)
		 `(when ,(caar new-args)
		    ,@guts))
		(t
		 (setf *only-simple-args* nil)
		 (hairy))))
	(hairy))))

(defun expand-true-false-conditional (true false)
  (let ((arg (expand-next-arg)))
    (flet ((hairy ()
	     `(if ,arg
		  (progn
		    ,@(expand-directive-list true))
		  (progn
		    ,@(expand-directive-list false)))))
      (if *only-simple-args*
	  (multiple-value-bind
	      (true-guts true-args true-simple)
	      (let ((*simple-args* *simple-args*)
		    (*only-simple-args* t))
		(values (expand-directive-list true)
			*simple-args*
			*only-simple-args*))
	    (multiple-value-bind
		(false-guts false-args false-simple)
		(let ((*simple-args* *simple-args*)
		      (*only-simple-args* t))
		  (values (expand-directive-list false)
			  *simple-args*
			  *only-simple-args*))
	      (if (= (length true-args) (length false-args))
		  `(if ,arg
		       (progn
			 ,@true-guts)
		       ,(do ((false false-args (cdr false))
			     (true true-args (cdr true))
			     (bindings nil (cons `(,(caar false) ,(caar true))
						 bindings)))
			    ((eq true *simple-args*)
			     (setf *simple-args* true-args)
			     (setf *only-simple-args*
				   (and true-simple false-simple))
			     (if bindings
				 `(let ,bindings
				    ,@false-guts)
				 `(progn
				    ,@false-guts)))))
		  (progn
		    (setf *only-simple-args* nil)
		    (hairy)))))
	  (hairy)))))



(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (multiple-value-bind
      (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (setf args
	  (if atsignp
	      (if colonp
		  (error 'format-error
			 :complaint
		     (intl:gettext "Cannot specify both the colon and at-sign modifiers."))
		  (if (cdr sublists)
		      (error 'format-error
			     :complaint
			     (intl:gettext "Can only specify one section"))
		      (interpret-bind-defaults () params
			(let ((prev-args args)
			      (arg (next-arg)))
			  (if arg
			      (interpret-directive-list stream
							(car sublists)
							orig-args
							prev-args)
			      args)))))
	      (if colonp
		  (if (= (length sublists) 2)
		      (interpret-bind-defaults () params
			(if (next-arg)
			    (interpret-directive-list stream (car sublists)
						      orig-args args)
			    (interpret-directive-list stream (cadr sublists)
						      orig-args args)))
		      (error 'format-error
			     :complaint
			     (intl:gettext "Must specify exactly two sections.")))
		  (interpret-bind-defaults ((index (next-arg))) params
		    (let* ((default (and last-semi-with-colon-p
					 (pop sublists)))
			   (last (1- (length sublists)))
			   (sublist
			    (if (<= 0 index last)
				(nth (- last index) sublists)
				default)))
		      (interpret-directive-list stream sublist orig-args
						args))))))
    remaining))

(def-complex-format-directive #\; ()
  (error 'format-error
	 :complaint
	 (intl:gettext "~~; not contained within either ~~[...~~] or ~~<...~~>.")))

(def-complex-format-interpreter #\; ()
  (error 'format-error
	 :complaint
	 (intl:gettext "~~; not contained within either ~~[...~~] or ~~<...~~>.")))

(def-complex-format-interpreter #\] ()
  (error 'format-error
	 :complaint
	 (intl:gettext "No corresponding open bracket.")))

(def-complex-format-directive #\] ()
  (error 'format-error
	 :complaint
	 (intl:gettext "No corresponding open bracket.")))


;;;; Up-and-out.

(defvar *outside-args*)

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the at-sign modifier.")))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint (intl:gettext "Attempt to use ~~:^ outside a ~~:{...~~} construct.")))
  ;; See the #\^ interpreter below for what happens here.
  `(when ,(case (length params)
	    (0 (if colonp
		   '(null outside-args)
		   (progn
		     (setf *only-simple-args* nil)
		     '(null args))))
	    (1 (expand-bind-defaults ((count nil)) params
		 `(if ,count
		      (eql ,count 0)
		      ,(if colonp
			   '(null outside-args)
			   (progn
			     (setf *only-simple-args* nil)
			     '(null args))))))
	    (2 (expand-bind-defaults ((arg1 nil) (arg2 nil)) params
		 `(if ,arg2
		      (eql ,arg1 ,arg2)
		      (eql ,arg1 0))))
	    (t (expand-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
		 `(if ,arg3
		      (<= ,arg1 ,arg2 ,arg3)
		      (if ,arg2
			  (eql ,arg1 ,arg2)
			  ;; Duplicate the case of 1 arg?
			  (eql ,arg1 0))))))
     ,(if colonp
	  '(return-from outside-loop nil)
	  '(return))))

(def-format-interpreter #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint (intl:gettext "Cannot specify the at-sign modifier.")))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint (intl:gettext "Attempt to use ~~:^ outside a ~~:{...~~} construct.")))
  ;; This is messy because, as I understand it, and as tested by
  ;; ansi-tests, a NIL parameter is the same as not given.  Thus for 2
  ;; args, if the second is nil, we have to pretend that only 1 was
  ;; given.  Similarly for 3 args.
  ;;
  ;; Also, ansi-tests interprets CLHS 22.3.9.2 such that "equal"
  ;; parameter means equal (or at least eql).
  ;;
  ;; FIXME: This needs to be done in a better way!
  (when (case (length params)
	  (0 (if colonp
		 (null *outside-args*)
		 (null args)))
	  (1 (interpret-bind-defaults ((count nil)) params
	       (if count
		   (eql count 0)
		   (if colonp
		       (null *outside-args*)
		       (null args)))))
	  (2 (interpret-bind-defaults ((arg1 nil) (arg2 nil)) params
	       (if arg2
		   (eql arg1 arg2)
		   ;; Should we duplicate the previous case here?
		   (eql arg1 0))))
	  (t (interpret-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
	       (if arg3
		   (<= arg1 arg2 arg3)
		   (if arg2
		       (eql arg1 arg2)
		       ;; Duplicate the case of 1 arg?
		       (eql arg1 0))))))
    (throw (if colonp 'up-up-and-out 'up-and-out)
	   args)))


;;;; Iteration.

(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint
	     (intl:gettext "No corresponding close brace.")))
    (let* ((closed-with-colon (format-directive-colonp close))
	   (posn (position close directives)))
      (labels
	  ((compute-insides ()
	     (if (zerop posn)
		 (if *orig-args-available*
		     `((handler-bind
			   ((format-error
			     #'(lambda (condition)
				 (error 'format-error
					:complaint
					(intl:gettext "~A~%while processing indirect format string:")
					:arguments (list condition)
					:print-banner nil
					:control-string ,string
					:offset ,(1- end)))))
			 (setf args
			       (%format stream inside-string orig-args args))))
		     (throw 'need-orig-args nil))
		 (let ((*up-up-and-out-allowed* colonp))
		   (expand-directive-list (subseq directives 0 posn)))))
	   (compute-loop (count)
	     (when atsignp
	       (setf *only-simple-args* nil))
	     `(loop
		,@(unless closed-with-colon
		    '((when (null args)
			(return))))
		,@(when count
		    `((when (and ,count (minusp (decf ,count)))
			(return))))
		,@(if colonp
		      (let ((*expander-next-arg-macro* 'expander-next-arg)
			    (*only-simple-args* nil)
			    (*orig-args-available* t))
			`((let* ((orig-args ,(expand-next-arg))
				 (outside-args args)
				 (args orig-args))
			    (declare (ignorable orig-args outside-args args))
			    (block nil
			      ,@(compute-insides)))))
		      (compute-insides))
		,@(when closed-with-colon
		    '((when (null args)
			(return))))))
	   (compute-block (count)
	     (if colonp
		 `(block outside-loop
		    ,(compute-loop count))
		 (compute-loop count)))
	   (compute-bindings (count)
	     (if atsignp
		 (compute-block count)
		 `(let* ((orig-args ,(expand-next-arg))
			 (args orig-args))
		    (declare (ignorable orig-args args))
		    ,(let ((*expander-next-arg-macro* 'expander-next-arg)
			   (*only-simple-args* nil)
			   (*orig-args-available* t))
			  (compute-block count))))))
	(values (if params
		    (expand-bind-defaults ((count nil)) params
		      (if (zerop posn)
			  `(let ((inside-string ,(expand-next-arg)))
			     ,(compute-bindings count))
			  (compute-bindings count)))
		    (if (zerop posn)
			`(let ((inside-string ,(expand-next-arg)))
			   ,(compute-bindings nil))
			(compute-bindings nil)))
		(nthcdr (1+ posn) directives))))))

(def-complex-format-interpreter #\{
				(colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint
	     (intl:gettext "No corresponding close brace.")))
    (interpret-bind-defaults ((max-count nil)) params
      (let* ((closed-with-colon (format-directive-colonp close))
	     (posn (position close directives))
	     (insides (if (zerop posn)
			  (next-arg)
			  (subseq directives 0 posn)))
	     (*up-up-and-out-allowed* colonp))
	(labels
	    ((do-guts (orig-args args)
	       (if (zerop posn)
		   (handler-bind
		       ((format-error
			 #'(lambda (condition)
			     (error 'format-error
				    :complaint
				    (intl:gettext "~A~%while processing indirect format string:")
				    :arguments (list condition)
				    :print-banner nil
				    :control-string string
				    :offset (1- end)))))
		     (%format stream insides orig-args args))
		   (interpret-directive-list stream insides
					     orig-args args)))
	     (bind-args (orig-args args)
	       (if colonp
		   (let* ((arg (next-arg))
			  (*logical-block-popper* nil)
			  (*outside-args* args))
		     (catch 'up-and-out
		       (do-guts arg arg))
		     args)
		   (do-guts orig-args args)))
	     (do-loop (orig-args args)
	       (catch (if colonp 'up-up-and-out 'up-and-out)
		 (loop
		   (when (and (not closed-with-colon) (null args))
		     (return))
		   (when (and max-count (minusp (decf max-count)))
		     (return))
		   (setf args (bind-args orig-args args))
		   (when (and closed-with-colon (null args))
		     (return)))
		 args)))
	  (if atsignp
	      (setf args (do-loop orig-args args))
	      (let ((arg (next-arg))
		    (*logical-block-popper* nil))
		(do-loop arg arg)))
	  (nthcdr (1+ posn) directives))))))

(def-complex-format-directive #\} ()
  (error 'format-error
	 :complaint (intl:gettext "No corresponding open brace.")))

(def-complex-format-interpreter #\} ()
  (error 'format-error
	 :complaint (intl:gettext "No corresponding open brace.")))



;;;; Justification.

(defparameter *illegal-inside-justification*
  (mapcar (lambda (x) (parse-directive x 0))
	  '("~W" "~:W" "~@W" "~:@W"
	    "~_" "~:_" "~@_" "~:@_"
	    "~:>" "~:@>"
	    "~I" "~:I" "~@I" "~:@I"
	    "~:T" "~:@T")))

(defun illegal-inside-justification-p (directive)
  (member directive *illegal-inside-justification*
	  :test (lambda (x y)
		  (and (format-directive-p x)
		       (format-directive-p y)
		       (eql (format-directive-character x) (format-directive-character y))
		       (eql (format-directive-colonp x) (format-directive-colonp y))
		       (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind
      (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (format-directive-colonp close)
	 (multiple-value-bind
	     (prefix per-line-p insides suffix)
	     (parse-format-logical-block segments colonp first-semi
					 close params string end)
	   (expand-format-logical-block prefix per-line-p insides
					suffix atsignp))
	 (let ((count (reduce #'+ (mapcar (lambda (x)
					    (count-if #'illegal-inside-justification-p x))
					  segments))))
	   (when (> count 0)
	     ;; ANSI specifies that "an error is signalled" in this
	     ;; situation.
	     (error 'format-error
		    :complaint (intl:ngettext "~D illegal directive found inside justification block"
					      "~D illegal directives found inside justification block"
					      count)
		    :arguments (list count)))
	   (expand-format-justification segments colonp atsignp
				      first-semi params)))
     remaining)))

(def-complex-format-interpreter #\<
				(colonp atsignp params string end directives)
  (multiple-value-bind
      (segments first-semi close remaining)
      (parse-format-justification directives)
    (setf args
	  (if (format-directive-colonp close)
	      (multiple-value-bind
		  (prefix per-line-p insides suffix)
		  (parse-format-logical-block segments colonp first-semi
					      close params string end)
		(interpret-format-logical-block stream orig-args args
						prefix per-line-p insides
						suffix atsignp))
	      (let ((count (reduce #'+ (mapcar (lambda (x)
                                                 (count-if #'illegal-inside-justification-p x))
                                               segments))))
		(when (> count 0)
		  ;; ANSI specifies that "an error is signalled" in this
		  ;; situation.
		  (error 'format-error
			 :complaint (intl:ngettext
				     "~D illegal directive found inside justification block"
				     "~D illegal directives found inside justification block"
				     count)
			 :arguments (list count)))
		(interpret-format-justification stream orig-args args
						segments colonp atsignp
						first-semi params))))
    remaining))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
	(close nil)
	(remaining directives))
    (collect ((segments))
      (loop
	(let ((close-or-semi (find-directive remaining #\> t)))
	  (unless close-or-semi
	    (error 'format-error
		   :complaint (intl:gettext "No corresponding close bracket.")))
	  (let ((posn (position close-or-semi remaining)))
	    (segments (subseq remaining 0 posn))
	    (setf remaining (nthcdr (1+ posn) remaining)))
	  (when (char= (format-directive-character close-or-semi)
		       #\>)
	    (setf close close-or-semi)
	    (return))
	  (unless first-semi
	    (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
	 (and first-semi
	      (format-directive-colonp first-semi))))
    (expand-bind-defaults
	((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
	params
      `(let ((segments nil)
	     ,@(when newline-segment-p
		 '((newline-segment nil)
		   (extra-space 0)
		   (line-len 72))))
	 (block nil
	   ,@(when newline-segment-p
	       `((setf newline-segment
		       (with-output-to-string (stream)
			 ,@(expand-directive-list (pop segments))))
		 ,(expand-bind-defaults
		      ((extra 0)
		       (line-len '(or (lisp::line-length stream) 72)))
		      (format-directive-params first-semi)
		    `(setf extra-space ,extra line-len ,line-len))))
	   ,@(mapcar #'(lambda (segment)
			 `(push (with-output-to-string (stream)
				  ,@(expand-directive-list segment))
				segments))
		     segments))
	 (format-justification stream
			       ,@(if newline-segment-p
				     '(newline-segment extra-space line-len)
				     '(nil 0 0))
			       segments ,colonp ,atsignp
			       ,mincol ,colinc ,minpad ,padchar)))))

(defun interpret-format-justification
       (stream orig-args args segments colonp atsignp first-semi params)
  (interpret-bind-defaults
      ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
      params
    (let ((newline-string nil)
	  (strings nil)
	  (extra-space 0)
	  (line-len 0))
      (setf args
	    (catch 'up-and-out
	      (when (and first-semi (format-directive-colonp first-semi))
		(interpret-bind-defaults
		    ((extra 0)
		     (len (or (lisp::line-length stream) 72)))
		    (format-directive-params first-semi)
		  (setf newline-string
			(with-output-to-string (stream)
			  (setf args
				(interpret-directive-list stream
							  (pop segments)
							  orig-args
							  args))))
		  (setf extra-space extra)
		  (setf line-len len)))
	      (dolist (segment segments)
		(push (with-output-to-string (stream)
			(setf args
			      (interpret-directive-list stream segment
							orig-args args)))
		      strings))
	      args))
      (format-justification stream newline-string extra-space line-len strings
			    colonp atsignp mincol colinc minpad padchar)))
  args)

(defun format-justification (stream newline-prefix extra-space line-len strings
			     pad-left pad-right mincol colinc minpad padchar)
  (setf strings (reverse strings))

  (let* ((num-gaps (+ (1- (length strings))
		      (if pad-left 1 0)
		      (if pad-right 1 0)))
	 (chars (+ (* num-gaps minpad)
		   (loop
		     for string in strings
		     summing (length string))))
	 (length (if (> chars mincol)
		     (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
		     mincol))
	 (padding (+ (- length chars) (* num-gaps minpad))))
    (when (and newline-prefix
	       (> (+ (or (lisp::charpos stream) 0)
		     length extra-space)
		  line-len))
      (write-string newline-prefix stream))

    #||
    (format t "mincol   = ~A~%" mincol)
    (format t "minpad   = ~A~%" minpad)
    (format t "num-gaps = ~A~%" num-gaps)
    (format t "chars    = ~A~%" chars)
    (format t "length   = ~A~%" length)
    (format t "padding  = ~A~%" padding)
    ||#
    
    (flet ((do-padding ()
	     (let ((pad-len (if (zerop num-gaps)
				padding
				(truncate padding num-gaps))))
	       (decf padding pad-len)
	       (decf num-gaps)
	       (dotimes (i pad-len) (write-char padchar stream)))))
      (when (or pad-left
		(and (not pad-right) (null (cdr strings))))
	(do-padding))
      (when strings
	(write-string (car strings) stream)
	(dolist (string (cdr strings))
	  (do-padding)
	  (write-string string stream)))
      (when pad-right
	(do-padding)))))

(defun parse-format-logical-block
       (segments colonp first-semi close params string end)
  (when params
    (error 'format-error
	   :complaint (intl:gettext "No parameters can be supplied with ~~<...~~:>.")
	   :offset (caar params)))
  (multiple-value-bind
      (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
	  (if colonp (values "(" ")") (values "" ""))
	(flet ((extract-string (list prefix-p)
		 (let ((directive (find-if #'format-directive-p list)))
		   (if directive
		       (error 'format-error
			      :complaint
			      (intl:gettext "Cannot include format directives inside the ~
			       ~:[suffix~;prefix~] segment of ~~<...~~:>")
			      :arguments (list prefix-p)
			      :offset (1- (format-directive-end directive)))
		       (apply #'concatenate 'string list)))))
	(case (length segments)
	  (0 (values prefix-default nil suffix-default))
	  (1 (values prefix-default (car segments) suffix-default))
	  (2 (values (extract-string (car segments) t)
		     (cadr segments) suffix-default))
	  (3 (values (extract-string (car segments) t)
		     (cadr segments)
		     (extract-string (caddr segments) nil)))
	  (t
	   (error 'format-error
		  :complaint (intl:gettext "Too many segments for ~~<...~~:>."))))))
    (when (format-directive-atsignp close)
      (setf insides
	    (add-fill-style-newlines insides
				     string
				     (if first-semi
					 (format-directive-end first-semi)
					 end))))
    (values prefix
	    (and first-semi (format-directive-atsignp first-semi))
	    insides
	    suffix)))

;; CLHS 22.3.5.2 says fill-style conditional newlines are
;; automatically inserted after each group of blanks except for blanks
;; after a newline directive.
(defun add-fill-style-newlines (list string offset &optional newlinep)
  (if list
      (let ((directive (car list)))
	(if (simple-string-p directive)
	    (nconc (add-fill-style-newlines-aux directive string offset newlinep)
		   (add-fill-style-newlines (cdr list)
					    string
					    (+ offset (length directive))))
	    (cons directive
		  (add-fill-style-newlines (cdr list)
					   string
					   (format-directive-end directive)
					   (char= (format-directive-character directive)
						  #\Newline)))))
      nil))

(defun add-fill-style-newlines-aux (literal string offset &optional newlinep)
  (let ((end (length literal))
	(posn 0))
    (collect ((results))
      (loop
	(let ((blank (position #\space literal :start posn)))
	  (when (null blank)
	    (results (subseq literal posn))
	    (return))
	  (let ((non-blank (or (position #\space literal :start blank
					 :test #'char/=)
			       end)))
	    (results (subseq literal posn non-blank))
	    (unless newlinep
	      (results (make-format-directive
			:string string :character #\_
			:start (+ offset non-blank) :end (+ offset non-blank)
			:colonp t :atsignp nil :params nil)))
	    (setf posn non-blank))
	  (when (= posn end)
	    (return))))
      (results))))

(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
	 (setf *only-simple-args* nil)
	 '((setf args nil)))
     (pprint-logical-block
	 (stream arg
		 ,(if per-line-p :per-line-prefix :prefix) (or ,prefix "")
		 :suffix (or ,suffix ""))
       (let ((args arg)
	     ,@(unless atsignp
		 `((orig-args arg))))
	 (declare (ignorable args ,@(unless atsignp '(orig-args))))
	 (block nil
	   ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
		   (*only-simple-args* nil)
		   (*orig-args-available* t))
	       (expand-directive-list insides)))))))

(defun interpret-format-logical-block
       (stream orig-args args prefix per-line-p insides suffix atsignp)
  (let ((arg (if atsignp args (next-arg))))
    (if per-line-p
	(pprint-logical-block
	    (stream arg :per-line-prefix prefix :suffix suffix)
	  (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
	    (catch 'up-and-out
	      (interpret-directive-list stream insides
					(if atsignp orig-args arg)
					arg))))
	(pprint-logical-block (stream arg :prefix prefix :suffix suffix)
	  (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
	    (catch 'up-and-out
	      (interpret-directive-list stream insides
					(if atsignp orig-args arg)
					arg))))))
  (if atsignp nil args))

(def-complex-format-directive #\> ()
  (error 'format-error
	 :complaint (intl:gettext "No corresponding open bracket.")))


;;;; User-defined method.

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (collect ((param-names) (bindings))
      (dolist (param-and-offset params)
	(let ((param (cdr param-and-offset)))
	  (let ((param-name (gensym)))
	    (param-names param-name)
	    (bindings `(,param-name
			,(case param
			   (:arg (expand-next-arg))
			   (:remaining '(length args))
			   (t param)))))))
      `(let ,(bindings)
	 (,symbol stream ,(expand-next-arg) ,colonp ,atsignp
		  ,@(param-names))))))

(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (collect ((args))
      (dolist (param-and-offset params)
	(let ((param (cdr param-and-offset)))
	  (case param
	    (:arg (args (next-arg)))
	    (:remaining (args (length args)))
	    (t (args param)))))
      (apply (fdefinition symbol) stream (next-arg) colonp atsignp (args)))))

(defun extract-user-function-name (string start end)
  (let ((slash (position #\/ string :start start :end (1- end)
			 :from-end t)))
    (unless slash
      (error 'format-error
	     :complaint (intl:gettext "Malformed ~~/ directive.")))
    (let* ((name (string-upcase (let ((foo string))
				  ;; Hack alert: This is to keep the compiler
				  ;; quiet about deleting code inside the
				  ;; subseq expansion.
				  (subseq foo (1+ slash) (1- end)))))
	   (first-colon (position #\: name))
	   (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
	   (package-name (if first-colon
			     (subseq name 0 first-colon)
			     "COMMON-LISP-USER"))
	   (package (find-package package-name)))
      (unless package
	(error 'format-error
	       :complaint (intl:gettext "No package named ~S")
	       :arguments (list package-name)))
      (intern (cond
                ((and second-colon (= second-colon (1+ first-colon)))
                 (subseq name (1+ second-colon)))
                (first-colon
                 (subseq name (1+ first-colon)))
                (t name))
              package))))


;;;; Compile-time checking of format arguments and control string

;;;
;;; Return the min/max numbers of arguments required for a call to
;;; FORMAT with control string FORMAT-STRING, null if we can't tell,
;;; or a string with an error message if parsing the control string
;;; causes a FORMAT-ERROR.
;;;
;;; This is called from FORMAT deftransforms.
;;;
(defun min/max-format-arguments-count (string)
  (handler-case
      (catch 'give-up
	;; For the side effect of validating the control string.
	(%formatter string)
	(%min/max-format-args (tokenize-control-string string)))
    (format-error (e)
      (format nil "~a" e))))

(defun %min/max-format-args (directives)
  (let ((min-req 0) (max-req 0))
    (flet ((incf-both (&optional (n 1))
	     (incf min-req n)
	     (incf max-req n)))
      (loop
	 (let ((dir (pop directives)))
	   (when (null dir)
	     (return (values min-req max-req)))
	   (when (format-directive-p dir)
	     (incf-both (count :arg (format-directive-params dir) :key #'cdr))
	     (let ((c (format-directive-character dir))
		   close)
	       (cond ((find c "ABCDEFGORSWX$/")
		      (incf-both))
		     ((char= c #\P)
		      (unless (format-directive-colonp dir)
			(incf-both)))
		     ((and (char= c #\<)
			   (not (format-directive-atsignp dir))
			   (setq close (find-directive directives #\> nil))
			   (format-directive-colonp close))
		      (incf-both)
		      (setq directives (cdr (member close directives))))
		     ((or (find c "IT%&|_<>();") (char= c #\newline) (char= c #\~)))
		     ((char= c #\[)
		      (multiple-value-bind (min max remaining)
			  (%min/max-conditional-args dir directives)
			(setq directives remaining)
			(incf min-req min)
			(incf max-req max)))
		     ((char= c #\{)
		      (multiple-value-bind (min max remaining)
			  (%min/max-iteration-args dir directives)
			(setq directives remaining)
			(incf min-req min)
			(incf max-req max)))
		     ((char= c #\?)
		      (cond ((format-directive-atsignp dir)
			     (incf min-req)
			     (setq max-req most-positive-fixnum))
			    (t (incf-both 2))))
		     (t (throw 'give-up nil))))))))))

;;;
;;; ANSI: if arg is out of range, no clause is selected.  That means
;;; the minimum number of args required for the interior of ~[~] is
;;; always zero.
;;;
(defun %min/max-conditional-args (conditional directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (declare (ignore last-semi-with-colon-p))
    (let ((sub-max (loop for s in sublists maximize
			   (nth-value 1 (%min/max-format-args s))))
	  (min-req 1)
	  max-req)
      (cond ((format-directive-atsignp conditional)
	     (setq max-req (max 1 sub-max)))
	    ((loop for p in (format-directive-params conditional)
		   thereis (or (integerp (cdr p))
			       (memq (cdr p) '(:remaining :arg))))
	     (setq min-req 0)
	     (setq max-req sub-max))
	    (t
	     (setq max-req (1+ sub-max))))
      (values min-req max-req remaining))))

(defun %min/max-iteration-args (iteration directives)
  (let* ((close (find-directive directives #\} nil))
	 (posn (position close directives))
	 (remaining (nthcdr (1+ posn) directives)))
    (if (format-directive-atsignp iteration)
	(values (if (zerop posn) 1 0) most-positive-fixnum remaining)
	(let ((nreq (if (zerop posn) 2 1)))
	  (values nreq nreq remaining)))))


