;;; -*- Package: FORMAT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/format.lisp,v 1.10 1991/11/29 19:38:19 wlott Exp $")
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

(in-package "LISP")
(export '(format formatter))

(in-package "FORMAT")

(deftype boolean ()
  '(member t nil))
(deftype index ()
  '(and unsigned-byte fixnum))

(defstruct (format-directive
	    (:print-function %print-format-directive))
  (string (required-argument) :type simple-string)
  (start (required-argument) :type (and unsigned-byte fixnum))
  (end (required-argument) :type (and unsigned-byte fixnum))
  (character (required-argument) :type base-character)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun %print-format-directive (struct stream depth)
  (declare (ignore depth))
  (format stream "#<~A>"
	  (subseq (format-directive-string struct)
		  (format-directive-start struct)
		  (format-directive-end struct))))

(defvar *format-directive-expanders*
  (make-array char-code-limit :initial-element nil))

(defun %print-format-error (condition stream)
  (format stream
	  "~:[~;Error in format: ~]~
	   ~?~@[~%  ~A~%  ~V@T^~]"
	  (format-error-print-banner condition)
	  (format-error-complaint condition)
	  (format-error-arguments condition)
	  (format-error-control-string condition)
	  (format-error-offset condition)))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

(define-condition format-error (error)
  ((complaint (required-argument))
   (arguments nil)
   (control-string *default-format-error-control-string*)
   (offset *default-format-error-offset*)
   (print-banner t))
  (:report %print-format-error))



;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
	(end (length string))
	(result nil))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
	(when (> next-directive index)
	  (push (subseq string index next-directive) result))
	(when (= next-directive end)
	  (return))
	(let ((directive (parse-directive string next-directive)))
	  (push directive result)
	  (setf index (format-directive-end directive)))))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
	(end (length string)))
    (flet ((get-char ()
	     (if (= posn end)
		 (error 'format-error
			:complaint "String ended before directive was found."
			:control-string string
			:offset start)
		 (schar string posn))))
      (loop
	(let ((char (get-char)))
	  (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
		 (multiple-value-bind
		     (param new-posn)
		     (parse-integer string :start posn :junk-allowed t)
		   (push param params)
		   (setf posn new-posn)
		   (case (get-char)
		     (#\,)
		     ((#\: #\@)
		      (decf posn))
		     (t
		      (return)))))
		((or (char= char #\v) (char= char #\V))
		 (push :arg params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\#)
		 (push :remaining params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\')
		 (incf posn)
		 (push (get-char) params))
		((char= char #\,)
		 (push nil params))
		((char= char #\:)
		 (if colonp
		     (error 'format-error
			    :complaint "Too many colons supplied."
			    :control-string string
			    :offset posn)
		     (setf colonp t)))
		((char= char #\@)
		 (if atsignp
		     (error 'format-error
			    :complaint "Too many at-signs supplied."
			    :control-string string
			    :offset posn)
		     (setf atsignp t)))
		(t
		 (when (char= (schar string (1- posn)) #\,)
		   (push nil params))
		 (return))))
	(incf posn))
      (let ((char (get-char)))
	(when (char= char #\/)
	  (let ((closing-slash (position #\/ string :start (1+ posn))))
	    (if closing-slash
		(setf posn closing-slash)
		(error 'format-error
		       :complaint "No matching closing slash."
		       :control-string string
		       :offset posn))))
	(make-format-directive
	    :string string :start start :end (1+ posn) :character char
	    :colonp colonp :atsignp atsignp
	    :params (nreverse params))))))




;;;; FORMAT

(defun my-format (dest string &rest args)
  (etypecase dest
    (null
     (with-output-to-string (stream)
       (%format stream string args)))
    (string
     (with-output-to-string (stream dest)
       (%format stream string args)))
    ((member t)
     (%format *standard-output* string args)
     nil)
    (stream
     (%format dest string args)
     nil)))

(defun %format (stream string-or-fun orig-args &optional (args orig-args))
  (if (functionp string-or-fun)
      (apply string-or-fun stream args)
      (funcall (make-%format-fun (etypecase string-or-fun
				   (simple-string
				    string-or-fun)
				   (string
				    (coerce string-or-fun 'simple-string))))
	       stream orig-args args)))

(defun-cached (make-%format-fun
	       :hash-function (lambda (string)
				(let ((sxhash (sxhash string)))
				  (logxor (ldb (byte 8 0) sxhash)
					  (ldb (byte 8 6) sxhash)
					  (ldb (byte 8 12) sxhash))))
	       :hash-bits 8)
	      ((string (lambda (s1 s2)
			 (and s1 s2 (string= s1 s2)))))
  (coerce `(lambda (stream orig-args args)
	     (declare (ignorable orig-args))
	     ,(expand-control-string string)
	     args)
	  'function))


;;;; FORMATTER

(defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  `(lambda (stream &rest orig-args)
     (let ((args orig-args))
       ,(expand-control-string control-string)
       args)))


;;;; 

(defun expand-control-string (string)
  (let* ((string (etypecase string
		   (simple-string
		    string)
		   (string
		    (coerce string 'simple-string))))
	 (*default-format-error-control-string* string)
	 (directives (tokenize-control-string string)))
    `(let ((*default-format-error-control-string* ',string))
       (block nil
	 (macrolet ((next-arg ()
		      '(if args
			   (pop args)
			   (error 'format-error
				  :complaint
				  "No more arguments to satisfy directive."))))
	   ,@(expand-directive-list directives))))))

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
	(push form results)
	(setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
	    (aref *format-directive-expanders*
		  (char-code (format-directive-character directive)))))
       (if expander
	   (let ((*default-format-error-offset*
		  (1- (format-directive-end directive))))
	     (multiple-value-bind
		 (form directives)
		 (funcall expander directive more-directives)
	       (values `(let ((*default-format-error-offset*
			       ',(1- (format-directive-end directive))))
			  ,form)
		       directives)))
	   (error 'format-error
		  :complaint "Unknown directive."
		  :control-string (format-directive-string directive)
		  :offset (1- (format-directive-end directive))))))
    (simple-string
     (values `(write-string ,directive stream)
	     more-directives))))


;;;; Format directive definition macros and runtime support.

(eval-when (compile eval)

(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (format nil "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
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

(defmacro bind-defaults (specs params &body body)
  (once-only ((params params))
    (collect ((expander-bindings) (runtime-bindings))
      (dolist (spec specs)
	(destructuring-bind (var default) spec
	  (let ((symbol (gensym)))
	    (expander-bindings
	     `(,var ',symbol))
	    (runtime-bindings
	     `(list ',symbol
		    (let ((param (pop ,params)))
		      (case param
			(:arg `(or (next-arg) ,,default))
			(:remaining '(length args))
			((nil) ,default)
			(t param))))))))
      `(let ,(expander-bindings)
	 `(let ,(list ,@(runtime-bindings))
	    ,@(if ,params
		  (error 'format-error
			 :complaint
			 "Too many parameters, expected no more than ~D"
			 :arguments (list ,(length specs))))
	    ,,@body))))))

); eval-when

(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
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

(defvar *up-up-and-out-allowed* nil)



;;;; Utility functions for outputting stuff.

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft
    (write-string string stream))
  (dotimes (i minpad)
    (write-char padchar stream))
  (do ((chars (+ (length string) minpad) (+ chars colinc)))
      ((>= chars mincol))
    (dotimes (i colinc)
      (write-char padchar stream)))
  (when padleft
    (write-string string stream)))

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
;;;
(defun format-print-integer (number stream print-commas-p print-sign-p
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
	(princ number))))

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



;;;; The actual format directives.

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (princ-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-directive #\A (colonp atsignp params)
  (if params
      (bind-defaults ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
		     params
	`(format-princ stream (next-arg) ',colonp ',atsignp
		       ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp '(or (next-arg) "()") '(next-arg)) stream)))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
		      (if (or arg (not colonp))
			  (prin1-to-string arg)
			  "()")
		      mincol colinc minpad padchar atsignp))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
	 (bind-defaults ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
			params
	   `(format-prin1 stream (next-arg) ,colonp ,atsignp
			  ,mincol ,colinc ,minpad ,padchar)))
	(colonp
	 `(let ((arg (next-arg)))
	    (if arg
		(prin1 arg stream)
		(princ "()" stream))))
	(t
	 '(prin1 (next-arg) stream))))

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (bind-defaults
	  ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
	  params
	`(format-print-integer stream (next-arg) ,colonp ,atsignp ,base ,mincol
			       ,padchar ,commachar ,commainterval))
      `(write (next-arg) :stream stream :base ,base :radix nil :escape nil)))
  

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-directive #\R (colonp atsignp params)
  (if params
      (bind-defaults
	  ((base 10) (mincol 0) (padchar #\space) (commachar #\,)
	   (commainterval 3))
	  params
	`(format-print-integer stream (next-arg) ,colonp ,atsignp ,base ,mincol
			       ,padchar ,commachar ,commainterval))
      (if atsignp
	  (if colonp
	      '(format-print-old-roman stream (next-arg))
	      '(format-print-roman stream (next-arg)))
	  (if colonp
	      '(format-print-ordinal stream (next-arg))
	      '(format-print-cardinal stream (next-arg))))))

(def-format-directive #\P (colonp atsignp params)
  (bind-defaults () params
    (let ((arg (if colonp
		   '(if (eq orig-args args)
			(error 'format-error
			       :complaint "No previous argument.")
			(do ((arg-ptr orig-args (cdr arg-ptr)))
			    ((eq (cdr arg-ptr) args)
			     (car arg-ptr))))
		   '(next-arg))))
      (if atsignp
	  `(write-string (if (eql ,arg 1) "y" "ies") stream)
	  `(unless (eql ,arg 1) (write-char #\s stream))))))

(defun format-print-named-character (char stream)
  (let* ((name (char-name char)))
    (cond (name
	   (write-string (string-capitalize name) stream))
	  ((<= 0 (char-code char) 31)
	   ;; Print control characters as "^"<char>
	   (write-char #\^ stream)
	   (write-char (code-char (+ 64 (char-code char))) stream))
	  (t
	   (write-char char stream)))))

(def-format-directive #\C (colonp atsignp params)
  (bind-defaults () params
    (if colonp
	'(format-print-named-character (next-arg) stream)
	(if atsignp
	    '(prin1 (next-arg) stream)
	    '(write-char (next-arg) stream)))))

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "Cannot specify the colon modifier with this directive."))
  (bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
    `(format-fixed stream (next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (floatp number)
      (format-fixed-aux stream number w d k ovf pad atsign)
      (if (rationalp number)
	  (format-fixed-aux stream
			    (coerce number 'single-float)
			    w d k ovf pad atsign)
	  (let ((*print-base* 10))
	    (format-write-field stream
				(princ-to-string number)
				w 1 0 #\space t)))))

;;; We return true if we overflowed, so that ~G can output the overflow char
;;; instead of spaces.
;;;
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (cond
   ((not (or w d))
    (prin1 number stream)
    nil)
   (t
    (let ((spaceleft w))
      (when (and w (or atsign (minusp number))) (decf spaceleft))
      (multiple-value-bind 
	  (str len lpoint tpoint)
	  (lisp::flonum-to-string (abs number) spaceleft d k)
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
	       (if (minusp number)
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
	   "Cannot specify the colon modifier with this directive."))
  (bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponent stream (next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (floatp number)
      (format-exp-aux stream number w d e k ovf pad marker atsign)
      (if (rationalp number)
	  (format-exp-aux stream
			  (coerce number 'single-float)
			  w d e k ovf pad marker atsign)
	  (let ((*print-base* 10))
	    (format-write-field stream
				(princ-to-string number)
				w 1 0 #\space t)))))

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
	(single-float #\f)
	(double-float #\d)
	(short-float #\s)
	(long-float #\l))))

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (not (or w d))
      (prin1 number stream)
      (multiple-value-bind (num expt)
			   (lisp::scale-exponent (abs number))
	(let* ((expt (- expt k))
	       (estr (princ-to-string (abs expt)))
	       (elen (if e (max (length estr) e) (length estr)))
	       (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
	       (fmin (if (minusp k) (- 1 k) nil))
	       (spaceleft (if w (- w 2 elen) nil)))
	  (when (or atsign (minusp number)) (decf spaceleft))
	  (if (and w e ovf (> elen e))
	      ;;exponent overflow
	      (dotimes (i w) (write-char ovf stream))
	      (multiple-value-bind
		  (fstr flen lpoint)
		  (lisp::flonum-to-string num spaceleft fdig k fmin)
		(when w 
		  (decf spaceleft flen)
		  (when lpoint
		    (if (> spaceleft 0)
			(decf spaceleft)
			(setq lpoint nil))))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf stream)))
		      (t (when w
			   (dotimes (i spaceleft) (write-char pad stream)))
			 (if (minusp number)
			     (write-char #\- stream)
			     (if atsign (write-char #\+ stream)))
			 (when lpoint (write-char #\0 stream))
			 (write-string fstr stream)
			 (write-char (if marker
					 marker
					 (format-exponent-marker number))
				     stream)
			 (write-char (if (minusp expt) #\- #\+) stream)
			 (when e 
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr)))
			     (write-char #\0 stream)))
			 (write-string estr stream)))))))))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "Cannot specify the colon modifier with this directive."))
  (bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-general stream (next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(defun format-general (stream number w d e k ovf pad marker atsign)
  ;;The Excelsior edition does not say what to do if
  ;;the argument is not a float.  Here, we adopt the
  ;;conventions used by ~F and ~E.
  (if (floatp number)
      (format-general-aux stream number w d e k ovf pad marker atsign)
      (if (rationalp number)
	  (format-general-aux stream
			      (coerce number 'single-float)
			      w d e k ovf pad marker atsign)
	  (let ((*print-base* 10))
	    (format-write-field stream
				(princ-to-string number)
				w 1 0 #\space t)))))

(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (multiple-value-bind (ignore n) 
		       (lisp::scale-exponent (abs number))
    (declare (ignore ignore))
    ;;Default d if omitted.  The procedure is taken directly
    ;;from the definition given in the manual, and is not
    ;;very efficient, since we generate the digits twice.
    ;;Future maintainers are encouraged to improve on this.
    (unless d
      (multiple-value-bind (str len) 
			   (lisp::flonum-to-string (abs number))
	(declare (ignore str))
	(let ((q (if (= len 1) 1 (1- len))))
	  (setq d (max q (min n 7))))))
    (let* ((ee (if e (+ e 2) 4))
	   (ww (if w (- w ee) nil))
	   (dd (- d n)))
      (cond ((<= 0 dd d)
	     (let ((char (if (format-fixed-aux stream number ww dd nil
					       ovf pad atsign)
			     ovf
			     #\space)))
	       (dotimes (i ee) (write-char char stream))))
	    (t
	     (format-exp-aux stream number w d e (or k 1)
			     ovf pad marker atsign))))))

(def-format-directive #\$ (colonp atsignp params)
  (bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    `(format-dollars stream (next-arg) ,d ,n ,w ,pad ,colonp ,atsignp)))

(defun format-dollars (stream number d n w pad colon atsign)
  (if (rationalp number) (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
	     (signlen (length signstr)))
	(multiple-value-bind (str strlen ig2 ig3 pointplace)
			     (lisp::flonum-to-string number nil d nil)
	  (declare (ignore ig2 ig3))
	  (when colon (write-string signstr stream))
	  (dotimes (i (- w signlen (- n pointplace) strlen))
	    (write-char pad stream))
	  (unless colon (write-string signstr stream))
	  (dotimes (i (- n pointplace)) (write-char #\0 stream))
	  (write-string str stream)))
      (let ((*print-base* 10))
	(format-write-field stream
			    (princ-to-string number)
			    w 1 0 #\space t))))

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "Cannot specify either colon or atsign for this directive."))
  (if params
      (bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (terpri stream)))
      '(terpri stream)))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "Cannot specify either colon or atsign for this directive."))
  (if params
      (bind-defaults ((count 1)) params
	`(progn
	   (fresh-line stream)
	   (dotimes (i (1- ,count))
	     (terpri stream))))
      '(fresh-line stream)))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "Cannot specify either colon or atsign for this directive."))
  (if params
      (bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char #\page stream)))
      '(write-char #\page stream)))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "Cannot specify either colon or atsign for this directive."))
  (if params
      (bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint
	   "Cannot specify both colon and atsign for this directive."))
  (values (bind-defaults () params
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

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (bind-defaults ((n 1) (m 1)) params
	`(pprint-tab ,(if atsignp :section-relative :section)
		     ,n ,m stream))
      (if atsignp
	  (bind-defaults ((colrel 1) (colinc 1)) params
	    `(format-relative-tab stream ,colrel ,colinc))
	  (bind-defaults ((colnum 1) (colinc 1)) params
	    `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-directive #\* (colonp atsignp params)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint "Cannot specify both colon and at-sign.")
	  (bind-defaults ((posn 0)) params
	    `(if (<= 0 ,posn (length orig-args))
		 (setf args (subseq orig-args ,posn))
		 (error 'format-error
			:complaint "Index ~D out of bounds.  Should have been ~
				    between 0 and ~D."
			:arguments (list ,posn (length orig-args))))))
      (if colonp
	  (bind-defaults ((n 1)) params
	    `(do ((cur-posn 0 (1+ cur-posn))
		  (arg-ptr orig-args (cdr arg-ptr)))
		 ((eq arg-ptr args)
		  (let ((new-posn (- cur-posn ,n)))
		    (if (<= 0 new-posn (length orig-args))
			(setf args (subseq orig-args new-posn))
			(error 'format-error
			       :complaint
			       "Index ~D out of bounds.  Should have been ~
				between 0 and ~D."
			       :arguments
			       (list new-posn (length orig-args))))))))
	  (if params
	      (bind-defaults ((n 1)) params
		`(dotimes (i ,n)
		   (next-arg)))
	      '(next-arg)))))

(def-format-directive #\? (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint "Cannot specify the colon modifier."))
  (bind-defaults () params
    `(handler-case
	 ,(if atsignp
	      '(setf args (%format stream (next-arg) orig-args args))
	      '(%format stream (next-arg) (next-arg)))
       (format-error (condition)
		     (error 'format-error
			    :complaint
			    "~A~%while processing indirect format string:"
			    :arguments (list condition)
			    :print-banner nil)))))

(def-format-directive #\_ (colonp atsignp params)
  (bind-defaults () params
    `(pprint-newline ,(if colonp
			  (if atsignp
			      :mandatory
			      :fill)
			  (if atsignp
			      :miser
			      :linear))
		     stream)))

(def-format-directive #\W (colonp atsignp params)
  (bind-defaults () params
    `(write (next-arg) :stream stream
	    ,@(when colonp
		'(:pretty t))
	    ,@(when atsignp
		'(:level nil :length nil)))))

(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "Cannot specify the at-sign modifier."))
  (bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint "No corresponding close paren."))
    (let* ((posn (position close directives))
	   (before (subseq directives 0 posn))
	   (after (subseq directives (1+ posn))))
      (values
       (bind-defaults () params
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

(def-complex-format-directive #\) ()
  (error 'format-error
	 :complaint "No corresponding open paren."))

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (let ((sublists nil)
	(last-semi-with-colon-p nil)
	(remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
	(unless close-or-semi
	  (error 'format-error
		 :complaint "No corresponding close bracket."))
	(let ((posn (position close-or-semi remaining)))
	  (push (subseq remaining 0 posn) sublists)
	  (setf remaining (subseq remaining (1+ posn)))
	  (when (char= (format-directive-character close-or-semi) #\])
	    (return))
	  (setf last-semi-with-colon-p
		(format-directive-colonp close-or-semi)))))
    (values
     (if atsignp
	 (if colonp
	     (error 'format-error
		    :complaint
		    "Cannot specify both the colon and at-sign modifiers.")
	     (if (cdr sublists)
		 (error 'format-error
			:complaint
			"Can only specify one section")
		 (bind-defaults () params
		   `(let ((prev-args args)
			  (arg (next-arg)))
		      (when arg
			(setf args prev-args)
			,@(expand-directive-list (car sublists)))))))
	 (if colonp
	     (if (= (length sublists) 2)
		 (bind-defaults () params
		   `(if (next-arg)
			(progn
			  ,@(expand-directive-list (car sublists)))
			(progn
			  ,@(expand-directive-list (cadr sublists)))))
		 (error 'format-error
			:complaint
			"Must specify exactly two sections."))
	     (bind-defaults ((index '(next-arg))) params
	       (let ((clauses nil))
		 (when last-semi-with-colon-p
		   (push `(t ,@(expand-directive-list (pop sublists)))
			 clauses))
		 (let ((count (length sublists)))
		   (dolist (sublist sublists)
		     (push `(,(decf count)
			     ,@(expand-directive-list sublist))
			   clauses)))
		 `(case ,index ,@clauses)))))
     remaining)))

(def-complex-format-directive #\; ()
  (error 'format-error
	 :complaint
	 "~~; not contained within either ~~[...~~] or ~~<...~~>."))

(def-complex-format-directive #\] ()
  (error 'format-error
	 :complaint
	 "No corresponding open bracket."))

(def-complex-format-directive #\{ (colonp atsignp params directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint
	     "No corresponding close brace."))
    (let* ((closed-with-colon (format-directive-colonp close))
	   (posn (position close directives))
	   (insides
	    (if (zerop posn)
		`((handler-case
		      (setf args (%format stream inside-string orig-args args))
		    (format-error
		     (condition)
		     (error 'format-error
			    :complaint
			    "~A~%while processing indirect format string:"
			    :arguments (list condition)
			    :print-banner nil))))
		(let ((*up-up-and-out-allowed* colonp))
		  (expand-directive-list (subseq directives 0 posn))))))
      (flet ((compute-loop (count)
	       `(loop
		  ,@(unless closed-with-colon
		      '((when (null args)
			  (return))))
		  ,@(when count
		      `((when (and ,count (minusp (decf ,count)))
			  (return))))
		  ,@(if colonp
			`((let* ((orig-args (next-arg))
				 (outside-args args)
				 (args orig-args))
			    (declare (ignorable orig-args outside-args args))
			    (block nil
			      ,@insides)))
			insides)
		  ,@(when closed-with-colon
		      '((when (null args)
			  (return)))))))
	(let ((loop
		(if params
		    (bind-defaults ((count nil)) params
		      (compute-loop count))
		    (compute-loop nil))))
	  (setf loop
	    `(block ,(if colonp 'outside-loop nil)
	       ,loop))
	  (unless atsignp
	    (setf loop
		  `(let* ((orig-args (next-arg))
			  (args orig-args))
		     (declare (ignorable orig-args args))
		     ,loop)))
	  (when (zerop posn)
	    (setf loop
		  `(let ((inside-string (next-arg)))
		     ,loop)))
	  (values loop (subseq directives (1+ posn))))))))

(def-complex-format-directive #\} ()
  (error 'format-error
	 :complaint "No corresponding open brace."))

(def-complex-format-directive #\< (colonp atsignp params directives)
  (let ((first-semi nil)
	(close nil)
	(remaining directives))
    (collect ((segments))
      (loop
	(let ((close-or-semi (find-directive remaining #\> t)))
	  (unless close-or-semi
	    (error 'format-error
		   :complaint "No corresponding close bracket."))
	  (let ((posn (position close-or-semi remaining)))
	    (segments (subseq remaining 0 posn))
	    (setf remaining (subseq remaining (1+ posn))))
	  (when (char= (format-directive-character close-or-semi)
		       #\>)
	    (setf close close-or-semi)
	    (return))
	  (unless first-semi
	    (setf first-semi close-or-semi))))
      (values
       (if (format-directive-colonp close)
	   (expand-format-logical-block (segments) colonp atsignp
					first-semi close params)
	   (expand-format-justification (segments) colonp atsignp
					first-semi params))
       remaining))))

(defun expand-format-justification
       (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
	 (and first-semi
	      (format-directive-colonp first-semi))))
    (bind-defaults ((mincol 0) (colinc 1) (minpad 0) (padchar #\space)) params
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
		 ,(bind-defaults ((extra 0)
				  (line-len '(or (lisp::line-length stream)
						 72)))
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

(defun format-justification (stream newline-prefix extra-space line-len strings
			     pad-left pad-right mincol colinc minpad padchar)
  (when (and (not pad-left) (not pad-right) (null (cdr strings)))
    (setf pad-left t))
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
	 (padding (- length chars)))
    (when (and newline-prefix
	       (> (+ (lisp::charpos stream) length extra-space) line-len))
      (write-string newline-prefix stream))
    (flet ((do-padding ()
	     (let ((pad-len (truncate padding num-gaps)))
	       (decf padding pad-len)
	       (decf num-gaps)
	       (dotimes (i pad-len) (write-char padchar stream)))))
      (when pad-left
	(do-padding))
      (when strings
	(write-string (car strings) stream)
	(dolist (string (cdr strings))
	  (do-padding)
	  (write-string string stream)))
      (when pad-right
	(do-padding)))))

(def-complex-format-directive #\> ()
  (error 'format-error
	 :complaint "No corresponding open bracket."))

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "Cannot specify the at-sign modifier."))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint "Attempt to use ~~:^ outside a ~~:{...~~} construct."))
  `(when ,(case (length params)
	    (0 (if colonp
		   '(null outside-args)
		   '(null args)))
	    (1 (bind-defaults ((count 0)) params
		 `(zerop ,count)))
	    (2 (bind-defaults ((arg1 0) (arg2 0)) params
		 `(= ,arg1 ,arg2)))
	    (t (bind-defaults ((arg1 0) (arg2 0) (arg3 0)) params
		 `(<= ,arg1 ,arg2 ,arg3))))
     ,(if colonp
	  '(return-from outside-loop nil)
	  '(return))))

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((slash (position #\/ string :start start :end (1- end)
			 :from-end t)))
    (unless slash
      (error 'format-error
	     :complaint "Malformed ~~/ directive."))
    (let* ((name (string-upcase (let ((foo string))
				  ;; Hack alert: This is to keep the compiler
				  ;; quit about deleting code inside the subseq
				  ;; expansion.
				  (subseq foo (1+ slash) (1- end)))))
	   (first-colon (position #\: name))
	   (last-colon (if first-colon (position #\: name :from-end t)))
	   (package-name (if last-colon
			     (subseq name (1+ last-colon))
			     "USER"))
	   (package (find-package package-name)))
      (unless package
	(error 'format-error
	       :complaint "No package named ``~A''."
	       :arguments (list package-name)))
      (let ((symbol (intern (if first-colon
				(subseq name 0 first-colon)
				name)
			    package)))
	(collect ((param-names) (bindings))
	  (dolist (param params)
	    (let ((param-name (gensym)))
	      (param-names param-name)
	      (bindings `(,param-name
			  ,(case param
			     (:arg '(next-arg))
			     (:remaining '(length args))
			     (t param))))))
	  `(let ,(bindings)
	     (,symbol stream (next-arg) ,colonp ,atsignp
		      ,@(param-names))))))))
