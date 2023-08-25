;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: reader.lisp,v 1.13 2004/07/22 06:06:33 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defstruct (readtable (:predicate readtablep) (:copier nil))
  "Map characters into syntax types and reader macro functions."
  (syntax (make-hash-table) :type hash-table)
  (case :upcase :type (member :upcase :downcase :preserve :invert)))

(defvar *read-base* '10
  "The radix in which integers and ratios are to be read by the Lisp reader.")
(defvar *read-default-float-format* 'single-float
  "Used for a floating-point number having no exponent marker or e or E.")
(defvar *read-eval* 't
  "If true, the #. reader macro has its normal effect. If false, reader-error.")
(defvar *read-suppress* 'nil
  "If true, the reader reads an object and returns a primary value of nil.")
(defvar *readtable* nil
  "The current readtable. Control the parsing behavior of the Lisp reader.")


(defvar *sharp-equal-alist* nil)
(defvar *sharp-sharp-alist* nil)
(defvar *consing-dot-allowed* nil)
(defvar *consing-dot* (gensym))
(defvar *preserve-whitespace-p* nil)
(defvar *input-stream* nil)
(defvar *backquote-level* 0)
(defvar *dispatch-macro-char* nil)
(defvar *standard-readtable*)

(define-condition invalid-character-error (reader-error)
  ((character :type character :reader invalid-character-error-character
	      :initarg :character))
  (:report
   (lambda (condition stream)
     (format stream "Invalid character ~S is read."
	     (invalid-character-error-character condition)))))

(defun reader-error (&optional format-control &rest format-arguments)
  (error 'reader-error
	 :format-control format-control :format-arguments format-arguments))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Copy FROM-READTABLE. If TO-READTABLE is nil, a new table is to be created."
  (flet ((copy-syntax (src)
	   (let ((new (make-hash-table)))
	     (maphash
	      #'(lambda (k v)
		  (let ((plist (copy-list v)))
		    (setf (gethash k new) plist)
		    (when (getf plist :dispatch-table)
		      (let ((hash (make-hash-table)))
			(maphash #'(lambda (k v) (setf (gethash k hash) v))
				 (getf plist :dispatch-table))
			(setf (getf plist :dispatch-table) hash)))))
	      src)
	     new)))
    (let ((from (or from-readtable *standard-readtable*)))
      (if to-readtable
	  (prog1 to-readtable
	    (setf (readtable-syntax to-readtable)
		  (copy-syntax (readtable-syntax from)))
	    (setf (readtable-case to-readtable) (readtable-case from)))
	  (make-readtable :syntax (copy-syntax (readtable-syntax from))
			  :case (readtable-case from))))))

(defun syntax-type (char &optional (readtable *readtable*))
  (let ((plist (gethash char (readtable-syntax readtable))))
    (getf plist :syntax :constituent)))

(defun get-macro-character (char &optional (readtable *readtable*))
  "Return a macro function for CHAR and non-terminating-p as the secondary value."
  (unless readtable (setq readtable *standard-readtable*))
  (let ((plist (gethash char (readtable-syntax readtable))))
    (case (syntax-type char readtable)
      (:terminating-macro-char (values (getf plist :macro-function) nil))
      (:non-terminating-macro-char (values (getf plist :macro-function) t))
      (t (values nil nil)))))

(defun set-macro-character (char new-function
			    &optional non-terminating-p (readtable *readtable*))
  "Make CHAR a macro character associated with NEW-FUNCTION in READTABLE."
  (check-type char character)
  (check-type new-function function-designator)
  (when (null readtable)
    (error "Standard readtable must not be changed."))
  (let ((plist (gethash char (readtable-syntax readtable))))
    (setf (getf plist :syntax) (if non-terminating-p
				   :non-terminating-macro-char
				   :terminating-macro-char)
	  (getf plist :macro-function) new-function
	  (gethash char (readtable-syntax readtable)) plist))
  t)

(defun get-dispatch-macro-character (disp-char sub-char
				     &optional (readtable *readtable*))
  "Retrieve the dispatch function for DISP-CHAR and SUB-CHAR in READTABLE."
  (unless readtable (setq readtable *standard-readtable*))
  (unless (eq (get-macro-character disp-char readtable)
	      'dispatch-macro-character)
    (error "~S is not a dispatching macro character." disp-char))
  (let* ((syntax-table (readtable-syntax readtable))
	 (dispatch-table (getf (gethash disp-char syntax-table)
                               :dispatch-table))
	 (sub-char (char-upcase sub-char)))
    (multiple-value-bind (value present-p) (gethash sub-char dispatch-table)
      (cond
	((digit-char-p sub-char 10) nil)
	(present-p value)
	(t
	 #'(lambda (stream sub-char number)
	     (declare (ignore stream number))
	     (reader-error "No dispatch function defined for ~S."
                           sub-char)))))))

(defun set-dispatch-macro-character (disp-char sub-char new-function
				     &optional (readtable *readtable*))
  "Install NEW-FUNCTION as the dispatch function for DISP-CHAR and SUB-CHAR."
  (when (null readtable) (error "Standard readtable must not be changed."))
  (unless (eq (get-macro-character disp-char readtable)
	      'dispatch-macro-character)
    (error "~S is not a dispatch character." disp-char))
  (let* ((syntax-table (readtable-syntax readtable))
	 (dispatch-table (getf (gethash disp-char syntax-table)
                               :dispatch-table))
	 (sub-char (char-upcase sub-char)))
    (setf (gethash sub-char dispatch-table) new-function)
    t))

(defun make-dispatch-macro-character (char &optional non-terminating-p
				      (readtable *readtable*))
  "Make CHAR be a dispatching macro character in READTABLE."
  (when (null readtable) (error "Standard readtable must not be changed."))
  (set-macro-character char 'dispatch-macro-character
		       non-terminating-p readtable)
  
  (setf (getf (gethash char (readtable-syntax readtable)) :dispatch-table)
	(make-hash-table))
  t)

(defun dispatch-macro-character (stream char)
  (let ((n (when (digit-char-p (peek-char nil stream t nil t) 10)
	     (loop
	      with n = 0
	      for digit = (read-char stream t nil t)
	      do (setq n (+ (* n 10) (digit-char-p digit 10)))
	      while (digit-char-p (peek-char nil stream t nil t) 10)
	      finally (return n))))
	(*dispatch-macro-char* char)
	(sub-char (char-upcase (read-char stream t nil t))))
    (funcall (get-dispatch-macro-character char sub-char) stream sub-char n)))

(defun set-syntax-from-char (to-char from-char
			     &optional (to-readtable *readtable*)
			     (from-readtable *standard-readtable*))
  "Make the syntax of TO-CHAR in TO-READTABLE be the same as that of FROM-CHAR."
  (check-type to-char character)
  (check-type from-char character)
  (check-type to-readtable readtable)
  (unless from-readtable (setq from-readtable *standard-readtable*))
  (check-type from-readtable readtable)
  (let ((plist (copy-list (gethash from-char
				   (readtable-syntax from-readtable)))))
    (when (getf plist :dispatch-table)
      (let ((hash (make-hash-table)))
	(maphash #'(lambda (k v) (setf (gethash k hash) v))
		 (getf plist :dispatch-table))
	(setf (getf plist :dispatch-table) hash)))
    (setf (gethash to-char (readtable-syntax to-readtable)) plist)
    t))

(defun read-preserving-whitespace (&optional (input-stream *standard-input*)
				   (eof-error-p t) eof-value recursive-p)
  "Read an object but preserves any whitespace character after it."
  (let ((*preserve-whitespace-p* (if recursive-p *preserve-whitespace-p* t)))
    (read-lisp-object input-stream eof-error-p eof-value recursive-p)))

(defun read (&optional (input-stream *standard-input*)
	     (eof-error-p t) eof-value recursive-p)
  "Parse a printed representation from INPUT-STREAM and return the object."
  (let ((*preserve-whitespace-p* (when recursive-p *preserve-whitespace-p*)))
    (read-lisp-object input-stream eof-error-p eof-value recursive-p)))

(defun read-from-string (string &optional (eof-error-p t) eof-value
			 &key (start 0) end preserve-whitespace)
  "Read an object from the subsequence of string bounded by START and END."
  (let ((index nil))
    (values (with-input-from-string (stream string :index index
					    :start start :end end)
	      (funcall (if preserve-whitespace
			   #'read-preserving-whitespace
			   #'read)
		       stream eof-error-p eof-value))
	    index)))

(defun make-str (chars)
  (make-array (length chars) :element-type 'character :initial-contents chars))

(defun sharp-equal (stream sub-char n)
  (declare (ignore sub-char))
  (if *read-suppress*
      (values)
      (let* ((this (gensym))
	     (object (progn
                       (setq *sharp-sharp-alist* (acons n this
                                                        *sharp-sharp-alist*))
                       (read stream t nil t))))
        (when (null n)
          (reader-error "Missing label number for #=."))
        (when (assoc this *sharp-equal-alist*)
          (reader-error "#~D= is already defined." n))
	(when (eq object this)
	  (reader-error "need to tag something more than just #~D#." n))
	(setq *sharp-equal-alist* (acons this object *sharp-equal-alist*))
        object)))

(defun sharp-sharp (stream sub-char n)
  (declare (ignore sub-char stream))
  (unless *read-suppress*
    (unless n (reader-error "Label is missing for ##."))
    (let ((assoc (assoc n *sharp-sharp-alist*)))
      (unless assoc
	(reader-error "No object labeled ~D is seen." n))
      (cdr assoc))))

(defun read-delimited-list (char &optional (stream *standard-input*)
                            recursive-p)
  "Read objects until CHAR appears, then return a list of objects read."
  (let ((list (read-list char stream :allow-consing-dot nil)))
    (unless *read-suppress* list)))

(defun read-list (char &optional (stream *standard-input*)
                  &key allow-consing-dot)
  (let ((*consing-dot-allowed* allow-consing-dot)
	c stack values list)
    (loop
     (setq c (peek-char t stream t nil t))
     (when (char= char c)               ; found the closing parenthesis.
       (when (eq (first stack) *consing-dot*)
	 (error "Nothing appears after . in list."))
       (read-char stream t nil t)
       (setq list (if (eq (second stack) *consing-dot*)
                      (nreconc (cddr stack) (first stack))
                      (nreverse stack)))
       (return))
     (when (setq values (multiple-value-list (lisp-object? stream t nil t)))
       (if (eq (second stack) *consing-dot*)
	   (error "More than one object follows . in list.")
	   (push (car values) stack))))
    list))

(defun lisp-object? (stream eof-error-p eof-value recursive-p)
  (loop
   (let* ((c (read-char stream eof-error-p eof-value recursive-p)))
     (when (and (not eof-error-p) (eq c eof-value)) (return eof-value))
     (ecase (syntax-type c)
       (:invalid (error 'invalid-character-error :character c))
       (:whitespace 'skip)
       ((:single-escape :multiple-escape :constituent)
	(return (read-number-or-symbol stream c)))
       ((:terminating-macro-char :non-terminating-macro-char)
	(return (funcall (get-macro-character c) stream c)))))))

(defmethod general-nsublis ((alist list) (object t))
  object)

(defmethod general-nsublis :around ((alist cons) (object t))
  (let ((assoc (assoc object alist :test #'eq)))
    (if assoc
        (cdr assoc)
        (call-next-method alist object))))

(defmethod general-nsublis ((alist cons) (object cons))
  (setf (car object) (general-nsublis alist (car object)))
  (setf (cdr object) (general-nsublis alist (cdr object)))
  object)

(defmethod general-nsublis ((alist cons) (object array))
  (do ((i 0 (1+ i)))
      ((= i (array-total-size object)) object)
    (setf (row-major-aref object i)
          (general-nsublis alist (row-major-aref object i)))))

(defun read-lisp-object (stream eof-error-p eof-value recursive-p)
  (unless recursive-p
    (setq *sharp-equal-alist* nil
          *sharp-sharp-alist* nil))
  (let ((object (loop
                 (let ((values (multiple-value-list
                                (lisp-object? stream
                                              eof-error-p eof-value
                                              recursive-p))))
                   (when values
                     (return (unless *read-suppress* (car values))))))))
    (if (and (not recursive-p) *sharp-equal-alist*)
        (general-nsublis *sharp-equal-alist* object)
        object)))

(defun read-ch () (read-char *input-stream* nil nil t))
(defun read-ch-or-die () (read-char *input-stream* t nil t))
(defun unread-ch (c) (unread-char c *input-stream*))

(defun collect-escaped-lexemes (c)
  (ecase (syntax-type c)
    (:invalid (error 'invalid-character-error :character c))
    (:multiple-escape nil)
    (:single-escape (cons (read-ch-or-die)
			  (collect-escaped-lexemes (read-ch-or-die))))
    ((:constituent
      :whitespace :terminating-macro-char :non-terminating-macro-char)
     (cons c (collect-escaped-lexemes (read-ch-or-die))))))

(defun collect-lexemes (c &optional (stream *input-stream*))
  (let ((*input-stream* stream))
    (when c
      (ecase (syntax-type c)
	(:invalid (error 'invalid-character-error :character c))
	(:whitespace (when *preserve-whitespace-p* (unread-ch c)))
	(:terminating-macro-char (unread-ch c))
	(:multiple-escape (cons (collect-escaped-lexemes (read-ch-or-die))
				(collect-lexemes (read-ch))))
	(:single-escape (cons (list (read-ch-or-die))
			      (collect-lexemes (read-ch))))
	((:constituent :non-terminating-macro-char)
	 (cons c (collect-lexemes (read-ch))))))))

;; integer ::= [sign] decimal-digit+ decimal-point
;;           | [sign] digit+
;; ratio   ::= [sign] {digit}+ slash {digit}+
;; float   ::= [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
;;           | [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
;; exponent::= exponent-marker [sign] {digit}+

(defun construct-number (chars)
  (labels ((sign ()
	     (let ((c (and chars (car chars))))
	       (cond
		 ((eql c #\-) (pop chars) -1)
		 ((eql c #\+) (pop chars) +1)
		 (t +1))))
	   (digit* (&optional (base *read-base*))
	     (let ((pos (or (position-if-not #'(lambda (d)
                                                 (digit-char-p d base))
					     chars)
			    (length chars))))
	       (prog1 (subseq chars 0 pos)
		 (setq chars (subseq chars pos)))))
	   (int? (sign digits &optional (base *read-base*))
	     (when (and digits
			(every #'(lambda (d) (digit-char-p d base)) digits))
	       (* sign (reduce #'(lambda (a b) (+ (* base a) b))
			       (mapcar #'(lambda (d) (digit-char-p d base))
				       digits)))))
	   (float? (sign)
	     (let* ((int (digit* 10))
		    (fraction (when (eql (car chars) #\.)
				(pop chars) (digit* 10)))
		    (exp-marker (when (and chars
					   (find (char-upcase (car chars))
						 '(#\D #\E #\F #\L #\S)))
				  (char-upcase (pop chars))))
		    (exp-sign (and exp-marker (sign)))
		    (exp-digits (and exp-sign (digit*))))
	       (when (and (null chars)
			  (or fraction (and int exp-marker exp-digits)))
		 (float (* (int? sign (append int fraction) 10)
			   (expt 10 (- (or (int? exp-sign exp-digits 10) 0)
				       (length fraction))))
			(ecase (or exp-marker *read-default-float-format*)
			  (#\E                1.0e0)
			  ((#\D double-float) 1.0d0)
			  ((#\F single-float) 1.0f0)
			  ((#\L long-float)   1.0l0)
			  ((#\S short-float)  1.0s0)))))))
    (let ((sign (sign))
	  pos numerator denominator)
      (when chars
	(or
	 ;; [sign] digit+
	 (int? sign chars)
	 ;; [sign] decimal-digit+ decimal-point
	 (and (eql (car (last chars)) #\.) (int? sign (butlast chars) 10))
	 ;; [sign] {digit}+ slash {digit}+
	 (and (setq pos (position #\/ chars))
	      (setq numerator (int? sign (subseq chars 0 pos)))
	      (setq denominator (int? 1 (subseq chars (1+ pos))))
	      (not (zerop denominator))
	      (/ numerator denominator))
	 ;; [sign] {decimal-digit}* decimal-point {decimal-digit}+ [exponent]
	 ;; [sign] {decimal-digit}+ [decimal-point {decimal-digit}*] exponent
	 (float? sign))))))

	       

(defun ensure-external-symbol (name package)
  (multiple-value-bind (symbol status) (find-symbol name package)
    (unless (eq status :external)
      (cerror (if (null status)
		  "Intern and export symbol ~S in package ~S."
		  "Export symbol ~S in package ~S.")
	      "There is no external symbol by the name of ~S in package ~S."
	      name package)
      (export (setq symbol (intern name package)) package))
    symbol))

(defun construct-symbol (lexemes &key uninterned-symbol-wanted)
  (labels ((up (x) (if (listp x) (copy-list x) (list (char-upcase x))))
	   (down (x) (if (listp x) (copy-list x) (list (char-downcase x))))
	   (chars (lexemes)
	     (ecase (readtable-case *readtable*)
	       (:upcase (mapcan #'up lexemes))
	       (:downcase (mapcan #'down lexemes))
	       (:invert
		(let ((unescaped (remove-if-not #'alpha-char-p
						(remove-if #'listp lexemes))))
		  (mapcan (cond
			    ((every #'upper-case-p unescaped) #'down)
			    ((every #'lower-case-p unescaped) #'up)
			    (t #'(lambda (x)
				   (if (listp x) (copy-list x) (list x)))))
			  lexemes)))
	       (:preserve (mapcan #'(lambda (x)
				      (if (listp x) (copy-list x) (list x)))
				  lexemes))))
	   (name (lexemes)
	     (when (find #\: lexemes) (error "Too many package markers."))
	     (make-str (chars lexemes))))
    (let* ((pos (position #\: lexemes))
	   (external-p (and pos (not (eql (nth (1+ pos) lexemes) #\:))))
	   (package (when pos (name (subseq lexemes 0 pos))))
	   (name (name (subseq lexemes
                               (if pos (+ pos (if external-p 1 2)) 0)))))
      (values (cond
		(uninterned-symbol-wanted
		 (if package
		     (reader-error)
		     (make-symbol name)))
		(external-p
		 (ensure-external-symbol name package))
		(t (intern name (or package *package*))))))))

(defun read-number-or-symbol (stream c)
  (let ((lexemes (collect-lexemes c stream)))
    (assert lexemes)
    (unless *read-suppress*
      (cond
	((and lexemes (every #'(lambda (x) (eql x #\.)) lexemes))
	 (when (rest lexemes)
	   (reader-error "Tokens consisting of only dots are invalid."))
	 (when (not *consing-dot-allowed*)
	   (reader-error "Consing dot is not allowed."))
	 *consing-dot*)
	(t 
	 (or (and (every #'characterp lexemes) (construct-number lexemes))
	     (construct-symbol lexemes)))))))


;; backquote

(defconstant backquote (gensym))
(defconstant backquote-comma (gensym))
(defconstant backquote-comma-at (gensym))
(defconstant backquote-comma-dot (gensym))
(defun backquoted-expression-type (exp)
  (if (atom exp)
      :normal
      (cond
	((eq (first exp) backquote-comma) :comma)
	((eq (first exp) backquote-comma-at) :comma-at)
	((eq (first exp) backquote-comma-dot) :comma-dot)
	(t :normal))))

(defmacro backquote (object)
  (if (atom object)
      (if (simple-vector-p object)
	  (list 'apply #'vector (list backquote (concatenate 'list object)))
	  (list 'quote object))
      (let* ((list (copy-list object))
	     (last (loop for x = list then (cdr x)
			 until (or (atom (cdr x))
				   (find (cadr x) (list backquote
							backquote-comma
							backquote-comma-at
							backquote-comma-dot)))
			 finally (return (prog1 (cdr x) (setf (cdr x) nil)))))
	     (types (mapcar #'backquoted-expression-type list)))
	(append
	 (cons (if (notany #'(lambda (x) (eq x :comma-at)) types) 'nconc 'append)
	       (mapcar #'(lambda (x)
			   (ecase (backquoted-expression-type x)
			     (:normal (list 'list (list 'backquote x)))
			     (:comma (list 'list x))
			     ((:comma-at :comma-dot) x)))
		       list))
	 (list (ecase (backquoted-expression-type last)
		 (:normal (list 'quote last))
		 (:comma last)
		 (:comma-at (error ",@ after dot"))
		 (:comma-dot (error ",. after dot"))))))))

(defmacro backquote-comma (obj) obj)
(setf (macro-function backquote) (macro-function 'backquote))
(setf (macro-function backquote-comma) (macro-function 'backquote-comma))
(setf (macro-function backquote-comma-at) (macro-function 'backquote-comma))
(setf (macro-function backquote-comma-dot) (macro-function 'backquote-comma))


(defun read-comma-form (stream c)
  (declare (ignore c))
  (unless (> *backquote-level* 0)
    (error "Comma must be used in a backquoted expression."))
  (let ((*backquote-level* (1- *backquote-level*)))
    (case (peek-char t stream t nil t)
      (#\@ (read-char stream t nil t)
	   (list backquote-comma-at (read stream t nil t)))
      (#\. (read-char stream t nil t)
	   (list backquote-comma-dot (read stream t nil t)))
      (t (list backquote-comma (read stream t nil t))))))

(defun read-backquoted-expression (stream c)
  (declare (ignore c))
  (let ((*backquote-level* (1+ *backquote-level*)))
    (list backquote (read stream t nil t))))


(defun sharp-backslash (stream sub-char n)
  (declare (ignore n))
  (let* ((lexemes (collect-lexemes sub-char stream))
	 (str (make-str (mapcan #'(lambda (x)
				    (if (listp x) (copy-list x) (list x)))
				lexemes))))
    (unless *read-suppress*
      (cond
	((= 1 (length str)) (char str 0))
	((name-char str))
	(t (reader-error "Unrecognized character name: ~S" str))))))

(defun sharp-single-quote (stream sub-char n)
  (declare (ignore sub-char n))
  `(function ,(read stream t nil t)))

(defun sharp-left-parenthesis (stream sub-char n)
  (declare (ignore sub-char))
  (let ((list (read-delimited-list #\) stream t)))
    (unless *read-suppress*
      (when (and n (> (length list) n))
	(reader-error "vector is longer than specified length #~A*~A."
		      n list))
      (apply #'vector
	     (if (and n (< (length list) n))
		 (append list (make-list (- n (length list))
					 :initial-element (car (last list))))
		 list)))))

(defun sharp-asterisk (stream sub-char n)
  (declare (ignore sub-char))
  (let* ((*input-stream* stream)
	 (lexemes (collect-lexemes (read-ch)))
	 (bits (mapcar #'(lambda (d)
			   (unless (characterp d)
			     (error "Binary digit must be given"))
			   (digit-char-p d 2)) lexemes)))
    (unless *read-suppress*
      (unless (every #'(lambda (d) (digit-char-p d 2)) lexemes)
	(reader-error "Illegal bit vector format."))
      (when (and n (> (length bits) n))
	(reader-error "Bit vector is longer than specified length #~A*~A."
		      n (make-str lexemes)))
      (when (and n (> n 0) (zerop (length bits)))
	(reader-error
	 "At least one bit must be given for non-zero #* bit-vectors."))
      (make-array (or n (length bits)) :element-type 'bit
		  :initial-contents
		  (if (and n (< (length bits) n))
		      (append bits
			      (make-list (- n (length bits))
					 :initial-element (car (last bits))))
		      bits)))))

(defun sharp-colon (stream sub-char n)
  (declare (ignore sub-char n))
  (let* ((*input-stream* stream)
	 (lexemes (collect-lexemes (read-ch))))
    (unless *read-suppress*
      (construct-symbol lexemes :uninterned-symbol-wanted t))))

(defun sharp-dot (stream sub-char n)
  (declare (ignore sub-char n))
  (let ((object (read stream t nil t)))
    (unless *read-suppress*
      (unless *read-eval*
	(reader-error "Attempt to read #. while *READ-EVAL* is bound to NIL."))
      (eval object))))

(defun sharp-b (stream sub-char n)
  (declare (ignore n))
  (sharp-r stream sub-char 2))

(defun sharp-o (stream sub-char n)
  (declare (ignore n))
  (sharp-r stream sub-char 8))

(defun sharp-x (stream sub-char n)
  (declare (ignore n))
  (sharp-r stream sub-char 16))

(defun sharp-r (stream sub-char n)
  (cond
    (*read-suppress* (read stream t nil t))
    ((not n) (reader-error "Radix missing in #R."))
    ((not (<= 2 n 36)) (reader-error "Illegal radix for #R: ~D." n))
    (t (let ((rational (let ((*read-base* n)) (read stream t nil t))))
	 (unless (typep rational 'rational)
	   (reader-error "#~A (base ~D) value is not a rational: ~S."
			 sub-char n rational))
	 rational))))


(defun sharp-c (stream sub-char n)
  (declare (ignore sub-char n))
  (let ((pair (read stream t nil t)))
    (unless *read-suppress*
      (unless (and (listp pair) (= (length pair) 2))
	(reader-error "Illegal complex number format: #C~S" pair))
      (complex (first pair) (second pair)))))

(defun sharp-a (stream sub-char rank)
  (declare (ignore sub-char))
  (cond
    (*read-suppress* (read stream t nil t))
    ((null rank)
     (reader-error "Rank for #A notation is missing."))
    (t (let* ((contents (read stream t nil t))
	      (dimensions (loop repeat rank
				for x = contents then (first x)
				collect (length x))))
	 (make-array dimensions :initial-contents contents)))))


(defun find-default-constructor (name)
  (declare (ignore name)))
    
(defun sharp-s (stream sub-char n)
  (declare (ignore sub-char n))
  (let ((structure-spec (read stream t nil t)))
    (unless *read-suppress*
      (unless (listp structure-spec)
	(reader-error "Non list follows #S."))
      (unless (symbolp (first structure-spec))
	(reader-error "Structure type is not a symbol: ~S"
                      (car structure-spec)))
      (let* ((name (first structure-spec))
	     (plist (loop
		     for list on (rest structure-spec) by #'cddr
		     append (list (intern (string (first list)) "KEYWORD")
				  (second list))))
	     (class (find-class name nil)))
	(unless (typep class 'structure-class)
	  (reader-error "~S is not a defined structure type." name))
	(let ((constructor (find-default-constructor name)))
	  (apply constructor plist))))))

(defun sharp-p (stream sub-char n)
  (declare (ignore sub-char n))
  (let ((namestring (read stream t nil t)))
    (unless *read-suppress* (parse-namestring namestring))))



(defun featurep (x)
  (if (atom x)
      (member x *features*)
      (ecase (first x)
	(:not (not (featurep (second x))))
	(:and (every #'featurep (rest x)))
	(:or (some #'featurep (rest x))))))

(defun read-feature-test (stream)
  (let ((*package* (or (find-package "KEYWORD")
		       (error "KEYWORD package not found."))))
    (read stream t nil t)))

(defun sharp-plus (stream sub-char n)
  (declare (ignore sub-char n))
  (if (featurep (read-feature-test stream))
      (read stream t nil t)
      (let ((*read-suppress* t)) (read stream t nil t) (values))))

(defun sharp-minus (stream sub-char n)
  (declare (ignore sub-char n))
  (if (not (featurep (read-feature-test stream)))
      (read stream t nil t)
      (let ((*read-suppress* t)) (read stream t nil t) (values))))

(defun sharp-vertical-bar (stream sub-char n)
  (declare (ignore sub-char n))
  (loop for c = (read-char stream t nil t)
	if (and (char= c #\#) (char= (read-char stream t nil t) #\|))
	do (sharp-vertical-bar stream #\| nil)
	until (and (char= c #\|) (char= (read-char stream t nil t) #\#)))
  (values))


(defvar *standard-syntax-table*
  (let ((table (make-hash-table)))
    (mapc #'(lambda (x)
	      (let ((syntax (first x))
		    (chars (rest x)))
		(dolist (c chars)
		  (setf (gethash c table) `(:syntax ,syntax)))))
	  '((:whitespace #\Tab #\Newline #\Linefeed #\Page #\Return #\Space)
	    (:single-escape #\\)
	    (:multiple-escape #\|)))
    table))

(setq *standard-readtable* (make-readtable :syntax *standard-syntax-table*))

(set-macro-character #\` 'read-backquoted-expression nil *standard-readtable*)
(set-macro-character #\, 'read-comma-form nil *standard-readtable*)

(set-macro-character #\( #'(lambda (stream char)
			     (declare (ignore char))
			     (read-list #\) stream :allow-consing-dot t))
		     nil *standard-readtable*)

(set-macro-character #\) #'(lambda (stream char)
			     (declare (ignore stream char))
			     (error "Unmatched close parenthesis."))
		     nil *standard-readtable*)

(set-macro-character #\' #'(lambda (stream char)
			     (declare (ignore char))
			     `(quote ,(read stream t nil t)))
		     nil *standard-readtable*)

(set-macro-character #\; #'(lambda (stream char)
			     (declare (ignore char))
			     (loop
			      for c = (read-char stream nil nil t)
			      until (or (null c) (eql c #\Newline)))
			     (values))
		     nil *standard-readtable*)

(set-macro-character #\" #'(lambda (stream char)
			     (declare (ignore char))
			     (loop
			      for c = (read-char stream t nil t)
			      until (char= c #\")
			      if (eq :single-escape (syntax-type c))
			      collect (read-char stream t nil t) into chars
			      else
			      collect c into chars
			      finally
			      (return (make-array (length chars)
						  :element-type 'character
						  :initial-contents chars))))
		     nil *standard-readtable*)
				   

(make-dispatch-macro-character #\# t *standard-readtable*)
(mapc
 #'(lambda (pair)
     (set-dispatch-macro-character #\# (first pair) (second pair)
				   *standard-readtable*))
 '((#\\ sharp-backslash) (#\' sharp-single-quote) (#\( sharp-left-parenthesis)
   (#\* sharp-asterisk) (#\: sharp-colon) (#\. sharp-dot) (#\b sharp-b)
   (#\o sharp-o) (#\x sharp-x) (#\r sharp-r) (#\c sharp-c) (#\a sharp-a)
   (#\s sharp-s) (#\p sharp-p) (#\= sharp-equal) (#\# sharp-sharp)
   (#\+ sharp-plus) (#\- sharp-minus) (#\| sharp-vertical-bar)))

(setq *readtable* (copy-readtable nil))
