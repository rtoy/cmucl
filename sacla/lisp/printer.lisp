;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: printer.lisp,v 1.14 2004/03/01 05:18:11 yuji Exp $
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

;;; Commentary:

;; Need to load reader.lisp in advance for the function SYNTAX-TYPE.
;;

;;; printer control variables
;; number
(defvar *print-base* 10
  "The radix in which the printer will print rationals.")
(defvar *print-radix* nil
  "If true, print a radix specifier when printing a rational number.")

;; symbol
(defvar *print-case* :upcase
  "One of the symbols :upcase, :downcase, or :capitalize.")
(defvar *print-gensym* t
  "If true, print `#:' before apparently uninterned symbols.")

;; container
(defvar *print-array* t                 ; implementation-dependent
  "If true, arrays are printed in readable #(...), #*, or #nA(...) syntax.")
(defvar *print-level* nil
  "Control how many levels deep a nested object will print.")
(defvar *print-length* nil
  "Control how many elements at a given level are printed.")
(defvar *print-circle* nil
  "If true, detect circularity and sharing in an object being printed.")

;; symbol, string
(defvar *print-escape* t
  "If false, escape characters and package prefixes are not output.")

;; readability
;; *print-readably*
;; true
;;   true: *print-escape*, *print-array*, and *print-gensym*
;;   false: *print-length*, *print-level*, and *print-lines*
(defvar *print-readably* nil
  "If true, print objects readably.")

;; layout
(defvar *print-pretty* t                ; implementation-dependent
  "If true, the pretty printer is used when printing.")
;;(defvar *print-pprint-dispatch*
;;  )
(defvar *print-lines* nil
  "Limit on the number of output lines produced when pretty printing.")
(defvar *print-miser-width* nil         ; implementation-dependent
  "Switch to a compact style of output whenever the width available for printing a substructure is less than or equal to this many ems when pretty printing.")
(defvar *print-right-margin* nil
  "Specify the right margin to use when the pretty printer is making layout decisions.")

(defmacro with-standard-io-syntax (&rest forms)
  "Bind all reader/printer control vars to the standard values then eval FORMS."
  `(let ((*package* (find-package "CL-USER"))
         (*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         ;;(*print-pprint-dispatch* *standard-print-pprint-dispatch*)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-right-margin* nil)
         (*read-base* 10)
         (*read-default-float-format* 'single-float)
         (*read-eval* t)
         (*read-suppress* nil)
         (*readtable* (copy-readtable nil)))
    ,@forms))

(defgeneric print-object (object stream))

(defun write (object &key
	      ((:array *print-array*) *print-array*)
	      ((:base *print-base*) *print-base*)
	      ((:case *print-case*) *print-case*)
	      ((:circle *print-circle*) *print-circle*)
	      ((:escape *print-escape*) *print-escape*)
	      ((:gensym *print-gensym*) *print-gensym*)
	      ((:length *print-length*) *print-length*)
	      ((:level *print-level*) *print-level*)
	      ((:lines *print-lines*) *print-lines*)
	      ((:miser-width *print-miser-width*) *print-miser-width*)
	      ((:pprint-dispatch *print-pprint-dispatch*)
	       *print-pprint-dispatch*)
	      ((:pretty *print-pretty*) *print-pretty*)
	      ((:radix *print-radix*) *print-radix*)
	      ((:readably *print-readably*) *print-readably*)
	      ((:right-margin *print-right-margin*) *print-right-margin*)
	      (stream *standard-output*))
  ;; http://www.lispworks.com/reference/HyperSpec/Body/22_ab.htm
  ;; 22.1.2 Printer Dispatching              
  ;; The Lisp printer makes its determination of how to print an object as
  ;; follows: If the value of *print-pretty* is true, printing is controlled
  ;; by the current pprint dispatch table; see Section 22.2.1.4 (Pretty Print
  ;; Dispatch Tables).
  ;; Otherwise (if the value of *print-pretty* is false), the object's 
  ;; print-object method is used;
  ;; see Section 22.1.3 (Default Print-Object Methods).
  (if *print-pretty*
      (print-object-prettily object stream)
      (print-object object stream))
  object)

(defun write-to-string (object &key
	      ((:array *print-array*) *print-array*)
	      ((:base *print-base*) *print-base*)
	      ((:case *print-case*) *print-case*)
	      ((:circle *print-circle*) *print-circle*)
	      ((:escape *print-escape*) *print-escape*)
	      ((:gensym *print-gensym*) *print-gensym*)
	      ((:length *print-length*) *print-length*)
	      ((:level *print-level*) *print-level*)
	      ((:lines *print-lines*) *print-lines*)
	      ((:miser-width *print-miser-width*) *print-miser-width*)
	      ((:pprint-dispatch *print-pprint-dispatch*)
	       *print-pprint-dispatch*)
	      ((:pretty *print-pretty*) *print-pretty*)
	      ((:radix *print-radix*) *print-radix*)
	      ((:readably *print-readably*) *print-readably*)
	      ((:right-margin *print-right-margin*) *print-right-margin*))
  (with-output-to-string (stream)
    (if *print-pretty*
	(print-object-prettily object stream)
	(print-object object stream))))

(defun prin1 (object &optional output-stream)
  (write object :stream output-stream :escape t))

(defun prin1-to-string (object) (write-to-string object :escape t))

(defun princ (object &optional output-stream)
  (write object :stream output-stream :escape nil :readably nil))

(defun princ-to-string (object)
  (write-to-string object :escape nil :readably nil))

(defun print (object &optional output-stream)
  (terpri output-stream)
  (prin1 object output-stream)
  (write-char #\Space output-stream)
  object)

(defun pprint (object &optional output-stream)
  (terpri output-stream)
  (write object :stream output-stream :pretty t :escape t)
  (values))


;; function    pprint-dispatch
;; macro       pprint-logical-block
;; local macro pprint-pop
;; local macro pprint-exit-if-list-exhausted
;; function    pprint-newline
;; function    pprint-tab
;; function    pprint-fill, pprint-linear, pprint-tabular
;; function    pprint-indent

(defun printer-escaping-enabled-p () (or *print-escape* *print-readably*))

(defmethod print-object ((object integer) stream) (print-integer object stream))
(defun print-integer (integer stream)
  (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
	digits)
    (loop with n = (abs integer)
	  do (multiple-value-bind (q r) (floor n *print-base*)
	       (push (char chars r) digits)
	       (setq n q))
	  until (zerop n))
    (when *print-radix*
      (case *print-base*
	(2 (write-string "#b" stream))
	(8 (write-string "#o" stream))
	(16 (write-string "#x" stream))
	(10 nil)
	(t (write-char #\# stream)
	   (let ((base *print-base*)
		 (*print-base* 10)
		 (*print-radix* nil))
	     (print-integer base stream))
	   (write-char #\r stream))))
    (write-string (concatenate 'string
			       (when (minusp integer) '(#\-))
			       digits
			       (when (and *print-radix* (= *print-base* 10))
				 "."))
		  stream)
    integer))

(defmethod print-object ((ratio ratio) stream)
  ;; http://www.lispworks.com/reference/HyperSpec/Body/v_pr_bas.htm
  ;; For integers, base ten is indicated by a trailing decimal point instead
  ;; of a leading radix specifier; for ratios, #10r is used.
  (if (and *print-radix* (= *print-base* 10))
      (progn
        (write-string "#10r" stream)
        (let ((*print-radix* nil))
          (print-integer (numerator ratio) stream)))
      (print-integer (numerator ratio) stream))
  (write-char #\/ stream)
  (let ((*print-radix* nil)) (print-integer (denominator ratio) stream))
  ratio)

(defmethod print-object ((complex complex) stream)
  (write-string "#C(" stream)
  (print-object (realpart complex) stream)
  (write-char #\Space stream)
  (print-object (imagpart complex) stream)
  (write-char #\))
  complex)


(defmethod print-object ((character character) stream)
  (cond
    ((printer-escaping-enabled-p)
     (write-string "#\\" stream)
     (if (and (graphic-char-p character) (not (char= character #\Space)))
	 (write-char character stream)
	 (write-string (char-name character) stream)))
    (t (write-char character stream)))
  character)

(defun string-invert (str)
  (cond
    ((every #'(lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) str)
     (map 'string #'char-downcase str))
    ((every #'(lambda (c) (or (not (alpha-char-p c)) (lower-case-p c))) str)
     (map 'string #'char-upcase str))
    (t str)))

(defun make-str (chars)
  (make-array (length chars) :element-type 'character :initial-contents chars))

(defun print-symbol-as-is (symbol stream)
  (let ((name (symbol-name symbol)))
    (ecase (readtable-case *readtable*)
      (:upcase
       (write-string
	(ecase *print-case*
	  (:upcase name)
	  (:downcase (map 'string #'char-downcase name))
	  (:capitalize
	   (make-str (loop for c across name and prev = nil then c
			   collecting
			   (if (and (upper-case-p c) prev (alpha-char-p prev))
			       (char-downcase c)
			       c)))))
	stream))
      (:downcase
       (write-string
	(ecase *print-case*
	  (:upcase (map 'string #'char-upcase name))
	  (:downcase name)
	  (:capitalize
	   (make-str (loop for c across name and prev = nil then c
			   collecting
			   (if (and (lower-case-p c)
				    (or (null prev) (not (alpha-char-p prev))))
			       (char-upcase c)
			       c)))))
	stream))
      (:preserve (write-string name stream))
      (:invert (write-string (string-invert name) stream)))
    symbol))

(defun print-name-escaping (name stream &key force-escaping)
  (let ((readtable-case (readtable-case *readtable*)))
    (if (or force-escaping
	    (loop with standard-table = (copy-readtable nil)
	     for c across name
	     thereis (not (and (eq (syntax-type c standard-table) :constituent)
			       (eq (syntax-type c) :constituent))))
	    (notevery #'graphic-char-p name)
	    (and (eq readtable-case :upcase) (some 'lower-case-p name))
	    (and (eq readtable-case :downcase) (some 'upper-case-p name)))
	(let ((escaped (loop for c across name
			     if (find c '(#\\ #\|)) append (list #\\ c)
			     else collect c)))
	  (write-string (concatenate 'string "|" escaped "|") stream))
	(write-string (case readtable-case
			((:upcase :downcase)
			 (ecase *print-case*
			   (:upcase (string-upcase name))
			   (:downcase (string-downcase name))
			   (:capitalize (string-capitalize name))))
			(:invert
			 (cond
			   ((notany #'both-case-p name) name)
			   ((notany #'upper-case-p name) (string-upcase name))
			   ((notany #'lower-case-p name) (string-downcase name))
			   (t name)))
			(t name))
		      stream))))

(defun print-symbol-escaping (symbol stream)
  (let* ((name (symbol-name symbol))
	 (accessible-p (eq symbol (find-symbol name))))
    (cond
      (accessible-p nil)
      ((symbol-package symbol)
       (let ((package-name (package-name (symbol-package symbol))))
	 (unless (string= package-name "KEYWORD")
	   (print-name-escaping package-name stream))
	 (multiple-value-bind (symbol status) (find-symbol name package-name)
	   (declare (ignore symbol))
	   (write-string (if (eq status :external) ":" "::") stream))))
      ((or *print-readably* *print-gensym*) (write-string "#:" stream))
      (t nil))
    (print-name-escaping
     name stream
     :force-escaping (and accessible-p
			  (every #'(lambda (c) (digit-char-p c *print-base*))
				 name)))
    symbol))

(defmethod print-object ((symbol symbol) stream)
  (funcall (if (printer-escaping-enabled-p)
	       #'print-symbol-escaping
	       #'print-symbol-as-is)
	   symbol
	   stream))


(defvar *shared-object-table* (make-hash-table))
(defvar *shared-object-label* (make-hash-table))
(defvar *shared-object-label-counter* 0)
(defvar *current-print-level* 0)

(defun print-max-level-p ()
  (and (not *print-readably*)
       *print-level*
       (= *current-print-level* *print-level*)))
(defun print-max-length-p (n)
  (and (not *print-readably*) *print-length* (= n *print-length*)))

(defun inc-shared-object-reference (object)
  (if (and (symbolp object) (symbol-package object))
      0
      (multiple-value-bind (n present-p) (gethash object *shared-object-table*)
        (if present-p
            (progn (when (zerop n)
                     (setf (gethash object *shared-object-label*)
                           (incf *shared-object-label-counter*)))
                   (incf (gethash object *shared-object-table*)))
            (setf (gethash object *shared-object-table*) 0)))))

(defmethod search-shared-object :around ((object t))
  (if (zerop *current-print-level*)
      (progn (setq *shared-object-label* (clrhash *shared-object-label*)
                   *shared-object-table* (clrhash *shared-object-table*)
                   *shared-object-label-counter* 0)
             (inc-shared-object-reference object)
             (call-next-method object)
             (maphash #'(lambda (object n)
                          (if (zerop n)
                              (remhash object *shared-object-table*)
                              (setf (gethash object *shared-object-table*)
                                    0)))
                      *shared-object-table*))
      (when (zerop (inc-shared-object-reference object))
        (call-next-method object))))

(defun search-shared-element (object)
  (let ((*current-print-level* (1+ *current-print-level*)))
    (unless (print-max-level-p) (search-shared-object object))))

(defmethod search-shared-object ((object t))) ; do nothing
(defmethod search-shared-object ((list list))
  (do ((x list)
       (l 0 (1+ l)))
      ((or (print-max-level-p) (print-max-length-p l) (atom x)))
    (search-shared-element (car x))
    (setq x (cdr x))
    (when (plusp (inc-shared-object-reference x))
      (return))))

(defmethod search-shared-object ((vector vector))
  (do ((i 0 (1+ i)))
      ((or (= i (length vector)) (print-max-level-p) (print-max-length-p i)))
    (search-shared-element (aref vector i))))

(defmethod search-shared-object ((array array))
  (do ((i 0 (1+ i)))
      ((or (= i (array-total-size array))
           (print-max-level-p) (print-max-length-p i)))
    (search-shared-element (row-major-aref array i))))

(defun print-element (object stream)
  (let ((*current-print-level* (1+ *current-print-level*)))
    (multiple-value-bind (n present-p) (gethash object *shared-object-table*)
      (if (and present-p *print-circle*)
          (if (zerop n)
              (progn
                (print-label object stream)
                (print-object object stream))
              (print-reference object stream))
          (print-object object stream)))))

(defun print-label (object stream)
  (multiple-value-bind (n present-p) (gethash object *shared-object-label*)
    (assert present-p)
    (write-string "#" stream)
    (let ((*print-base* 10) (*print-radix* nil)) (print-integer n stream))
    (write-string "=" stream)
    (incf (gethash object *shared-object-table*))))

(defun print-reference (object stream)
  (multiple-value-bind (n present-p) (gethash object *shared-object-label*)
    (assert present-p)
    (write-string "#" stream)
    (let ((*print-base* 10) (*print-radix* nil)) (print-integer n stream))
    (write-string "#" stream)))

(defmethod print-object ((list cons) stream)
  (when (and *print-circle* (zerop *current-print-level*))
    (search-shared-object list))
  (if (print-max-level-p)
      (write-string "#" stream)
      (let ((x list)
            (l 0))
        (multiple-value-bind (n present-p) (gethash x *shared-object-table*)
          (when (and (zerop *current-print-level*) present-p *print-circle*)
            (print-label x stream))
          (write-string "(" stream)
          (loop (when (atom x)
                  (when x
                    (write-string " . " stream)
                    (print-element x stream))
                  (write-string ")" stream)
                  (return))
                (when (print-max-length-p l)
                  (write-string "...)" stream)
                  (return))
                (print-element (car x) stream)
                (setq x (cdr x)
                      l (1+ l))
                (when (consp x)
                  (write-string " " stream)
                  (multiple-value-setq (n present-p)
                    (gethash x *shared-object-table*))
                  (when (and present-p *print-circle*)
                    (write-string ". " stream)
                    (if (zerop n)
                        (print-element x stream)
                        (print-reference x stream))
                    (write-string ")" stream)
                    (return))))))))

(defmethod print-object :around ((array array) stream)
  (cond
    ((and (not *print-readably*) (not *print-array*) (not (stringp array)))
     ;; 22.1.3.4 Printing Strings
     ;; http://www.lispworks.com/reference/HyperSpec/Body/22_acd.htm
     ;; The printing of strings is not affected by *print-array*. 
     (print-unreadable-object (array stream :type t :identity t)))
    ((and (print-max-level-p) (not (stringp array)) (not (bit-vector-p array)))
     ;; Variable *PRINT-LEVEL*, *PRINT-LENGTH*
     ;; http://www.lispworks.com/reference/HyperSpec/Body/v_pr_lev.htm
     ;; *print-level* and *print-length* affect the printing of
     ;; an any object printed with a list-like syntax. They do not affect
     ;; the printing of symbols, strings, and bit vectors.
     (write-string "#" stream))
    (t (when (and *print-circle* (zerop *current-print-level*)
                  (not (stringp array)) (not (bit-vector-p array)))
         (search-shared-object array)
         (multiple-value-bind (n present-p)
             (gethash array *shared-object-table*)
           (declare (ignore n))
           (when present-p (print-label array stream))))
       (call-next-method array stream))))

(defmethod print-object ((vector vector) stream)
  (let ((l 0)
        (length (length vector)))
    (write-string "#(" stream)
    (loop (when (= l length)
            (write-string ")" stream)
            (return))
          (when (print-max-length-p l)
            (write-string "...)" stream)
            (return))
          (print-element (aref vector l) stream)
          (setq l (1+ l))
          (when (< l length) (write-string " " stream)))))

(defmethod print-object ((array array) stream)
  (let* ((dimensions (array-dimensions array))
         (indices (make-list (array-rank array) :initial-element 0)))
    (labels
        ((p-array (i-list d-list)
           (cond
             ((print-max-level-p) (write-string "#" stream))
             ((null i-list) (print-element (apply #'aref array indices) stream))
             (t (write-string "(" stream)
                (do ((i 0 (1+ i)))
                    ((= i (car d-list)))
                  (when (plusp i) (write-string " " stream))
                  (when (print-max-length-p i)
                    (write-string "..." stream)
                    (return))
                  (setf (car i-list) i)
                  (if (null (cdr i-list))
                      (print-element (apply #'aref array indices) stream)
                      (let ((*current-print-level* (1+ *current-print-level*)))
                        (p-array (cdr i-list) (cdr d-list)))))
                (write-string ")" stream)))))
      (write-string "#" stream)
      (let ((*print-base* 10) (*print-radix* nil))
        (print-integer (array-rank array) stream))
      (write-string "A" stream)
      (p-array indices dimensions))))

(defmethod print-object ((string string) stream)
  (let ((escape-p (printer-escaping-enabled-p)))
    (when escape-p (write-char #\" stream))
    (loop for c across string
	  if (and escape-p (member c '(#\" #\\))) do (write-char #\\ stream)
	  do (write-char c stream))
    (when escape-p (write-char #\" stream))
    string))

(defmethod print-object ((bit-vector bit-vector) stream)
  (if (or *print-array* *print-readably*)
      (progn
	(write-string "#*" stream)
	(loop for bit across bit-vector
	      do (write-char (if (zerop bit) #\0 #\1) stream)))
      (print-unreadable-object (bit-vector stream :type t :identity t)))
  bit-vector)

(defmethod print-object ((object t) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defun print-object-prettily (object stream)
  (print-object object stream))



;; format
;; (defun format (destination format-control &rest args)
;;   (apply (if (stringp format-control) (formatter format-control) format-control)
;; 	    destination args))
;; 
;; 
;; (defmacro formatter (control-string)
;;   
;;   )
