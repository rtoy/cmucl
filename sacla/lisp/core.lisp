;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: core.lisp,v 1.30 2004/05/26 07:57:52 yuji Exp $
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

;;; primitives
;;(defun alloc-pointers (n)
;;  (alloc-pointers n))


;;; objects

;; cons
(defstruct (cons (:constructor cons (car cdr))
                 (:conc-name "") (:predicate consp) (:copier nil))
  (car)
  (cdr)
  )

(defun rplaca (cons object)
  "Replace the car of CONS with OBJECT."
  (setf (car cons) object)
  cons)

(defun rplacd (cons object)
  "Replace the cdr of CONS with OBJECT."
  (setf (cdr cons) object)
  cons)


;; symbol
(defstruct (symbol (:constructor make-symbol (name)) (:predicate symbolp) (:copier nil))
  (name "" (:type string))
  (%value 'unbound)
  (function nil)
  (package nil)
  (plist nil (:type list))
  )

(defconstant unbound
  'nil
  "Represent variable's unbound state as a symbol itself.")

(defconstant nil
  'nil
  "Represent both boolean (and generalized boolean) false and the empty list.")

(defconstant t
  't
  "The boolean representing true, and the canonical generalized boolean representing true.")

(defun boundp (symbol)
  (not (eq (symbol-%value symbol) 'unbound)))

(defun makunbound (symbol)
  (setf (symbol-%value symbol) 'unbound))

(defun symbol-value (symbol)
  (if (boundp symbol)
      (symbol-%value symbol)
      (error 'unbound-variable :name symbol)))

(defsetf symbol-value (symbol) (new-value)
  (setf (symbol-%value symbol) new-value))

(defun set (symbol value)
  (setf (symbol-value symbol) value))

(defvar *gensym-counter* 0)
(defun gensym (&optional (x "G"))
  (gensym x))


;; function
(defstruct (function (:predicate functionp))
  (lambda-expression)
  )
;; functionp,,, are defined here

(defun fdefinition (function-name)
  (etypecase function-name
    (symbol (symbol-function function-name))
    (setf-function-name )))

(defsetf fdefinition (function-name) (new-function)
  )

(defun fboundp (name)
  )

(defun fmakunbound (name)
  )

(defun function-lambda-expression (function)
  )

(defun compiled-function-p (object)
  )

(defmacro function (name)
  )



;;; special operators
(defmacro quote (object)
  )

(defmacro eval-when (situations &body body)
  )

(defmacro if (test-form then-form &optional else-form)
  )

(defmacro load-time-value (form &optional read-only-p)
  )

(defmacro locally (&rest declarations-and-forms)
  )

(defmacro symbol-macrolet ((symbol-expansions) &body body)
  )

(defmacro the (value-type form)
  )


;; data and control flow
(defun apply (function arg &rest more-args)
  )

(defmacro defun (function-name lambda-list &body body)
  )

(defmacro flet (functions &body body)
  )

(defmacro labels (functions &body body)
  )

(defmacro macrolet (macros &body body)
  )

(defun funcall (function &rest args)
  (when (and (symbolp function)
	     (or (not (fboundp function))
		 (do ((x '(block catch eval-when flet function go if labels let
		           let* load-time-value locally macrolet
			   multiple-value-call multiple-value-prog1 progn progv
			   quote return-from setq symbol-macrolet tagbody the
			   throw unwind-protect)
			 (cdr x)))
		     ((null x) nil)
		   (when (eq function (car x))
		     (return t)))
		 (macro-function function)))
    (error 'undefined-function :name function))
  (apply function args))

(defconstant call-arguments-limit
  50
  "An integer not smaller than 50 and at least as great as the value of lambda-parameters-limit, the exact magnitude of which is implementation-dependent.")

(defconstant lambda-parameters-limit
  50
  "A positive integer that is the upper exclusive bound on the number of parameter names that can appear in a single lambda list.")

(defconstant lambda-list-keywords
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)
  "a list, the elements of which are implementation-dependent, but which must contain at least the symbols &allow-other-keys, &aux, &body, &environment, &key, &optional, &rest, and &whole.")

(defmacro defparameter (name initial-value 
			&optional (documentation nil documentation-p))
  `(progn (declaim (special ,name))
    (setf (symbol-value ',name) ,initial-value)
    ,(when documentation-p
	   `(setf (documentation ',name 'variable) ',documentation))
    ',name))

(defmacro defvar (name &optional
		  (initial-value nil initial-value-p)
		  (documentation nil documentation-p))
  `(progn (declaim (special ,name))
    ,(when initial-value-p
	   `(unless (boundp ',name)
	     (setf (symbol-value ',name) ,initial-value)))
    ,(when documentation-p
	   `(setf (documentation ',name 'variable) ',documentation))
    ',name))


(defmacro defconstant (name initial-value &optional documentation)
  )

(defmacro destructuring-bind (lambda-list expression &body body)
  )

(defmacro let (vars &body body)
  )

(defmacro let* (vars &body body)
  )

(defmacro progv (symbols values &body body)
  )

(defmacro setq (&rest pairs)
  )

(defmacro block (name &body body)
  )

(defmacro catch (tag &body body)
  )

(defmacro go (tag)
  )

(defmacro return-from (name &optional result)
  )

(defmacro tagbody (&body body)
  )

(defmacro throw (tag result-form)
  )

(defmacro unwind-protect (protected-form &rest cleanup-forms)
  )

(defun eq (x y)
  )

(defun eql (x y)
  (or (eq x y)
      (and (numberp x) (numberp y) (= x y) (eq (type-of x) (type-of y)))
      (and (characterp x) (characterp y) (char= x y))))

(defun equalp (x y)
  (cond
   ((eq x y) t)
   ((characterp x) (and (characterp y) (char-equal x y)))
   ((numberp x) (and (numberp y) (= x y)))
   ((consp x) (and (consp y) (equalp (car x) (car y)) (equalp (cdr x) (cdr y))))
   ((arrayp x) (and (arrayp y)
		    (equal (array-dimensions x) (array-dimensions y))
		    (dotimes (i (array-total-size x) t)
		      (unless (equalp (row-major-aref x i) (row-major-aref y i))
			(return nil)))))
   ((hash-table-p x) (and (hash-table-p y)
			  (= (hash-table-count x) (hash-table-count y))
			  (eq (hash-table-test x) (hash-table-test y))
			  (with-hash-table-iterator (get x)
			    (loop
			     (multiple-value-bind (entry-returned key x-value)
				 (get)
			       (unless entry-returned
				 (return t))
			       (multiple-value-bind (y-value present-p)
				   (gethash key y)
				 (unless (and present-p (equalp x-value y-value))
				   (return nil))))))))
   ((typep x 'structure-object) (and (typep x 'structure-object)
				     (eq (class-of x) (class-of y))
				     ))
   (t nil)))


(defun values (&rest object)
  )

(defmacro multiple-value-call (function-form &body body)
  )

(defmacro multiple-value-prog1 (first-form &rest forms)
  )

(defconstant multiple-values-limit most-positive-fixnum "")

(defmacro progn (&rest forms)
  )

(defmacro define-modify-macro (name lambda-list function &optional documentation)
  )

(defmacro defsetf (access-fn &rest rest)
  )

(defmacro define-setf-expander (access-fn lambda-list &body body)
  )

(defun get-setf-expansion (place &optional environment)
  )


;; eval
(defun compile (name &optional definition)
  (compile name definition))

(defun eval (form)
  )

(defun compiler-macro-function (name &optional environment)
  )

(defsetf compiler-macro-function (name &optional environment) (new-function)
  )

(defmacro define-compiler-macro (name lambda-list &body body)
  )

(defmacro defmacro (name lambda-list &body body)
  )

(defun macro-function (symbol &optional environment)
  )

(defsetf macro-function (symbol &optional environment) (new-function)
  )

(defun macroexpand-1 (form &optional env)
  )

(defmacro define-symbol-macro (symbol expansion)
  )

(defvar *macroexpand-hook* #'funcall
  "")

(defun proclaim (declaration-specifier)
  )

(defmacro declaim (&rest declaration-specifiers)
  )

(defun constantp (form &optional environment)
  )


;; array
(defun arrayp (object)
  (arrayp object))

(defun make-array (dimensions &key (element-type t)
			      initial-element initial-contents adjustable
			      fill-pointer displaced-to displaced-index-offset)
  )

(defun adjust-array (array new-dimensions &key
			   (element-type (array-element-type array))
			   initial-element initial-contents
			   fill-pointer displaced-to displaced-index-offset)
  )


(defun adjustable-array-p (array)
  (adjustable-array-p array))

(defun array-dimensions (array)
  (array-dimensions array))

(defun array-element-type (array)
  (array-element-type array))

(defun array-has-fill-pointer-p (array)
  (array-has-fill-pointer-p array))

(defun array-displacement (array)
  (array-displacement array))

(defun fill-pointer (vector)
  (fill-pointer vector))

(defsetf fill-pointer (vector) (value)
  `(setf (fill-pointer ,vector) ,value))

(defun row-major-aref (array index)
  (row-major-aref array index))

(defsetf row-major-aref (array index) (value)
  `(setf (row-major-aref ,array ,index) ,value))

(defun upgraded-array-element-type (typespec &optional environment)
  (upgraded-array-element-type typespec environment))

(defconst array-dimension-limit 1024
  "")

(defconst array-rank-limit 8
  "")

(defconst array-total-size-limit 1024
  "")

(defun simple-vector-p (object)
  ""
  (simple-vector-p object))

(defun svref (simple-vector index)
  ""
  (svref simple-vector index))

(defsetf svref (simple-vector index) (value)
  `(setf (svref ,simple-vector ,index) ,value))

(defun bit (bit-array &rest subscripts)
  (apply #'bit bit-array subscripts))

(defsetf bit (bit-array &rest subscripts) (value)
  `(setf (apply #'bit ,bit-array ,subscripts) ,value))

(defun sbit (bit-array &rest subscripts)
  (apply #'sbit bit-array subscripts))

(defsetf sbit (bit-array &rest subscripts) (value)
  `(setf (apply #'sbit ,bit-array ,subscripts) ,value))


(defun bit-and (bit-array1 bit-array2 &optional opt-arg)
  (bit-and bit-array1 bit-array2 opt-arg))

(defun bit-ior (bit-array1 bit-array2 &optional opt-arg)
  (bit-ior bit-array1 bit-array2 opt-arg))

(defun bit-xor (bit-array1 bit-array2 &optional opt-arg)
  (bit-xor bit-array1 bit-array2 opt-arg))

(defun bit-not (bit-array &optional opt-arg)
  (bit-not bit-array opt-arg))


;; string
(defun char (string index)
  (char string index))

(defsetf char (string index) (value)
  `(setf (char ,string ,index) ,value))

(defun schar (string index)
  (schar string index))

(defsetf schar (string index) (value)
  `(setf (schar ,string ,index) ,value))


;; character
(defconst char-code-limit 256
  "")

(defun char= (character &rest more-characters)
  (apply #'char= character more-characters))

(defun char< (character &rest more-characters)
  (apply #'char< character more-characters))

(defun characterp (object)
  (characterp object))

(defun alpha-char-p (character)
  (alpha-char-p character))

(defun alphanumericp (character)
  (alphanumericp character))

(defun graphic-char-p (character)
  (graphic-char-p character))

(defun char-upcase (character)
  (char-upcase character))

(defun char-downcase (character)
  (char-downcase character))

(defun upper-case-p (character)
  (upper-case-p character))

(defun lower-case-p (character)
  (lower-case-p character))

(defun both-case-p (character)
  (both-case-p character))

(defun char-code (character)
  (char-code character))

(defun char-int (character)
  (char-int character))

(defun char-name (character)
  (char-name character))

(defun name-char (name)
  (name-char name))


;; sequence
(defun make-sequence (result-type size &key initial-element)
  "Return a sequence of the type RESULT-TYPE and of length SIZE."
  )


;; hash-table

;; (defun hash-table-p (object)
;;   )
;; 
;; (defun make-hash-table (&key test size rehash-size rehash-threshold)
;;   )

;; (defun hash-table-count (hash-table)
;;   )
;; 
;; (defun hash-table-size (hash-table)
;;   )
;; 
;; (defun hash-table-rehash-size (hash-table)
;;   )
;; 
;; (defun hash-table-rehash-threshold (hash-table)
;;   )
;; 
;; (defun hash-table-test (hash-table)
;;   )

;; (defun gethash (key hash-table &optional default)
;;   )
;; 
;; (defsetf gethash (key hash-table &optional default) (value)
;;   `(setf (gethash ,key ,hash-table ,default) ,value))

;; (defun remhash (key hash-table)
;;   )

;; (defmacro with-hash-table-iterator ((name hash-table) &body body)
;;   )

;; (defun clrhash (hash-table)
;;   )

(defun sxhash (object)
  (rem (equal-hash) most-positive-fixnum))


;; stream
(defun streamp (object)
  )

(defun input-stream-p (stream)
  )

(defun output-stream-p (stream)
  )

(defun interactive-stream-p (stream)
  )

(defun open-stream-p (stream)
  )

(defun stream-element-type (stream)
  )

(defun read-byte (stream &optional eof-error-p eof-value)
  )

(defun write-byte (byte stream)
  )

(defun peek-char (&optional peek-type input-stream eof-error-p eof-value
			    recursive-p)
  )

(defun read-char (&optional input-stream eof-error-p eof-value recursive-p)
  )

(defun read-char-no-hang (&optional input-stream eof-error-p eof-value
				    recursive-p)
  )

(defun unread-char (character &optional input-stream)
  )

(defun write-char (character &optional output-stream)
  )

(defun fresh-line (&optional output-stream)
  )

(defun file-length (stream)
  )

(defun file-position (stream &optional position)
  )

(defun file-string-length (stream object)
  )

(defun open (filespec &key direction element-type
		      if-exists if-does-not-exist external-format)
  )

(defun stream-external-format (stream)
  )

(defun close (stream &key abort)
  )

(defun listen (&optional input-stream)
  )

(defun clear-input (&optional input-stream)
  )

(defun finish-output (&optional output-stream)
  )

(defun force-output (&optional output-stream)
  )

(defun clear-output (&optional output-stream)
  )

(defun y-or-n-p (&optional control &rest arguments)
  )

(defun yes-or-no-p (&optional control &rest arguments)
  )

(defun make-synonym-stream (symbol)
  )

(defun synonym-stream-symbol (synonym-stream)
  )

(defun make-broadcast-stream (&rest streams)
  )

(defun broadcast-stream-streams (broadcast-stream)
  )

(defun make-two-way-stream (input-stream output-stream)
  )

(defun two-way-stream-input-stream (two-way-stream)
  )

(defun two-way-stream-output-stream (two-way-stream)
  )

(defun make-echo-stream (input-stream output-stream)
  )

(defun echo-stream-input-stream (echo-stream)
  )

(defun echo-stream-output-stream (echo-stream)
  )

(defun make-concatenated-stream (&rest input-streams)
  )

(defun concatenated-stream-streams (concatenated-stream)
  )


(defun make-string-input-stream (string &optional start end)
  )

(defun make-string-output-stream (&key element-type)
  )

(defun get-output-stream-string (string-output-stream)
  )


(defun stream-error-stream (condition)
  )

(defvar *DEBUG-IO*)
(defvar *ERROR-OUTPUT*)
(defvar *QUERY-IO*)
(defvar *STANDARD-INPUT*)
(defvar *STANDARD-OUTPUT*)
(defvar *TRACE-OUTPUT*)
(defvar *TERMINAL-IO*)

(defmacro with-input-from-string ((var string &key index (start 0) end)
				  &body body)
  (multiple-value-bind (decls forms) (declarations-and-forms body)
    `(let ((,var (make-string-input-stream ,string ,start ,end)))
       ,@decls
       (unwind-protect
	   (progn ,@forms)
	 (close ,var)
	 ,@(when index
	     `((setf ,index (string-input-stream-current-position ,var))))))))


(defmacro with-output-to-string ((var &optional string-form &key element-type)
				 &body body))


;;; package
