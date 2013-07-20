;;; -*- Package: ALIEN -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: src/code/alien-callback.lisp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains any the part of the Alien implementation that
;;; is not part of the compiler.
;;;
(in-package "ALIEN")
(use-package "EXT")
(use-package "SYSTEM")

(intl:textdomain "cmucl")

(export '(alien * array struct union enum function integer signed unsigned
	  boolean values single-float double-float long-float
	  system-area-pointer def-alien-type def-alien-variable sap-alien
	  extern-alien with-alien slot deref addr cast alien-sap alien-size
	  alien-funcall def-alien-routine make-alien free-alien
	  null-alien
	  def-callback callback
	  callback-funcall))

(in-package "ALIEN-INTERNALS")
(in-package "ALIEN")

(import '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions	  
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")

(export '(alien alien-value alien-value-type parse-alien-type
	  unparse-alien-type alien-type-= alien-subtype-p alien-typep

	  def-alien-type-class def-alien-type-translator def-alien-type-method
	  invoke-alien-type-method

	  alien-type alien-type-p alien-type-bits alien-type-alignment
	  alien-integer-type alien-integer-type-p alien-integer-type-signed
	  alien-boolean-type alien-boolean-type-p
	  alien-enum-type alien-enum-type-p
	  alien-float-type alien-float-type-p
	  alien-single-float-type alien-single-float-type-p
	  alien-double-float-type alien-double-float-type-p
	  alien-long-float-type alien-long-float-type-p
	  alien-pointer-type alien-pointer-type-p alien-pointer-type-to
	  make-alien-pointer-type
	  alien-array-type alien-array-type-p alien-array-type-element-type
	  alien-array-type-dimensions	  
	  alien-record-type alien-record-type-p alien-record-type-fields
	  alien-record-field alien-record-field-p alien-record-field-name
	  alien-record-field-type alien-record-field-offset
	  alien-function-type alien-function-type-p make-alien-function-type
	  alien-function-type-result-type alien-function-type-arg-types
	  alien-values-type alien-values-type-p alien-values-type-values
	  *values-type-okay*

	  %set-slot %slot-addr %set-deref %deref-addr

	  %heap-alien %set-heap-alien %heap-alien-addr
	  heap-alien-info heap-alien-info-p heap-alien-info-type
	  heap-alien-info-sap-form

	  local-alien %set-local-alien %local-alien-addr
	  local-alien-info local-alien-info-p local-alien-info-type
	  local-alien-info-force-to-memory-p
	  %local-alien-forced-to-memory-p
	  make-local-alien dispose-local-alien note-local-alien-type

	  %cast %sap-alien align-offset

	  extract-alien-value deposit-alien-value naturalize deport
	  compute-lisp-rep-type compute-alien-rep-type
	  compute-extract-lambda compute-deposit-lambda
	  compute-naturalize-lambda compute-deport-lambda)
	"ALIEN-INTERNALS")


;;;; Alien callback support
;;;;
;;;; This is basically the implementation posted by Helmut Eller,
;;;; posted to cmucl-imp on 04/13/2003.  It has been modified to live
;;;; in the ALIEN package and to fit the same style as the ALIEN
;;;; package.

;;; This package provides a mechanism for defining callbacks: lisp
;;; functions which can be called from foreign code.  The user
;;; interface consists of the macros DEFCALLBACK and CALLBACK.  (See
;;; the doc-strings for details.)
;;;
;;; Below are two examples.  The first example defines a callback FOO
;;; and calls it with alien-funcall.  The second illustrates the use
;;; of the libc qsort function.
;;;
;;; The implementation generates a piece machine code -- a
;;; "trampoline" -- for each callback function.  A pointer to this
;;; trampoline can then be passed to foreign code.  The trampoline is
;;; allocated with malloc and is not moved by the GC.
;;;
;;; When called, the trampoline passes a pointer to the arguments
;;; (essentially the stack pointer) together with an index to
;;; CALL-CALLBACK.  CALL-CALLBACK uses the index to find the
;;; corresponding lisp function and calls this function with the
;;; argument pointer.  The lisp function uses the pointer to copy the
;;; arguments from the stack to local variables.  On return, the lisp
;;; function stores the result into the location given by the argument
;;; pointer, and the trampoline code copies the return value from
;;; there into the right return register.
;;;
;;; The address of CALL-CALLBACK is used in every trampoline and must
;;; not be moved by the gc.  It is therefore necessary to either
;;; include this package into the image (core) or to purify before
;;; creating any trampolines (or to invent some other trick).
;;;
;;; Examples: 

#||
;;; Example 1:

(alien:def-callback foo (c-call:int (arg1 c-call:int) (arg2 c-call:int))
  (format t "~&foo: ~S, ~S~%" arg1 arg2)
  (+ arg1 arg2))

(alien:alien-funcall (alien:sap-alien (alien:callback foo)
				      (function c-call:int c-call:int c-call:int))
	       555 444444)

;;; Example 2:

(alien:def-alien-routine qsort c-call:void
  (base (* t))
  (nmemb c-call:int)
  (size c-call:int)
  (compar (* (function c-call:int (* t) (* t)))))

(alien:def-callback my< (c-call:int (arg1 (* c-call:double))
		      (arg2 (* c-call:double)))
  (let ((a1 (alien:deref arg1))
	(a2 (alien:deref arg2)))
    (cond ((= a1 a2)  0)
	  ((< a1 a2) -1)
	  (t         +1))))

(let ((a (make-array 10 :element-type 'double-float
		     :initial-contents '(0.1d0 0.5d0 0.2d0 1.2d0 1.5d0
					 2.5d0 0.0d0 0.1d0 0.2d0 0.3d0))))
  (print a)
  (qsort (sys:vector-sap a)
	 (length a)
	 (alien:alien-size c-call:double :bytes)
	 (alien:callback my<))
  (print a))

||#

(defstruct (callback
	     (:constructor make-callback (trampoline lisp-fn function-type)))
  "A callback consists of a piece assembly code -- the trampoline --
and a lisp function.  We store the function type (including return
type and arg types), so we can detect incompatible redefinitions."
  (trampoline (required-argument) :type system-area-pointer)
  (lisp-fn (required-argument) :type (function (fixnum fixnum) (values)))
  (function-type (required-argument) :type alien::alien-function-type))

(declaim (type (vector callback) *callbacks*))
(defvar *callbacks* (make-array 10 :element-type 'callback
				:fill-pointer 0 :adjustable t)
  "Vector of all callbacks.")

(defun call-callback (index sp-fixnum ret-addr)
  (declare (type fixnum index sp-fixnum ret-addr)
	   (optimize speed))
  (funcall (callback-lisp-fn (aref *callbacks* index))
	   sp-fixnum ret-addr))

(defun create-callback (lisp-fn fn-type)
  (let* ((index (fill-pointer *callbacks*))
	 (tramp (vm:make-callback-trampoline index fn-type))
	 (cb (make-callback tramp lisp-fn fn-type)))
    (vector-push-extend cb *callbacks*)
    cb))

(defun address-of-call-callback ()
  (kernel:get-lisp-obj-address #'call-callback))

(defun address-of-funcall3 ()
  (sys:sap-int (alien-sap (extern-alien "funcall3" (function (* t))))))

;;; Some abbreviations for alien-type classes.  The $ suffix is there
;;; to prevent name clashes.

(deftype void$ () '(satisfies alien-void-type-p))
(deftype integer$ () 'alien-integer-type)
(deftype integer-64$ () '(satisfies alien-integer-64-type-p))
(deftype signed-integer$ () '(satisfies alien-signed-integer-type-p))
(deftype pointer$ () 'alien-pointer-type)
(deftype single$ () 'alien-single-float-type)
(deftype double$ () 'alien-double-float-type)
(deftype sap$ () '(satisfies alien-sap-type=))

(defun alien-sap-type= (type)
  (alien-type-= type (parse-alien-type 'system-area-pointer)))

(defun alien-void-type-p (type)
  (and (alien-values-type-p type)
       (null (alien-values-type-values type))))

(defun alien-integer-64-type-p (type)
  (and (alien-integer-type-p type)
       (= (alien-type-bits type) 64)))

(defun alien-signed-integer-type-p (type)
  (and (alien-integer-type-p type)
       (alien-integer-type-signed type)))

(defun segment-to-trampoline (segment length)
  (let* ((code (alien-funcall 
		(extern-alien "malloc" (function system-area-pointer unsigned))
		length))
	 (fill-pointer code))
    ;; Make sure the malloc'ed area is executable.  
    (let* ((page-size (get-page-size))
	   ;; mprotect wants address on a page boundary, so round down
	   ;; the address and round up the length
	   (code-base (sys:int-sap (* page-size
				      (floor (sys:sap-int code) page-size))))
	   (len (* page-size (ceiling length page-size))))
      (unless (unix::unix-mprotect code-base len
				   (logior unix:prot_exec unix:prot_read unix:prot_write))
	(warn (intl:gettext "Unable to mprotect ~S bytes (~S) at ~S (~S).  Callbacks may not work.")
	      len length code-base code)))
    (new-assem:segment-map-output segment
      (lambda (sap length)
	(kernel:system-area-copy sap 0 fill-pointer 0
				 (* length vm:byte-bits))
	(setf fill-pointer (sys:sap+ fill-pointer length))))
    code))

(defun symbol-trampoline (symbol)
  (callback-trampoline (symbol-value symbol)))

(defmacro callback (name)
  "Return the trampoline pointer for the callback NAME."
  `(symbol-trampoline ',name))

;; Convenience macro to make it easy to call callbacks.
(defmacro callback-funcall (name &rest args)
  `(alien-funcall (sap-alien (callback ,name)
			     ,(unparse-alien-type
			       (callback-function-type (symbol-value name))))
		  ,@args))

(defun define-callback-function (name lisp-fn fn-type)
  (declare (type symbol name)
	   (type function lisp-fn))
  (flet ((register-new-callback () 
	   (setf (symbol-value name)
		 (create-callback lisp-fn fn-type))))
    (if (and (boundp name)
	     (callback-p (symbol-value name)))
	;; try do redefine the existing callback
	(let ((callback (find (symbol-trampoline name) *callbacks*
			      :key #'callback-trampoline :test #'sys:sap=)))
	  (cond (callback
		 (let ((old-type (callback-function-type callback)))
		   (cond ((vm::compatible-function-types-p old-type fn-type)
			  ;; (format t "~&; Redefining callback ~A~%" name)
			  (setf (callback-lisp-fn callback) lisp-fn)
			  (setf (callback-function-type callback) fn-type)
			  callback)
			 (t
			  (let ((e (format nil (intl:gettext "~
Attempt to redefine callback with incompatible return type.
   Old type was: ~A 
    New type is: ~A") old-type fn-type))
				(c (format nil (intl:gettext "~
Create new trampoline (old trampoline calls old lisp function)."))))
			    (cerror c e)
			    (register-new-callback))))))
		(t (register-new-callback))))
	(register-new-callback))))

(defun word-aligned-bits (type)
  (align-offset (alien-type-bits type) vm:word-bits))

(defun argument-size (spec)
  (let ((type (parse-alien-type spec)))
    (typecase type
      ((or integer$ single$ double$ pointer$ sap$)
       (ceiling (word-aligned-bits type) vm:byte-bits))
      (t (error (intl:gettext "Unsupported argument type: ~A") spec)))))

(defun parse-return-type (spec)
  (let ((*values-type-okay* t))
    (parse-alien-type spec)))

(defun parse-function-type (return-type arg-specs)
  (parse-alien-type
   `(function ,return-type ,@(mapcar #'second arg-specs))))

(defun return-exp (spec sap body)
  (flet ((store (spec) `(setf (deref (sap-alien ,sap (* ,spec))) ,body)))
    (let ((type (parse-return-type spec)))
      (typecase type
	(void$ body)
	(signed-integer$ 
	 (store `(signed ,(word-aligned-bits type))))
	(integer$
	 (store `(unsigned ,(word-aligned-bits type))))
	((or single$ double$ pointer$ sap$)
	 (store spec))
	(t (error (intl:gettext "Unsupported return type: ~A") spec))))))

(defmacro def-callback (name (return-type &rest arg-specs) &parse-body (body decls doc))
  "(defcallback NAME (RETURN-TYPE {(ARG-NAME ARG-TYPE)}*)
     {doc-string} {decls}* {FORM}*)

Define a function which can be called by foreign code.  The pointer
returned by (callback NAME), when called by foreign code, invokes the
lisp function.  The lisp function expects alien arguments of the
specified ARG-TYPEs and returns an alien of type RETURN-TYPE.

If (callback NAME) is already a callback function pointer, its value
is not changed (though it's arranged that an updated version of the
lisp callback function will be called).  This feature allows for
incremental redefinition of callback functions."
  (let ((sp-fixnum (gensym (string :sp-fixnum-)))
	(ret-addr (gensym (string :ret-addr-)))
	(sp (gensym (string :sp-)))
	(ret (gensym (string :ret-))))
    `(progn
      (defun ,name (,sp-fixnum ,ret-addr)
	,@(when doc (list doc))
	(declare (type fixnum ,sp-fixnum ,ret-addr))
	,@decls
	;; We assume sp-fixnum is word aligned and pass it untagged to
	;; this function.  The shift compensates this.
 	(let ((,sp (sys:int-sap (bignum:%ashl (ldb (byte vm:word-bits 0) ,sp-fixnum)
 					      2)))
 	      (,ret (sys:int-sap (bignum:%ashl (ldb (byte vm:word-bits 0) ,ret-addr)
 					       2))))
	  (declare (ignorable ,sp ,ret))
	  ;; Copy all arguments to local variables.
	  (with-alien ,(loop for offset = 0 then (+ offset 
						    (argument-size type))
			     for (name type) in arg-specs
			     collect `(,name ,type
				       :local ,(vm:callback-accessor-form type sp offset)))
	    ,(return-exp return-type ret `(progn ,@body))
	    (values))))
      (define-callback-function 
	  ',name #',name ',(parse-function-type return-type arg-specs)))))

;;; dumping support

(defun restore-callbacks ()
  ;; Create new trampolines on reload.
  (loop for cb across *callbacks*
	for i from 0
	do (setf (callback-trampoline cb)
		 (vm:make-callback-trampoline i (callback-function-type cb)))))

;; *after-save-initializations* contains
;; new-assem::forget-output-blocks, and the assembler may not work
;; before forget-output-blocks was called.  We add 'restore-callback at
;; the end of *after-save-initializations* to sidestep this problem.
(setf *after-save-initializations*
      (append *after-save-initializations* (list 'restore-callbacks)))

;;; callback.lisp ends here
