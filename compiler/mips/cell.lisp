;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/mips/cell.lisp,v 1.3 1990/02/03 16:11:35 wlott Exp $
;;;
;;;    This file contains the VM definition of various primitive memory access
;;; VOPs for the MIPS.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 
(in-package "C")


;;;; Data object definition macros.

(eval-when (compile eval load)
  (defun parse-slot (slot)
    (if (atom slot)
	(values slot nil nil nil nil nil)
	(values (car slot)
		(getf (cdr slot) :ref-vop)
		(getf (cdr slot) :ref-trans)
		(getf (cdr slot) :set-vop)
		(getf (cdr slot) :set-trans)
		(getf (cdr slot) :docs)))))

(defmacro defslots ((name &key (header t) (lowtag 0)) &rest slots)
  (let ((compile-time nil)
	(load-time nil)
	(index (if header 1 0))
	(rest nil))
    (dolist (slot slots)
      (if (eq slot '&rest)
	  (setf rest t)
	  (multiple-value-bind
	      (slot-name ref-vop ref-trans set-vop set-trans docs)
	      (parse-slot slot)
	    (let ((const (intern (concatenate 'simple-string
					      (string name)
					      "-"
					      (string slot-name)
					      (if rest "-OFFSET" "-SLOT")))))
	      (push `(defconstant ,const
		       ,index
		       ,@(if docs (list docs)))
		    compile-time)
	      (when (or set-vop set-trans ref-vop ref-trans)
		(push `(define-cell-accessors ,const ,lowtag
			 ,ref-vop ,ref-trans ,set-vop ,set-trans)
		      load-time))
	      (if rest
		  (setf rest nil)
		  (incf index))))))
    `(progn
       (eval-when (compile load eval)
	 ,@(nreverse compile-time))
       ,@(nreverse load-time))))


(defslots (%cons :lowtag list-pointer-type :header nil)
  (car :ref-vop car :ref-trans car
       :set-vop set-car :set-trans %rplaca)
  (cdr :ref-vop cdr :ref-trans cdr
       :set-vop set-cdr :set-trans %rplacd))


(defslots (%symbol :lowtag other-pointer-type)
  (value :set-vop set :set-trans set)
  (function :set-vop set-symbol-function :set-trans %sp-set-definition)
  (plist :ref-vop symbol-plist :ref-trans symbol-plist
	 :set-vop set-symbol-plist :set-trans %sp-set-plist)
  (name :ref-vop symbol-name :ref-trans symbol-name)
  (package :ref-vop symbol-package :ref-trans symbol-package
	   :set-vop set-package))


(defslots (%array)
  fill-pointer
  elements
  data
  displacement
  displaced-p
  &rest
  dimensions)

(defslots (%code)
  code-size
  entry-points
  debug-info
  &rest
  constants)
  
(defslots (%function-header)
  self
  next
  name
  arglist
  type
  &rest
  code)

(defslots (%closure)
  function
  &rest
  info)




;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:node-var node)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp %symbol-value-slot)
    #+nil
    (let ((err-lab (generate-error-code node clc::error-symbol-unbound
					obj-temp)))
      (test-special-value value temp '%trap-object err-lab nil))))

;;; With Symbol-Function, we check that the result is a function, so NIL is
;;; always un-fbound.
;;;
(define-vop (symbol-function checked-cell-ref)
  (:translate symbol-function)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp %symbol-function-slot)
    #+nil
    (let ((err-lab (generate-error-code node clc::error-symbol-undefined
					obj-temp)))
      (test-simple-type value temp err-lab t system:%function-type))))


;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:type random  :scs (non-descriptor-reg)) temp))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  #+nil
  (:generator 9
    (loadw value object (/ clc::symbol-value 4))
    (test-special-value value temp '%trap-object target (not not-p))))


;;; SYMBOL isn't a primitive type, so we can't use it for the arg restriction
;;; on the symbol case of fboundp.  Instead, we transform to a funny function.

(defknown fboundp/symbol (t) boolean (flushable))
;;;
(deftransform fboundp ((x) (symbol))
  '(fboundp/symbol x))
;;;
(define-vop (fboundp/symbol boundp-frob)
  (:translate fboundp/symbol)
  #+nil
  (:generator 10
    (loadw value object (/ clc::symbol-definition 4))
    (test-simple-type value temp target not-p system:%function-type)))

#+nil
(def-source-transform makunbound (x)
  `(set ,x (%primitive make-immediate-type 0 system:%trap-type)))


(define-vop (fast-symbol-value cell-ref)
  (:variant %symbol-value-slot other-pointer-type)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (fast-symbol-function cell-ref)
  (:variant %symbol-function-slot other-pointer-type)
  (:policy :fast)
  (:translate symbol-function))


#+nil
(define-miscop bind (val symbol) :results ())

#+nil
(define-miscop unbind (num) :results ())


;;;; List hackery:

#+nil
(define-miscop cons (x y) :translate cons)


;;;; Value cell and closure hackery:

#+nil
(define-miscop make-value-cell (val))
#+nil
(define-miscop make-closure (nvars entry))

#+nil
(define-vop (value-cell-ref cell-ref)
  (:variant (+ clc::g-vector-header-size-in-words
	       system:%function-value-cell-value-slot)))

#+nil
(define-vop (value-cell-set cell-set)
  (:variant (+ clc::g-vector-header-size-in-words
	       system:%function-value-cell-value-slot)))

#+nil
(define-vop (closure-init slot-set))
#+nil
(define-vop (closure-ref slot-ref))


;;;; Structure hackery:

#+nil
(define-vop (structure-ref slot-ref)
  (:variant vector-header-length other-pointer-type))

#+nil
(define-vop (structure-set slot-set)
  (:variant vector-header-length other-pointer-type))
