;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/x86/alloc.lisp,v 1.2 1997/02/08 21:15:38 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the x86 port.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; 

(in-package :x86)


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:sc dword-reg) ptr temp)
  (:temporary (:sc dword-reg :to (:result 0) :target result) res)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond ((zerop num)
	   ;; (move result nil-value)
	   (inst mov result nil-value))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((store-car (tn list &optional (slot vm:cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    ((descriptor-stack immediate-stack)
			     (move temp ,tn)
			     temp))))
		     (storew reg ,list ,slot vm:list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       #-cgc
	       (with-allocation (temp)
		 (inst lea res
		       (make-ea :byte :base temp :disp list-pointer-type))
		 (inst add temp (* (pad-data-block cons-size) cons-cells)))
	       #+cgc
	       (with-cgc-allocation (temp (* (pad-data-block cons-size)
					     cons-cells))
		 (inst lea res
		       (make-ea :byte :base temp :disp list-pointer-type)))
	       (move ptr res)
	       (dotimes (i (1- cons-cells))
		 (store-car (tn-ref-tn things) ptr)
		 (setf things (tn-ref-across things))
		 (inst add ptr (pad-data-block cons-size))
		 (storew ptr ptr (- cons-cdr-slot cons-size)
			 list-pointer-type))
	       (store-car (tn-ref-tn things) ptr)
	       (cond (star
		      (setf things (tn-ref-across things))
		      (store-car (tn-ref-tn things) ptr cons-cdr-slot))
		     (t
		      (storew nil-value ptr cons-cdr-slot
			      list-pointer-type)))
	       (assert (null (tn-ref-across things)))))
	   (move result res)))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg) :target boxed)
	 (unboxed-arg :scs (any-reg) :target unboxed))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc dword-reg :from :eval) temp)
  (:temporary (:sc dword-reg :from (:argument 0)) boxed)
  (:temporary (:sc dword-reg :from (:argument 1)) unboxed)
  (:generator 100
    (move boxed boxed-arg)
    (inst add boxed (fixnum (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (move unboxed unboxed-arg)
    (inst shr unboxed word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    #+nil ;; keep this here in case we load code in dynamic again
    (with-allocation (temp)
      (inst lea result (make-ea :byte :base temp :disp other-pointer-type))
      (inst add temp boxed)
      (inst add temp unboxed))
    (progn ;; now loading code into static space cause it can't move
      (load-symbol-value temp lisp::*static-space-free-pointer*)
      (inst lea result (make-ea :byte :base temp :disp other-pointer-type))
      (inst add temp boxed)
      (inst add temp unboxed)
      (store-symbol-value temp lisp::*static-space-free-pointer*))
    (inst shl boxed (- type-bits word-shift))
    (inst or boxed code-header-type)
    (storew boxed result 0 other-pointer-type)
    (storew unboxed result code-code-size-slot other-pointer-type)
    ;; (move temp nil-value)
    (inst mov temp nil-value)
    (storew temp result code-entry-points-slot other-pointer-type)
    (storew temp result code-debug-info-slot other-pointer-type)))

;;; Top-Level-Forms have a short lifetime as they are unreachable
;;; once the loader eval's them. No point loading them in static
;;; space. This justs gives the loader a way to grab a chunk
;;; of the dynamic heap.
(define-vop (allocate-dynamic-code-object)
  (:args (boxed-arg :scs (any-reg) :target boxed)
	 (unboxed-arg :scs (any-reg) :target unboxed))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc dword-reg :from :eval) temp)
  (:temporary (:sc dword-reg :from (:argument 0)) boxed)
  (:temporary (:sc dword-reg :from (:argument 1)) unboxed)
  (:generator 100
    (move boxed boxed-arg)
    (inst add boxed (fixnum (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (move unboxed unboxed-arg)
    (inst shr unboxed word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    #-cgc
    (with-allocation (temp)
      (inst lea result (make-ea :byte :base temp :disp other-pointer-type))
      (inst add temp boxed)
      (inst add temp unboxed))
    #+cgc
    (progn 
      (move temp boxed)
      (inst add temp unboxed)
      (with-cgc-allocation (temp temp)
	(inst lea result (make-ea :byte :base temp :disp other-pointer-type))))
    (inst shl boxed (- type-bits word-shift))
    (inst or boxed code-header-type)
    (storew boxed result 0 other-pointer-type)
    (storew unboxed result code-code-size-slot other-pointer-type)
    (inst mov temp nil-value)
    (storew temp result code-entry-points-slot other-pointer-type)
    (storew temp result code-debug-info-slot other-pointer-type)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc dword-reg) alloc)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result alloc fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew nil-value result fdefn-function-slot other-pointer-type)
      (storew (make-fixup (extern-alien-name "undefined_tramp") :foreign) result
	      fdefn-raw-addr-slot other-pointer-type))))


(define-vop (make-closure)
	    (:args (function :to :save :scs (descriptor-reg)))
  (:info length)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
   (let ((size (+ length closure-info-offset)))
     #-cgc
     (with-allocation (temp)
       (inst lea result (make-ea :byte :base temp :disp function-pointer-type))
       (inst add temp (pad-data-block size))
       (storew 0 temp 0)
       (storew (logior (ash (1- size) type-bits) closure-header-type)
	       result 0 function-pointer-type))
     #+cgc
     (with-cgc-allocation(temp (pad-data-block size))
       (inst lea result
	     (make-ea :byte :base temp :disp function-pointer-type))
       (storew (logior (ash (1- size) type-bits) closure-header-type)
	       temp 0))
     
     (loadw temp function closure-function-slot function-pointer-type)
     (storew temp result closure-function-slot function-pointer-type))))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result temp value-cell-header-type value-cell-size))
    (storew value result value-cell-value-slot other-pointer-type)))



;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst mov result unbound-marker-type)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc descriptor-reg) temp)
  (:generator #-cgc 4 #+cgc 50
    #-cgc
    (with-allocation(temp)		; dynamic free ptr
      (inst lea result (make-ea :byte :base temp :disp lowtag))
      (inst add temp (pad-data-block words))
      (storew 0 temp 0 0))
    #+cgc
    (with-cgc-allocation(temp (pad-data-block words))
      (inst lea result (make-ea :byte :base temp :disp lowtag)))

    (when type
      (inst mov temp (logior (ash (1- words) type-bits) type))
      (storew temp result 0 lowtag))))

(define-vop (var-alloc)
    (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc any-reg) bytes header alloc)
  (:generator #-cgc 6 #+cgc 50
    (inst lea bytes
	  (make-ea :dword :base extra :disp (* (1+ words) word-bytes)))
    (inst mov header bytes)
    (inst shl header (- type-bits 2))	; w+1 to length field
    
    (inst lea header			; (w-1 << 8) | type
	  (make-ea :dword :base header :disp (+ (ash -2 type-bits) type)))
    (inst and bytes (lognot lowtag-mask))
    #-cgc
    (with-allocation (alloc)
      (inst lea result (make-ea :byte :base alloc :disp lowtag))
      (storew header result 0 lowtag)
      (inst add alloc bytes)
      (storew 0 alloc 0))
    ;; Note that the above inst stores into the free space which
    ;; is probably already zeroed. It is a lose in the cgc case because
    ;; it might trash the next region header.
    #+cgc
    (with-cgc-allocation (alloc bytes)
      (inst lea result (make-ea :byte :base alloc :disp lowtag))
      (storew header result 0 lowtag))))

