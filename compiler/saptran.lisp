;;; -*- Log: C.Log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/saptran.lisp,v 1.6 1997/02/15 17:10:46 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains some magic hacks for optimizing SAP operations.
;;;
;;; Written by William Lott.
;;;
(in-package "C")



;;;; Defknowns

    
(defknown foreign-symbol-address (simple-string) system-area-pointer
  (movable flushable))

(defknown (sap< sap<= sap= sap>= sap>)
	  (system-area-pointer system-area-pointer) boolean
  (movable flushable))

(defknown sap+ (system-area-pointer integer) system-area-pointer
  (movable flushable))
(defknown sap- (system-area-pointer system-area-pointer) (signed-byte 32)
  (movable flushable))

(defknown sap-int (system-area-pointer) (unsigned-byte #-alpha 32 #+alpha 64)
  (movable flushable))
(defknown int-sap ((unsigned-byte #-alpha 32 #+alpha 64))
  system-area-pointer (movable))


(defknown sap-ref-8 (system-area-pointer #+x86 fixnum #-x86 index) (unsigned-byte 8)
  (flushable))
(defknown %set-sap-ref-8 (system-area-pointer #+x86 fixnum #-x86 index (unsigned-byte 8))
  (unsigned-byte 8)
  ())

(defknown sap-ref-16 (system-area-pointer #+x86 fixnum #-x86 index) (unsigned-byte 16)
  (flushable))
(defknown %set-sap-ref-16 (system-area-pointer #+x86 fixnum #-x86 index (unsigned-byte 16))
  (unsigned-byte 16)
  ())

(defknown sap-ref-32 (system-area-pointer #+x86 fixnum #-x86 index) (unsigned-byte 32)
  (flushable))
(defknown %set-sap-ref-32 (system-area-pointer #+x86 fixnum #-x86 index (unsigned-byte 32))
  (unsigned-byte 32)
  ())

#+alpha
(defknown sap-ref-64 (system-area-pointer #+x86 fixnum #-x86 index) (unsigned-byte 64)
  (flushable))
#+alpha
(defknown %set-sap-ref-64 (system-area-pointer #+x86 fixnum #-x86 index (unsigned-byte 64))
  (unsigned-byte 64)
  ())


(defknown signed-sap-ref-8 (system-area-pointer #+x86 fixnum #-x86 index) (signed-byte 8)
  (flushable))
(defknown %set-signed-sap-ref-8 (system-area-pointer #+x86 fixnum #-x86 index (signed-byte 8))
  (signed-byte 8)
  ())

(defknown signed-sap-ref-16 (system-area-pointer #+x86 fixnum #-x86 index) (signed-byte 16)
  (flushable))
(defknown %set-signed-sap-ref-16 (system-area-pointer #+x86 fixnum #-x86 index (signed-byte 16))
  (signed-byte 16)
  ())

(defknown signed-sap-ref-32 (system-area-pointer #+x86 fixnum #-x86 index) (signed-byte 32)
  (flushable))
(defknown %set-signed-sap-ref-32 (system-area-pointer #+x86 fixnum #-x86 index (signed-byte 32))
  (signed-byte 32)
  ())

#+alpha
(defknown signed-sap-ref-64 (system-area-pointer #+x86 fixnum #-x86 index) (signed-byte 64)
  (flushable))
#+alpha
(defknown %set-signed-sap-ref-64 (system-area-pointer #+x86 fixnum #-x86 index (signed-byte 64))
  (signed-byte 64)
  ())


(defknown sap-ref-sap (system-area-pointer #+x86 fixnum #-x86 index) system-area-pointer
  (flushable))
(defknown %set-sap-ref-sap (system-area-pointer #+x86 fixnum #-x86 index system-area-pointer)
  system-area-pointer
  ())

(defknown sap-ref-single (system-area-pointer #+x86 fixnum #-x86 index) single-float
  (flushable))
(defknown sap-ref-double (system-area-pointer #+x86 fixnum #-x86 index) double-float
  (flushable))

(defknown %set-sap-ref-single
	  (system-area-pointer #+x86 fixnum #-x86 index single-float) single-float
  ())
(defknown %set-sap-ref-double
	  (system-area-pointer #+x86 fixnum #-x86 index double-float) double-float
  ())


;;;; Transforms for converting sap relation operators.

(dolist (info '((sap< <) (sap<= <=) (sap= =) (sap>= >=) (sap> >)))
  (destructuring-bind (sap-fun int-fun) info
    (deftransform sap-fun ((x y) '* '* :eval-name t)
      `(,int-fun (sap-int x) (sap-int y)))))


;;;; Transforms for optimizing sap+

(deftransform sap+ ((sap offset))
  (cond ((and (constant-continuation-p offset)
	      (eql (continuation-value offset) 0))
	 'sap)
	(t
	 (extract-function-args sap 'sap+ 2)
	 '(lambda (sap offset1 offset2)
	    (sap+ sap (+ offset1 offset2))))))

(dolist (fun '(sap-ref-8 %set-sap-ref-8
	       signed-sap-ref-8 %set-signed-sap-ref-8
	       sap-ref-16 %set-sap-ref-16
	       signed-sap-ref-16 %set-signed-sap-ref-16
	       sap-ref-32 %set-sap-ref-32
	       signed-sap-ref-32 %set-signed-sap-ref-32
	       sap-ref-sap %set-sap-ref-sap
	       sap-ref-single %set-sap-ref-single
	       sap-ref-double %set-sap-ref-double))
  (deftransform fun ((sap offset) '* '* :eval-name t)
    (extract-function-args sap 'sap+ 2)
    `(lambda (sap offset1 offset2)
       (,fun sap (+ offset1 offset2)))))
