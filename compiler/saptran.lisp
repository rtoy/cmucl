;;; -*- Log: C.Log; Package: C -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/saptran.lisp,v 1.1 1992/01/25 05:32:18 wlott Exp $")
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

(defknown sap-int (system-area-pointer) (unsigned-byte 32) (movable flushable))
(defknown int-sap ((unsigned-byte 32)) system-area-pointer (movable))

(defknown sap-ref-8 (system-area-pointer index) (unsigned-byte 8)
  (flushable))
(defknown sap-ref-16 (system-area-pointer index) (unsigned-byte 16)
  (flushable))
(defknown sap-ref-32 (system-area-pointer index) (unsigned-byte 32)
  (flushable))

(defknown signed-sap-ref-8 (system-area-pointer index) (signed-byte 8)
  (flushable))
(defknown signed-sap-ref-16 (system-area-pointer index) (signed-byte 16)
  (flushable))
(defknown signed-sap-ref-32 (system-area-pointer index) (signed-byte 32)
  (flushable))

(defknown %set-sap-ref-8
	  (system-area-pointer index (or (unsigned-byte 8) (signed-byte 8)))
  (or (unsigned-byte 8) (signed-byte 8)) ()
  :derive-type #'result-type-last-arg)
(defknown %set-sap-ref-16
	  (system-area-pointer index (or (unsigned-byte 16) (signed-byte 16)))
  (or (unsigned-byte 16) (signed-byte 16)) ()
  :derive-type #'result-type-last-arg)
(defknown %set-sap-ref-32
	  (system-area-pointer index (or (unsigned-byte 32) (signed-byte 32)))
  (or (unsigned-byte 32) (signed-byte 32)) ()
  :derive-type #'result-type-last-arg)

(defknown sap-ref-sap (system-area-pointer index) system-area-pointer
  (flushable))
(defknown %set-sap-ref-sap (system-area-pointer index system-area-pointer)
  system-area-pointer
  ())

(defknown sap-ref-single (system-area-pointer index) single-float
  (flushable))
(defknown sap-ref-double (system-area-pointer index) double-float
  (flushable))

(defknown %set-sap-ref-single
	  (system-area-pointer index single-float) single-float
  ())
(defknown %set-sap-ref-double
	  (system-area-pointer index double-float) double-float
  ())


;;;; Transforms for converting sap relation operators.

(deftransform sap< ((x y))
  '(< (sap-int x) (sap-int y)))

(deftransform sap<= ((x y))
  '(<= (sap-int x) (sap-int y)))

(deftransform sap= ((x y))
  '(= (sap-int x) (sap-int y)))

(deftransform sap>= ((x y))
  '(>= (sap-int x) (sap-int y)))

(deftransform sap> ((x y))
  '(> (sap-int x) (sap-int y)))


;;;; Transforms for optimizing sap+

(deftransform sap+ ((sap offset))
  (cond ((and (constant-continuation-p offset)
	      (eql (continuation-value offset) 0))
	 'sap)
	(t
	 (extract-function-args sap 'sap+ 2)
	 '(lambda (sap offset1 offset2)
	    (sap+ sap (+ offset1 offset2))))))

(deftransform sap-ref-8 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap offset1 offset2)
     (sap-ref-8 sap (+ offset1 offset2))))

(deftransform signed-sap-ref-8 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap offset1 offset2)
     (signed-sap-ref-8 sap (+ offset1 offset2))))

(deftransform sap-ref-16 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset half-word-offset)
     (sap-ref-16 sap (+ (/ byte-offset 2) half-word-offset))))

(deftransform signed-sap-ref-16 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset half-word-offset)
     (signed-sap-ref-16 sap (+ (/ byte-offset 2) half-word-offset))))

(deftransform sap-ref-32 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-32 sap (+ (/ byte-offset 4) word-offset))))

(deftransform signed-sap-ref-32 ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (signed-sap-ref-32 sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-sap ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-sap sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-single ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-single sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-double ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-double sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-sap ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-sap sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-single ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-single sap (+ (/ byte-offset 4) word-offset))))

(deftransform sap-ref-double ((sap offset))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset)
     (sap-ref-double sap (+ (/ byte-offset 4) word-offset))))

(deftransform %set-sap-ref-8 ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap offset1 offset2 value)
     (%set-sap-ref-8 sap (+ offset1 offset2) value)))

(deftransform %set-sap-ref-16 ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset half-word-offset value)
     (%set-sap-ref-16 sap (+ (/ byte-offset 2) half-word-offset) value)))

(deftransform %set-sap-ref-32 ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset value)
     (%set-sap-ref-32 sap (+ (/ byte-offset 4) word-offset) value)))

(deftransform %set-sap-ref-sap ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset value)
     (%set-sap-ref-sap sap (+ (/ byte-offset 4) word-offset) value)))

(deftransform %set-sap-ref-single ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset value)
     (%set-sap-ref-single sap (+ (/ byte-offset 4) word-offset) value)))

(deftransform %set-sap-ref-double ((sap offset value))
  (extract-function-args sap 'sap+ 2)
  '(lambda (sap byte-offset word-offset value)
     (%set-sap-ref-double sap (+ (/ byte-offset 4) word-offset) value)))
