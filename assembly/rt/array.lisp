;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you want to use this code or any part of CMU Common
;;; Lisp, please contact Scott Fahlman (Scott.Fahlman@CS.CMU.EDU)
;;; **********************************************************************
;;;
;;; $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/assembly/rt/array.lisp,v 1.1 1991/02/18 15:43:32 chiles Exp $
;;;
;;; This file contains the support routines for arrays and vectors.
;;;
;;; Written by William Lott.
;;;

(in-package "RT")


(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type any-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:arg words any-reg a2-offset)
			  (:res result descriptor-reg a0-offset)

			  (:temp ndescr non-descriptor-reg nl0-offset)
			  (:temp alloc word-pointer-reg ocfp-offset)
			  (:temp vector descriptor-reg cname-offset))
  (pseudo-atomic (ndescr)
    (load-symbol-value alloc *allocation-pointer*)
    (inst cal vector alloc vm:other-pointer-type)
    (inst cal alloc alloc (+ (1- (ash 1 vm:lowtag-bits))
			     (* vm:vector-data-offset vm:word-bytes)))
    (inst cal alloc alloc words)
    (inst li ndescr (lognot vm:lowtag-mask))
    (inst n alloc ndescr)
    (move ndescr type)
    (inst sr ndescr vm:word-shift)
    (storew ndescr vector 0 vm:other-pointer-type)
    (storew length vector vm:vector-length-slot vm:other-pointer-type))
  (move result vector))



;;;; Hash primitives

(defparameter sxhash-simple-substring-entry (gen-label))

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:res result any-reg a0-offset)
			  (:temp length any-reg a1-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nargs-offset)
			  (:temp temp non-descriptor-reg ocfp-offset))
  (declare (ignore result accum data temp offset))
  (inst b sxhash-simple-substring-entry)
  (loadw length string vm:vector-length-slot vm:other-pointer-type))


(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg a0-offset)
			  (:arg length any-reg a1-offset)
			  (:res result any-reg a0-offset)
			  (:temp accum non-descriptor-reg nl0-offset)
			  (:temp data non-descriptor-reg nargs-offset)
			  (:temp temp non-descriptor-reg ocfp-offset)
			  (:temp lip interior-reg lip-offset))
  (emit-label sxhash-simple-substring-entry)
  ;; Get a stack slot and save the contents of NFP.
  (inst inc csp-tn word-bytes)
  (storew nfp-tn csp-tn (* -1 word-bytes))
  ;; Save the return-pc in NFP as a byte offset from the component start.
  (move nfp-tn lip)
  (inst s nfp-tn code-tn)
  (inst a lip string (- (* vector-data-offset word-bytes) other-pointer-type))
  (inst b test)
  (inst li accum 0)
  LOOP
  (inst x accum data)
  (move temp accum)
  (inst sl temp 27)
  (inst sr accum 5)
  (inst o accum temp)
  (inst inc lip 4)
  TEST
  (inst s length (fixnum 4))
  (inst bnc :lt loop)
  (loadw data lip)
  (inst a length (fixnum 4))
  (inst bc :eq done)
  (inst neg length)
  (inst sl length 1)
  (inst sr data length)
  (inst x accum data)
  DONE
  (inst sl result accum 5)
  (inst sr result result 3)
  (move code-tn lip)
  (inst a lip nfp-tn)
  (loadw nft-tn csp-tn (* -1 word-bytes))
  (inst dec csp-tn word-bytes))
