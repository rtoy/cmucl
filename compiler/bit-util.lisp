;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/compiler/bit-util.lisp,v 1.5 1993/08/12 17:32:09 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    Bit-vector hacking utilities, potentially implementation-dependent for
;;; speed.
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)

(declaim (inline clear-bit-vector set-bit-vector bit-vector-replace
		 bit-vector-copy))

;;; Clear-Bit-Vector  --  Interface
;;;
;;;    Clear a bit-vector to zeros.
;;;
(defun clear-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (do ((i vm:vector-data-offset (1+ i))
       (end (+ vm:vector-data-offset
	       (ash (+ (length vec) (1- vm:word-bits))
		    (- (1- (integer-length vm:word-bits)))))))
      ((= i end) vec)
    (setf (kernel:%raw-bits vec i) 0)))

(defmacro clear-ltn-bit-vector (vec)
  (once-only ((n-vec vec))
    (collect ((res))
      (do ((i local-tn-limit (- i vm:word-bits))
	   (word vm:vector-data-offset (1+ word)))
	  ((<= i 0)
	   (unless (zerop i)
	     (error "local-tn-limit not a vm:word-bits multiple.")))
	(res `(setf (kernel:%raw-bits ,n-vec ,word) 0)))
      `(progn ,@(res) ,n-vec))))

;;; Set-Bit-Vector  --  Interface
;;;
;;;    Fill a bit vector with ones.
;;;
(defun set-bit-vector (vec)
  (declare (type simple-bit-vector vec))
  (bit-orc2 vec vec t))


;;; Bit-Vector-Replace  --  Interface
;;;
;;;    Replace the bits in To with the bits in From.
;;;
(defun bit-vector-replace (to from)
  (declare (type simple-bit-vector to from))
  (bit-ior from from to))


;;; Bit-Vector-Copy  --  Interface
;;;
;;;    Copy a bit-vector.
;;;
(defun bit-vector-copy (vec)
  (declare (type simple-bit-vector vec))
  (bit-ior vec vec (make-array (length vec) :element-type 'bit)))
