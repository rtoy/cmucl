;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rand.lisp,v 1.7 1994/10/31 04:11:27 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to random number functions for Spice Lisp
;;;
;;; Originally written by David Adam.  Python tuning, better large integer
;;; randomness and efficient IEEE float support by Rob MacLachlan.
;;;
(in-package "LISP")
(export '(random-state random-state-p random *random-state*
	  make-random-state))

(in-package "KERNEL")
(export '(%random-single-float %random-double-float random-chunk
			       random-fixnum-max))


;;;; Random state hackery:

(defconstant random-const-a 8373)
(defconstant random-const-c 101010101)
(defconstant random-max 54)

;;; Inclusive upper bound on the size of fixnum kept in the state (and returned
;;; by random-chunk.)  Must be even.
;;;
(defconstant random-upper-bound (- most-positive-fixnum 3))
(defconstant random-chunk-length (integer-length random-upper-bound))
(deftype random-chunk () `(integer 0 ,random-upper-bound))

(defvar rand-seed 0)
(defstruct (random-state
	    (:constructor make-random-object)
	    (:make-load-form-fun :just-dump-it-normally))
  (j 24 :type index)
  (k 0 :type index)
  (seed (make-array (1+ random-max) :initial-contents
		    (do ((list-rands () (cons (rand1) list-rands))
			 (i 0 (1+ i)))
			((> i random-max) list-rands)
		      (declare (fixnum i))))
	:type simple-vector))


;;; Generates a random number from rand-seed.
(defun rand1 ()
  (declare (optimize (inhibit-warnings 3)))
  (setq rand-seed
	(mod (+ (* rand-seed random-const-a) random-const-c)
	     (1+ random-upper-bound))))


(defvar *random-state* (make-random-object))


(defun copy-state (cur-state)
  (let ((state (make-random-object
		:seed (make-array 55)
		:j (random-state-j cur-state)
		:k (random-state-k cur-state))))
    (do ((i 0 (1+ i)))
	((= i 55) state)
      (declare (fixnum i))
      (setf (aref (random-state-seed  state) i)
	    (aref (random-state-seed cur-state) i)))))

(defun make-random-state (&optional state)
  "Make a random state object.  If State is not supplied, return a copy
  of the default random state.  If State is a random state, then return a
  copy of it.  If state is T then return a random state generated from
  the universal time."
  (cond ((not state) (copy-state *random-state*))
	((random-state-p state) (copy-state state))
	((eq state t) (setq rand-seed (get-universal-time))
		      (make-random-object))
	(t (error "Argument is not a RANDOM-STATE, T or NIL: ~S" state))))


;;;; Random entries:

(declaim (start-block random %random-single-float %random-double-float
		      random-chunk))

;;; random-chunk  --  Internal
;;;
;;; This function generates fixnums between 0 and random-upper-bound, 
;;; inclusive.  For the algorithm to work random-upper-bound must be an 
;;; even positive fixnum.  State is the random state to use.
;;;
(declaim (ftype (function (random-state) random-chunk) random-chunk))
(defun random-chunk (state)
  (let* ((seed (random-state-seed state))
	 (j (random-state-j state))
	 (k (random-state-k state))
	 (a (- (- random-upper-bound
		  (the random-chunk
		       (svref seed
			      (setf (random-state-j state)
				    (if (= j 0) random-max (1- j))))))
	       (the random-chunk
		    (svref seed
			   (setf (random-state-k state)
				 (if (= k 0) random-max (1- k))))))))
    (declare (fixnum a))
    (setf (svref seed k)
	  (the random-chunk (if (minusp a) (- a) (- random-upper-bound a))))))


;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  We generate a float
;;; between 0.0 and 1.0 by clobbering the significand of 1.0 with random bits,
;;; then subtracting 1.0.  This hides the fact that we have a hidden bit.
;;;
(declaim (inline %random-single-float %random-double-float))
(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type (or random-state null) state))
  (* arg
     (- (make-single-float
	 (dpb (ash (random-chunk (or state *random-state*))
		   (- vm:single-float-digits random-chunk-length))
	      vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
;;;
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type (or random-state null) state))
  (let ((state (or state *random-state*)))
    (* arg
       (- (make-double-float
	   (dpb (ash (random-chunk state)
		     (- vm:double-float-digits random-chunk-length
			vm:word-bits))
		vm:double-float-significand-byte
		(double-float-high-bits 1d0))
	   (logxor (ash (random-chunk state)
			(- vm:word-bits random-chunk-length))
		   (ash (random-chunk state)
			(- random-chunk-length vm:word-bits))))
	  1d0))))


;;;; Random integers:

;;; Amount we overlap chunks by when building a large integer to make up for
;;; the loss of randomness in the low bits.
;;;
(defconstant random-integer-overlap 3)

;;; Extra bits of randomness that we generate before taking the value MOD the
;;; limit, to avoid loss of randomness near the limit.
;;;
(defconstant random-integer-extra-bits 10)

;;; Largest fixnum we can compute from one chunk of bits.
;;;
(defconstant random-fixnum-max
  (1- (ash 1 (- random-chunk-length random-integer-extra-bits))))


;;; %RANDOM-INTEGER  --  Internal
;;;
(defun %random-integer (arg state)
  (declare (type (integer 1) arg) (type random-state state))
  (let ((shift (- random-chunk-length random-integer-overlap)))
    (do ((bits (random-chunk state)
	       (logxor (ash bits shift) (random-chunk state)))
	 (count (+ (integer-length arg)
		   (- random-integer-extra-bits shift))
		(- count shift)))
	((minusp count)
	 (rem bits arg))
      (declare (fixnum count)))))

(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (declare (inline %random-single-float %random-double-float))
  (cond
   ((and (fixnump arg) (<= arg random-fixnum-max))
    (rem (random-chunk state) arg))
   ((typep arg 'single-float)
    (%random-single-float arg state))
   ((typep arg 'double-float)
    (%random-double-float arg state))
   ((integerp arg)
    (%random-integer arg state))
   (t
    (error 'simple-type-error :expected-type '(real (0)) :datum arg
	   :format-control "Argument is not a positive real number: ~S"
	   :format-arguments (list arg)))))
