;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and Raymond Toy based
;;; on public domain code from Carnegie Mellon University and has been
;;; placed in the Public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rand-mt19937.lisp,v 1.5 1998/01/13 19:18:00 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura. This implementation has been
;;; placed in the public domain with permission from M. Matsumoto.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.

(in-package "LISP")
(export '(random-state random-state-p random *random-state*
	  make-random-state))

(in-package "KERNEL")
(export '(%random-single-float %random-double-float random-chunk))


;;;; Random state hackery:

;;; The state is stored in a (simple-array (unsigned-byte 32) (627))
;;; wrapped in a random-state structure:
;;;
;;;  0-1:   Constant matrix A. [0, #x9908b0df]
;;;  2:     Index k.
;;;  3-626: State.

;;; Generate and initialise a new random-state array. Index is
;;; initialised to 1 and the states to 32bit integers excluding zero.
;;;
;;; Seed - A 32bit number, not zero.
;;;
;;; Apparently uses the generator Line 25 of Table 1 in
;;; [KNUTH 1981, The Art of Computer Programming, Vol. 2 (2nd Ed.), pp102]
;;;
(defun init-random-state (&optional (seed 4357) state)
  (declare (type (integer 1 #xffffffff) seed))
  (let ((state (or state (make-array 627 :element-type '(unsigned-byte 32)))))
    (declare (type (simple-array (unsigned-byte 32) (627)) state))
    (setf (aref state 1) #x9908b0df)
    (setf (aref state 2) 1)
    (setf (aref state 3) seed)
    (do ((k 1 (1+ k)))
	((>= k 624))
      (declare (type (mod 625) k))
      (setf (aref state (+ 3 k))
	    (logand (* 69069 (aref state (+ 3 (1- k)))) #xffffffff)))
    state))

(defstruct (random-state
	     (:constructor make-random-object)
	     (:make-load-form-fun :just-dump-it-normally))
  (state (init-random-state) :type (simple-array (unsigned-byte 32) (627))))

(defvar *random-state* (make-random-object))

(defun make-random-state (&optional state)
  "Make a random state object.  If State is not supplied, return a copy
  of the default random state.  If State is a random state, then return a
  copy of it.  If state is T then return a random state generated from
  the universal time."
  (flet ((copy-random-state (state)
	   (let ((state (random-state-state state))
		 (new-state
		  (make-array 627 :element-type '(unsigned-byte 32))))
	     (dotimes (i 627)
	       (setf (aref new-state i) (aref state i)))
	     (make-random-object :state new-state))))
    (cond ((not state) (copy-random-state *random-state*))
	  ((random-state-p state) (copy-random-state state))
	  ((eq state t)
	   (make-random-object :state (init-random-state
				       (logand (get-universal-time)
					       #xffffffff))))
	  (t (error "Argument is not a RANDOM-STATE, T or NIL: ~S" state)))))

(pushnew #'(lambda ()
	     (init-random-state (logand (get-universal-time) #xffffffff)
				(random-state-state *random-state*)))
	 ext:*after-save-initializations*)


;;;; Random entries:

;;; Size of the chunks returned by random-chunk.
;;;
(defconstant random-chunk-length 32)

;;; random-chunk -- Internal
;;;
;;; This function generaters a 32bit integer between 0 and #xffffffff
;;; inclusive.
;;;
(declaim (inline random-chunk))
;;;
;;; Portable implementation.
(defconstant mt19937-n 624)
(defconstant mt19937-m 397)
(defconstant mt19937-upper-mask #x80000000)
(defconstant mt19937-lower-mask #x7fffffff)
(defconstant mt19937-b #x9D2C5680)
(defconstant mt19937-c #xEFC60000)
;;;
#-x86
(defun random-mt19937-update (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((y 0))
    (declare (type (unsigned-byte 32) y))
    (do ((kk 3 (1+ kk)))
	((>= kk (+ 3 (- mt19937-n mt19937-m))))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk mt19937-m))
				    (ash y -1) (aref state (logand y 1)))))
    (do ((kk (+ (- mt19937-n mt19937-m) 3) (1+ kk)))
	((>= kk (+ (1- mt19937-n) 3)))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk (- mt19937-m mt19937-n)))
				    (ash y -1) (aref state (logand y 1)))))
    (setf y (logior (logand (aref state (+ 3 (1- mt19937-n)))
			    mt19937-upper-mask)
		    (logand (aref state 3) mt19937-lower-mask)))
    (setf (aref state (+ 3 (1- mt19937-n)))
	  (logxor (aref state (+ 3 (1- mt19937-m)))
		  (ash y -1) (aref state (logand y 1)))))
  (values))
;;;
#-x86
(defun random-chunk (state)
  (declare (type random-state state))
  (let* ((state (random-state-state state))
	 (k (aref state 2)))
    (declare (type (mod 628) k))
    (when (= k mt19937-n)
      (random-mt19937-update state)
      (setf k 0))
    (setf (aref state 2) (1+ k))
    (let ((y (aref state (+ 3 k))))
      (declare (type (unsigned-byte 32) y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (ash (logand y (ash mt19937-b -7)) 7)))
      (setf y (logxor y (ash (logand y (ash mt19937-c -15)) 15)))
      (setf y (logxor y (ash y -18)))
      y)))

;;; Using inline VOP support, only available on the x86 so far.
#+x86
(defun random-chunk (state)
  (declare (type random-state state))
  (vm::random-mt19937 (random-state-state state)))



;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  We generate a float
;;; between 0.0 and 1.0 by clobbering the significand of 1.0 with random bits,
;;; then subtracting 1.0.  This hides the fact that we have a hidden bit.
;;;
(declaim (inline %random-single-float %random-double-float))
(declaim (ftype (function ((single-float (0f0)) random-state)
			  (single-float 0f0))
		%random-single-float))
;;;
(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type random-state state))
  (* arg
     (- (make-single-float
	 (dpb (ash (random-chunk state)
		   (- vm:single-float-digits random-chunk-length))
	      vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
;;;
(declaim (ftype (function ((double-float (0d0)) random-state)
			  (double-float 0d0))
		%random-double-float))
;;;
;;; 32bit version
;;;
#+nil
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* (float (random-chunk state) 1d0) (/ 1d0 (expt 2 32))))
;;;
;;; 53bit version.
;;;
#-x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* arg
     (- (lisp::make-double-float
	 (dpb (ash (random-chunk state)
		   (- vm:double-float-digits random-chunk-length
		      vm:word-bits))
	      vm:double-float-significand-byte
	      (lisp::double-float-high-bits 1d0))
	 (random-chunk state))
	1d0)))

;;; Using a faster inline VOP.
#+x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (let ((state-vector (random-state-state state)))
    (* arg
       (- (lisp::make-double-float
	   (dpb (ash (vm::random-mt19937 state-vector)
		     (- vm:double-float-digits random-chunk-length
			vm:word-bits))
		vm:double-float-significand-byte
		(lisp::double-float-high-bits 1d0))
	   (vm::random-mt19937 state-vector))
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
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0))) :datum arg
	    :format-control "Argument is not a positive integer or a positive float: ~S"
	    :format-arguments (list arg)))))
