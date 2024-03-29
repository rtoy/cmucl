;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
;;; This code was written as part of CMU Common Lisp and has been
;;; placed in the public domain, and is provided 'as is'.
;;;
(ext:file-comment
  "$Header: src/code/rand-xoroshiro.lisp $")

;;;
;;; **********************************************************************
;;;
;;; Support for the xoroshiro128+ random number generator by David
;;; Blackman and Sebastiano Vigna (vigna@acm.org). See
;;; http://xoroshiro.di.unimi.it/.

(in-package "LISP")
(intl:textdomain "cmucl")

(export '(random-state random-state-p random *random-state*
	  make-random-state))

(in-package "KERNEL")
(export '(%random-single-float %random-double-float random-chunk init-random-state
	  random-state-jump))

(sys:register-lisp-feature :random-xoroshiro)


;;;; Random state hackery:

;; Generate a random seed that can be used for seeding the generator.
;; If /dev/urandom is available, it is used to generate random data as
;; the seed.  Otherwise, the current time is used as the seed.
(defun generate-seed (&optional (nwords 1))
  ;; On some systems (as reported by Ole Rohne on cmucl-imp),
  ;; /dev/urandom isn't what we think it is, so if it doesn't work,
  ;; silently generate the seed from the current time.
  (or (ignore-errors
	(let ((words (make-array nwords :element-type '(unsigned-byte 32))))
	  (with-open-file (rand "/dev/urandom"
				:direction :input
				:element-type '(unsigned-byte 32))
	    (read-sequence words rand))
	  (if (= nwords 1)
	      (aref words 0)
	      (let ((vec (make-array (floor nwords 2) :element-type '(unsigned-byte 64))))
		(do ((k 0 (+ k 1))
		     (j 0 (+ j 2)))
		    ((>= k (length vec))
		     vec)
		  (setf (aref vec k)
			(logior (ash (aref words j) 32)
				(aref words (+ j 1)))))))))
      (logand (get-universal-time) #xffffffff)))

(defun int-init-xoro-state (&optional (seed 5772156649015328606) state)
  (let ((state (or state (make-array 2 :element-type 'double-float)))
	(splitmix-state (ldb (byte 64 0) seed)))
    (flet ((splitmix64 ()
	     ;; See http://xoroshiro.di.unimi.it/splitmix64.c for the
	     ;; definitive reference.  The basic algorithm, where x is
	     ;; the 64-bit state of the generator, is:
	     ;;
	     ;;   uint64_t z = (x += 0x9e3779b97f4a7c15);
	     ;;   z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9;
	     ;;   z = (z ^ (z >> 27)) * 0x94d049bb133111eb;
	     ;;   return z ^ (z >> 31);
	     ;;
	     ;; This is only used occasionally for initializing the
	     ;; RNG, so this is a very straight-forward
	     ;; implementation.
	     (let ((z (setf splitmix-state
			    (ldb (byte 64 0) (+ splitmix-state #x9e3779b97f4a7c15)))))
	       (declare (type (unsigned-byte 64) z))
	       (setf z (ldb (byte 64 0)
			    (* (logxor z (ash z -30))
			       #xbf58476d1ce4e5b9)))
	       (setf z (ldb (byte 64 0)
			    (* (logxor z (ash z -27))
			       #x94d049bb133111eb)))
	       (logxor z (ash z -31))))
	   (make-double (x)
	     (let ((lo (ldb (byte 32 0) x))
		   (hi (ldb (byte 32 32) x)))
	       (kernel:make-double-float
		(if (< hi #x80000000)
		    hi
		    (- hi #x100000000))
		lo))))
      (let* ((s0 (splitmix64))
	     (s1 (splitmix64)))
	   (setf (aref state 0) (make-double s0)
		 (aref state 1) (make-double s1))
	   state))))

;; Initialize from an array.  The KEY is a 2-element array of unsigned
;; 64-bit integers.  The state is set to the given 64-bit integer
;; values.
(defun vec-init-xoro-state (key &optional (state (make-array 2 :element-type 'double-float)))
  (declare (type (array (unsigned-byte 64) (2)) key)
	   (type (simple-array double-float (2)) state))
  (flet ((make-double (x)
	   (declare (type (unsigned-byte 64) x))
	   (let ((hi (ldb (byte 32 32) x))
		 (lo (ldb (byte 32 0) x)))
	     (kernel:make-double-float
	      (if (< hi #x80000000)
		  hi
		  (- hi #x100000000))
	      lo))))
    (setf (aref state 0) (make-double (aref key 0))
	  (aref state 1) (make-double (aref key 1)))
    state))

;; The default seed is the digits of Euler's constant, 0.5772....
(defun init-random-state (&optional (seed 5772156649015328606) state)
  _N"Generate an random state vector from the given SEED.  The seed can be
  either an integer or a vector of (unsigned-byte 64)"
  (declare (type (or null integer
		     (array (unsigned-byte 64) (*)))
		 seed))
  (let ((state (or state (make-array 2 :element-type 'double-float))))
    (etypecase seed
      (integer
       (int-init-xoro-state (ldb (byte 64 0) seed) state))
      ((array (unsigned-byte 64) (2))
       (vec-init-xoro-state seed state)))))

(defstruct (random-state
	     (:constructor make-random-object)
	     (:print-function %print-xoro-state)
	     (:make-load-form-fun :just-dump-it-normally))
  ;; The state of the RNG.  The actual algorithm uses 2 64-bit words
  ;; of state.  To reduce consing, we use an array of double-float's
  ;; since a double-float is 64 bits long.  At no point do we operate
  ;; on these as floats; they're just convenient objects to hold the
  ;; state we need.
  (state (init-random-state)
   :type (simple-array double-float (2)))
  ;; The generator produces 64-bit results.  We separate the 64-bit
  ;; result into two parts.  One is returned and the other is cached
  ;; here for later use.
  (rand 0 :type (unsigned-byte 32))
  ;; Indicates if RAND holds a valid value.  If NIL, we need to
  ;; generate a new 64-bit result.
  (cached-p nil :type (member t nil)))

(defun %print-xoro-state (rng-state stream depth)
  (declare (ignore depth))
  ;; Basically the same as the default structure printer, but we want
  ;; to print the state as an array of integers instead of doubles,
  ;; because it's a bit confusing to see the state as doubles.
  (let ((state (random-state-state rng-state)))
    (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
      (prin1 'random-state stream)
      (write-char #\space stream)
      (pprint-indent :block 2 stream)
      (pprint-newline :linear stream)
      (prin1 :state stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (pprint-logical-block (stream nil :prefix "#.(" :suffix ")")
	(prin1 'init-random-state stream)
	(write-char #\space stream)
	(flet ((c (x)
		 (multiple-value-bind (hi lo)
		     (double-float-bits x)
		   (logior (ash (ldb (byte 32 0) hi) 32)
			   lo))))
	  (write (make-array 2 :element-type '(unsigned-byte 64)
			     :initial-contents (list (c (aref state 0))
						     (c (aref state 1))))
		 :stream stream
		 :base 16
		 :radix t)))
      (write-char #\space stream)
      (pprint-newline :linear stream)

      (prin1 :rand stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (prin1 (random-state-rand rng-state) stream)
      (write-char #\space stream)
      (pprint-newline :linear stream)

      (prin1 :cached-p stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (prin1 (random-state-cached-p rng-state) stream))))

(defvar *random-state*
  (make-random-object))

(defun make-random-state (&optional state)
  _N"Make a random state object.  If STATE is not supplied, return a copy
  of the default random state.  If STATE is a random state, then return a
  copy of it.  If STATE is T then return a random state generated from
  the universal time or /dev/urandom if available."
  (flet ((copy-random-state (state)
	   (let ((old-state (random-state-state state))
		 (new-state
		  (make-array 2 :element-type 'double-float)))
	     (setf (aref new-state 0) (aref old-state 0))
	     (setf (aref new-state 1) (aref old-state 1))
	     (make-random-object :state new-state
				 :rand (random-state-rand state)
				 :cached-p (random-state-cached-p state)))))
    (cond ((not state)
	   (copy-random-state *random-state*))
	  ((random-state-p state)
	   (copy-random-state state))
	  ((eq state t)
	   (make-random-object :state (init-random-state (generate-seed 4))
			       :rand 0
			       :cached-p nil))
	  (t
	   (error _"Argument is not a RANDOM-STATE, T, or NIL: ~S" state)))))

(defun rand-initializer ()
  (init-random-state (generate-seed)
                     (random-state-state *random-state*)))

(pushnew 'rand-initializer ext:*after-save-initializations*)

;;;; Random entries:

;; Sparc and x86 have vops to implement xoroshiro-gen that are much
;; faster than the portable lisp version.  Use them.
#+(or x86 sparc)
(declaim (inline xoroshiro-gen))
#+(or x86 sparc)
(defun xoroshiro-gen (state)
  (declare (type (simple-array double-float (2)) state)
	   (optimize (speed 3) (safety 0)))
  (vm::xoroshiro-next state))

#-(or x86 sparc)
(defun xoroshiro-gen (state)
  (declare (type (simple-array double-float (2)) state)
	   (optimize (speed 3) (safety 0)))
  ;; Portable implementation of the xoroshiro128+ generator. See
  ;; http://xoroshiro.di.unimi.it/xoroshiro128plus.c for the
  ;; definitive definition.
  ;;
  ;; uint64_t s[2];
  ;;
  ;; static inline uint64_t rotl(const uint64_t x, int k) {
  ;; 	return (x << k) | (x >> (64 - k));
  ;; }
  ;;
  ;; uint64_t next(void) {
  ;; 	const uint64_t s0 = s[0];
  ;; 	uint64_t s1 = s[1];
  ;; 	const uint64_t result = s0 + s1;
  ;;
  ;; 	s1 ^= s0;
  ;; 	s[0] = rotl(s0, 55) ^ s1 ^ (s1 << 14); // a, b
  ;; 	s[1] = rotl(s1, 36); // c
  ;;
  ;; 	return result;
  ;; }
  ;;
  (flet ((rotl-55 (x1 x0)
	   ;; Rotate [x1|x0] left 55 bits, returning the result as two
	   ;; values.
	   (declare (type (unsigned-byte 32) x0 x1)
		    (optimize (speed 3) (safety 0)))
	   ;; x << 55
	   (let ((sl55-h (ldb (byte 32 0) (ash x0 (- 55 32))))
		 (sl55-l 0))
	     ;; x >> 9
	     (let ((sr9-h (ash x1 -9))
		   (sr9-l (ldb (byte 32 0)
			       (logior (ash x0 -9)
				       (ash x1 23)))))
	       (values (logior sl55-h sr9-h)
		       (logior sl55-l sr9-l)))))
	 (rotl-36 (x1 x0)
	   ;; Rotate [x1|x0] left 36 bits, returning the result as two
	   ;; values.
	   (declare (type (unsigned-byte 32) x0 x1)
		    (optimize (speed 3) (safety 0)))
	   ;; x << 36
	   (let ((sl36-h (ldb (byte 32 0) (ash x0 4))))
	     ;; x >> 28
	     (let ((sr28-l (ldb (byte 32 0)
				(logior (ash x0 -28)
					(ash x1 4))))
		   (sr28-h (ash x1 -28)))
	       (values (logior sl36-h sr28-h)
		       sr28-l))))
	 (shl-14 (x1 x0)
	   ;; Shift [x1|x0] left by 14 bits, returning the result as
	   ;; two values.
	   (declare (type (unsigned-byte 32) x1 x0)
		    (optimize (speed 3) (safety 0)))
	   (values (ldb (byte 32 0)
			(logior (ash x1 14)
				(ash x0 (- 14 32))))
		   (ldb (byte 32 0)
			(ash x0 14))))
	 (make-double (hi lo)
	   (kernel:make-double-float
	    (if (< hi #x80000000)
		hi
		(- hi #x100000000))
	    lo)))
    (let ((s0-1 0)
	  (s0-0 0)
	  (s1-1 0)
	  (s1-0 0))
      (declare (type (unsigned-byte 32) s0-1 s0-0 s1-1 s1-0))
      ;; Load the state to s0 and s1. s0-1 is the high 32-bit part and
      ;; s0-0 is the low 32-bit part of the 64-bit value.  Similarly
      ;; for s1.
      (multiple-value-bind (x1 x0)
	  (kernel:double-float-bits (aref state 0))
	(setf s0-1 (ldb (byte 32 0) x1)
	      s0-0 x0))
      (multiple-value-bind (x1 x0)
	  (kernel:double-float-bits (aref state 1))
	(setf s1-1 (ldb (byte 32 0) x1)
	      s1-0 x0))

      ;; Compute the 64-bit random value: s0 + s1
      (multiple-value-prog1
	  (multiple-value-bind (sum-0 c)
	      (bignum::%add-with-carry s0-0 s1-0 0)
	    (values (bignum::%add-with-carry s0-1 s1-1 c)
		    sum-0))
	;; s1 ^= s0
	(setf s1-1 (logxor s1-1 s0-1)
	      s1-0 (logxor s1-0 s0-0))
	;; s[0] = rotl(s0,55) ^ s1 ^ (s1 << 14)
	(multiple-value-setq (s0-1 s0-0)
	  (rotl-55 s0-1 s0-0))
	(setf s0-1 (logxor s0-1 s1-1)
	      s0-0 (logxor s0-0 s1-0))
	(multiple-value-bind (s14-1 s14-0)
	    (shl-14 s1-1 s1-0)
	  (setf s0-1 (logxor s0-1 s14-1)
		s0-0 (logxor s0-0 s14-0)))

	(multiple-value-bind (r1 r0)
	    (rotl-36 s1-1 s1-0)
	  (setf (aref state 0) (make-double s0-1 s0-0)
		(aref state 1) (make-double r1 r0)))))))

;;; Size of the chunks returned by random-chunk.
;;;
(defconstant random-chunk-length 32)

;;; random-chunk -- Internal
;;;
;;; This function generaters a 32bit integer between 0 and #xffffffff
;;; inclusive.
;;;
(declaim (inline random-chunk))

(defun random-chunk (rng-state)
  (declare (type random-state rng-state)
	   (optimize (speed 3) (safety 0)))
  (let ((cached (random-state-cached-p rng-state)))
    (cond (cached
	   (setf (random-state-cached-p rng-state) nil)
	   (random-state-rand rng-state))
	  (t
	   (let ((s (random-state-state rng-state)))
	     (declare (type (simple-array double-float (2)) s))
	     (multiple-value-bind (r1 r0)
		 (xoroshiro-gen s)
	       (setf (random-state-rand rng-state) r0)
	       (setf (random-state-cached-p rng-state) t)
	       r1))))))


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
;;; 53-bit version.
;;;
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  ;; xoroshiro-gen produces 64-bit values.  Should we use that
  ;; directly to get the random bits instead of two calls to
  ;; RANDOM-CHUNK?
  (* arg
     (- (lisp::make-double-float
	 (dpb (ash (random-chunk state)
		   (- vm:double-float-digits random-chunk-length
		      vm:word-bits))
	      vm:double-float-significand-byte
	      (lisp::double-float-high-bits 1d0))
	 (random-chunk state))
	1d0)))

#+double-double
(defun %random-double-double-float (arg state)
  (declare (type (double-double-float (0w0)) arg)
	   (type random-state state))
  ;; Generate a 31-bit integer, scale it and sum them up
  (let* ((r 0w0)
	 (scale (scale-float 1d0 -31))
	 (mult scale))
    (declare (double-float mult)
	     (type double-double-float r)
	     (optimize (speed 3) (inhibit-warnings 3)))
    (dotimes (k 4)
      (setf r (+ r (* mult (ldb (byte 31 0) (random-chunk state)))))
      (setf mult (* mult scale)))
    (* arg r)))

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
  (declare (type (integer 1) arg)
	   (type random-state state))
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
  _N"Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (declare (inline %random-single-float %random-double-float))
  (cond
    ((typep arg '(integer 1 #x100000000))
     ;; Do the same thing as the deftransform would do.
     (if (= arg (expt 2 32))
	 (random-chunk state)
	 (values (bignum::%multiply (random-chunk state)
				    arg))))
    ((and (typep arg 'single-float) (> arg 0.0F0))
     (%random-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0D0))
     (%random-double-float arg state))
    #+double-double
    ((and (typep arg 'double-double-float) (> arg 0.0w0))
     (%random-double-double-float arg state))
    ((and (integerp arg) (> arg 0))
     (%random-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0.0))) :datum arg
	    :format-control _"Argument is not a positive integer or a positive float: ~S"
	    :format-arguments (list arg)))))

;; Jump function for the generator.  See the jump function in
;; http://xoroshiro.di.unimi.it/xoroshiro128plus.c
(defun random-state-jump (&optional (rng-state *random-state*))
  _N"Jump the RNG-STATE.  This is equivalent to 2^64 calls to the
  xoroshiro128+ generator.  It can be used to generate 2^64
  non-overlapping subsequences for parallel computations."
  (declare (type random-state rng-state))
  (let ((state (random-state-state rng-state))
	(s0-0 0)
	(s0-1 0)
	(s1-0 0)
	(s1-1 0))
    (declare (type (unsigned-byte 32) s0-0 s0-1 s1-0 s1-1)
	     (optimize (speed 3) (safety 0)))
    ;; The constants are #xbeac0467eba5facb and #xd86b048b86aa9922,
    ;; and we process these numbers starting from the LSB.  We want ot
    ;; process these in 32-bit chunks, so word-reverse the constants.
    (dolist (jump '(#xeba5facb #xbeac0467 #x86aa9922 #xd86b048b))
      (declare (type (unsigned-byte 32) jump))
      (dotimes (b 32)
	(declare (fixnum b))
	(when (logbitp b jump)
	  (multiple-value-bind (x1 x0)
	      (kernel:double-float-bits (aref state 0))
	    (setf s0-1 (logxor s0-1 (ldb (byte 32 0) x1))
		  s0-0 (logxor s0-0 x0)))
	  
	  (multiple-value-bind (x1 x0)
	      (kernel:double-float-bits (aref state 1))
	    (setf s1-1 (logxor s1-1 (ldb (byte 32 0) x1))
		  s1-0 (logxor s1-0 x0))))
	(xoroshiro-gen state)))

    (flet ((convert (x1 x0)
	     (declare (type (unsigned-byte 32) x1 x0))
	     (kernel:make-double-float
	      (if (< x1 #x80000000) x1 (- x1 #x100000000))
	      x0)))
      (setf (aref state 0) (convert s0-1 s0-0))
      (setf (aref state 1) (convert s1-1 s1-0)))
      rng-state))
