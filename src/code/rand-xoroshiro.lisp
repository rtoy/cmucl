;;; -*- Mode: Lisp; Package: Kernel -*-
;;;
;;; **********************************************************************
(ext:file-comment
  "$Header: src/code/rand-xoroshiro.lisp $")

;;;
;;; **********************************************************************
;;;
;;; Support for the xoroshiro128+ random number generator by David
;;; Blackman and Sebastiano Vigna (vigna@acm.org)

(in-package "LISP")
(intl:textdomain "cmucl")

#+nil
(export '(xoro-random-state xoro-random-state-p xoro-random *xoro-random-state*
	  make-xoro-random-state))

(in-package "KERNEL")
(export '(%xoroshiro-single-float %xoroshiro-double-float xoroshiro-chunk init-xoro-state))

(sys:register-lisp-feature :random-xoroshiro)

(defun int-init-xoro-state (&optional (seed 5772156649015328606) state)
  (let ((state (or state (make-array 2 :element-type 'double-float)))
	(splitmix-state (ldb (byte 64 0) seed)))
    (flet ((splitmix64 ()
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

(defun vec-init-xoro-state (key &optional state)
  (declare (type (array (unsigned-byte 32) (4)) key)
	   (type (simple-array double-float (2)) state))
  (flet ((make-double (hi lo)
	   (kernel:make-double-float
		(if (< hi #x80000000)
		    hi
		    (- hi #x100000000))
		lo)))
    (setf (aref state 0) (make-double (aref key 0) (aref key 1))
	  (aref state 1) (make-double (aref key 2) (aref key 3)))
    state))
  
  
(defun init-xoro-state (&optional (seed 5772156649015328606) state)
  "Generate an random state vector from the given SEED.  The seed can be
  either an integer or a vector of (unsigned-byte 32)"
  (declare (type (or null integer
		     (array (unsigned-byte 32) (*)))
		 seed))
  (let ((state (or state (make-array 2 :element-type 'double-float))))
    (etypecase seed
      (integer
       (int-init-xoro-state (ldb (byte 64 0) seed) state))
      ((array (unsigned-byte 32) (4))
       (vec-init-xoro-state seed state)))))

(defstruct (xoro-random-state
	     (:constructor make-xoroshiro-object)
	     (:print-function %print-xoro-state)
	     (:make-load-form-fun :just-dump-it-normally))
  ;; The state of the RNG.  The actual algorithm uses 2 64-bit words
  ;; of state.  To reduce consing, we use an array of double-float's
  ;; since a double-float is 64 bits long.  At no point do we operate
  ;; on these as floats; they're just convenient objects to hold the
  ;; state we need.
  (state (init-xoro-state)
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
  (let ((state (xoro-random-state-state rng-state)))
    (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
      (prin1 'xoro-random-state stream)
      (write-char #\space stream)
      (pprint-indent :block 2 stream)
      (pprint-newline :linear stream)
      (prin1 :state stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (pprint-logical-block (stream nil :prefix "#.(" :suffix ")")
	(prin1 'init-xoro-state stream)
	(write-char #\space stream)
	(prin1 (make-array 4 :element-type '(unsigned-byte 32)
			 :initial-contents (list (ldb (byte 32 0)
						      (double-float-high-bits (aref state 0)))
						 (double-float-low-bits (aref state 0))
						 (ldb (byte 32 0)
						      (double-float-high-bits (aref state 1)))
						 (double-float-low-bits (aref state 1))))
	       stream))
      (write-char #\space stream)
      (pprint-newline :linear stream)

      (prin1 :rand stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (prin1 (xoro-random-state-rand rng-state) stream)
      (write-char #\space stream)
      (pprint-newline :linear stream)

      (prin1 :cached-p stream)
      (write-char #\space stream)
      (pprint-newline :miser stream)
      (prin1 (xoro-random-state-cached-p rng-state) stream))))

(defvar *xoro-random-state*
  (make-xoroshiro-object))

(defun make-xoro-random-state (&optional state)
  (flet ((copy-random-state (state)
	   (let ((old-state (xoro-random-state-state state))
		 (new-state
		  (make-array 2 :element-type 'double-float)))
	     (setf (aref new-state 0) (aref old-state 0))
	     (setf (aref new-state 1) (aref old-state 1))
	     (make-xoroshiro-object :state new-state
				    :rand (xoro-random-state-rand state)
				    :cached-p (xoro-random-state-cached-p state)))))
    (cond ((not state)
	   (copy-random-state *xoro-random-state*))
	  ((xoro-random-state-p state)
	   (copy-random-state state))
	  ((eq state t)
	   (make-xoroshiro-object :state (init-xoro-state (generate-seed 4))
				  :rand 0
				  :cached-p nil))
	  (t
	   (error "Argument is not a RANDOM-STATE, T, or NIL: ~S" state)))))

;;;; Random entries:

(declaim (ext:start-block xoroshiro-gen xoroshiro-chunk
			  %xoroshiro-single-float %xoroshiro-double-float
			  %xoroshiro-integer
			  #+double-double
			  %xoroshiro-double-double-float))
;;#+x86
;;(declaim (inline xoroshiro-next))
#+x86
(defun xoroshiro-gen (state)
  (declare (type (simple-array double-float (2)) state)
	   (optimize (speed 3) (safety 0)))
  (vm::xoroshiro-next state))

#-x86
(defun xoroshiro-gen (state)
  (declare (type (simple-array double-float (2)) state)
	   (optimize (speed 3) (safety 0)))
  (flet ((rotl-55 (x1 x0)
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
      (multiple-value-bind (x1 x0)
	  (kernel:double-float-bits (aref state 0))
	(setf s0-1 (ldb (byte 32 0) x1)
	      s0-0 x0))
      (multiple-value-bind (x1 x0)
	  (kernel:double-float-bits (aref state 1))
	(setf s1-1 (ldb (byte 32 0) x1)
	      s1-0 x0))

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

;;; Size of the chunks returned by xoroshiro-chunk.
;;;
;;(defconstant random-chunk-length 32)

;;; xoroshiro-chunk -- Internal
;;;
;;; This function generaters a 32bit integer between 0 and #xffffffff
;;; inclusive.
;;;
(declaim (inline xoroshiro-chunk))

(defun xoroshiro-chunk (rng-state)
  (declare (type xoro-random-state rng-state)
	   (optimize (speed 3) (safety 0)))
  (let ((cached (xoro-random-state-cached-p rng-state)))
    (cond (cached
	   (setf (xoro-random-state-cached-p rng-state) nil)
	   (xoro-random-state-rand rng-state))
	  (t
	   (let ((s (xoro-random-state-state rng-state)))
	     (declare (type (simple-array double-float (2)) s))
	     (multiple-value-bind (r1 r0)
		 (xoroshiro-gen s)
	       (setf (xoro-random-state-rand rng-state) r0)
	       (setf (xoro-random-state-cached-p rng-state) t)
	       r1))))))


;;; %RANDOM-SINGLE-FLOAT, %RANDOM-DOUBLE-FLOAT  --  Interface
;;;
;;;    Handle the single or double float case of RANDOM.  We generate a float
;;; between 0.0 and 1.0 by clobbering the significand of 1.0 with random bits,
;;; then subtracting 1.0.  This hides the fact that we have a hidden bit.
;;;
(declaim (inline %xoroshiro-single-float %xoroshiro-double-float))
(declaim (ftype (function ((single-float (0f0)) xoro-random-state)
			  (single-float 0f0))
		%xoroshiro-single-float))
;;;
(defun %xoroshiro-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type xoro-random-state state))
  (* arg
     (- (make-single-float
	 (dpb (ash (xoroshiro-chunk state)
		   (- vm:single-float-digits random-chunk-length))
	      vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
;;;
(declaim (ftype (function ((double-float (0d0)) xoro-random-state)
			  (double-float 0d0))
		%xoroshiro-double-float))
;;;
;;; 53bit version.
;;;
(defun %xoroshiro-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type xoro-random-state state))
  (* arg
     (- (lisp::make-double-float
	 (dpb (ash (xoroshiro-chunk state)
		   (- vm:double-float-digits random-chunk-length
		      vm:word-bits))
	      vm:double-float-significand-byte
	      (lisp::double-float-high-bits 1d0))
	 (xoroshiro-chunk state))
	1d0)))

#+double-double
(defun %xoroshiro-double-double-float (arg state)
  (declare (type (double-double-float (0w0)) arg)
	   (type xoro-random-state state))
  ;; Generate a 31-bit integer, scale it and sum them up
  (let* ((r 0w0)
	 (scale (scale-float 1d0 -31))
	 (mult scale))
    (declare (double-float mult)
	     (type double-double-float r)
	     (optimize (speed 3) (inhibit-warnings 3)))
    (dotimes (k 4)
      (setf r (+ r (* mult (ldb (byte 31 0) (xoroshiro-chunk state)))))
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
(defun %xoroshiro-integer (arg state)
  (declare (type (integer 1) arg)
	   (type xoro-random-state state))
  (let ((shift (- random-chunk-length random-integer-overlap)))
    (do ((bits (xoroshiro-chunk state)
	       (logxor (ash bits shift) (xoroshiro-chunk state)))
	 (count (+ (integer-length arg)
		   (- random-integer-extra-bits shift))
		(- count shift)))
	((minusp count)
	 (rem bits arg))
      (declare (fixnum count)))))

(declaim (ext:end-block))

(defun xoro-random (arg &optional (state *xoro-random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (declare (inline %xoroshiro-single-float %xoroshiro-double-float
		   #+long-float %long-float))
  (cond
    ((typep arg '(integer 1 #x100000000))
     ;; Let the compiler deftransform take care of this case.
     (%xoroshiro-integer arg state))
    ((and (typep arg 'single-float) (> arg 0.0F0))
     (%xoroshiro-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0D0))
     (%xoroshiro-double-float arg state))
    #+long-float
    ((and (typep arg 'long-float) (> arg 0.0L0))
     (%xoroshiro-long-float arg state))
    #+double-double
    ((and (typep arg 'double-double-float) (> arg 0.0w0))
     (%xoroshiro-double-double-float arg state))
    ((and (integerp arg) (> arg 0))
     (%xoroshiro-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0.0))) :datum arg
	    :format-control (intl:gettext "Argument is not a positive integer or a positive float: ~S")
	    :format-arguments (list arg)))))

(defun xoroshiro-jump (rng-state)
  (declare (type xoro-random-state rng-state))
  (let ((state (xoro-random-state-state rng-state))
	(s0-0 0)
	(s0-1 0)
	(s1-0 0)
	(s1-1 0))
    (declare (type (unsigned-byte 32) s0-0 s0-1 s1-0 s1-1)
	     (optimize (speed 3) (safety 0)))
    (dolist (jump '(#xbeac0467eba5facb #xd86b048b86aa9922))
      (declare (type (unsigned-byte 64) jump))
      (dotimes (b 64)
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
	(format t "jump: ~D s0, s1 = ~X~8,'0X  ~X~8,'0X~%" b s0-1 s0-0 s1-1 s1-0)
	(xoroshiro-next state)))

    (flet ((convert (x1 x0)
	     (declare (type (unsigned-byte 32) x1 x0))
	     (kernel:make-double-float
	      (if (< x1 #x80000000) x1 (- x1 #x100000000))
	      x0)))
      (setf (aref state 0) (convert s0-1 s0-0))
      (setf (aref state 1) (convert s1-1 s1-0)))
      rng-state))

