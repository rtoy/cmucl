;;; -*- Mode: Lisp; Package: Lisp; Log: code.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/rand.lisp,v 1.3 1991/02/08 13:35:04 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Functions to random number functions for Spice Lisp 
;;; Written by David Adam.
;;;
;;; The random number functions are part of the standard Spicelisp environment.
;;;
;;; **********************************************************************
;;;
(in-package 'lisp)
(export '(random-state random-state-p random *random-state*
	  make-random-state))

(defconstant random-const-a 8373)
(defconstant random-const-c 101010101)
(defconstant random-upper-bound (1- most-positive-fixnum))
(defconstant random-max 54)
(defconstant %fixnum-length (integer-length most-positive-fixnum))
(defvar rand-seed 0)

(defstruct (random-state (:constructor make-random-object))
  (j 24 :type integer)
  (k 0 :type integer)
  (seed (make-array (1+ random-max) :initial-contents
		    (do ((list-rands () (cons (rand1) list-rands))
			 (i 0 (1+ i)))
			((> i random-max) list-rands)))
	:type simple-vector))


;;; Generates a random number from rand-seed.
(defun rand1 ()
   (setq rand-seed (mod (+ (* rand-seed random-const-a) random-const-c)
			(1+ random-upper-bound))))


(defvar *random-state* (make-random-object))


;;; rand3  --  Internal
;;;
;;; This function generates fixnums between 0 and random-upper-bound, 
;;; inclusive For the algorithm to work random-upper-bound must be an 
;;; even positive fixnum.  State is the random state to use.
;;;
(defun rand3 (state)
  (let ((seed (random-state-seed state))
	(j (random-state-j state))
	(k (random-state-k state)))
    (declare (fixnum j k) (simple-vector seed))
    (setf (svref seed k)
	  (let ((a (- random-upper-bound
		      (svref seed
			      (setf (random-state-j state)
				    (if (= j 0) random-max (1- j))))
		      (svref seed
			      (setf (random-state-k state)
				    (if (= k 0) random-max (1- k)))))))
	    (if (minusp a) (- a) (- random-upper-bound a))))))


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
	(t (error "Bad argument, ~A, for RANDOM-STATE." state))))

(proclaim '(ftype (function (t) fixnum) rand3))
(defun random (arg &optional (state *random-state*))
  "Generate a uniformly distributed pseudo-random number between zero
  and Arg.  State, if supplied, is the random state to use."
  (typecase arg
    (fixnum
     (unless (plusp (the fixnum arg))
       (error "Non-positive argument, ~A, to RANDOM." arg))     
     (rem (the fixnum (rand3 state)) (the fixnum arg)))
    (float
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (let ((arg-length (float-digits arg)))
       (* arg (/ (float (random (ash 2 arg-length) state))
		 (float (ash 2 arg-length))))))
    (integer
     (unless (plusp arg)
       (error "Non-positive argument, ~A, to RANDOM." arg))
     (do ((tot (rand3 state) (+ (ash tot %fixnum-length) (rand3 state)))
	  (end (ash arg (- %fixnum-length))
	       (ash end (- %fixnum-length))))
	 ((zerop end) (mod tot arg))))
    (t (error "Wrong type argument, ~A, to RANDOM." arg))))
