;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; Storage purifier for Spice Lisp.
;;; Written by Rob MacLachlan and Skef Wholey.
;;;
;;;    The function Purify, defined herein, puts as much of the Lisp system as
;;; possible into Read-Only and Static spaces so that subsequent garbage
;;; collections are quicker.  This is done by frobbing the free-pointers for
;;; spaces so that new objects are put in static or read-only space as
;;; appropiate, then doing a GC.
;;;
;;;    We also transport all of the dynamic symbols in Lisp code so we
;;; can do clever things that improve locality in the resulting Lisp. 
;;; Some constant conses and g-vectors are also transported in macrocode
;;; so that we can put them in read-only space.
;;;
(in-package 'lisp)


(def-c-routine ("purify" %purify) (void)
  (static-roots unsigned-long)
  (read-only-roots unsigned-long))

(defun purify (&key root-structures constants)
  (write-string "[Doing purification: ")
  (force-output)
  (without-gcing
   (clear-auto-gc-trigger)
   (%purify (di::get-lisp-obj-address root-structures)
	    (di::get-lisp-obj-address constants))
   (when *gc-trigger*
     (setf *gc-trigger* *bytes-coned-between-gcs*)
     (set-auto-gc-trigger *gc-trigger*)))
  (write-line "Done.]")
  (force-output)
  nil)

